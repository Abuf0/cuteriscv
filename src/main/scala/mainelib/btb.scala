package mainelib
import spinal.core._
import spinal.lib._

/**  **/

class btb (cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val pc_fl = slave(Flow(UInt(cfg.InstAddrBus bits)))
    val predict_if = master(branch_predict_entry(cfg))
    val resolved_if = slave(branch_predict_entry(cfg))
  }

  //io.predict_if.assignFromBits(B(0, io.predict_if.asBits.getWidth bits))
  io.predict_if.is_ret := False
  io.predict_if.is_call := False
  io.predict_if.is_jump := False
  io.predict_if.is_branch := False
  io.predict_if.ret_cor := False
  io.predict_if.branch_cor := False
  io.predict_if.call_cor := False
  io.predict_if.jump_cor := False
  io.predict_if.branch_taken := False
  io.predict_if.branch_target := 0
  io.predict_if.branch_valid := False
  io.predict_if.pc := io.pc_fl.payload

  val btb_lut = Vec.fill(cfg.BTB_PAGE_NUM)(
    Reg(UInt(cfg.InstAddrBus+cfg.InstAddrBus+(CFI_STATE().getBitsWidth) bits)) init(0)
  )

  //val replace_index = U"0"  // TODO with LRU/LFU replace strategy
  val replace_index = Reg(UInt(cfg.BTB_PAGE_WIDTH bits)) init(0)
  val replace_index_last = Reg(UInt(cfg.BTB_PAGE_WIDTH bits)) init(0)

  val hit_vec = Vec(Bool(),cfg.BTB_PAGE_NUM)
  val hit = hit_vec.sContains(True)


  object CFI_STATE extends SpinalEnum {
    val IS_CALL, IS_RET, IS_JUMP, IS_BRANCH, IS_NONE = newElement()
  }


  val TARGET_LSB = 0
  val TARGET_MSB = cfg.InstAddrBus-1
  val INDEX_LSB = TARGET_MSB+1
  val INDEX_MSB = INDEX_LSB+cfg.InstAddrBus-1
  val CFI_LSB = INDEX_MSB+1
  val CFI_MSB = CFI_LSB+(CFI_STATE().getBitsWidth)-1

  for(i <- 0 until cfg.BTB_PAGE_NUM) {
    val target_pc = btb_lut(i)(TARGET_MSB downto TARGET_LSB)
    val btb_index = btb_lut(i)(INDEX_MSB downto INDEX_LSB)
    val btb_cfi = btb_lut(i)(CFI_MSB downto CFI_LSB) // call/ret/jump/branch/none
    val btb_cfi_t = btb_cfi.as(CFI_STATE())


    when(io.pc_fl.payload === btb_index && io.pc_fl.valid){
      //io.predict_if.branch_taken := (btb_cfi_t =/= CFI_STATE.IS_NONE) // btb不参与taken的判断
      io.predict_if.branch_target := target_pc
      io.predict_if.is_call := (btb_cfi_t === CFI_STATE.IS_CALL)
      io.predict_if.is_ret := (btb_cfi_t === CFI_STATE.IS_CALL)
      io.predict_if.is_jump := (btb_cfi_t === CFI_STATE.IS_JUMP)
      io.predict_if.is_branch := (btb_cfi_t === CFI_STATE.IS_BRANCH)
      io.predict_if.branch_valid := True
      io.predict_if.pc := io.pc_fl.payload
    }


    when(io.resolved_if.branch_valid && io.resolved_if.branch_taken){
      when(io.resolved_if.pc === btb_index){
        replace_index := i
      } .otherwise {
        replace_index := replace_index_last + 1
        replace_index_last := replace_index_last + 1
      }
    }

  }

  for(i <- 0 until cfg.BTB_PAGE_NUM) {
    hit_vec(i) := btb_lut(i)(cfg.InstAddrBus+cfg.InstAddrBus-1 downto cfg.InstAddrBus) === io.resolved_if.pc
  }

  val resolved_cfi = CFI_STATE()
  resolved_cfi := CFI_STATE.IS_NONE
  when(io.resolved_if.branch_taken) {
    when(io.resolved_if.is_call)  {resolved_cfi := CFI_STATE.IS_CALL}
      .elsewhen(io.resolved_if.is_ret)  {resolved_cfi := CFI_STATE.IS_RET}
      .elsewhen(io.resolved_if.is_jump)  {resolved_cfi := CFI_STATE.IS_JUMP}
      .elsewhen(io.resolved_if.is_branch)  {resolved_cfi := CFI_STATE.IS_BRANCH}
  }

  val resolved_cfi_t = resolved_cfi.asBits.asUInt
  for(i <- 0 until cfg.BTB_PAGE_NUM) {
    when(i === replace_index && io.resolved_if.branch_valid === True){
      btb_lut(i) := ((CFI_MSB downto CFI_LSB) -> resolved_cfi_t, (INDEX_MSB downto INDEX_LSB) -> io.resolved_if.pc, (TARGET_MSB downto TARGET_LSB) -> io.resolved_if.branch_target)
    }
  }

}
