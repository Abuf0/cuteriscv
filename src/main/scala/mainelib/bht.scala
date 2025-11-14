package mainelib
import spinal.core._
import spinal.lib._

/**  **/

class bht (cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val pc_fl = slave(Flow(UInt(cfg.InstAddrBus bits)))
    val predict_fl = master(Flow(UInt(cfg.InstAddrBus bits)))
    val resolved_if = slave(branch_predict_entry(cfg))
  }
  object BHT_STATE extends SpinalEnum {
    val Strongly_not_taken, Weakly_not_taken, Weakly_taken, Strongly_taken = newElement()
  }

  val fsm_num = (1<<cfg.BHT_OFFSET)

  val taken_fsm = Vec.fill(fsm_num)(
    Reg(BHT_STATE()) init(BHT_STATE.Weakly_not_taken)   // 每项是一个枚举寄存器，带复位值
  )


  for(i <- 0 until fsm_num) {
    val trans_weak = io.resolved_if.branch_valid && ~io.resolved_if.branch_taken && (io.resolved_if.pc(cfg.BHT_OFFSET+1 downto 2) === i)
    val trans_strong = io.resolved_if.branch_valid && io.resolved_if.branch_taken && (io.resolved_if.pc(cfg.BHT_OFFSET+1 downto 2) === i)

    switch(taken_fsm(i)){
      is(BHT_STATE.Strongly_taken){
        when(trans_strong){
          taken_fsm(i) := BHT_STATE.Strongly_taken
        } .elsewhen(trans_weak){
          taken_fsm(i) := BHT_STATE.Weakly_taken
        } .otherwise{}
      }
      is(BHT_STATE.Weakly_taken){
        when(trans_strong){
          taken_fsm(i) := BHT_STATE.Strongly_taken
        } .elsewhen(trans_weak){
          taken_fsm(i) := BHT_STATE.Weakly_not_taken
        } .otherwise{}
      }
      is(BHT_STATE.Weakly_not_taken){
        when(trans_strong){
          taken_fsm(i) := BHT_STATE.Weakly_taken
        } .elsewhen(trans_weak){
          taken_fsm(i) := BHT_STATE.Strongly_not_taken
        } .otherwise{}
      }
      is(BHT_STATE.Strongly_not_taken){
        when(trans_strong){
          taken_fsm(i) := BHT_STATE.Weakly_not_taken
        } .elsewhen(trans_weak){
          taken_fsm(i) := BHT_STATE.Strongly_not_taken
        } .otherwise{}
      }

    }
  }

  io.predict_fl.valid := io.pc_fl.valid && (taken_fsm(io.pc_fl.payload(cfg.BHT_OFFSET+1 downto 2)) === BHT_STATE.Strongly_taken) || (taken_fsm(io.pc_fl.payload(cfg.BHT_OFFSET+1 downto 2)) === BHT_STATE.Weakly_taken)

}
