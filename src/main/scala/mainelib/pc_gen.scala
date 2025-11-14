package mainelib
import spinal.core._
import spinal.lib._

/** 包含了if1-pc ==> if2-tm 的pipeline寄存器 **/

case class pc_gen(cfg : CoreConfig) extends Component {
  val io = new Bundle{
    val initvtor = in UInt(cfg.InstAddrBus bits) // 复位VTOR偏移值
    val trap_fl = slave(Flow(UInt(cfg.InstAddrBus bits))) // from trap ctrl
    val bj_cor_fl = slave(Flow(UInt(cfg.InstAddrBus bits))) // from exe
    val predict_fl = slave(Flow(UInt(cfg.InstAddrBus bits)))  // from bht/btb/ras
    val mmu_st = master(Stream(UInt(cfg.InstAddrBus bits)))  // from mmu
    val pc_fl = master(Flow(UInt(cfg.InstAddrBus bits)))  // to bht/btb/ras
    val in_flush = in Bool()  // from ctrl
    val in_stall = in Bool()  // from ctrl
  }

  val first_cycle = RegNext(True) init(False)
  val first_cycle_d1 = RegNext(first_cycle) init(False)
  val first_cycle_pos = first_cycle & ~first_cycle_d1

  val pc_fetch = Reg(UInt(cfg.InstAddrBus bits)) init(0)
  val pc_fetch_vld = Reg(Bool()) init(False)

  when(io.in_flush || ~first_cycle) { // unreachable
    pc_fetch := io.initvtor
    pc_fetch_vld := False
  } .otherwise {
    when(first_cycle_pos) {
      pc_fetch := io.initvtor
      pc_fetch_vld := True
    }.elsewhen(io.trap_fl.valid) {
      pc_fetch := io.trap_fl.payload
      pc_fetch_vld := True
    }.elsewhen(io.bj_cor_fl.valid) {
      pc_fetch := io.bj_cor_fl.payload
      pc_fetch_vld := True
    }.elsewhen(io.predict_fl.valid) {
      pc_fetch := io.predict_fl.payload
      pc_fetch_vld := True
    }.elsewhen(io.mmu_st.ready && !io.in_stall) {
      pc_fetch := pc_fetch + (cfg.InstLen * cfg.issueWidth)
      pc_fetch_vld := True
    } .otherwise{
      pc_fetch_vld := False
    }
  }

  io.pc_fl.valid := pc_fetch_vld
  io.pc_fl.payload := pc_fetch

}
