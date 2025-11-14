package mainelib
import spinal.core._
import spinal.lib._

/** 不包含寄存器 **/
case class if2_tm(cfg : CoreConfig) extends Component {
  val io = new Bundle{
    val pc_fl = slave(Flow(UInt(cfg.InstAddrBus bits))) // from pc
    val predict_fl = master(Flow(UInt(cfg.InstAddrBus bits)))  // to pc
    val resolved_if = slave(branch_predict_entry(cfg))  // from exe
    val tm_fl = master(Flow(UInt(cfg.InstAddrBus bits))) // to mmu
    val predict_branch_if = master(branch_predict_entry(cfg)) // to if3-fm
  }

  if(cfg.withbranch_predict) {
    /** 仅做了轻量化分支预测，默认只对LSB指令做分支预测， todo: 后续考虑支持混长/多发射 **/
    val bht = new bht(cfg)
    val btb = new btb(cfg)
    val ras = new ras(cfg)

    bht.io.pc_fl := io.pc_fl
    bht.io.resolved_if := io.resolved_if

    btb.io.pc_fl := io.pc_fl
    btb.io.resolved_if := io.resolved_if

    ras.io.pc_fl := io.pc_fl
    ras.io.resolved_if := io.resolved_if
    ras.io.predict_branch_if := btb.io.predict_if

    // to pc
    io.predict_fl.valid := bht.io.predict_fl.valid
    io.predict_fl.payload := 0

    // to next stage
    io.predict_branch_if := btb.io.predict_if

    when(bht.io.predict_fl.valid) {
      io.predict_branch_if.branch_taken := bht.io.predict_fl.valid
      when(ras.io.predict_fl.valid) {
        io.predict_fl.payload := ras.io.predict_fl.payload // to pc
        io.predict_branch_if.branch_target := ras.io.predict_fl.payload
      }.elsewhen(btb.io.predict_if.branch_valid) {
        io.predict_fl.payload := btb.io.predict_if.branch_target
      }
    }

    // to mmu
    io.tm_fl := io.pc_fl
    when(io.predict_branch_if.branch_taken){
      io.tm_fl.payload := io.predict_branch_if.branch_target
    }



  } else {
    io.tm_fl := io.pc_fl
    io.predict_branch_if.assignFromBits(B(0, io.predict_branch_if.asBits.getWidth bits))
  }

}
