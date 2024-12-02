package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class nop_unit() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val dec_entry = slave(decorder_entry(CoreConfig())) // from issue stage
    val ex_operand_entry = slave(operand_entry(CoreConfig()))  // from issue
    val nopu_ex_entry = master(nop_res_entry(CoreConfig()))  // to commit
  }
  val nopu_trans_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val nopu_instr = Reg(UInt(InstBus bits)) init(0)
  val nopu_pc = Reg(UInt(InstAddrBus bits)) init(0)

  nopu_trans_id := io.ex_operand_entry.trans_id
  nopu_instr := io.ex_operand_entry.instr
  nopu_pc := io.ex_operand_entry.pc

  io.nopu_ex_entry.trans_id := nopu_trans_id
  io.nopu_ex_entry.instr := nopu_instr
  io.nopu_ex_entry.pc := nopu_pc

}