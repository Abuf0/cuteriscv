package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class exc_arbit() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val exc_commit_entry = slave(except_commit_entry(CoreConfig()))  // from commit
    val exc_entry = master(except_entry(CoreConfig()))  // to commit stage
  }
  when(io.exc_commit_entry.commit_req){  // 有优先级顺序，todo：考虑持续时间
    when(io.exc_commit_entry.pc >= U"hfffffffc"){ // todo : 先设一个试试，并且不应该从pc判断，而应该从icache或者MMU接口处捕获异常
      io.exc_entry.exc_req := True
      io.exc_entry.exc_pc := io.exc_commit_entry.pc
      io.exc_entry.exc_cause := U(B(EXC_CAUSE.ILEGAL_PC)).resized
      io.exc_entry.exc_val := io.exc_commit_entry.pc
    } .elsewhen(io.exc_commit_entry.reg_wb_en && io.exc_commit_entry.reg_wb_addr > DataMemNum){ // TODO :应该从Dcache或者MMU接口处捕获异常
      io.exc_entry.exc_req := True
      io.exc_entry.exc_pc := io.exc_commit_entry.pc
      io.exc_entry.exc_cause := U(B(EXC_CAUSE.ILEGAL_ACCESS)).resized
      io.exc_entry.exc_val := io.exc_commit_entry.pc
    } .elsewhen(io.exc_commit_entry.dec_valid === False){ // TODO :非法指令编码异常测试
      io.exc_entry.exc_req := True
      io.exc_entry.exc_pc := io.exc_commit_entry.pc
      io.exc_entry.exc_cause := U(B(EXC_CAUSE.ILEGAL_CODING)).resized
      io.exc_entry.exc_val := io.exc_commit_entry.instr
    } .otherwise{ // 非对齐，调试断点等todo
      io.exc_entry.exc_req := False
      io.exc_entry.exc_pc := 0
      io.exc_entry.exc_cause := 0
      io.exc_entry.exc_val := 0
    }
  } .otherwise{
    io.exc_entry.exc_req := False
    io.exc_entry.exc_pc := 0
    io.exc_entry.exc_cause := 0
    io.exc_entry.exc_val := 0
  }
}