package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class nop_unit(cfg : CoreConfig, id: Int, latency: Int = 1) extends Component {
  val io = new Bundle {
    val ex_in_entry = Vec.fill(cfg.issueWidth)(slave(issue_entry(cfg)))  // from issue, broadcast
    val ex_out_entry = master(exe_nop_res_entry(cfg))  // to commit
    val unit_busy = out Bool()
    val flush_mispredict_in = slave(flush_mispredict_entry(cfg))

  }


  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
  //val idx = if (cfg.issueWidth == 1) U(0) else issue_id
  val unit_hit = Bool()
  issue_id := 0
  unit_hit := False

  io.unit_busy := False  // todo
  io.ex_out_entry.issue_id := issue_id
  for (i <- 0 until cfg.issueWidth) {
    when(io.ex_in_entry(i).fu_id.asBits.asUInt === id){
      issue_id := i
      unit_hit := io.ex_in_entry(i).valid
    }
  }

  val out_trans_id = io.ex_in_entry(issue_id).trans_id
  val in_trans_id = io.flush_mispredict_in.trans_id
  val after_mispredict = io.flush_mispredict_in.hit && ((in_trans_id(cfg.SCB_ID_WIDTH - 1) === out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) <= out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0))) || (in_trans_id(cfg.SCB_ID_WIDTH - 1) =/= out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) > out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0)) ))


  io.ex_out_entry.trans_id := io.ex_in_entry(issue_id).trans_id
  io.ex_out_entry.instr := io.ex_in_entry(issue_id).instr
  io.ex_out_entry.pc := io.ex_in_entry(issue_id).pc
  io.ex_out_entry.result_vld := unit_hit & ~after_mispredict
  io.ex_out_entry.issue_id := issue_id
  io.ex_out_entry.dec_valid := io.ex_in_entry(issue_id).dec_valid
  io.ex_out_entry.instr_err := io.ex_in_entry(issue_id).instr_err


}
