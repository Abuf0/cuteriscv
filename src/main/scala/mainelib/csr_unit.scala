package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class csr_unit(cfg : CoreConfig, id: Int, latency: Int = 1) extends Component {
  val io = new Bundle {
    val ex_in_entry = Vec.fill(cfg.issueWidth)(slave(issue_entry(cfg)))  // from issue, broadcast
    val ex_out_entry = master(exe_csr_res_entry(cfg))  // to commit
    val unit_busy = out Bool()
    val flush_mispredict_in = slave(flush_mispredict_entry(cfg))

  }
  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
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

  val rs1 = io.ex_in_entry(issue_id).rs1_scb_entry.reg_rdata
  val csr = io.ex_in_entry(issue_id).rs1_scb_entry.reg_rdata
  val imm = io.ex_in_entry(issue_id).imm
  val pc = io.ex_in_entry(issue_id).pc

  val rd_res = UInt(cfg.RegDataBus bits)
  val csr_res = UInt(cfg.CSRDataBus bits)


  val out_trans_id = io.ex_in_entry(issue_id).trans_id
  val in_trans_id = io.flush_mispredict_in.trans_id
  val after_mispredict = io.flush_mispredict_in.hit && ((in_trans_id(cfg.SCB_ID_WIDTH - 1) === out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) <= out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0))) || (in_trans_id(cfg.SCB_ID_WIDTH - 1) =/= out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) > out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0)) ))

  io.ex_out_entry.trans_id := io.ex_in_entry(issue_id).trans_id
  io.ex_out_entry.instr := io.ex_in_entry(issue_id).instr
  io.ex_out_entry.pc := io.ex_in_entry(issue_id).pc
  io.ex_out_entry.result := rd_res
  io.ex_out_entry.result_csr := csr_res
  io.ex_out_entry.result_vld := unit_hit & ~after_mispredict

  io.ex_out_entry.reg_wif.preg_addr := io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_rename
  io.ex_out_entry.reg_wif.areg_addr := io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_real
  io.ex_out_entry.reg_wif.reg_wten := io.ex_in_entry(issue_id).rd_scb_entry.reg_wten
  io.ex_out_entry.reg_wif.reg_wdata := io.ex_out_entry.result

  io.ex_out_entry.csr_wif.reg_wten := io.ex_in_entry(issue_id).csr_entry.reg_wten
  io.ex_out_entry.csr_wif.reg_addr := io.ex_in_entry(issue_id).csr_entry.reg_addr
  io.ex_out_entry.csr_wif.reg_wdata := io.ex_out_entry.result_csr
  io.ex_out_entry.csr_wif.reg_rden := False

  // todo with other ALU instructions
  when(unit_hit) {
    switch(io.ex_in_entry(issue_id).instr) {
      is(CSRRW) {
        rd_res := csr
        csr_res := rs1
      }
      is(CSRRS) {
        rd_res := csr
        csr_res := rs1 | csr
      }
      is(CSRRC) {
        rd_res := csr
        csr_res := ~rs1 | csr
      }
      is(CSRRWI) {
        rd_res := csr
        csr_res := U(imm)
      }
      is(CSRRSI) {
        rd_res := csr
        csr_res := csr | U(imm)
      }
      is(CSRRCI) {
        rd_res := csr
        csr_res := csr | ~U(imm)
      }
      //...//
      default {
        rd_res := 0
        csr_res := 0
      }
    }


  } .otherwise{
    rd_res := 0
    csr_res := 0
  }


}

