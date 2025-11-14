package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._
/**先不管M/S**/

case class CSR_FILE(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val csr_wif = Vec.fill(cfg.issueWidth)(slave(commit_csr_entry(cfg)))
    val cmt_exc_csr_entry = slave(exc_csr_entry(cfg))
    val CSR_REGFILE = Vec.fill(cfg.CSR_NUM)(out UInt(cfg.CSRDataBus bits))
  }

  val CSR_UNIT = Vec.fill(cfg.CSR_NUM)(Reg(UInt(cfg.CSRDataBus bits)) init(0))

  io.CSR_REGFILE := CSR_UNIT

  for (i <- 0 until cfg.issueWidth) {
    when(io.csr_wif(i).reg_wten){
      CSR_UNIT(io.csr_wif(i).reg_addr) := io.csr_wif(i).reg_wdata
    }
  }

  when(io.cmt_exc_csr_entry.exc_req){
    CSR_UNIT(CSR.MEPC(cfg.debug_en)) := io.cmt_exc_csr_entry.exc_pc
    CSR_UNIT(CSR.MCAUSE(cfg.debug_en)) := io.cmt_exc_csr_entry.exc_cause
    CSR_UNIT(CSR.MTVAL(cfg.debug_en)) := io.cmt_exc_csr_entry.exc_val
    CSR_UNIT(CSR.MSTATUS(cfg.debug_en)) := io.cmt_exc_csr_entry.exc_status

  }

}

