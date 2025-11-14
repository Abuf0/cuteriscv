package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class PRF_FILE(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val prf_wif = Vec.fill(cfg.issueWidth)(slave(preg_wr_entry(cfg)))
    val rs1_if = Vec.fill(cfg.issueWidth)(slave(preg_rd_if(cfg)))
    val rs2_if = Vec.fill(cfg.issueWidth)(slave(preg_rd_if(cfg)))
  }

  val PRF_UNIT = Vec.fill(cfg.REG_PRF_NUM)(Reg(UInt(cfg.RegDataBus bits)) init(0))

  for (i <- 0 until cfg.issueWidth) {
    io.rs1_if(i).reg_rdata := 0
    io.rs2_if(i).reg_rdata := 0
  }

  for (i <- 0 until cfg.issueWidth) {
    when(io.rs1_if(i).reg_rden) {
      io.rs1_if(i).reg_rdata := PRF_UNIT(io.rs1_if(i).reg_addr)
    }
    when(io.rs2_if(i).reg_rden) {
      io.rs2_if(i).reg_rdata := PRF_UNIT(io.rs2_if(i).reg_addr)
    }
  }

  for (i <- 0 until cfg.issueWidth) {
    when(io.prf_wif(i).reg_wten){
      when(io.prf_wif(i).areg_addr =/= 0) {
        PRF_UNIT(io.prf_wif(i).preg_addr) := io.prf_wif(i).reg_wdata

        when(io.rs1_if(i).reg_rden && io.rs1_if(i).reg_addr === io.prf_wif(i).preg_addr) {
          io.rs1_if(i).reg_rdata := io.prf_wif(i).reg_wdata
        }
        when(io.rs2_if(i).reg_rden && io.rs2_if(i).reg_addr === io.prf_wif(i).preg_addr) {
          io.rs2_if(i).reg_rdata := io.prf_wif(i).reg_wdata
        }
      }
    }
  }


}
