package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave

class maineriscv(cfg: CoreConfig) extends Component{
  val io = new Bundle {
    val cosim_dpi_if = Vec.fill(cfg.issueWidth)(master(trace_entry(cfg)))
  }
  val core = new mainecore(cfg)
  val itcm = new itcm_wrapper(cfg)
  val dtcm = new dtcm_wrapper(cfg)

  core.io.itcm_if <> itcm.io.itcm_if
  core.io.dtcm_if <> dtcm.io.dtcm_if
  io.cosim_dpi_if <> core.io.cosim_dpi_if
  // todo
  core.io.biu_if.mem_rvalid := True
  core.io.biu_if.mem_wvalid := True
  core.io.biu_if.mem_rdata := 0
  core.io.biu_if.mem_ready := True

}


object scfg extends SpinalConfig(
  targetDirectory        = "build/maine/s2rtl",        // 输出目录
  oneFilePerComponent    = true,         // 每个组件单独一个 .v
  globalPrefix           = "",
  anonymSignalPrefix     = "tmp",
  noAssert               = true,

  defaultClockDomainFrequency = FixedFrequency(1000 MHz),
  defaultConfigForClockDomains = ClockDomainConfig(
    resetKind         = ASYNC,
    resetActiveLevel  = LOW
  )
)


object top {
  def main(args: Array[String]) {
    val ccfg = CoreConfig()
    CfgPrinter.printAll(ccfg)
    val FU_ID_selDef = FU_ID.build(ccfg.issueWidth)
    scfg.generateSystemVerilog(new maineriscv(ccfg))
  }
}
