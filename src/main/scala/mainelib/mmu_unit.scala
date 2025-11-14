package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class mmu_unit(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    //  config
    val mmu_en = in Bool()  // 是否使能VA虚拟地址映射 + PA的权限管理
    val pmp_en = in Bool()  // 是否开启PA的权限管理

    // from IFU (maybe VA)
    val ifu_mem_va_if = slave(mem_if(cfg,cfg.InstAddrBus,cfg.InstBus))
    // from LSU (maybe VA)
    val lsu_mem_va_if = slave(mem_if(cfg,cfg.DataAddrBus,cfg.DataBus))

    // PA from IFU
    val ifu_mem_pa_if = master(mem_if(cfg,cfg.InstAddrBus,cfg.InstBus))

    // PA from LSU
    val lsu_mem_pa_if = master(mem_if(cfg,cfg.DataAddrBus,cfg.DataBus))

    // flush out
    val page_fault_fl = master(Flow(UInt(cfg.InstAddrBus bits)))
    val ifu_rfault = out Bool()
    val lsu_rfault = out Bool()
    val lsu_wfault = out Bool()

  }

  /**
   core给出虚拟地址VA
   TLB+pgae遍历翻译物理地址： 如果TLB hit，PA = PPN+offset；如果TLB miss，PA = Page Table Walk并更新TLB；若权限或PTE无效，发出page fault；
   查询PMP权限
   PA送到cache/bus
  **/
  io.ifu_mem_pa_if <> io.ifu_mem_va_if
  io.lsu_mem_pa_if <> io.lsu_mem_va_if
  // todo
  io.ifu_rfault := False
  io.lsu_rfault := False
  io.lsu_wfault := False
  io.page_fault_fl.valid := False
  io.page_fault_fl.payload := 0


}
