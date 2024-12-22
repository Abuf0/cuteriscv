package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class store_buffer() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val flush = in Bool()

    val wb_dcache_interface_wb = slave(dcache_write_interface(CoreConfig())) // from wb
    val wb_dacahe_interfacec_commit = slave(dcache_write_interface(CoreConfig()))  // from commit

    val toload_addr = in UInt (DataAddrBus bits) // to wb
    val toload_hit = out UInt (4 bits)
    val toload_data = out UInt (DataBus bits)
  }

  val MEM_ADDR_TAB = Vec(Reg(UInt(DataAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val MEM_DATA_0_TAB = Vec(Reg(UInt(8 bits)) init(0), SCB_INSTR_DEEPTH)
  val MEM_DATA_1_TAB = Vec(Reg(UInt(8 bits)) init(0), SCB_INSTR_DEEPTH)
  val MEM_DATA_2_TAB = Vec(Reg(UInt(8 bits)) init(0), SCB_INSTR_DEEPTH)
  val MEM_DATA_3_TAB = Vec(Reg(UInt(8 bits)) init(0), SCB_INSTR_DEEPTH)

  val MEM_SEL_0_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val MEM_SEL_1_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val MEM_SEL_2_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val MEM_SEL_3_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)

  val MEM_VLD_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)

  val (store_hit, store_index): (Bool, UInt) = MEM_ADDR_TAB.sFindFirst(_===io.wb_dcache_interface_wb.waddr)
  val (idle_hit, idle_index): (Bool, UInt) = MEM_VLD_TAB.sFindFirst(_===False)
  val (commit_hit, commit_index): (Bool, UInt) = MEM_ADDR_TAB.sFindFirst(_===io.wb_dacahe_interfacec_commit.waddr)
  val (load_hit, load_index): (Bool, UInt) = MEM_ADDR_TAB.sFindFirst(_===io.toload_addr)

  when(load_hit){
    io.toload_hit := U(MEM_SEL_3_TAB(load_index) ## MEM_SEL_2_TAB(load_index) ## MEM_SEL_1_TAB(load_index) ## MEM_SEL_0_TAB(load_index))
    io.toload_data := MEM_DATA_3_TAB(load_index) @@ MEM_DATA_2_TAB(load_index) @@ MEM_DATA_1_TAB(load_index) @@ MEM_DATA_0_TAB(load_index)
  } .otherwise{
    io.toload_hit := U"0000"
    io.toload_data := 0
  }

  when(io.flush){
    for (i <- 0 until SCB_INSTR_DEEPTH) {
      MEM_VLD_TAB(i) := False
      MEM_SEL_0_TAB(i) := False
      MEM_SEL_1_TAB(i) := False
      MEM_SEL_2_TAB(i) := False
      MEM_SEL_3_TAB(i) := False
    }
  } .otherwise{
    when(io.wb_dcache_interface_wb.we === True){
      when(store_hit) { // 保证地址唯一性
        MEM_VLD_TAB(store_index) := True
        when(io.wb_dcache_interface_wb.sel(0) === True) {
          MEM_DATA_0_TAB(store_index) := io.wb_dcache_interface_wb.wdata(7 downto 0)
          MEM_SEL_0_TAB(store_index) := True
        }.otherwise {}
        when(io.wb_dcache_interface_wb.sel(1) === True) {
          MEM_DATA_1_TAB(store_index) := io.wb_dcache_interface_wb.wdata(15 downto 8)
          MEM_SEL_1_TAB(store_index) := True
        }.otherwise {}
        when(io.wb_dcache_interface_wb.sel(2) === True) {
          MEM_DATA_2_TAB(store_index) := io.wb_dcache_interface_wb.wdata(23 downto 16)
          MEM_SEL_2_TAB(store_index) := True
        }.otherwise {}
        when(io.wb_dcache_interface_wb.sel(3) === True) {
          MEM_DATA_3_TAB(store_index) := io.wb_dcache_interface_wb.wdata(31 downto 24)
          MEM_SEL_3_TAB(store_index) := True
        }.otherwise {}
      } .otherwise{
        //when(idle_hit){ // todo: confirm that :when not hit --> idle_index == 0
        MEM_VLD_TAB(idle_index) := True
        MEM_ADDR_TAB(idle_index) := io.wb_dcache_interface_wb.waddr
        when(io.wb_dcache_interface_wb.sel(0) === True) {
            MEM_DATA_0_TAB(idle_index) := io.wb_dcache_interface_wb.wdata(7 downto 0)
            MEM_SEL_0_TAB(idle_index) := True
          }.otherwise {}
          when(io.wb_dcache_interface_wb.sel(1) === True) {
            MEM_DATA_1_TAB(idle_index) := io.wb_dcache_interface_wb.wdata(15 downto 8)
            MEM_SEL_1_TAB(idle_index) := True
          }.otherwise {}
          when(io.wb_dcache_interface_wb.sel(2) === True) {
            MEM_DATA_2_TAB(idle_index) := io.wb_dcache_interface_wb.wdata(23 downto 16)
            MEM_SEL_2_TAB(idle_index) := True
          }.otherwise {}
          when(io.wb_dcache_interface_wb.sel(3) === True) {
            MEM_DATA_3_TAB(idle_index) := io.wb_dcache_interface_wb.wdata(31 downto 24)
            MEM_SEL_3_TAB(idle_index) := True
          }.otherwise {}
        //} .otherwise{ }
      }
    }

    when(io.wb_dacahe_interfacec_commit.we && ~(io.wb_dcache_interface_wb.we && io.wb_dcache_interface_wb.wdata === io.wb_dacahe_interfacec_commit.waddr)){  // 现在有指令提交 && 该提交指令没撞上同一个地址的store wb指令
      when(commit_hit) {
        MEM_VLD_TAB(commit_index) := False
        MEM_SEL_0_TAB(commit_index) := False
        MEM_SEL_1_TAB(commit_index) := False
        MEM_SEL_2_TAB(commit_index) := False
        MEM_SEL_3_TAB(commit_index) := False
      } .otherwise{ }
    } .otherwise{ }

  }
}