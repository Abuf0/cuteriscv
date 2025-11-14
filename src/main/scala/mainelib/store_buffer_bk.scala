package mainelib

import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

/**
 * 2 发射（小核 / 轻 OoO 或顺序核）：1×LSU
 *
 * 1 条地址管线（AGU）同时服务 Load/Store；配 1 端口 L1D + Store Buffer。
 *
 * 3–4 发射（中核 / 常见 OoO）：2×LSU 最普遍
 *
 * 通常等效 2×Load 并发 + 1×Store/拍（通过 Store Buffer 平滑写入）；L1D 至少 2 读端口或多拍仲裁。
 *
 * 6–8 发射（大 OoO）：2–3×LSU
 *
 * 高吞吐负载（如内存密集、向量前后端）会上到 3 条地址管线，但受限于 L1D 端口与功耗。
 *
 * 向量/服务器核：2×LSU + 专用向量内存端口 或 3×LSU
 *
 * 结合更深的 Load/Store Queue（如 64–128 entries）与更大的 Store Buffer。
 **/
// 不支持同时多条指令写入
case class store_buffer_bk(cfg : CoreConfig, LSU_NUM : Int) extends Component {
  val io = new Bundle {

    val in_flush_g = in Bool()  // global
    val in_flush_l = in Bool()  // local
    val in_flush_l_id = in UInt(cfg.SCB_ID_WIDTH bits)

    val store_ex_if = slave(store_entry(cfg)) // from ex
    val mem_cmt_if = slave(cmt_memw_entry(CoreConfig()))  // from commit

    val toload_addr = Vec.fill(LSU_NUM)(in UInt(cfg.DataAddrBus bits))
    val toload_hit = Vec.fill(LSU_NUM)(out UInt(cfg.DMemSelBus bits))
    val toload_data = Vec.fill(LSU_NUM)(out UInt(cfg.DataBus bits))
    val store_buffer_busy = out Bool()
  }
  io.toload_hit.setAsReg()
  io.toload_data.setAsReg()
  io.store_buffer_busy := False

  val MEM_ADDR_TAB = Vec(Reg(UInt(cfg.DataAddrBus bits)) init(0), cfg.SCB_INSTR_DEPTH)
  val MEM_DATA_0_TAB = Vec(Reg(UInt(cfg.ByteWidth bits)) init(0), cfg.SCB_INSTR_DEPTH)
  val MEM_DATA_1_TAB = Vec(Reg(UInt(cfg.ByteWidth bits)) init(0), cfg.SCB_INSTR_DEPTH)
  val MEM_DATA_2_TAB = Vec(Reg(UInt(cfg.ByteWidth bits)) init(0), cfg.SCB_INSTR_DEPTH)
  val MEM_DATA_3_TAB = Vec(Reg(UInt(cfg.ByteWidth bits)) init(0), cfg.SCB_INSTR_DEPTH)

  val MEM_SEL_0_TAB = Vec(Reg(Bool()) init(False), cfg.SCB_INSTR_DEPTH)
  val MEM_SEL_1_TAB = Vec(Reg(Bool()) init(False), cfg.SCB_INSTR_DEPTH)
  val MEM_SEL_2_TAB = Vec(Reg(Bool()) init(False), cfg.SCB_INSTR_DEPTH)
  val MEM_SEL_3_TAB = Vec(Reg(Bool()) init(False), cfg.SCB_INSTR_DEPTH)

  val MEM_VLD_TAB = Vec(Reg(Bool()) init(False), cfg.SCB_INSTR_DEPTH)

  val (store_hit, store_index): (Bool, UInt) = MEM_ADDR_TAB.sFindFirst(_===io.store_ex_if.store_wb_addr)
  val (idle_hit, idle_index): (Bool, UInt) = MEM_VLD_TAB.sFindFirst(_===False)
  val (commit_hit, commit_index): (Bool, UInt) = MEM_ADDR_TAB.sFindFirst(_===io.mem_cmt_if.mem_wdata)

  for (i <- 0 until LSU_NUM) {
    val (load_hit, load_index): (Bool, UInt) = MEM_ADDR_TAB.sFindFirst(_===io.toload_addr(i))
    when(io.store_ex_if.store_wb_en && (io.store_ex_if.store_wb_addr === io.toload_addr(i))) {
      io.toload_hit(i) := io.store_ex_if.store_wb_byte.asUInt
      io.toload_data(i) := io.store_ex_if.store_wb_data
    }.elsewhen(load_hit) {
      io.toload_hit(i) := U(MEM_SEL_3_TAB(load_index) ## MEM_SEL_2_TAB(load_index) ## MEM_SEL_1_TAB(load_index) ## MEM_SEL_0_TAB(load_index))
      io.toload_data(i) := MEM_DATA_3_TAB(load_index) @@ MEM_DATA_2_TAB(load_index) @@ MEM_DATA_1_TAB(load_index) @@ MEM_DATA_0_TAB(load_index)
    }.otherwise {
      io.toload_hit(i) := U"0000"
      io.toload_data(i) := 0
    }
  }

  when(io.in_flush_g){
    for (i <- 0 until cfg.SCB_INSTR_DEPTH) {
      MEM_VLD_TAB(i) := False
      MEM_SEL_0_TAB(i) := False
      MEM_SEL_1_TAB(i) := False
      MEM_SEL_2_TAB(i) := False
      MEM_SEL_3_TAB(i) := False
    }
  } .otherwise{
    when(io.store_ex_if.store_wb_en === True){
      when(store_hit) { // 保证地址唯一性
        MEM_VLD_TAB(store_index) := True
        when(io.store_ex_if.store_wb_byte(0) === True) {
          MEM_DATA_0_TAB(store_index) := io.store_ex_if.store_wb_data(7 downto 0)
          MEM_SEL_0_TAB(store_index) := True
        }.otherwise {}
        when(io.store_ex_if.store_wb_byte(1) === True) {
          MEM_DATA_1_TAB(store_index) := io.store_ex_if.store_wb_data(15 downto 8)
          MEM_SEL_1_TAB(store_index) := True
        }.otherwise {}
        when(io.store_ex_if.store_wb_byte(2) === True) {
          MEM_DATA_2_TAB(store_index) := io.store_ex_if.store_wb_data(23 downto 16)
          MEM_SEL_2_TAB(store_index) := True
        }.otherwise {}
        when(io.store_ex_if.store_wb_byte(3) === True) {
          MEM_DATA_3_TAB(store_index) := io.store_ex_if.store_wb_data(31 downto 24)
          MEM_SEL_3_TAB(store_index) := True
        }.otherwise {}
      } .otherwise{
        //when(idle_hit){ // todo: confirm that :when not hit --> idle_index == 0
        MEM_VLD_TAB(idle_index) := True
        MEM_ADDR_TAB(idle_index) := io.store_ex_if.store_wb_addr
        when(io.store_ex_if.store_wb_byte(0) === True) {
          MEM_DATA_0_TAB(idle_index) := io.store_ex_if.store_wb_data(7 downto 0)
          MEM_SEL_0_TAB(idle_index) := True
        }.otherwise {}
        when(io.store_ex_if.store_wb_byte(1) === True) {
          MEM_DATA_1_TAB(idle_index) := io.store_ex_if.store_wb_data(15 downto 8)
          MEM_SEL_1_TAB(idle_index) := True
        }.otherwise {}
        when(io.store_ex_if.store_wb_byte(2) === True) {
          MEM_DATA_2_TAB(idle_index) := io.store_ex_if.store_wb_data(23 downto 16)
          MEM_SEL_2_TAB(idle_index) := True
        }.otherwise {}
        when(io.store_ex_if.store_wb_byte(3) === True) {
          MEM_DATA_3_TAB(idle_index) := io.store_ex_if.store_wb_data(31 downto 24)
          MEM_SEL_3_TAB(idle_index) := True
        }.otherwise {}
        //} .otherwise{ }
      }
    }

    when(io.mem_cmt_if.mem_wen && ~(io.store_ex_if.store_wb_en && io.store_ex_if.store_wb_addr === io.mem_cmt_if.mem_waddr)){  // 现在有指令提交 && 该提交指令没撞上同一个地址的store wb指令
      when(commit_hit) {
        MEM_VLD_TAB(commit_index) := False
        //MEM_SEL_0_TAB(commit_index) := False
        //MEM_SEL_1_TAB(commit_index) := False
        //MEM_SEL_2_TAB(commit_index) := False
        //MEM_SEL_3_TAB(commit_index) := False
      } .otherwise{ }
    } .otherwise{ }

  }
}
