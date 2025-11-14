package mainelib

import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._
import RangeFirstDyn._

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
// todo : 当前0_1_2_3_TAB只支持DMEM width=32bit，后续需要修改
case class store_buffer(cfg : CoreConfig, LSU_NUM : Int) extends Component {
  val io = new Bundle {
    
    val in_flush_g = in Bool()  // global
    val in_flush_l = in Bool()  // local
    val in_flush_l_id = in UInt(cfg.SCB_ID_WIDTH bits)

    val store_ex_if = slave(store_entry(cfg)) // from ex
    val mem_cmt_if = slave(cmt_memw_entry(CoreConfig()))  // from commit
    val mem_cmt_vld = in Bool()
    val mem_cmt_trans_id = in UInt(cfg.SCB_ID_WIDTH bits)
    val cmt_trans_id = in UInt(cfg.SCB_ID_WIDTH bits)

    val toload_need = Vec.fill(LSU_NUM)(in Bool())
    val toload_id = Vec.fill(LSU_NUM)(in UInt(cfg.SCB_ID_WIDTH bits))
    val toload_addr = Vec.fill(LSU_NUM)(in UInt(cfg.DataAddrBus bits))
    val toload_hit = Vec.fill(LSU_NUM)(out UInt(cfg.DMemSelBus bits))
    val toload_data = Vec.fill(LSU_NUM)(out UInt(cfg.DataBus bits))
    val toload_vld = Vec.fill(LSU_NUM)(out Bool())
    val store_buffer_busy = out Bool()
  }
  io.toload_hit.setAsReg()
  io.toload_data.setAsReg()
  io.toload_vld.setAsReg()
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

//  val (store_hit, store_index): (Bool, UInt) = MEM_ADDR_TAB.sFindFirst(_===io.store_ex_if.store_wb_addr)
//  val (idle_hit, idle_index): (Bool, UInt) = MEM_VLD_TAB.sFindFirst(_===False)
  val store_index = io.store_ex_if.store_wb_trans_id.resize(cfg.SCB_INSTR_WIDTH)
  val commit_index = io.mem_cmt_trans_id.resize(cfg.SCB_INSTR_WIDTH)
  //val cmt_ptr = RegNextWhen(io.mem_cmt_trans_id,(io.mem_cmt_if.mem_wen & io.mem_cmt_vld)) init(0)
  val cmt_ptr = RegNext(io.cmt_trans_id) init(0)
  val load_hit_tab = Vec.fill(LSU_NUM)(Vec.fill(4)(Bool()))
  val load_index_tab = Vec.fill(LSU_NUM)(Vec.fill(4)(UInt(cfg.SCB_INSTR_WIDTH bits)))

  for (i <- 0 until LSU_NUM) {
    when(load_hit_tab(i).orR) {
      io.toload_hit(i) := U(load_hit_tab(i)(3)##load_hit_tab(i)(2)##load_hit_tab(i)(1)##load_hit_tab(i)(0))
      io.toload_data(i) := MEM_DATA_3_TAB(load_index_tab(i)(3)) @@ MEM_DATA_2_TAB(load_index_tab(i)(2)) @@ MEM_DATA_1_TAB(load_index_tab(i)(1)) @@ MEM_DATA_0_TAB(load_index_tab(i)(0))
    }.otherwise {
      io.toload_hit(i) := U"0000"
      io.toload_data(i) := 0
    }

    when(io.toload_need(i)) {
      io.toload_vld(i) := True
    } .otherwise {
      io.toload_vld(i) := False
    }
  }


  val mem_sel = Vec.fill(4)(Vec.fill(cfg.SCB_INSTR_DEPTH)(Bool()))
  mem_sel(0) := MEM_SEL_0_TAB
  mem_sel(1) := MEM_SEL_1_TAB
  mem_sel(2) := MEM_SEL_2_TAB
  mem_sel(3) := MEM_SEL_3_TAB
  val load_hit_dbg = Vec.fill(4)(Vec.fill(cfg.SCB_INSTR_DEPTH)(Bool()))
  val start_index = cmt_ptr.resize(cfg.SCB_INSTR_WIDTH)
  val end_index = io.toload_id(0).resize(cfg.SCB_INSTR_WIDTH)

  for (i <- 0 until LSU_NUM) {
    val load_hit = Vec.fill(4)(Vec.fill(cfg.SCB_INSTR_DEPTH)(Bool()))
    for (j <- 0 until 4) {
      for (k <- 0 until cfg.SCB_INSTR_DEPTH) {
        load_hit(j)(k) := MEM_VLD_TAB(k) && (MEM_ADDR_TAB(k) === io.toload_addr(i)) && io.toload_need(i) && mem_sel(j)(k)
        if(i==0){
          load_hit_dbg(j)(k) := MEM_VLD_TAB(k) && (MEM_ADDR_TAB(k) === io.toload_addr(i)) && io.toload_need(i) && mem_sel(j)(k)
        }
      }
      val (found,index) = firstTrueInRangeDyn(load_hit(j), start = cmt_ptr.resize(cfg.SCB_INSTR_WIDTH), end = io.toload_id(i).resize(cfg.SCB_INSTR_WIDTH), preferEndNearest = true, circular = true)
      load_hit_tab(i)(j) := found
      load_index_tab(i)(j) := index
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
  } .elsewhen(io.in_flush_l) {
    for (i <- 0 until cfg.SCB_INSTR_DEPTH) {
      when(i > io.in_flush_l_id.resize(cfg.SCB_INSTR_WIDTH)) {
        MEM_VLD_TAB(i) := False
        MEM_SEL_0_TAB(i) := False
        MEM_SEL_1_TAB(i) := False
        MEM_SEL_2_TAB(i) := False
        MEM_SEL_3_TAB(i) := False
      }
    }
  } .otherwise{
    when(io.store_ex_if.store_wb_en === True){
        MEM_VLD_TAB(store_index) := True
        MEM_ADDR_TAB(store_index) := io.store_ex_if.store_wb_addr
        when(io.store_ex_if.store_wb_byte(0) === True) {
          MEM_DATA_0_TAB(store_index) := io.store_ex_if.store_wb_data(7 downto 0)
          MEM_SEL_0_TAB(store_index) := True
        }.otherwise {
          MEM_SEL_0_TAB(store_index) := False
        }
        when(io.store_ex_if.store_wb_byte(1) === True) {
          MEM_DATA_1_TAB(store_index) := io.store_ex_if.store_wb_data(15 downto 8)
          MEM_SEL_1_TAB(store_index) := True
        }.otherwise {
          MEM_SEL_1_TAB(store_index) := False
        }
        when(io.store_ex_if.store_wb_byte(2) === True) {
          MEM_DATA_2_TAB(store_index) := io.store_ex_if.store_wb_data(23 downto 16)
          MEM_SEL_2_TAB(store_index) := True
        }.otherwise {
          MEM_SEL_2_TAB(store_index) := False
        }
        when(io.store_ex_if.store_wb_byte(3) === True) {
          MEM_DATA_3_TAB(store_index) := io.store_ex_if.store_wb_data(31 downto 24)
          MEM_SEL_3_TAB(store_index) := True
        }.otherwise {
          MEM_SEL_3_TAB(store_index) := False
        }
    }

    when(io.mem_cmt_if.mem_wen & io.mem_cmt_vld){  // 现在有指令提交 && 该提交指令没撞上同一个地址的store wb指令
      MEM_VLD_TAB(commit_index) := False
    } .otherwise{ }

  }
}