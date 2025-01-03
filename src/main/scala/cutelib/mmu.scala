package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._
import BundleImplicit._   // connect with master & slave


case class mmu() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val s_memory_write_interface = slave(cpu_write_interface(CoreConfig()))  // from commit
    val s_memory_read_interface = slave(cpu_read_interface(CoreConfig()))  // from lsu_unit
    //val m_dcache_write_interface = master(dcache_write_interface(CoreConfig()))  // to dcache
    //val m_dcache_read_interface = master(dcache_read_interface(CoreConfig()))  // to dcache
    val s_icache_entry = slave(cpu_read_interface(CoreConfig()))  // from instr
    //val m_icache_entry = master(icache_interface(CoreConfig()))  // to icache
    //val m_icache_read_entry = master(icache_read_interface(CoreConfig()))  // to icache
    //val m_icache_write_entry = master(icache_write_interface(CoreConfig()))  // to icache
    val m_memory_write_interface = master(cpu_write_interface(CoreConfig()))  // to bus
    val m_memory_read_interface = master(cpu_read_interface(CoreConfig()))  // to bus
    val stall_for_ifu_r = out Bool()
    val stall_for_lsu_w = out Bool()
    val stall_for_lsu_r = out Bool()
    val m_memory_read_id = out UInt(2 bits) // 01: IFU, 10: LSU
  }
  //io.m_dcache_write_interface connect io.s_memory_write_interface
  //io.m_icache_entry connect io.s_icache_entry

  // todo with PMP and 优先级 with 三个slave接口//
  //io.m_memory_write_interface connect io.s_memory_write_interface
  /*
  io.m_memory_write_interface.we := io.s_memory_write_interface.we
  io.m_memory_write_interface.sel := io.s_memory_write_interface.sel
  io.m_memory_write_interface.waddr := io.s_memory_write_interface.waddr
  io.m_memory_write_interface.wdata := io.s_memory_write_interface.wdata
  io.stall_for_lsu_w := io.s_memory_write_interface.we && ~io.m_memory_write_interface.wvalid
  io.s_memory_write_interface.wvalid := io.s_memory_write_interface.we && io.m_memory_write_interface.wvalid
     */

  //io.m_memory_read_interface connect io.s_memory_read_interface
  //io.m_memory_read_interface connect io.s_icache_entry
  //io.s_icache_entry.data := io.m_memory_read_interface.rdata
  io.s_icache_entry.rdata := io.m_memory_read_interface.rdata
  io.s_memory_read_interface.rdata := io.m_memory_read_interface.rdata
  io.s_icache_entry.rvalid_ifu := io.m_memory_read_interface.rvalid_ifu
  io.s_icache_entry.rvalid_lsu := io.m_memory_read_interface.rvalid_lsu
  io.s_memory_read_interface.rvalid_lsu := io.m_memory_read_interface.rvalid_lsu
  io.s_memory_read_interface.rvalid_ifu := io.m_memory_read_interface.rvalid_ifu
  io.s_memory_write_interface.wvalid := io.m_memory_write_interface.wvalid
  io.m_memory_read_id := 0
  when(io.s_memory_write_interface.we === True){ // LSU比IFU优先级更高
    io.m_memory_write_interface.we := io.s_memory_write_interface.we
    io.m_memory_write_interface.sel := io.s_memory_write_interface.sel
    io.m_memory_write_interface.waddr := io.s_memory_write_interface.waddr
    io.m_memory_write_interface.wdata := io.s_memory_write_interface.wdata
    io.stall_for_lsu_w := False
    io.m_memory_read_interface.re := False
    io.m_memory_read_interface.raddr := io.s_memory_read_interface.raddr
    io.m_memory_read_interface.sel := io.s_memory_read_interface.sel
    //io.s_memory_read_interface.rdata := io.m_memory_read_interface.rdata
    io.stall_for_ifu_r := True
    io.stall_for_lsu_r := True
  } .elsewhen(io.s_memory_read_interface.re === True){  // LSU读
    io.m_memory_read_interface.re := io.s_memory_read_interface.re
    io.m_memory_read_interface.raddr := io.s_memory_read_interface.raddr
    io.m_memory_read_interface.sel := io.s_memory_read_interface.sel
    //io.s_memory_read_interface.rdata := io.m_memory_read_interface.rdata
    io.stall_for_ifu_r := True
    io.stall_for_lsu_r := False//~io.m_memory_read_interface.rvalid
    io.m_memory_write_interface.we := False
    io.m_memory_write_interface.sel := io.s_memory_write_interface.sel
    io.m_memory_write_interface.waddr := io.s_memory_write_interface.waddr
    io.m_memory_write_interface.wdata := io.s_memory_write_interface.wdata
    io.stall_for_lsu_w := True
    io.m_memory_read_id := U"10"
  } .otherwise{
    io.m_memory_read_interface.re := io.s_icache_entry.re
    io.m_memory_read_interface.raddr := io.s_icache_entry.raddr
    io.m_memory_read_interface.sel := U"1111"
    //io.s_icache_entry.rdata := io.m_memory_read_interface.rdata
    io.stall_for_ifu_r := False//~io.m_memory_read_interface.rvalid
    io.stall_for_lsu_r := True
    io.m_memory_write_interface.we := False
    io.m_memory_write_interface.sel := io.s_memory_write_interface.sel
    io.m_memory_write_interface.waddr := io.s_memory_write_interface.waddr
    io.m_memory_write_interface.wdata := io.s_memory_write_interface.wdata
    io.stall_for_lsu_w := True
    io.m_memory_read_id := U"01"
  }
  //io.stall_for_lsu_r := io.s_memory_read_interface.re && ~io.m_memory_read_interface.rvalid
  //io.stall_for_ifu_r := io.s_icache_entry.re && ~io.m_memory_read_interface.rvalid


  /*
  io.m_memory_read_interface.raddr := io.s_memory_read_interface.raddr
  io.m_memory_read_interface.re := io.s_memory_read_interface.re
  io.m_memory_read_interface.sel := io.s_memory_read_interface.sel
  io.s_memory_read_interface.rdata := io.m_memory_read_interface.rdata

  io.m_memory_write_interface := io.s_memory_write_interface
  */
  // todo with PMP //

  /*
  when(io.s_memory_read_interface.raddr >= InstStartAddr && io.s_memory_read_interface.raddr < (InstStartAddr+InstMemNum)){
    io.m_icache_read_entry connect io.s_memory_read_interface
    io.m_dcache_read_interface.re := False
    io.m_dcache_read_interface.sel := U"1111"
    io.m_dcache_read_interface.raddr := 0
  } .otherwise{
    //io.m_dcache_read_interface connect io.s_memory_read_interface
    io.m_dcache_read_interface.re := io.s_memory_read_interface.re
    io.m_dcache_read_interface.raddr := (io.s_memory_read_interface.raddr - DataStartAddr)
    io.s_memory_read_interface.rdata := io.m_dcache_read_interface.rdata
    io.m_dcache_read_interface.sel := io.s_memory_read_interface.sel
    io.m_icache_read_entry.re := False
    io.m_icache_read_entry.sel := U"1111"
    io.m_icache_read_entry.raddr := 0
  }

  // todo with PMP //
  when(io.s_memory_write_interface.waddr >= InstStartAddr && io.s_memory_write_interface.waddr < (InstStartAddr+InstMemNum)){
    io.m_icache_write_entry connect io.s_memory_write_interface
    io.m_dcache_write_interface.we := False
    io.m_dcache_write_interface.sel := U"1111"
    io.m_dcache_write_interface.waddr := 0
    io.m_dcache_write_interface.wdata := 0
  } .otherwise{
    io.m_dcache_write_interface.we := io.s_memory_write_interface.we
    io.m_dcache_write_interface.sel := io.s_memory_write_interface.sel
    io.m_dcache_write_interface.waddr := (io.s_memory_write_interface.waddr - DataStartAddr)
    io.m_dcache_write_interface.wdata := io.s_memory_write_interface.wdata
    io.m_icache_write_entry.we := False
    io.m_icache_write_entry.sel := U"1111"
    io.m_icache_write_entry.waddr := 0
    io.m_icache_write_entry.wdata := 0
  }
  */
}