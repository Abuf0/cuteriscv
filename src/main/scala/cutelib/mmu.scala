package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._
import BundleImplicit._   // connect with master & slave


case class mmu() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val s_memory_write_interface = slave(dcache_write_interface(CoreConfig()))  // from commit
    val s_memory_read_interface = slave(dcache_read_interface(CoreConfig()))  // from lsu_unit
    val m_dcache_write_interface = master(dcache_write_interface(CoreConfig()))  // to dcache
    val m_dcache_read_interface = master(dcache_read_interface(CoreConfig()))  // to dcache
    val s_icache_entry = slave(icache_interface(CoreConfig()))  // from instr
    val m_icache_entry = master(icache_interface(CoreConfig()))  // to icache
    val m_icache_read_entry = master(icache_read_interface(CoreConfig()))  // to icache
    val m_icache_write_entry = master(icache_write_interface(CoreConfig()))  // to icache

  }
  //io.m_dcache_write_interface connect io.s_memory_write_interface
  io.m_icache_entry connect io.s_icache_entry

  // todo with PMP //
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

}