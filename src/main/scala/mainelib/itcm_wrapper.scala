package mainelib
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import Plugin._

import java.io.File

case class itcm_wrapper(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    // From ITCM
    val itcm_if = slave(tcm_if(cfg,cfg.InstAddrBus,cfg.InstBus))

    // From TCM-AHB
    val tcm_ahb_if = slave(AhbLite3(AhbLite3Config(cfg.BusAW,cfg.BusDW)))
  }

  val ram_bank = cfg.IMemDataWidth/cfg.ByteWidth
  val rom_deepth = cfg.IROMSize/ram_bank
  val IRAMSize = cfg.InstMemNum - cfg.IROMSize
  val ram_deepth = IRAMSize/ram_bank

  val bytes       = loadPat4B("./src/main/scala/mainelib/inst.pat")                   // 整个镜像字节流
  // 把连续字节按“word 交织”分发到各 bank（bank = a % ram_bank, idx = a / ram_bank）
  val banksInit = Array.fill(ram_bank)(Array.fill(rom_deepth)(BigInt(0)))
  for (a <- bytes.indices) {
    val bank = a % ram_bank
    val idx  = a / ram_bank
    if (idx < rom_deepth) banksInit(bank)(idx) = BigInt(bytes(a) & 0xFF)
  }

  // 建 ROM：每个 bank 存一个字节
  val ROM = List.tabulate(ram_bank){ i =>
    val m = Mem(Bits(8 bits), wordCount = rom_deepth)
    m.initBigInt(banksInit(i))       // 利用初始化表灌入
    m
  }


  //val ROM = List.fill(ram_bank)(Mem(UInt(cfg.ByteWidth bits),rom_deepth))
  val RAM = List.fill(ram_bank)(Mem(UInt(cfg.ByteWidth bits),ram_deepth))

  val rom_hit = io.itcm_if.tcm_cs && (io.itcm_if.tcm_addr >= cfg.InstMemStart) && (io.itcm_if.tcm_addr <= cfg.InstMemStart + cfg.IROMSize - 1 )
  val ram_hit = io.itcm_if.tcm_cs && (io.itcm_if.tcm_addr >= cfg.InstMemStart + cfg.IROMSize) && (io.itcm_if.tcm_addr <= cfg.InstMemEnd)

  val rom_addr = io.itcm_if.tcm_addr.resize(log2Up(rom_deepth))
  val ram_addr = io.itcm_if.tcm_addr.resize(log2Up(ram_deepth))

  io.itcm_if.tcm_wait := False
  io.itcm_if.tcm_err := False


  for (i <- 0 until ram_bank) {
    val lsb = cfg.ByteWidth * i
    val msb = lsb + cfg.ByteWidth - 1

    when(rom_hit){
      io.itcm_if.tcm_rdata(msb downto lsb) := ROM(i)(rom_addr).asUInt
      when(io.itcm_if.tcm_wr) {
        io.itcm_if.tcm_err := True
      }
    } .otherwise {
      io.itcm_if.tcm_rdata(msb downto lsb) := RAM(i)(ram_addr)
      RAM(i).write(
        address = ram_addr,
        data = io.itcm_if.tcm_wdata(msb downto lsb),
        enable = io.itcm_if.tcm_wr && io.itcm_if.tcm_bytewr(i) && ram_hit
      )
    }
  }


}



