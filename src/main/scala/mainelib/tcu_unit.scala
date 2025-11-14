package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class tcu_unit(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    // PA from IFU
    val ifu_mem_pa_if = slave(mem_if(cfg,cfg.InstAddrBus,cfg.InstBus))

    // PA from LSU
    val lsu_mem_pa_if = slave(mem_if(cfg,cfg.DataAddrBus,cfg.DataBus))

    // to ITCM
    val itcm_if = master(tcm_if(cfg,cfg.InstAddrBus,cfg.InstBus))

    // to DTCM
    val dtcm_if = master(tcm_if(cfg,cfg.DataAddrBus,cfg.DataBus))

    // to BIU
    val biu_mem_if = master(mem_if(cfg,cfg.BusAW,cfg.BusDW))
  }

  /**
   如果地址命中IROM/IRAM，则转到ITCM；DTCM同理；否则转到BUS；
  仲裁：如果IFU和LSU同时命中，一般IFU > LSU，LSU stall一拍
   **/

  io.ifu_mem_pa_if.mem_rdata := 0
  io.ifu_mem_pa_if.mem_ready := True
  io.ifu_mem_pa_if.mem_wvalid := False
  io.ifu_mem_pa_if.mem_rvalid := False
  io.lsu_mem_pa_if.mem_rdata := 0
  io.lsu_mem_pa_if.mem_ready := True
  io.lsu_mem_pa_if.mem_wvalid := False
  io.lsu_mem_pa_if.mem_rvalid := False


  object BUS_REGION extends SpinalEnum(SpinalEnumEncoding("dynamicEncoding", _*1)) {
    val BUS_NON, BUS_ITCM, BUS_DTCM, BUS_BIU = newElement()
  }
  val ifu_sel = BUS_REGION()
  val lsu_sel = BUS_REGION()
  ifu_sel := BUS_REGION.BUS_NON
  lsu_sel := BUS_REGION.BUS_NON

  when(!io.ifu_mem_pa_if.mem_cen){
    when(io.ifu_mem_pa_if.mem_addr >= cfg.InstMemStart && io.ifu_mem_pa_if.mem_addr <= cfg.InstMemEnd) {
      ifu_sel := BUS_REGION.BUS_ITCM
    } .elsewhen(io.ifu_mem_pa_if.mem_addr >= cfg.DataMemStart && io.ifu_mem_pa_if.mem_addr <= cfg.DataMemEnd) {
      ifu_sel := BUS_REGION.BUS_DTCM
    } .otherwise {
      ifu_sel := BUS_REGION.BUS_BIU
    }
  }

  when(!io.lsu_mem_pa_if.mem_cen){
    when(io.lsu_mem_pa_if.mem_addr >= cfg.InstMemStart && io.lsu_mem_pa_if.mem_addr <= cfg.InstMemEnd) {
      lsu_sel := BUS_REGION.BUS_ITCM
    } .elsewhen(io.lsu_mem_pa_if.mem_addr >= cfg.DataMemStart && io.lsu_mem_pa_if.mem_addr <= cfg.DataMemEnd) {
      lsu_sel := BUS_REGION.BUS_DTCM
    } .otherwise {
      lsu_sel := BUS_REGION.BUS_BIU
    }
  }

  val arbiter_hit = (ifu_sel === lsu_sel) && (ifu_sel =/= BUS_REGION.BUS_NON)
  val arbiter_hit_tog = Reg(Bool()) init(False)
  val ifu_sel_real = BUS_REGION()
  val lsu_sel_real = BUS_REGION()
  val ifu_valid = io.ifu_mem_pa_if.mem_wvalid || io.ifu_mem_pa_if.mem_rvalid
  val lsu_valid = io.lsu_mem_pa_if.mem_wvalid || io.lsu_mem_pa_if.mem_rvalid

  when(ifu_valid) {
    arbiter_hit_tog := arbiter_hit
  } .elsewhen(lsu_valid) {
    arbiter_hit_tog := False
  }

  when(arbiter_hit & ~arbiter_hit_tog) {
    ifu_sel_real := ifu_sel
    lsu_sel_real := BUS_REGION.BUS_NON
  } .elsewhen(arbiter_hit_tog) {
    ifu_sel_real := BUS_REGION.BUS_NON
    lsu_sel_real := lsu_sel
  } .otherwise{
    ifu_sel_real := ifu_sel
    lsu_sel_real := lsu_sel
  }

  when(ifu_sel_real === BUS_REGION.BUS_BIU) {
    io.biu_mem_if <> io.ifu_mem_pa_if
  } .elsewhen(lsu_sel_real === BUS_REGION.BUS_BIU) {
    io.biu_mem_if <> io.lsu_mem_pa_if
  } .otherwise {
    io.biu_mem_if.mem_cen := True
    io.biu_mem_if.mem_we := False
    io.biu_mem_if.mem_sel := 0
    io.biu_mem_if.mem_addr := 0
    io.biu_mem_if.mem_wdata := 0
  }

  val itcm_nowait_noerr = (~io.itcm_if.tcm_wait) && (~io.itcm_if.tcm_err)
  val itcm_rvalid = RegNext(itcm_nowait_noerr && ~io.itcm_if.tcm_wr)
  val itcm_rdata = RegNext(io.itcm_if.tcm_rdata)  // for timing
  when(ifu_sel_real === BUS_REGION.BUS_ITCM) {
    io.itcm_if.tcm_cs := ~io.ifu_mem_pa_if.mem_cen
    io.itcm_if.tcm_addr := (io.ifu_mem_pa_if.mem_addr >> cfg.IMemOffset).resized
    io.itcm_if.tcm_wr := io.ifu_mem_pa_if.mem_we
    io.itcm_if.tcm_wdata := io.ifu_mem_pa_if.mem_wdata
    io.itcm_if.tcm_bytewr := io.ifu_mem_pa_if.mem_sel
    io.itcm_if.tcm_master := 0
    io.itcm_if.tcm_priv := False
    io.ifu_mem_pa_if.mem_rdata := itcm_rdata
    io.ifu_mem_pa_if.mem_ready := itcm_nowait_noerr
    io.ifu_mem_pa_if.mem_wvalid := itcm_nowait_noerr && io.itcm_if.tcm_wr && io.itcm_if.tcm_cs
    io.ifu_mem_pa_if.mem_rvalid := itcm_nowait_noerr && (~io.itcm_if.tcm_wr) && itcm_rvalid && io.itcm_if.tcm_cs
  } .elsewhen(lsu_sel_real === BUS_REGION.BUS_ITCM) {
    io.itcm_if.tcm_cs := ~io.lsu_mem_pa_if.mem_cen
    io.itcm_if.tcm_addr := (io.lsu_mem_pa_if.mem_addr >> cfg.IMemOffset).resized
    io.itcm_if.tcm_wr := io.lsu_mem_pa_if.mem_we
    io.itcm_if.tcm_wdata := io.lsu_mem_pa_if.mem_wdata
    io.itcm_if.tcm_bytewr := io.lsu_mem_pa_if.mem_sel
    io.itcm_if.tcm_master := 0
    io.itcm_if.tcm_priv := False
    io.lsu_mem_pa_if.mem_rdata := itcm_rdata
    io.lsu_mem_pa_if.mem_ready := itcm_nowait_noerr
    io.lsu_mem_pa_if.mem_wvalid := itcm_nowait_noerr && io.itcm_if.tcm_wr && io.itcm_if.tcm_cs
    io.lsu_mem_pa_if.mem_rvalid := itcm_nowait_noerr && (~io.itcm_if.tcm_wr) && itcm_rvalid && io.itcm_if.tcm_cs
  } .otherwise {
    io.itcm_if.tcm_cs := False
    io.itcm_if.tcm_addr := 0
    io.itcm_if.tcm_wr := False
    io.itcm_if.tcm_wdata := 0
    io.itcm_if.tcm_bytewr := 0
    io.itcm_if.tcm_master := 0
    io.itcm_if.tcm_priv := False
  }

  val dtcm_nowait_noerr = (~io.dtcm_if.tcm_wait) && (~io.dtcm_if.tcm_err)
  val dtcm_rvalid = RegNext(dtcm_nowait_noerr && ~io.dtcm_if.tcm_wr)
  val dtcm_rdata = RegNext(io.dtcm_if.tcm_rdata)

  when(ifu_sel_real === BUS_REGION.BUS_DTCM) {
    io.dtcm_if.tcm_cs := ~io.ifu_mem_pa_if.mem_cen
    io.dtcm_if.tcm_addr := (io.ifu_mem_pa_if.mem_addr >> cfg.DMemOffset).resized
    io.dtcm_if.tcm_wr := io.ifu_mem_pa_if.mem_we
    io.dtcm_if.tcm_wdata := io.ifu_mem_pa_if.mem_wdata
    io.dtcm_if.tcm_bytewr := io.ifu_mem_pa_if.mem_sel
    io.dtcm_if.tcm_master := 0
    io.dtcm_if.tcm_priv := False
    io.ifu_mem_pa_if.mem_rdata := dtcm_rdata
    io.ifu_mem_pa_if.mem_ready := dtcm_nowait_noerr
    io.ifu_mem_pa_if.mem_wvalid := dtcm_nowait_noerr && io.dtcm_if.tcm_wr  && io.dtcm_if.tcm_cs
    io.ifu_mem_pa_if.mem_rvalid := dtcm_nowait_noerr && (~io.dtcm_if.tcm_wr) && dtcm_rvalid  && io.dtcm_if.tcm_cs
  } .elsewhen(lsu_sel_real === BUS_REGION.BUS_DTCM) {
    io.dtcm_if.tcm_cs := ~io.lsu_mem_pa_if.mem_cen
    io.dtcm_if.tcm_addr := (io.lsu_mem_pa_if.mem_addr >> cfg.DMemOffset).resized
    io.dtcm_if.tcm_wr := io.lsu_mem_pa_if.mem_we
    io.dtcm_if.tcm_wdata := io.lsu_mem_pa_if.mem_wdata
    io.dtcm_if.tcm_bytewr := io.lsu_mem_pa_if.mem_sel
    io.dtcm_if.tcm_master := 0
    io.dtcm_if.tcm_priv := False
    io.lsu_mem_pa_if.mem_rdata := dtcm_rdata
    io.lsu_mem_pa_if.mem_ready := dtcm_nowait_noerr
    io.lsu_mem_pa_if.mem_wvalid := dtcm_nowait_noerr && io.dtcm_if.tcm_wr  && io.dtcm_if.tcm_cs
    io.lsu_mem_pa_if.mem_rvalid := dtcm_nowait_noerr && (~io.dtcm_if.tcm_wr) && dtcm_rvalid  && io.dtcm_if.tcm_cs
  } .otherwise {
    io.dtcm_if.tcm_cs := False
    io.dtcm_if.tcm_addr := 0
    io.dtcm_if.tcm_wr := False
    io.dtcm_if.tcm_wdata := 0
    io.dtcm_if.tcm_bytewr := 0
    io.dtcm_if.tcm_master := 0
    io.dtcm_if.tcm_priv := False
  }





}
