package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class commit(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    // connect with wb
    val cmt_in_if = Vec.fill(cfg.issueWidth)(slave(commit_entry(cfg)))
    val cmt_out_if = Vec.fill(cfg.issueWidth)(master(commit_entry(cfg)))

    val in_flush = in Bool()
    val in_stall = in Bool()

    val commit_ready = out Bool()

    // to reg/mmu
    val cmt_mem_wif = master(cmt_memw_entry(cfg)) // to mmu
    val cmt_mem_id = out UInt(cfg.SCB_ID_WIDTH bits)
    val cmt_mem_wready = in Bool()  // from mmu
    val cmt_mem_wvalid = in Bool()  // from mmu
    val cmt_mem_werr = in Bool()  // from mmu

    val cmt_reg_wif = Vec.fill(cfg.issueWidth)(master(preg_wr_entry(cfg)))
    val cmt_csr_wif = Vec.fill(cfg.issueWidth)(master(commit_csr_entry(cfg)))
    val cmt_exc_csr_entry = master(exc_csr_entry(cfg))

    // connect with plic, clint
    val plic_int_entry = slave(int_entry(cfg)) // 外部中断源，from PLIC, sync-pulse
    val clint_int_entry = slave(int_entry(cfg))

    // to top
    val trace_if = Vec.fill(cfg.issueWidth)(master(trace_entry(cfg)))
    val out_flush = out Bool()
    val out_flush_id = out UInt(cfg.SCB_ID_WIDTH bits)
  }

  io.cmt_mem_wif.setAsReg()
  io.cmt_mem_id.setAsReg()
  io.cmt_reg_wif.setAsReg()
  io.cmt_csr_wif.setAsReg()
  io.trace_if.setAsReg()
  io.cmt_out_if.setAsReg().map(_.init(commit_entry(cfg).getZero))
  io.cmt_exc_csr_entry.setAsReg()

  io.cmt_mem_wif.mem_wen.init(False)
  io.cmt_reg_wif.foreach(_.reg_wten.init(False))
  io.cmt_csr_wif.foreach(_.reg_wten.init(False))
  io.trace_if.foreach(_.commit_info.commit_vld.init(False))
  io.cmt_exc_csr_entry.exc_req.init(False)

  val memwenSeq  = io.cmt_in_if.map(_.mem_wif.mem_wen)  // Seq[Bool]
  val memwenhit  = FFONE.first(memwenSeq)

  val flush_seq = Vec.fill(cfg.issueWidth)(trap_entry(cfg))
  val flush_hit = FFONE.first(flush_seq.map(_.trap_hit))

  io.out_flush := flush_hit.has // todo regout?
  io.out_flush_id := 0

  val cmt_run = memwenhit.has && io.cmt_in_if(memwenhit.idx).commit_vld//  && io.cmt_mem_wready

  val cmt_done = Vec.fill(cfg.issueWidth)(Bool())
  for (i <- 0 until cfg.issueWidth) {
    cmt_done(i) := True
  }
  when(!io.in_stall && !io.in_flush && cmt_run){
    when((flush_hit.has && (memwenhit.idx < flush_hit.idx)) || ~flush_hit.has) {
      io.cmt_mem_wif := io.cmt_in_if(memwenhit.idx).mem_wif
      io.cmt_mem_id := io.cmt_in_if(memwenhit.idx) .trans_id
      cmt_done(memwenhit.idx) := io.cmt_mem_wvalid
    } .otherwise {
      io.cmt_mem_wif.mem_wen := False
    }
  } .otherwise{
    io.cmt_mem_wif.mem_wen := False
  }

  io.commit_ready := cmt_done.andR
  //io.commit_ready := (~cmt_mem_wen) || io.cmt_mem_wvalid

  for (i <- 0 until cfg.issueWidth) {
    val next = commit_entry(cfg)
    next := io.cmt_in_if(i)
    when(!io.in_stall && !io.in_flush && io.cmt_in_if(i).commit_vld) {
      when((flush_hit.has && (i < flush_hit.idx)) || (~flush_hit.has)) {
        io.cmt_reg_wif(i).reg_wten := io.cmt_in_if(i).reg_wif.reg_wten
        io.cmt_reg_wif(i).preg_addr := io.cmt_in_if(i).reg_wif.preg_addr
        io.cmt_reg_wif(i).areg_addr := io.cmt_in_if(i).reg_wif.areg_addr
        io.cmt_reg_wif(i).reg_wdata := io.cmt_in_if(i).reg_wif.reg_wdata
        io.cmt_csr_wif(i).reg_wten := io.cmt_in_if(i).csr_wif.reg_wten
        io.cmt_csr_wif(i).reg_addr := io.cmt_in_if(i).csr_wif.reg_addr
        io.cmt_csr_wif(i).reg_wdata := io.cmt_in_if(i).csr_wif.reg_wdata
        next.commit_vld := io.cmt_in_if(i).commit_vld & cmt_done(i)
        //io.cmt_out_if(i).commit_vld := io.cmt_in_if(i).commit_vld & cmt_done(i) // 前递用
        //io.cmt_out_if(i) := io.cmt_in_if(i)
        //io.trace_if(i).commit_info := io.cmt_in_if(i)
        io.cmt_out_if(i) := next
        io.trace_if(i).commit_info := next
        io.cmt_exc_csr_entry.exc_req := False
      } .elsewhen(flush_hit.has && (i === flush_hit.idx)){
        // todo csr & trace_trap_info
        io.cmt_reg_wif(i).reg_wten := False
        io.cmt_csr_wif(i).reg_wten := False
        io.cmt_out_if(i).commit_vld := True
        io.trace_if(i).commit_info := io.cmt_in_if(i)
        io.trace_if(i).trap_info := flush_seq(i)
        io.out_flush_id := io.cmt_in_if(i).trans_id

        io.cmt_exc_csr_entry.exc_req := True
        io.cmt_exc_csr_entry.exc_pc := flush_seq(i).trap_pc
        io.cmt_exc_csr_entry.exc_cause := flush_seq(i).trap_cause
        io.cmt_exc_csr_entry.exc_val := flush_seq(i).trap_value
        io.cmt_exc_csr_entry.exc_status := flush_seq(i).trap_status


      } .otherwise{
        io.cmt_reg_wif(i).reg_wten := False
        io.cmt_csr_wif(i).reg_wten := False
        io.cmt_exc_csr_entry.exc_req := False

        io.trace_if(i).commit_info.commit_vld := False
        io.cmt_out_if(i).commit_vld := False
      }
    } .otherwise{
      io.cmt_reg_wif(i).reg_wten := False
      io.cmt_csr_wif(i).reg_wten := False
      io.cmt_exc_csr_entry.exc_req := False

      io.trace_if(i).commit_info.commit_vld := False
      io.cmt_out_if(i).commit_vld := False
    }

  }

  // 初始化flush seq
  flush_seq.assignFromBits(B(0,flush_seq.asBits.getBitsWidth bits))

  for (i <- 0 until cfg.issueWidth) {
    // todo flush_seq
    when(io.cmt_in_if(i).commit_vld) {
      when(io.cmt_in_if(i).instr_err) {
        flush_seq(i).trap_hit := True
        flush_seq(i).trap_pc := io.cmt_in_if(i).pc
        flush_seq(i).trap_cause := U(B(EXC_CAUSE.ILEGAL_PC)).resized
        flush_seq(i).trap_value := io.cmt_in_if(i).pc
        flush_seq(i).trap_status := 0 // todo
      }.elsewhen(io.cmt_in_if(i).mem_rif.mem_rerr) {
        flush_seq(i).trap_hit := True
        flush_seq(i).trap_pc := io.cmt_in_if(i).pc
        flush_seq(i).trap_cause := U(B(EXC_CAUSE.ILEGAL_ACCESS)).resized
        flush_seq(i).trap_value := io.cmt_in_if(i).mem_rif.mem_raddr
        flush_seq(i).trap_status := 0 // todo
      }.elsewhen(io.cmt_mem_werr && (i === memwenhit.idx)) {
        flush_seq(i).trap_hit := True
        flush_seq(i).trap_pc := io.cmt_in_if(i).pc
        flush_seq(i).trap_cause := U(B(EXC_CAUSE.ILEGAL_ACCESS)).resized
        flush_seq(i).trap_value := io.cmt_in_if(i).mem_wif.mem_waddr
        flush_seq(i).trap_status := 0 // todo
      }.elsewhen(io.cmt_in_if(i).dec_valid === False) {
        flush_seq(i).trap_hit := True
        flush_seq(i).trap_pc := io.cmt_in_if(i).pc
        flush_seq(i).trap_cause := U(B(EXC_CAUSE.ILEGAL_CODING)).resized
        flush_seq(i).trap_value := io.cmt_in_if(i).instr
        flush_seq(i).trap_status := 0 // todo
      }.elsewhen(io.cmt_in_if(i).pc(1 downto 0) =/= U"2'b0") {
        flush_seq(i).trap_hit := True
        flush_seq(i).trap_pc := io.cmt_in_if(i).pc
        flush_seq(i).trap_cause := U(B(EXC_CAUSE.NON_ALIGNED)).resized
        flush_seq(i).trap_value := io.cmt_in_if(i).pc
        flush_seq(i).trap_status := 0 // todo
      }.elsewhen(io.cmt_in_if(i).instr === EBREAK) {
        flush_seq(i).trap_hit := True
        flush_seq(i).trap_pc := io.cmt_in_if(i).pc
        flush_seq(i).trap_cause := U(B(EXC_CAUSE.EBREAK)).resized
        flush_seq(i).trap_value := 0 // todo
        flush_seq(i).trap_status := 0 // todo
      }.elsewhen(io.plic_int_entry.int_req) {
        flush_seq(i).trap_hit := True
        flush_seq(i).trap_pc := io.cmt_in_if(i).pc
        flush_seq(i).trap_cause := io.plic_int_entry.int_source
        flush_seq(i).trap_value := 0
        flush_seq(i).trap_status := 0 // todo
      }.elsewhen(io.clint_int_entry.int_req) {
        flush_seq(i).trap_hit := True
        flush_seq(i).trap_pc := io.cmt_in_if(i).pc
        flush_seq(i).trap_cause := io.clint_int_entry.int_source
        flush_seq(i).trap_value := 0
        flush_seq(i).trap_status := 0 // todo
      }
    }

  }



}
