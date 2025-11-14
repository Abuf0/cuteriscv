package mainelib
import spinal.core._
import spinal.lib._
import RISCV_ISA._
import BundleImplicit._
import spinal.lib.bus.amba3.ahblite._

class mainecore(cfg: CoreConfig) extends Component{
  val io = new Bundle {
    // to ITCM
    val itcm_if = master(tcm_if(cfg,cfg.InstAddrBus,cfg.InstBus))

    // to DTCM
    val dtcm_if = master(tcm_if(cfg,cfg.DataAddrBus,cfg.DataBus))

    // to BIU
    val biu_if = master(mem_if(cfg,cfg.BusAW,cfg.BusDW))

    // to COSIM
    val cosim_dpi_if = Vec.fill(cfg.issueWidth)(master(trace_entry(cfg)))

  }
  /********************************* init **********************************/

  val pipe_stall = Vec(Bool(), cfg.PipeDepth)
  val pipe_flush = Vec(Bool(), cfg.PipeDepth)

  /** IF1_PC ==> IF2_TM **/
  val if1_pc = new pc_gen(cfg)
  val if2_tm = new if2_tm(cfg)

  /** IF2_TM ==> IF3_FM **/
  val pipe_2t3 = new if2_3(cfg)
  val (o_2t3, ov_2t3) = Pipeline(HardType(new Payload2_3(cfg)))(pipe_2t3.i, pipe_2t3.iv, pipe_2t3.st, pipe_2t3.fl) // 内部已调用 build()

  /** IF3_FM ==> ID **/
  val if3_instr_realign = new instr_realign(cfg)
  val if3_instr_queue =  new instr_queue(cfg)

  /** ID ==> ISSUE **/
  val id_dec = new decoder(cfg)
  val pipe_id2issue = new id2issue(cfg)
  val (o_id2issue, ov_id2issue) = Pipeline(HardType(new Payload_id2issue(cfg)))(pipe_id2issue.i, pipe_id2issue.iv, pipe_id2issue.st, pipe_id2issue.fl) // 内部已调用 build()

  /** ISSUE =(SCB)=> EXE **/
  val issue_stage = new issue_stage(cfg)

  /** EXE ==> WB **/
  val alu_base = 0
  val mul_base = alu_base + FU_ID.enums.ALU.size
  val div_base = mul_base + FU_ID.enums.MUL.size
  val bju_base = div_base + FU_ID.enums.MUL.size
  val lsu_base = bju_base + FU_ID.enums.MUL.size
  val csr_base = lsu_base + FU_ID.enums.MUL.size
  val fpu_base = csr_base + FU_ID.enums.MUL.size
  val nop_base = fpu_base + FU_ID.enums.MUL.size

  // bju arbiter
  val bju_flush_mispredict = flush_mispredict_entry(cfg)

  // ALU_UNIT
  val alu_unit: Seq[alu_unit] = Seq.tabulate(FU_ID.enums.ALU.size){ i =>
    val u = new alu_unit(cfg, id = alu_base + i, latency = 1)
    u.setDefinitionName(s"alu_unit_$i")
    u
  }


  // MUL_UNIT
  val mul_unit: Seq[mul_unit] = Seq.tabulate(FU_ID.enums.MUL.size){ i =>
    val u = new mul_unit(cfg, id = mul_base + i, latency = 1)
    u.setDefinitionName(s"mul_unit_$i")
    u
  }

  // DIV_UNIT
  val div_unit: Seq[div_unit] = Seq.tabulate(FU_ID.enums.DIV.size){ i =>
    val u = new div_unit(cfg, id = div_base + i, latency = 1)
    u.setDefinitionName(s"div_unit_$i")
    u
  }

  // BJU_UNIT
  val bju_unit: Seq[bju_unit] = Seq.tabulate(FU_ID.enums.BJU.size){ i =>
    val u = new bju_unit(cfg, id = bju_base + i, latency = 1)
    u.setDefinitionName(s"bju_unit_$i")
    u
  }

  // LSU_UNIT
  val lsu_unit: Seq[lsu_unit] = Seq.tabulate(FU_ID.enums.LSU.size){ i =>
    val u = new lsu_unit(cfg, id = lsu_base + i, latency = 1)
    u.setDefinitionName(s"lsu_unit_$i")
    u
  }
  val store_buffer = new store_buffer(cfg,FU_ID.enums.LSU.size)

  // CSR_UNIT
  val csr_unit: Seq[csr_unit] = Seq.tabulate(FU_ID.enums.CSR.size){ i =>
    val u = new csr_unit(cfg, id = csr_base + i, latency = 1)
    u.setDefinitionName(s"csr_unit_$i")
    u
  }

  if(cfg.withFpu) {
    // FPU_UNIT
    val fpu_unit: Seq[fpu_unit] = Seq.tabulate(FU_ID.enums.FPU.size){ i =>
      val u = new fpu_unit(cfg, id = csr_base + i, latency = 1)
      u.setDefinitionName(s"fpu_unit_$i")
      u
    }
  }

  // NOP_UNIT
  val nop_unit: Seq[nop_unit] = Seq.tabulate(FU_ID.enums.NOP.size){ i =>
    val u = new nop_unit(cfg, id = nop_base + i, latency = 1)
    u.setDefinitionName(s"nop_unit_$i")
    u
  }

  val pipe_ex2wb = new ex2wb(cfg)
  val (o_ex2wb, ov_ex2wb) = Pipeline(HardType(new Payload_ex2wb(cfg)))(pipe_ex2wb.i, pipe_ex2wb.iv, pipe_ex2wb.st, pipe_ex2wb.fl) // 内部已调用 build()

  /** WB ==> COMMIT **/
  val wb = new wb(cfg)

  /** COMMIT **/
  val commit = new commit(cfg)

  val prf_file = new PRF_FILE(cfg)

  val csr_file = new CSR_FILE(cfg)

  // FLUSH && STALL CTRL
  val flush_stall_ctrl = new flush_stall_ctrl(cfg)

  // MMU && TCU
  val mmu = new mmu_unit(cfg)
  val tcu = new tcu_unit(cfg)


  /********************************* connection **********************************/

  /** IF1_PC ==> IF2_TM **/
  if1_pc.io.in_flush := pipe_flush(PP_STATE.IF1_PC.position) // tie 0
  if1_pc.io.in_stall := pipe_stall(PP_STATE.IF1_PC.position)
  if1_pc.io.pc_fl <> if2_tm.io.pc_fl
  if1_pc.io.predict_fl <> if2_tm.io.predict_fl
  if1_pc.io.trap_fl.valid := commit.io.out_flush
  if1_pc.io.trap_fl.payload := csr_file.io.CSR_REGFILE(CSR.MTVEC(cfg.debug_en))
  if1_pc.io.bj_cor_fl.valid := bju_flush_mispredict.hit
  if1_pc.io.bj_cor_fl.payload := bju_flush_mispredict.target
  // todo mmu : io.mmu_st
  if1_pc.io.initvtor := cfg.INITVTOR  // todo initvtor

  /** IF2_TM ==> IF3_FM **/
  if2_tm.io.resolved_if <> bju_unit(0).io.bju_branch_predict  // todo arbiter
  pipe_2t3.i.predict_branch_if := if2_tm.io.predict_branch_if
  pipe_2t3.i.pc_fl := if2_tm.io.pc_fl
  pipe_2t3.st := pipe_stall(PP_STATE.IF2_TM.position)
  pipe_2t3.fl := pipe_flush(PP_STATE.IF2_TM.position)
  pipe_2t3.iv := if1_pc.io.pc_fl.valid
  pipe_2t3.o := o_2t3
  pipe_2t3.ov := ov_2t3

  /** IF3_FM ==> ID **/
  if3_instr_realign.io.pc_fl <> pipe_2t3.o.pc_fl
  if3_instr_queue.io.instr_err_fl0 <> if3_instr_realign.io.instr_err_fl0
  if3_instr_queue.io.instr_realign_fl0 <> if3_instr_realign.io.instr_realign_fl0
  if3_instr_queue.io.pc_realign_fl0 <> if3_instr_realign.io.pc_realign_fl0
  if3_instr_queue.io.predict_branch_if <> pipe_2t3.o.predict_branch_if
  if3_instr_queue.io.in_stall := pipe_stall(PP_STATE.IF3_FM.position)
  if3_instr_queue.io.in_flush := pipe_flush(PP_STATE.IF3_FM.position)

  /** ID ==> ISSUE **/
  id_dec.io.instr_fl <> if3_instr_queue.io.instr_2id_fl
  id_dec.io.instr_err := if3_instr_queue.io.instr_2id_err
  pipe_id2issue.i.pc_fl := if3_instr_queue.io.pc_2id_fl
  pipe_id2issue.i.instr_fl := if3_instr_queue.io.instr_2id_fl
  pipe_id2issue.i.predict_if := if3_instr_queue.io.predict_2id_if
  pipe_id2issue.i.dec_if := id_dec.io.dec_if
  pipe_id2issue.st := pipe_stall(PP_STATE.ID.position)
  pipe_id2issue.fl := pipe_flush(PP_STATE.ID.position)
  pipe_id2issue.iv := if3_instr_queue.io.instr_2id_fl.map(_.valid).asBits.orR
  pipe_id2issue.o := o_id2issue
  pipe_id2issue.ov := ov_id2issue

  /** ISSUE =(SCB)=> EXE **/
  issue_stage.io.instr_fl := pipe_id2issue.o.instr_fl
  issue_stage.io.pc_fl := pipe_id2issue.o.pc_fl
  issue_stage.io.predict_if := pipe_id2issue.o.predict_if
  issue_stage.io.dec_if := pipe_id2issue.o.dec_if
  issue_stage.io.in_stall := pipe_stall(PP_STATE.ISSUE.position)
  issue_stage.io.in_flush_g := pipe_flush(PP_STATE.ISSUE.position)
  issue_stage.io.in_flush_g_id := flush_stall_ctrl.io.out_flush_id_global
  issue_stage.io.in_flush_l := flush_stall_ctrl.io.out_flush_hit_local
  issue_stage.io.in_flush_l_id := flush_stall_ctrl.io.out_flush_id_local
  val bju_hit_d1 = RegNext(bju_unit(0).io.bju_unit_hit) init(False)
  issue_stage.io.bju_hit := bju_unit(0).io.bju_unit_hit && ~bju_hit_d1
  issue_stage.io.bju_mis_flush := flush_stall_ctrl.io.out_flush_hit_local

  issue_stage.io.rs1_if <> prf_file.io.rs1_if
  issue_stage.io.rs2_if <> prf_file.io.rs2_if
  // todo fowarding
  issue_stage.io.ex_fwd_if.assignFromBits(B(0,issue_stage.io.ex_fwd_if.getBitsWidth bits))
  //issue_stage.io.wb_fwd_if.assignFromBits(B(0,issue_stage.io.wb_fwd_if.getBitsWidth bits))
  //issue_stage.io.cmt_if.assignFromBits(B(0,issue_stage.io.cmt_if.getBitsWidth bits))
  for (i <- 0 until cfg.issueWidth) {
    // cmt forwarding
    issue_stage.io.cmt_if(i).cmt_valid := commit.io.cmt_out_if(i).commit_vld
    issue_stage.io.cmt_if(i).cmt_id := commit.io.cmt_out_if(i).trans_id
    issue_stage.io.cmt_if(i).reg_commitin.valid := commit.io.cmt_out_if(i).commit_vld
    issue_stage.io.cmt_if(i).reg_commitin.hasDest := commit.io.cmt_out_if(i).reg_wif.reg_wten
    issue_stage.io.cmt_if(i).reg_commitin.rd_arch := commit.io.cmt_out_if(i).reg_wif.areg_addr
    issue_stage.io.cmt_if(i).reg_commitin.old_p := commit.io.cmt_out_if(i).reg_wif.preg_addr
    issue_stage.io.cmt_if(i).reg_commitin.rd_data := commit.io.cmt_out_if(i).reg_wif.reg_wdata
    issue_stage.io.cmt_if(i).csr_commitin.valid := commit.io.cmt_out_if(i).commit_vld
    issue_stage.io.cmt_if(i).csr_commitin.hasDest := commit.io.cmt_out_if(i).csr_wif.reg_wten
    issue_stage.io.cmt_if(i).csr_commitin.rd_arch := commit.io.cmt_out_if(i).csr_wif.reg_addr
    issue_stage.io.cmt_if(i).csr_commitin.old_p := 0  // dont care
    issue_stage.io.cmt_if(i).csr_commitin.rd_data := commit.io.cmt_out_if(i).csr_wif.reg_wdata

    // wb forwarding
    issue_stage.io.wb_fwd_if(i).cmt_valid := wb.io.cmt_if(i).commit_vld
    issue_stage.io.wb_fwd_if(i).cmt_id := wb.io.cmt_if(i).trans_id
    issue_stage.io.wb_fwd_if(i).reg_commitin.valid := wb.io.cmt_if(i).commit_vld
    issue_stage.io.wb_fwd_if(i).reg_commitin.hasDest := wb.io.cmt_if(i).reg_wif.reg_wten
    issue_stage.io.wb_fwd_if(i).reg_commitin.rd_arch := wb.io.cmt_if(i).reg_wif.areg_addr
    issue_stage.io.wb_fwd_if(i).reg_commitin.old_p := wb.io.cmt_if(i).reg_wif.preg_addr
    issue_stage.io.wb_fwd_if(i).reg_commitin.rd_data := wb.io.cmt_if(i).reg_wif.reg_wdata
    issue_stage.io.wb_fwd_if(i).csr_commitin.valid := wb.io.cmt_if(i).commit_vld
    issue_stage.io.wb_fwd_if(i).csr_commitin.hasDest := wb.io.cmt_if(i).csr_wif.reg_wten
    issue_stage.io.wb_fwd_if(i).csr_commitin.rd_arch := wb.io.cmt_if(i).csr_wif.reg_addr
    issue_stage.io.wb_fwd_if(i).csr_commitin.old_p := 0  // dont care
    issue_stage.io.wb_fwd_if(i).csr_commitin.rd_data := wb.io.cmt_if(i).csr_wif.reg_wdata

  }


  /** EXE ==> WB **/

  // ALU_UNIT
  for (i <- 0 until FU_ID.enums.ALU.size){
    alu_unit(i).io.ex_in_entry <> issue_stage.io.issue_out_if
    issue_stage.io.alu_busy(i) := alu_unit(i).io.unit_busy
    alu_unit(i).io.flush_mispredict_in := bju_flush_mispredict

  }

  // MUL_UNIT
  for (i <- 0 until FU_ID.enums.MUL.size){
    mul_unit(i).io.ex_in_entry <> issue_stage.io.issue_out_if
    issue_stage.io.mul_busy(i) := mul_unit(i).io.unit_busy
    mul_unit(i).io.flush_mispredict_in := bju_flush_mispredict

  }

  // DIV_UNIT
  for (i <- 0 until FU_ID.enums.DIV.size){
    div_unit(i).io.ex_in_entry <> issue_stage.io.issue_out_if
    issue_stage.io.div_busy(i) := div_unit(i).io.unit_busy
    div_unit(i).io.flush_mispredict_in := bju_flush_mispredict

  }

  // BJU_UNIT
  for (i <- 0 until FU_ID.enums.BJU.size){
    bju_unit(i).io.ex_in_entry <> issue_stage.io.issue_out_if
    bju_unit(i).io.ex_branch_predict <> issue_stage.io.issue_out_if(i).predict_info
    bju_unit(i).io.flush_mispredict_in := bju_flush_mispredict
    issue_stage.io.bju_busy(i) := bju_unit(i).io.unit_busy
  }

  //bju_flush_mispredict := bju_unit(0).io.flush_mispredict_out
  bju_flush_mispredict := pipe_ex2wb.o.flush_mispredict

  for (i <- 0 until FU_ID.enums.BJU.size) {
    if (i != 0) {
      val in_trans_id = bju_unit(i - 1).io.flush_mispredict_out.trans_id
      val out_trans_id = bju_unit(i).io.flush_mispredict_out.trans_id
      when(bju_unit(i).io.flush_mispredict_out.hit) {
        when(in_trans_id(cfg.SCB_ID_WIDTH - 1) === out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) >= out_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0))) {
          bju_flush_mispredict := bju_unit(i).io.flush_mispredict_out
        }.elsewhen(in_trans_id(cfg.SCB_ID_WIDTH - 1) =/= out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) < out_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0))) {
          bju_flush_mispredict := bju_unit(i).io.flush_mispredict_out
        }
      }
    }
  }

  // LSU_UNIT
  store_buffer.io.store_ex_if <> lsu_unit(0).io.store_if
  for (i <- 0 until FU_ID.enums.LSU.size){
    lsu_unit(i).io.ex_in_entry <> issue_stage.io.issue_out_if
    issue_stage.io.lsu_busy(i) := lsu_unit(i).io.unit_busy
    issue_stage.io.lsu_vld(i) := lsu_unit(i).io.load_vld
    store_buffer.io.toload_need(i) := lsu_unit(i).io.toload_need
    store_buffer.io.toload_addr(i) := lsu_unit(i).io.toload_addr
    store_buffer.io.toload_id(i) := lsu_unit(i).io.toload_id
    lsu_unit(i).io.toload_hit := store_buffer.io.toload_hit(i)
    lsu_unit(i).io.toload_vld := store_buffer.io.toload_vld(i)
    lsu_unit(i).io.toload_data := store_buffer.io.toload_data(i)
    lsu_unit(i).io.flush_mispredict_in := bju_flush_mispredict
    // todo : lsu_unit().io.mem_read_interface
    when(lsu_unit(i).io.store_if.store_wb_en){
      store_buffer.io.store_ex_if <> lsu_unit(i).io.store_if
    }
  }

  store_buffer.io.in_flush_g := pipe_flush(PP_STATE.EXE.position)
  store_buffer.io.in_flush_l := flush_stall_ctrl.io.out_flush_hit_local
  store_buffer.io.in_flush_l_id := flush_stall_ctrl.io.out_flush_id_local
  store_buffer.io.cmt_trans_id := commit.io.cmt_out_if(0).trans_id  // dont care
  store_buffer.io.mem_cmt_trans_id := commit.io.cmt_mem_id
  store_buffer.io.mem_cmt_vld := mmu.io.lsu_mem_va_if.mem_wvalid  // memory commit write done
  issue_stage.io.store_busy := store_buffer.io.store_buffer_busy
  // 并发条件：两路 load 命中且 bank 不同 → 真并发；
  // 若 bank 冲突 → 选 1 条发，另 1 条在 LQ/RS 等；
  // 若命中不同 way/同 bank → 仍冲突，因为每 bank 每拍只读 1 次数据阵列（Tag 可并发）。



  // CSR_UNIT
  for (i <- 0 until FU_ID.enums.CSR.size){
    csr_unit(i).io.ex_in_entry <> issue_stage.io.issue_out_if
    issue_stage.io.csr_busy(i) := csr_unit(i).io.unit_busy
    csr_unit(i).io.flush_mispredict_in := bju_flush_mispredict

  }


  // FPU_UNIT
  for (i <- 0 until FU_ID.enums.FPU.size){
    issue_stage.io.fpu_busy(i) := False
  }
  if(cfg.withFpu) {
    // FPU_UNIT
    //for (i <- 0 until FU_ID.enums.FPU.size){
      //fpu_unit(i).io.ex_in_entry <> issue_stage.io.issue_out_if
      //issue_stage.io.csr_busy(i) := fpu_unit(i).io.unit_busy
    //}
  }

  // NOP_UNIT
  for (i <- 0 until FU_ID.enums.NOP.size){
    nop_unit(i).io.ex_in_entry <> issue_stage.io.issue_out_if
    issue_stage.io.nop_busy(i) := nop_unit(i).io.unit_busy
    nop_unit(i).io.flush_mispredict_in := bju_flush_mispredict

  }

  pipe_ex2wb.i.alu_ex_out := Vec(alu_unit.map(_.io.ex_out_entry))
  pipe_ex2wb.i.mul_ex_out := Vec(mul_unit.map(_.io.ex_out_entry))
  pipe_ex2wb.i.div_ex_out := Vec(div_unit.map(_.io.ex_out_entry))
  pipe_ex2wb.i.bju_ex_out := Vec(bju_unit.map(_.io.ex_out_entry))
  pipe_ex2wb.i.bju_ex_branch_predict := Vec(bju_unit.map(_.io.bju_branch_predict))
  pipe_ex2wb.i.flush_mispredict := bju_unit(0).io.flush_mispredict_out
  pipe_ex2wb.i.lsu_ex_out := Vec(lsu_unit.map(_.io.ex_out_entry))
  pipe_ex2wb.i.lsu_ex_store_if := Vec(lsu_unit.map(_.io.store_if))
  pipe_ex2wb.i.lsu_ex_load_if := Vec(lsu_unit.map(_.io.load_if))
  pipe_ex2wb.i.csr_ex_out := Vec(csr_unit.map(_.io.ex_out_entry))
  //if(cfg.withFpu){
  //  pipe_ex2wb.i.fpu_ex_out := Vec(fpu_unit.map(_.io.ex_out_entry))
  //}
  pipe_ex2wb.i.nop_ex_out := Vec(nop_unit.map(_.io.ex_out_entry))

  pipe_ex2wb.st := pipe_stall(PP_STATE.EXE.position)
  pipe_ex2wb.fl := pipe_flush(PP_STATE.EXE.position)
  pipe_ex2wb.iv := issue_stage.io.issue_out_if.map(_.valid).asBits.orR
  pipe_ex2wb.o := o_ex2wb
  pipe_ex2wb.ov := ov_ex2wb

  /** WB ==> COMMIT **/
  wb.io.in_stall := pipe_stall(PP_STATE.WB.position)
  wb.io.in_flush_g := pipe_flush(PP_STATE.WB.position)
  wb.io.in_flush_l := flush_stall_ctrl.io.out_flush_hit_local
  wb.io.in_flush_g_id := flush_stall_ctrl.io.out_flush_id_global
  wb.io.in_flush_l_id := flush_stall_ctrl.io.out_flush_id_local
  wb.io.pre_cmt_info.ex2wb_payload := pipe_ex2wb.o
  // todo : wb.io.mem_rdata :=

  /** COMMIT **/
  commit.io.in_stall := pipe_stall(PP_STATE.COMMIT.position)
  commit.io.in_flush := pipe_flush(PP_STATE.COMMIT.position)
  commit.io.cmt_in_if := wb.io.cmt_if
  wb.io.cmt_back_if := commit.io.cmt_out_if
  store_buffer.io.mem_cmt_if <> commit.io.cmt_mem_wif

  commit.io.plic_int_entry.int_req := False // todo : plict/clint
  commit.io.clint_int_entry.int_req := False

  // todo : mmu + i/dtcm + i/dcache + bus

  commit.io.cmt_reg_wif <> prf_file.io.prf_wif

  commit.io.cmt_csr_wif <> csr_file.io.csr_wif
  commit.io.cmt_exc_csr_entry <> csr_file.io.cmt_exc_csr_entry


  // FLUSH && STALL CTRL
  pipe_flush := flush_stall_ctrl.io.out_flush
  pipe_stall := flush_stall_ctrl.io.out_stall
  flush_stall_ctrl.io.branch_predict_hit := if2_tm.io.predict_fl.valid
  flush_stall_ctrl.io.bju_mispredict := bju_flush_mispredict.hit
  flush_stall_ctrl.io.bju_mispredict_id := bju_flush_mispredict.trans_id
  flush_stall_ctrl.io.trap_hit := commit.io.out_flush
  flush_stall_ctrl.io.trap_id := commit.io.out_flush_id
  flush_stall_ctrl.io.instr_queue_full := if3_instr_queue.io.queue_full_stall
  flush_stall_ctrl.io.instr_queue_empty := if3_instr_queue.io.queue_empty_stall
  flush_stall_ctrl.io.scb_buff_full := issue_stage.io.buffer_full
  flush_stall_ctrl.io.scb_buff_empty := issue_stage.io.buffer_empry

  // MMU && TCU
  if1_pc.io.mmu_st.ready := mmu.io.ifu_mem_va_if.mem_ready
  mmu.io.mmu_en := False
  mmu.io.pmp_en := False
  mmu.io.ifu_mem_va_if.mem_we := False
  mmu.io.ifu_mem_va_if.mem_cen := !if2_tm.io.tm_fl.valid
  mmu.io.ifu_mem_va_if.mem_addr := if2_tm.io.tm_fl.payload
  mmu.io.ifu_mem_va_if.mem_sel.assignFromBits(B(1,cfg.IMemSelBus bits))
  mmu.io.ifu_mem_va_if.mem_wdata := 0
  if3_instr_realign.io.instr_fl.valid := mmu.io.ifu_mem_va_if.mem_rvalid
  if3_instr_realign.io.instr_fl.payload := mmu.io.ifu_mem_va_if.mem_rdata
  if3_instr_realign.io.instr_err := mmu.io.ifu_rfault

  // 简单多LSU仲裁,如果RAM多bank可读并且访问地址落在RAM中，可以考虑同时多LSU读
  val lsu_read_seq = lsu_unit.map(_.io.mem_read_interface.re)
  val lsu_read_hit = FFONE.first(lsu_read_seq)
  val lsu_id = lsu_read_hit.idx
  val lsu_arbit_r = ~commit.io.cmt_mem_wif.mem_wen  // todo arbiter
  for (i <- 0 until FU_ID.enums.LSU.size) {
    when(lsu_read_hit.has && (i === lsu_id) && lsu_arbit_r) {
      mmu.io.lsu_mem_va_if.mem_we := False
      mmu.io.lsu_mem_va_if.mem_addr := lsu_unit(i).io.mem_read_interface.raddr
      mmu.io.lsu_mem_va_if.mem_sel := lsu_unit(i).io.mem_read_interface.sel
      lsu_unit(i).io.mem_read_interface.rvalid := mmu.io.lsu_mem_va_if.mem_rvalid
      lsu_unit(i).io.mem_read_interface.rdata := mmu.io.lsu_mem_va_if.mem_rdata
      commit.io.cmt_mem_wready := False // LOAD优先于STORE
    } .otherwise {
      mmu.io.lsu_mem_va_if.mem_we := commit.io.cmt_mem_wif.mem_wen
      mmu.io.lsu_mem_va_if.mem_addr := commit.io.cmt_mem_wif.mem_waddr
      mmu.io.lsu_mem_va_if.mem_sel := commit.io.cmt_mem_wif.mem_sel
      lsu_unit(i).io.mem_read_interface.rvalid := False
      lsu_unit(i).io.mem_read_interface.rdata := mmu.io.lsu_mem_va_if.mem_rdata
      commit.io.cmt_mem_wready := mmu.io.lsu_mem_va_if.mem_ready
    }
  }

  mmu.io.lsu_mem_va_if.mem_cen := !(lsu_read_hit.has || commit.io.cmt_mem_wif.mem_wen)
  mmu.io.lsu_mem_va_if.mem_wdata := commit.io.cmt_mem_wif.mem_wdata

  wb.io.mem_rdata := mmu.io.lsu_mem_va_if.mem_rdata
  wb.io.mem_rvalid := mmu.io.lsu_mem_va_if.mem_rvalid
  wb.io.mem_rerr := mmu.io.lsu_rfault
  commit.io.cmt_mem_werr := mmu.io.lsu_wfault
  commit.io.cmt_mem_wvalid := mmu.io.lsu_mem_va_if.mem_wvalid

  wb.io.commit_ready := commit.io.commit_ready

  mmu.io.ifu_mem_pa_if <> tcu.io.ifu_mem_pa_if
  mmu.io.lsu_mem_pa_if <> tcu.io.lsu_mem_pa_if

  io.itcm_if <> tcu.io.itcm_if
  io.dtcm_if <> tcu.io.dtcm_if
  io.biu_if <> tcu.io.biu_mem_if

  io.cosim_dpi_if := commit.io.trace_if

  // todo clint/plic
  commit.io.clint_int_entry.int_source := 0
  commit.io.plic_int_entry.int_source := 0
}