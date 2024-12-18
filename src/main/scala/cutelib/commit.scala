package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class commit() extends Component with Global_parameter with Interface_MS{
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val ex_commit_entry = slave(commit_entry(CoreConfig())) // from scb
    //val lsu_ex_entry = slave(lsu_res_entry(CoreConfig()))
    val wb_regfile_interface = master(wregfile_interface(CoreConfig()))  // to regfile
    val wb_csr_interface = master(wcsr_interface(CoreConfig())) // to csr regfile
    val wb_dacahe_interfacec = master(dcache_write_interface(CoreConfig()))  // to dcache
    val bju_mis_predict = slave(branch_mispredict_entry(CoreConfig())) // from ex stage
    val flush = out Bool()
    val flush_except = out Bool()
    val flush_mis_predict = out Bool()
    val flush_mis_predict_target_pc = out UInt(InstAddrBus bits)
    // exception interface 侠义异常 // todo: 由于同一周期可能会有多个异常发生，因此仲裁后送出最高优先级的异常
    val exc_entry = slave(except_entry(CoreConfig())) // from exc_arbit, sync-pulse
    val exc_commit_entry = master(except_commit_entry(CoreConfig()))  // to exc_arbit
    // trap entry和mstatus 暂时统一处理
    // interruption interface 侠义中断 //
    val plic_int_entry = slave(int_entry(CoreConfig())) // 外部中断源，from PLIC, sync-pulse
    val clint_int_entry = slave(int_entry(CoreConfig()))  // 软件中断和定时器中断，from CLINT, sync-pulse
    val csr_exc_entry = master(csr_except_entry(CoreConfig()))  // 发生异常时，同时更新异常相关的特殊csr寄存器
  }

  // 异常列表+杂项 //
  io.exc_commit_entry.commit_req := io.ex_commit_entry.commit_req
  io.exc_commit_entry.instr := io.ex_commit_entry.instr
  io.exc_commit_entry.pc := io.ex_commit_entry.pc
  io.exc_commit_entry.target_pc := io.ex_commit_entry.target_pc
  io.exc_commit_entry.reg_wb_en := io.ex_commit_entry.reg_wb_en
  io.exc_commit_entry.reg_wb_addr := io.ex_commit_entry.reg_wb_addr
  io.exc_commit_entry.dcache_rd_en := io.ex_commit_entry.dcache_rd_en
  io.exc_commit_entry.dcache_rd_addr := io.ex_commit_entry.dcache_rd_addr
  io.exc_commit_entry.dcache_wb_en := io.ex_commit_entry.dcache_wb_en
  io.exc_commit_entry.dcache_wb_addr := io.ex_commit_entry.dcache_wb_addr
  io.exc_commit_entry.csr_wb_en := io.ex_commit_entry.csr_wb_en
  io.exc_commit_entry.csr_wb_addr := io.ex_commit_entry.csr_wb_addr
  io.exc_commit_entry.dec_valid := io.ex_commit_entry.dec_valid
  // branch predict flush //
  //val flush_mis_predict = io.bju_mis_predict.branch_cor || io.bju_mis_predict.ret_cor || io.bju_mis_predict.call_cor
  //val flush_mis_predict = io.ex_commit_entry.branch_cor || io.ex_commit_entry.ret_cor || io.ex_commit_entry.call_cor
  val flush_mis_predict = (io.ex_commit_entry.branch_cor || io.ex_commit_entry.ret_cor || io.ex_commit_entry.call_cor) && io.ex_commit_entry.commit_req  // fix: 修复连续多条分支预测失败时，cor为长电平，得到第一个flush pulse

  val flush_mis_predict_target_pc = io.ex_commit_entry.target_pc
  val flush_mis_predict_d1 = Reg(Bool()) init(False)
  flush_mis_predict_d1 := flush_mis_predict
  val flush_mis_predict_pulse = flush_mis_predict && ~flush_mis_predict_d1

  // except flush //
  //val flush_req = io.exc_entry.exc_req | io.plic_int_entry.int_req | io.clint_int_entry.int_req
  val flush_req = io.exc_entry.exc_req
  val flush_req_mask = Reg(Bool()) init(False)
  val exit_exc_flag = Bool()
  when(flush_req){
    flush_req_mask := True
  } .elsewhen(exit_exc_flag === True){
    flush_req_mask := False
  }
  val flush_req_pulse = flush_req && ~flush_req_mask

  // flush 输出 //
  io.flush := flush_mis_predict_pulse || flush_req_pulse
  io.flush_mis_predict := flush_mis_predict_pulse
  io.flush_mis_predict_target_pc := flush_mis_predict_target_pc
  io.flush_except := flush_req_pulse
  // 进入异常or退出异常更新CSR寄存器值//
  // todo
  exit_exc_flag := io.ex_commit_entry.commit_req && ((io.ex_commit_entry.instr === MRET) || (io.ex_commit_entry.instr === SRET))
  val flush_req_pulse_d1 = RegNext(flush_req_pulse, init=False)
  val mepc_buff    = RegNextWhen(io.csr_exc_entry.mepc  , flush_req_pulse_d1, U(0))
  val mcause_buff  = RegNextWhen(io.csr_exc_entry.mcause, flush_req_pulse_d1, U(0))
  val mtval_buff   = RegNextWhen(io.csr_exc_entry.mtval , flush_req_pulse_d1, U(0))
  /*
  val exc_pc_next =  UInt(InstAddrBus bits)
  when(io.ex_commit_entry.instr === BEQ || io.ex_commit_entry.instr === BNE || io.ex_commit_entry.instr === BLT || io.ex_commit_entry.instr === BGE || io.ex_commit_entry.instr === BLTU || io.ex_commit_entry.instr === BGEU || io.ex_commit_entry.instr === JAL || io.ex_commit_entry.instr === JALR){
    exc_pc_next := io.ex_commit_entry.target_pc
  } .otherwise{
    exc_pc_next := io.ex_commit_entry.pc + 4
  }
   */
  when(flush_req_pulse === True){ // 发生异常时，更新csr寄存器
    when(io.exc_entry.exc_req){
      io.csr_exc_entry.csr_except_wen := True
      io.csr_exc_entry.mepc    := io.exc_entry.exc_pc // 异常的返回地址为当前pc
      io.csr_exc_entry.mcause  := io.exc_entry.exc_cause
      io.csr_exc_entry.mtval   := io.exc_entry.exc_val
      io.csr_exc_entry.mstatus := 0 // todo mask int
    } .elsewhen(io.clint_int_entry.int_req){
      io.csr_exc_entry.csr_except_wen := True
      io.csr_exc_entry.mepc    := io.ex_commit_entry.target_pc // 中断的返回地址为下一条pc
      io.csr_exc_entry.mcause  := io.clint_int_entry.int_source
      io.csr_exc_entry.mtval   := 0 // 中断不需要更新mtval，清零/keep
      io.csr_exc_entry.mstatus := 0 // todo
    } .elsewhen(io.plic_int_entry.int_req){
      io.csr_exc_entry.csr_except_wen := True
      io.csr_exc_entry.mepc    := io.ex_commit_entry.target_pc
      io.csr_exc_entry.mcause  := io.plic_int_entry.int_source // 中断的返回地址为下一条pc
      io.csr_exc_entry.mtval   := 0 // 中断不需要更新mtval，清零/keep
      io.csr_exc_entry.mstatus := 0 // todo
    } .otherwise{
      io.csr_exc_entry.csr_except_wen := False
      io.csr_exc_entry.mepc    := 0
      io.csr_exc_entry.mcause  := 0
      io.csr_exc_entry.mtval   := 0
      io.csr_exc_entry.mstatus := 0
    }
  } .elsewhen(exit_exc_flag === True){  // 退出异常时，更新csr寄存器
    io.csr_exc_entry.csr_except_wen := True
    io.csr_exc_entry.mepc    := mepc_buff   // keep
    io.csr_exc_entry.mcause  := mcause_buff // keep
    io.csr_exc_entry.mtval   := mtval_buff  // keep
    io.csr_exc_entry.mstatus := 0 // todo
  }.otherwise{
    io.csr_exc_entry.csr_except_wen := False
    io.csr_exc_entry.mepc    := 0
    io.csr_exc_entry.mcause  := 0
    io.csr_exc_entry.mtval   := 0
    io.csr_exc_entry.mstatus := 0
  }

  // commit to regfile & memory //
  val commit_req_d1 = RegNext(io.ex_commit_entry.commit_req, False)
  val  commit_req_pulse = io.ex_commit_entry.commit_req & ~commit_req_d1
  //when(commit_req_pulse && ~io.flush){ // added flush
  when(commit_req_pulse && ~flush_req_pulse){  // todo: 分支预测失败指令是否要提交？ 中断异常指令是否要提交？
    io.wb_regfile_interface.reg_wen := io.ex_commit_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.ex_commit_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.ex_commit_entry.reg_wb_data // todo for lsu
    io.wb_csr_interface.reg_wen := io.ex_commit_entry.csr_wb_en
    io.wb_csr_interface.reg_waddr := io.ex_commit_entry.csr_wb_addr
    io.wb_csr_interface.reg_wdata := io.ex_commit_entry.csr_wb_data
    io.wb_dacahe_interfacec.we := io.ex_commit_entry.dcache_wb_en
    io.wb_dacahe_interfacec.waddr := io.ex_commit_entry.dcache_wb_addr
    io.wb_dacahe_interfacec.wdata := io.ex_commit_entry.dcache_wb_data
    //io.wb_dacahe_interfacec.re := io.ex_commit_entry.dcache_rd_en
    //io.wb_dacahe_interfacec.raddr := io.ex_commit_entry.dcache_rd_addr  // todo delete
    io.wb_dacahe_interfacec.sel := U(io.ex_commit_entry.dcache_wb_sel)
    //io.ex_commit_entry.dcache_rd_data := io.wb_dacahe_interfacec.rdata
  }.otherwise{
    io.wb_regfile_interface.reg_wen := False
    io.wb_regfile_interface.reg_waddr := 0
    io.wb_regfile_interface.reg_wdata := 0
    io.wb_csr_interface.reg_wen := False
    io.wb_csr_interface.reg_waddr := 0
    io.wb_csr_interface.reg_wdata := 0
    io.wb_dacahe_interfacec.we := False
    io.wb_dacahe_interfacec.waddr := 0
    io.wb_dacahe_interfacec.wdata := 0
    //io.wb_dacahe_interfacec.re := False
    //io.wb_dacahe_interfacec.raddr := 0
    io.wb_dacahe_interfacec.sel := U"1111"
    //io.ex_commit_entry.dcache_rd_data := 0
  }


  val commit_req_ack = Reg(Bool()) init(False)
  val recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  when(commit_req_pulse){
    commit_req_ack := True  // todo with regfile/csr/dcache ack
    recv_id := io.ex_commit_entry.trans_id
  } .otherwise{
    commit_req_ack := False
    recv_id := SCB_IU_DEEPTH
  }
  io.ex_commit_entry.commit_ack := commit_req_ack
  io.ex_commit_entry.recv_id := recv_id

  val report_cnt = Reg(UInt(1 bits)) init(0)
  when(commit_req_pulse){
    report_cnt := 0
  } .elsewhen(io.ex_commit_entry.commit_ack){
    report_cnt := report_cnt + 1
  }

  when(commit_req_pulse){
    report(Seq("retire pc : ", io.ex_commit_entry.pc))
    report(Seq("retire instr : ", io.ex_commit_entry.instr))
    when(io.wb_regfile_interface.reg_wen){
      report(Seq("update regfile [addr] : ", io.wb_regfile_interface.reg_waddr))
      report(Seq("update regfile [data] : ", io.wb_regfile_interface.reg_wdata))
    } .otherwise{}
    when(io.wb_dacahe_interfacec.we){
      report(Seq("write dcache [addr] : ", io.wb_dacahe_interfacec.waddr))
      report(Seq("write dcache [data] : ", io.wb_dacahe_interfacec.wdata))
    } .otherwise{}
    /*
    when(io.wb_dacahe_interfacec.re){
      report(Seq("read dcache", io.wb_dacahe_interfacec.raddr, io.wb_dacahe_interfacec.rdata))
    } .otherwise{}
    */
  } .otherwise{}

}