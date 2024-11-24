package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class commit() extends Component with Global_parameter with Interface_MS{
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val ex_commit_entry = slave(commit_entry(CoreConfig())) // from scb
    val wb_regfile_interface = master(wregfile_interface(CoreConfig()))  // to regfile
    val wb_csr_interface = master(wcsr_interface(CoreConfig())) // to csr regfile
    val wb_dacahe_interfacec = master(dcache_interface(CoreConfig()))  // to dcache
    val bju_mis_predict = slave(branch_mispredict_entry(CoreConfig())) // from ex stage
    val flush = out Bool()
    val flush_mis_predict = out Bool()
    val flush_mis_predict_target_pc = out UInt(InstAddrBus bits)
    // exception interface //
    val extern_int_req = in Bool()  // 外部中断源，from PLIC
    val software_int_req = in Bool()  // 软件中断，from CLINT
    val timer_int_req = in Bool() // 定时器中断，from CLINT
  }

  // 异常列表+杂项 //
  //val flush_mis_predict = io.bju_mis_predict.branch_cor || io.bju_mis_predict.ret_cor || io.bju_mis_predict.call_cor
  val flush_mis_predict = io.ex_commit_entry.branch_cor || io.ex_commit_entry.ret_cor || io.ex_commit_entry.call_cor
  val flush_mis_predict_target_pc = io.ex_commit_entry.target_pc
  val flush_mis_predict_d1 = Reg(Bool()) init(False)
  flush_mis_predict_d1 := flush_mis_predict
  val flush_mis_predict_pulse = flush_mis_predict && ~flush_mis_predict_d1

  // flush 输出 //
  io.flush := flush_mis_predict_pulse
  io.flush_mis_predict := flush_mis_predict_pulse
  io.flush_mis_predict_target_pc := flush_mis_predict_target_pc

  when(io.ex_commit_entry.commit_req){
    io.wb_regfile_interface.reg_wen := io.ex_commit_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.ex_commit_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.ex_commit_entry.reg_wb_data
    io.wb_csr_interface.reg_wen := io.ex_commit_entry.csr_wb_en
    io.wb_csr_interface.reg_waddr := io.ex_commit_entry.csr_wb_addr
    io.wb_csr_interface.reg_wdata := io.ex_commit_entry.csr_wb_data
    io.wb_dacahe_interfacec.we := io.ex_commit_entry.dcache_wb_en
    io.wb_dacahe_interfacec.waddr := io.ex_commit_entry.dcache_wb_addr
    io.wb_dacahe_interfacec.wdata := io.ex_commit_entry.dcache_wb_data
    io.wb_dacahe_interfacec.re := io.ex_commit_entry.dcache_rd_en
    io.wb_dacahe_interfacec.raddr := io.ex_commit_entry.dcache_rd_addr
    io.wb_dacahe_interfacec.sel := U(io.ex_commit_entry.dcache_wb_sel)
    io.ex_commit_entry.dcache_rd_data := io.wb_dacahe_interfacec.rdata
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
    io.wb_dacahe_interfacec.re := False
    io.wb_dacahe_interfacec.raddr := 0
    io.wb_dacahe_interfacec.sel := U"1111"
    io.ex_commit_entry.dcache_rd_data := 0
  }
  val commit_req_ack = Reg(Bool()) init(False)
  val recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  when(io.ex_commit_entry.commit_req){
    commit_req_ack := True  // todo with regfile/csr/dcache ack
    recv_id := io.ex_commit_entry.trans_id
  } .otherwise{
    commit_req_ack := False
    recv_id := SCB_IU_DEEPTH
  }
  io.ex_commit_entry.commit_ack := commit_req_ack
  io.ex_commit_entry.recv_id := recv_id

  val report_cnt = Reg(UInt(1 bits)) init(0)
  when(io.ex_commit_entry.commit_ack){
    report_cnt := report_cnt + 1
  }

  when(io.ex_commit_entry.commit_ack && report_cnt===0){
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