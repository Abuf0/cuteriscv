package mainelib
import spinal.core._
import spinal.lib._

case class Payload() extends Bundle {
  val pc  = UInt(32 bits)
  val rd  = UInt(5 bits)
  val imm = Bits(32 bits)
}

case class if12()  extends Bundle {
  val i  = (new Payload)
  val iv =  Bool()
  val st =  Bool()
  val fl =  Bool()
  val o  = (new Payload)
  val ov =  Bool()
}

case class branch_predict_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val pc = UInt(cfg.InstBus bits)
  val branch_valid = Bool()
  val branch_taken = Bool()
  val branch_target = UInt(cfg.InstBus bits)
  val is_branch = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val is_jump = Bool()
  val branch_cor = Bool()
  val call_cor = Bool()
  val ret_cor = Bool()
  val jump_cor = Bool()

  override def asMaster(): Unit = {
    out(pc, branch_taken, branch_target,branch_valid,is_branch,is_call,is_ret,is_jump,branch_cor,call_cor,ret_cor,jump_cor)
  }
}

case class dec_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val alu_sel = ALU_UNIT_SEL()
  val op_type = OP_TYPE()
  val rs1_entry = areg_wr_entry(cfg)
  val rs2_entry = areg_wr_entry(cfg)
  val rd_entry = areg_wr_entry(cfg)
  val csr_entry = csr_wr_entry(cfg)
  val imm = SInt(cfg.ImmBus bits)
  val dec_valid = Bool()
  val instr_err = Bool()
  val dec_bju_info = dec_bju_entry(cfg)

  override def asMaster(): Unit = {
    out(alu_sel,op_type,rs1_entry,rs2_entry,rd_entry,csr_entry,imm,dec_valid,instr_err)
    out(dec_bju_info)
  }
}

case class areg_wr_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_addr = UInt(cfg.Reg_AID_WIDTH bits)
  val reg_wten = Bool()
  val reg_rden = Bool()

  override def asMaster(): Unit = {
    out(reg_addr,reg_wten,reg_rden)
  }
}

case class preg_wr_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val areg_addr = UInt(cfg.Reg_AID_WIDTH bits)
  val preg_addr = UInt(cfg.Reg_PID_WIDTH bits)
  val reg_wten = Bool()
  val reg_wdata = UInt(cfg.RegDataBus bits)

  override def asMaster(): Unit = {
    out(areg_addr,preg_addr,reg_wten,reg_wdata)
  }
}

case class csr_wr_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_addr = UInt(cfg.CSRAddrBus bits)
  val reg_wten = Bool()
  val reg_rden = Bool()
  val reg_wdata = UInt(cfg.CSRDataBus bits)

  override def asMaster(): Unit = {
    out(reg_addr,reg_wten,reg_rden,reg_wdata)
  }
}

// PRF
case class preg_rd_if(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_addr = UInt(cfg.Reg_PID_WIDTH bits)
  val reg_rden = Bool()
  val reg_rdata = UInt(cfg.RegDataBus bits)

  override def asMaster(): Unit = {
    out(reg_addr,reg_rden)
    in(reg_rdata)
  }
}

case class csr_rd_if(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_addr = UInt(cfg.CSRAddrBus bits)
  val reg_rden = Bool()
  val reg_rdata = UInt(cfg.CSRDataBus bits)

  override def asMaster(): Unit = {
    out(reg_addr,reg_rden,reg_rdata)
  }
}

case class reg_scb_rename_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_addr_real = UInt(cfg.Reg_AID_WIDTH bits)
  val reg_addr_rename = UInt(cfg.Reg_PID_WIDTH bits)
  val reg_rden, reg_wten = Bool()
  val reg_rdata = UInt(cfg.RegDataBus bits)
  val reg_ready = Bool()

  override def asMaster(): Unit = {
    out(reg_addr_real,reg_addr_rename,reg_rden,reg_wten,reg_rdata,reg_ready)
  }
}

case class reg_scb_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_addr = UInt(cfg.Reg_AID_WIDTH bits)
  val reg_rden, reg_wten = Bool()
  val reg_rdata = UInt(cfg.RegDataBus bits)

  override def asMaster(): Unit = {
    out(reg_addr,reg_rden,reg_wten,reg_rdata)
  }
}

case class csr_scb_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_addr = UInt(cfg.CSRAddrBus bits)
  val reg_rden, reg_wten = Bool()
  val reg_rdata = UInt(cfg.CSRDataBus bits)
  val reg_ready = Bool()

  override def asMaster(): Unit = {
    out(reg_addr,reg_rden,reg_wten,reg_rdata,reg_ready)
  }
}

// IF2_TM ==> IF3_FM //
case class Payload2_3(cfg: CoreConfig) extends Bundle {
  val predict_branch_if = branch_predict_entry(cfg)
  val pc_fl = Flow(UInt(cfg.InstBus bits))
}

case class if2_3(cfg: CoreConfig)  extends Bundle {
  val i  = (new Payload2_3(cfg))
  val iv =  Bool()
  val st =  Bool()
  val fl =  Bool()
  val o  = (new Payload2_3(cfg))
  val ov =  Bool()
}

// ID ==> ISSUE //
case class Payload_id2issue(cfg: CoreConfig) extends Bundle {
  val instr_fl = Vec.fill(cfg.issueWidth)(Flow(UInt(cfg.InstBus bits)))
  val pc_fl = Vec.fill(cfg.issueWidth)(Flow(UInt(cfg.InstAddrBus bits)))
  val predict_if = branch_predict_entry(cfg)
  val dec_if = Vec.fill(cfg.issueWidth)(dec_entry(cfg))
}

case class id2issue(cfg: CoreConfig)  extends Bundle {
  val i  = (new Payload_id2issue(cfg))
  val iv =  Bool()
  val st =  Bool()
  val fl =  Bool()
  val o  = (new Payload_id2issue(cfg))
  val ov =  Bool()
}

// EX ==> WB //
case class Payload_ex2wb(cfg: CoreConfig) extends Bundle {
  val alu_ex_out = Vec.fill(FU_ID.enums.ALU.size)(exe_res_entry(cfg))
  val mul_ex_out = Vec.fill(FU_ID.enums.MUL.size)(exe_res_entry(cfg))
  val div_ex_out = Vec.fill(FU_ID.enums.DIV.size)(exe_res_entry(cfg))
  val bju_ex_out = Vec.fill(FU_ID.enums.BJU.size)(exe_res_entry(cfg))
  val bju_ex_branch_predict = Vec.fill(FU_ID.enums.BJU.size)(branch_predict_entry(cfg))
  val flush_mispredict = flush_mispredict_entry(cfg)
  val lsu_ex_out = Vec.fill(FU_ID.enums.LSU.size)(exe_res_entry(cfg))
  val lsu_ex_store_if = Vec.fill(FU_ID.enums.LSU.size)(store_entry(cfg))
  val lsu_ex_load_if = Vec.fill(FU_ID.enums.LSU.size)(load_entry(cfg))
  val csr_ex_out = Vec.fill(FU_ID.enums.CSR.size)(exe_csr_res_entry(cfg))
  if(cfg.withFpu) {
    val fpu_ex_out = Vec.fill(FU_ID.enums.BJU.size)(exe_res_entry(cfg))
  }
  val nop_ex_out = Vec.fill(FU_ID.enums.NOP.size)(exe_nop_res_entry(cfg))
}

case class ex2wb(cfg: CoreConfig)  extends Bundle {
  val i  = (new Payload_ex2wb(cfg))
  val iv =  Bool()
  val st =  Bool()
  val fl =  Bool()
  val o  = (new Payload_ex2wb(cfg))
  val ov =  Bool()
}

case class fu_id_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val fu_id = FU_ID()
  override def asMaster(): Unit = {
    out(fu_id)
  }

}

case class issue_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val valid = Bool()
  val trans_id = UInt(cfg.SCB_ID_WIDTH bits)
  val pc = UInt(cfg.InstAddrBus bits)
  val instr = UInt(cfg.InstBus bits)
  val predict_info = branch_predict_entry(cfg)
  val alu_sel = ALU_UNIT_SEL()
  val op_type = OP_TYPE()
  val fu_id = FU_ID()
  val rs1_scb_entry = reg_scb_rename_entry(cfg)
  val rs2_scb_entry = reg_scb_rename_entry(cfg)
  val rd_scb_entry = reg_scb_rename_entry(cfg)
  val csr_entry = csr_scb_entry(cfg)
  val imm = SInt(cfg.ImmBus bits)
  val dec_valid = Bool()
  val instr_err = Bool()
  val issue_busy = Bool()
  val dec_bju_info = dec_bju_entry(cfg)

  override def asMaster(): Unit = {
    out(valid,trans_id,pc,instr,predict_info)
    out(alu_sel,op_type,fu_id)
    out(rs1_scb_entry,rs2_scb_entry,rd_scb_entry)
    out(csr_entry)
    out(imm)
    out(dec_valid)
    out(instr_err)
    out(issue_busy)
    out(dec_bju_info)
  }
}

case class dec_bju_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val dec_is_jump,dec_is_call,dec_is_ret,dec_is_branch = Bool()
  override def asMaster(): Unit = {
    out(dec_is_jump,dec_is_call,dec_is_ret,dec_is_branch)
  }
}

case class exe_res_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val result = SInt(cfg.RegDataBus bits)
  val instr = UInt(cfg.InstBus bits)
  val trans_id = UInt(cfg.SCB_ID_WIDTH bits)
  val pc = UInt(cfg.InstAddrBus bits)
  val result_vld = Bool()
  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
  val reg_wif = preg_wr_entry(cfg)

  override def asMaster(): Unit = {
    out(result,instr,trans_id,pc,result_vld,issue_id)
    out(reg_wif)
  }
}

case class exe_nop_res_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val instr = UInt(cfg.InstBus bits)
  val trans_id = UInt(cfg.SCB_ID_WIDTH bits)
  val pc = UInt(cfg.InstAddrBus bits)
  val result_vld = Bool()
  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
  val dec_valid = Bool()
  val instr_err = Bool()

  override def asMaster(): Unit = {
    out(instr,trans_id,pc,result_vld,issue_id)
    out(dec_valid,instr_err)
  }
}

case class lsu_res_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val result = UInt(cfg.RegDataBus bits)  // load result to rd
  val pc = UInt(cfg.InstAddrBus bits)
  val instr = UInt(cfg.InstBus bits)
  val trans_id = UInt(cfg.SCB_ID_WIDTH bits)
  val store_if = store_entry(cfg)
  val load_if = load_entry(cfg)
  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
  val reg_wif = preg_wr_entry(cfg)

  override def asMaster(): Unit = {
    out(result,instr,trans_id,pc,issue_id)
    out(store_if,load_if)
    out(reg_wif)
  }
}

case class exe_csr_res_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val instr = UInt(cfg.InstBus bits)
  val trans_id = UInt(cfg.SCB_ID_WIDTH bits)
  val pc = UInt(cfg.InstAddrBus bits)
  val result = UInt(cfg.RegDataBus bits)
  val result_csr = UInt(cfg.CSRDataBus bits)
  val result_vld = Bool()
  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
  val reg_wif = preg_wr_entry(cfg)
  val csr_wif = csr_wr_entry(cfg)

  override def asMaster(): Unit = {
    out(instr,trans_id,pc,result,result_csr,result_vld,issue_id)
    out(reg_wif,csr_wif)
  }
}

case class DispatchIn(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val valid    = Bool()
  val rs1      = UInt(cfg.Reg_AID_WIDTH bits)
  val rs2      = UInt(cfg.Reg_AID_WIDTH bits)
  val rs1_rden = Bool()
  val rs2_rden = Bool()
  val rd       = UInt(cfg.Reg_AID_WIDTH bits)
  val hasDest  = Bool()
  val isBranch = Bool()          // 该指令是否作为建立检查点的分支
  override def asMaster(): Unit = {
    out(valid,rs1,rs2,rs1_rden,rs2_rden,rd,hasDest,isBranch)
  }
}

case class RenamedOut(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val valid    = Bool()
  val src1_p   = UInt(cfg.Reg_PID_WIDTH+1 bits)
  val src2_p   = UInt(cfg.Reg_PID_WIDTH+1 bits)
  val dst_p    = UInt(cfg.Reg_PID_WIDTH bits)  // 无目的时值无意义
  val old_p    = UInt(cfg.Reg_PID_WIDTH bits)  // 供 ROB 记录和提交回收
  val hasDest  = Bool()
  val rd_arch  = UInt(cfg.Reg_AID_WIDTH bits)  // 供 ROB 记录
  val isBranch = Bool()
  val src1_p_ready = Bool()
  val src2_p_ready = Bool()
  override def asMaster(): Unit = {
    out(valid,src1_p,src2_p,dst_p,old_p,hasDest,rd_arch,isBranch)
    out(src1_p_ready,src2_p_ready)
  }
}

case class CkptToken(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val rat     = Vec(UInt(cfg.Reg_PID_WIDTH bits), cfg.REG_NUM) // 简易快照（演示）
  val flHead  = UInt(log2Up(cfg.REG_PRF_NUM) bits)
  val flTail  = UInt(log2Up(cfg.REG_PRF_NUM) bits)
  val flCount = UInt(log2Up(cfg.REG_PRF_NUM+1) bits)
  override def asMaster(): Unit = {
    out(rat,flHead,flTail,flCount)
  }
}

case class CommitIn(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val valid    = Bool()
  val hasDest  = Bool()
  val rd_arch  = UInt(cfg.Reg_AID_WIDTH bits)
  val old_p    = UInt(cfg.Reg_PID_WIDTH bits)
  val rd_data  = UInt(cfg.RegDataBus bits)
  override def asMaster(): Unit = {
    out(valid,hasDest,rd_arch,old_p,rd_data)
    //in(rd_data)
  }
}

case class cmt_reg_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_commitin = CommitIn(cfg)
  val csr_commitin = CommitIn(cfg)
  val cmt_id = UInt(cfg.SCB_ID_WIDTH bits)
  val cmt_valid = Bool()
  override def asMaster(): Unit = {
    out(reg_commitin,csr_commitin,cmt_id,cmt_valid)
  }
}

case class commit_reg_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_wten = Bool()
  val reg_addr = UInt(cfg.Reg_PID_WIDTH bits)
  val reg_wdata = UInt(cfg.RegDataBus bits)
  override def asMaster(): Unit = {
    out(reg_wten,reg_addr,reg_wdata)
  }
}

case class commit_csr_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val reg_wten = Bool()
  val reg_addr = UInt(cfg.CSRAddrBus bits)
  val reg_wdata = UInt(cfg.CSRDataBus bits)
  override def asMaster(): Unit = {
    out(reg_wten,reg_addr,reg_wdata)
  }
}

case class store_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val store_wb_en = Bool()
  val store_wb_addr = UInt(cfg.DataAddrBus bits)
  val store_wb_data = UInt(cfg.DMemDataWidth bits)
  val store_wb_byte = Bits(cfg.DMemSelBus bits)
  val store_wb_trans_id = UInt(cfg.SCB_ID_WIDTH bits)
  override def asMaster(): Unit = {
    out(store_wb_en,store_wb_addr,store_wb_data,store_wb_byte,store_wb_trans_id)
  }
}

case class load_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val load_rd_en = Bool()
  val load_rd_addr = UInt(cfg.DataAddrBus bits)
  val load_rd_data = UInt(cfg.DMemDataWidth bits)
  val load_rd_byte = Bits(cfg.DMemSelBus bits)
  override def asMaster(): Unit = {
    out(load_rd_en,load_rd_addr,load_rd_byte)
    in(load_rd_data)
  }
}

case class cmt_memw_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val mem_wen = Bool()
  val mem_waddr = UInt(cfg.DataAddrBus bits)
  val mem_wdata = UInt(cfg.DMemDataWidth bits)
  val mem_sel = Bits(cfg.DMemSelBus bits)
  override def asMaster(): Unit = {
    out(mem_wen,mem_waddr,mem_wdata,mem_sel)
  }
}

case class cmt_memr_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val mem_ren = Bool()
  val mem_raddr = UInt(cfg.DataAddrBus bits)
  val mem_rdata = UInt(cfg.DMemDataWidth bits)
  val mem_sel = Bits(cfg.DMemSelBus bits)
  val mem_rerr = Bool()
  override def asMaster(): Unit = {
    out(mem_ren,mem_raddr,mem_rdata,mem_sel,mem_rerr)
  }
}

case class mem_read_if(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val re = Bool()
  val raddr = UInt(cfg.DataAddrBus bits)
  val rdata = UInt(cfg.DMemDataWidth bits)
  val sel = Bits(cfg.DMemSelBus bits)
  val rvalid = Bool()
  override def asMaster(): Unit = {
    out(re,raddr,sel)
    in(rdata,rvalid)
  }
}

case class ex2wb_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val ex2wb_payload = Payload_ex2wb(cfg)
  override def asMaster(): Unit = {
    out(ex2wb_payload)
  }
}

case class commit_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val commit_vld = Bool()
  val trans_id = UInt(cfg.SCB_ID_WIDTH bits)
  val instr = UInt(cfg.InstBus bits)
  val pc = UInt(cfg.InstAddrBus bits)
  val mem_wif = cmt_memw_entry(cfg)
  val mem_rif = cmt_memr_entry(cfg)
  val reg_wif = preg_wr_entry(cfg)
  val csr_wif = csr_wr_entry(cfg)
  val dec_valid = Bool()
  val instr_err = Bool()

  override def asMaster(): Unit = {
    out(commit_vld,trans_id,instr,pc)
    out(mem_wif,mem_rif,reg_wif,csr_wif)
    out(dec_valid,instr_err)
  }
}

case class trace_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val commit_info = commit_entry(cfg)
  val trap_info = trap_entry(cfg)

  override def asMaster(): Unit = {
    out(commit_info)
    out(trap_info)
  }
}

case class trap_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val trap_hit = Bool()
  val trap_pc = UInt(cfg.InstAddrBus bits)
  val trap_cause = UInt(cfg.CSRDataBus bits)
  val trap_value = UInt(cfg.CSRDataBus bits)
  val trap_status = UInt(cfg.CSRDataBus bits)
  override def asMaster(): Unit = {
    out(trap_hit,trap_pc,trap_cause,trap_value,trap_status)
  }
}

// int_entry interface
case class int_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val int_req = Bool()  // 中断申请
  val int_source = UInt(cfg.CSRDataBus bits) // 中断源
  val int_clear = Bool()  // 清除中断

  override def asMaster(): Unit = {
    out(int_req,int_source)
    in(int_clear)
  }
}

case class exc_csr_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val exc_req = Bool()  // 异常标志
  val exc_cause = UInt(cfg.CSRDataBus bits)// 异常原因
  val exc_pc = UInt(cfg.CSRDataBus bits)  // 精确异常引发pc地址
  val exc_val = UInt(cfg.CSRDataBus bits)  // 非法指令编码值or非法访存地址 // 位宽todo
  val exc_status = UInt(cfg.CSRDataBus bits)

  override def asMaster(): Unit = {
    out(exc_req,exc_cause,exc_pc,exc_val,exc_status)
  }
}

case class flush_mispredict_entry(cfg: CoreConfig) extends Bundle with IMasterSlave {
  val hit = Bool()
  val target = UInt(cfg.InstAddrBus bits)
  val trans_id = UInt(cfg.SCB_ID_WIDTH bits)
  override def asMaster(): Unit = {
    out(hit,target,trans_id)
  }
}

case class mem_if(cfg: CoreConfig, AW: Int, DW: Int) extends Bundle with IMasterSlave {
  val mem_cen = Bool()
  val mem_we = Bool()
  val mem_addr = UInt(AW bits)
  val mem_wdata = UInt(DW bits)
  val mem_sel = Bits(DW/cfg.ByteWidth bits)
  val mem_rdata = UInt(DW bits)
  val mem_rvalid = Bool()
  val mem_wvalid = Bool() // alawys 1
  val mem_ready = Bool()
  override def asMaster(): Unit = {
    out(mem_cen,mem_we,mem_addr,mem_wdata,mem_sel)
    in(mem_rdata,mem_rvalid,mem_wvalid,mem_ready)
  }
}

case class tcm_if(cfg: CoreConfig, AW: Int, DW: Int) extends Bundle with IMasterSlave {
  val tcm_cs = Bool()
  val tcm_wr = Bool()
  val tcm_addr = UInt(AW bits)
  val tcm_wdata = UInt(DW bits)
  val tcm_bytewr = Bits(DW/cfg.ByteWidth bits)
  val tcm_rdata = UInt(DW bits)
  val tcm_wait = Bool()
  val tcm_err = Bool()
  val tcm_master = UInt(4 bits)
  val tcm_priv = Bool()
  override def asMaster(): Unit = {
    out(tcm_cs,tcm_wr,tcm_addr,tcm_wdata,tcm_bytewr,tcm_master,tcm_priv)
    in(tcm_rdata,tcm_wait,tcm_err)
  }
}