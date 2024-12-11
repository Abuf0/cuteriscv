package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class wb() extends Component with Global_parameter with Interface_MS{
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val ex_wb_entry = slave(commit_entry(CoreConfig())) // from scb
    val alu_ex_wb_entry  = slave(commit_entry(CoreConfig()))
    val mul1_ex_wb_entry = slave(commit_entry(CoreConfig()))
    val mul2_ex_wb_entry = slave(commit_entry(CoreConfig()))
    val divu_ex_wb_entry = slave(commit_entry(CoreConfig()))
    val bju_ex_wb_entry  = slave(commit_entry(CoreConfig()))
    val lsu_ex_wb_entry  = slave(commit_entry(CoreConfig()))
    val csr_ex_wb_entry  = slave(commit_entry(CoreConfig()))
    val nopu_ex_wb_entry = slave(commit_entry(CoreConfig()))
    val wb_regfile_interface = master(wregfile_interface(CoreConfig()))  // to regfile
    val wb_csr_interface = master(wcsr_interface(CoreConfig())) // to csr regfile
    val wb_scb_entry = master(commit_entry(CoreConfig()))  // to scb
    val head_ptr = in UInt(SCB_INSTR_WIDTH bits) // from scb
    val toload_addr = in UInt(DataAddrBus bits)  // to wb
    val toload_hit = out Bool()
    val toload_data = out UInt(DataBus bits)
  }
  val INSTR_TAB = Vec(Reg(UInt(InstBus bits)) init(0), SCB_INSTR_DEEPTH)
  val PC_TAB = Vec(Reg(UInt(InstAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val REG_WEN_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val REG_WADDR_TAB = Vec(Reg(UInt(RegAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val REG_WDATA_TAB = Vec(Reg(UInt(RegDataBus bits)) init(0), SCB_INSTR_DEEPTH)
  val CSR_WEN_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val CSR_WADDR_TAB = Vec(Reg(UInt(CSRAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val CSR_WDATA_TAB = Vec(Reg(UInt(CSRDataBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DCACHE_REN_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val DCACHE_RADDR_TAB = Vec(Reg(UInt(DataAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DCACHE_WEN_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val DCACHE_WADDR_TAB = Vec(Reg(UInt(DataAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DCACHE_WDATA_TAB = Vec(Reg(UInt(DataBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DCACHE_WSEL_TAB = Vec(Reg(Bits(4 bits)) init(0), SCB_INSTR_DEEPTH)
  val BRANCH_COR_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val CALL_COR_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val RET_COR_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val IS_BRANCH_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val IS_CALL_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val IS_RET_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val IS_JUMP_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val TARGET_PC_TAB = Vec(Reg(UInt(InstAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DEC_VLD_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)

  val (toload_hit, toload_index): (Bool, UInt) = DCACHE_WADDR_TAB.sFindFirst(_===io.toload_addr)
  io.toload_hit := toload_hit
  io.toload_data := DCACHE_WDATA_TAB(toload_index)

  val index_alu = io.alu_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.alu_ex_wb_entry.commit_req){
    INSTR_TAB(index_alu)        := io.alu_ex_wb_entry.instr
    PC_TAB(index_alu)           := io.alu_ex_wb_entry.pc
    REG_WEN_TAB(index_alu)      := io.alu_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_alu)    := io.alu_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_alu)    := io.alu_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_alu)      := io.alu_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_alu)    := io.alu_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_alu)    := io.alu_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_alu)   := io.alu_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_alu) := io.alu_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_alu)   := io.alu_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_alu)  := io.alu_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_alu) := io.alu_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_alu) := io.alu_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_alu)   := io.alu_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_alu)     := io.alu_ex_wb_entry.call_cor
    RET_COR_TAB(index_alu)      := io.alu_ex_wb_entry.ret_cor
    IS_BRANCH_TAB(index_alu)    := io.alu_ex_wb_entry.is_branch
    IS_CALL_TAB(index_alu)      := io.alu_ex_wb_entry.is_call
    IS_RET_TAB(index_alu)       := io.alu_ex_wb_entry.is_ret
    IS_JUMP_TAB(index_alu)      := io.alu_ex_wb_entry.is_jump
    TARGET_PC_TAB(index_alu)    := io.alu_ex_wb_entry.target_pc
    DEC_VLD_TAB(index_alu)      := True
  } .otherwise{ }

  val index_mul1 = io.mul1_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.mul1_ex_wb_entry.commit_req){
    INSTR_TAB(index_mul1)        := io.mul1_ex_wb_entry.instr
    PC_TAB(index_mul1)           := io.mul1_ex_wb_entry.pc
    REG_WEN_TAB(index_mul1)      := io.mul1_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_mul1)    := io.mul1_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_mul1)    := io.mul1_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_mul1)      := io.mul1_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_mul1)    := io.mul1_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_mul1)    := io.mul1_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_mul1)   := io.mul1_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_mul1) := io.mul1_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_mul1)   := io.mul1_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_mul1)  := io.mul1_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_mul1) := io.mul1_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_mul1) := io.mul1_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_mul1)   := io.mul1_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_mul1)     := io.mul1_ex_wb_entry.call_cor
    IS_BRANCH_TAB(index_mul1)    := io.mul1_ex_wb_entry.is_branch
    IS_CALL_TAB(index_mul1)      := io.mul1_ex_wb_entry.is_call
    IS_RET_TAB(index_mul1)       := io.mul1_ex_wb_entry.is_ret
    IS_JUMP_TAB(index_mul1)      := io.alu_ex_wb_entry.is_jump
    RET_COR_TAB(index_mul1)      := io.mul1_ex_wb_entry.ret_cor
    TARGET_PC_TAB(index_mul1)    := io.mul1_ex_wb_entry.target_pc
    DEC_VLD_TAB(index_mul1)      := True
  } .otherwise{ }

  val index_mul2 = io.mul2_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.mul2_ex_wb_entry.commit_req){
    INSTR_TAB(index_mul2)        := io.mul2_ex_wb_entry.instr
    PC_TAB(index_mul2)           := io.mul2_ex_wb_entry.pc
    REG_WEN_TAB(index_mul2)      := io.mul2_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_mul2)    := io.mul2_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_mul2)    := io.mul2_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_mul2)      := io.mul2_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_mul2)    := io.mul2_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_mul2)    := io.mul2_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_mul2)   := io.mul2_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_mul2) := io.mul2_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_mul2)   := io.mul2_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_mul2)  := io.mul2_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_mul2) := io.mul2_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_mul2) := io.mul2_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_mul2)   := io.mul2_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_mul2)     := io.mul2_ex_wb_entry.call_cor
    RET_COR_TAB(index_mul2)      := io.mul2_ex_wb_entry.ret_cor
    IS_BRANCH_TAB(index_mul2)    := io.mul2_ex_wb_entry.is_branch
    IS_CALL_TAB(index_mul2)      := io.mul2_ex_wb_entry.is_call
    IS_RET_TAB(index_mul2)       := io.mul2_ex_wb_entry.is_ret
    IS_JUMP_TAB(index_mul2)       := io.mul2_ex_wb_entry.is_jump
    TARGET_PC_TAB(index_mul2)    := io.mul2_ex_wb_entry.target_pc
    DEC_VLD_TAB(index_mul2)      := True
  } .otherwise{ }

  val index_divu = io.divu_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.divu_ex_wb_entry.commit_req){
    INSTR_TAB(index_divu)        := io.divu_ex_wb_entry.instr
    PC_TAB(index_divu)           := io.divu_ex_wb_entry.pc
    REG_WEN_TAB(index_divu)      := io.divu_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_divu)    := io.divu_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_divu)    := io.divu_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_divu)      := io.divu_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_divu)    := io.divu_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_divu)    := io.divu_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_divu)   := io.divu_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_divu) := io.divu_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_divu)   := io.divu_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_divu)  := io.divu_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_divu) := io.divu_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_divu) := io.divu_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_divu)   := io.divu_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_divu)     := io.divu_ex_wb_entry.call_cor
    RET_COR_TAB(index_divu)      := io.divu_ex_wb_entry.ret_cor
    IS_BRANCH_TAB(index_divu)    := io.divu_ex_wb_entry.is_branch
    IS_CALL_TAB(index_divu)      := io.divu_ex_wb_entry.is_call
    IS_RET_TAB(index_divu)       := io.divu_ex_wb_entry.is_ret
    IS_JUMP_TAB(index_divu)       := io.divu_ex_wb_entry.is_jump
    TARGET_PC_TAB(index_divu)    := io.divu_ex_wb_entry.target_pc
    DEC_VLD_TAB(index_divu)      := True
  } .otherwise{ }

  val index_bju = io.bju_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.bju_ex_wb_entry.commit_req){
    INSTR_TAB(index_bju)        := io.bju_ex_wb_entry.instr
    PC_TAB(index_bju)           := io.bju_ex_wb_entry.pc
    REG_WEN_TAB(index_bju)      := io.bju_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_bju)    := io.bju_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_bju)    := io.bju_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_bju)      := io.bju_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_bju)    := io.bju_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_bju)    := io.bju_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_bju)   := io.bju_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_bju) := io.bju_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_bju)   := io.bju_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_bju)  := io.bju_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_bju) := io.bju_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_bju) := io.bju_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_bju)   := io.bju_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_bju)     := io.bju_ex_wb_entry.call_cor
    RET_COR_TAB(index_bju)      := io.bju_ex_wb_entry.ret_cor
    IS_BRANCH_TAB(index_bju)    := io.bju_ex_wb_entry.is_branch
    IS_CALL_TAB(index_bju)      := io.bju_ex_wb_entry.is_call
    IS_RET_TAB(index_bju)       := io.bju_ex_wb_entry.is_ret
    IS_JUMP_TAB(index_bju)       := io.bju_ex_wb_entry.is_jump
    TARGET_PC_TAB(index_bju)    := io.bju_ex_wb_entry.target_pc
    DEC_VLD_TAB(index_bju)      := True
  } .otherwise{ }

  val index_lsu = io.lsu_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.lsu_ex_wb_entry.commit_req){
    INSTR_TAB(index_lsu)        := io.lsu_ex_wb_entry.instr
    PC_TAB(index_lsu)           := io.lsu_ex_wb_entry.pc
    REG_WEN_TAB(index_lsu)      := io.lsu_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_lsu)    := io.lsu_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_lsu)    := io.lsu_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_lsu)      := io.lsu_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_lsu)    := io.lsu_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_lsu)    := io.lsu_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_lsu)   := io.lsu_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_lsu) := io.lsu_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_lsu)   := io.lsu_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_lsu)  := io.lsu_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_lsu) := io.lsu_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_lsu) := io.lsu_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_lsu)   := io.lsu_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_lsu)     := io.lsu_ex_wb_entry.call_cor
    RET_COR_TAB(index_lsu)      := io.lsu_ex_wb_entry.ret_cor
    IS_BRANCH_TAB(index_lsu)    := io.lsu_ex_wb_entry.is_branch
    IS_CALL_TAB(index_lsu)      := io.lsu_ex_wb_entry.is_call
    IS_RET_TAB(index_lsu)       := io.lsu_ex_wb_entry.is_ret
    IS_JUMP_TAB(index_lsu)       := io.lsu_ex_wb_entry.is_jump
    TARGET_PC_TAB(index_lsu)    := io.lsu_ex_wb_entry.target_pc
    DEC_VLD_TAB(index_lsu)      := True
  } .otherwise{ }

  val index_csr = io.csr_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.csr_ex_wb_entry.commit_req){
    INSTR_TAB(index_csr)        := io.csr_ex_wb_entry.instr
    PC_TAB(index_csr)           := io.csr_ex_wb_entry.pc
    REG_WEN_TAB(index_csr)      := io.csr_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_csr)    := io.csr_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_csr)    := io.csr_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_csr)      := io.csr_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_csr)    := io.csr_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_csr)    := io.csr_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_csr)   := io.csr_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_csr) := io.csr_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_csr)   := io.csr_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_csr)  := io.csr_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_csr) := io.csr_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_csr) := io.csr_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_csr)   := io.csr_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_csr)     := io.csr_ex_wb_entry.call_cor
    RET_COR_TAB(index_csr)      := io.csr_ex_wb_entry.ret_cor
    IS_BRANCH_TAB(index_csr)    := io.csr_ex_wb_entry.is_branch
    IS_CALL_TAB(index_csr)      := io.csr_ex_wb_entry.is_call
    IS_RET_TAB(index_csr)       := io.csr_ex_wb_entry.is_ret
    IS_JUMP_TAB(index_csr)       := io.csr_ex_wb_entry.is_jump
    TARGET_PC_TAB(index_csr)    := io.csr_ex_wb_entry.target_pc
    DEC_VLD_TAB(index_csr)      := True
  } .otherwise{ }

  val index_nopu = io.nopu_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.nopu_ex_wb_entry.commit_req){
    INSTR_TAB(index_nopu)        := io.nopu_ex_wb_entry.instr
    PC_TAB(index_nopu)           := io.nopu_ex_wb_entry.pc
    REG_WEN_TAB(index_nopu)      := io.nopu_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_nopu)    := io.nopu_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_nopu)    := io.nopu_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_nopu)      := io.nopu_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_nopu)    := io.nopu_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_nopu)    := io.nopu_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_nopu)   := io.nopu_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_nopu) := io.nopu_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_nopu)   := io.nopu_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_nopu)  := io.nopu_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_nopu) := io.nopu_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_nopu) := io.nopu_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_nopu)   := io.nopu_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_nopu)     := io.nopu_ex_wb_entry.call_cor
    RET_COR_TAB(index_nopu)      := io.nopu_ex_wb_entry.ret_cor
    IS_BRANCH_TAB(index_nopu)    := io.nopu_ex_wb_entry.is_branch
    IS_CALL_TAB(index_nopu)      := io.nopu_ex_wb_entry.is_call
    IS_RET_TAB(index_nopu)       := io.nopu_ex_wb_entry.is_ret
    IS_JUMP_TAB(index_nopu)       := io.nopu_ex_wb_entry.is_jump
    TARGET_PC_TAB(index_nopu)    := io.nopu_ex_wb_entry.target_pc
    DEC_VLD_TAB(index_nopu)      := False
  } .otherwise{ }

  when(io.alu_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.alu_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.alu_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.alu_ex_wb_entry.reg_wb_data
  } .elsewhen(io.mul1_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.mul1_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.mul1_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.mul1_ex_wb_entry.reg_wb_data
  } .elsewhen(io.mul2_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.mul2_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.mul2_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.mul2_ex_wb_entry.reg_wb_data
  } .elsewhen(io.divu_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.divu_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.divu_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.divu_ex_wb_entry.reg_wb_data
  } .elsewhen(io.bju_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.bju_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.bju_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.bju_ex_wb_entry.reg_wb_data
  } .elsewhen(io.lsu_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.lsu_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.lsu_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.lsu_ex_wb_entry.reg_wb_data
  } .elsewhen(io.csr_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.csr_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.csr_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.csr_ex_wb_entry.reg_wb_data
  }.otherwise{
    io.wb_regfile_interface.reg_wen := False
    io.wb_regfile_interface.reg_waddr := 0
    io.wb_regfile_interface.reg_wdata := 0
  }
  when(io.csr_ex_wb_entry.commit_req){
    io.wb_csr_interface.reg_wen       := io.csr_ex_wb_entry.csr_wb_en
    io.wb_csr_interface.reg_waddr     := io.csr_ex_wb_entry.csr_wb_addr
    io.wb_csr_interface.reg_wdata     := io.csr_ex_wb_entry.csr_wb_data
  } .otherwise{
    io.wb_csr_interface.reg_wen := False
    io.wb_csr_interface.reg_waddr := 0
    io.wb_csr_interface.reg_wdata := 0
  }
  val alu_commit_req_ack = Reg(Bool()) init(False)
  val alu_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val mul1_commit_req_ack = Reg(Bool()) init(False)
  val mul1_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val mul2_commit_req_ack = Reg(Bool()) init(False)
  val mul2_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val divu_commit_req_ack = Reg(Bool()) init(False)
  val divu_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val bju_commit_req_ack = Reg(Bool()) init(False)
  val bju_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val lsu_commit_req_ack = Reg(Bool()) init(False)
  val lsu_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val csr_commit_req_ack = Reg(Bool()) init(False)
  val csr_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val nop_commit_req_ack = Reg(Bool()) init(False)
  val nop_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)

  when(io.alu_ex_wb_entry.commit_req){
    alu_commit_req_ack := True  // todo with regfile/csr/dcache ack
    alu_recv_id := io.alu_ex_wb_entry.trans_id
  } .otherwise{
    alu_commit_req_ack := False
    alu_recv_id := SCB_IU_DEEPTH
  }
  when(io.mul1_ex_wb_entry.commit_req){
    mul1_commit_req_ack := True  // todo with regfile/csr/dcache ack
    mul1_recv_id := io.mul1_ex_wb_entry.trans_id
  } .otherwise{
    mul1_commit_req_ack := False
    mul1_recv_id := SCB_IU_DEEPTH
  }
  when(io.mul2_ex_wb_entry.commit_req){
    mul2_commit_req_ack := True  // todo with regfile/csr/dcache ack
    mul2_recv_id := io.mul2_ex_wb_entry.trans_id
  } .otherwise{
    mul2_commit_req_ack := False
    mul2_recv_id := SCB_IU_DEEPTH
  }
  when(io.divu_ex_wb_entry.commit_req){
    divu_commit_req_ack := True  // todo with regfile/csr/dcache ack
    divu_recv_id := io.divu_ex_wb_entry.trans_id
  } .otherwise{
    divu_commit_req_ack := False
    divu_recv_id := SCB_IU_DEEPTH
  }
  when(io.bju_ex_wb_entry.commit_req){
    bju_commit_req_ack := True  // todo with regfile/csr/dcache ack
    bju_recv_id := io.bju_ex_wb_entry.trans_id
  } .otherwise{
    bju_commit_req_ack := False
    bju_recv_id := SCB_IU_DEEPTH
  }
  when(io.lsu_ex_wb_entry.commit_req){
    lsu_commit_req_ack := True  // todo with regfile/csr/dcache ack
    lsu_recv_id := io.lsu_ex_wb_entry.trans_id
  } .otherwise{
    lsu_commit_req_ack := False
    lsu_recv_id := SCB_IU_DEEPTH
  }
  when(io.csr_ex_wb_entry.commit_req){
    csr_commit_req_ack := True  // todo with regfile/csr/dcache ack
    csr_recv_id := io.csr_ex_wb_entry.trans_id
  } .otherwise{
    csr_commit_req_ack := False
    csr_recv_id := SCB_IU_DEEPTH
  }
  when(io.nopu_ex_wb_entry.commit_req){
    nop_commit_req_ack := True  // todo with regfile/csr/dcache ack
    nop_recv_id := io.nopu_ex_wb_entry.trans_id
  } .otherwise{
    nop_commit_req_ack := False
    nop_recv_id := SCB_IU_DEEPTH
  }

  val wb_scb_entry_reg_wb_addr = UInt(RegAddrBus bits)
  val wb_scb_entry_reg_wb_data = UInt(RegDataBus bits)
  val wb_scb_entry_reg_wb_en = Bool()
  val wb_scb_entry_csr_wb_addr = UInt(CSRAddrBus bits)
  val wb_scb_entry_csr_wb_data = UInt(CSRDataBus bits)
  val wb_scb_entry_csr_wb_en   = Bool()
  val wb_scb_entry_dcache_wb_en = Bool()
  val wb_scb_entry_dcache_wb_addr = UInt(DataAddrBus bits)
  val wb_scb_entry_dcache_wb_data = UInt(DataBus bits)
  val wb_scb_entry_dcache_rd_en = Bool()
  val wb_scb_entry_dcache_rd_addr = UInt(DataAddrBus bits)
  val wb_scb_entry_dcache_rd_data = UInt(DataBus bits)
  val wb_scb_entry_instr = UInt(InstBus bits)
  val wb_scb_entry_pc = UInt(InstAddrBus bits)
  val wb_scb_entry_trans_id = UInt(SCB_ID_WIDTH bits)
  val wb_scb_entry_dcache_wb_sel = Bits(4 bits)
  val wb_scb_entry_branch_cor = Bool()
  val wb_scb_entry_call_cor = Bool()
  val wb_scb_entry_ret_cor = Bool()
  val wb_scb_entry_is_branch = Bool()
  val wb_scb_entry_is_call = Bool()
  val wb_scb_entry_is_ret = Bool()
  val wb_scb_entry_is_jump = Bool()
  val wb_scb_entry_target_pc = UInt(InstAddrBus bits)
  val wb_scb_entry_dec_valid = Bool()

  val hindex = io.head_ptr
  when(U"0"@@hindex === io.alu_ex_wb_entry.trans_id && io.alu_ex_wb_entry.commit_req){
    wb_scb_entry_instr          := io.alu_ex_wb_entry.instr
    wb_scb_entry_pc             := io.alu_ex_wb_entry.pc
    wb_scb_entry_reg_wb_en      := io.alu_ex_wb_entry.reg_wb_en
    wb_scb_entry_reg_wb_addr    := io.alu_ex_wb_entry.reg_wb_addr
    wb_scb_entry_reg_wb_data    := io.alu_ex_wb_entry.reg_wb_data
    wb_scb_entry_trans_id       := io.alu_ex_wb_entry.trans_id
    wb_scb_entry_dcache_rd_en   := io.alu_ex_wb_entry.dcache_rd_en
    wb_scb_entry_dcache_rd_addr := io.alu_ex_wb_entry.dcache_rd_addr
    wb_scb_entry_dcache_wb_en   := io.alu_ex_wb_entry.dcache_wb_en
    wb_scb_entry_dcache_wb_sel  := io.alu_ex_wb_entry.dcache_wb_sel
    wb_scb_entry_dcache_wb_addr := io.alu_ex_wb_entry.dcache_wb_addr
    wb_scb_entry_dcache_wb_data := io.alu_ex_wb_entry.dcache_wb_data
    wb_scb_entry_branch_cor     := io.alu_ex_wb_entry.branch_cor
    wb_scb_entry_call_cor       := io.alu_ex_wb_entry.call_cor
    wb_scb_entry_ret_cor        := io.alu_ex_wb_entry.ret_cor
    wb_scb_entry_is_branch      := io.alu_ex_wb_entry.is_branch
    wb_scb_entry_is_call        := io.alu_ex_wb_entry.is_call
    wb_scb_entry_is_ret         := io.alu_ex_wb_entry.is_ret
    wb_scb_entry_is_jump         := io.alu_ex_wb_entry.is_jump
    wb_scb_entry_target_pc      := io.alu_ex_wb_entry.target_pc
    wb_scb_entry_csr_wb_addr    := io.alu_ex_wb_entry.csr_wb_addr
    wb_scb_entry_csr_wb_data    := io.alu_ex_wb_entry.csr_wb_data
    wb_scb_entry_csr_wb_en      := io.alu_ex_wb_entry.csr_wb_en
    wb_scb_entry_dec_valid      := True
  } .elsewhen(U"0"@@hindex === io.mul1_ex_wb_entry.trans_id && io.mul1_ex_wb_entry.commit_req){
    wb_scb_entry_instr          := io.mul1_ex_wb_entry.instr
    wb_scb_entry_pc             := io.mul1_ex_wb_entry.pc
    wb_scb_entry_reg_wb_en      := io.mul1_ex_wb_entry.reg_wb_en
    wb_scb_entry_reg_wb_addr    := io.mul1_ex_wb_entry.reg_wb_addr
    wb_scb_entry_reg_wb_data    := io.mul1_ex_wb_entry.reg_wb_data
    wb_scb_entry_trans_id       := io.mul1_ex_wb_entry.trans_id
    wb_scb_entry_dcache_rd_en   := io.mul1_ex_wb_entry.dcache_rd_en
    wb_scb_entry_dcache_rd_addr := io.mul1_ex_wb_entry.dcache_rd_addr
    wb_scb_entry_dcache_wb_en   := io.mul1_ex_wb_entry.dcache_wb_en
    wb_scb_entry_dcache_wb_sel  := io.mul1_ex_wb_entry.dcache_wb_sel
    wb_scb_entry_dcache_wb_addr := io.mul1_ex_wb_entry.dcache_wb_addr
    wb_scb_entry_dcache_wb_data := io.mul1_ex_wb_entry.dcache_wb_data
    wb_scb_entry_branch_cor     := io.mul1_ex_wb_entry.branch_cor
    wb_scb_entry_call_cor       := io.mul1_ex_wb_entry.call_cor
    wb_scb_entry_ret_cor        := io.mul1_ex_wb_entry.ret_cor
    wb_scb_entry_is_branch      := io.mul1_ex_wb_entry.is_branch
    wb_scb_entry_is_call        := io.mul1_ex_wb_entry.is_call
    wb_scb_entry_is_ret         := io.mul1_ex_wb_entry.is_ret
    wb_scb_entry_is_jump         := io.mul1_ex_wb_entry.is_jump
    wb_scb_entry_target_pc      := io.mul1_ex_wb_entry.target_pc
    wb_scb_entry_csr_wb_addr    := io.mul1_ex_wb_entry.csr_wb_addr
    wb_scb_entry_csr_wb_data    := io.mul1_ex_wb_entry.csr_wb_data
    wb_scb_entry_csr_wb_en      := io.mul1_ex_wb_entry.csr_wb_en
    wb_scb_entry_dec_valid      := True
  } .elsewhen(U"0"@@hindex === io.mul2_ex_wb_entry.trans_id && io.mul2_ex_wb_entry.commit_req){
    wb_scb_entry_instr          := io.mul2_ex_wb_entry.instr
    wb_scb_entry_pc             := io.mul2_ex_wb_entry.pc
    wb_scb_entry_reg_wb_en      := io.mul2_ex_wb_entry.reg_wb_en
    wb_scb_entry_reg_wb_addr    := io.mul2_ex_wb_entry.reg_wb_addr
    wb_scb_entry_reg_wb_data    := io.mul2_ex_wb_entry.reg_wb_data
    wb_scb_entry_trans_id       := io.mul2_ex_wb_entry.trans_id
    wb_scb_entry_dcache_rd_en   := io.mul2_ex_wb_entry.dcache_rd_en
    wb_scb_entry_dcache_rd_addr := io.mul2_ex_wb_entry.dcache_rd_addr
    wb_scb_entry_dcache_wb_en   := io.mul2_ex_wb_entry.dcache_wb_en
    wb_scb_entry_dcache_wb_sel  := io.mul2_ex_wb_entry.dcache_wb_sel
    wb_scb_entry_dcache_wb_addr := io.mul2_ex_wb_entry.dcache_wb_addr
    wb_scb_entry_dcache_wb_data := io.mul2_ex_wb_entry.dcache_wb_data
    wb_scb_entry_branch_cor     := io.mul2_ex_wb_entry.branch_cor
    wb_scb_entry_call_cor       := io.mul2_ex_wb_entry.call_cor
    wb_scb_entry_ret_cor        := io.mul2_ex_wb_entry.ret_cor
    wb_scb_entry_is_branch      := io.mul2_ex_wb_entry.is_branch
    wb_scb_entry_is_call        := io.mul2_ex_wb_entry.is_call
    wb_scb_entry_is_ret         := io.mul2_ex_wb_entry.is_ret
    wb_scb_entry_is_jump         := io.mul2_ex_wb_entry.is_jump
    wb_scb_entry_target_pc      := io.mul2_ex_wb_entry.target_pc
    wb_scb_entry_csr_wb_addr    := io.mul2_ex_wb_entry.csr_wb_addr
    wb_scb_entry_csr_wb_data    := io.mul2_ex_wb_entry.csr_wb_data
    wb_scb_entry_csr_wb_en      := io.mul2_ex_wb_entry.csr_wb_en
    wb_scb_entry_dec_valid      := True
  } .elsewhen(U"0"@@hindex === io.divu_ex_wb_entry.trans_id && io.divu_ex_wb_entry.commit_req){
    wb_scb_entry_instr          := io.divu_ex_wb_entry.instr
    wb_scb_entry_pc             := io.divu_ex_wb_entry.pc
    wb_scb_entry_reg_wb_en      := io.divu_ex_wb_entry.reg_wb_en
    wb_scb_entry_reg_wb_addr    := io.divu_ex_wb_entry.reg_wb_addr
    wb_scb_entry_reg_wb_data    := io.divu_ex_wb_entry.reg_wb_data
    wb_scb_entry_trans_id       := io.divu_ex_wb_entry.trans_id
    wb_scb_entry_dcache_rd_en   := io.divu_ex_wb_entry.dcache_rd_en
    wb_scb_entry_dcache_rd_addr := io.divu_ex_wb_entry.dcache_rd_addr
    wb_scb_entry_dcache_wb_en   := io.divu_ex_wb_entry.dcache_wb_en
    wb_scb_entry_dcache_wb_sel  := io.divu_ex_wb_entry.dcache_wb_sel
    wb_scb_entry_dcache_wb_addr := io.divu_ex_wb_entry.dcache_wb_addr
    wb_scb_entry_dcache_wb_data := io.divu_ex_wb_entry.dcache_wb_data
    wb_scb_entry_branch_cor     := io.divu_ex_wb_entry.branch_cor
    wb_scb_entry_call_cor       := io.divu_ex_wb_entry.call_cor
    wb_scb_entry_ret_cor        := io.divu_ex_wb_entry.ret_cor
    wb_scb_entry_is_branch      := io.divu_ex_wb_entry.is_branch
    wb_scb_entry_is_call        := io.divu_ex_wb_entry.is_call
    wb_scb_entry_is_ret         := io.divu_ex_wb_entry.is_ret
    wb_scb_entry_is_jump         := io.divu_ex_wb_entry.is_jump
    wb_scb_entry_target_pc      := io.divu_ex_wb_entry.target_pc
    wb_scb_entry_csr_wb_addr    := io.divu_ex_wb_entry.csr_wb_addr
    wb_scb_entry_csr_wb_data    := io.divu_ex_wb_entry.csr_wb_data
    wb_scb_entry_csr_wb_en      := io.divu_ex_wb_entry.csr_wb_en
    wb_scb_entry_dec_valid      := True
  } .elsewhen(U"0"@@hindex === io.bju_ex_wb_entry.trans_id && io.bju_ex_wb_entry.commit_req){
    wb_scb_entry_instr          := io.bju_ex_wb_entry.instr
    wb_scb_entry_pc             := io.bju_ex_wb_entry.pc
    wb_scb_entry_reg_wb_en      := io.bju_ex_wb_entry.reg_wb_en
    wb_scb_entry_reg_wb_addr    := io.bju_ex_wb_entry.reg_wb_addr
    wb_scb_entry_reg_wb_data    := io.bju_ex_wb_entry.reg_wb_data
    wb_scb_entry_trans_id       := io.bju_ex_wb_entry.trans_id
    wb_scb_entry_dcache_rd_en   := io.bju_ex_wb_entry.dcache_rd_en
    wb_scb_entry_dcache_rd_addr := io.bju_ex_wb_entry.dcache_rd_addr
    wb_scb_entry_dcache_wb_en   := io.bju_ex_wb_entry.dcache_wb_en
    wb_scb_entry_dcache_wb_sel  := io.bju_ex_wb_entry.dcache_wb_sel
    wb_scb_entry_dcache_wb_addr := io.bju_ex_wb_entry.dcache_wb_addr
    wb_scb_entry_dcache_wb_data := io.bju_ex_wb_entry.dcache_wb_data
    wb_scb_entry_branch_cor     := io.bju_ex_wb_entry.branch_cor
    wb_scb_entry_call_cor       := io.bju_ex_wb_entry.call_cor
    wb_scb_entry_ret_cor        := io.bju_ex_wb_entry.ret_cor
    wb_scb_entry_is_branch      := io.bju_ex_wb_entry.is_branch
    wb_scb_entry_is_call        := io.bju_ex_wb_entry.is_call
    wb_scb_entry_is_ret         := io.bju_ex_wb_entry.is_ret
    wb_scb_entry_is_jump         := io.bju_ex_wb_entry.is_jump
    wb_scb_entry_target_pc      := io.bju_ex_wb_entry.target_pc
    wb_scb_entry_csr_wb_addr    := io.bju_ex_wb_entry.csr_wb_addr
    wb_scb_entry_csr_wb_data    := io.bju_ex_wb_entry.csr_wb_data
    wb_scb_entry_csr_wb_en      := io.bju_ex_wb_entry.csr_wb_en
    wb_scb_entry_dec_valid      := True
  } .elsewhen(U"0"@@hindex === io.lsu_ex_wb_entry.trans_id && io.lsu_ex_wb_entry.commit_req){
    wb_scb_entry_instr          := io.lsu_ex_wb_entry.instr
    wb_scb_entry_pc             := io.lsu_ex_wb_entry.pc
    wb_scb_entry_reg_wb_en      := io.lsu_ex_wb_entry.reg_wb_en
    wb_scb_entry_reg_wb_addr    := io.lsu_ex_wb_entry.reg_wb_addr
    wb_scb_entry_reg_wb_data    := io.lsu_ex_wb_entry.reg_wb_data
    wb_scb_entry_trans_id       := io.lsu_ex_wb_entry.trans_id
    wb_scb_entry_dcache_rd_en   := io.lsu_ex_wb_entry.dcache_rd_en
    wb_scb_entry_dcache_rd_addr := io.lsu_ex_wb_entry.dcache_rd_addr
    wb_scb_entry_dcache_wb_en   := io.lsu_ex_wb_entry.dcache_wb_en
    wb_scb_entry_dcache_wb_sel  := io.lsu_ex_wb_entry.dcache_wb_sel
    wb_scb_entry_dcache_wb_addr := io.lsu_ex_wb_entry.dcache_wb_addr
    wb_scb_entry_dcache_wb_data := io.lsu_ex_wb_entry.dcache_wb_data
    wb_scb_entry_branch_cor     := io.lsu_ex_wb_entry.branch_cor
    wb_scb_entry_call_cor       := io.lsu_ex_wb_entry.call_cor
    wb_scb_entry_ret_cor        := io.lsu_ex_wb_entry.ret_cor
    wb_scb_entry_is_branch      := io.lsu_ex_wb_entry.is_branch
    wb_scb_entry_is_call        := io.lsu_ex_wb_entry.is_call
    wb_scb_entry_is_ret         := io.lsu_ex_wb_entry.is_ret
    wb_scb_entry_is_jump         := io.lsu_ex_wb_entry.is_jump
    wb_scb_entry_target_pc      := io.lsu_ex_wb_entry.target_pc
    wb_scb_entry_csr_wb_addr    := io.lsu_ex_wb_entry.csr_wb_addr
    wb_scb_entry_csr_wb_data    := io.lsu_ex_wb_entry.csr_wb_data
    wb_scb_entry_csr_wb_en      := io.lsu_ex_wb_entry.csr_wb_en
    wb_scb_entry_dec_valid      := True
  } .elsewhen(U"0"@@hindex === io.csr_ex_wb_entry.trans_id && io.csr_ex_wb_entry.commit_req){
    wb_scb_entry_instr          := io.csr_ex_wb_entry.instr
    wb_scb_entry_pc             := io.csr_ex_wb_entry.pc
    wb_scb_entry_reg_wb_en      := io.csr_ex_wb_entry.reg_wb_en
    wb_scb_entry_reg_wb_addr    := io.csr_ex_wb_entry.reg_wb_addr
    wb_scb_entry_reg_wb_data    := io.csr_ex_wb_entry.reg_wb_data
    wb_scb_entry_trans_id       := io.csr_ex_wb_entry.trans_id
    wb_scb_entry_dcache_rd_en   := io.csr_ex_wb_entry.dcache_rd_en
    wb_scb_entry_dcache_rd_addr := io.csr_ex_wb_entry.dcache_rd_addr
    wb_scb_entry_dcache_wb_en   := io.csr_ex_wb_entry.dcache_wb_en
    wb_scb_entry_dcache_wb_sel  := io.csr_ex_wb_entry.dcache_wb_sel
    wb_scb_entry_dcache_wb_addr := io.csr_ex_wb_entry.dcache_wb_addr
    wb_scb_entry_dcache_wb_data := io.csr_ex_wb_entry.dcache_wb_data
    wb_scb_entry_branch_cor     := io.csr_ex_wb_entry.branch_cor
    wb_scb_entry_call_cor       := io.csr_ex_wb_entry.call_cor
    wb_scb_entry_ret_cor        := io.csr_ex_wb_entry.ret_cor
    wb_scb_entry_is_branch      := io.csr_ex_wb_entry.is_branch
    wb_scb_entry_is_call        := io.csr_ex_wb_entry.is_call
    wb_scb_entry_is_ret         := io.csr_ex_wb_entry.is_ret
    wb_scb_entry_is_jump         := io.csr_ex_wb_entry.is_jump
    wb_scb_entry_target_pc      := io.csr_ex_wb_entry.target_pc
    wb_scb_entry_csr_wb_addr    := io.csr_ex_wb_entry.csr_wb_addr
    wb_scb_entry_csr_wb_data    := io.csr_ex_wb_entry.csr_wb_data
    wb_scb_entry_csr_wb_en      := io.csr_ex_wb_entry.csr_wb_en
    wb_scb_entry_dec_valid      := True
  } .elsewhen(U"0"@@hindex === io.nopu_ex_wb_entry.trans_id && io.nopu_ex_wb_entry.commit_req){
    wb_scb_entry_instr          := io.nopu_ex_wb_entry.instr
    wb_scb_entry_pc             := io.nopu_ex_wb_entry.pc
    wb_scb_entry_reg_wb_en      := io.nopu_ex_wb_entry.reg_wb_en
    wb_scb_entry_reg_wb_addr    := io.nopu_ex_wb_entry.reg_wb_addr
    wb_scb_entry_reg_wb_data    := io.nopu_ex_wb_entry.reg_wb_data
    wb_scb_entry_trans_id       := io.nopu_ex_wb_entry.trans_id
    wb_scb_entry_dcache_rd_en   := io.nopu_ex_wb_entry.dcache_rd_en
    wb_scb_entry_dcache_rd_addr := io.nopu_ex_wb_entry.dcache_rd_addr
    wb_scb_entry_dcache_wb_en   := io.nopu_ex_wb_entry.dcache_wb_en
    wb_scb_entry_dcache_wb_sel  := io.nopu_ex_wb_entry.dcache_wb_sel
    wb_scb_entry_dcache_wb_addr := io.nopu_ex_wb_entry.dcache_wb_addr
    wb_scb_entry_dcache_wb_data := io.nopu_ex_wb_entry.dcache_wb_data
    wb_scb_entry_branch_cor     := io.nopu_ex_wb_entry.branch_cor
    wb_scb_entry_call_cor       := io.nopu_ex_wb_entry.call_cor
    wb_scb_entry_ret_cor        := io.nopu_ex_wb_entry.ret_cor
    wb_scb_entry_is_branch      := io.nopu_ex_wb_entry.is_branch
    wb_scb_entry_is_call        := io.nopu_ex_wb_entry.is_call
    wb_scb_entry_is_ret         := io.nopu_ex_wb_entry.is_ret
    wb_scb_entry_is_jump         := io.nopu_ex_wb_entry.is_jump
    wb_scb_entry_target_pc      := io.nopu_ex_wb_entry.target_pc
    wb_scb_entry_csr_wb_addr    := io.nopu_ex_wb_entry.csr_wb_addr
    wb_scb_entry_csr_wb_data    := io.nopu_ex_wb_entry.csr_wb_data
    wb_scb_entry_csr_wb_en      := io.nopu_ex_wb_entry.csr_wb_en
    wb_scb_entry_dec_valid      := False
  }.otherwise{
    wb_scb_entry_instr := INSTR_TAB(hindex)
    wb_scb_entry_pc := PC_TAB(hindex)
    wb_scb_entry_reg_wb_en := REG_WEN_TAB(hindex)
    wb_scb_entry_reg_wb_addr := REG_WADDR_TAB(hindex)
    wb_scb_entry_reg_wb_data := REG_WDATA_TAB(hindex)
    wb_scb_entry_trans_id := U"0"@@hindex
    wb_scb_entry_dcache_rd_en := DCACHE_REN_TAB(hindex)
    wb_scb_entry_dcache_rd_addr := DCACHE_RADDR_TAB(hindex)
    //io.ex_wb_entry.dcache_rd_data := wb_scb_entry_dcache_rd_data
    wb_scb_entry_dcache_wb_en := DCACHE_WEN_TAB(hindex)
    wb_scb_entry_dcache_wb_sel := DCACHE_WSEL_TAB(hindex)
    wb_scb_entry_dcache_wb_addr := DCACHE_WADDR_TAB(hindex)
    wb_scb_entry_dcache_wb_data := DCACHE_WDATA_TAB(hindex)
    wb_scb_entry_branch_cor := BRANCH_COR_TAB(hindex)
    wb_scb_entry_call_cor := CALL_COR_TAB(hindex)
    wb_scb_entry_ret_cor  := RET_COR_TAB(hindex)
    wb_scb_entry_is_branch := IS_BRANCH_TAB(hindex)
    wb_scb_entry_is_call := IS_CALL_TAB(hindex)
    wb_scb_entry_is_ret  := IS_RET_TAB(hindex)
    wb_scb_entry_is_jump  := IS_JUMP_TAB(hindex)
    wb_scb_entry_target_pc  := TARGET_PC_TAB(hindex)
    wb_scb_entry_csr_wb_addr    := CSR_WADDR_TAB(hindex)
    wb_scb_entry_csr_wb_data    := CSR_WDATA_TAB(hindex)
    wb_scb_entry_csr_wb_en      := CSR_WEN_TAB(hindex)
    wb_scb_entry_dec_valid      := DEC_VLD_TAB(hindex)
  }

  io.wb_scb_entry.instr := wb_scb_entry_instr
  io.wb_scb_entry.pc := wb_scb_entry_pc
  io.wb_scb_entry.reg_wb_en := wb_scb_entry_reg_wb_en
  io.wb_scb_entry.reg_wb_addr := wb_scb_entry_reg_wb_addr
  io.wb_scb_entry.reg_wb_data := wb_scb_entry_reg_wb_data
  io.wb_scb_entry.trans_id := wb_scb_entry_trans_id
  io.wb_scb_entry.dcache_rd_en :=wb_scb_entry_dcache_rd_en
  io.wb_scb_entry.dcache_rd_addr := wb_scb_entry_dcache_rd_addr
  //io.ex_wb_entry.dcache_rd_data := io.wb_scb_entry.dcache_rd_data
  io.wb_scb_entry.dcache_wb_en := wb_scb_entry_dcache_wb_en
  io.wb_scb_entry.dcache_wb_sel := wb_scb_entry_dcache_wb_sel
  io.wb_scb_entry.dcache_wb_addr := wb_scb_entry_dcache_wb_addr
  io.wb_scb_entry.dcache_wb_data := wb_scb_entry_dcache_wb_data
  io.wb_scb_entry.branch_cor := wb_scb_entry_branch_cor
  io.wb_scb_entry.call_cor := wb_scb_entry_call_cor
  io.wb_scb_entry.ret_cor := wb_scb_entry_ret_cor
  io.wb_scb_entry.is_branch := wb_scb_entry_is_branch
  io.wb_scb_entry.is_call := wb_scb_entry_is_call
  io.wb_scb_entry.is_ret := wb_scb_entry_is_ret
  io.wb_scb_entry.is_jump := wb_scb_entry_is_jump
  io.wb_scb_entry.target_pc := wb_scb_entry_target_pc
  io.wb_scb_entry.csr_wb_en   := wb_scb_entry_csr_wb_en
  io.wb_scb_entry.csr_wb_addr := wb_scb_entry_csr_wb_addr
  io.wb_scb_entry.csr_wb_data := wb_scb_entry_csr_wb_data
  io.wb_scb_entry.dec_valid := wb_scb_entry_dec_valid

  io.alu_ex_wb_entry.commit_ack := alu_commit_req_ack
  io.alu_ex_wb_entry.recv_id    := alu_recv_id
  io.mul1_ex_wb_entry.commit_ack := mul1_commit_req_ack
  io.mul1_ex_wb_entry.recv_id    := mul1_recv_id
  io.mul2_ex_wb_entry.commit_ack := mul2_commit_req_ack
  io.mul2_ex_wb_entry.recv_id    := mul2_recv_id
  io.divu_ex_wb_entry.commit_ack := divu_commit_req_ack
  io.divu_ex_wb_entry.recv_id    := divu_recv_id
  io.bju_ex_wb_entry.commit_ack := bju_commit_req_ack
  io.bju_ex_wb_entry.recv_id    := bju_recv_id
  io.lsu_ex_wb_entry.commit_ack := lsu_commit_req_ack
  io.lsu_ex_wb_entry.recv_id    := lsu_recv_id
  io.csr_ex_wb_entry.commit_ack := csr_commit_req_ack
  io.csr_ex_wb_entry.recv_id    := csr_recv_id
  io.nopu_ex_wb_entry.commit_ack := nop_commit_req_ack
  io.nopu_ex_wb_entry.recv_id    := nop_recv_id

}