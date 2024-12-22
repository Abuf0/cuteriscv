package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._
import BundleImplicit._   // connect with master & slave

case class scoreboard () extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val flush = in Bool()
    val issue_intr_entry = slave(instr_entry(CoreConfig())) // from id2issue
    val issue_branch_predict_entry = slave(branch_predict_entry(CoreConfig())) // from id2issue
    val issue_dec_entry = slave(decorder_entry(CoreConfig())) // from id2issue
    //val issue_reaop_ack = in Bool() // from issue readop
    //val scb_instr_entry = master(instr_entry(CoreConfig())) // to issue readop
    val scb_branch_predict_entry = master(branch_predict_entry(CoreConfig())) // to ex stage
    //val scb_dec_entry = master(decorder_entry(CoreConfig()))  // to issue readop
    //val scb_fwd = out Bool()  // to issue readop
    //val stall = out Bool()
    val scb_readop_ro = out Vec(Bool(),REG_NUM) // to regfile
    val scb_readop_wo = out Vec(Bool(),REG_NUM) // to regfile
    val scb_readop_i = in Vec(UInt(RegDataBus bits),REG_NUM) // from regfile not used before commit
    val scb_readop_csr_i =  in Vec(UInt(CSRDataBus bits),CSR_NUM) // from csr regfile not used before commit // todo: 接口太多了。需要改成reg接口
    val scb_readop_wb_i = in Vec(UInt(RegDataBus bits),REG_NUM) // from regfile wb
    //val scb_readop_csr_wb_i =  in Vec(UInt(CSRDataBus bits),CSR_NUM) // from csr regfile wb // todo: 接口太多了。需要改成reg接口
    val csr_wb_read_addr = out UInt(CSRAddrBus bits)
    val csr_wb_read_data = in UInt(CSRDataBus bits)
    // todo ： 修改 csr_oprand_entry 的接口（无需rs2，新增csr），修改csr unit模块
    val alu_oprand_entry = master(operand_entry(CoreConfig()))  // with ALUU
    val mul1_oprand_entry = master(operand_entry(CoreConfig()))  // with MUL1
    val mul2_oprand_entry = master(operand_entry(CoreConfig()))  // with MUL2
    val div_oprand_entry = master(operand_entry(CoreConfig()))  // with DIV
    val bju_oprand_entry = master(operand_entry(CoreConfig()))  // with BJUU
    val bju_mis_predict_entry = slave(branch_mispredict_entry(CoreConfig()))
    val lsu_oprand_entry = master(operand_entry(CoreConfig()))  // with LSU
    val csr_oprand_entry = master(operand_entry(CoreConfig()))  // with CSR
    val nopu_oprand_entry = master(operand_entry(CoreConfig()))  // with NOP
    val alu_ex_entry = slave(alu_res_entry(CoreConfig())) // from ex stage
    val mul1_ex_entry = slave(mul_res_entry(CoreConfig())) // from ex stage
    val mul2_ex_entry = slave(mul_res_entry(CoreConfig())) // from ex stage
    val div_ex_entry = slave(div_res_entry(CoreConfig())) // from ex stage
    val bju_ex_entry = slave(bju_res_entry(CoreConfig())) // from ex stage
    val lsu_ex_entry = slave(lsu_res_entry(CoreConfig())) // from ex stage
    val csr_ex_entry = slave(csr_res_entry(CoreConfig())) // from ex stage
    val nopu_ex_entry = slave(nop_res_entry(CoreConfig())) // from ex stage
    val alu_ex_wb_entry = master(commit_entry(CoreConfig())) // to wb stage
    val mul1_ex_wb_entry = master(commit_entry(CoreConfig()))
    val mul2_ex_wb_entry = master(commit_entry(CoreConfig()))
    val divu_ex_wb_entry = master(commit_entry(CoreConfig()))
    val bju_ex_wb_entry = master(commit_entry(CoreConfig()))
    val lsu_ex_wb_entry = master(commit_entry(CoreConfig()))
    val csr_ex_wb_entry = master(commit_entry(CoreConfig()))
    val nopu_ex_wb_entry = master(commit_entry(CoreConfig()))
    val wb_commit_entry = master(commit_entry(CoreConfig())) // to commit stage
    val wb_ras_entry = master(commit_entry(CoreConfig()))
    val wb_scb_entry = slave(commit_entry(CoreConfig()))  // from wb
    val head_ptr = out UInt(SCB_INSTR_WIDTH bits)  // to wb
    val scb_full = out Bool() // to instr queue
  }

  //val dec_package = io.scb_dec_entry.asBits
  //val dec_size = dec_package.getWidth
  //val fu_size = 1+2+RegAddrBus*5+OP_TYPE().getBitsWidth
  val INSTR_TAB = Vec(Reg(UInt(InstBus bits)) init(0), SCB_INSTR_DEEPTH)
  val ALU_SEL_TAB = Vec(Reg(Bits(io.issue_dec_entry.alu_sel.getBitsWidth bits)) init(0), SCB_INSTR_DEEPTH)
  val OP_TYPE_TAB = Vec(Reg(Bits(io.issue_dec_entry.op_type.getBitsWidth bits)) init(0), SCB_INSTR_DEEPTH)
  val RS1_TAB = Vec(Reg(UInt(RegAddrBus+2 bits)) init(0), SCB_INSTR_DEEPTH)
  val RS2_TAB = Vec(Reg(UInt(RegAddrBus+2 bits)) init(0), SCB_INSTR_DEEPTH)
  val RD_TAB = Vec(Reg(UInt(RegAddrBus+2 bits)) init(0), SCB_INSTR_DEEPTH)
  val CSR_TAB = Vec(Reg(UInt(CSRAddrBus+2 bits)) init(0), SCB_INSTR_DEEPTH)
  val IMM_TAB = Vec(Reg(SInt(ImmBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DEC_VLD = Vec(Reg(Bool()) init(False),SCB_INSTR_DEEPTH)
  val PC_TAB = Vec(Reg(UInt(InstAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val BP_TAB = Vec(Reg(UInt(InstAddrBus+InstAddrBus+6 bits)) init(0), SCB_INSTR_DEEPTH)
  val PRE_TAB = Vec(Reg(Bool()) init(False),SCB_INSTR_DEEPTH)

  val wptr = Reg(UInt(SCB_INSTR_WIDTH+1 bit)) init(0)
  val rptr = Reg(UInt(SCB_INSTR_WIDTH+1 bit)) init(0)
  val wptr_next = UInt(SCB_INSTR_WIDTH+1 bit)
  val rptr_next = UInt(SCB_INSTR_WIDTH+1 bit)
  val instr_tab_full = (wptr_next(SCB_INSTR_WIDTH) ^ rptr(SCB_INSTR_WIDTH)) && (wptr_next(SCB_INSTR_WIDTH-1 downto 0) === rptr(SCB_INSTR_WIDTH-1 downto 0))
  val instr_tab_full_real = (wptr(SCB_INSTR_WIDTH) ^ rptr(SCB_INSTR_WIDTH)) && (wptr(SCB_INSTR_WIDTH-1 downto 0) === rptr(SCB_INSTR_WIDTH-1 downto 0))
  val instr_tab_empty = (rptr === wptr)
  io.scb_full := instr_tab_full_real
  val instr_end = Bool()
  //val instr_end_tab = Vec(Bool(),REG_NUM)
  //val instr_end_tab = Vec.fill(REG_NUM)(False)
  val instr_end_tab = Vec(Reg(Bool()) init(False),SCB_INSTR_DEEPTH)
  val ex_wb_req = Vec(Vec(Reg(Bool()) init(False),SCB_INSTR_DEEPTH) , 8)
  val wb_commit_req = Vec(Reg(Bool()) init(False),SCB_INSTR_DEEPTH)

  val ex_wb_entry_reg_wb_addr = Vec(Reg(UInt(RegAddrBus bits)) init(0) , 8)
  val ex_wb_entry_reg_wb_data = Vec(Reg(UInt(RegDataBus bits)) init(0) , 8)
  val ex_wb_entry_reg_wb_en = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_csr_wb_addr = Vec(Reg(UInt(CSRAddrBus bits)) init(0) , 8)
  val ex_wb_entry_csr_wb_data = Vec(Reg(UInt(CSRDataBus bits)) init(0) , 8)
  val ex_wb_entry_csr_wb_en = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_dcache_wb_en = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_dcache_wb_addr = Vec(Reg(UInt(DataAddrBus bits)) init(0) , 8)
  val ex_wb_entry_dcache_wb_data = Vec(Reg(UInt(DataBus bits)) init(0) , 8)
  val ex_wb_entry_dcache_rd_en = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_dcache_rd_addr = Vec(Reg(UInt(DataAddrBus bits)) init(0) , 8)
  val ex_wb_entry_dcache_rd_data = Vec(UInt(DataBus bits) , 8)
  //val ex_wb_entry_commit_req = Reg(Bool()) init(False)
  val ex_wb_entry_commit_req = Vec(Bool() , 8)
  val ex_wb_entry_instr = Vec(Reg(UInt(InstBus bits)) init(0) , 8)
  val ex_wb_entry_trans_id = Vec(Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH) , 8)
  val ex_wb_entry_dcache_wb_sel = Vec(Reg(Bits(4 bits)) init(B"1111") , 8)
  val ex_wb_entry_pc = Vec(Reg(UInt(InstAddrBus bits)) init(0) , 8)

  val ex_wb_entry_branch_cor = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_call_cor = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_ret_cor = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_target_pc = Vec(Reg(UInt(InstAddrBus bits)) init(0) , 8)

  val ex_wb_entry_is_branch = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_is_call = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_is_ret = Vec(Reg(Bool()) init(False) , 8)
  val ex_wb_entry_is_jump = Vec(Reg(Bool()) init(False) , 8)

  //val ex_wb_entry_alusel = Reg(Bits(io.issue_dec_entry.alu_sel.getBitsWidth bits)) init(B(ALU_UNIT_SEL.NOPU))

  io.alu_ex_wb_entry.reg_wb_addr    := ex_wb_entry_reg_wb_addr(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.reg_wb_data    := ex_wb_entry_reg_wb_data(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.reg_wb_en      := ex_wb_entry_reg_wb_en(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.csr_wb_addr    := ex_wb_entry_csr_wb_addr(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.csr_wb_data    := ex_wb_entry_csr_wb_data(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.csr_wb_en      := ex_wb_entry_csr_wb_en(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.dcache_wb_addr := ex_wb_entry_dcache_wb_addr(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.dcache_wb_data := ex_wb_entry_dcache_wb_data(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.dcache_wb_en   := ex_wb_entry_dcache_wb_en(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.dcache_wb_sel  := ex_wb_entry_dcache_wb_sel(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.commit_req     := ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.trans_id       := ex_wb_entry_trans_id(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.dcache_rd_addr := ex_wb_entry_dcache_rd_addr(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.dcache_rd_en   := ex_wb_entry_dcache_rd_en(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.instr          := ex_wb_entry_instr(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.pc             := ex_wb_entry_pc(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.branch_cor     := ex_wb_entry_branch_cor(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.call_cor       := ex_wb_entry_call_cor(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.ret_cor        := ex_wb_entry_ret_cor(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.target_pc      := ex_wb_entry_target_pc(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.is_branch     := ex_wb_entry_is_branch(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.is_call       := ex_wb_entry_is_call(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.is_ret        := ex_wb_entry_is_ret(U(B(ALU_UNIT_SEL.ALUU)))
  io.alu_ex_wb_entry.is_jump        := ex_wb_entry_is_jump(U(B(ALU_UNIT_SEL.ALUU)))
  //io.alu_ex_wb_entry.alusel         := ex_wb_entry_alusel(U(B(ALU_UNIT_SEL.ALUU)))
  //ex_wb_entry_dcache_rd_data(U(B(ALU_UNIT_SEL.ALUU)))    := io.alu_ex_wb_entry.dcache_rd_data

  io.mul1_ex_wb_entry.reg_wb_addr    := ex_wb_entry_reg_wb_addr(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.reg_wb_data    := ex_wb_entry_reg_wb_data(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.reg_wb_en      := ex_wb_entry_reg_wb_en(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.csr_wb_addr    := ex_wb_entry_csr_wb_addr(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.csr_wb_data    := ex_wb_entry_csr_wb_data(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.csr_wb_en      := ex_wb_entry_csr_wb_en(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.dcache_wb_addr := ex_wb_entry_dcache_wb_addr(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.dcache_wb_data := ex_wb_entry_dcache_wb_data(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.dcache_wb_en   := ex_wb_entry_dcache_wb_en(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.dcache_wb_sel  := ex_wb_entry_dcache_wb_sel(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.commit_req     := ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.trans_id       := ex_wb_entry_trans_id(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.dcache_rd_addr := ex_wb_entry_dcache_rd_addr(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.dcache_rd_en   := ex_wb_entry_dcache_rd_en(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.instr          := ex_wb_entry_instr(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.pc             := ex_wb_entry_pc(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.branch_cor     := ex_wb_entry_branch_cor(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.call_cor       := ex_wb_entry_call_cor(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.ret_cor        := ex_wb_entry_ret_cor(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.target_pc      := ex_wb_entry_target_pc(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.is_branch     := ex_wb_entry_is_branch(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.is_call       := ex_wb_entry_is_call(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.is_ret        := ex_wb_entry_is_ret(U(B(ALU_UNIT_SEL.MULU1)))
  io.mul1_ex_wb_entry.is_jump        := ex_wb_entry_is_jump(U(B(ALU_UNIT_SEL.MULU1)))
  //io.mul1_ex_wb_entry.alusel         := ex_wb_entry_alusel(U(B(ALU_UNIT_SEL.ALUU)))
  //ex_wb_entry_dcache_rd_data(U(B(ALU_UNIT_SEL.MULU1)))    := io.mul1_ex_wb_entry.dcache_rd_data

  io.mul2_ex_wb_entry.reg_wb_addr    := ex_wb_entry_reg_wb_addr(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.reg_wb_data    := ex_wb_entry_reg_wb_data(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.reg_wb_en      := ex_wb_entry_reg_wb_en(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.csr_wb_addr    := ex_wb_entry_csr_wb_addr(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.csr_wb_data    := ex_wb_entry_csr_wb_data(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.csr_wb_en      := ex_wb_entry_csr_wb_en(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.dcache_wb_addr := ex_wb_entry_dcache_wb_addr(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.dcache_wb_data := ex_wb_entry_dcache_wb_data(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.dcache_wb_en   := ex_wb_entry_dcache_wb_en(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.dcache_wb_sel  := ex_wb_entry_dcache_wb_sel(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.commit_req     := ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.trans_id       := ex_wb_entry_trans_id(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.dcache_rd_addr := ex_wb_entry_dcache_rd_addr(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.dcache_rd_en   := ex_wb_entry_dcache_rd_en(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.instr          := ex_wb_entry_instr(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.pc             := ex_wb_entry_pc(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.branch_cor     := ex_wb_entry_branch_cor(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.call_cor       := ex_wb_entry_call_cor(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.ret_cor        := ex_wb_entry_ret_cor(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.target_pc      := ex_wb_entry_target_pc(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.is_branch     := ex_wb_entry_is_branch(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.is_call       := ex_wb_entry_is_call(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.is_ret        := ex_wb_entry_is_ret(U(B(ALU_UNIT_SEL.MULU2)))
  io.mul2_ex_wb_entry.is_jump        := ex_wb_entry_is_jump(U(B(ALU_UNIT_SEL.MULU2)))
  //io.mul2_ex_wb_entry.alusel         := ex_wb_entry_alusel(U(B(ALU_UNIT_SEL.MULU2)))
  //ex_wb_entry_dcache_rd_data(U(B(ALU_UNIT_SEL.MULU2)))    := io.mul2_ex_wb_entry.dcache_rd_data

  io.divu_ex_wb_entry.reg_wb_addr    := ex_wb_entry_reg_wb_addr(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.reg_wb_data    := ex_wb_entry_reg_wb_data(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.reg_wb_en      := ex_wb_entry_reg_wb_en(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.csr_wb_addr    := ex_wb_entry_csr_wb_addr(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.csr_wb_data    := ex_wb_entry_csr_wb_data(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.csr_wb_en      := ex_wb_entry_csr_wb_en(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.dcache_wb_addr := ex_wb_entry_dcache_wb_addr(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.dcache_wb_data := ex_wb_entry_dcache_wb_data(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.dcache_wb_en   := ex_wb_entry_dcache_wb_en(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.dcache_wb_sel  := ex_wb_entry_dcache_wb_sel(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.commit_req     := ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.trans_id       := ex_wb_entry_trans_id(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.dcache_rd_addr := ex_wb_entry_dcache_rd_addr(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.dcache_rd_en   := ex_wb_entry_dcache_rd_en(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.instr          := ex_wb_entry_instr(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.pc             := ex_wb_entry_pc(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.branch_cor     := ex_wb_entry_branch_cor(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.call_cor       := ex_wb_entry_call_cor(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.ret_cor        := ex_wb_entry_ret_cor(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.target_pc      := ex_wb_entry_target_pc(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.is_branch     := ex_wb_entry_is_branch(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.is_call       := ex_wb_entry_is_call(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.is_ret        := ex_wb_entry_is_ret(U(B(ALU_UNIT_SEL.DIVU)))
  io.divu_ex_wb_entry.is_jump        := ex_wb_entry_is_jump(U(B(ALU_UNIT_SEL.DIVU)))
  //io.divu_ex_wb_entry.alusel         := ex_wb_entry_alusel(U(B(ALU_UNIT_SEL.DIVU)))
  //ex_wb_entry_dcache_rd_data(U(B(ALU_UNIT_SEL.DIVU)))    := io.divu_ex_wb_entry.dcache_rd_data

  io.bju_ex_wb_entry.reg_wb_addr    := ex_wb_entry_reg_wb_addr(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.reg_wb_data    := ex_wb_entry_reg_wb_data(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.reg_wb_en      := ex_wb_entry_reg_wb_en(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.csr_wb_addr    := ex_wb_entry_csr_wb_addr(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.csr_wb_data    := ex_wb_entry_csr_wb_data(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.csr_wb_en      := ex_wb_entry_csr_wb_en(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.dcache_wb_addr := ex_wb_entry_dcache_wb_addr(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.dcache_wb_data := ex_wb_entry_dcache_wb_data(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.dcache_wb_en   := ex_wb_entry_dcache_wb_en(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.dcache_wb_sel  := ex_wb_entry_dcache_wb_sel(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.commit_req     := ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.trans_id       := ex_wb_entry_trans_id(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.dcache_rd_addr := ex_wb_entry_dcache_rd_addr(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.dcache_rd_en   := ex_wb_entry_dcache_rd_en(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.instr          := ex_wb_entry_instr(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.pc             := ex_wb_entry_pc(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.branch_cor     := ex_wb_entry_branch_cor(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.call_cor       := ex_wb_entry_call_cor(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.ret_cor        := ex_wb_entry_ret_cor(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.target_pc      := ex_wb_entry_target_pc(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.is_branch     := ex_wb_entry_is_branch(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.is_call       := ex_wb_entry_is_call(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.is_ret        := ex_wb_entry_is_ret(U(B(ALU_UNIT_SEL.BJU)))
  io.bju_ex_wb_entry.is_jump        := ex_wb_entry_is_jump(U(B(ALU_UNIT_SEL.BJU)))
  //io.bju_ex_wb_entry.alusel         := ex_wb_entry_alusel(U(B(ALU_UNIT_SEL.BJU)))
  //ex_wb_entry_dcache_rd_data(U(B(ALU_UNIT_SEL.BJU)))    := io.bju_ex_wb_entry.dcache_rd_data

  io.lsu_ex_wb_entry.reg_wb_addr    := ex_wb_entry_reg_wb_addr(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.reg_wb_data    := ex_wb_entry_reg_wb_data(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.reg_wb_en      := ex_wb_entry_reg_wb_en(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.csr_wb_addr    := ex_wb_entry_csr_wb_addr(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.csr_wb_data    := ex_wb_entry_csr_wb_data(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.csr_wb_en      := ex_wb_entry_csr_wb_en(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.dcache_wb_addr := ex_wb_entry_dcache_wb_addr(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.dcache_wb_data := ex_wb_entry_dcache_wb_data(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.dcache_wb_en   := ex_wb_entry_dcache_wb_en(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.dcache_wb_sel  := ex_wb_entry_dcache_wb_sel(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.commit_req     := ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.trans_id       := ex_wb_entry_trans_id(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.dcache_rd_addr := ex_wb_entry_dcache_rd_addr(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.dcache_rd_en   := ex_wb_entry_dcache_rd_en(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.instr          := ex_wb_entry_instr(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.pc             := ex_wb_entry_pc(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.branch_cor     := ex_wb_entry_branch_cor(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.call_cor       := ex_wb_entry_call_cor(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.ret_cor        := ex_wb_entry_ret_cor(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.target_pc      := ex_wb_entry_target_pc(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.is_branch     := ex_wb_entry_is_branch(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.is_call       := ex_wb_entry_is_call(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.is_ret        := ex_wb_entry_is_ret(U(B(ALU_UNIT_SEL.LSU)))
  io.lsu_ex_wb_entry.is_jump        := ex_wb_entry_is_jump(U(B(ALU_UNIT_SEL.LSU)))
  //io.lsu_ex_wb_entry.alusel         := ex_wb_entry_alusel(U(B(ALU_UNIT_SEL.LSU)))
  //ex_wb_entry_dcache_rd_data(U(B(ALU_UNIT_SEL.LSU)))    := io.lsu_ex_wb_entry.dcache_rd_data

  io.csr_ex_wb_entry.reg_wb_addr    := ex_wb_entry_reg_wb_addr(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.reg_wb_data    := ex_wb_entry_reg_wb_data(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.reg_wb_en      := ex_wb_entry_reg_wb_en(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.csr_wb_addr    := ex_wb_entry_csr_wb_addr(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.csr_wb_data    := ex_wb_entry_csr_wb_data(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.csr_wb_en      := ex_wb_entry_csr_wb_en(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.dcache_wb_addr := ex_wb_entry_dcache_wb_addr(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.dcache_wb_data := ex_wb_entry_dcache_wb_data(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.dcache_wb_en   := ex_wb_entry_dcache_wb_en(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.dcache_wb_sel  := ex_wb_entry_dcache_wb_sel(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.commit_req     := ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.trans_id       := ex_wb_entry_trans_id(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.dcache_rd_addr := ex_wb_entry_dcache_rd_addr(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.dcache_rd_en   := ex_wb_entry_dcache_rd_en(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.instr          := ex_wb_entry_instr(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.pc             := ex_wb_entry_pc(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.branch_cor     := ex_wb_entry_branch_cor(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.call_cor       := ex_wb_entry_call_cor(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.ret_cor        := ex_wb_entry_ret_cor(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.target_pc      := ex_wb_entry_target_pc(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.is_branch     := ex_wb_entry_is_branch(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.is_call       := ex_wb_entry_is_call(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.is_ret        := ex_wb_entry_is_ret(U(B(ALU_UNIT_SEL.CSR)))
  io.csr_ex_wb_entry.is_jump        := ex_wb_entry_is_jump(U(B(ALU_UNIT_SEL.CSR)))
  //io.csr_ex_wb_entry.alusel         := ex_wb_entry_alusel(U(B(ALU_UNIT_SEL.CSR)))
  //ex_wb_entry_dcache_rd_data(U(B(ALU_UNIT_SEL.CSR)))    := io.csr_ex_wb_entry.dcache_rd_data

  io.nopu_ex_wb_entry.reg_wb_addr    := ex_wb_entry_reg_wb_addr(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.reg_wb_data    := ex_wb_entry_reg_wb_data(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.reg_wb_en      := ex_wb_entry_reg_wb_en(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.csr_wb_addr    := ex_wb_entry_csr_wb_addr(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.csr_wb_data    := ex_wb_entry_csr_wb_data(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.csr_wb_en      := ex_wb_entry_csr_wb_en(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.dcache_wb_addr := ex_wb_entry_dcache_wb_addr(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.dcache_wb_data := ex_wb_entry_dcache_wb_data(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.dcache_wb_en   := ex_wb_entry_dcache_wb_en(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.dcache_wb_sel  := ex_wb_entry_dcache_wb_sel(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.commit_req     := ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.trans_id       := ex_wb_entry_trans_id(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.dcache_rd_addr := ex_wb_entry_dcache_rd_addr(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.dcache_rd_en   := ex_wb_entry_dcache_rd_en(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.instr          := ex_wb_entry_instr(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.pc             := ex_wb_entry_pc(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.branch_cor     := ex_wb_entry_branch_cor(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.call_cor       := ex_wb_entry_call_cor(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.ret_cor        := ex_wb_entry_ret_cor(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.target_pc      := ex_wb_entry_target_pc(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.is_branch     := ex_wb_entry_is_branch(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.is_call       := ex_wb_entry_is_call(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.is_ret        := ex_wb_entry_is_ret(U(B(ALU_UNIT_SEL.NOPU)))
  io.nopu_ex_wb_entry.is_jump        := ex_wb_entry_is_jump(U(B(ALU_UNIT_SEL.NOPU)))
  //io.nopu_ex_wb_entry.alusel         := ex_wb_entry_alusel(U(B(ALU_UNIT_SEL.NOPU)))
  //ex_wb_entry_dcache_rd_data(U(B(ALU_UNIT_SEL.NOPU)))    := io.nopu_ex_wb_entry.dcache_rd_data

  val wb_commit_entry_reg_wb_addr = Reg(UInt(RegAddrBus bits)) init(0)
  val wb_commit_entry_reg_wb_data = Reg(UInt(RegDataBus bits)) init(0)
  val wb_commit_entry_reg_wb_en = Reg(Bool()) init(False)
  val wb_commit_entry_csr_wb_addr = Reg(UInt(CSRAddrBus bits)) init(0)
  val wb_commit_entry_csr_wb_data = Reg(UInt(CSRDataBus bits)) init(0)
  val wb_commit_entry_csr_wb_en = Reg(Bool()) init(False)
  val wb_commit_entry_dcache_wb_en = Reg(Bool()) init(False)
  val wb_commit_entry_dcache_wb_addr = Reg(UInt(DataAddrBus bits)) init(0)
  val wb_commit_entry_dcache_wb_data = Reg(UInt(DataBus bits)) init(0)
  val wb_commit_entry_dcache_rd_en = Reg(Bool()) init(False)
  val wb_commit_entry_dcache_rd_addr = Reg(UInt(DataAddrBus bits)) init(0)
  val wb_commit_entry_dcache_rd_data = UInt(DataBus bits)
  //val wb_commit_entry_commit_req = Reg(Bool()) init(False)
  val wb_commit_entry_commit_req = Bool()
  val wb_commit_entry_instr = Reg(UInt(InstBus bits)) init(0)
  val wb_commit_entry_trans_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val wb_commit_entry_dcache_wb_sel = Reg(Bits(4 bits)) init(B"1111")
  val wb_commit_entry_pc = Reg(UInt(InstAddrBus bits)) init(0)
  val wb_commit_entry_branch_cor = Reg(Bool()) init(False)
  val wb_commit_entry_call_cor = Reg(Bool()) init(False)
  val wb_commit_entry_ret_cor = Reg(Bool()) init(False)
  val wb_commit_entry_target_pc = Reg(UInt(InstAddrBus bits)) init(0)
  val wb_commit_entry_dec_valid = Reg(Bool()) init(False)
  val wb_commit_entry_is_branch = Reg(Bool()) init(False)
  val wb_commit_entry_is_call = Reg(Bool()) init(False)
  val wb_commit_entry_is_ret = Reg(Bool()) init(False)
  val wb_commit_entry_is_jump = Reg(Bool()) init(False)

  io.wb_commit_entry.reg_wb_addr := wb_commit_entry_reg_wb_addr
  io.wb_commit_entry.reg_wb_data := wb_commit_entry_reg_wb_data
  io.wb_commit_entry.reg_wb_en := wb_commit_entry_reg_wb_en
  io.wb_commit_entry.csr_wb_addr := wb_commit_entry_csr_wb_addr
  io.wb_commit_entry.csr_wb_data := wb_commit_entry_csr_wb_data
  io.wb_commit_entry.csr_wb_en := wb_commit_entry_csr_wb_en
  io.wb_commit_entry.dcache_wb_addr := wb_commit_entry_dcache_wb_addr
  io.wb_commit_entry.dcache_wb_data := wb_commit_entry_dcache_wb_data
  io.wb_commit_entry.dcache_wb_en := wb_commit_entry_dcache_wb_en
  io.wb_commit_entry.dcache_wb_sel := wb_commit_entry_dcache_wb_sel
  io.wb_commit_entry.commit_req := wb_commit_entry_commit_req
  io.wb_commit_entry.trans_id := wb_commit_entry_trans_id
  io.wb_commit_entry.dcache_rd_addr := wb_commit_entry_dcache_rd_addr
  io.wb_commit_entry.instr := wb_commit_entry_instr
  wb_commit_entry_dcache_rd_data := io.wb_commit_entry.dcache_rd_data
  io.wb_commit_entry.dcache_rd_en := wb_commit_entry_dcache_rd_en
  io.wb_commit_entry.pc := wb_commit_entry_pc
  io.wb_commit_entry.dec_valid := wb_commit_entry_dec_valid

  io.wb_commit_entry.branch_cor := wb_commit_entry_branch_cor
  io.wb_commit_entry.call_cor := wb_commit_entry_call_cor
  io.wb_commit_entry.ret_cor := wb_commit_entry_ret_cor
  io.wb_commit_entry.target_pc := wb_commit_entry_target_pc
  io.wb_commit_entry.is_branch := wb_commit_entry_is_branch
  io.wb_commit_entry.is_call := wb_commit_entry_is_call
  io.wb_commit_entry.is_ret := wb_commit_entry_is_ret
  io.wb_commit_entry.is_jump := wb_commit_entry_is_jump


  io.wb_ras_entry.is_call := io.wb_commit_entry.is_call
  io.wb_ras_entry.is_ret := io.wb_commit_entry.is_ret
  io.wb_ras_entry.call_cor := io.wb_commit_entry.call_cor
  io.wb_ras_entry.ret_cor := io.wb_commit_entry.ret_cor

  // todo : 如果不加以下默认值，或索引改为:U(B(ALU_UNIT_SEL.XXX))时，会报LATCH问题？？？
  // todo ：以下写法不利于扩展，后续必须修改
  // todo : EX--WB过程中每个ALU都会向WB中的重排序缓存中写入一组数据，代码冗余，尽量修改
  // todo ：WB阶段可能会有多个指令同时向regfile/csr_regfile_wb中写回内存(暂时只支持各ALU一条)，后续需要思考前后顺序
  //ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.ALUU )))   := ex_wb_req(U(B(ALU_UNIT_SEL.ALUU ))).orR
  //ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.MULU1)))   := ex_wb_req(U(B(ALU_UNIT_SEL.MULU1))).orR
  //ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.MULU2)))   := ex_wb_req(U(B(ALU_UNIT_SEL.MULU2))).orR
  //ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.DIVU )))   := ex_wb_req(U(B(ALU_UNIT_SEL.DIVU ))).orR
  //ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.BJU  )))   := ex_wb_req(U(B(ALU_UNIT_SEL.BJU  ))).orR
  //ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.LSU  )))   := ex_wb_req(U(B(ALU_UNIT_SEL.LSU  ))).orR
  //ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.CSR  )))   := ex_wb_req(U(B(ALU_UNIT_SEL.CSR  ))).orR
  //ex_wb_entry_commit_req(U(B(ALU_UNIT_SEL.NOPU )))   := ex_wb_req(U(B(ALU_UNIT_SEL.NOPU ))).orR

  ex_wb_entry_commit_req(0)   := ex_wb_req(U(B(ALU_UNIT_SEL.ALUU ))).orR
  ex_wb_entry_commit_req(1)   := ex_wb_req(U(B(ALU_UNIT_SEL.MULU1))).orR
  ex_wb_entry_commit_req(2)   := ex_wb_req(U(B(ALU_UNIT_SEL.MULU2))).orR
  ex_wb_entry_commit_req(3)   := ex_wb_req(U(B(ALU_UNIT_SEL.DIVU ))).orR
  ex_wb_entry_commit_req(4)   := ex_wb_req(U(B(ALU_UNIT_SEL.BJU  ))).orR
  ex_wb_entry_commit_req(5)   := ex_wb_req(U(B(ALU_UNIT_SEL.LSU  ))).orR
  ex_wb_entry_commit_req(6)   := ex_wb_req(U(B(ALU_UNIT_SEL.CSR  ))).orR
  ex_wb_entry_commit_req(7)   := ex_wb_req(U(B(ALU_UNIT_SEL.NOPU ))).orR

  ex_wb_entry_dcache_rd_data(0) :=  U"0".resized
  ex_wb_entry_dcache_rd_data(1) :=  U"0".resized
  ex_wb_entry_dcache_rd_data(2) :=  U"0".resized
  ex_wb_entry_dcache_rd_data(3) :=  U"0".resized
  ex_wb_entry_dcache_rd_data(4) :=  U"0".resized
  ex_wb_entry_dcache_rd_data(5) :=  U"0".resized
  ex_wb_entry_dcache_rd_data(6) :=  U"0".resized
  ex_wb_entry_dcache_rd_data(7) :=  U"0".resized

  wb_commit_entry_commit_req := wb_commit_req.orR

  //io.scb_branch_predict_entry.setAsReg()

  when(io.flush){
    wptr := rptr + 1
  } .elsewhen(~instr_tab_full_real){
    wptr := wptr_next
  } .otherwise{}

  when(io.flush) {
    rptr := rptr
  } .elsewhen(~instr_tab_empty){
    rptr := rptr_next
  } .otherwise{}

  io.head_ptr := rptr(SCB_INSTR_WIDTH-1 downto 0)

  val PRE_TAB_MASK = Vec(Bool(),SCB_INSTR_DEEPTH)
  for (i <- 0 until SCB_IU_DEEPTH){
    when(i<=rptr(SCB_INSTR_WIDTH-1 downto 0)){
      PRE_TAB_MASK(i) := False
    } .otherwise{
      PRE_TAB_MASK(i) := PRE_TAB(i)
    }
  }

  val (flush_ptr_flag, flush_ptr): (Bool, UInt) = PRE_TAB.sFindFirst(_===True) // get the index of the first element lower than 10
  val (flush_ptr_mask_flag, flush_mask_ptr): (Bool, UInt) = PRE_TAB_MASK.sFindFirst(_===True) // get the index of the first element lower than 10
  val flush_mask_wptr_next = U(rptr(SCB_INSTR_WIDTH)) @@ flush_mask_ptr +1
  val flush_wptr_next = U(wptr(SCB_INSTR_WIDTH)) @@ flush_ptr +1

  /*
  when(io.flush === True){
    when(flush_ptr_mask_flag) {
      wptr_next := flush_mask_wptr_next
    } .otherwise{
      wptr_next := flush_wptr_next
    }
    wptr_next := wptr + 1
  } .elsewhen(io.issue_dec_entry.dec_valid){
    wptr_next := wptr + 1
  }. otherwise{
    wptr_next := wptr
  }
  */

  when(io.issue_dec_entry.dec_valid){
    wptr_next := wptr + 1
  }. otherwise{
    wptr_next := wptr
  }


  //instr_end := instr_end_tab.orR
  /*
  when(io.flush === True){
    //rptr_next := U(rptr(SCB_INSTR_WIDTH)) @@ flush_ptr
    rptr_next := rptr
  } .elsewhen(instr_end_tab(rptr(SCB_INSTR_WIDTH - 1 downto 0)) === True){  // 当rptr处指令commit结束后，才会把该条指令丢弃, rptr类似sp的存在
    rptr_next := rptr + 1
  } .otherwise{
    rptr_next := rptr
  }
  */

  when(instr_end_tab(rptr(SCB_INSTR_WIDTH - 1 downto 0)) === True){  // 当rptr处指令commit结束后，才会把该条指令丢弃, rptr类似sp的存在
    rptr_next := rptr + 1
  } .otherwise{
    rptr_next := rptr
  }

  val windex = wptr(SCB_INSTR_WIDTH - 1 downto 0)

  when(io.issue_dec_entry.dec_valid && ~instr_tab_full_real){
    INSTR_TAB(windex) := io.issue_dec_entry.instr
    ALU_SEL_TAB(windex) := B(io.issue_dec_entry.alu_sel)
    OP_TYPE_TAB(windex) := B(io.issue_dec_entry.op_type)
    RS1_TAB(windex) := U(io.issue_dec_entry.rs1_entry.reg_addr##io.issue_dec_entry.rs1_entry.reg_rden##io.issue_dec_entry.rs1_entry.reg_wten)
    RS2_TAB(windex) := U(io.issue_dec_entry.rs2_entry.reg_addr##io.issue_dec_entry.rs2_entry.reg_rden##io.issue_dec_entry.rs2_entry.reg_wten)
    RD_TAB(windex) := U(io.issue_dec_entry.rd_entry.reg_addr##io.issue_dec_entry.rd_entry.reg_rden##io.issue_dec_entry.rd_entry.reg_wten)
    CSR_TAB(windex) := U(io.issue_dec_entry.csr_entry.reg_addr##io.issue_dec_entry.csr_entry.reg_rden##io.issue_dec_entry.csr_entry.reg_wten)
    IMM_TAB(windex) := io.issue_dec_entry.imm
    DEC_VLD(windex) := io.issue_dec_entry.dec_valid
    PC_TAB(windex) := io.issue_dec_entry.pc
    BP_TAB(windex) := U(io.issue_branch_predict_entry.pc##io.issue_branch_predict_entry.branch_target##io.issue_branch_predict_entry.is_branch##io.issue_branch_predict_entry.is_call##io.issue_branch_predict_entry.is_ret##io.issue_branch_predict_entry.is_jump##io.issue_branch_predict_entry.branch_valid##io.issue_branch_predict_entry.branch_taken)
    PRE_TAB(windex) := io.issue_dec_entry.predict_flag
  }. otherwise{}

  val SCB_IU_TAB = Vec(Reg(Bits(5 bits)) init(IDLE), SCB_IU_DEEPTH)
  val fwb_flag = Vec(Bool(),REG_NUM) // todo
  val REG_ST_R = Vec(Reg(Bool( )) init(False),REG_NUM) // r,W todo to wire
  val REG_ST_W = Vec(Reg(Bool( )) init(False),REG_NUM) // r,W todo to wire
  val REG_ST_NW = Vec(Reg(Bool( )) init(False),REG_NUM) // wb new w todo to wire
  //val REG_ST = Vec(Bits(2 bit),REG_NUM) // r,W todo to wire
  //REG_ST := Vec.fill(REG_NUM)(B"00")
  val CSR_ST_R = Vec(Reg(Bool( )) init(False),CSR_NUM)
  val CSR_ST_W = Vec(Reg(Bool( )) init(False),CSR_NUM)
  val CSR_ST_NW = Vec(Reg(Bool( )) init(False),CSR_NUM)

  val FU_ST = Vec(Reg(Bool()) init(False),8)  // True --> busy  // todo

  val scb_iu_tab_ocu = UInt(SCB_INSTR_WIDTH+1 bits)
  when(wptr(SCB_INSTR_WIDTH)===rptr(SCB_INSTR_WIDTH)){
    scb_iu_tab_ocu := (wptr(SCB_INSTR_WIDTH-1 downto 0) - rptr(SCB_INSTR_WIDTH-1 downto 0)).resized
  } .otherwise{
    scb_iu_tab_ocu := SCB_IU_DEEPTH -(rptr(SCB_INSTR_WIDTH-1 downto 0)-wptr(SCB_INSTR_WIDTH-1 downto 0))
  }

  fwb_flag := Vec.fill(REG_NUM)(False)

  io.alu_oprand_entry.rs1_data := 0
  io.alu_oprand_entry.rs2_data := 0
  io.alu_oprand_entry.imm := 0
  io.alu_oprand_entry.rd_addr := 0
  io.alu_oprand_entry.rd_wten := False
  io.alu_oprand_entry.instr := 0
  io.alu_oprand_entry.op_type := OP_TYPE.OP_NOP
  io.alu_oprand_entry.dec_valid := False
  io.alu_oprand_entry.trans_id := U(SCB_IU_DEEPTH)
  io.alu_oprand_entry.pc := 0

  io.mul1_oprand_entry.rs1_data := 0
  io.mul1_oprand_entry.rs2_data := 0
  io.mul1_oprand_entry.imm := 0
  io.mul1_oprand_entry.rd_addr := 0
  io.mul1_oprand_entry.rd_wten := False
  io.mul1_oprand_entry.instr := 0
  io.mul1_oprand_entry.op_type := OP_TYPE.OP_NOP
  io.mul1_oprand_entry.dec_valid := False
  io.mul1_oprand_entry.trans_id := U(SCB_IU_DEEPTH)
  io.mul1_oprand_entry.pc := 0

  io.mul2_oprand_entry.rs1_data := 0
  io.mul2_oprand_entry.rs2_data := 0
  io.mul2_oprand_entry.imm := 0
  io.mul2_oprand_entry.rd_addr := 0
  io.mul2_oprand_entry.rd_wten := False
  io.mul2_oprand_entry.instr := 0
  io.mul2_oprand_entry.op_type := OP_TYPE.OP_NOP
  io.mul2_oprand_entry.dec_valid := False
  io.mul2_oprand_entry.trans_id := U(SCB_IU_DEEPTH)
  io.mul2_oprand_entry.pc := 0

  io.div_oprand_entry.rs1_data := 0
  io.div_oprand_entry.rs2_data := 0
  io.div_oprand_entry.imm := 0
  io.div_oprand_entry.rd_addr := 0
  io.div_oprand_entry.rd_wten := False
  io.div_oprand_entry.instr := 0
  io.div_oprand_entry.op_type := OP_TYPE.OP_NOP
  io.div_oprand_entry.dec_valid := False
  io.div_oprand_entry.trans_id := U(SCB_IU_DEEPTH)
  io.div_oprand_entry.pc := 0

  io.lsu_oprand_entry.rs1_data := 0
  io.lsu_oprand_entry.rs2_data := 0
  io.lsu_oprand_entry.imm := 0
  io.lsu_oprand_entry.rd_addr := 0
  io.lsu_oprand_entry.rd_wten := False
  io.lsu_oprand_entry.instr := 0
  io.lsu_oprand_entry.op_type := OP_TYPE.OP_NOP
  io.lsu_oprand_entry.dec_valid := False
  io.lsu_oprand_entry.trans_id := U(SCB_IU_DEEPTH)
  io.lsu_oprand_entry.pc := 0

  io.bju_oprand_entry.rs1_data := 0
  io.bju_oprand_entry.rs2_data := 0
  io.bju_oprand_entry.imm := 0
  io.bju_oprand_entry.rd_addr := 0
  io.bju_oprand_entry.rd_wten := False
  io.bju_oprand_entry.instr := 0
  io.bju_oprand_entry.op_type := OP_TYPE.OP_NOP
  io.bju_oprand_entry.dec_valid := False
  io.bju_oprand_entry.trans_id := U(SCB_IU_DEEPTH)
  io.bju_oprand_entry.pc := 0

  io.nopu_oprand_entry.rs1_data := 0
  io.nopu_oprand_entry.rs2_data := 0
  io.nopu_oprand_entry.imm := 0
  io.nopu_oprand_entry.rd_addr := 0
  io.nopu_oprand_entry.rd_wten := False
  io.nopu_oprand_entry.instr := 0
  io.nopu_oprand_entry.op_type := OP_TYPE.OP_NOP
  io.nopu_oprand_entry.dec_valid := False
  io.nopu_oprand_entry.trans_id := U(SCB_IU_DEEPTH)
  io.nopu_oprand_entry.pc := 0

  io.csr_oprand_entry.rs1_data := 0
  io.csr_oprand_entry.rs2_data := 0
  io.csr_oprand_entry.imm := 0
  io.csr_oprand_entry.rd_addr := 0
  io.csr_oprand_entry.rd_wten := False
  io.csr_oprand_entry.instr := 0
  io.csr_oprand_entry.op_type := OP_TYPE.OP_NOP
  io.csr_oprand_entry.dec_valid := False
  io.csr_oprand_entry.trans_id := U(SCB_IU_DEEPTH)
  io.csr_oprand_entry.pc := 0
  // avoid latch
  io.csr_wb_read_addr := 0

  io.scb_branch_predict_entry.pc := 0
  io.scb_branch_predict_entry.branch_target := 0
  io.scb_branch_predict_entry.is_branch := False
  io.scb_branch_predict_entry.is_call := False
  io.scb_branch_predict_entry.is_ret := False
  io.scb_branch_predict_entry.is_jump := False
  io.scb_branch_predict_entry.branch_valid := False
  io.scb_branch_predict_entry.branch_taken := False

  val flush_hold_tmp = Reg(Bool()) init(False)
  when(io.flush){
    flush_hold_tmp := True
  } .elsewhen(io.issue_dec_entry.dec_valid){
    flush_hold_tmp := False
  }
  val flush_hold_tmp1 = flush_hold_tmp && ~io.issue_dec_entry.dec_valid
  //val flush_hold = flush_hold_tmp1 || io.flush
  val flush_hold = flush_hold_tmp || io.flush

  for(i <- 0 until SCB_IU_DEEPTH) {
    //val index = (rptr + i)(SCB_INSTR_WIDTH - 1 downto 0)
    val index = i
    //val rs1_addr, rs1_rden, rs1_wten = RS1_TAB(index)
    //val rs2_addr, rs2_rden, rs2_wten = RS2_TAB(index)
    //val rd_addr, rd_rden, rd_wten = RD_TAB(index)
    val rs1_addr = RS1_TAB(index)(RegAddrBus+1 downto 2)
    val rs1_rden = RS1_TAB(index)(1)
    val rs1_wten = RS1_TAB(index)(0)
    val rs2_addr = RS2_TAB(index)(RegAddrBus+1 downto 2)
    val rs2_rden = RS2_TAB(index)(1)
    val rs2_wten = RS2_TAB(index)(0)
    val rd_addr = RD_TAB(index)(RegAddrBus+1 downto 2)
    val rd_rden = RD_TAB(index)(1)
    //val rd_wten = RD_TAB(index)(0)
    val rd_wten = RD_TAB(index)(0) && (rd_addr =/= 0) // fix error: 写x0寄存器
    val csr_addr = CSR_TAB(index)(CSRAddrBus+1 downto 2)
    val csr_rden = CSR_TAB(index)(1)
    val csr_wten = CSR_TAB(index)(0)
    val imm_value = IMM_TAB(index)
    val alu_sel = ALU_SEL_TAB(index)
    val instr = INSTR_TAB(index)
    val op_type = OP_TYPE_TAB(index)
    val dec_vld = DEC_VLD(index)
    val trans_id = UInt(SCB_ID_WIDTH bits)
    //trans_id := index.resized
    trans_id := index
    val pc = PC_TAB(index)
    val bp_pc = BP_TAB(index)(InstAddrBus+InstAddrBus+5 downto InstAddrBus+6)
    val bp_branch_target = BP_TAB(index)(InstAddrBus+5 downto 6)
    val bp_is_branch = BP_TAB(index)(5 downto 5)
    val bp_is_call = BP_TAB(index)(4 downto 4)
    val bp_is_ret = BP_TAB(index)(3 downto 3)
    val bp_is_jump = BP_TAB(index)(2 downto 2)
    val bp_branch_valid = BP_TAB(index)(1 downto 1)
    val bp_branch_taken = BP_TAB(index)(0 downto 0)
    //instr_end_tab(i) := False
    val tab_enable = Reg(Bool()) init(True)
    val predict_flag = PRE_TAB(index)
    val rptr_real = rptr(SCB_INSTR_WIDTH-1 downto 0)


    // 指令状态切换 // todo with 握手
    //if (i == 0) {
    //if (U(i) == rptr) {
    val last_iu_state = Bits(5 bit)
    if(i==0) {
      last_iu_state := SCB_IU_TAB(SCB_IU_DEEPTH-1)
    } else {
      last_iu_state := SCB_IU_TAB(i-1)
    }
    when(instr_end_tab(i)===True){
      tab_enable := False
    } .elsewhen( rptr_real===0 ){
      tab_enable := True
    }
    // todo: 暂时按照--如果FU被占用，则无法issue；EX2COMMIT阶段释放FU来设计【传统是Commit之后释放FU】 //
    switch(SCB_IU_TAB(i)) {
      instr_end_tab(i) := False
      is(IDLE) {
        when (flush_hold === True){
          SCB_IU_TAB(i) := IDLE
          //instr_end_tab(i) := True
        } .elsewhen(i === rptr_real && (scb_iu_tab_ocu <= 0) === True) {
          SCB_IU_TAB(i) := IDLE
        } .elsewhen( i=/= rptr_real && last_iu_state === IDLE) {
          SCB_IU_TAB(i) := IDLE
        }.elsewhen (REG_ST_NW(rd_addr) === True && rd_wten) { // WAW hazard
          SCB_IU_TAB(i) := IDLE
        } .elsewhen (FU_ST(U(alu_sel)) === True) { // Structure hazard with Function Unit // todo
          SCB_IU_TAB(i) := IDLE
        } .elsewhen(dec_vld === True && tab_enable === True){
          SCB_IU_TAB(i) := ISSUE
          //REG_ST(rs1_addr) := rs1_rden##B"0"
          //REG_ST(rs2_addr) := rs2_rden##B"0"
          //REG_ST_W(rd_addr) := rd_wten
          when(rd_wten){
            REG_ST_W(rd_addr) := True
            REG_ST_NW(rd_addr) := True
          } .otherwise{ }
          when(csr_wten){
            CSR_ST_W(csr_addr) := True
            CSR_ST_NW(csr_addr) := True
          } .otherwise{ }
          when(alu_sel=/=B(ALU_UNIT_SEL.NOPU)) {
            FU_ST(U(alu_sel)) := True
          } .otherwise{
            FU_ST(U(alu_sel)) := False  // 保证了无效指令不会阻塞
          }
        } .otherwise {
          SCB_IU_TAB(i) := IDLE
        }
      }
      is(ISSUE) {
        when(flush_hold === True) {
          SCB_IU_TAB(i) := IDLE
          //instr_end_tab(i) := True
          FU_ST(U(alu_sel)) := False
          REG_ST_W(rd_addr) := False
          REG_ST_NW(rd_addr) := False
          REG_ST_R(rs1_addr) := False
          REG_ST_R(rs2_addr) := False
          CSR_ST_R(csr_addr) := False
          CSR_ST_W(csr_addr) := False
          CSR_ST_NW(csr_addr) := False
        }.elsewhen((REG_ST_NW(rs1_addr) === True && fwb_flag(rs1_addr) === False && rs1_rden && ~(rs1_addr === rd_addr && rd_wten)) || (REG_ST_NW(rs2_addr) === True && fwb_flag(rs2_addr) === False && rs2_rden && ~(rs2_addr === rd_addr && rd_wten))) { // RAW issue --> fwb
          //.elsewhen ((REG_ST_W(rs1_addr) === True && fwb_flag(rs1_addr) === False && rs1_rden) || (REG_ST_W(rs2_addr) === True && fwb_flag(rs2_addr) === False && rs2_rden && )) {v
          SCB_IU_TAB(i) := ISSUE
        }.otherwise {
          SCB_IU_TAB(i) := READOP
          when(rs1_rden) {
            REG_ST_R(rs1_addr) := rs1_rden
          }
          when(rs2_rden) {
            REG_ST_R(rs2_addr) := rs2_rden
          }
          when(csr_rden) {
            CSR_ST_R(csr_addr) := csr_rden
            io.csr_wb_read_addr := csr_addr
          }
          //REG_ST(rd_addr) := B"0"##rd_wten
        }
      }
      is(READOP) {
        //when(io.flush === True && predict_flag === True) {  // todo : 现在统一在commit时处理flush，因此保证了该条指令是oldest，不需要predict_flag了
        when(io.flush === True) {
          SCB_IU_TAB(i) := IDLE
          //instr_end_tab(i) := True
          FU_ST(U(alu_sel)) := False
          REG_ST_W(rd_addr) := False
          REG_ST_NW(rd_addr) := False
          REG_ST_R(rs1_addr) := False
          REG_ST_R(rs2_addr) := False
          CSR_ST_NW(csr_addr) := False
          CSR_ST_R(csr_addr) := False
          CSR_ST_R(csr_addr) := False

        }.otherwise {
          SCB_IU_TAB(i) := EXE // todo
          val rs1_data_real = UInt(RegDataBus bits)
          val rs2_data_real = UInt(RegDataBus bits)
          val csr_data_real = UInt(CSRDataBus bits)
          rs1_data_real := io.scb_readop_wb_i(rs1_addr)
          rs2_data_real := io.scb_readop_wb_i(rs2_addr)
          csr_data_real := io.csr_wb_read_data
          switch(alu_sel) {
            is(B(ALU_UNIT_SEL.ALUU)) {
              io.alu_oprand_entry.rs1_data := rs1_data_real
              io.alu_oprand_entry.rs2_data := rs2_data_real
              io.alu_oprand_entry.imm := imm_value
              io.alu_oprand_entry.rd_addr := rd_addr
              io.alu_oprand_entry.rd_wten := rd_wten
              io.alu_oprand_entry.instr := instr
              io.alu_oprand_entry.op_type.assignFromBits(op_type)
              io.alu_oprand_entry.dec_valid := True
              io.alu_oprand_entry.trans_id := trans_id
              io.alu_oprand_entry.pc := pc
            }
            is(B(ALU_UNIT_SEL.MULU1)) {
              io.mul1_oprand_entry.rs1_data := rs1_data_real
              io.mul1_oprand_entry.rs2_data := rs2_data_real
              io.mul1_oprand_entry.imm := imm_value
              io.mul1_oprand_entry.rd_addr := rd_addr
              io.mul1_oprand_entry.rd_wten := rd_wten
              io.mul1_oprand_entry.instr := instr
              io.mul1_oprand_entry.op_type.assignFromBits(op_type)
              io.mul1_oprand_entry.dec_valid := True
              io.mul1_oprand_entry.trans_id := trans_id
              io.mul1_oprand_entry.pc := pc
            }
            is(B(ALU_UNIT_SEL.MULU2)) {
              io.mul2_oprand_entry.rs1_data := rs1_data_real
              io.mul2_oprand_entry.rs2_data := rs2_data_real
              io.mul2_oprand_entry.imm := imm_value
              io.mul2_oprand_entry.rd_addr := rd_addr
              io.mul2_oprand_entry.rd_wten := rd_wten
              io.mul2_oprand_entry.instr := instr
              io.mul2_oprand_entry.op_type.assignFromBits(op_type)
              io.mul2_oprand_entry.dec_valid := True
              io.mul2_oprand_entry.trans_id := trans_id
              io.mul2_oprand_entry.pc := pc
            }
            is(B(ALU_UNIT_SEL.DIVU)) {
              io.div_oprand_entry.rs1_data := rs1_data_real
              io.div_oprand_entry.rs2_data := rs2_data_real
              io.div_oprand_entry.imm := imm_value
              io.div_oprand_entry.rd_addr := rd_addr
              io.div_oprand_entry.rd_wten := rd_wten
              io.div_oprand_entry.instr := instr
              io.div_oprand_entry.op_type.assignFromBits(op_type)
              io.div_oprand_entry.dec_valid := True
              io.div_oprand_entry.trans_id := trans_id
              io.div_oprand_entry.pc := pc
            }
            is(B(ALU_UNIT_SEL.LSU)) {
              io.lsu_oprand_entry.rs1_data := rs1_data_real
              io.lsu_oprand_entry.rs2_data := rs2_data_real
              io.lsu_oprand_entry.imm := imm_value
              io.lsu_oprand_entry.rd_addr := rd_addr
              io.lsu_oprand_entry.rd_wten := rd_wten
              io.lsu_oprand_entry.instr := instr
              io.lsu_oprand_entry.op_type.assignFromBits(op_type)
              io.lsu_oprand_entry.dec_valid := True
              io.lsu_oprand_entry.trans_id := trans_id
              io.lsu_oprand_entry.pc := pc
            }
            is(B(ALU_UNIT_SEL.BJU)) {
              io.bju_oprand_entry.rs1_data := rs1_data_real
              io.bju_oprand_entry.rs2_data := rs2_data_real
              io.bju_oprand_entry.imm := imm_value
              io.bju_oprand_entry.rd_addr := rd_addr
              io.bju_oprand_entry.rd_wten := rd_wten
              io.bju_oprand_entry.instr := instr
              io.bju_oprand_entry.op_type.assignFromBits(op_type)
              io.bju_oprand_entry.dec_valid := True
              io.bju_oprand_entry.trans_id := trans_id
              io.bju_oprand_entry.pc := pc
              // todo with output branch predict entry to bju unit
              // branch predict entry pop out //
              io.scb_branch_predict_entry.pc := bp_pc
              io.scb_branch_predict_entry.branch_target := bp_branch_target
              io.scb_branch_predict_entry.is_branch := bp_is_branch.asBool
              io.scb_branch_predict_entry.is_call := bp_is_call.asBool
              io.scb_branch_predict_entry.is_ret := bp_is_ret.asBool
              io.scb_branch_predict_entry.is_jump := bp_is_jump.asBool
              io.scb_branch_predict_entry.branch_valid := bp_branch_valid.asBool
              io.scb_branch_predict_entry.branch_taken := bp_branch_taken.asBool
            }
            is(B(ALU_UNIT_SEL.CSR)) {
              io.csr_oprand_entry.rs1_data := rs1_data_real
              io.csr_oprand_entry.rs2_data := csr_data_real // 复用:前提是csr和reg的data位宽一致
              io.csr_oprand_entry.imm := imm_value
              io.csr_oprand_entry.rd_addr := rd_addr
              io.csr_oprand_entry.rd_wten := rd_wten
              io.csr_oprand_entry.instr := instr
              io.csr_oprand_entry.op_type.assignFromBits(op_type)
              io.csr_oprand_entry.dec_valid := True
              io.csr_oprand_entry.trans_id := trans_id
              io.csr_oprand_entry.pc := pc
            }
            is(B(ALU_UNIT_SEL.NOPU))  {
              io.nopu_oprand_entry.instr := instr
              io.nopu_oprand_entry.op_type.assignFromBits(op_type)
              io.nopu_oprand_entry.dec_valid := False
              io.nopu_oprand_entry.trans_id := trans_id
              io.nopu_oprand_entry.pc := pc
            }
          }
        }
      }
      is(EXE) {
        //when (FU_ST(U(alu_sel)) === True) { // todo
        //when (io.flush === True && predict_flag === True){  // todo : 现在统一在commit时处理flush，因此保证了该条指令是oldest，不需要predict_flag了
        when(io.flush === True) {
          SCB_IU_TAB(i) := IDLE
          //instr_end_tab(i) := True
          FU_ST(U(alu_sel)) := False
          REG_ST_W(rd_addr) := False
          REG_ST_NW(rd_addr) := False
          REG_ST_R(rs1_addr) := False
          REG_ST_R(rs2_addr) := False
          CSR_ST_R(csr_addr) := False
          CSR_ST_W(csr_addr) := False
          CSR_ST_NW(csr_addr) := False

          ex_wb_entry_instr(U(alu_sel)) := U"0".resized
          ex_wb_entry_pc(U(alu_sel)) := U"0".resized
          ex_wb_entry_reg_wb_en(U(alu_sel)) := False
          ex_wb_entry_reg_wb_addr(U(alu_sel)) := U"0".resized
          ex_wb_entry_reg_wb_data(U(alu_sel)) := U"0".resized
          ex_wb_entry_trans_id(U(alu_sel)) := SCB_IU_DEEPTH
          ex_wb_entry_dcache_wb_en(U(alu_sel)) := False
          ex_wb_entry_dcache_wb_addr(U(alu_sel)) := U"0".resized
          ex_wb_entry_dcache_wb_data(U(alu_sel)) := U"0".resized
          ex_wb_entry_dcache_rd_en(U(alu_sel)) := False
          ex_wb_entry_dcache_rd_addr(U(alu_sel)) := U"0".resized
          ex_wb_entry_dcache_rd_data(U(alu_sel)) := U"0".resized
          ex_wb_entry_dcache_wb_sel(U(alu_sel)) := B"1111"
          ex_wb_entry_branch_cor(U(alu_sel)) := False
          ex_wb_entry_call_cor(U(alu_sel)) := False
          ex_wb_entry_ret_cor(U(alu_sel)) := False
          ex_wb_entry_is_branch(U(alu_sel)) := False
          ex_wb_entry_is_call(U(alu_sel)) := False
          ex_wb_entry_is_ret(U(alu_sel)) := False
          ex_wb_entry_is_jump(U(alu_sel)) := False
          ex_wb_entry_target_pc(U(alu_sel)) := U"0".resized
          ex_wb_entry_csr_wb_en(U(alu_sel)) := False
          ex_wb_entry_csr_wb_addr(U(alu_sel)) := U"0".resized
          ex_wb_entry_csr_wb_data(U(alu_sel)) := U"0".resized

        } .elsewhen ((alu_sel===B(ALU_UNIT_SEL.MULU1) || alu_sel===B(ALU_UNIT_SEL.MULU2) || alu_sel===B(ALU_UNIT_SEL.DIVU)) && FU_ST(U(alu_sel)) === True) {  // todo
          SCB_IU_TAB(i) := EXE
        } .elsewhen((REG_ST_R(rd_addr) === True && rd_wten && ~(rd_addr===rs1_addr || rd_addr===rs2_addr))){
          SCB_IU_TAB(i) := EXE
        } .otherwise {
          //SCB_IU_TAB(i) := COMMIT
          SCB_IU_TAB(i) := WB
          //ex_commit_entry_commit_req := True
          ex_wb_req(U(alu_sel))(i) := True
          FU_ST(U(alu_sel)) := False  // 释放Fucntion Unit
          when(rd_wten === True) {  // todo
            REG_ST_NW(rd_addr) := False // 释放WB的dest寄存器
          } .otherwise{}
          switch(alu_sel){
            is(B(ALU_UNIT_SEL.ALUU)){ // todo 每个ALLU_UNIT需要一组ex_wb_entry，支持并行送入WB //
              ex_wb_entry_instr(U(alu_sel)) := io.alu_ex_entry.instr
              ex_wb_entry_pc(U(alu_sel)) := io.alu_ex_entry.pc
              ex_wb_entry_reg_wb_en(U(alu_sel)) := rd_wten
              ex_wb_entry_reg_wb_addr(U(alu_sel)) := rd_addr
              ex_wb_entry_reg_wb_data(U(alu_sel)) := io.alu_ex_entry.result.asUInt
              ex_wb_entry_trans_id(U(alu_sel)) := io.alu_ex_entry.trans_id
              //ex_wb_entry_alusel(U(alu_sel)) := B(ALU_UNIT_SEL.ALUU)
              ex_wb_entry_dcache_wb_en(U(alu_sel)) := False
              ex_wb_entry_dcache_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_en(U(alu_sel)) := False
              ex_wb_entry_dcache_rd_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_sel(U(alu_sel)) := B"1111"
              ex_wb_entry_branch_cor(U(alu_sel)) := False
              ex_wb_entry_call_cor(U(alu_sel)) := False
              ex_wb_entry_ret_cor(U(alu_sel)) := False
              ex_wb_entry_is_branch(U(alu_sel)) := False
              ex_wb_entry_is_call(U(alu_sel)) := False
              ex_wb_entry_is_ret(U(alu_sel)) := False
              ex_wb_entry_is_jump(U(alu_sel)) := False
              ex_wb_entry_target_pc(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_en(U(alu_sel)) := False
              ex_wb_entry_csr_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_data(U(alu_sel)) := U"0".resized
            }
            is(B(ALU_UNIT_SEL.MULU1)){
              ex_wb_entry_instr(U(alu_sel)) := io.mul1_ex_entry.instr
              ex_wb_entry_pc(U(alu_sel)) := io.mul1_ex_entry.pc
              ex_wb_entry_reg_wb_en(U(alu_sel)) := rd_wten
              ex_wb_entry_reg_wb_addr(U(alu_sel)) := rd_addr
              ex_wb_entry_reg_wb_data(U(alu_sel)) := io.mul1_ex_entry.result
              ex_wb_entry_trans_id(U(alu_sel)) := io.mul1_ex_entry.trans_id
              //ex_wb_entry_alusel(U(alu_sel)) := B(ALU_UNIT_SEL.MULU1)
              ex_wb_entry_dcache_wb_en(U(alu_sel)) := False
              ex_wb_entry_dcache_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_en(U(alu_sel)) := False
              ex_wb_entry_dcache_rd_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_sel(U(alu_sel)) := B"1111"
              ex_wb_entry_branch_cor(U(alu_sel)) := False
              ex_wb_entry_call_cor(U(alu_sel)) := False
              ex_wb_entry_ret_cor(U(alu_sel)) := False
              ex_wb_entry_is_branch(U(alu_sel)) := False
              ex_wb_entry_is_call(U(alu_sel)) := False
              ex_wb_entry_is_ret(U(alu_sel)) := False
              ex_wb_entry_is_jump(U(alu_sel)) := False
              ex_wb_entry_target_pc(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_en(U(alu_sel)) := False
              ex_wb_entry_csr_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_data(U(alu_sel)) := U"0".resized
            }
            is(B(ALU_UNIT_SEL.MULU2)){
              ex_wb_entry_instr(U(alu_sel)) := io.mul2_ex_entry.instr
              ex_wb_entry_pc(U(alu_sel)) := io.mul2_ex_entry.pc
              ex_wb_entry_reg_wb_en(U(alu_sel)) := rd_wten
              ex_wb_entry_reg_wb_addr(U(alu_sel)) := rd_addr
              ex_wb_entry_reg_wb_data(U(alu_sel)) := io.mul2_ex_entry.result
              ex_wb_entry_trans_id(U(alu_sel)) := io.mul2_ex_entry.trans_id
              //ex_wb_entry_alusel(U(alu_sel)) := B(ALU_UNIT_SEL.MULU2)
              ex_wb_entry_dcache_wb_en(U(alu_sel)) := False
              ex_wb_entry_dcache_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_en(U(alu_sel)) := False
              ex_wb_entry_dcache_rd_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_sel(U(alu_sel)) := B"1111"
              ex_wb_entry_branch_cor(U(alu_sel)) := False
              ex_wb_entry_call_cor(U(alu_sel)) := False
              ex_wb_entry_ret_cor(U(alu_sel)) := False
              ex_wb_entry_is_branch(U(alu_sel)) := False
              ex_wb_entry_is_call(U(alu_sel)) := False
              ex_wb_entry_is_ret(U(alu_sel)) := False
              ex_wb_entry_is_jump(U(alu_sel)) := False
              ex_wb_entry_target_pc(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_en(U(alu_sel)) := False
              ex_wb_entry_csr_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_data(U(alu_sel)) := U"0".resized
            }
            is(B(ALU_UNIT_SEL.DIVU)){
              ex_wb_entry_instr(U(alu_sel)) := io.div_ex_entry.instr
              ex_wb_entry_pc(U(alu_sel)) := io.div_ex_entry.pc
              ex_wb_entry_reg_wb_en(U(alu_sel)) := rd_wten
              ex_wb_entry_reg_wb_addr(U(alu_sel)) := rd_addr
              ex_wb_entry_reg_wb_data(U(alu_sel)) := io.div_ex_entry.result
              ex_wb_entry_trans_id(U(alu_sel)) := io.div_ex_entry.trans_id
              //ex_wb_entry_alusel(U(alu_sel)) := B(ALU_UNIT_SEL.DIVU)
              ex_wb_entry_dcache_wb_en(U(alu_sel)) := False
              ex_wb_entry_dcache_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_en(U(alu_sel)) := False
              ex_wb_entry_dcache_rd_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_sel(U(alu_sel)) := B"1111"
              ex_wb_entry_branch_cor(U(alu_sel)) := False
              ex_wb_entry_call_cor(U(alu_sel)) := False
              ex_wb_entry_ret_cor(U(alu_sel)) := False
              ex_wb_entry_is_branch(U(alu_sel)) := False
              ex_wb_entry_is_call(U(alu_sel)) := False
              ex_wb_entry_is_ret(U(alu_sel)) := False
              ex_wb_entry_is_jump(U(alu_sel)) := False
              ex_wb_entry_target_pc(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_en(U(alu_sel)) := False
              ex_wb_entry_csr_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_data(U(alu_sel)) := U"0".resized
            }
            is(B(ALU_UNIT_SEL.LSU)){
              ex_wb_entry_instr(U(alu_sel)) := io.lsu_ex_entry.instr
              ex_wb_entry_pc(U(alu_sel)) := io.lsu_ex_entry.pc
              ex_wb_entry_reg_wb_en(U(alu_sel)) := rd_wten
              ex_wb_entry_reg_wb_addr(U(alu_sel)) := rd_addr
              ex_wb_entry_reg_wb_data(U(alu_sel)) := io.lsu_ex_entry.result
              ex_wb_entry_trans_id(U(alu_sel)) := io.lsu_ex_entry.trans_id
              //ex_wb_entry_alusel(U(alu_sel)) := B(ALU_UNIT_SEL.LSU)
              // todo with store
              ex_wb_entry_dcache_wb_en(U(alu_sel)) := io.lsu_ex_entry.store_wb_en
              ex_wb_entry_dcache_wb_addr(U(alu_sel)) := io.lsu_ex_entry.store_wb_addr
              ex_wb_entry_dcache_wb_data(U(alu_sel)) := io.lsu_ex_entry.store_wb_data
              ex_wb_entry_dcache_wb_sel(U(alu_sel)) := io.lsu_ex_entry.store_wb_byte
              // todo with dcache read
              ex_wb_entry_dcache_rd_en(U(alu_sel)) := io.lsu_ex_entry.load_rd_en
              ex_wb_entry_dcache_rd_addr(U(alu_sel)) := io.lsu_ex_entry.load_rd_addr  // ex stage 计算得到的dcache地址
              ex_wb_entry_dcache_rd_data(U(alu_sel)) := io.lsu_ex_entry.load_rd_data  // 从dcache地址读出的数据
              ex_wb_entry_branch_cor(U(alu_sel)) := False
              ex_wb_entry_call_cor(U(alu_sel)) := False
              ex_wb_entry_ret_cor(U(alu_sel)) := False
              ex_wb_entry_is_branch(U(alu_sel)) := False
              ex_wb_entry_is_call(U(alu_sel)) := False
              ex_wb_entry_is_ret(U(alu_sel)) := False
              ex_wb_entry_is_jump(U(alu_sel)) := False
              ex_wb_entry_target_pc(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_en(U(alu_sel)) := False
              ex_wb_entry_csr_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_data(U(alu_sel)) := U"0".resized
            }
            is(B(ALU_UNIT_SEL.BJU)){
              ex_wb_entry_instr(U(alu_sel)) := io.bju_ex_entry.instr
              ex_wb_entry_pc(U(alu_sel)) := io.bju_ex_entry.pc
              ex_wb_entry_reg_wb_en(U(alu_sel)) := rd_wten
              ex_wb_entry_reg_wb_addr(U(alu_sel)) := rd_addr
              ex_wb_entry_reg_wb_data(U(alu_sel)) := io.bju_ex_entry.result
              ex_wb_entry_trans_id(U(alu_sel)) := io.bju_ex_entry.trans_id
              //ex_wb_entry_alusel(U(alu_sel)) := B(ALU_UNIT_SEL.BJU)
              // todo with mispredict
              ex_wb_entry_branch_cor(U(alu_sel)) := io.bju_mis_predict_entry.branch_cor
              ex_wb_entry_call_cor(U(alu_sel)) := io.bju_mis_predict_entry.call_cor
              ex_wb_entry_ret_cor(U(alu_sel)) := io.bju_mis_predict_entry.ret_cor
              ex_wb_entry_target_pc(U(alu_sel)) := io.bju_mis_predict_entry.target_pc
              ex_wb_entry_is_branch(U(alu_sel)) := io.bju_mis_predict_entry.is_branch
              ex_wb_entry_is_call(U(alu_sel)) := io.bju_mis_predict_entry.is_call
              ex_wb_entry_is_ret(U(alu_sel)) := io.bju_mis_predict_entry.is_ret
              ex_wb_entry_is_jump(U(alu_sel)) := io.bju_mis_predict_entry.is_jump
              ex_wb_entry_dcache_wb_en(U(alu_sel)) := False
              ex_wb_entry_dcache_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_en(U(alu_sel)) := False
              ex_wb_entry_dcache_rd_addr(U(alu_sel)) := U"0".resized  
              ex_wb_entry_dcache_rd_data(U(alu_sel)) := U"0".resized  
              ex_wb_entry_dcache_wb_sel(U(alu_sel)) := B"1111"
              ex_wb_entry_csr_wb_en(U(alu_sel)) := False
              ex_wb_entry_csr_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_data(U(alu_sel)) := U"0".resized
            }
            is(B(ALU_UNIT_SEL.CSR)){
              ex_wb_entry_instr(U(alu_sel)) := io.csr_ex_entry.instr
              ex_wb_entry_pc(U(alu_sel)) := io.csr_ex_entry.pc
              ex_wb_entry_reg_wb_en(U(alu_sel)) := rd_wten
              ex_wb_entry_reg_wb_addr(U(alu_sel)) := rd_addr
              ex_wb_entry_reg_wb_data(U(alu_sel)) := io.csr_ex_entry.result
              ex_wb_entry_trans_id(U(alu_sel)) := io.csr_ex_entry.trans_id
              //ex_wb_entry_alusel(U(alu_sel)) := B(ALU_UNIT_SEL.CSR)
              ex_wb_entry_dcache_wb_en(U(alu_sel)) := False
              ex_wb_entry_dcache_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_en(U(alu_sel)) := False
              ex_wb_entry_dcache_rd_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_sel(U(alu_sel)) := B"1111"
              ex_wb_entry_branch_cor(U(alu_sel)) := False
              ex_wb_entry_call_cor(U(alu_sel)) := False
              ex_wb_entry_ret_cor(U(alu_sel)) := False
              ex_wb_entry_is_branch(U(alu_sel)) := False
              ex_wb_entry_is_call(U(alu_sel)) := False
              ex_wb_entry_is_ret(U(alu_sel)) := False
              ex_wb_entry_is_jump(U(alu_sel)) := False
              ex_wb_entry_target_pc(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_en(U(alu_sel)) := csr_wten
              ex_wb_entry_csr_wb_addr(U(alu_sel)) := csr_addr
              ex_wb_entry_csr_wb_data(U(alu_sel)) := io.csr_ex_entry.result_csr
            }
            is(B(ALU_UNIT_SEL.NOPU)){
              ex_wb_entry_instr(U(alu_sel)) := io.nopu_ex_entry.instr
              ex_wb_entry_pc(U(alu_sel)) := io.nopu_ex_entry.pc
              ex_wb_entry_reg_wb_en(U(alu_sel)) := False
              ex_wb_entry_reg_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_reg_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_trans_id(U(alu_sel)) := io.nopu_ex_entry.trans_id
              //ex_wb_entry_alusel(U(alu_sel)) := B(ALU_UNIT_SEL.CSR)
              ex_wb_entry_dcache_wb_en(U(alu_sel)) := False
              ex_wb_entry_dcache_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_en(U(alu_sel)) := False
              ex_wb_entry_dcache_rd_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_sel(U(alu_sel)) := B"1111"
              ex_wb_entry_branch_cor(U(alu_sel)) := False
              ex_wb_entry_call_cor(U(alu_sel)) := False
              ex_wb_entry_ret_cor(U(alu_sel)) := False
              ex_wb_entry_is_branch(U(alu_sel)) := False
              ex_wb_entry_is_call(U(alu_sel)) := False
              ex_wb_entry_is_ret(U(alu_sel)) := False
              ex_wb_entry_is_jump(U(alu_sel)) := False
              ex_wb_entry_target_pc(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_en(U(alu_sel)) := False
              ex_wb_entry_csr_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_data(U(alu_sel)) := U"0".resized
            }
            default{
              ex_wb_entry_instr(U(alu_sel)) := U"0".resized
              ex_wb_entry_pc(U(alu_sel)) := U"0".resized
              ex_wb_entry_reg_wb_en(U(alu_sel)) := False
              ex_wb_entry_reg_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_reg_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_trans_id(U(alu_sel)) := SCB_IU_DEEPTH
              ex_wb_entry_dcache_wb_en(U(alu_sel)) := False
              ex_wb_entry_dcache_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_wb_data(U(alu_sel)) := U"0".resized
              ex_wb_entry_dcache_rd_en(U(alu_sel)) := False
              ex_wb_entry_dcache_rd_addr(U(alu_sel)) := U"0".resized  
              ex_wb_entry_dcache_rd_data(U(alu_sel)) := U"0".resized  
              ex_wb_entry_dcache_wb_sel(U(alu_sel)) := B"1111"
              ex_wb_entry_branch_cor(U(alu_sel)) := False
              ex_wb_entry_call_cor(U(alu_sel)) := False
              ex_wb_entry_ret_cor(U(alu_sel)) := False
              ex_wb_entry_is_branch(U(alu_sel)) := False
              ex_wb_entry_is_call(U(alu_sel)) := False
              ex_wb_entry_is_ret(U(alu_sel)) := False
              ex_wb_entry_is_jump(U(alu_sel)) := False
              ex_wb_entry_target_pc(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_en(U(alu_sel)) := False
              ex_wb_entry_csr_wb_addr(U(alu_sel)) := U"0".resized
              ex_wb_entry_csr_wb_data(U(alu_sel)) := U"0".resized
              //ex_wb_entry_alusel(U(alu_sel)) := B(ALU_UNIT_SEL.NOPU)
            }

          }
        }
      }
      is(WB){
        //when (io.flush === True && predict_flag === True){  // todo : 现在统一在commit时处理flush，因此保证了该条指令是oldest，不需要predict_flag了
        when (io.flush === True){
          SCB_IU_TAB(i) := IDLE
          ex_wb_req(U(alu_sel))(i) := False
          //instr_end_tab(i) := True
          FU_ST(U(alu_sel)) := False
          REG_ST_W(rd_addr) := False
          REG_ST_NW(rd_addr) := False
          REG_ST_R(rs1_addr) := False
          REG_ST_R(rs2_addr) := False
          CSR_ST_NW(csr_addr) := False
          CSR_ST_R(csr_addr) := False
          CSR_ST_R(csr_addr) := False

        } .elsewhen(((i === rptr_real) || (i=/= rptr_real && last_iu_state === IDLE)) && last_iu_state =/= COMMIT) {  // todo : 保证最旧的指令才能commit，按顺序提交
          //. elsewhen(((i === rptr_real) || (i=/= rptr_real && last_iu_state === IDLE)) && io.ex_wb_entry.commit_ack=== True && last_iu_state =/= COMMIT) {
          SCB_IU_TAB(i) := COMMIT
          ex_wb_req(U(alu_sel))(i) := False
          wb_commit_req(i) := True
          wb_commit_entry_instr := io.wb_scb_entry.instr
          wb_commit_entry_pc := io.wb_scb_entry.pc
          wb_commit_entry_reg_wb_en := io.wb_scb_entry.reg_wb_en
          wb_commit_entry_reg_wb_addr := io.wb_scb_entry.reg_wb_addr
          wb_commit_entry_reg_wb_data := io.wb_scb_entry.reg_wb_data
          wb_commit_entry_trans_id := io.wb_scb_entry.trans_id
          wb_commit_entry_dec_valid := io.wb_scb_entry.dec_valid
          // todo with store
          wb_commit_entry_dcache_wb_en := io.wb_scb_entry.dcache_wb_en
          wb_commit_entry_dcache_wb_addr := io.wb_scb_entry.dcache_wb_addr
          wb_commit_entry_dcache_wb_data := io.wb_scb_entry.dcache_wb_data
          wb_commit_entry_dcache_rd_en := io.wb_scb_entry.dcache_rd_en
          wb_commit_entry_dcache_rd_addr := io.wb_scb_entry.dcache_rd_addr
          wb_commit_entry_dcache_rd_data := io.wb_scb_entry.dcache_rd_data
          wb_commit_entry_dcache_wb_sel := io.wb_scb_entry.dcache_wb_sel
          // todo with bju mispredict
          wb_commit_entry_branch_cor := io.wb_scb_entry.branch_cor
          wb_commit_entry_call_cor := io.wb_scb_entry.call_cor
          wb_commit_entry_ret_cor := io.wb_scb_entry.ret_cor
          wb_commit_entry_is_branch := io.wb_scb_entry.is_branch
          wb_commit_entry_is_call := io.wb_scb_entry.is_call
          wb_commit_entry_is_ret := io.wb_scb_entry.is_ret
          wb_commit_entry_is_jump := io.wb_scb_entry.is_jump
          wb_commit_entry_target_pc := io.wb_scb_entry.target_pc
          wb_commit_entry_csr_wb_en := io.wb_scb_entry.csr_wb_en
          wb_commit_entry_csr_wb_addr := io.wb_scb_entry.csr_wb_addr
          wb_commit_entry_csr_wb_data := io.wb_scb_entry.csr_wb_data
        } .otherwise{
          SCB_IU_TAB(i) := WB
        }
      }
      is(COMMIT) {
        when (io.wb_commit_entry.commit_ack === True && io.wb_commit_entry.recv_id === index) {  // 会被sp+1信息顶替
          //SCB_IU_TAB(i) := IDLE
          instr_end_tab(i) := True
          //when(instr_end_tab(i) === True){
          //  SCB_IU_TAB(i) := IDLE
          //}
          //            REG_ST_R(rs1_addr) := False
          //            REG_ST_R(rs2_addr) := False
          //            REG_ST_W(rd_addr) := False
          //ex_commit_entry_commit_req := False
          wb_commit_req(i) := False
          /*
          for (i <- 0 until REG_NUM) {
            report(Seq("reflash [x] ", io.scb_readop_i(i)))
          }
           */
        } .elsewhen(instr_end_tab(i) === True){
          SCB_IU_TAB(i) := IDLE
          REG_ST_R(rs1_addr) := False
          REG_ST_R(rs2_addr) := False
          REG_ST_W(rd_addr) := False
          CSR_ST_R(csr_addr) := False
          CSR_ST_W(csr_addr) := False
        }.otherwise {
          SCB_IU_TAB(i) := COMMIT
        }

      }
      default {}
    }
    //}
    //}
    /*
  else {
    switch(SCB_IU_TAB(i)) {
      is(IDLE) {
        instr_end_tab(i) := False
        if(i==0) {
          when(SCB_IU_TAB(SCB_IU_DEEPTH-1) === IDLE) { // different from i==0
            SCB_IU_TAB(i) := IDLE
          }.elsewhen((scb_iu_tab_ocu <= i) === True) {
            SCB_IU_TAB(i) := IDLE
          }.elsewhen(REG_ST(rd_addr)(0) === True) { // WAW hazard
            SCB_IU_TAB(i) := IDLE
          }.elsewhen(FU_ST(U(alu_sel)) === True) { // Structure hazard
            SCB_IU_TAB(i) := IDLE
          }.elsewhen(dec_vld === True) {
            SCB_IU_TAB(i) := ISSUE
            //REG_ST(rs1_addr) := rs1_rden##B"0"
            //REG_ST(rs2_addr) := rs2_rden##B"0"
            REG_ST(rd_addr) := B"0" ## rd_wten
          }.otherwise {
            SCB_IU_TAB(i) := IDLE
          }
        } else {
          when(SCB_IU_TAB(i - 1) === IDLE) { // different from i==0
            SCB_IU_TAB(i) := IDLE
          }.elsewhen((scb_iu_tab_ocu <= i) === True) {
            SCB_IU_TAB(i) := IDLE
          }.elsewhen(REG_ST(rd_addr)(0) === True) { // WAW hazard
            SCB_IU_TAB(i) := IDLE
          }.elsewhen(FU_ST(U(alu_sel)) === True) { // Structure hazard
            SCB_IU_TAB(i) := IDLE
          }.elsewhen(dec_vld === True) {
            SCB_IU_TAB(i) := ISSUE
            //REG_ST(rs1_addr) := rs1_rden##B"0"
            //REG_ST(rs2_addr) := rs2_rden##B"0"
            REG_ST(rd_addr) := B"0" ## rd_wten
          }.otherwise {
            SCB_IU_TAB(i) := IDLE
          }
        }
      }
      is(ISSUE) {
        when ((REG_ST(rs1_addr)(0) === True && fwb_flag(rs1_addr) === False) || (REG_ST(rs2_addr)(0) === True && fwb_flag(rs2_addr) === False)) { // RAW issue --> fwb
          SCB_IU_TAB(i) := ISSUE
        } .otherwise {
          SCB_IU_TAB(i) := READOP
          REG_ST(rs1_addr) := rs1_rden##B"0"
          REG_ST(rs2_addr) := rs2_rden##B"0"
          //REG_ST(rd_addr) := B"0"##rd_wten
        }
        is(READOP) {
          SCB_IU_TAB(i) := EXE // todo
          switch(alu_sel){
            is(B(ALU_UNIT_SEL.ALUU)){
              io.alu_oprand_entry.rs1_data := io.scb_readop_i(rs1_addr)
              io.alu_oprand_entry.rs2_data := io.scb_readop_i(rs2_addr)
              io.alu_oprand_entry.imm := imm_value
              io.alu_oprand_entry.rd_addr := rd_addr
              io.alu_oprand_entry.rd_wten := rd_wten
              io.alu_oprand_entry.instr := instr
              io.alu_oprand_entry.op_type.assignFromBits(op_type)
              io.alu_oprand_entry.dec_valid := True
              io.alu_oprand_entry.trans_id := trans_id
            }
            is(B(ALU_UNIT_SEL.MULU1)){
              io.mul1_oprand_entry.rs1_data := io.scb_readop_i(rs1_addr)
              io.mul1_oprand_entry.rs2_data := io.scb_readop_i(rs2_addr)
              io.mul1_oprand_entry.imm := imm_value
              io.mul1_oprand_entry.rd_addr := rd_addr
              io.mul1_oprand_entry.rd_wten := rd_wten
              io.mul1_oprand_entry.instr := instr
              io.mul1_oprand_entry.op_type.assignFromBits(op_type)
              io.mul1_oprand_entry.dec_valid := True
              io.mul1_oprand_entry.trans_id := trans_id
            }
            is(B(ALU_UNIT_SEL.MULU2)){
              io.mul2_oprand_entry.rs1_data := io.scb_readop_i(rs1_addr)
              io.mul2_oprand_entry.rs2_data := io.scb_readop_i(rs2_addr)
              io.mul2_oprand_entry.imm := imm_value
              io.mul2_oprand_entry.rd_addr := rd_addr
              io.mul2_oprand_entry.rd_wten := rd_wten
              io.mul2_oprand_entry.instr := instr
              io.mul2_oprand_entry.op_type.assignFromBits(op_type)
              io.mul2_oprand_entry.dec_valid := True
              io.mul2_oprand_entry.trans_id := trans_id
            }
            is(B(ALU_UNIT_SEL.DIVU)){
              io.div_oprand_entry.rs1_data := io.scb_readop_i(rs1_addr)
              io.div_oprand_entry.rs2_data := io.scb_readop_i(rs2_addr)
              io.div_oprand_entry.imm := imm_value
              io.div_oprand_entry.rd_addr := rd_addr
              io.div_oprand_entry.rd_wten := rd_wten
              io.div_oprand_entry.instr := instr
              io.div_oprand_entry.op_type.assignFromBits(op_type)
              io.div_oprand_entry.dec_valid := True
              io.div_oprand_entry.trans_id := trans_id
            }
            is(B(ALU_UNIT_SEL.LSU)){
              io.lsu_oprand_entry.rs1_data := io.scb_readop_i(rs1_addr)
              io.lsu_oprand_entry.rs2_data := io.scb_readop_i(rs2_addr)
              io.lsu_oprand_entry.imm := imm_value
              io.lsu_oprand_entry.rd_addr := rd_addr
              io.lsu_oprand_entry.rd_wten := rd_wten
              io.lsu_oprand_entry.instr := instr
              io.lsu_oprand_entry.op_type.assignFromBits(op_type)
              io.lsu_oprand_entry.dec_valid := True
              io.lsu_oprand_entry.trans_id := trans_id
            }
            is(B(ALU_UNIT_SEL.BJU)){
              io.bju_oprand_entry.rs1_data := io.scb_readop_i(rs1_addr)
              io.bju_oprand_entry.rs2_data := io.scb_readop_i(rs2_addr)
              io.bju_oprand_entry.imm := imm_value
              io.bju_oprand_entry.rd_addr := rd_addr
              io.bju_oprand_entry.rd_wten := rd_wten
              io.bju_oprand_entry.instr := instr
              io.bju_oprand_entry.op_type.assignFromBits(op_type)
              io.bju_oprand_entry.dec_valid := True
              io.bju_oprand_entry.trans_id := trans_id
            }
            is(B(ALU_UNIT_SEL.CSR)){
              io.csr_oprand_entry.rs1_data := io.scb_readop_i(rs1_addr)
              io.csr_oprand_entry.rs2_data := io.scb_readop_i(rs2_addr)
              io.csr_oprand_entry.imm := imm_value
              io.csr_oprand_entry.rd_addr := rd_addr
              io.csr_oprand_entry.rd_wten := rd_wten
              io.csr_oprand_entry.instr := instr
              io.csr_oprand_entry.op_type.assignFromBits(op_type)
              io.csr_oprand_entry.dec_valid := True
              io.csr_oprand_entry.trans_id := trans_id
            }
          }
        }
      }
      is(EXE) {
        //when (FU_ST(U(alu_sel)) === True) {
        when ((alu_sel===B(ALU_UNIT_SEL.MULU1) || alu_sel===B(ALU_UNIT_SEL.MULU2) || alu_sel===B(ALU_UNIT_SEL.DIVU)) && FU_ST(U(alu_sel)) === True) { // todo
          SCB_IU_TAB(i) := EXE
        } .elsewhen (REG_ST(rd_addr)(1) === True){
          SCB_IU_TAB(i) := EXE
        } .otherwise {
          SCB_IU_TAB(i) := COMMIT
          ex_commit_entry_commit_req := True
          switch(alu_sel){
            is(B(ALU_UNIT_SEL.ALUU)){
              ex_commit_entry_instr := io.alu_ex_entry.instr
              ex_commit_entry_reg_wb_en := rd_wten
              ex_commit_entry_reg_wb_addr := rd_addr
              ex_commit_entry_reg_wb_data := io.alu_ex_entry.result.asUInt
              ex_commit_entry_trans_id := io.alu_ex_entry.trans_id

            }
            is(B(ALU_UNIT_SEL.MULU1)){
              ex_commit_entry_instr := io.mul1_ex_entry.instr
              ex_commit_entry_reg_wb_en := rd_wten
              ex_commit_entry_reg_wb_addr := rd_addr
              ex_commit_entry_reg_wb_data := io.mul1_ex_entry.result
              ex_commit_entry_trans_id := io.mul1_ex_entry.trans_id
            }
            is(B(ALU_UNIT_SEL.MULU2)){
              ex_commit_entry_instr := io.mul2_ex_entry.instr
              ex_commit_entry_reg_wb_en := rd_wten
              ex_commit_entry_reg_wb_addr := rd_addr
              ex_commit_entry_reg_wb_data := io.mul2_ex_entry.result
              ex_commit_entry_trans_id := io.mul2_ex_entry.trans_id
            }
            is(B(ALU_UNIT_SEL.DIVU)){
              ex_commit_entry_instr := io.div_ex_entry.instr
              ex_commit_entry_reg_wb_en := rd_wten
              ex_commit_entry_reg_wb_addr := rd_addr
              ex_commit_entry_reg_wb_data := io.div_ex_entry.result
              ex_commit_entry_trans_id := io.div_ex_entry.trans_id
            }
            is(B(ALU_UNIT_SEL.LSU)){
              ex_commit_entry_instr := io.lsu_ex_entry.instr
              ex_commit_entry_reg_wb_en := rd_wten
              ex_commit_entry_reg_wb_addr := rd_addr
              ex_commit_entry_reg_wb_data := io.lsu_ex_entry.result
              ex_commit_entry_trans_id := io.lsu_ex_entry.trans_id
              // todo with store
              ex_commit_entry_dcache_wb_en := io.lsu_ex_entry.store_wb_en
              ex_commit_entry_dcache_wb_addr := io.lsu_ex_entry.store_wb_addr
              ex_commit_entry_dcache_wb_data := io.lsu_ex_entry.store_wb_data
              ex_commit_entry_dcache_rd_en := io.lsu_ex_entry.load_rd_en
              ex_commit_entry_dcache_rd_addr := io.lsu_ex_entry.load_rd_addr
              ex_commit_entry_dcache_rd_data := io.lsu_ex_entry.load_rd_data
            }
            is(B(ALU_UNIT_SEL.BJU)){
              ex_commit_entry_instr := io.bju_ex_entry.instr
              ex_commit_entry_reg_wb_en := rd_wten
              ex_commit_entry_reg_wb_addr := rd_addr
              ex_commit_entry_reg_wb_data := io.bju_ex_entry.result
              ex_commit_entry_trans_id := io.bju_ex_entry.trans_id
              // todo with mispredict
            }
            is(B(ALU_UNIT_SEL.CSR)){
              ex_commit_entry_instr := io.csr_ex_entry.instr
              ex_commit_entry_reg_wb_en := rd_wten
              ex_commit_entry_reg_wb_addr := rd_addr
              ex_commit_entry_reg_wb_data := io.csr_ex_entry.result
              ex_commit_entry_trans_id := io.csr_ex_entry.trans_id
            }
            default{
              ex_commit_entry_instr := 0
              ex_commit_entry_reg_wb_en := False
              ex_commit_entry_reg_wb_addr := 0
              ex_commit_entry_reg_wb_data := 0
              ex_commit_entry_trans_id := SCB_IU_DEEPTH
            }

          }
        }
      }
      is(COMMIT) {
        when(io.ex_commit_entry.commit_ack === True && io.ex_commit_entry.recv_id === index) {
          //SCB_IU_TAB(i) := IDLE
          instr_end_tab(i) := True
          REG_ST(rs1_addr) := B"0"##rs1_wten
          REG_ST(rs2_addr) := B"0"##rs2_wten
          REG_ST(rd_addr) := rd_wten##B"0"
          ex_commit_entry_commit_req := False
        } .otherwise {
          SCB_IU_TAB(i) := COMMIT
        }

      }
      default {}
    }
  }
  */

  }

  /*
  FU_ST(U(B(ALU_UNIT_SEL.ALUU))) := io.alu_oprand_entry.busy
  FU_ST(U(B(ALU_UNIT_SEL.MULU1))) := io.mul1_oprand_entry.busy
  FU_ST(U(B(ALU_UNIT_SEL.MULU2))) := io.mul2_oprand_entry.busy
  FU_ST(U(B(ALU_UNIT_SEL.DIVU))) := io.div_oprand_entry.busy
  FU_ST(U(B(ALU_UNIT_SEL.BJU))) := io.bju_oprand_entry.busy
  FU_ST(U(B(ALU_UNIT_SEL.LSU))) := io.lsu_oprand_entry.busy
  FU_ST(U(B(ALU_UNIT_SEL.CSR))) := io.csr_oprand_entry.busy
  */
  io.scb_readop_ro := REG_ST_R
  io.scb_readop_wo := REG_ST_W

}