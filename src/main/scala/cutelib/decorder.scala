package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class decorder() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val id_instr_entry = slave(instr_entry(CoreConfig())) // from instr_queue
    //val id_branch_predict_entry = slave(branch_predict_entry(CoreConfig())) // from instr
    val id_dec_entry = master(decorder_entry(CoreConfig())) // to id2is stage
  }
  val dec_alu_sel = ALU_UNIT_SEL()  // Enum
  val dec_op_type = OP_TYPE()
  val dec_rs1_entry = register_entry(CoreConfig())
  val dec_rs2_entry = register_entry(CoreConfig())
  val dec_rd_entry = register_entry(CoreConfig())
  val dec_csr_entry = csr_register_entry(CoreConfig())
  val dec_imm = SInt(ImmBus bits)
  val dec_valid = Bool()
  val predict_flag = Bool()

  val imm_i = SInt(ImmBus bits)
  val imm_bj = SInt(ImmBus bits)
  val imm_csr = SInt(ImmBus bits)
  imm_i := S(io.id_instr_entry.inst(31 downto 20)).resize(ImmBus bits)
  imm_bj := S(io.id_instr_entry.inst(31 downto 31)@@io.id_instr_entry.inst(7 downto 7)@@io.id_instr_entry.inst(30 downto 25)@@io.id_instr_entry.inst(11 downto 8)@@U"0").resize(ImmBus bits)
  imm_csr := S(io.id_instr_entry.inst(19 downto 15).resize(ImmBus bits))

  dec_alu_sel := ALU_UNIT_SEL.NOPU
  dec_op_type := OP_TYPE.OP_NOP
  dec_rs1_entry.reg_addr := io.id_instr_entry.inst(19 downto 15)
  dec_rs2_entry.reg_addr := io.id_instr_entry.inst(24 downto 20)
  dec_rd_entry.reg_addr := io.id_instr_entry.inst(11 downto 7)
  dec_csr_entry.reg_addr := io.id_instr_entry.inst(31 downto 20).resized
  dec_rs1_entry.reg_rden := False
  dec_rs1_entry.reg_wten := False
  dec_rs2_entry.reg_rden := False
  dec_rs2_entry.reg_wten := False
  dec_rd_entry.reg_rden := False
  dec_rd_entry.reg_wten := False
  dec_csr_entry.reg_rden := False
  dec_csr_entry.reg_wten := False
  dec_imm := 0
  dec_valid := False
  predict_flag := False

  when(io.id_instr_entry.valid === True){
    switch(io.id_instr_entry.inst){
      is(ADD,SUB){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_ARITHMETIC
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_valid := True
      }
      is(ADDI){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_ARITHMETIC
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(OR,AND,XOR){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_LOGIC
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_valid := True
      }
      is(ANDI,ORI,XORI){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_LOGIC
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(SLL,SRL,SRA){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_valid := True
      }
      is(SLLI,SRLI,SRAI){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(SLT,SLTU){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_valid := True
      }
      is(SLTI,SLTIU){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(LUI){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rd_entry.reg_wten := True
        dec_imm := S(io.id_instr_entry.inst(31 downto 12)).resize(ImmBus bits)
        dec_valid := True
      }
      is(AUIPC){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rd_entry.reg_wten := True
        dec_imm := S(io.id_instr_entry.inst(31 downto 12)).resize(ImmBus bits)
        dec_valid := True
      }
      is(LB,LH,LW,LBU,LHU){
        dec_alu_sel := ALU_UNIT_SEL.LSU
        dec_op_type := OP_TYPE.OP_LOAD_STORE
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(SB,SH,SW){
        dec_alu_sel := ALU_UNIT_SEL.LSU
        dec_op_type := OP_TYPE.OP_LOAD_STORE
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_imm := S(io.id_instr_entry.inst(31 downto 25)@@io.id_instr_entry.inst(11 downto 7)).resize(ImmBus bits)
        dec_valid := True
      }
      is(JAL){
        dec_alu_sel := ALU_UNIT_SEL.BJU
        dec_op_type := OP_TYPE.OP_JUMP_BRANCH
        dec_rd_entry.reg_wten := (dec_rd_entry.reg_addr =/= U("0"))
        dec_imm := S(io.id_instr_entry.inst(31 downto 31)@@io.id_instr_entry.inst(19 downto 12)@@io.id_instr_entry.inst(20 downto 20)@@io.id_instr_entry.inst(30 downto 21)@@U"0").resize(ImmBus bits)
        dec_valid := True
      }
      is(JALR){
        dec_alu_sel := ALU_UNIT_SEL.BJU
        dec_op_type := OP_TYPE.OP_JUMP_BRANCH
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := (dec_rd_entry.reg_addr =/= U("0"))
        dec_imm := imm_i
        dec_valid := True
      }
      is(BEQ,BNE,BLT,BGE,BLTU,BGEU){
        dec_alu_sel := ALU_UNIT_SEL.BJU
        dec_op_type := OP_TYPE.OP_JUMP_BRANCH
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_imm := imm_bj
        dec_valid := True
        predict_flag := True
      }

      // TODO //
      is(CSRRW,CSRRS,CSRRC){
        dec_alu_sel := ALU_UNIT_SEL.CSR
        dec_op_type := OP_TYPE.OP_NOP
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_csr_entry.reg_rden := True
        dec_csr_entry.reg_wten := True
        dec_valid := True
      }
      is(CSRRWI,CSRRSI,CSRRCI){
        dec_alu_sel := ALU_UNIT_SEL.CSR
        dec_op_type := OP_TYPE.OP_NOP
        dec_rd_entry.reg_wten := True
        dec_csr_entry.reg_rden := True
        dec_csr_entry.reg_wten := True
        dec_imm := imm_csr
        dec_valid := True
      }

      is(MRET,SRET){  // 退出异常
        dec_alu_sel := ALU_UNIT_SEL.NOPU
        dec_op_type := OP_TYPE.OP_NOP
        dec_valid := True
      }

      // NOP实际上汇编编译成ADDI x0,x0,0 //

      default{  // 无论是NOP或者非法指令，都顺序往NOPU发，期间不会有阻塞，FU_ST(NOPU)不会拉高
        dec_alu_sel := ALU_UNIT_SEL.NOPU
        dec_op_type := OP_TYPE.OP_NOP
        dec_rs1_entry.reg_addr := io.id_instr_entry.inst(19 downto 15)
        dec_rs2_entry.reg_addr := io.id_instr_entry.inst(24 downto 20)
        dec_rd_entry.reg_addr := io.id_instr_entry.inst(11 downto 7)
        dec_csr_entry.reg_addr := io.id_instr_entry.inst(31 downto 20).resized
        dec_rs1_entry.reg_rden := False
        dec_rs1_entry.reg_wten := False
        dec_rs2_entry.reg_rden := False
        dec_rs2_entry.reg_wten := False
        dec_rd_entry.reg_rden := False
        dec_rd_entry.reg_wten := False
        dec_csr_entry.reg_rden := False
        dec_csr_entry.reg_wten := False
        dec_imm := 0
        dec_valid := False
        predict_flag := False
        assert( //需要被替代为ilegal instruction 异常 todo
          assertion = True,
          message   = "Unsupported instruction !!!",
          severity  = ERROR
        )
      }
    }
  } .otherwise{
    dec_alu_sel := ALU_UNIT_SEL.NOPU
    dec_op_type := OP_TYPE.OP_NOP
    dec_rs1_entry.reg_addr := io.id_instr_entry.inst(19 downto 15)
    dec_rs2_entry.reg_addr := io.id_instr_entry.inst(24 downto 20)
    dec_rd_entry.reg_addr := io.id_instr_entry.inst(11 downto 7)
    //dec_csr_entry.reg_addr := io.id_instr_entry.inst(31 downto 20)
    dec_csr_entry.reg_addr := io.id_instr_entry.inst(31 downto 20).resized
    dec_rs1_entry.reg_rden := False
    dec_rs1_entry.reg_wten := False
    dec_rs2_entry.reg_rden := False
    dec_rs2_entry.reg_wten := False
    dec_rd_entry.reg_rden := False
    dec_rd_entry.reg_wten := False
    dec_imm := 0
    dec_valid := False
    predict_flag := False
  }

  io.id_dec_entry.pc := io.id_instr_entry.pc
  io.id_dec_entry.instr := io.id_instr_entry.inst
  io.id_dec_entry.alu_sel := dec_alu_sel
  io.id_dec_entry.op_type := dec_op_type
  io.id_dec_entry.rs1_entry := dec_rs1_entry
  io.id_dec_entry.rs2_entry := dec_rs2_entry
  io.id_dec_entry.csr_entry := dec_csr_entry
  io.id_dec_entry.rd_entry := dec_rd_entry
  io.id_dec_entry.imm := dec_imm
  io.id_dec_entry.dec_valid := dec_valid
  io.id_dec_entry.predict_flag := predict_flag

}
