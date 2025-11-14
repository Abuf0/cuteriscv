package mainelib
import spinal.core._
import spinal.lib._
import RISCV_ISA._

case class decoder(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val instr_fl =  Vec.fill(cfg.issueWidth)(slave(Flow(UInt(cfg.InstBus bits))))
    val dec_if = Vec.fill(cfg.issueWidth)(master(dec_entry(cfg)))
    val instr_err = Vec.fill(cfg.issueWidth)(in Bool())
  }

  /** expand instr **/
  val instr_exp = Vec.fill(cfg.issueWidth)(UInt(cfg.InstBus bits))
  for (i <- 0 until cfg.issueWidth) {
    if(cfg.rvc) {
      instr_exp(i) := RvcDecompressor(io.instr_fl(i).payload.asBits, xlen = cfg.xlen).asUInt
    } else {
      instr_exp(i) := io.instr_fl(i).payload
    }
  }

  /** initial **/
  val imm_i = Vec.fill(cfg.issueWidth)(SInt(cfg.ImmBus bits))
  val imm_bj = Vec.fill(cfg.issueWidth)(SInt(cfg.ImmBus bits))
  val imm_csr = Vec.fill(cfg.issueWidth)(SInt(cfg.ImmBus bits))
  for (i <- 0 until cfg.issueWidth) {
    io.dec_if(i).alu_sel := ALU_UNIT_SEL.NOP
    io.dec_if(i).op_type := OP_TYPE.OP_NOP
    io.dec_if(i).rs1_entry.reg_addr := io.instr_fl(i).payload(19 downto 15).resized
    io.dec_if(i).rs2_entry.reg_addr := io.instr_fl(i).payload(24 downto 20).resized
    io.dec_if(i).rd_entry.reg_addr := io.instr_fl(i).payload(11 downto 7).resized
    io.dec_if(i).csr_entry.reg_addr := io.instr_fl(i).payload(31 downto 20).resized
    io.dec_if(i).rs1_entry.reg_rden := False
    io.dec_if(i).rs1_entry.reg_wten := False
    io.dec_if(i).rs2_entry.reg_rden := False
    io.dec_if(i).rs2_entry.reg_wten := False
    io.dec_if(i).rd_entry.reg_rden := False
    io.dec_if(i).rd_entry.reg_wten := False
    io.dec_if(i).csr_entry.reg_rden := False
    io.dec_if(i).csr_entry.reg_wten := False
    io.dec_if(i).imm := 0
    io.dec_if(i).dec_valid := False
    io.dec_if(i).instr_err := False
    io.dec_if(i).dec_bju_info.assignFromBits(B(0,dec_bju_entry(cfg).getBitsWidth bits))
    imm_i(i) := S(io.instr_fl(i).payload(31 downto 20)).resized
    imm_bj(i) := S((io.instr_fl(i).payload(31 downto 31)@@io.instr_fl(i).payload(7 downto 7)@@io.instr_fl(i).payload(30 downto 25)@@io.instr_fl(i).payload(11 downto 8)@@U"0")).resized
    imm_csr(i) := S(io.instr_fl(i).payload(19 downto 15)).resized
  }

  /** decode **/
  for (i <- 0 until cfg.issueWidth) {
    when(io.instr_fl(i).valid === True) { // valid include err
      when(io.instr_err(i)){
        io.dec_if(i).alu_sel := ALU_UNIT_SEL.NOP
        io.dec_if(i).op_type := OP_TYPE.OP_NOP
        io.dec_if(i).instr_err := True
        io.dec_if(i).dec_valid := True
      } .otherwise {
        io.dec_if(i).dec_bju_info.dec_is_branch := io.instr_fl(i).payload(6 downto 0) === U"1100011" // branch
        io.dec_if(i).dec_bju_info.dec_is_call := io.instr_fl(i).payload(6 downto 4) === U"110" && io.instr_fl(i).payload(2 downto 0) === U"111" && (io.instr_fl(i).payload(11 downto 7) === U"00001" || io.instr_fl(i).payload(11 downto 7) === U"00101")// JAL or JALR, rd=x1/x5
        io.dec_if(i).dec_bju_info.dec_is_ret := io.instr_fl(i).payload(6 downto 0) === U"1100111" && io.instr_fl(i).payload(11 downto 7) === U"0" && io.instr_fl(i).payload(19 downto 18) === U"00" && io.instr_fl(i).payload(16 downto 15) === U"01"   // JALR, rd=0, rs=x1/x5
        io.dec_if(i).dec_bju_info.dec_is_jump := io.instr_fl(i).payload(6 downto 0) === U"1100111" //&& io.if_branch_predict.is_call === False && io.if_branch_predict.is_ret === False // JALR
        switch(io.instr_fl(i).payload) {
          is(ADD, SUB) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_ARITHMETIC
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rs2_entry.reg_rden := (io.dec_if(i).rs2_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).dec_valid := True
          }
          is(ADDI) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_ARITHMETIC
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := imm_i(i)
            io.dec_if(i).dec_valid := True
          }
          is(OR, AND, XOR) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_LOGIC
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rs2_entry.reg_rden := (io.dec_if(i).rs2_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).dec_valid := True
          }
          is(ANDI, ORI, XORI) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_LOGIC
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := imm_i(i)
            io.dec_if(i).dec_valid := True
          }
          is(SLL, SRL, SRA) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_SHIFT
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rs2_entry.reg_rden := (io.dec_if(i).rs2_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).dec_valid := True
          }
          is(SLLI, SRLI, SRAI) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_SHIFT
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := imm_i(i)
            io.dec_if(i).dec_valid := True
          }
          is(SLT, SLTU) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_SHIFT
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rs2_entry.reg_rden := (io.dec_if(i).rs2_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).dec_valid := True
          }
          is(SLTI, SLTIU) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_SHIFT
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := imm_i(i)
            io.dec_if(i).dec_valid := True
          }
          is(LUI) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_SHIFT
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := S(io.instr_fl(i).payload(31 downto 12)).resized
            io.dec_if(i).dec_valid := True
          }
          is(AUIPC) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.ALU
            io.dec_if(i).op_type := OP_TYPE.OP_SHIFT
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := S(io.instr_fl(i).payload(31 downto 12)).resized
            io.dec_if(i).dec_valid := True
          }
          is(LB, LH, LW, LBU, LHU) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.LSU
            io.dec_if(i).op_type := OP_TYPE.OP_LOAD
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := imm_i(i)
            io.dec_if(i).dec_valid := True
          }
          is(SB, SH, SW) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.LSU
            io.dec_if(i).op_type := OP_TYPE.OP_STORE
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rs2_entry.reg_rden := (io.dec_if(i).rs2_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := S(io.instr_fl(i).payload(31 downto 25) @@ io.instr_fl(i).payload(11 downto 7)).resized
            io.dec_if(i).dec_valid := True
          }
          is(JAL(cfg.rvc)) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.BJU
            io.dec_if(i).op_type := OP_TYPE.OP_JUMP_BRANCH
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := S(io.instr_fl(i).payload(31 downto 31) @@ io.instr_fl(i).payload(19 downto 12) @@ io.instr_fl(i).payload(20 downto 20) @@ io.instr_fl(i).payload(30 downto 21) @@ U"0").resized
            io.dec_if(i).dec_valid := True
          }
          is(JALR) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.BJU
            io.dec_if(i).op_type := OP_TYPE.OP_JUMP_BRANCH
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := imm_i(i)
            io.dec_if(i).dec_valid := True
          }
          is(BEQ(cfg.rvc), BNE(cfg.rvc), BLT(cfg.rvc), BGE(cfg.rvc), BLTU(cfg.rvc), BGEU(cfg.rvc)) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.BJU
            io.dec_if(i).op_type := OP_TYPE.OP_JUMP_BRANCH
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rs2_entry.reg_rden := (io.dec_if(i).rs2_entry.reg_addr =/= U("0"))
            io.dec_if(i).imm := imm_bj(i)
            io.dec_if(i).dec_valid := True
          }

          // TODO //
          is(CSRRW, CSRRS, CSRRC) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.CSR
            io.dec_if(i).op_type := OP_TYPE.OP_NOP
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).csr_entry.reg_rden := True
            io.dec_if(i).csr_entry.reg_wten := True
            io.dec_if(i).dec_valid := True
          }
          is(CSRRWI, CSRRSI, CSRRCI) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.CSR
            io.dec_if(i).op_type := OP_TYPE.OP_NOP
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).csr_entry.reg_rden := True
            io.dec_if(i).csr_entry.reg_wten := True
            io.dec_if(i).imm := imm_csr(i)
            io.dec_if(i).dec_valid := True
          }

          is(MRET, SRET) { // 退出异常
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.NOP
            io.dec_if(i).op_type := OP_TYPE.OP_NOP
            io.dec_if(i).dec_valid := True
          }

          is(EBREAK) { // breakpoint
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.NOP
            io.dec_if(i).op_type := OP_TYPE.OP_NOP
            io.dec_if(i).dec_valid := True
          }

          // M Extension //
          is(MUL, MULH, MULHU, MULHSU) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.MUL
            io.dec_if(i).op_type := OP_TYPE.OP_ARITHMETIC
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rs2_entry.reg_rden := (io.dec_if(i).rs2_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).dec_valid := True
          }

          is(DIV, DIVU, REM, REMU) {
            io.dec_if(i).alu_sel := ALU_UNIT_SEL.DIV
            io.dec_if(i).op_type := OP_TYPE.OP_ARITHMETIC
            io.dec_if(i).rs1_entry.reg_rden := (io.dec_if(i).rs1_entry.reg_addr =/= U("0"))
            io.dec_if(i).rs2_entry.reg_rden := (io.dec_if(i).rs2_entry.reg_addr =/= U("0"))
            io.dec_if(i).rd_entry.reg_wten := (io.dec_if(i).rd_entry.reg_addr =/= U("0"))
            io.dec_if(i).dec_valid := True
          }
        }
      }
    }
  }

}
