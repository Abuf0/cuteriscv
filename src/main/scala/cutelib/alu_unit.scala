package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class alu_unit() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val dec_entry = slave(decorder_entry(CoreConfig())) // from issue stage
    val ex_operand_entry = slave(operand_entry(CoreConfig()))  // from issue
    val alu_ex_entry = master(alu_res_entry(CoreConfig()))  // to commit
  }
  val rs1 = io.ex_operand_entry.rs1_data
  val rs2 = io.ex_operand_entry.rs2_data
  val imm = io.ex_operand_entry.imm
  val shamt = io.ex_operand_entry.instr(24 downto 20)
  val rs2_shift = rs2(4 downto 0) // for rv32ui [rv64:(5 downto 0)]
  val pc = io.ex_operand_entry.pc
  val alu_res_logic = UInt(RegDataBus bits)
  val alu_res_shift = UInt(RegDataBus bits)
  val alu_res_arithmetic = SInt(RegDataBus bits)
  val alu_res_move = UInt(RegDataBus bits)
  val alu_res = SInt(RegDataBus bits)
  val alu_trans_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val alu_instr = Reg(UInt(InstBus bits)) init(0)
  val alu_pc = Reg(UInt(InstAddrBus bits)) init(0)
  val alu_res_out = Reg(SInt(RegDataBus bits)) init(0)

  when(io.ex_operand_entry.dec_valid){
    io.ex_operand_entry.busy := True
  } . otherwise{
    io.ex_operand_entry.busy := False
  }

  when(io.ex_operand_entry.dec_valid){
    alu_trans_id := io.ex_operand_entry.trans_id
    alu_instr := io.ex_operand_entry.instr
    alu_pc := io.ex_operand_entry.pc
    alu_res_out := alu_res
  } . otherwise{  }

  io.alu_ex_entry.trans_id := alu_trans_id
  io.alu_ex_entry.instr := alu_instr
  io.alu_ex_entry.pc := alu_pc
  io.alu_ex_entry.result := alu_res_out

  alu_res_logic := 0
  alu_res_shift := 0
  alu_res_move := 0
  alu_res_arithmetic := 0

  // todo with other ALU instructions
  when(io.ex_operand_entry.busy === True) {
    switch(io.ex_operand_entry.op_type) {
      is(OP_TYPE.OP_LOGIC) {
        switch(io.ex_operand_entry.instr) {
          is(AND) {
            alu_res_logic := rs1 & rs2
          }
          is(OR) {
            alu_res_logic := rs1 | rs2
          }
          is(XOR) {
            alu_res_logic := rs1 ^ rs2
          }
          is(ANDI) {
            alu_res_logic := rs1 & U(imm)
          }
          is(ORI) {
            alu_res_logic := rs1 | U(imm)
          }
          is(XORI) {
            alu_res_logic := rs1 ^ U(imm)
          }
          //...//
          default {
            alu_res_logic := 0
          }
        }
      }
      is(OP_TYPE.OP_SHIFT) {
        switch(io.ex_operand_entry.instr) {
          is(SLL) {
            //alu_res_shift := rs1 |<< rs2
            alu_res_shift := rs1 |<< rs2_shift  // fix rv32ui
          }
          is(SRL) {
            //alu_res_shift := rs1 |>> rs2
            alu_res_shift := rs1 |>> rs2_shift  // fix rv32ui
          }
          is(SLLI) {
            alu_res_shift := rs1 |<< shamt
          }
          is(SRLI) {
            alu_res_shift := rs1 |>> shamt
          }
          is(SRA) {
            //alu_res_shift := U(S(rs1) >> rs2)  // fix shift
            alu_res_shift := U(S(rs1) >> rs2_shift)  // fix shift + rv32ui
          }
          is(SRAI) {
            alu_res_shift := U(S(rs1) >> shamt)  // fix shift
          }
          is(SLT) {
            alu_res_shift := U(S(rs1) < S(rs2)).resized
          }
          is(SLTI) {
            alu_res_shift := U(S(rs1) < imm).resized
          }
          is(SLTU) {
            alu_res_shift := U(rs1 < rs2).resized
          }
          is(SLTIU) {
            alu_res_shift := U(rs1 < U(imm)).resized
          }
          is(LUI) {
            alu_res_shift := U(imm) |<<12
          }
          is(AUIPC) {
            alu_res_shift := (U(imm) |<<12) + pc
          }
          //...//
          default {
            alu_res_shift := 0
          }
        }
      }
      is(OP_TYPE.OP_MOVE) {
        switch(io.ex_operand_entry.instr) {
          //...//
          default {
            alu_res_move := 0
          }
        }
      }
      is(OP_TYPE.OP_ARITHMETIC) {
        switch(io.ex_operand_entry.instr) {
          is(ADD) {
            alu_res_arithmetic := S(rs1) + S(rs2)
          }
          is(SUB) {
            alu_res_arithmetic := S(rs1) - S(rs2)
          }
          is(ADDI) {
            alu_res_arithmetic := S(rs1) + imm
          }
          //...//
          default {
            alu_res_arithmetic := 0
          }
        }
      }
      default{
        alu_res_logic := 0
        alu_res_shift := 0
        alu_res_move := 0
        alu_res_arithmetic := 0
      }
    }
  } .otherwise{
    alu_res_logic := 0
    alu_res_shift := 0
    alu_res_move := 0
    alu_res_arithmetic := 0
  }

  switch(io.ex_operand_entry.op_type){
    is(OP_TYPE.OP_LOGIC){
      alu_res := S(alu_res_logic)
    }
    is(OP_TYPE.OP_SHIFT){
      alu_res := S(alu_res_shift)
    }
    is(OP_TYPE.OP_MOVE){
      alu_res := S(alu_res_move)
    }
    is(OP_TYPE.OP_ARITHMETIC){
      alu_res := alu_res_arithmetic
    }
    default{
      alu_res := 0
    }
  }

}