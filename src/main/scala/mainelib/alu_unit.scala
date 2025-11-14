package mainelib

import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class alu_unit(cfg : CoreConfig, id: Int, latency: Int = 1) extends Component {
  val io = new Bundle {
    val ex_in_entry = Vec.fill(cfg.issueWidth)(slave(issue_entry(cfg)))  // from issue, broadcast
    val ex_out_entry = master(exe_res_entry(cfg))  // to commit
    val unit_busy = out Bool()
    val flush_mispredict_in = slave(flush_mispredict_entry(cfg))
  }
  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
  val unit_hit = Bool()
  issue_id := 0
  unit_hit := False

  io.unit_busy := False  // todo
  io.ex_out_entry.issue_id := issue_id


  for (i <- 0 until cfg.issueWidth) {
    when(io.ex_in_entry(i).fu_id.asBits.asUInt === id){
      issue_id := i
      unit_hit := io.ex_in_entry(i).valid
    }
  }

  val rs1 = io.ex_in_entry(issue_id).rs1_scb_entry.reg_rdata
  val rs2 = io.ex_in_entry(issue_id).rs2_scb_entry.reg_rdata
  val imm = io.ex_in_entry(issue_id).imm
  val shamt = io.ex_in_entry(issue_id).instr(24 downto 20)
  val rs2_shift = if(cfg.rv32)  rs2(4 downto 0) else rs2(5 downto 0) // for rv32ui [rv64:(5 downto 0)]
  val pc = io.ex_in_entry(issue_id).pc
  val alu_res_logic = UInt(cfg.RegDataBus bits)
  val alu_res_shift = UInt(cfg.RegDataBus bits)
  val alu_res_arithmetic = SInt(cfg.RegDataBus bits)
  val alu_res_move = UInt(cfg.RegDataBus bits)
  val alu_res = SInt(cfg.RegDataBus bits)


  val out_trans_id = io.ex_in_entry(issue_id).trans_id
  val in_trans_id = io.flush_mispredict_in.trans_id
  val after_mispredict = io.flush_mispredict_in.hit && ((in_trans_id(cfg.SCB_ID_WIDTH - 1) === out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) <= out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0))) || (in_trans_id(cfg.SCB_ID_WIDTH - 1) =/= out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) > out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0)) ))

  io.ex_out_entry.trans_id := io.ex_in_entry(issue_id).trans_id
  io.ex_out_entry.instr := io.ex_in_entry(issue_id).instr
  io.ex_out_entry.pc := io.ex_in_entry(issue_id).pc
  io.ex_out_entry.result := alu_res
  io.ex_out_entry.result_vld := unit_hit & ~after_mispredict
  io.ex_out_entry.issue_id := issue_id

  io.ex_out_entry.reg_wif.preg_addr := io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_rename
  io.ex_out_entry.reg_wif.areg_addr := io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_real
  io.ex_out_entry.reg_wif.reg_wten := io.ex_in_entry(issue_id).rd_scb_entry.reg_wten
  io.ex_out_entry.reg_wif.reg_wdata := io.ex_out_entry.result.asUInt

  alu_res_logic := 0
  alu_res_shift := 0
  alu_res_move := 0
  alu_res_arithmetic := 0

  // todo with other ALU instructions
  when(unit_hit) {
    switch(io.ex_in_entry(issue_id).op_type) {
      is(OP_TYPE.OP_LOGIC) {
        switch(io.ex_in_entry(issue_id).instr) {
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
        switch(io.ex_in_entry(issue_id).instr) {
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
        switch(io.ex_in_entry(issue_id).instr) {
          //...//
          default {
            alu_res_move := 0
          }
        }
      }
      is(OP_TYPE.OP_ARITHMETIC) {
        switch(io.ex_in_entry(issue_id).instr) {
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

  switch(io.ex_in_entry(issue_id).op_type){
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