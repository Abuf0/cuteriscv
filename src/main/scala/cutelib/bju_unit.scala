package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class bju_unit() extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val dec_entry = slave(decorder_entry(CoreConfig())) // from issue stage
    val ex_operand_entry = slave(operand_entry(CoreConfig()))  // from issue
    val ex_branch_predict = slave(branch_predict_entry(CoreConfig())) // from issue
    //val bju_res = out UInt(RegDataBus bits) // bju to commit
    val bju_branch_predict = master(branch_predict_entry(CoreConfig())) // to commit
    val bju_busy_ack = out Bool()  // 0: idle 1: busy
    val bju_mispredict = master(branch_mispredict_entry(CoreConfig())) // to ras/...
    val bju_ex_entry = master(bju_res_entry(CoreConfig()))  // to commit
    // todo with exception //
  }

  val target_pc = Reg(UInt(InstAddrBus bits)) init(0)
  val rs1 = io.ex_operand_entry.rs1_data
  val rs2 = io.ex_operand_entry.rs2_data
  val imm = io.ex_operand_entry.imm
  val pc_c = io.ex_branch_predict.pc
  val bju_result = Reg(UInt(RegDataBus bits)) init(0)
  val branch_taken = Reg(Bool()) init(False)
  val bju_trans_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val bju_pc = Reg(UInt(InstAddrBus bits)) init(0)
  val bju_instr = Reg(UInt(InstBus bits)) init(0)
  /*
  val bju_mis_branch_cor = Reg(Bool()) init(False)
  val bju_mis_call_cor = Reg(Bool()) init(False)
  val bju_mis_ret_cor = Reg(Bool()) init(False)
  val bju_mis_target_pc = Reg(UInt(InstAddrBus bits)) init(0)
*/

  when(io.ex_operand_entry.dec_valid){
    io.ex_operand_entry.busy := True
  } . otherwise{
    io.ex_operand_entry.busy := False
  }

  when(io.ex_operand_entry.dec_valid){
    bju_trans_id := io.ex_operand_entry.trans_id
    bju_pc := io.ex_operand_entry.pc
    bju_instr := io.ex_operand_entry.instr
  } . otherwise{  }


  when(io.ex_operand_entry.busy === True){
    switch(io.ex_operand_entry.instr){
      is(JAL){
        target_pc := pc_c + U(imm)
        bju_result := pc_c + 4
      }
      is(JALR){
        target_pc := rs1 + U(imm)
        bju_result := pc_c + 4
      }
      is(BEQ){
        when(rs1 === rs2) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + 4
          branch_taken := False
        }
      }
      is(BNE){
        when(rs1 =/= rs2) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + 4
          branch_taken := False
        }
      }
      is(BLT){
        when(S(rs1) < S(rs2)) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + 4
          branch_taken := False
        }
      }
      is(BGE){
        when(S(rs1) >= S(rs2)) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + 4
          branch_taken := False
        }
      }
      is(BLTU){
        when(rs1 < rs2) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + 4
          branch_taken := False
        }
      }
      is(BGEU){
        when(rs1 >= rs2) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + 4
          branch_taken := False
        }
      }
      default{
        target_pc := pc_c+4
        bju_result := 0
        branch_taken := False
      }
    }
  } .otherwise{
    target_pc := pc_c+4
    bju_result := 0
    branch_taken := False
  }
  val ex_branch_predict_is_branch = Reg(Bool()) init(False)
  val ex_branch_predict_branch_taken = Reg(Bool()) init(False)
  val ex_branch_predict_branch_target = Reg(UInt(InstAddrBus bits)) init(0)
  ex_branch_predict_is_branch := io.ex_branch_predict.is_branch
  ex_branch_predict_branch_taken := io.ex_branch_predict.branch_taken
  ex_branch_predict_branch_target := io.ex_branch_predict.branch_target
  // todo with call/ret mis
  io.bju_mispredict.call_cor := False
  io.bju_mispredict.ret_cor := False
  io.bju_mispredict.branch_cor := ex_branch_predict_is_branch && ((branch_taken =/= ex_branch_predict_branch_taken) || (target_pc =/= ex_branch_predict_branch_target))
  io.bju_mispredict.target_pc := target_pc

  /*
  io.bju_mispredict.call_cor := bju_mis_call_cor
  io.bju_mispredict.ret_cor := bju_mis_ret_cor
  io.bju_mispredict.branch_cor := bju_mis_branch_cor
  io.bju_mispredict.target_pc := bju_mis_target_pc

  bju_mis_call_cor := False
  bju_mis_ret_cor := False
  bju_mis_branch_cor := ex_branch_predict_is_branch && ((branch_taken =/= ex_branch_predict_branch_taken) || (target_pc =/= ex_branch_predict_branch_target))
  bju_mis_target_pc := target_pc
*/

  io.bju_ex_entry.result := bju_result
  io.bju_ex_entry.trans_id := bju_trans_id
  io.bju_ex_entry.pc := bju_pc
  io.bju_ex_entry.instr := bju_instr

  io.bju_branch_predict.setAsReg()
  io.bju_branch_predict.branch_valid := io.ex_branch_predict.branch_valid
  io.bju_branch_predict.branch_taken := branch_taken
  io.bju_branch_predict.branch_target := target_pc
  io.bju_branch_predict.is_branch := io.ex_branch_predict.is_branch
  io.bju_branch_predict.is_call := io.ex_branch_predict.is_call
  io.bju_branch_predict.is_ret := io.ex_branch_predict.is_ret
  io.bju_branch_predict.pc := io.ex_branch_predict.pc

}
