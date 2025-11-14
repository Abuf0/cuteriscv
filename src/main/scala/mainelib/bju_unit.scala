package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class bju_unit(cfg : CoreConfig, id: Int, latency: Int = 1) extends Component {
  val io = new Bundle {
    val ex_in_entry = Vec.fill(cfg.issueWidth)(slave(issue_entry(cfg)))  // from issue, broadcast
    val ex_out_entry = master(exe_res_entry(cfg))  // to commit
    val unit_busy = out Bool()
    val bju_unit_hit = out Bool()

    val ex_branch_predict = slave(branch_predict_entry(cfg)) // from issue
    val bju_branch_predict = master(branch_predict_entry(cfg)) // to commit
    // todo with mispredict //
    val flush_mispredict_in = slave(flush_mispredict_entry(cfg))
    val flush_mispredict_out = master(flush_mispredict_entry(cfg))
  }

  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
  val unit_hit = Bool()
  issue_id := 0
  unit_hit := False
  io.bju_unit_hit := unit_hit

  io.unit_busy := False  // todo

  val out_trans_id = io.ex_in_entry(issue_id).trans_id
  val in_trans_id = io.flush_mispredict_in.trans_id
  val after_mispredict = io.flush_mispredict_in.hit && ((in_trans_id(cfg.SCB_ID_WIDTH - 1) === out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) <= out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0))) || (in_trans_id(cfg.SCB_ID_WIDTH - 1) =/= out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) > out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0)) ))

  for (i <- 0 until cfg.issueWidth) {
    when(io.ex_in_entry(i).fu_id.asBits.asUInt === id){
      issue_id := i
      unit_hit := io.ex_in_entry(i).valid & !after_mispredict
    }
  }

  val rs1 = io.ex_in_entry(issue_id).rs1_scb_entry.reg_rdata
  val rs2 = io.ex_in_entry(issue_id).rs2_scb_entry.reg_rdata
  val imm = io.ex_in_entry(issue_id).imm
  val pc_c = io.ex_branch_predict.pc

  val bju_result = UInt(cfg.RegDataBus bits)
  val target_pc = UInt(cfg.InstAddrBus bits)
  val branch_taken = Bool()
  val dec_bju = io.ex_in_entry(issue_id).dec_bju_info
  bju_result := 0
  target_pc := 0
  branch_taken := False

  when(unit_hit){
    switch(io.ex_in_entry(issue_id).instr){
      is(JAL(cfg.rvc)){
        target_pc := pc_c + U(imm)
        bju_result := pc_c + cfg.InstLen
        branch_taken := True
      }
      is(JALR){
        target_pc := rs1 + U(imm)
        bju_result := pc_c + cfg.InstLen
        branch_taken := True
      }
      is(BEQ(cfg.rvc)){
        when(rs1 === rs2) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + cfg.InstLen
          branch_taken := False
        }
      }
      is(BNE(cfg.rvc)){
        when(rs1 =/= rs2) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + cfg.InstLen
          branch_taken := False
        }
      }
      is(BLT(cfg.rvc)){
        when(S(rs1) < S(rs2)) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + cfg.InstLen
          branch_taken := False
        }
      }
      is(BGE(cfg.rvc)){
        when(S(rs1) >= S(rs2)) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + cfg.InstLen
          branch_taken := False
        }
      }
      is(BLTU(cfg.rvc)){
        when(rs1 < rs2) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + cfg.InstLen
          branch_taken := False
        }
      }
      is(BGEU(cfg.rvc)){
        when(rs1 >= rs2) {
          target_pc := pc_c + U(imm)
          branch_taken := True
        } .otherwise{
          target_pc := pc_c + cfg.InstLen
          branch_taken := False
        }
      }
      default{
        target_pc := pc_c + cfg.InstLen
        bju_result := 0
        branch_taken := False
      }
    }
  } .otherwise{
    target_pc := pc_c + cfg.InstLen
    bju_result := 0
    branch_taken := False
  }
  // todo: 把上面三个看情况改成dec_valid寄存器输出，为了修复EXE hold时组合逻辑会变（其实其他unit都是寄存器输出来着，）

  // todo with call/ret mis

  io.flush_mispredict_out.hit := (io.bju_branch_predict.call_cor || io.bju_branch_predict.ret_cor || io.bju_branch_predict.branch_cor || io.bju_branch_predict.jump_cor) & unit_hit
  io.flush_mispredict_out.target := io.bju_branch_predict.branch_target
  io.flush_mispredict_out.trans_id := io.ex_in_entry(issue_id).trans_id


  io.bju_branch_predict.call_cor := (io.ex_branch_predict.is_call =/= dec_bju.dec_is_call) || (target_pc =/= io.ex_branch_predict.branch_target)
  io.bju_branch_predict.ret_cor := (io.ex_branch_predict.is_ret =/= dec_bju.dec_is_ret) || (target_pc =/= io.ex_branch_predict.branch_target)
  io.bju_branch_predict.branch_cor := (io.ex_branch_predict.is_branch =/= dec_bju.dec_is_branch) || ((branch_taken =/= io.ex_branch_predict.branch_taken) || (target_pc =/= io.ex_branch_predict.branch_target))
  io.bju_branch_predict.jump_cor :=  (io.ex_branch_predict.is_jump =/= dec_bju.dec_is_jump) || (target_pc =/= io.ex_branch_predict.branch_target)
  io.bju_branch_predict.branch_target := target_pc
  io.bju_branch_predict.is_branch := io.ex_branch_predict.is_branch
  io.bju_branch_predict.is_call := io.ex_branch_predict.is_call
  io.bju_branch_predict.is_ret := io.ex_branch_predict.is_ret
  io.bju_branch_predict.is_jump := io.ex_branch_predict.is_jump

  io.ex_out_entry.result := bju_result.resized.asSInt // todo
  io.ex_out_entry.result_vld := unit_hit & ~after_mispredict
  io.ex_out_entry.trans_id := io.ex_in_entry(issue_id).trans_id
  io.ex_out_entry.pc := io.ex_in_entry(issue_id).pc
  io.ex_out_entry.instr := io.ex_in_entry(issue_id).instr

  io.ex_out_entry.reg_wif.preg_addr := io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_rename
  io.ex_out_entry.reg_wif.areg_addr := io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_real
  io.ex_out_entry.reg_wif.reg_wten := io.ex_in_entry(issue_id).rd_scb_entry.reg_wten
  io.ex_out_entry.reg_wif.reg_wdata := io.ex_out_entry.result.asUInt

  io.bju_branch_predict.branch_valid := io.ex_branch_predict.branch_valid
  io.bju_branch_predict.branch_taken := branch_taken
  io.bju_branch_predict.pc := io.ex_branch_predict.pc

  io.ex_out_entry.issue_id := issue_id


}

