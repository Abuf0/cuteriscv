package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class flush_stall_ctrl(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    // flush source
    val branch_predict_hit = in Bool()  // from if2_fm
    val bju_mispredict = in Bool()  // from exe.bju_unit
    val bju_mispredict_id = in UInt(cfg.SCB_ID_WIDTH bits)
    val trap_hit = in Bool()  // from commit
    val trap_id = in UInt(cfg.SCB_ID_WIDTH bits)
    // stall source
    val instr_queue_full = in Bool()
    val instr_queue_empty = in Bool()
    val scb_buff_full = in Bool()
    val scb_buff_empty = in Bool()
    // out
    val out_flush = Vec.fill(cfg.PipeDepth)(out Bool())
    val out_stall = Vec.fill(cfg.PipeDepth)(out Bool())
    val out_flush_id_local = out UInt(cfg.SCB_ID_WIDTH bits)
    val out_flush_hit_local = out Bool()
    val out_flush_id_global = out UInt(cfg.SCB_ID_WIDTH bits)
  }
  val branch_predict_hit = io.branch_predict_hit  // todo
  // flush行为预留2拍左右处理，而不是一直拉高
  val bju_mispredict_hit_d1 = RegNext(io.bju_mispredict) init(False)
  val bju_mispredict_hit_d2 = RegNext(bju_mispredict_hit_d1) init(False)
  val trap_hit_d1 = RegNext(io.trap_hit) init(False)
  val trap_hit_d2 = RegNext(trap_hit_d1) init(False)

  val bju_mispredict_hit = io.bju_mispredict & ~bju_mispredict_hit_d2
  val trap_hit = io.trap_hit & ~trap_hit_d2

  /*** 注意： branch_predict_hit只flush前级
             bju_mispredict对于ISSUE->EXE->WB都需要保留旧的，flush新的
            trap hit会flush所有
  ***/
  io.out_flush(PP_STATE.IF1_PC.position) := False
  io.out_stall(PP_STATE.IF1_PC.position) := io.instr_queue_full
  io.out_flush(PP_STATE.IF2_TM.position) := branch_predict_hit || bju_mispredict_hit || trap_hit
  io.out_stall(PP_STATE.IF2_TM.position) := io.instr_queue_full
  io.out_flush(PP_STATE.IF3_FM.position) := bju_mispredict_hit || trap_hit
  io.out_stall(PP_STATE.IF3_FM.position) := io.scb_buff_full
  io.out_flush(PP_STATE.ID.position) := bju_mispredict_hit || trap_hit
  io.out_stall(PP_STATE.ID.position) := io.instr_queue_empty || io.scb_buff_full
  io.out_flush(PP_STATE.ISSUE.position) := trap_hit
  io.out_stall(PP_STATE.ISSUE.position) := io.instr_queue_empty
  io.out_flush(PP_STATE.EXE.position) := trap_hit/*bju_mispredict_hit || trap_hit*/
  io.out_stall(PP_STATE.EXE.position) := False/*io.scb_buff_empty*/
  io.out_flush(PP_STATE.WB.position) :=  trap_hit/*trap_hit*/
  io.out_stall(PP_STATE.WB.position) := False
  io.out_flush(PP_STATE.COMMIT.position) := False
  io.out_stall(PP_STATE.COMMIT.position) := False

//  when(bju_mispredict_hit) {
//    io.out_flush_id_issue := io.bju_mispredict_id
//  } .elsewhen(trap_hit) {
//    io.out_flush_id_issue := io.trap_id
//  } .otherwise {
//    io.out_flush_id_issue := 0
//  }
  io.out_flush_id_local := io.bju_mispredict_id
  io.out_flush_hit_local := bju_mispredict_hit
  io.out_flush_id_global := io.trap_id
}
