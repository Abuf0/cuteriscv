package mainelib
import spinal.core._
import spinal.lib._

/**  **/

case class ras(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val pc_fl = slave(Flow(UInt(cfg.InstAddrBus bits)))
    val predict_fl = master(Flow(UInt(cfg.InstAddrBus bits)))
    val predict_branch_if = slave(branch_predict_entry(cfg)) // from btb
    val resolved_if = slave(branch_predict_entry(cfg))  // from exe
  }
  val ras_stack = Vec(Reg(UInt(cfg.InstAddrBus bits)) init(0), cfg.RAS_STACK_DEPTH)
  val ras_stack_backup = Vec(Reg(UInt(cfg.InstAddrBus bits)) init(0), cfg.RAS_STACK_DEPTH)
  val ras_ptr = Reg(UInt(cfg.RAS_PTR_WIDTH bits)) init(0)
  val ras_ptr_backup = Reg(UInt(cfg.RAS_PTR_WIDTH bits)) init(0)

  io.predict_fl.valid := io.predict_branch_if.is_ret
  io.predict_fl.payload := ras_stack(ras_ptr-1)
  // RAS的call/ret对错主要和前级分支预测的对错有关

  when(io.resolved_if.call_cor){ // 预测call错误
    ras_ptr := ras_ptr - 1
  } .elsewhen(io.resolved_if.ret_cor) {  // 预测ret错误
    ras_stack(ras_ptr) := ras_stack_backup(ras_ptr)
    ras_ptr := ras_ptr + 1
  }.elsewhen(io.predict_branch_if.is_call) {
    ras_stack(ras_ptr) := io.predict_branch_if.branch_target
    ras_ptr := ras_ptr + 1
  }.elsewhen(io.predict_branch_if.is_ret) {
    ras_ptr := ras_ptr - 1
  }



  // 提交栈
  when(~io.resolved_if.call_cor && io.resolved_if.is_call){
    ras_stack_backup(ras_ptr_backup) := ras_stack(ras_ptr_backup)
    ras_ptr_backup := ras_ptr_backup + 1
  } .elsewhen(~io.resolved_if.ret_cor && io.resolved_if.is_ret){
    ras_ptr_backup := ras_ptr_backup - 1
  }


}
