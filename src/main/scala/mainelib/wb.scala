package mainelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

case class wb(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val pre_cmt_info = slave(ex2wb_entry(cfg))
    val cmt_if = Vec.fill(cfg.issueWidth)(master(commit_entry(cfg)))
    val cmt_back_if = Vec.fill(cfg.issueWidth)(slave(commit_entry(cfg)))
    val mem_rdata = in UInt(cfg.DMemDataWidth bits) // from mmu
    val mem_rvalid = in Bool() // from mmu
    val mem_rerr = in Bool() // from mmu
    val commit_ready = in Bool()
    val in_stall = in Bool()
    val in_flush_g = in Bool() // global
    val in_flush_l = in Bool()  // local
    val in_flush_g_id = in UInt(cfg.SCB_ID_WIDTH bits)
    val in_flush_l_id = in UInt(cfg.SCB_ID_WIDTH bits)
  }

  val WB_TAB = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(Reg(commit_entry(cfg))))
  val retired_flag = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(Reg(Bool()) init(False)))
  WB_TAB.foreach(_.foreach((_.init(commit_entry(cfg).getZero))))
  val wb_ptr = Reg(UInt(cfg.SCB_INSTR_WIDTH bits)) init(0)
  val cmt_ptr = Reg(UInt(cfg.SCB_INSTR_WIDTH bits)) init(0)
  val flush_l_ptr = io.in_flush_l_id.resize(cfg.SCB_INSTR_WIDTH bits)
  val flush_g_ptr = io.in_flush_g_id.resize(cfg.SCB_INSTR_WIDTH bits)

  io.cmt_if := Vec(WB_TAB.map(_(wb_ptr)))

  when(io.in_flush_g) {
    wb_ptr := flush_g_ptr
  } .elsewhen(!io.in_stall && io.commit_ready){
    when(WB_TAB.map(_(wb_ptr).commit_vld).asBits.andR === True){
      wb_ptr := wb_ptr + 1  // todo: 要求SCB_IU_DEPTH必须2的幂次方
    }
  }


  when(!io.in_stall && (io.cmt_if.map(_.commit_vld).asBits.andR)){
    cmt_ptr := io.cmt_if(0).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
  }

    for (i <- 0 until cfg.issueWidth) {
    for (j <- 0 until cfg.SCB_IU_DEPTH) {
      val cmt_vld = io.cmt_back_if(i).commit_vld
      val cmt_id = io.cmt_back_if(i).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
      when(cmt_vld){
        when(j === cmt_id) {
          retired_flag(i)(j) := True
        } .elsewhen(j === (cmt_id-1).resize(cfg.SCB_INSTR_WIDTH)) { // 这个“-2”很有灵性
          retired_flag(i)(j) := False
        }
      }

      val is_old_instr = False
      when(cmt_ptr < flush_l_ptr) {
        is_old_instr := (j > cmt_ptr) && (j <= flush_l_ptr)
      } .elsewhen(cmt_ptr > flush_l_ptr) {
        is_old_instr := (j > cmt_ptr) || (j <= flush_l_ptr)
      }

      // todo 考虑同issue中的flush
      when(io.in_flush_l && !is_old_instr) {  /*** cmt_ptr -> flush_l_ptr之间的不刷新，包括flush_l_ptr ***/
        WB_TAB(i)(j).commit_vld := False
      }.elsewhen(!io.in_stall) {

        // ALU
        for (k <- 0 until FU_ID.enums.ALU.size) {
          val wb_vld = io.pre_cmt_info.ex2wb_payload.alu_ex_out(k).result_vld
          val issue_id = io.pre_cmt_info.ex2wb_payload.alu_ex_out(k).issue_id
          val wb_id = io.pre_cmt_info.ex2wb_payload.alu_ex_out(k).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
          when(wb_vld && (issue_id === i) && ~retired_flag(i)(wb_id)) {
            WB_TAB(i)(wb_id).commit_vld := True
            WB_TAB(i)(wb_id).trans_id := io.pre_cmt_info.ex2wb_payload.alu_ex_out(k).trans_id
            WB_TAB(i)(wb_id).dec_valid := True
            WB_TAB(i)(wb_id).pc := io.pre_cmt_info.ex2wb_payload.alu_ex_out(k).pc
            WB_TAB(i)(wb_id).instr := io.pre_cmt_info.ex2wb_payload.alu_ex_out(k).instr
            WB_TAB(i)(wb_id).reg_wif := io.pre_cmt_info.ex2wb_payload.alu_ex_out(k).reg_wif
          }.elsewhen(cmt_vld){
            WB_TAB(i)(cmt_id).commit_vld := False
            WB_TAB(i)(cmt_id).reg_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_rden := False
            WB_TAB(i)(cmt_id).mem_wif.mem_wen := False
            WB_TAB(i)(cmt_id).mem_rif.mem_ren := False
            WB_TAB(i)(cmt_id).mem_rif.mem_rerr := False
          }
        }

        // MUL
        for (k <- 0 until FU_ID.enums.MUL.size) {
          val wb_vld = io.pre_cmt_info.ex2wb_payload.mul_ex_out(k).result_vld
          val issue_id = io.pre_cmt_info.ex2wb_payload.mul_ex_out(k).issue_id
          val wb_id = io.pre_cmt_info.ex2wb_payload.mul_ex_out(k).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
          when(wb_vld && (issue_id === i) && ~retired_flag(i)(wb_id)) {
            WB_TAB(i)(wb_id).commit_vld := True
            WB_TAB(i)(wb_id).trans_id := io.pre_cmt_info.ex2wb_payload.mul_ex_out(k).trans_id
            WB_TAB(i)(wb_id).dec_valid := True
            WB_TAB(i)(wb_id).pc := io.pre_cmt_info.ex2wb_payload.mul_ex_out(k).pc
            WB_TAB(i)(wb_id).instr := io.pre_cmt_info.ex2wb_payload.mul_ex_out(k).instr
            WB_TAB(i)(wb_id).reg_wif := io.pre_cmt_info.ex2wb_payload.mul_ex_out(k).reg_wif
          }.elsewhen(cmt_vld){
            WB_TAB(i)(cmt_id).commit_vld := False
            WB_TAB(i)(cmt_id).reg_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_rden := False
            WB_TAB(i)(cmt_id).mem_wif.mem_wen := False
            WB_TAB(i)(cmt_id).mem_rif.mem_ren := False
            WB_TAB(i)(cmt_id).mem_rif.mem_rerr := False
          }
        }

        // DIV
        for (k <- 0 until FU_ID.enums.DIV.size) {
          val wb_vld = io.pre_cmt_info.ex2wb_payload.div_ex_out(k).result_vld
          val issue_id = io.pre_cmt_info.ex2wb_payload.div_ex_out(k).issue_id
          val wb_id = io.pre_cmt_info.ex2wb_payload.div_ex_out(k).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
          when(wb_vld && (issue_id === i) && ~retired_flag(i)(wb_id)) {
            WB_TAB(i)(wb_id).commit_vld := True
            WB_TAB(i)(wb_id).trans_id := io.pre_cmt_info.ex2wb_payload.div_ex_out(k).trans_id
            WB_TAB(i)(wb_id).dec_valid := True
            WB_TAB(i)(wb_id).pc := io.pre_cmt_info.ex2wb_payload.div_ex_out(k).pc
            WB_TAB(i)(wb_id).instr := io.pre_cmt_info.ex2wb_payload.div_ex_out(k).instr
            WB_TAB(i)(wb_id).reg_wif := io.pre_cmt_info.ex2wb_payload.div_ex_out(k).reg_wif
          }.elsewhen(cmt_vld){
            WB_TAB(i)(cmt_id).commit_vld := False
            WB_TAB(i)(cmt_id).reg_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_rden := False
            WB_TAB(i)(cmt_id).mem_wif.mem_wen := False
            WB_TAB(i)(cmt_id).mem_rif.mem_ren := False
            WB_TAB(i)(cmt_id).mem_rif.mem_rerr := False
          }
        }


        // BJU
        for (k <- 0 until FU_ID.enums.BJU.size) {
          val wb_vld = io.pre_cmt_info.ex2wb_payload.bju_ex_out(k).result_vld
          val issue_id = io.pre_cmt_info.ex2wb_payload.bju_ex_out(k).issue_id
          val wb_id = io.pre_cmt_info.ex2wb_payload.bju_ex_out(k).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
          when(wb_vld && (issue_id === i) && ~retired_flag(i)(wb_id)) {
            WB_TAB(i)(wb_id).commit_vld := True
            WB_TAB(i)(wb_id).trans_id := io.pre_cmt_info.ex2wb_payload.bju_ex_out(k).trans_id
            WB_TAB(i)(wb_id).dec_valid := True
            WB_TAB(i)(wb_id).pc := io.pre_cmt_info.ex2wb_payload.bju_ex_out(k).pc
            WB_TAB(i)(wb_id).instr := io.pre_cmt_info.ex2wb_payload.bju_ex_out(k).instr
            WB_TAB(i)(wb_id).reg_wif := io.pre_cmt_info.ex2wb_payload.bju_ex_out(k).reg_wif
            // todo : 考虑branch_predict
          }.elsewhen(cmt_vld){
            WB_TAB(i)(cmt_id).commit_vld := False
            WB_TAB(i)(cmt_id).reg_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_rden := False
            WB_TAB(i)(cmt_id).mem_wif.mem_wen := False
            WB_TAB(i)(cmt_id).mem_rif.mem_ren := False
            WB_TAB(i)(cmt_id).mem_rif.mem_rerr := False
          }
        }

        // LSU
        for (k <- 0 until FU_ID.enums.LSU.size) {
          val is_store = io.pre_cmt_info.ex2wb_payload.lsu_ex_store_if(k).store_wb_en
          val is_load = io.pre_cmt_info.ex2wb_payload.lsu_ex_load_if(k).load_rd_en
          val wb_vld = io.pre_cmt_info.ex2wb_payload.lsu_ex_out(k).result_vld //&& (is_store || (is_load && (io.mem_rvalid || io.mem_rerr)))
          val issue_id = io.pre_cmt_info.ex2wb_payload.lsu_ex_out(k).issue_id
          val wb_id = io.pre_cmt_info.ex2wb_payload.lsu_ex_out(k).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
          when(wb_vld && (issue_id === i) && ~retired_flag(i)(wb_id)) {
            WB_TAB(i)(wb_id).commit_vld := True
            WB_TAB(i)(wb_id).trans_id := io.pre_cmt_info.ex2wb_payload.lsu_ex_out(k).trans_id
            WB_TAB(i)(wb_id).dec_valid := True
            WB_TAB(i)(wb_id).pc := io.pre_cmt_info.ex2wb_payload.lsu_ex_out(k).pc
            WB_TAB(i)(wb_id).instr := io.pre_cmt_info.ex2wb_payload.lsu_ex_out(k).instr
            WB_TAB(i)(wb_id).reg_wif := io.pre_cmt_info.ex2wb_payload.lsu_ex_out(k).reg_wif
            WB_TAB(i)(wb_id).mem_wif.mem_waddr := io.pre_cmt_info.ex2wb_payload.lsu_ex_store_if(k).store_wb_addr
            WB_TAB(i)(wb_id).mem_wif.mem_wen := io.pre_cmt_info.ex2wb_payload.lsu_ex_store_if(k).store_wb_en
            WB_TAB(i)(wb_id).mem_wif.mem_wdata := io.pre_cmt_info.ex2wb_payload.lsu_ex_store_if(k).store_wb_data
            WB_TAB(i)(wb_id).mem_wif.mem_sel := io.pre_cmt_info.ex2wb_payload.lsu_ex_store_if(k).store_wb_byte
            WB_TAB(i)(wb_id).mem_rif.mem_ren := io.pre_cmt_info.ex2wb_payload.lsu_ex_load_if(k).load_rd_en
            WB_TAB(i)(wb_id).mem_rif.mem_raddr := io.pre_cmt_info.ex2wb_payload.lsu_ex_load_if(k).load_rd_addr
            WB_TAB(i)(wb_id).mem_rif.mem_sel := io.pre_cmt_info.ex2wb_payload.lsu_ex_load_if(k).load_rd_byte
            WB_TAB(i)(wb_id).mem_rif.mem_rdata := io.mem_rdata  // todo
            WB_TAB(i)(wb_id).mem_rif.mem_rerr := io.mem_rerr
          }.elsewhen(cmt_vld){
            WB_TAB(i)(cmt_id).commit_vld := False
            WB_TAB(i)(cmt_id).reg_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_rden := False
            WB_TAB(i)(cmt_id).mem_wif.mem_wen := False
            WB_TAB(i)(cmt_id).mem_rif.mem_ren := False
            WB_TAB(i)(cmt_id).mem_rif.mem_rerr := False
          }
        }

        // CSR
        for (k <- 0 until FU_ID.enums.CSR.size) {
          val wb_vld = io.pre_cmt_info.ex2wb_payload.csr_ex_out(k).result_vld
          val issue_id = io.pre_cmt_info.ex2wb_payload.csr_ex_out(k).issue_id
          val wb_id = io.pre_cmt_info.ex2wb_payload.csr_ex_out(k).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
          when(wb_vld && (issue_id === i) && ~retired_flag(i)(wb_id)) {
            WB_TAB(i)(wb_id).commit_vld := True
            WB_TAB(i)(wb_id).trans_id := io.pre_cmt_info.ex2wb_payload.csr_ex_out(k).trans_id
            WB_TAB(i)(wb_id).dec_valid := True
            WB_TAB(i)(wb_id).pc := io.pre_cmt_info.ex2wb_payload.csr_ex_out(k).pc
            WB_TAB(i)(wb_id).instr := io.pre_cmt_info.ex2wb_payload.csr_ex_out(k).instr
            WB_TAB(i)(wb_id).reg_wif := io.pre_cmt_info.ex2wb_payload.csr_ex_out(k).reg_wif
            WB_TAB(i)(wb_id).csr_wif := io.pre_cmt_info.ex2wb_payload.csr_ex_out(k).csr_wif
          }.elsewhen(cmt_vld){
            WB_TAB(i)(cmt_id).commit_vld := False
            WB_TAB(i)(cmt_id).reg_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_rden := False
            WB_TAB(i)(cmt_id).mem_wif.mem_wen := False
            WB_TAB(i)(cmt_id).mem_rif.mem_ren := False
            WB_TAB(i)(cmt_id).mem_rif.mem_rerr := False
          }
        }

        // FPU
        /*
        for (k <- 0 until FU_ID.enums.FPU.size) {
          val wb_vld = io.pre_cmt_info.ex2wb_payload.fpu_ex_out(k).result_vld
          val issue_id = io.pre_cmt_info.ex2wb_payload.fpu_ex_out(k).issue_id
          val wb_id = io.pre_cmt_info.ex2wb_payload.fpu_ex_out(k).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
          when(wb_vld && (issue_id === i) && ~retired_flag(i)(wb_id)) {
            WB_TAB(i)(wb_id).commit_vld := True
            WB_TAB(i)(wb_id).trans_id := io.pre_cmt_info.ex2wb_payload.fpu_ex_out(k).trans_id
            WB_TAB(i)(wb_id).pc := io.pre_cmt_info.ex2wb_payload.fpu_ex_out(k).pc
            WB_TAB(i)(wb_id).instr := io.pre_cmt_info.ex2wb_payload.fpu_ex_out(k).instr
            WB_TAB(i)(wb_id).reg_wif := io.pre_cmt_info.ex2wb_payload.fpu_ex_out(k).reg_wif
          }.elsewhen(cmt_vld){
            WB_TAB(i)(cmt_id).commit_vld := False
            WB_TAB(i)(cmt_id).reg_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_rden := False
            WB_TAB(i)(cmt_id).mem_wif.mem_wen := False
            WB_TAB(i)(cmt_id).mem_rif.mem_ren := False
            WB_TAB(i)(cmt_id).mem_rif.mem_rerr := False
          }
        }
         */

        // NOP
        for (k <- 0 until FU_ID.enums.NOP.size) {
          val wb_vld = io.pre_cmt_info.ex2wb_payload.nop_ex_out(k).result_vld
          val issue_id = io.pre_cmt_info.ex2wb_payload.nop_ex_out(k).issue_id
          val wb_id = io.pre_cmt_info.ex2wb_payload.nop_ex_out(k).trans_id(cfg.SCB_INSTR_WIDTH-1 downto 0)
          when(wb_vld && (issue_id === i) && ~retired_flag(i)(wb_id)) {
            WB_TAB(i)(wb_id).commit_vld := True
            WB_TAB(i)(wb_id).trans_id := io.pre_cmt_info.ex2wb_payload.nop_ex_out(k).trans_id
            WB_TAB(i)(wb_id).pc := io.pre_cmt_info.ex2wb_payload.nop_ex_out(k).pc
            WB_TAB(i)(wb_id).instr := io.pre_cmt_info.ex2wb_payload.nop_ex_out(k).instr
            WB_TAB(i)(wb_id).instr_err := io.pre_cmt_info.ex2wb_payload.nop_ex_out(k).instr_err
            WB_TAB(i)(wb_id).dec_valid := io.pre_cmt_info.ex2wb_payload.nop_ex_out(k).dec_valid
          }.elsewhen(cmt_vld){
            WB_TAB(i)(cmt_id).commit_vld := False
            WB_TAB(i)(cmt_id).reg_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_wten := False
            WB_TAB(i)(cmt_id).csr_wif.reg_rden := False
            WB_TAB(i)(cmt_id).mem_wif.mem_wen := False
            WB_TAB(i)(cmt_id).mem_rif.mem_ren := False
            WB_TAB(i)(cmt_id).mem_rif.mem_rerr := False
          }
        }


      }
    }
  }

}
