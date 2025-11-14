package mainelib
import spinal.core._
import spinal.lib._
/**  **/
case class issue_buffer(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val issue_in_if = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(slave(issue_entry(cfg))))
    val push_ptr = slave(Flow(UInt(cfg.SCB_ID_WIDTH bits)))
    val retire_ptr = slave(Flow(UInt(cfg.SCB_ID_WIDTH bits)))
    val issue_ptr = Vec.fill(cfg.issueWidth)(slave(Flow(UInt(cfg.SCB_ID_WIDTH bits))))
    val issue_ptr_next_real = Vec.fill(cfg.issueWidth)(in UInt(cfg.SCB_ID_WIDTH-1 bits))
    val issue_out_buff = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(master(issue_entry(cfg))))
    val flush_g_hit = in Bool()
    val flush_l_hit = in Bool()
    val flush_l_id = in UInt(cfg.SCB_ID_WIDTH bits)
    val fu_id_update = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(slave(fu_id_entry(cfg))) )
    val fu_id_update_valid = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)((in Bool())))
    val cmt_if = Vec.fill(cfg.issueWidth)(slave(cmt_reg_entry(cfg)))  // from commit
    val ex_fwd_if = Vec.fill(cfg.issueWidth)(slave(cmt_reg_entry(cfg)))  // from exe done
    val wb_fwd_if = Vec.fill(cfg.issueWidth)(slave(cmt_reg_entry(cfg)))  // from wb done
  }
  val push_ptr_real = io.push_ptr.payload(cfg.SCB_ID_WIDTH-2 downto 0)
  val issue_ptr_real = Vec.fill(cfg.issueWidth)(UInt(cfg.SCB_ID_WIDTH-1 bits))
  val retire_ptr_real = io.retire_ptr.payload(cfg.SCB_ID_WIDTH-2 downto 0).asBits.asUInt
  val flush_l_ptr = io.flush_l_id.resize(cfg.SCB_INSTR_WIDTH bits)

  io.issue_out_buff.setAsReg()
  io.issue_out_buff.foreach(_.foreach((_.init(issue_entry(cfg).getZero))))

  for (i <- 0 until cfg.issueWidth) {
    issue_ptr_real(i) := io.issue_ptr(i).payload(cfg.SCB_ID_WIDTH - 2 downto 0)

    for (j <- 0 until cfg.SCB_IU_DEPTH) {

      val is_old_instr = False

      // 区别于WB的flush，issue只需保留flush_l_ptr之前的信息
      // todo ： 考虑同issue中的flush
      when(retire_ptr_real < flush_l_ptr) {
        is_old_instr := (j > retire_ptr_real) && (j < flush_l_ptr)
      } .elsewhen(retire_ptr_real > flush_l_ptr) {
        is_old_instr := (j > retire_ptr_real) || (j < flush_l_ptr)
      }

      when(io.flush_g_hit || (io.flush_l_hit && !is_old_instr)) {
        io.issue_out_buff(i)(j).valid := False
        io.issue_out_buff(i)(j).issue_busy := False
      } .otherwise {
        when(io.push_ptr.valid && (j===push_ptr_real)) {
          io.issue_out_buff(i)(j) := io.issue_in_if(i)(j)

          /*** 增加corner情况
                         ____
          issue_hit ____|    \____
                         ____
          cmt_hit   ____|    \____
                              _____
          pready    _________|
                              ______
          issue_buff_________|_______ 此时issue_buff在push时吃不到pready，push完后也和cmt_hit擦肩而过；
                                      因此在push时增加cmt判断，forwarding ready和rdata
           ***/
          for (k <- 0 until cfg.issueWidth) {
            val cmt_hit = io.cmt_if(k).cmt_valid && io.cmt_if(k).reg_commitin.hasDest
            val rs1_fwd = (io.cmt_if(k).reg_commitin.old_p === io.issue_in_if(i)(j).rs1_scb_entry.reg_addr_rename) && io.issue_in_if(i)(j).rs1_scb_entry.reg_rden
            val rs2_fwd = (io.cmt_if(k).reg_commitin.old_p === io.issue_in_if(i)(j).rs2_scb_entry.reg_addr_rename) && io.issue_in_if(i)(j).rs2_scb_entry.reg_rden
            when(cmt_hit && rs1_fwd) {
              io.issue_out_buff(i)(j).rs1_scb_entry.reg_ready := True
              io.issue_out_buff(i)(j).rs1_scb_entry.reg_rdata := io.cmt_if(k).reg_commitin.rd_data
            }
            when(cmt_hit && rs2_fwd) {
              io.issue_out_buff(i)(j).rs2_scb_entry.reg_ready := True
              io.issue_out_buff(i)(j).rs2_scb_entry.reg_rdata := io.cmt_if(k).reg_commitin.rd_data
            }
          }

        }
        when(io.issue_ptr(i).valid && (j===issue_ptr_real(i))) {
          io.issue_out_buff(i)(j).issue_busy := True
        }
        when(io.fu_id_update_valid(i)(io.issue_ptr_next_real(i)) && (j===io.issue_ptr_next_real(i))){
          io.issue_out_buff(i)(j).fu_id := io.fu_id_update(i)(j).fu_id
        }
        when(io.retire_ptr.valid && (j===retire_ptr_real)) {
          io.issue_out_buff(i)(j).valid := False
          io.issue_out_buff(i)(j).issue_busy := False
        }
      }

    }
  }

  //// forwarding
  for (i <- 0 until cfg.issueWidth) {
    for (j <- 0 until cfg.SCB_IU_DEPTH) {
      val rs1_rden = io.issue_out_buff(i)(j).rs1_scb_entry.reg_rden
      val rs2_rden = io.issue_out_buff(i)(j).rs2_scb_entry.reg_rden
      val csr_rden = io.issue_out_buff(i)(j).csr_entry.reg_rden
      val rs1_addr = io.issue_out_buff(i)(j).rs1_scb_entry.reg_addr_rename
      val rs2_addr = io.issue_out_buff(i)(j).rs2_scb_entry.reg_addr_rename
      val csr_addr = io.issue_out_buff(i)(j).csr_entry.reg_addr
      for (k <- 0 until cfg.issueWidth) {
        when(rs1_rden && (rs1_addr === io.ex_fwd_if(k).reg_commitin.old_p) && io.ex_fwd_if(k).cmt_valid && io.ex_fwd_if(k).reg_commitin.hasDest) {
          io.issue_out_buff(i)(j).rs1_scb_entry.reg_rdata := io.ex_fwd_if(k).reg_commitin.rd_data
          io.issue_out_buff(i)(j).rs1_scb_entry.reg_ready := True
        }
        when(rs2_rden && (rs2_addr === io.ex_fwd_if(k).reg_commitin.old_p) && io.ex_fwd_if(k).cmt_valid && io.ex_fwd_if(k).reg_commitin.hasDest) {
          io.issue_out_buff(i)(j).rs2_scb_entry.reg_rdata := io.ex_fwd_if(k).reg_commitin.rd_data
          io.issue_out_buff(i)(j).rs2_scb_entry.reg_ready := True
        }
        when(csr_rden && (csr_addr === io.ex_fwd_if(k).csr_commitin.rd_arch) && io.ex_fwd_if(k).cmt_valid && io.ex_fwd_if(k).csr_commitin.hasDest) {
          io.issue_out_buff(i)(j).csr_entry.reg_rdata := io.ex_fwd_if(k).reg_commitin.rd_data
          io.issue_out_buff(i)(j).csr_entry.reg_ready := True
        }
        when(rs1_rden && (rs1_addr === io.wb_fwd_if(k).reg_commitin.old_p) && io.wb_fwd_if(k).cmt_valid && io.wb_fwd_if(k).reg_commitin.hasDest) {
          io.issue_out_buff(i)(j).rs1_scb_entry.reg_rdata := io.wb_fwd_if(k).reg_commitin.rd_data
          io.issue_out_buff(i)(j).rs1_scb_entry.reg_ready := True
        }
        when(rs2_rden && (rs2_addr === io.wb_fwd_if(k).reg_commitin.old_p) && io.wb_fwd_if(k).cmt_valid && io.wb_fwd_if(k).reg_commitin.hasDest) {
          io.issue_out_buff(i)(j).rs2_scb_entry.reg_rdata := io.wb_fwd_if(k).reg_commitin.rd_data
          io.issue_out_buff(i)(j).rs2_scb_entry.reg_ready := True
        }
        when(csr_rden && (csr_addr === io.wb_fwd_if(k).csr_commitin.rd_arch) && io.wb_fwd_if(k).cmt_valid && io.wb_fwd_if(k).csr_commitin.hasDest) {
          io.issue_out_buff(i)(j).csr_entry.reg_rdata := io.wb_fwd_if(k).reg_commitin.rd_data
          io.issue_out_buff(i)(j).csr_entry.reg_ready := True
        }
        when(rs1_rden && (rs1_addr === io.cmt_if(k).reg_commitin.old_p) && io.cmt_if(k).cmt_valid && io.cmt_if(k).reg_commitin.hasDest) {
          io.issue_out_buff(i)(j).rs1_scb_entry.reg_rdata := io.cmt_if(k).reg_commitin.rd_data
          io.issue_out_buff(i)(j).rs1_scb_entry.reg_ready := True
        }
        when(rs2_rden && (rs2_addr === io.cmt_if(k).reg_commitin.old_p) && io.cmt_if(k).cmt_valid && io.cmt_if(k).reg_commitin.hasDest) {
          io.issue_out_buff(i)(j).rs2_scb_entry.reg_rdata := io.cmt_if(k).reg_commitin.rd_data
          io.issue_out_buff(i)(j).rs2_scb_entry.reg_ready := True
        }
        when(csr_rden && (csr_addr === io.cmt_if(k).csr_commitin.rd_arch) && io.cmt_if(k).cmt_valid && io.cmt_if(k).csr_commitin.hasDest) {
          io.issue_out_buff(i)(j).csr_entry.reg_rdata := io.cmt_if(k).reg_commitin.rd_data
          io.issue_out_buff(i)(j).csr_entry.reg_ready := True
        }

      }
    }
  }

}
