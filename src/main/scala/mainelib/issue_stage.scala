package mainelib
import spinal.core._
import spinal.lib._
/** with scb **/
case class issue_stage(cfg : CoreConfig) extends Component {
  val io = new Bundle {
    val instr_fl = Vec.fill(cfg.issueWidth)(slave(Flow(UInt(cfg.InstBus bits)))) // from id
    val pc_fl = Vec.fill(cfg.issueWidth)(slave(Flow(UInt(cfg.InstAddrBus bits)))) // from id
    val predict_if = slave(branch_predict_entry(cfg)) // from id
    val dec_if = Vec.fill(cfg.issueWidth)(slave(dec_entry(cfg)))  // from id, reg from rename
    val in_stall = in Bool()
    val in_flush_g = in Bool()  // global
    val in_flush_g_id = in UInt(cfg.SCB_ID_WIDTH bits)
    val in_flush_l = in Bool()  // local
    val in_flush_l_id = in UInt(cfg.SCB_ID_WIDTH bits)

    val rs1_if = Vec.fill(cfg.issueWidth)(master(preg_rd_if(cfg))) // to reg_tmp
    val rs2_if = Vec.fill(cfg.issueWidth)(master(preg_rd_if(cfg))) // to reg_tmp
    val csr_if = Vec.fill(cfg.issueWidth)(master(csr_rd_if(cfg))) // to csr_tmp

    val cmt_if = Vec.fill(cfg.issueWidth)(slave(cmt_reg_entry(cfg)))  // from commit
    val ex_fwd_if = Vec.fill(cfg.issueWidth)(slave(cmt_reg_entry(cfg)))  // from exe done
    val wb_fwd_if = Vec.fill(cfg.issueWidth)(slave(cmt_reg_entry(cfg)))  // from wb done

    val alu_busy = Vec.fill(FU_ID.enums.ALU.size)(in Bool())  // from exe
    val mul_busy = Vec.fill(FU_ID.enums.MUL.size)(in Bool())
    val div_busy = Vec.fill(FU_ID.enums.DIV.size)(in Bool())
    val lsu_busy = Vec.fill(FU_ID.enums.LSU.size)(in Bool())
    val store_busy = in Bool()
    val bju_busy = Vec.fill(FU_ID.enums.BJU.size)(in Bool())
    val csr_busy = Vec.fill(FU_ID.enums.CSR.size)(in Bool())
    val fpu_busy = Vec.fill(FU_ID.enums.FPU.size)(in Bool())
    val nop_busy = Vec.fill(FU_ID.enums.NOP.size)(in Bool())

    val lsu_vld = Vec.fill(FU_ID.enums.LSU.size)(in Bool())


    val issue_out_if = Vec.fill(cfg.issueWidth)(master(issue_entry(cfg))) // to exe
    val buffer_full = out Bool()
    val buffer_empry = out Bool()
    val bju_hit = in Bool()
    val bju_mis_flush = in Bool()
  }
  val reg_rename = new RegRename(cfg)
  val issue_buff = new issue_buffer(cfg)

  val in_flush_all = io.in_flush_l || io.in_flush_g
  val push_ptr, retire_ptr = Flow(UInt(cfg.SCB_ID_WIDTH bits))
  val issue_ptr = Vec.fill(cfg.issueWidth)(Flow(UInt(cfg.SCB_ID_WIDTH bits)))
  val issue_ptr_next = Vec.fill(cfg.issueWidth)(UInt(cfg.SCB_ID_WIDTH bits))

  val push_ptr_next = push_ptr.payload + 1

  val buffer_full = (push_ptr.payload(cfg.SCB_INSTR_WIDTH) ^ retire_ptr.payload(cfg.SCB_INSTR_WIDTH)) && (push_ptr.payload(cfg.SCB_INSTR_WIDTH-1 downto 0) === retire_ptr.payload(cfg.SCB_INSTR_WIDTH-1 downto 0))
  /*** buffer near full ***/
  //val buffer_full = (push_ptr_next(cfg.SCB_INSTR_WIDTH) ^ retire_ptr.payload(cfg.SCB_INSTR_WIDTH)) && (push_ptr_next(cfg.SCB_INSTR_WIDTH-1 downto 0) === retire_ptr.payload(cfg.SCB_INSTR_WIDTH-1 downto 0))

  val buffer_empty = (retire_ptr.payload === push_ptr.payload)
  val issue_next_ov = Vec.fill(cfg.issueWidth)(Bool())
  val hazard = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(Bool()))
  val raw_hazard = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(Bool()))
  val fu_hazard = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(Bool()))
  val lsu_hazard = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(Bool()))
  val hazard_hit = hazard.asBits.andR

  val fu_id_update = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(fu_id_entry(cfg)))
  val fu_id_update_valid = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(Bool()))

  io.buffer_full := buffer_full || reg_rename.io.out_stall
  io.buffer_empry := buffer_empty

  push_ptr.valid := !buffer_full && !io.in_stall && reg_rename.io.reg_out.map(_.valid).asBits.andR && !in_flush_all && io.instr_fl.map(_.valid).asBits.orR
  retire_ptr.valid := !buffer_empty  && !in_flush_all && io.cmt_if(0).cmt_valid
  val issue_ptr_real = Vec.fill(cfg.issueWidth)(UInt(cfg.SCB_ID_WIDTH-1 bits))
  val issue_ptr_next_real = Vec.fill(cfg.issueWidth)(UInt(cfg.SCB_ID_WIDTH-1 bits))
  val push_ptr_real = push_ptr.payload(cfg.SCB_ID_WIDTH-2 downto 0)

  push_ptr.payload.setAsReg() init(0)
  retire_ptr.payload.setAsReg() init(0)
  issue_ptr.foreach(_.payload.setAsReg() init(0))

  when(io.in_flush_g) {
    push_ptr.payload := io.in_flush_g_id + 1
  } .elsewhen(io.in_flush_l){
    push_ptr.payload := io.in_flush_l_id + 1
  } .elsewhen(push_ptr.valid){
    push_ptr.payload := push_ptr.payload + 1
  }

  when(io.in_flush_g){
    retire_ptr.payload := retire_ptr.payload
  } .elsewhen(retire_ptr.valid){
    //when(io.cmt_if(0).cmt_id === cfg.SCB_IU_DEPTH-1){
    //  retire_ptr.payload := 0
    //} .otherwise{
      retire_ptr.payload := io.cmt_if(0).cmt_id
    //}
  }

  for (i <- 0 until cfg.issueWidth) {
    issue_ptr_real(i) := issue_ptr(i).payload(cfg.SCB_ID_WIDTH-2 downto 0)
    issue_ptr_next_real(i) := issue_ptr_next(i)(cfg.SCB_ID_WIDTH-2 downto 0)
    issue_ptr(i).valid := !io.in_stall && !buffer_empty && (!in_flush_all) && !hazard(i)(issue_ptr_next_real(i))
    issue_ptr(i).payload.setAsReg()
    when(io.in_flush_g){
      issue_ptr(i).payload := retire_ptr.payload
    } .elsewhen(issue_ptr(i).valid){
      issue_ptr(i).payload := issue_ptr_next(i)
    }
  }


  for (i <- 0 until cfg.issueWidth) {
    reg_rename.io.reg_in(i).valid := io.instr_fl(i).valid && ~buffer_full
    reg_rename.io.reg_in(i).rs1 := io.dec_if(i).rs1_entry.reg_addr
    reg_rename.io.reg_in(i).rs1_rden := io.dec_if(i).rs1_entry.reg_rden
    reg_rename.io.reg_in(i).rs2 := io.dec_if(i).rs2_entry.reg_addr
    reg_rename.io.reg_in(i).rs2_rden := io.dec_if(i).rs2_entry.reg_rden
    reg_rename.io.reg_in(i).rd := io.dec_if(i).rd_entry.reg_addr
    reg_rename.io.reg_in(i).hasDest := io.dec_if(i).rd_entry.reg_wten
    reg_rename.io.reg_in(i).isBranch := (io.dec_if(i).op_type === OP_TYPE.OP_JUMP_BRANCH)
    reg_rename.io.commit(i) := io.cmt_if(i).reg_commitin
    reg_rename.io.wb(i) := io.wb_fwd_if(i).reg_commitin
  }
  // todo: rollback/ckptOut ...
  reg_rename.io.rollback.assignFromBits(B(0,reg_rename.io.rollback.getBitsWidth bits))
  val rob_token = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(Flow(CkptToken(cfg))))
  rob_token.setAsReg()
  rob_token.foreach(_.foreach(_.valid init(False)))

  val ckpt_trans_id = push_ptr.payload(cfg.SCB_ID_WIDTH-2 downto 0)
  val rollback_trans_id = io.in_flush_l_id.resize(cfg.SCB_INSTR_WIDTH)

  val is_old_instr = False

  // 区别于WB的flush，issue只需保留flush_l_ptr之前的信息
  // todo ： 考虑同issue中的flush
  val retire_ptr_real = retire_ptr.payload(cfg.SCB_ID_WIDTH-2 downto 0).asBits.asUInt
  val flush_l_ptr = io.in_flush_l_id.resize(cfg.SCB_INSTR_WIDTH bits)


  for (i <- 0 until cfg.issueWidth) {
    for (j <- 0 until cfg.SCB_IU_DEPTH) {

      when(retire_ptr_real < flush_l_ptr) {
        is_old_instr := (j > retire_ptr_real) && (j < flush_l_ptr)
      } .elsewhen(retire_ptr_real > flush_l_ptr) {
        is_old_instr := (j > retire_ptr_real) || (j < flush_l_ptr)
      }

      when(reg_rename.io.ckptOut.valid && reg_rename.io.reg_in(i).isBranch && (j===ckpt_trans_id)) {
        rob_token(i)(j) := reg_rename.io.ckptOut // todo 后续考虑 i->rob_issue_id（考虑同issue之间的flush）
      }
      when(io.bju_mis_flush && (j===rollback_trans_id)) {
        reg_rename.io.rollback := rob_token(i)(j) // todo 后续考虑 i->rob_issue_id（考虑同issue之间的flush）
      }

      when(io.bju_mis_flush && !is_old_instr) {
        rob_token(i)(j).valid := False
      }

    }
  }


  issue_buff.io.push_ptr := push_ptr
  issue_buff.io.retire_ptr := retire_ptr
  issue_buff.io.flush_g_hit := io.in_flush_g
  issue_buff.io.flush_l_hit := io.in_flush_l
  issue_buff.io.flush_l_id := io.in_flush_l_id
  issue_buff.io.cmt_if := io.cmt_if
  issue_buff.io.ex_fwd_if := io.ex_fwd_if
  issue_buff.io.wb_fwd_if := io.wb_fwd_if
  issue_buff.io.fu_id_update := fu_id_update
  issue_buff.io.fu_id_update_valid := fu_id_update_valid
  issue_buff.io.issue_ptr_next_real := issue_ptr_next_real

  for (i <- 0 until cfg.issueWidth) {
    issue_buff.io.issue_ptr(i).valid := io.issue_out_if(i).valid
    issue_buff.io.issue_ptr(i).payload := issue_ptr(i).payload
  }
  // todo: 根据模块输入信息rename+READOP+安排好FUID，--> issue_buff
  // todo: SCB根据Hazard判断是否issue，issue_buff --> EXE
  // 寄存器的forwarding自己会写到reg_wb

  for (i <- 0 until cfg.issueWidth) {
    for (j <- 0 until cfg.SCB_IU_DEPTH) {
      issue_buff.io.issue_in_if(i)(j).assignFromBits(B(0,issue_buff.io.issue_in_if(i)(j).getBitsWidth bits))
    }
  }

  for (i <- 0 until cfg.issueWidth) {
    issue_buff.io.issue_in_if(i)(push_ptr_real).dec_valid := io.dec_if(i).dec_valid
    issue_buff.io.issue_in_if(i)(push_ptr_real).alu_sel := io.dec_if(i).alu_sel
    issue_buff.io.issue_in_if(i)(push_ptr_real).imm := io.dec_if(i).imm
    issue_buff.io.issue_in_if(i)(push_ptr_real).valid := io.pc_fl(i).valid
    issue_buff.io.issue_in_if(i)(push_ptr_real).pc := io.pc_fl(i).payload
    issue_buff.io.issue_in_if(i)(push_ptr_real).instr := io.instr_fl(i).payload
    if(i==0){
      issue_buff.io.issue_in_if(i)(push_ptr_real).predict_info := io.predict_if
    } else {
      issue_buff.io.issue_in_if(i)(push_ptr_real).predict_info.assignFromBits(B(0, io.predict_if.asBits.getWidth bits))
    }
    issue_buff.io.issue_in_if(i)(push_ptr_real).csr_entry.reg_addr := io.dec_if(i).csr_entry.reg_addr
    issue_buff.io.issue_in_if(i)(push_ptr_real).csr_entry.reg_wten := io.dec_if(i).csr_entry.reg_wten
    issue_buff.io.issue_in_if(i)(push_ptr_real).csr_entry.reg_rden := io.dec_if(i).csr_entry.reg_rden
    issue_buff.io.issue_in_if(i)(push_ptr_real).csr_entry.reg_rdata := io.csr_if(i).reg_rdata
    issue_buff.io.issue_in_if(i)(push_ptr_real).csr_entry.reg_ready := True  // todo

    issue_buff.io.issue_in_if(i)(push_ptr_real).rs1_scb_entry.reg_addr_real := io.dec_if(i).rs1_entry.reg_addr
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs1_scb_entry.reg_addr_rename := reg_rename.io.reg_out(i).src1_p.resized
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs1_scb_entry.reg_rden := io.dec_if(i).rs1_entry.reg_rden
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs1_scb_entry.reg_wten := False
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs1_scb_entry.reg_rdata := io.rs1_if(i).reg_rdata
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs1_scb_entry.reg_ready := reg_rename.io.reg_out(i).src1_p_ready

    issue_buff.io.issue_in_if(i)(push_ptr_real).rs2_scb_entry.reg_addr_real := io.dec_if(i).rs2_entry.reg_addr
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs2_scb_entry.reg_addr_rename := reg_rename.io.reg_out(i).src2_p.resized
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs2_scb_entry.reg_rden := io.dec_if(i).rs2_entry.reg_rden
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs2_scb_entry.reg_wten := False
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs2_scb_entry.reg_rdata := io.rs2_if(i).reg_rdata
    issue_buff.io.issue_in_if(i)(push_ptr_real).rs2_scb_entry.reg_ready := reg_rename.io.reg_out(i).src2_p_ready

    issue_buff.io.issue_in_if(i)(push_ptr_real).rd_scb_entry.reg_addr_real := io.dec_if(i).rd_entry.reg_addr
    issue_buff.io.issue_in_if(i)(push_ptr_real).rd_scb_entry.reg_addr_rename := reg_rename.io.reg_out(i).dst_p
    issue_buff.io.issue_in_if(i)(push_ptr_real).rd_scb_entry.reg_rden := False
    issue_buff.io.issue_in_if(i)(push_ptr_real).rd_scb_entry.reg_wten := io.dec_if(i).rd_entry.reg_wten

    issue_buff.io.issue_in_if(i)(push_ptr_real).trans_id := push_ptr.payload
//    when(fu_id_update_valid(i) && (push_ptr_real ===issue_ptr_next_real(i))) {
//      issue_buff.io.issue_in_if(i)(push_ptr_real).fu_id := fu_id_update(i)(push_ptr_real).fu_id
//    } .otherwise {
//      issue_buff.io.issue_in_if(i)(push_ptr_real).fu_id := FU_ID.enums.NOP(0) // todo, dont care
//    }
    issue_buff.io.issue_in_if(i)(push_ptr_real).fu_id := FU_ID.enums.NOP(0) // todo, dont care
    issue_buff.io.issue_in_if(i)(push_ptr_real).op_type := io.dec_if(i).op_type

    issue_buff.io.issue_in_if(i)(push_ptr_real).instr_err := io.dec_if(i).instr_err
    issue_buff.io.issue_in_if(i)(push_ptr_real).issue_busy := False
  }

  for (i <- 0 until cfg.issueWidth) {
    io.csr_if(i).reg_addr := io.dec_if(i).csr_entry.reg_addr
    io.csr_if(i).reg_rden := io.dec_if(i).csr_entry.reg_rden

    io.rs1_if(i).reg_addr := reg_rename.io.reg_out(i).src1_p.resized
    io.rs1_if(i).reg_rden := io.dec_if(i).rs1_entry.reg_rden

    io.rs2_if(i).reg_addr := reg_rename.io.reg_out(i).src2_p.resized
    io.rs2_if(i).reg_rden := io.dec_if(i).rs2_entry.reg_rden
  }


  for (i <- 0 until cfg.issueWidth) {
    for (j <- 0 until cfg.SCB_IU_DEPTH) {
      raw_hazard(i)(j) := ~(issue_buff.io.issue_out_buff(i)(j).rs1_scb_entry.reg_ready && issue_buff.io.issue_out_buff(i)(j).rs2_scb_entry.reg_ready)
      fu_hazard(i)(j) := False
      lsu_hazard(i)(j) := False
      hazard(i)(j) := raw_hazard(i)(j) | fu_hazard(i)(j) | lsu_hazard(i)(j)
    }
  }

  val is_load_dbg = issue_buff.io.issue_out_buff(0)(4).op_type === OP_TYPE.OP_LOAD
  val load_addr_dbg = issue_buff.io.issue_out_buff(0)(4).rs1_scb_entry.reg_rdata + issue_buff.io.issue_out_buff(0)(4).imm.asUInt
  val is_store_dbg = issue_buff.io.issue_out_buff(0)(0).op_type === OP_TYPE.OP_STORE
  val store_addr_dbg = issue_buff.io.issue_out_buff(0)(0).rs1_scb_entry.reg_rdata + issue_buff.io.issue_out_buff(0)(0).imm.asUInt
  val un_issue_dbg = issue_buff.io.issue_out_buff(0)(0).valid & ~issue_buff.io.issue_out_buff(0)(0).issue_busy

  for (i <- 0 until cfg.issueWidth) {
      for (j <- 0 until cfg.SCB_IU_DEPTH) {
        val is_load = issue_buff.io.issue_out_buff(i)(j).op_type === OP_TYPE.OP_LOAD
        val load_addr = issue_buff.io.issue_out_buff(i)(j).rs1_scb_entry.reg_rdata + issue_buff.io.issue_out_buff(i)(j).imm.asUInt
        for (k <- 0 until cfg.SCB_IU_DEPTH) {
          if(k!=j) {
            for (p <- 0 until cfg.issueWidth) {
              val is_store = issue_buff.io.issue_out_buff(p)(k).op_type === OP_TYPE.OP_STORE
              val store_addr = issue_buff.io.issue_out_buff(p)(k).rs1_scb_entry.reg_rdata + issue_buff.io.issue_out_buff(p)(k).imm.asUInt
              val un_issue = issue_buff.io.issue_out_buff(p)(k).valid & ~issue_buff.io.issue_out_buff(p)(k).issue_busy
              val is_pre = Bool()
              if(j>k) {
                is_pre := (j < push_ptr_real) || (k > push_ptr_real)
              } else {
                is_pre := (j < push_ptr_real) && (k > push_ptr_real)
              }
              when(is_load && is_store && (load_addr === store_addr) && un_issue && is_pre) {
                lsu_hazard(i)(j) := True
              }
            }
          } else {
            for (p <- 0 until i) {
              val is_store = issue_buff.io.issue_out_buff(p)(k).op_type === OP_TYPE.OP_STORE
              val store_addr = issue_buff.io.issue_out_buff(p)(k).rs1_scb_entry.reg_rdata + issue_buff.io.issue_out_buff(p)(k).imm.asUInt
              val un_issue = issue_buff.io.issue_out_buff(p)(k).valid & ~issue_buff.io.issue_out_buff(p)(k).issue_busy
              when(is_load && is_store && (load_addr === store_addr) && un_issue) {
                lsu_hazard(i)(j) := True
              }
            }
          }
        }

      }


  }


  // issue out初始化，必须写在一堆when elsewhen之前
  for (i <- 0 until cfg.issueWidth) {
//    when(issue_ptr(i).valid) {
//      //io.issue_out_if(i) := issue_buff.io.issue_out_buff(i)(issue_ptr_next_real(i))
//      io.issue_out_if(i) := issue_buff.io.issue_out_buff(i)(issue_ptr_real(i))
//      fu_id_update_valid(i) := True
//    } .otherwise{
//      io.issue_out_if(i) := issue_buff.io.issue_out_buff(i)(issue_ptr_real(i))  // todo 需要斟酌
//      fu_id_update_valid(i) := False
//    }
    io.issue_out_if(i) := issue_buff.io.issue_out_buff(i)(issue_ptr_real(i))

    for (j <- 0 until cfg.SCB_IU_DEPTH) {
      fu_id_update_valid(i)(j) := False
    }
  }


  // todo : issue_ptr_next 下一个符合 no raw hazard
  for (i <- 0 until cfg.issueWidth) {
    //issue_next_ov(i) := (push_ptr.payload(cfg.SCB_INSTR_WIDTH) ^ issue_ptr(i).payload(cfg.SCB_INSTR_WIDTH)) && (push_ptr.payload(cfg.SCB_INSTR_WIDTH-1 downto 0) === issue_ptr(i).payload(cfg.SCB_INSTR_WIDTH-1 downto 0))
    val issue_seq = Vec.fill(cfg.SCB_IU_DEPTH)(Flow(UInt(cfg.SCB_ID_WIDTH-1 bits)))
    val issue_seq_hit = FFONE.first(issue_seq.map(_.valid))
    for(j <- 0 until cfg.SCB_IU_DEPTH) {
      when(j =/= issue_ptr_real(i)) {
      // 支持乱序issue
        issue_seq(j).valid := issue_buff.io.issue_out_buff(i)(j).valid && ~issue_buff.io.issue_out_buff(i)(j).issue_busy && ~(raw_hazard(i)(j) | lsu_hazard(i)(j))
        issue_seq(j).payload := j
      } .otherwise {
        issue_seq(j).valid := False
        issue_seq(j).payload := j
      }
    }
    when(issue_seq_hit.has){
      val issue_ptr_msb = UInt(1 bits)
      when(issue_seq_hit.idx > issue_ptr_real(i)) {
        issue_ptr_msb := issue_ptr(i).payload(cfg.SCB_ID_WIDTH-1).asUInt
      } .otherwise {
        issue_ptr_msb := ~issue_ptr(i).payload(cfg.SCB_ID_WIDTH-1).asUInt
      }
      issue_ptr_next(i) := issue_ptr_msb@@issue_seq_hit.idx
    } .otherwise{
      issue_ptr_next(i) := issue_ptr(i).payload
    }
  }


  // fu hazard
  val alu_w = log2Up(io.alu_busy.length + 1)
  val alu_free_cnt: UInt = io.alu_busy.map(b => (!b).asUInt.resize(alu_w)).reduce(_ + _)
  val mul_w = log2Up(io.mul_busy.length + 1)
  val mul_free_cnt: UInt = io.mul_busy.map(b => (!b).asUInt.resize(mul_w)).reduce(_ + _)
  val div_w = log2Up(io.div_busy.length + 1)
  val div_free_cnt: UInt = io.div_busy.map(b => (!b).asUInt.resize(div_w)).reduce(_ + _)
  val bju_w = log2Up(io.bju_busy.length + 1)
  val bju_free_cnt: UInt = io.bju_busy.map(b => (!b).asUInt.resize(bju_w)).reduce(_ + _)
  val lsu_w = log2Up(io.lsu_busy.length + 1)
  val lsu_free_cnt: UInt = io.lsu_busy.map(b => (!b).asUInt.resize(lsu_w)).reduce(_ + _)
  val csr_w = log2Up(io.csr_busy.length + 1)
  val csr_free_cnt: UInt = io.csr_busy.map(b => (!b).asUInt.resize(csr_w)).reduce(_ + _)
  val fpu_w = log2Up(io.fpu_busy.length + 1)
  val fpu_free_cnt: UInt = io.fpu_busy.map(b => (!b).asUInt.resize(fpu_w)).reduce(_ + _)

  val fu_id = FU_ID.enums
  var alu_n = 0
  var mul_n = 0
  var div_n = 0
  var bju_n = 0
  var lsu_n = 0
  var csr_n = 0
  var fpu_n = 0
  var nop_n = 0

  val load_issue = Vec.fill(FU_ID.enums.LSU.size)(Bool())
  val load_issue_d1,load_issue_d2 = Vec.fill(FU_ID.enums.LSU.size)(Reg(Bool()) init(False))
  val load_free = Vec.fill(FU_ID.enums.LSU.size)(Bool())
  for (i <- 0 until FU_ID.enums.LSU.size) {
    load_issue(i) := False
  }
  for (i <- 0 until FU_ID.enums.LSU.size) {
    load_issue_d1(i) := load_issue(i)
    load_issue_d2(i) := load_issue_d1(i)
    load_free(i) := ~(load_issue_d1(i) && !load_issue_d2(i))
  }
  for (i <- 0 until cfg.issueWidth) {
    for (j <- 0 until cfg.SCB_IU_DEPTH) {
      fu_id_update(i)(j).fu_id := fu_id.NOP(nop_n)
    }
  }

  for (i <- 0 until cfg.issueWidth) {
    //val alu_sel = issue_buff.io.issue_out_buff(i)(issue_ptr_next_real(i)).alu_sel
    //val op_type = issue_buff.io.issue_out_buff(i)(issue_ptr_next_real(i)).op_type
    val alu_sel = ALU_UNIT_SEL()
    val op_type = OP_TYPE()
    when(push_ptr_real === issue_ptr_next_real(i)){
      alu_sel := io.dec_if(i).alu_sel
      op_type := io.dec_if(i).op_type
    } .otherwise{
      alu_sel := issue_buff.io.issue_out_buff(i)(issue_ptr_next_real(i)).alu_sel
      op_type := issue_buff.io.issue_out_buff(i)(issue_ptr_next_real(i)).op_type
    }
      //todo : 同一拍issue的FU占用优先级
      when(alu_sel === ALU_UNIT_SEL.ALU) {
        when(io.alu_busy.asBits.andR) {
          fu_hazard(i)(issue_ptr_next_real(i)) := True
        }.elsewhen(alu_n < alu_free_cnt) {
          fu_hazard(i)(issue_ptr_next_real(i)) := False
          //io.issue_out_if(i).fu_id := fu_id.ALU(alu_n)
          fu_id_update(i)(issue_ptr_next_real(i)).fu_id := fu_id.ALU(alu_n)
          fu_id_update_valid(i)(issue_ptr_next_real(i)) := True
          alu_n = alu_n + 1
        }
      }.elsewhen(alu_sel === ALU_UNIT_SEL.MUL) {
        when(io.mul_busy.asBits.andR) {
          fu_hazard(i)(issue_ptr_next_real(i)) := True
        }.elsewhen(mul_n < mul_free_cnt) {
          fu_hazard(i)(issue_ptr_next_real(i)) := False
          //io.issue_out_if(i).fu_id := fu_id.MUL(mul_n)
          fu_id_update(i)(issue_ptr_next_real(i)).fu_id := fu_id.MUL(mul_n)
          fu_id_update_valid(i)(issue_ptr_next_real(i)) := True
          mul_n = mul_n + 1
        }
      }.elsewhen(alu_sel === ALU_UNIT_SEL.DIV) {
        when(io.div_busy.asBits.andR) {
          fu_hazard(i)(issue_ptr_next_real(i)) := True
        }.elsewhen(div_n < div_free_cnt) {
          fu_hazard(i)(issue_ptr_next_real(i)) := False
          //io.issue_out_if(i).fu_id := fu_id.DIV(div_n)
          fu_id_update(i)(issue_ptr_next_real(i)).fu_id := fu_id.DIV(div_n)
          fu_id_update_valid(i)(issue_ptr_next_real(i)) := True
          div_n = div_n + 1
        }
      }.elsewhen(alu_sel === ALU_UNIT_SEL.BJU) {
        when(io.bju_busy.asBits.andR) {
          fu_hazard(i)(issue_ptr_next_real(i)) := True
        }.elsewhen(bju_n < bju_free_cnt) {
          fu_hazard(i)(issue_ptr_next_real(i)) := False
          //io.issue_out_if(i).fu_id := fu_id.BJU(bju_n)
          fu_id_update(i)(issue_ptr_next_real(i)).fu_id := fu_id.BJU(bju_n)
          fu_id_update_valid(i)(issue_ptr_next_real(i)) := True
          bju_n = bju_n + 1
        }
      }.elsewhen(alu_sel === ALU_UNIT_SEL.LSU) {
        when(io.lsu_busy.asBits.andR) {
          fu_hazard(i)(issue_ptr_next_real(i)) := True
        } .elsewhen(op_type === OP_TYPE.OP_STORE && io.store_busy) {
          fu_hazard(i)(issue_ptr_next_real(i)) := True
        } .elsewhen(lsu_n < lsu_free_cnt){
          fu_hazard(i)(issue_ptr_next_real(i)) := False
          //io.issue_out_if(i).fu_id := fu_id.LSU(lsu_n)
          fu_id_update(i)(issue_ptr_next_real(i)).fu_id := fu_id.LSU(lsu_n) // todo: 对于其他可能busy的ALU都需要加fu_id_update
          fu_id_update_valid(i)(issue_ptr_next_real(i)) := True
          load_issue(lsu_n) := (op_type === OP_TYPE.OP_LOAD)
          lsu_n = lsu_n + 1
        }
        /*
        when(op_type === OP_TYPE.OP_LOAD) { // LOAD的busy需要分开处理
          when(io.lsu_busy.asBits.andR) {
            fu_hazard(i)(issue_ptr_next_real(i)) := True
          }.elsewhen(lsu_n < lsu_free_cnt) {
            fu_hazard(i)(issue_ptr_next_real(i)) := False
            io.issue_out_if(i).fu_id := fu_id.LSU(lsu_n)
            lsu_n = lsu_n + 1
          }
        }.elsewhen(op_type === OP_TYPE.OP_STORE) {
          when(io.store_busy) {
            fu_hazard(i)(issue_ptr_next_real(i)) := True
          } .otherwise {
            when(io.lsu_busy.asBits.andR) {
              fu_hazard(i)(issue_ptr_next_real(i)) := True
            }.elsewhen(lsu_n < lsu_free_cnt) {
              fu_hazard(i)(issue_ptr_next_real(i)) := False
              io.issue_out_if(i).fu_id := fu_id.LSU(lsu_n)
              lsu_n = lsu_n + 1
            }
          }
        }*/
      }.elsewhen(alu_sel === ALU_UNIT_SEL.CSR) {
        when(io.csr_busy.asBits.andR) {
          fu_hazard(i)(issue_ptr_next_real(i)) := True
        }.elsewhen(csr_n < csr_free_cnt) {
          fu_hazard(i)(issue_ptr_next_real(i)) := False
          //io.issue_out_if(i).fu_id := fu_id.CSR(csr_n)
          fu_id_update(i)(issue_ptr_next_real(i)).fu_id := fu_id.CSR(csr_n)
          fu_id_update_valid(i)(issue_ptr_next_real(i)) := True
          csr_n = csr_n + 1
        }
      }.elsewhen(alu_sel === ALU_UNIT_SEL.FPU) {
        when(io.fpu_busy.asBits.andR) {
          fu_hazard(i)(issue_ptr_next_real(i)) := True
        }.elsewhen(fpu_n < fpu_free_cnt) {
          fu_hazard(i)(issue_ptr_next_real(i)) := False
          //io.issue_out_if(i).fu_id := fu_id.FPU(fpu_n)
          fu_id_update(i)(issue_ptr_next_real(i)).fu_id := fu_id.FPU(fpu_n)
          fu_id_update_valid(i)(issue_ptr_next_real(i)) := True
          fpu_n = fpu_n + 1
        }
      }.elsewhen(alu_sel === ALU_UNIT_SEL.NOP) {
        when(io.nop_busy.asBits.andR) {
          fu_hazard(i)(issue_ptr_next_real(i)) := True
        }.otherwise {
          fu_hazard(i)(issue_ptr_next_real(i)) := False
          //io.issue_out_if(i).fu_id := fu_id.NOP(nop_n)
          fu_id_update(i)(issue_ptr_next_real(i)).fu_id := fu_id.NOP(nop_n)
          fu_id_update_valid(i)(issue_ptr_next_real(i)) := True
        }
      }


  }




  val hazard_d1 = Vec.fill(cfg.issueWidth)(Vec.fill(cfg.SCB_IU_DEPTH)(Reg(Bool) init(False)))
  for (i <- 0 until cfg.issueWidth) {
    for (j <- 0 until cfg.SCB_IU_DEPTH) {
      when(issue_ptr(i).valid){
        hazard_d1(i)(j) := hazard(i)(j)
      }
    }
  }
  // issue out处理
  /*
  for (i <- 0 until cfg.issueWidth) {
    when(hazard.asBits.andR){
      io.issue_out_if(i).valid := False
    } .elsewhen(hazard.asBits.orR){
      io.issue_out_if(i).valid := (~B(hazard.take(i)).orR) && issue_buff.io.issue_out_buff(i)(issue_ptr_next_real(i)).valid
      //io.issue_out_if(i).valid := (~B(hazard_d1.take(i)).orR) && issue_buff.io.issue_out_buff(i)(issue_ptr_real(i)).valid

    }
    // todo 其他out，包括考虑fwd的src数据MUX

  }

   */


}
