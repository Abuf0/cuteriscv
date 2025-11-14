package mainelib

import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._

/** 暂时支支持xlen=32 **/
case class lsu_unit(cfg : CoreConfig, id: Int, latency: Int = 1) extends Component {
  val io = new Bundle {
    val ex_in_entry = Vec.fill(cfg.issueWidth)(slave(issue_entry(cfg)))  // from issue, broadcast
    val ex_out_entry = master(exe_res_entry(cfg))  // to commit
    val unit_busy = out Bool()
    val store_if = master(store_entry(cfg))
    val load_if = master(load_entry(cfg))

    // connect with mmu
    val mem_read_interface = master(mem_read_if(cfg))

    // connect with store_buffer
    val toload_need = out Bool()
    val toload_id = out UInt(cfg.SCB_ID_WIDTH bits)
    val toload_addr = out UInt(cfg.DataAddrBus bits)
    val toload_hit = in UInt(cfg.DataSelBus bits)
    val toload_data = in UInt(cfg.DataBus bits)
    val toload_vld = in Bool()

    val load_vld = out Bool()

    val flush_mispredict_in = slave(flush_mispredict_entry(cfg))

  }

  val issue_id = UInt(log2Up(cfg.issueWidth) bits)
  val unit_hit = Bool()
  val unit_hit_d1 = RegNext(unit_hit) init(False)
  val issue_id_lat = RegNextWhen(issue_id,unit_hit)
  val issue_id_hold = UInt(log2Up(cfg.issueWidth) bits)
  val ex_entry_latch = RegNextWhen(io.ex_in_entry(issue_id),unit_hit)
  val ex_entry = issue_entry(cfg)
  when(unit_hit) {
    ex_entry := io.ex_in_entry(issue_id)
    issue_id_hold := issue_id
  } .otherwise{
    ex_entry := ex_entry_latch
    issue_id_hold := issue_id_lat
  }

  val out_trans_id = /*ex_entry.trans_id*/  io.ex_in_entry(issue_id).trans_id
  val in_trans_id = io.flush_mispredict_in.trans_id
  val after_mispredict = io.flush_mispredict_in.hit && ((in_trans_id(cfg.SCB_ID_WIDTH - 1) === out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) <= out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0))) || (in_trans_id(cfg.SCB_ID_WIDTH - 1) =/= out_trans_id(cfg.SCB_ID_WIDTH - 1) && in_trans_id(cfg.SCB_ID_WIDTH - 2 downto (0)) > out_trans_id(cfg.SCB_ID_WIDTH-2 downto(0)) ))

  //val load_raddr = Reg(UInt(DataAddrBus bits)) init(0)
  //val load_rden = Reg(Bool()) init(False)
  //val load_byte = Reg(Bits(4 bits)) init(B"1111")
  //val load_sign = Reg(Bool()) init(False)
  // todo Reg, otherwise need multicycle
  val load_raddr = UInt(cfg.DataAddrBus bits)
  val load_raddr_align = UInt(cfg.DataAddrBus bits)
  val load_rden  = Bool()
  val load_byte  = Bits(cfg.DataSelBus bits)  // 4bit: rv32
  val load_sign  = Bool()
  val load_rden_real = Bool()
  val load_raddr_offset = Reg(UInt(2 bits)) init(0)

  load_raddr := 0
  load_raddr_align := 0
  load_rden  := False
  load_byte  := ~B(0, cfg.DataSelBus bits)  // 全1
  load_sign  := True

  load_raddr_offset := load_raddr(1 downto 0)

  val load_hit_mask = Reg(Bool())  init(False)
//  when(unit_hit) {
//    load_hit_mask := False
//  } .elsewhen(io.toload_hit.andR === True){
//    load_hit_mask := True
//  } .otherwise{
//    load_hit_mask := False
//  }

  issue_id := 0
  unit_hit := False

  val unit_on_tmp = Reg(Bool()) init(False)
  val unit_on = unit_on_tmp || unit_hit
  io.unit_busy := (unit_hit && !io.ex_out_entry.result_vld)//~unit_free  // todo

  when(unit_hit) {
    unit_on_tmp := True
  } .elsewhen(io.ex_out_entry.result_vld) {
    unit_on_tmp := False
  }


  io.ex_out_entry.issue_id := issue_id

  for (i <- 0 until cfg.issueWidth) {
    when(io.ex_in_entry(i).fu_id.asBits.asUInt === id){
      issue_id := i
      unit_hit := io.ex_in_entry(i).valid & !after_mispredict
    }
  }

  load_rden_real := load_rden && io.toload_vld && ((io.toload_hit & U(load_byte)) =/= U(load_byte))

  //val load_result = Reg(UInt(cfg.DataBus bits)) init(0) // todo with timing
  val load_result = UInt(cfg.DataBus bits)
  load_result := 0
  val store_waddr = UInt(cfg.DataAddrBus bits)
  store_waddr := 0
//  val store_waddr_align = Reg(UInt(cfg.DataAddrBus bits)) init(0)
//  val store_wten = Reg(Bool()) init(False)
//  val store_byte = Reg(Bits(cfg.DataSelBus bits)) init(~B(0, cfg.DataSelBus bits))
//  val store_wdata = Reg(UInt(cfg.DataBus bits)) init(0)
  val store_waddr_align = UInt(cfg.DataAddrBus bits)
  val store_wten = Bool()
  val store_byte = Bits(cfg.DataSelBus bits)
  val store_wdata = UInt(cfg.DataBus bits)
  io.store_if.store_wb_trans_id := ex_entry.trans_id  //io.ex_in_entry(issue_id).trans_id
  store_waddr_align := 0
  store_wten := False
  store_byte := 0
  store_wdata := 0



  val rs1_data = ex_entry.rs1_scb_entry.reg_rdata //io.ex_in_entry(issue_id).rs1_scb_entry.reg_rdata
  val rs2_data = ex_entry.rs2_scb_entry.reg_rdata //io.ex_in_entry(issue_id).rs2_scb_entry.reg_rdata
  val imm = ex_entry.imm  //io.ex_in_entry(issue_id).imm

  /*
  load_raddr := 0
  load_rden := False
  load_byte := B"1111"
  load_sign := False
  store_waddr := 0
  store_wten := False
  store_byte := B"1111"
  store_wdata := 0
  load_result := 0
  */


  io.store_if.store_wb_en := store_wten
  io.store_if.store_wb_addr := store_waddr_align
  io.store_if.store_wb_data := store_wdata
  io.store_if.store_wb_byte := store_byte

  val result_rd_vld, result_wt_vld = Bool()
  result_rd_vld := ((load_rden_real & io.mem_read_interface.rvalid) || (io.toload_hit & U(load_byte)) === U(load_byte))
  result_wt_vld := store_wten
  io.load_vld := result_rd_vld

  io.ex_out_entry.instr := ex_entry.instr//io.ex_in_entry(issue_id).instr
  io.ex_out_entry.trans_id := ex_entry.trans_id//io.ex_in_entry(issue_id).trans_id
  io.ex_out_entry.pc := ex_entry.pc//io.ex_in_entry(issue_id).pc
  io.ex_out_entry.result := load_result.resized.asSInt
  io.ex_out_entry.result_vld := result_rd_vld || result_wt_vld


  io.ex_out_entry.reg_wif.preg_addr := ex_entry.rd_scb_entry.reg_addr_rename  //io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_rename
  io.ex_out_entry.reg_wif.areg_addr := ex_entry.rd_scb_entry.reg_addr_real  //io.ex_in_entry(issue_id).rd_scb_entry.reg_addr_real
  io.ex_out_entry.reg_wif.reg_wten := ex_entry.rd_scb_entry.reg_wten  //io.ex_in_entry(issue_id).rd_scb_entry.reg_wten
  io.ex_out_entry.reg_wif.reg_wdata := io.ex_out_entry.result.asUInt


  io.load_if.load_rd_en := load_rden_real   // to dcache
  io.load_if.load_rd_addr := load_raddr_align  // to dcache
  io.load_if.load_rd_byte := load_byte

  io.mem_read_interface.re := load_rden_real
  io.mem_read_interface.raddr := load_raddr //load_raddr(cfg.DataAddrBus-1 downto cfg.DMemOffset).resized//load_raddr_align
  io.mem_read_interface.sel := load_byte.rotateLeft(load_raddr(cfg.DMemOffset-1 downto 0))
  val rdata_rotate = UInt(log2Up(cfg.DataBus) bits)
  rdata_rotate := (load_raddr(cfg.DMemOffset-1 downto 0) * cfg.ByteWidth).resized
  val dcache_rdata = io.mem_read_interface.rdata.rotateRight(rdata_rotate).resized

  val dcache_rdata_real = UInt(cfg.DataBus bits)

  //io.toload_addr := load_raddr_align
  io.toload_need := load_rden
  io.toload_addr := load_raddr_align  // fix sfind
  io.toload_id := ex_entry.trans_id //io.ex_in_entry(issue_id).trans_id
  /*
  when(io.toload_hit){ // 如果wb buffer中有待提交的SW指令，且写地址==读地址，则forwarding（类似于store buffer）
    dcache_rdata_real := io.toload_data
  } .otherwise{
    dcache_rdata_real := dcache_rdata
  }
   */
  
  val dcache_rdata_real_0 = UInt(8 bits)
  val dcache_rdata_real_1 = UInt(8 bits)
  val dcache_rdata_real_2 = UInt(8 bits)
  val dcache_rdata_real_3 = UInt(8 bits)

  //val toload_hit = RegNext(io.toload_hit, U"4'b0")
  //val toload_data = RegNext(io.toload_data, U"32'b0")
  val toload_hit = io.toload_hit
  val toload_data = io.toload_data

  when(toload_hit(0)){
    dcache_rdata_real_0 := toload_data(7 downto 0)
  } .otherwise{
    dcache_rdata_real_0 := dcache_rdata(7 downto 0)
  }
  when(toload_hit(1)){
    dcache_rdata_real_1 := toload_data(15 downto 8)
  } .otherwise{
    dcache_rdata_real_1 := dcache_rdata(15 downto 8)
  }
  when(toload_hit(2)){
    dcache_rdata_real_2 := toload_data(23 downto 16)
  } .otherwise{
    dcache_rdata_real_2 := dcache_rdata(23 downto 16)
  }
  when(toload_hit(3)){
    dcache_rdata_real_3 := toload_data(31 downto 24)
  } .otherwise{
    dcache_rdata_real_3 := dcache_rdata(31 downto 24)
  }

  dcache_rdata_real := dcache_rdata_real_3 @@ dcache_rdata_real_2 @@ dcache_rdata_real_1 @@ dcache_rdata_real_0


  when(/*unit_hit*/unit_on) {
    load_raddr := rs1_data + U(imm)
    load_raddr_align := load_raddr(cfg.DataAddrBus - 1 downto 2) @@ U"2'b00" // todo:也可以不做，因为cache里会丢弃最后2bit
    load_rden := True

    switch(/*io.ex_in_entry(issue_id).instr*/ex_entry.instr) { // latch
      is(LB) {
        switch(load_raddr_offset(1 downto 0)) {
          is(U"00") {
            load_byte := B"0001"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(7), (7 downto 0) -> dcache_rdata_real(7 downto 0))
          }
          is(U"01") {
            load_byte := B"0010"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(15), (7 downto 0) -> dcache_rdata_real(15 downto 8))
          }
          is(U"10") {
            load_byte := B"0100"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(23), (7 downto 0) -> dcache_rdata_real(23 downto 16))
          }
          is(U"11") {
            load_byte := B"1000"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(31), (7 downto 0) -> dcache_rdata_real(31 downto 24))
          }
        }
      }
      is(LH) { // todo 跨地址问题，按理编译器不会支持的
        switch(load_raddr_offset(1 downto 0)) {
          is(U"00") {
            load_byte := B"0011"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(15), (15 downto 0) -> dcache_rdata_real(15 downto 0))
          }
          /*
        is(U"01") { // todo 跨地址了
          load_byte := B"0110"
          load_sign := True
          load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(23), (15 downto 0) -> dcache_rdata_real(23 downto 8))
        }
         */
          is(U"10") {
            load_byte := B"1100"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(31), (15 downto 0) -> dcache_rdata_real(31 downto 16))
          }
          /*
        is(U"11") { // todo
          load_byte := B"1000"
          load_sign := True
          load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(31), (7 downto 0) -> dcache_rdata_real(31 downto 24))
        }
        */
        }
      }
      is(LW) {
        load_byte := B"1111"
        load_sign := True
        load_result := dcache_rdata_real
        store_wten := False
      }
      is(LBU) {
        switch(load_raddr_offset(1 downto 0)) {
          is(U"00") {
            load_byte := B"0001"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> False, (7 downto 0) -> dcache_rdata_real(7 downto 0))
          }
          is(U"01") {
            load_byte := B"0010"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> False, (7 downto 0) -> dcache_rdata_real(15 downto 8))
          }
          is(U"10") {
            load_byte := B"0100"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> False, (7 downto 0) -> dcache_rdata_real(23 downto 16))
          }
          is(U"11") {
            load_byte := B"1000"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> False, (7 downto 0) -> dcache_rdata_real(31 downto 24))
          }
        }
      }
      is(LHU) {
        switch(load_raddr_offset(1 downto 0)) {
          is(U"00") {
            load_byte := B"0011"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> False, (15 downto 0) -> dcache_rdata_real(15 downto 0))
          }
          /*
        is(U"01") { // todo 跨地址了
          load_byte := B"0110"
          load_sign := True
          load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(23), (15 downto 0) -> dcache_rdata_real(23 downto 8))
        }
         */
          is(U"10") {
            load_byte := B"1100"
            load_sign := True
            load_result := U(cfg.DataBus bits, default -> False, (15 downto 0) -> dcache_rdata_real(31 downto 16))
          }
          /*
        is(U"11") { // todo
          load_byte := B"1000"
          load_sign := True
          load_result := U(cfg.DataBus bits, default -> dcache_rdata_real(31), (7 downto 0) -> dcache_rdata_real(31 downto 24))
        }
        */
        }
      }
    }
  }

  when(/*unit_hit*/unit_on){
    switch(/*io.ex_in_entry(issue_id).instr*/ex_entry.instr){
      is(SB){
        store_waddr := rs1_data+U(imm)
        store_waddr_align := store_waddr(cfg.DataAddrBus-1 downto 2)@@U"2'b00"
        store_wten := True
        switch(store_waddr(1 downto 0)) {
          is(U"00") {
            store_byte := B"0001"
            store_wdata := U(cfg.DataBus bits, default -> False, (7 downto 0) -> rs2_data(7 downto 0))
          }
          is(U"01") {
            store_byte := B"0010"
            store_wdata := U(cfg.DataBus bits, default -> False, (15 downto 8) -> rs2_data(7 downto 0))
          }
          is(U"10") {
            store_byte := B"0100"
            store_wdata := U(cfg.DataBus bits, default -> False, (23 downto 16) -> rs2_data(7 downto 0))
          }
          is(U"11") {
            store_byte := B"1000"
            store_wdata := U(cfg.DataBus bits, default -> False, (31 downto 24) -> rs2_data(7 downto 0))
          }
        }
        load_rden := False
      }
      is(SH){
        store_waddr := rs1_data+U(imm)
        store_waddr_align := store_waddr(cfg.DataAddrBus-1 downto 2)@@U"2'b00"
        store_wten := True
        switch(store_waddr(1 downto 0)) { // todo:同样跨边界问题
          is(U"00") {
            store_byte := B"0011"
            store_wdata := U(cfg.DataBus bits, default -> False, (15 downto 0) -> rs2_data(15 downto 0))
          }
          /*
          is(U"01") {
            store_byte := B"0010"
            store_wdata := U(cfg.DataBus bits, default -> 0, (15 downto 8) -> rs2_data(7 downto 0))
          }
           */
          is(U"10") {
            store_byte := B"1100"
            store_wdata := U(cfg.DataBus bits, default -> False, (31 downto 16) -> rs2_data(15 downto 0))
          }
          /*
          is(U"11") {
            store_byte := B"1000"
            store_wdata := U(cfg.DataBus bits, default -> 0, (31 downto 24) -> rs2_data(7 downto 0))
          }
           */
        }
        load_rden := False
      }
      is(SW){
        store_waddr := rs1_data+U(imm)
        store_waddr_align := store_waddr(cfg.DataAddrBus-1 downto 2)@@U"2'b00"
        store_wten := True
        store_byte := B"1111"
        store_wdata := rs2_data
        load_rden := False
      }
      default{
        store_wten := False
        /*
        load_raddr := 0
        load_rden := False
        load_byte := B"1111"
        load_sign := False
        store_waddr := 0
        store_wten := False
        store_byte := 4
        store_wdata := 0
         */
      }
    }
  } .otherwise{
    store_wten := False
    /*
    load_raddr := 0
    load_rden := False
    load_byte := B"1111"
    load_sign := False
    store_waddr := 0
    store_wten := False
    store_byte := 4
    store_wdata := 0
     */
  }


}
