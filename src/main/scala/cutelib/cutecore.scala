package cutelib
import spinal.core._
import spinal.lib._   // IMasterSlave
import RISCV_ISA._
import BundleImplicit._   // connect with master & slave
import scoreboard._
import pc_gen._
import decorder._
import wb._
import commit._
import alu_unit._
import bju_unit._
import lsu_unit._
import csr_unit._
import nop_unit._


/*
class pc_gen extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val mtvec = in UInt (InstAddrBus bits) // CSR设定初始pc地址 <--csr regfile
    val csr_epc1 = in UInt (InstAddrBus bits) // CSR中exception target地址？？ <--csr regfile
    val csr_epc2 = in UInt (InstAddrBus bits) // CSR中exception target地址？？ <--csr regfile
    val epc = in UInt (InstAddrBus bits) // commit时exception target地址 <--commit
    val flush = out Bool()
    val instr_realign = slave(instr_entry(CoreConfig())) // ICache instr-->realign-->
    val pc_valid = out Bool()
    //val bht_taken = in Bool() // BHT预测是否进入分支 <--BHT
    val predict_bht_entry = slave(bht_predict_entry(CoreConfig()))  // from bht
    val predict_btb_entry = slave(btb_predict_entry(CoreConfig()))  // from btb
    //val btb_target = in UInt (InstAddrBus bits) // BTB预测的target地址 <--BTB
    val ras_target = in UInt (InstAddrBus bits) // call返回地址 <--RAS
    val pc = out UInt (InstAddrBus bits) // 当前pc --> ICache等
    val if_branch_predict = master(branch_predict_entry(CoreConfig())) // to instr_queue
    val ex_branch_predict = slave(branch_mispredict_entry(CoreConfig())) // from ex stage
    val bju_branch_predict = slave(branch_predict_entry(CoreConfig())) // from ex stage
    val resolved_bht_entry = master(bht_predict_entry(CoreConfig())) // ex stage to bht
    val resolved_btb_entry = master(btb_predict_entry(CoreConfig())) // ex stage to btb
    val mispredict_entry = master(branch_mispredict_entry(CoreConfig())) // ex stage to ras
  }
  val pc_r = Reg(UInt(InstAddrBus bits)) init (io.mtvec)

  val is_jump = Bool()
  val jump_target = UInt(InstAddrBus bits)
  //val flush_r = Reg(Bool()) init(False)
  val flush_r = Bool()
  flush_r := io.ex_branch_predict.branch_cor || io.ex_branch_predict.ret_cor || io.ex_branch_predict.call_cor
  io.flush := flush_r

  io.mispredict_entry := io.ex_branch_predict

  is_jump := False

  when(io.ex_branch_predict.branch_cor){
    io.resolved_bht_entry.bht_valid := io.bju_branch_predict.branch_valid
    io.resolved_bht_entry.bht_taken := io.bju_branch_predict.branch_taken
    io.resolved_bht_entry.pc := io.bju_branch_predict.branch_target
    io.resolved_btb_entry.btb_valid := io.bju_branch_predict.branch_valid
    io.resolved_btb_entry.btb_taken := io.bju_branch_predict.branch_taken
    io.resolved_btb_entry.btb_target := io.bju_branch_predict.branch_target
    io.resolved_btb_entry.pc := io.bju_branch_predict.branch_target
  } .otherwise {
    io.resolved_bht_entry.bht_valid := False
    io.resolved_bht_entry.bht_taken := False
    io.resolved_bht_entry.pc := 0
    io.resolved_btb_entry.btb_valid := False
    io.resolved_btb_entry.btb_taken := False
    io.resolved_btb_entry.btb_target := 0
    io.resolved_btb_entry.pc := 0
  }

  when(flush_r === True) { // todo wrong
    pc_r := io.ex_branch_predict.target_pc
  }.elsewhen(io.instr_realign.valid === True) { // 包含了stall_push
    when(is_jump === True) {
      pc_r := jump_target
    }.elsewhen(io.predict_bht_entry.bht_valid === True && io.predict_bht_entry.bht_taken === True) {
      pc_r := io.predict_btb_entry.btb_target
    }.elsewhen(io.if_branch_predict.is_call === True) {
      pc_r := io.predict_btb_entry.btb_target
    }.elsewhen(io.if_branch_predict.is_ret === True) {
      pc_r := io.ras_target
    }.otherwise {
      pc_r := pc_r + InstLen
    }
} .otherwise{}

  // RSICV中没有显式的call ret
  when(io.instr_realign.valid===True){
    io.if_branch_predict.is_branch := io.instr_realign.inst(6 downto 0) === U"1100011" // branch
    io.if_branch_predict.is_call := io.instr_realign.inst(6 downto 4) === U"110" && io.instr_realign.inst(2 downto 0) === U"111" && io.instr_realign.inst(7) === True// JAL or JALR, rd=x1/x5
    io.if_branch_predict.is_ret := io.instr_realign.inst(6 downto 0) === U"1100111" && io.instr_realign.inst(7) === False && io.instr_realign.inst(19 downto 18) === U"00" && io.instr_realign.inst(16 downto 15) === U"01"   // JALR, rd=0, rs=x1/x5
    //is_jump := io.if_branch_predict.is_call // TODO
    when(io.instr_realign.inst(6 downto 0) === U"1101111"){  // JAL
      jump_target := pc_r + U(InstBus-20 bits,default -> io.instr_realign.inst(31)) @@ io.instr_realign.inst(19 downto 12)@@io.instr_realign.inst(20 downto 20)@@io.instr_realign.inst(30 downto 21)@@U"0"  // 符号位扩展
      is_jump := True
    } .elsewhen(io.instr_realign.inst(6 downto 0) === U"1100111"){ // JALR
      jump_target := pc_r + U(InstBus-12 bits,default -> io.instr_realign.inst(31)) @@ io.instr_realign.inst(31 downto 20) // 符号位扩展
      is_jump := True
    } .otherwise{
      jump_target := io.mtvec
    }
  } .otherwise {
    io.if_branch_predict.is_branch := False
    io.if_branch_predict.is_call := False
    io.if_branch_predict.is_ret := False
    is_jump := False
    jump_target := io.mtvec
  }

  io.pc := pc_r
  io.pc_valid := io.instr_realign.valid

  io.if_branch_predict.pc := io.pc
  io.if_branch_predict.branch_valid := io.if_branch_predict.is_branch || io.if_branch_predict.is_call
  io.if_branch_predict.branch_taken := io.predict_bht_entry.bht_taken
  io.if_branch_predict.branch_target := io.predict_btb_entry.btb_target
}
*/

class bht extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val pc = in UInt(InstAddrBus bits)  // from pc_gen
    val pc_valid = in Bool()  // from pc_gen
    val if_branch_predict = slave(branch_predict_entry(CoreConfig())) // from pc_gen
    val resolved_bht_entry = slave(bht_predict_entry(CoreConfig())) // from ex stage
    val predict_bht_entry = master(bht_predict_entry(CoreConfig())) // to pc_gen
  }

  val Strongly_not_taken = B"00"
  val Weakly_not_taken = B"01"
  val Weakly_taken = B"10"
  val Strongly_taken = B"11"
  val taken_fsm = Vec(Bits(2 bits),(1<<12)) // pc offset : 12bit

  for(i <- 0 until (1<<12)) {
    val fsm = Reg(Bits(2 bits)) init (Weakly_taken)
    switch(fsm){
      is(Strongly_taken){
        when(io.resolved_bht_entry.bht_valid === True && io.resolved_bht_entry.bht_taken === True && io.resolved_bht_entry.pc(11 downto 0) ===i){
          fsm := Strongly_taken
        } .elsewhen(io.resolved_bht_entry.bht_valid === True && io.resolved_bht_entry.bht_taken === False && io.resolved_bht_entry.pc(11 downto 0) ===i){
          fsm := Weakly_taken
        } .otherwise{}
      }
      is(Weakly_taken){
        when(io.resolved_bht_entry.bht_valid === True && io.resolved_bht_entry.bht_taken === True && io.resolved_bht_entry.pc(11 downto 0) ===i){
          fsm := Strongly_taken
        } .elsewhen(io.resolved_bht_entry.bht_valid === True && io.resolved_bht_entry.bht_taken === False && io.resolved_bht_entry.pc(11 downto 0) ===i){
          fsm := Weakly_not_taken
        } .otherwise{}
      }
      is(Weakly_not_taken){
        when(io.resolved_bht_entry.bht_valid === True && io.resolved_bht_entry.bht_taken === True && io.resolved_bht_entry.pc(11 downto 0) ===i){
          fsm := Weakly_taken
        } .elsewhen(io.resolved_bht_entry.bht_valid === True && io.resolved_bht_entry.bht_taken === False && io.resolved_bht_entry.pc(11 downto 0) ===i){
          fsm := Strongly_not_taken
        } .otherwise{}
      }
      is(Strongly_not_taken){
        when(io.resolved_bht_entry.bht_valid === True && io.resolved_bht_entry.bht_taken === True && io.resolved_bht_entry.pc(11 downto 0) ===i){
          fsm := Weakly_not_taken
        } .elsewhen(io.resolved_bht_entry.bht_valid === True && io.resolved_bht_entry.bht_taken === False && io.resolved_bht_entry.pc(11 downto 0) ===i){
          fsm := Strongly_not_taken
        } .otherwise{}
      }
      default{
        fsm := Weakly_taken
      }
    }
    taken_fsm(i) := B(fsm)
  }

  when(io.pc_valid === True && (io.if_branch_predict.is_branch === True || io.if_branch_predict.is_call === True)){
    io.predict_bht_entry.pc := io.if_branch_predict.pc
    io.predict_bht_entry.bht_taken := taken_fsm(io.if_branch_predict.pc(11 downto 0)) === Weakly_taken || taken_fsm(io.if_branch_predict.pc(11 downto 0)) === Strongly_taken
    io.predict_bht_entry.bht_valid := True
  } .otherwise{
    io.predict_bht_entry.pc := io.if_branch_predict.pc
    io.predict_bht_entry.bht_taken := False
    io.predict_bht_entry.bht_valid := False
  }

}

class btb extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val if_branch_predict = slave(branch_predict_entry(CoreConfig())) // from pc_gen
    val pc = in UInt (InstAddrBus bits) // from pc_gen
    val pc_valid = in Bool() // from pc_gen
    val resolved_btb_entry = slave(btb_predict_entry(CoreConfig())) // from ex stage
    val predict_btb_entry = master(btb_predict_entry(CoreConfig())) // to pc_gen
  }
  val btb_lut = Vec(Reg(UInt((InstAddrBus+InstAddrBus+1) bits)) init (0),BTB_PAGE_NUM)

  for(i <- 0 until BTB_PAGE_NUM) {
    val target_pc = btb_lut(i)(InstAddrBus downto 1)
    val btb_index = btb_lut(i)(InstAddrBus+InstAddrBus downto InstAddrBus)
    val btb_taken = btb_lut(i)(0)
    when(io.pc_valid === True && (io.if_branch_predict.is_branch === True || io.if_branch_predict.is_call === True)){
      when(io.if_branch_predict.pc === btb_index){
        io.predict_btb_entry.btb_valid := True
        io.predict_btb_entry.btb_target := target_pc
        io.predict_btb_entry.btb_taken := btb_taken
      } .otherwise{
        io.predict_btb_entry.btb_valid := True
        io.predict_btb_entry.btb_target := 0
        io.predict_btb_entry.btb_taken := False
      }
    } .otherwise{
      io.predict_btb_entry.btb_valid := False
      io.predict_btb_entry.btb_target := 0
      io.predict_btb_entry.btb_taken := False
    }
  }

  val hit_vec = Vec(Bool(),BTB_PAGE_NUM)
  val hit = hit_vec.sContains(True)
  for(i <- 0 until BTB_PAGE_NUM) {
    hit_vec(i) := btb_lut(i)(InstAddrBus+InstAddrBus downto InstAddrBus+1) === io.resolved_btb_entry.pc
  }
  val replace_index = U"0"  // TODO with LRU/LFU replace strategy
  for(i <- 0 until BTB_PAGE_NUM) {
    when(~hit && i === replace_index && io.resolved_btb_entry.btb_valid === True){
      btb_lut(i) := ((InstAddrBus+InstAddrBus downto InstAddrBus+1) -> io.resolved_btb_entry.pc, (InstAddrBus downto 1) -> io.resolved_btb_entry.btb_target,0 -> io.resolved_btb_entry.btb_taken)
    } .otherwise{}
  }

}

class ras extends Component with Global_parameter with Interface_MS{
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val predict_branch_entry = slave(branch_predict_entry(CoreConfig())) // from bp
    val mispredict_entry = slave(branch_mispredict_entry(CoreConfig())) // from ex stage
    val ras_target = out UInt (InstAddrBus bits) // to pc_gen
  }
  val ras_stack = Vec(Reg(UInt(InstAddrBus bits)) init(0), RAS_STACK_DEEPTH)
  val ras_ptr = Reg(UInt(RAS_PTR_WIDTH bits)) init(0)
  io.ras_target := 0
  when(io.mispredict_entry.call_cor){ // 预测call错误
    //ras_stack(ras_ptr) := 0
    ras_ptr := ras_ptr - 1
  } .elsewhen(io.mispredict_entry.ret_cor) {  // 预测ret错误
    ras_stack(ras_ptr) := io.mispredict_entry.target_pc // ex stage correct target pc
    ras_ptr := ras_ptr + 1
  }.elsewhen(io.predict_branch_entry.is_call) {
    ras_stack(ras_ptr) := io.predict_branch_entry.branch_target + 4 //
    ras_ptr := ras_ptr + 1
  }.elsewhen(io.predict_branch_entry.is_ret) {
    io.ras_target := ras_stack(ras_ptr)
    ras_ptr := ras_ptr - 1
  }
}

class instr_realign extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val icache_rdy = in Bool() // from icache
    val icache_entry = master(icache_interface(CoreConfig())) // to icache
    val pc = in UInt(InstAddrBus bits)  // from pc_gen
    val instr_realign = master(instr_entry(CoreConfig())) // to pc_gen
  }
  // todo with Complex Instructions //
  io.icache_entry.addr := io.pc
  io.instr_realign.valid := io.icache_rdy
  io.instr_realign.inst := io.icache_entry.data

  //val icache_valid = Reg(Bool()) init(False)
  val icache_valid = Bool()
  icache_valid := True
  io.icache_entry.valid := icache_valid
}

class instr_queue extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val flush = in Bool() // from ex stage
    val icache_rdy = out Bool() // to cache
    val if_intr_entry = slave(instr_entry(CoreConfig())) // from realign
    val if_branch_predict_entry = slave(branch_predict_entry(CoreConfig())) // from pc_gen
    val if2id_instr_entry = master(instr_entry(CoreConfig())) // to id stage
    val if2id_branch_predict_entry = master(branch_predict_entry(CoreConfig())) // to id_stage
    val stall_pop = in Bool() // from scb & ... todo
    val stall_push = out Bool()  // to pc_gen & icache todo
  }
  val streamA,streamB = Stream(Bits(InstAddrBus+InstAddrBus+InstBus+5 bits))

  val instr_fifo = StreamFifo(
    dataType = Bits(InstAddrBus+InstAddrBus+InstBus+5 bits),
    depth    = Instr_FIFO_DEEPTH
  )
  val fifo_full = instr_fifo.io.occupancy > (Instr_FIFO_DEEPTH-1)
  val fifo_empty = instr_fifo.io.occupancy === 0

  instr_fifo.io.flush := io.flush

  when(io.if_intr_entry.valid && ~fifo_full){
    streamA.valid := True
    streamA.payload := io.if_intr_entry.inst##io.if_intr_entry.pc##io.if_branch_predict_entry.branch_target##io.if_branch_predict_entry.branch_taken##
                        io.if_branch_predict_entry.branch_valid##io.if_branch_predict_entry.is_branch##
                        io.if_branch_predict_entry.is_call##io.if_branch_predict_entry.is_ret
  } .otherwise{
    streamA.valid := False
    streamA.payload := 0
  }
  when(~fifo_empty && ~io.stall_pop){
    streamB.ready := True
  } .otherwise{
    streamB.ready := False
  }
  instr_fifo.io.push << streamA
  instr_fifo.io.pop  >> streamB

  io.if2id_instr_entry.valid := streamB.valid

/*
  (io.if2id_instr_entry.pc##io.if2id_branch_predict_entry.branch_target##io.if2id_branch_predict_entry.branch_taken##
    io.if2id_branch_predict_entry.branch_valid##io.if2id_branch_predict_entry.is_branch##
    io.if2id_branch_predict_entry.is_call##io.if2id_branch_predict_entry.is_ret) := streamB.payload.asBits
*/
  val streamB_payload = B(streamB.payload)
  io.if2id_instr_entry.inst := U(streamB_payload(InstAddrBus+InstAddrBus+InstBus+4 downto InstAddrBus+InstAddrBus+5))
  io.if2id_instr_entry.pc := U(streamB_payload(InstAddrBus+InstAddrBus+4 downto InstAddrBus+5))
  io.if2id_branch_predict_entry.pc := U(streamB_payload(InstAddrBus+InstAddrBus+4 downto InstAddrBus+5))
  io.if2id_branch_predict_entry.branch_target := U(streamB_payload(InstAddrBus+4 downto 5))
  io.if2id_branch_predict_entry.branch_taken := B(streamB_payload(4)).asBool
  io.if2id_branch_predict_entry.branch_valid := B(streamB_payload(3)).asBool
  io.if2id_branch_predict_entry.is_branch := B(streamB_payload(2)).asBool
  io.if2id_branch_predict_entry.is_call := B(streamB_payload(1)).asBool
  io.if2id_branch_predict_entry.is_ret := B(streamB_payload(0)).asBool



  io.icache_rdy.setAsReg() init(False)
  io.icache_rdy := ~fifo_full

  io.stall_push := fifo_full

}

class icache extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val icache_rdy = out Bool()  // from instr_queue
    val icache_entry = slave(icache_interface(CoreConfig())) // to instr_realign
  }
  // todo
  val init_array = new Array[UInt](InstMemNum)
  // ALU指令的测试
  /*
    init_array(0)=U"32'b00000000000000000001001000110111" // LUI x4,0x1
    init_array(1)=U"32'b00000000000000000001000110110111" // LUI x3,0x1
    init_array(2)=U"32'b00000000110000011101001010010011" // SRLLI x5, x3, 0xc
    //init_array(3)=U"32'b00000000011100011000001100010011" // ADDI x6, x3, 0x7
    init_array(3)=U"32'b01111111111100110000001100010011" // ADDI x6, x6, 0x7ff
    init_array(4)=U"32'b00000000011000101010010110100011" // SW x6, b, x5
    init_array(5)=U"32'b00000000010000110111010001100011" // BGE x6, x4, 0x8
    init_array(6)=U"32'b11111111010111111111000011101111" // JAL X1, -12
  */
  init_array(0)= U"32'h00000093"
  init_array(1)= U"32'h00000113"
  init_array(2)= U"32'h00000193"
  init_array(3)= U"32'h00000213"
  init_array(4)= U"32'h00000293"
  init_array(5)= U"32'h00000313"
  init_array(6)= U"32'h00000393"
  init_array(7)= U"32'h00000413"
  init_array(8)= U"32'h00000493"
  init_array(9)= U"32'h00000513"
  init_array(10)=U"32'h00000593"
  init_array(11)=U"32'h00000613"
  init_array(12)=U"32'h00000693"
  init_array(13)=U"32'h00000713"
  init_array(14)=U"32'h00000793"
  init_array(15)=U"32'h00000813"
  init_array(16)=U"32'h00000893"
  init_array(17)=U"32'h00000913"
  init_array(18)=U"32'h00000993"
  init_array(19)=U"32'h00000a13"
  init_array(20)=U"32'h00000a93"
  init_array(21)=U"32'h00000b13"
  init_array(22)=U"32'h00000b93"
  init_array(23)=U"32'h00000c13"
  init_array(24)=U"32'h00000c93"
  init_array(25)=U"32'h00000d13"
  init_array(26)=U"32'h00000d93"
  init_array(27)=U"32'h00000e13"
  init_array(28)=U"32'h00000e93"
  init_array(29)=U"32'h00000f13"
  init_array(30)=U"32'h00000f93"
  init_array(31)=U"32'hf8410113"
  init_array(32)=U"32'h04c000ef"
  init_array(33)=U"32'hfd010113"
  init_array(34)=U"32'h03010413"
  init_array(35)=U"32'hfca42e23"
  init_array(36)=U"32'hfcb42c23"
  init_array(37)=U"32'hfd842783"
  init_array(38)=U"32'h00e7d863"
  init_array(39)=U"32'hfdc42783"
  init_array(40)=U"32'h00c0006f"
  init_array(41)=U"32'hfd842783"
  init_array(42)=U"32'hfef42623"
  init_array(43)=U"32'h00078513"
  init_array(44)=U"32'h02c12403"
  init_array(45)=U"32'h03010113"
  init_array(46)=U"32'hfe010113"
  init_array(47)=U"32'h00112e23"
  init_array(48)=U"32'h00812c23"
  init_array(49)=U"32'h00200793"
  init_array(50)=U"32'hfef42623"
  init_array(51)=U"32'h00500793"
  init_array(52)=U"32'hfe842583"
  init_array(53)=U"32'hfec42503"
  init_array(54)=U"32'hf91ff0ef"
  init_array(55)=U"32'h00000793"
  init_array(56)=U"32'h00078513"
  init_array(57)=U"32'h01c12083"
  init_array(58)=U"32'h02010113"
  init_array(59)=U"32'h00008067"

  for(i <- 60 until InstMemNum) {init_array(i)=U"32'h00000000"}

  //  val mem_rom = Mem(UInt(InstBus bits),InstMemNum)
  val icache_rom = Mem(UInt(InstBus bits),initialContent = init_array)
  when( io.icache_entry.valid === False){
    io.icache_entry.data := 0
    io.icache_rdy := False
  }.otherwise{
    io.icache_entry.data := icache_rom(io.icache_entry.addr(InstMemNumLog2+1 downto 2))  // 所以取pc_addr具有32bits，而ROM储存区条目只有InstMemNum（2^17=131071)
    // 所以取pc_addr[16:0]，又因为pc_addr每一clk加2^2=4，相当于低端的2位没有作用
    // 或者说，等同于pc_addr右移2bit，故而取pc_addr[18:2]即可反映Ori指令下的地址
    io.icache_rdy := True
  }
}

class dcache extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val dcache_rdy = out Bool()
    val dcache_entry = slave(dcache_interface(CoreConfig()))
  }
  // todo
  val ram_0 = Mem(UInt(ByteWidth bits),DataMemNum)  // 每个ram模块内含8bit*DataMemNum的数据，则四个ram并行共有32bit数据
  val ram_1 = Mem(UInt(ByteWidth bits),DataMemNum)
  val ram_2 = Mem(UInt(ByteWidth bits),DataMemNum)
  val ram_3 = Mem(UInt(ByteWidth bits),DataMemNum)

  // 读内存操作,mem访存请求读取内存时，内存一次提供字，由mem决定取字的字节/半字/全字
    when(io.dcache_entry.re === True){
      // 取对应地址（去掉后两位，即对齐4字节，共取16bit地址）
      io.dcache_entry.rdata := ram_3(io.dcache_entry.raddr(DataMemNumLog2+1 downto 2)) @@
        ram_2(io.dcache_entry.raddr(DataMemNumLog2+1 downto 2)) @@
        ram_1(io.dcache_entry.raddr(DataMemNumLog2+1 downto 2)) @@
        ram_0(io.dcache_entry.raddr(DataMemNumLog2+1 downto 2))
    }
      .otherwise{
        io.dcache_entry.rdata := 0
      }

  when(io.dcache_entry.re === True || io.dcache_entry.we === True){
    io.dcache_rdy := True
  } .otherwise{
    io.dcache_rdy := False
  }


  /*
  // 写操作：时序逻辑电路, rst==RstDisable触发
  val wClockDomain = ClockDomain(
    clock = io.clk,
    reset = null,
    config = ClockConfig_rstH
  )
  */

  val r_ramAddr = io.dcache_entry.waddr(DataMemNumLog2+1 downto 2)  // 舍去原始地址后两位，对齐4的倍数
  val r_ram0En = io.dcache_entry.sel(0)
  val r_ram1En = io.dcache_entry.sel(1)
  val r_ram2En = io.dcache_entry.sel(2)
  val r_ram3En = io.dcache_entry.sel(3)
  val r_data0 = io.dcache_entry.wdata(7 downto 0)
  val r_data1 = io.dcache_entry.wdata(15 downto 8)
  val r_data2 = io.dcache_entry.wdata(23 downto 16)
  val r_data3 = io.dcache_entry.wdata(31 downto 24)

  //val areaClk = new ClockingArea(wClockDomain) {
  ram_0.write(
    address = r_ramAddr,
    data = r_data0,
    enable = r_ram0En && (io.dcache_entry.we === True)
  )
  ram_1.write(
    address = r_ramAddr,
    data = r_data1,
    enable = r_ram1En && (io.dcache_entry.we === True)
  )
  ram_2.write(
    address = r_ramAddr,
    data = r_data2,
    enable = r_ram2En && (io.dcache_entry.we === True)
  )
  ram_3.write(
    address = r_ramAddr,
    data = r_data3,
    enable = r_ram3En && (io.dcache_entry.we === True)
  )
  //}

//  io.dcache_rdy := r_ram0En && r_ram0En && r_ram0En && r_ram0En

}

/*
class decorder extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val id_instr_entry = slave(instr_entry(CoreConfig())) // from instr_queue
    //val id_branch_predict_entry = slave(branch_predict_entry(CoreConfig())) // from instr
    val id_dec_entry = master(decorder_entry(CoreConfig())) // to id2is stage
  }
  val dec_alu_sel = ALU_UNIT_SEL()  // Enum
  val dec_op_type = OP_TYPE()
  val dec_rs1_entry = register_entry(CoreConfig())
  val dec_rs2_entry = register_entry(CoreConfig())
  val dec_rd_entry = register_entry(CoreConfig())
  val dec_imm = SInt(ImmBus bits)
  val dec_valid = Bool()
  val predict_flag = Bool()

  val imm_i = SInt(ImmBus bits)
  val imm_bj = SInt(ImmBus bits)
  imm_i := S(io.id_instr_entry.inst(31 downto 20)).resize(ImmBus bits)
  imm_bj := S(io.id_instr_entry.inst(31 downto 31)@@io.id_instr_entry.inst(7 downto 7)@@io.id_instr_entry.inst(30 downto 25)@@io.id_instr_entry.inst(11 downto 8)@@U"0").resize(ImmBus bits)

  dec_alu_sel := ALU_UNIT_SEL.NOPU
  dec_op_type := OP_TYPE.OP_NOP
  dec_rs1_entry.reg_addr := io.id_instr_entry.inst(19 downto 15)
  dec_rs2_entry.reg_addr := io.id_instr_entry.inst(24 downto 20)
  dec_rd_entry.reg_addr := io.id_instr_entry.inst(11 downto 7)
  dec_rs1_entry.reg_rden := False
  dec_rs1_entry.reg_wten := False
  dec_rs2_entry.reg_rden := False
  dec_rs2_entry.reg_wten := False
  dec_rd_entry.reg_rden := False
  dec_rd_entry.reg_wten := False
  dec_imm := 0
  dec_valid := False
  predict_flag := False

  when(io.id_instr_entry.valid === True){
    switch(io.id_instr_entry.inst){
      is(ADD,SUB){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_ARITHMETIC
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_valid := True
      }
      is(ADDI){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_ARITHMETIC
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(OR,AND,XOR){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_LOGIC
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_valid := True
      }
      is(ANDI,ORI,XORI){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_LOGIC
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(SLL,SRL,SRA){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_valid := True
      }
      is(SLLI,SRLI,SRAI){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(SLT,SLTU){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_valid := True
      }
      is(SLTI,SLTIU){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(LUI){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rd_entry.reg_wten := True
        dec_imm := S(io.id_instr_entry.inst(31 downto 12)).resize(ImmBus bits)
        dec_valid := True
      }
      is(AUIPC){
        dec_alu_sel := ALU_UNIT_SEL.ALUU
        dec_op_type := OP_TYPE.OP_SHIFT
        dec_rd_entry.reg_wten := True
        dec_imm := S(io.id_instr_entry.inst(31 downto 12)).resize(ImmBus bits)
        dec_valid := True
      }
      is(LB,LH,LW,LBU,LHU){
        dec_alu_sel := ALU_UNIT_SEL.LSU
        dec_op_type := OP_TYPE.OP_LOAD_STORE
        dec_rs1_entry.reg_rden := True
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(SB,SH,SW){
        dec_alu_sel := ALU_UNIT_SEL.LSU
        dec_op_type := OP_TYPE.OP_LOAD_STORE
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_imm := S(io.id_instr_entry.inst(31 downto 25)@@io.id_instr_entry.inst(11 downto 7)).resize(ImmBus bits)
        dec_valid := True
      }
      is(JAL){
        dec_alu_sel := ALU_UNIT_SEL.BJU
        dec_op_type := OP_TYPE.OP_JUMP_BRANCH
        dec_rd_entry.reg_wten := True
        dec_imm := S(io.id_instr_entry.inst(31 downto 31)@@io.id_instr_entry.inst(19 downto 12)@@io.id_instr_entry.inst(20 downto 20)@@io.id_instr_entry.inst(30 downto 21)@@U"0").resize(ImmBus bits)
        dec_valid := True
      }
      is(JALR){
        dec_alu_sel := ALU_UNIT_SEL.BJU
        dec_op_type := OP_TYPE.OP_JUMP_BRANCH
        dec_rd_entry.reg_wten := True
        dec_imm := imm_i
        dec_valid := True
      }
      is(BEQ,BNE,BLT,BGE,BLTU,BGEU){
        dec_alu_sel := ALU_UNIT_SEL.BJU
        dec_op_type := OP_TYPE.OP_JUMP_BRANCH
        dec_rs1_entry.reg_rden := True
        dec_rs2_entry.reg_rden := True
        dec_imm := imm_bj
        dec_valid := True
        predict_flag := True
      }

      // TODO //

      default{
        dec_alu_sel := ALU_UNIT_SEL.NOPU
        dec_op_type := OP_TYPE.OP_NOP
        dec_rs1_entry.reg_addr := io.id_instr_entry.inst(19 downto 15)
        dec_rs2_entry.reg_addr := io.id_instr_entry.inst(24 downto 20)
        dec_rd_entry.reg_addr := io.id_instr_entry.inst(11 downto 7)
        dec_rs1_entry.reg_rden := False
        dec_rs1_entry.reg_wten := False
        dec_rs2_entry.reg_rden := False
        dec_rs2_entry.reg_wten := False
        dec_rd_entry.reg_rden := False
        dec_rd_entry.reg_wten := False
        dec_imm := 0
        dec_valid := False
        predict_flag := False
        assert(
          assertion = True,
          message   = "Unsupported instruction !!!",
          severity  = ERROR
        )
      }
    }
  } .otherwise{
    dec_alu_sel := ALU_UNIT_SEL.NOPU
    dec_op_type := OP_TYPE.OP_NOP
    dec_rs1_entry.reg_addr := io.id_instr_entry.inst(19 downto 15)
    dec_rs2_entry.reg_addr := io.id_instr_entry.inst(24 downto 20)
    dec_rd_entry.reg_addr := io.id_instr_entry.inst(11 downto 7)
    dec_rs1_entry.reg_rden := False
    dec_rs1_entry.reg_wten := False
    dec_rs2_entry.reg_rden := False
    dec_rs2_entry.reg_wten := False
    dec_rd_entry.reg_rden := False
    dec_rd_entry.reg_wten := False
    dec_imm := 0
    dec_valid := False
    predict_flag := False
  }

  io.id_dec_entry.pc := io.id_instr_entry.pc
  io.id_dec_entry.instr := io.id_instr_entry.inst
  io.id_dec_entry.alu_sel := dec_alu_sel
  io.id_dec_entry.op_type := dec_op_type
  io.id_dec_entry.rs1_entry := dec_rs1_entry
  io.id_dec_entry.rs2_entry := dec_rs2_entry
  io.id_dec_entry.rd_entry := dec_rd_entry
  io.id_dec_entry.imm := dec_imm
  io.id_dec_entry.dec_valid := dec_valid
  io.id_dec_entry.predict_flag := predict_flag

}
*/

class id2issue extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val flush = in Bool()
    val id_dec_entry = slave(decorder_entry(CoreConfig())) // from decorder
    val id_branch_predict_entry = slave(branch_predict_entry(CoreConfig())) // from instr
    val id2issue_dec_entry = master(decorder_entry(CoreConfig())) // to issue
    val id2issue_branch_predict_entry = master(branch_predict_entry(CoreConfig())) // to issue
    val icache_rdy = in Bool()
  }

  io.id2issue_dec_entry.setAsReg()
  io.id2issue_branch_predict_entry.setAsReg()
  io.id2issue_dec_entry.pc init(0)
  io.id2issue_dec_entry.instr init(0)
  io.id2issue_dec_entry.alu_sel init(ALU_UNIT_SEL.NOPU)
  io.id2issue_dec_entry.op_type init(OP_TYPE.OP_NOP)
  io.id2issue_dec_entry.rs1_entry.reg_rden init(False)
  io.id2issue_dec_entry.rs1_entry.reg_wten init(False)
  io.id2issue_dec_entry.rs1_entry.reg_addr init(0)
  io.id2issue_dec_entry.rs2_entry.reg_rden init(False)
  io.id2issue_dec_entry.rs2_entry.reg_wten init(False)
  io.id2issue_dec_entry.rs2_entry.reg_addr init(0)
  io.id2issue_dec_entry.rd_entry.reg_rden init(False)
  io.id2issue_dec_entry.rd_entry.reg_wten init(False)
  io.id2issue_dec_entry.rd_entry.reg_addr init(0)
  io.id2issue_dec_entry.csr_entry.reg_rden init(False)
  io.id2issue_dec_entry.csr_entry.reg_wten init(False)
  io.id2issue_dec_entry.csr_entry.reg_addr init(0)
  io.id2issue_dec_entry.imm init(0)
  io.id2issue_dec_entry.dec_valid init(False)
  io.id2issue_dec_entry.predict_flag init(False)
  io.id2issue_branch_predict_entry.pc init(0)
  io.id2issue_branch_predict_entry.branch_target init(0)
  io.id2issue_branch_predict_entry.branch_taken init(False)
  io.id2issue_branch_predict_entry.branch_valid init(False)
  io.id2issue_branch_predict_entry.is_branch init(False)
  io.id2issue_branch_predict_entry.is_call init(False)
  io.id2issue_branch_predict_entry.is_ret init(False)

  when(io.flush === True){
    io.id2issue_dec_entry.pc := 0
    io.id2issue_dec_entry.instr := 0
    io.id2issue_dec_entry.alu_sel := ALU_UNIT_SEL.NOPU
    io.id2issue_dec_entry.op_type := OP_TYPE.OP_NOP
    io.id2issue_dec_entry.rs1_entry.reg_rden := False
    io.id2issue_dec_entry.rs1_entry.reg_wten := False
    io.id2issue_dec_entry.rs1_entry.reg_addr := 0
    io.id2issue_dec_entry.rs2_entry.reg_rden := False
    io.id2issue_dec_entry.rs2_entry.reg_wten := False
    io.id2issue_dec_entry.rs2_entry.reg_addr := 0
    io.id2issue_dec_entry.rd_entry.reg_rden := False
    io.id2issue_dec_entry.rd_entry.reg_wten := False
    io.id2issue_dec_entry.rd_entry.reg_addr := 0
    io.id2issue_dec_entry.csr_entry.reg_rden := False
    io.id2issue_dec_entry.csr_entry.reg_wten := False
    io.id2issue_dec_entry.csr_entry.reg_addr := 0
    io.id2issue_dec_entry.imm := 0
    io.id2issue_dec_entry.dec_valid := False
    io.id2issue_dec_entry.predict_flag := False
    io.id2issue_branch_predict_entry.pc := 0
    io.id2issue_branch_predict_entry.branch_target := 0
    io.id2issue_branch_predict_entry.branch_taken := False
    io.id2issue_branch_predict_entry.branch_valid := False
    io.id2issue_branch_predict_entry.is_branch :=False
    io.id2issue_branch_predict_entry.is_call :=False
    io.id2issue_branch_predict_entry.is_ret :=False
  } .elsewhen(io.icache_rdy === True){
    //io.id2issue_dec_entry := io.id_dec_entry
    //io.id2issue_branch_predict_entry := io.id_branch_predict_entry
    io.id2issue_dec_entry.pc := io.id_dec_entry.pc
    io.id2issue_dec_entry.instr := io.id_dec_entry.instr
    io.id2issue_dec_entry.alu_sel := io.id_dec_entry.alu_sel
    io.id2issue_dec_entry.op_type := io.id_dec_entry.op_type
    io.id2issue_dec_entry.rs1_entry.reg_rden := io.id_dec_entry.rs1_entry.reg_rden
    io.id2issue_dec_entry.rs1_entry.reg_wten := io.id_dec_entry.rs1_entry.reg_wten
    io.id2issue_dec_entry.rs1_entry.reg_addr := io.id_dec_entry.rs1_entry.reg_addr
    io.id2issue_dec_entry.rs2_entry.reg_rden := io.id_dec_entry.rs2_entry.reg_rden
    io.id2issue_dec_entry.rs2_entry.reg_wten := io.id_dec_entry.rs2_entry.reg_wten
    io.id2issue_dec_entry.rs2_entry.reg_addr := io.id_dec_entry.rs2_entry.reg_addr
    io.id2issue_dec_entry.rd_entry.reg_rden := io.id_dec_entry.rd_entry.reg_rden
    io.id2issue_dec_entry.rd_entry.reg_wten := io.id_dec_entry.rd_entry.reg_wten
    io.id2issue_dec_entry.rd_entry.reg_addr := io.id_dec_entry.rd_entry.reg_addr
    io.id2issue_dec_entry.csr_entry.reg_rden := io.id_dec_entry.csr_entry.reg_rden
    io.id2issue_dec_entry.csr_entry.reg_wten := io.id_dec_entry.csr_entry.reg_wten
    io.id2issue_dec_entry.csr_entry.reg_addr := io.id_dec_entry.csr_entry.reg_addr
    io.id2issue_dec_entry.imm := io.id_dec_entry.imm
    io.id2issue_dec_entry.dec_valid := io.id_dec_entry.dec_valid
    io.id2issue_branch_predict_entry.pc := io.id_branch_predict_entry.pc
    io.id2issue_branch_predict_entry.branch_target := io.id_branch_predict_entry.branch_target
    io.id2issue_branch_predict_entry.branch_taken := io.id_branch_predict_entry.branch_taken
    io.id2issue_branch_predict_entry.branch_valid := io.id_branch_predict_entry.branch_valid
    io.id2issue_branch_predict_entry.is_branch :=io.id_branch_predict_entry.is_branch
    io.id2issue_branch_predict_entry.is_call :=io.id_branch_predict_entry.is_call
    io.id2issue_branch_predict_entry.is_ret :=io.id_branch_predict_entry.is_ret
    when(io.id_dec_entry.predict_flag === True){
      io.id2issue_dec_entry.predict_flag := True
    } .otherwise{ }
  }
}

/*
class commit extends Component with Global_parameter with Interface_MS{
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val ex_commit_entry = slave(commit_entry(CoreConfig())) // from scb
    val wb_regfile_interface = master(wregfile_interface(CoreConfig()))  // to regfile
    val wb_csr_interface = master(wcsr_interface(CoreConfig())) // to csr regfile
    val wb_dacahe_interfacec = master(dcache_interface(CoreConfig()))  // to dcache
    val bju_mis_predict = slave(branch_mispredict_entry(CoreConfig())) // from ex stage
    val flush = out Bool()
    val flush_mis_predict = out Bool()
    val flush_mis_predict_target_pc = out UInt(InstAddrBus bits)
    // exception interface //
    val extern_int_req = in Bool()  // 外部中断源，from PLIC
    val software_int_req = in Bool()  // 软件中断，from CLINT
    val timer_int_req = in Bool() // 定时器中断，from CLINT
  }

  // 异常列表+杂项 //
  //val flush_mis_predict = io.bju_mis_predict.branch_cor || io.bju_mis_predict.ret_cor || io.bju_mis_predict.call_cor
  val flush_mis_predict = io.ex_commit_entry.branch_cor || io.ex_commit_entry.ret_cor || io.ex_commit_entry.call_cor
  val flush_mis_predict_target_pc = io.ex_commit_entry.target_pc
  val flush_mis_predict_d1 = Reg(Bool()) init(False)
  flush_mis_predict_d1 := flush_mis_predict
  val flush_mis_predict_pulse = flush_mis_predict && ~flush_mis_predict_d1

  io.flush := flush_mis_predict_pulse
  io.flush_mis_predict := flush_mis_predict_pulse
  io.flush_mis_predict_target_pc := flush_mis_predict_target_pc

  when(io.ex_commit_entry.commit_req){
    io.wb_regfile_interface.reg_wen := io.ex_commit_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.ex_commit_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.ex_commit_entry.reg_wb_data
    io.wb_csr_interface.reg_wen := io.ex_commit_entry.csr_wb_en
    io.wb_csr_interface.reg_waddr := io.ex_commit_entry.csr_wb_addr
    io.wb_csr_interface.reg_wdata := io.ex_commit_entry.csr_wb_data
    io.wb_dacahe_interfacec.we := io.ex_commit_entry.dcache_wb_en
    io.wb_dacahe_interfacec.waddr := io.ex_commit_entry.dcache_wb_addr
    io.wb_dacahe_interfacec.wdata := io.ex_commit_entry.dcache_wb_data
    io.wb_dacahe_interfacec.re := io.ex_commit_entry.dcache_rd_en
    io.wb_dacahe_interfacec.raddr := io.ex_commit_entry.dcache_rd_addr
    io.wb_dacahe_interfacec.sel := U(io.ex_commit_entry.dcache_wb_sel)
    io.ex_commit_entry.dcache_rd_data := io.wb_dacahe_interfacec.rdata
    }.otherwise{
    io.wb_regfile_interface.reg_wen := False
    io.wb_regfile_interface.reg_waddr := 0
    io.wb_regfile_interface.reg_wdata := 0
    io.wb_csr_interface.reg_wen := False
    io.wb_csr_interface.reg_waddr := 0
    io.wb_csr_interface.reg_wdata := 0
    io.wb_dacahe_interfacec.we := False
    io.wb_dacahe_interfacec.waddr := 0
    io.wb_dacahe_interfacec.wdata := 0
    io.wb_dacahe_interfacec.re := False
    io.wb_dacahe_interfacec.raddr := 0
    io.wb_dacahe_interfacec.sel := U"1111"
    io.ex_commit_entry.dcache_rd_data := 0
  }
  val commit_req_ack = Reg(Bool()) init(False)
  val recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  when(io.ex_commit_entry.commit_req){
    commit_req_ack := True  // todo with regfile/csr/dcache ack
    recv_id := io.ex_commit_entry.trans_id
  } .otherwise{
    commit_req_ack := False
    recv_id := SCB_IU_DEEPTH
  }
  io.ex_commit_entry.commit_ack := commit_req_ack
  io.ex_commit_entry.recv_id := recv_id

  val report_cnt = Reg(UInt(1 bits)) init(0)
  when(io.ex_commit_entry.commit_ack){
    report_cnt := report_cnt + 1
  }

  when(io.ex_commit_entry.commit_ack && report_cnt===0){
    report(Seq("retire pc : ", io.ex_commit_entry.pc))
    report(Seq("retire instr : ", io.ex_commit_entry.instr))
    when(io.wb_regfile_interface.reg_wen){
      report(Seq("update regfile [addr] : ", io.wb_regfile_interface.reg_waddr))
      report(Seq("update regfile [data] : ", io.wb_regfile_interface.reg_wdata))
    } .otherwise{}
    when(io.wb_dacahe_interfacec.we){
      report(Seq("write dcache [addr] : ", io.wb_dacahe_interfacec.waddr))
      report(Seq("write dcache [data] : ", io.wb_dacahe_interfacec.wdata))
    } .otherwise{}
    /*
    when(io.wb_dacahe_interfacec.re){
      report(Seq("read dcache", io.wb_dacahe_interfacec.raddr, io.wb_dacahe_interfacec.rdata))
    } .otherwise{}
    */
  } .otherwise{}

}
*/

/*
class wb extends Component with Global_parameter with Interface_MS{
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val ex_wb_entry = slave(commit_entry(CoreConfig())) // from scb
    val alu_ex_wb_entry  = slave(commit_entry(CoreConfig()))
    val mul1_ex_wb_entry = slave(commit_entry(CoreConfig()))
    val mul2_ex_wb_entry = slave(commit_entry(CoreConfig()))
    val divu_ex_wb_entry = slave(commit_entry(CoreConfig()))
    val bju_ex_wb_entry  = slave(commit_entry(CoreConfig()))
    val lsu_ex_wb_entry  = slave(commit_entry(CoreConfig()))
    val csr_ex_wb_entry  = slave(commit_entry(CoreConfig()))
    val nopu_ex_wb_entry = slave(commit_entry(CoreConfig()))
    val wb_regfile_interface = master(wregfile_interface(CoreConfig()))  // to regfile
    val wb_csr_interface = master(wcsr_interface(CoreConfig())) // to csr regfile
    val wb_scb_entry = master(commit_entry(CoreConfig()))  // to scb
    val head_ptr = in UInt(SCB_INSTR_WIDTH bits) // from scb
  }
  val INSTR_TAB = Vec(Reg(UInt(InstBus bits)) init(0), SCB_INSTR_DEEPTH)
  val PC_TAB = Vec(Reg(UInt(InstAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val REG_WEN_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val REG_WADDR_TAB = Vec(Reg(UInt(RegAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val REG_WDATA_TAB = Vec(Reg(UInt(RegDataBus bits)) init(0), SCB_INSTR_DEEPTH)
  val CSR_WEN_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val CSR_WADDR_TAB = Vec(Reg(UInt(RegAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val CSR_WDATA_TAB = Vec(Reg(UInt(RegDataBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DCACHE_REN_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val DCACHE_RADDR_TAB = Vec(Reg(UInt(DataAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DCACHE_WEN_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val DCACHE_WADDR_TAB = Vec(Reg(UInt(DataAddrBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DCACHE_WDATA_TAB = Vec(Reg(UInt(DataBus bits)) init(0), SCB_INSTR_DEEPTH)
  val DCACHE_WSEL_TAB = Vec(Reg(Bits(4 bits)) init(0), SCB_INSTR_DEEPTH)
  val BRANCH_COR_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val CALL_COR_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val RET_COR_TAB = Vec(Reg(Bool()) init(False), SCB_INSTR_DEEPTH)
  val TARGET_PC_TAB = Vec(Reg(UInt(InstAddrBus bits)) init(0), SCB_INSTR_DEEPTH)

  val index_alu = io.alu_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.alu_ex_wb_entry.commit_req){
    INSTR_TAB(index_alu)        := io.alu_ex_wb_entry.instr
    PC_TAB(index_alu)           := io.alu_ex_wb_entry.pc
    REG_WEN_TAB(index_alu)      := io.alu_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_alu)    := io.alu_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_alu)    := io.alu_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_alu)      := io.alu_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_alu)    := io.alu_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_alu)    := io.alu_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_alu)   := io.alu_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_alu) := io.alu_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_alu)   := io.alu_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_alu)  := io.alu_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_alu) := io.alu_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_alu) := io.alu_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_alu)   := io.alu_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_alu)     := io.alu_ex_wb_entry.call_cor
    RET_COR_TAB(index_alu)      := io.alu_ex_wb_entry.ret_cor
    TARGET_PC_TAB(index_alu)    := io.alu_ex_wb_entry.target_pc
  } .otherwise{ }

  val index_mul1 = io.mul1_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.mul1_ex_wb_entry.commit_req){
    INSTR_TAB(index_mul1)        := io.mul1_ex_wb_entry.instr
    PC_TAB(index_mul1)           := io.mul1_ex_wb_entry.pc
    REG_WEN_TAB(index_mul1)      := io.mul1_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_mul1)    := io.mul1_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_mul1)    := io.mul1_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_mul1)      := io.mul1_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_mul1)    := io.mul1_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_mul1)    := io.mul1_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_mul1)   := io.mul1_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_mul1) := io.mul1_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_mul1)   := io.mul1_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_mul1)  := io.mul1_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_mul1) := io.mul1_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_mul1) := io.mul1_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_mul1)   := io.mul1_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_mul1)     := io.mul1_ex_wb_entry.call_cor
    RET_COR_TAB(index_mul1)      := io.mul1_ex_wb_entry.ret_cor
    TARGET_PC_TAB(index_mul1)    := io.mul1_ex_wb_entry.target_pc
  } .otherwise{ }

  val index_mul2 = io.mul2_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.mul2_ex_wb_entry.commit_req){
    INSTR_TAB(index_mul2)        := io.mul2_ex_wb_entry.instr
    PC_TAB(index_mul2)           := io.mul2_ex_wb_entry.pc
    REG_WEN_TAB(index_mul2)      := io.mul2_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_mul2)    := io.mul2_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_mul2)    := io.mul2_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_mul2)      := io.mul2_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_mul2)    := io.mul2_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_mul2)    := io.mul2_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_mul2)   := io.mul2_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_mul2) := io.mul2_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_mul2)   := io.mul2_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_mul2)  := io.mul2_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_mul2) := io.mul2_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_mul2) := io.mul2_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_mul2)   := io.mul2_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_mul2)     := io.mul2_ex_wb_entry.call_cor
    RET_COR_TAB(index_mul2)      := io.mul2_ex_wb_entry.ret_cor
    TARGET_PC_TAB(index_mul2)    := io.mul2_ex_wb_entry.target_pc
  } .otherwise{ }

  val index_divu = io.divu_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.divu_ex_wb_entry.commit_req){
    INSTR_TAB(index_divu)        := io.divu_ex_wb_entry.instr
    PC_TAB(index_divu)           := io.divu_ex_wb_entry.pc
    REG_WEN_TAB(index_divu)      := io.divu_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_divu)    := io.divu_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_divu)    := io.divu_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_divu)      := io.divu_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_divu)    := io.divu_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_divu)    := io.divu_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_divu)   := io.divu_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_divu) := io.divu_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_divu)   := io.divu_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_divu)  := io.divu_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_divu) := io.divu_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_divu) := io.divu_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_divu)   := io.divu_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_divu)     := io.divu_ex_wb_entry.call_cor
    RET_COR_TAB(index_divu)      := io.divu_ex_wb_entry.ret_cor
    TARGET_PC_TAB(index_divu)    := io.divu_ex_wb_entry.target_pc
  } .otherwise{ }

  val index_bju = io.bju_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.bju_ex_wb_entry.commit_req){
    INSTR_TAB(index_bju)        := io.bju_ex_wb_entry.instr
    PC_TAB(index_bju)           := io.bju_ex_wb_entry.pc
    REG_WEN_TAB(index_bju)      := io.bju_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_bju)    := io.bju_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_bju)    := io.bju_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_bju)      := io.bju_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_bju)    := io.bju_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_bju)    := io.bju_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_bju)   := io.bju_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_bju) := io.bju_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_bju)   := io.bju_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_bju)  := io.bju_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_bju) := io.bju_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_bju) := io.bju_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_bju)   := io.bju_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_bju)     := io.bju_ex_wb_entry.call_cor
    RET_COR_TAB(index_bju)      := io.bju_ex_wb_entry.ret_cor
    TARGET_PC_TAB(index_bju)    := io.bju_ex_wb_entry.target_pc
  } .otherwise{ }

  val index_lsu = io.lsu_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.lsu_ex_wb_entry.commit_req){
    INSTR_TAB(index_lsu)        := io.lsu_ex_wb_entry.instr
    PC_TAB(index_lsu)           := io.lsu_ex_wb_entry.pc
    REG_WEN_TAB(index_lsu)      := io.lsu_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_lsu)    := io.lsu_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_lsu)    := io.lsu_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_lsu)      := io.lsu_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_lsu)    := io.lsu_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_lsu)    := io.lsu_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_lsu)   := io.lsu_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_lsu) := io.lsu_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_lsu)   := io.lsu_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_lsu)  := io.lsu_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_lsu) := io.lsu_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_lsu) := io.lsu_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_lsu)   := io.lsu_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_lsu)     := io.lsu_ex_wb_entry.call_cor
    RET_COR_TAB(index_lsu)      := io.lsu_ex_wb_entry.ret_cor
    TARGET_PC_TAB(index_lsu)    := io.lsu_ex_wb_entry.target_pc
  } .otherwise{ }

  val index_csr = io.csr_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.csr_ex_wb_entry.commit_req){
    INSTR_TAB(index_csr)        := io.csr_ex_wb_entry.instr
    PC_TAB(index_csr)           := io.csr_ex_wb_entry.pc
    REG_WEN_TAB(index_csr)      := io.csr_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_csr)    := io.csr_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_csr)    := io.csr_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_csr)      := io.csr_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_csr)    := io.csr_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_csr)    := io.csr_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_csr)   := io.csr_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_csr) := io.csr_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_csr)   := io.csr_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_csr)  := io.csr_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_csr) := io.csr_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_csr) := io.csr_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_csr)   := io.csr_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_csr)     := io.csr_ex_wb_entry.call_cor
    RET_COR_TAB(index_csr)      := io.csr_ex_wb_entry.ret_cor
    TARGET_PC_TAB(index_csr)    := io.csr_ex_wb_entry.target_pc
  } .otherwise{ }

  val index_nopu = io.nopu_ex_wb_entry.trans_id(SCB_INSTR_WIDTH-1 downto 0)
  when(io.nopu_ex_wb_entry.commit_req){
    INSTR_TAB(index_nopu)        := io.nopu_ex_wb_entry.instr
    PC_TAB(index_nopu)           := io.nopu_ex_wb_entry.pc
    REG_WEN_TAB(index_nopu)      := io.nopu_ex_wb_entry.reg_wb_en
    REG_WADDR_TAB(index_nopu)    := io.nopu_ex_wb_entry.reg_wb_addr
    REG_WDATA_TAB(index_nopu)    := io.nopu_ex_wb_entry.reg_wb_data
    CSR_WEN_TAB(index_nopu)      := io.nopu_ex_wb_entry.csr_wb_en
    CSR_WADDR_TAB(index_nopu)    := io.nopu_ex_wb_entry.csr_wb_addr
    CSR_WDATA_TAB(index_nopu)    := io.nopu_ex_wb_entry.csr_wb_data
    DCACHE_REN_TAB(index_nopu)   := io.nopu_ex_wb_entry.dcache_rd_en
    DCACHE_RADDR_TAB(index_nopu) := io.nopu_ex_wb_entry.dcache_rd_addr
    DCACHE_WEN_TAB(index_nopu)   := io.nopu_ex_wb_entry.dcache_wb_en
    DCACHE_WSEL_TAB(index_nopu)  := io.nopu_ex_wb_entry.dcache_wb_sel
    DCACHE_WADDR_TAB(index_nopu) := io.nopu_ex_wb_entry.dcache_wb_addr
    DCACHE_WDATA_TAB(index_nopu) := io.nopu_ex_wb_entry.dcache_wb_data
    BRANCH_COR_TAB(index_nopu)   := io.nopu_ex_wb_entry.branch_cor
    CALL_COR_TAB(index_nopu)     := io.nopu_ex_wb_entry.call_cor
    RET_COR_TAB(index_nopu)      := io.nopu_ex_wb_entry.ret_cor
    TARGET_PC_TAB(index_nopu)    := io.nopu_ex_wb_entry.target_pc
  } .otherwise{ }

  when(io.alu_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.alu_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.alu_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.alu_ex_wb_entry.reg_wb_data
  } .elsewhen(io.mul1_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.mul1_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.mul1_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.mul1_ex_wb_entry.reg_wb_data
  } .elsewhen(io.mul2_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.mul2_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.mul2_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.mul2_ex_wb_entry.reg_wb_data
  } .elsewhen(io.divu_ex_wb_entry.commit_req){
  io.wb_regfile_interface.reg_wen   := io.divu_ex_wb_entry.reg_wb_en
  io.wb_regfile_interface.reg_waddr := io.divu_ex_wb_entry.reg_wb_addr
  io.wb_regfile_interface.reg_wdata := io.divu_ex_wb_entry.reg_wb_data
  } .elsewhen(io.bju_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.bju_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.bju_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.bju_ex_wb_entry.reg_wb_data
  } .elsewhen(io.lsu_ex_wb_entry.commit_req){
    io.wb_regfile_interface.reg_wen   := io.lsu_ex_wb_entry.reg_wb_en
    io.wb_regfile_interface.reg_waddr := io.lsu_ex_wb_entry.reg_wb_addr
    io.wb_regfile_interface.reg_wdata := io.lsu_ex_wb_entry.reg_wb_data
  } .otherwise{
  io.wb_regfile_interface.reg_wen := False
  io.wb_regfile_interface.reg_waddr := 0
  io.wb_regfile_interface.reg_wdata := 0
  }
  when(io.csr_ex_wb_entry.commit_req){
    io.wb_csr_interface.reg_wen       := io.csr_ex_wb_entry.csr_wb_en
    io.wb_csr_interface.reg_waddr     := io.csr_ex_wb_entry.csr_wb_addr
    io.wb_csr_interface.reg_wdata     := io.csr_ex_wb_entry.csr_wb_data
  } .otherwise{
    io.wb_csr_interface.reg_wen := False
    io.wb_csr_interface.reg_waddr := 0
    io.wb_csr_interface.reg_wdata := 0
  }
  val alu_commit_req_ack = Reg(Bool()) init(False)
  val alu_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val mul1_commit_req_ack = Reg(Bool()) init(False)
  val mul1_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val mul2_commit_req_ack = Reg(Bool()) init(False)
  val mul2_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val divu_commit_req_ack = Reg(Bool()) init(False)
  val divu_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val bju_commit_req_ack = Reg(Bool()) init(False)
  val bju_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val lsu_commit_req_ack = Reg(Bool()) init(False)
  val lsu_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val csr_commit_req_ack = Reg(Bool()) init(False)
  val csr_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val nop_commit_req_ack = Reg(Bool()) init(False)
  val nop_recv_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)

  when(io.alu_ex_wb_entry.commit_req){
    alu_commit_req_ack := True  // todo with regfile/csr/dcache ack
    alu_recv_id := io.alu_ex_wb_entry.trans_id
  } .otherwise{
    alu_commit_req_ack := False
    alu_recv_id := SCB_IU_DEEPTH
  }
  when(io.mul1_ex_wb_entry.commit_req){
    mul1_commit_req_ack := True  // todo with regfile/csr/dcache ack
    mul1_recv_id := io.mul1_ex_wb_entry.trans_id
  } .otherwise{
    mul1_commit_req_ack := False
    mul1_recv_id := SCB_IU_DEEPTH
  }
  when(io.mul2_ex_wb_entry.commit_req){
    mul2_commit_req_ack := True  // todo with regfile/csr/dcache ack
    mul2_recv_id := io.mul2_ex_wb_entry.trans_id
  } .otherwise{
    mul2_commit_req_ack := False
    mul2_recv_id := SCB_IU_DEEPTH
  }
  when(io.divu_ex_wb_entry.commit_req){
    divu_commit_req_ack := True  // todo with regfile/csr/dcache ack
    divu_recv_id := io.divu_ex_wb_entry.trans_id
  } .otherwise{
    divu_commit_req_ack := False
    divu_recv_id := SCB_IU_DEEPTH
  }
  when(io.bju_ex_wb_entry.commit_req){
    bju_commit_req_ack := True  // todo with regfile/csr/dcache ack
    bju_recv_id := io.bju_ex_wb_entry.trans_id
  } .otherwise{
    bju_commit_req_ack := False
    bju_recv_id := SCB_IU_DEEPTH
  }
  when(io.lsu_ex_wb_entry.commit_req){
    lsu_commit_req_ack := True  // todo with regfile/csr/dcache ack
    lsu_recv_id := io.lsu_ex_wb_entry.trans_id
  } .otherwise{
    lsu_commit_req_ack := False
    lsu_recv_id := SCB_IU_DEEPTH
  }
  when(io.csr_ex_wb_entry.commit_req){
    csr_commit_req_ack := True  // todo with regfile/csr/dcache ack
    csr_recv_id := io.csr_ex_wb_entry.trans_id
  } .otherwise{
    csr_commit_req_ack := False
    csr_recv_id := SCB_IU_DEEPTH
  }
  when(io.nopu_ex_wb_entry.commit_req){
    nop_commit_req_ack := True  // todo with regfile/csr/dcache ack
    nop_recv_id := io.nopu_ex_wb_entry.trans_id
  } .otherwise{
    nop_commit_req_ack := False
    nop_recv_id := SCB_IU_DEEPTH
  }

  val wb_scb_entry_reg_wb_addr = UInt(RegAddrBus bits)
  val wb_scb_entry_reg_wb_data = UInt(RegDataBus bits)
  val wb_scb_entry_reg_wb_en = Bool()
  val wb_scb_entry_csr_wb_addr = UInt(RegAddrBus bits)
  val wb_scb_entry_csr_wb_data = UInt(RegDataBus bits)
  val wb_scb_entry_csr_wb_en = Bool()
  val wb_scb_entry_dcache_wb_en = Bool()
  val wb_scb_entry_dcache_wb_addr = UInt(DataAddrBus bits)
  val wb_scb_entry_dcache_wb_data = UInt(DataBus bits)
  val wb_scb_entry_dcache_rd_en = Bool()
  val wb_scb_entry_dcache_rd_addr = UInt(DataAddrBus bits)
  val wb_scb_entry_dcache_rd_data = UInt(DataBus bits)
  val wb_scb_entry_instr = UInt(InstBus bits)
  val wb_scb_entry_pc = UInt(InstAddrBus bits)
  val wb_scb_entry_trans_id = UInt(SCB_ID_WIDTH bits)
  val wb_scb_entry_dcache_wb_sel = Bits(4 bits)
  val wb_scb_entry_branch_cor = Bool()
  val wb_scb_entry_call_cor = Bool()
  val wb_scb_entry_ret_cor = Bool()
  val wb_scb_entry_target_pc = UInt(InstAddrBus bits)

  val hindex = io.head_ptr
  when(hindex === index_alu){
    wb_scb_entry_instr          := io.alu_ex_wb_entry.instr
    wb_scb_entry_pc             := io.alu_ex_wb_entry.pc
    wb_scb_entry_reg_wb_en      := io.alu_ex_wb_entry.reg_wb_en
    wb_scb_entry_reg_wb_addr    := io.alu_ex_wb_entry.reg_wb_addr
    wb_scb_entry_reg_wb_data    := io.alu_ex_wb_entry.reg_wb_data
    wb_scb_entry_trans_id       := io.alu_ex_wb_entry.trans_id
    wb_scb_entry_dcache_rd_en   := io.alu_ex_wb_entry.dcache_rd_en
    wb_scb_entry_dcache_rd_addr := io.alu_ex_wb_entry.dcache_rd_addr
    wb_scb_entry_dcache_wb_en   := io.alu_ex_wb_entry.dcache_wb_en
    wb_scb_entry_dcache_wb_sel  := io.alu_ex_wb_entry.dcache_wb_sel
    wb_scb_entry_dcache_wb_addr := io.alu_ex_wb_entry.dcache_wb_addr
    wb_scb_entry_dcache_wb_data := io.alu_ex_wb_entry.dcache_wb_data
    wb_scb_entry_branch_cor     := io.alu_ex_wb_entry.branch_cor
    wb_scb_entry_call_cor       := io.alu_ex_wb_entry.call_cor
    wb_scb_entry_ret_cor        := io.alu_ex_wb_entry.ret_cor
    wb_scb_entry_target_pc      := io.alu_ex_wb_entry.target_pc
  } .elsewhen(hindex === index_mul1){
      wb_scb_entry_instr          := io.mul1_ex_wb_entry.instr
      wb_scb_entry_pc             := io.mul1_ex_wb_entry.pc
      wb_scb_entry_reg_wb_en      := io.mul1_ex_wb_entry.reg_wb_en
      wb_scb_entry_reg_wb_addr    := io.mul1_ex_wb_entry.reg_wb_addr
      wb_scb_entry_reg_wb_data    := io.mul1_ex_wb_entry.reg_wb_data
      wb_scb_entry_trans_id       := io.mul1_ex_wb_entry.trans_id
      wb_scb_entry_dcache_rd_en   := io.mul1_ex_wb_entry.dcache_rd_en
      wb_scb_entry_dcache_rd_addr := io.mul1_ex_wb_entry.dcache_rd_addr
      wb_scb_entry_dcache_wb_en   := io.mul1_ex_wb_entry.dcache_wb_en
      wb_scb_entry_dcache_wb_sel  := io.mul1_ex_wb_entry.dcache_wb_sel
      wb_scb_entry_dcache_wb_addr := io.mul1_ex_wb_entry.dcache_wb_addr
      wb_scb_entry_dcache_wb_data := io.mul1_ex_wb_entry.dcache_wb_data
      wb_scb_entry_branch_cor     := io.mul1_ex_wb_entry.branch_cor
      wb_scb_entry_call_cor       := io.mul1_ex_wb_entry.call_cor
      wb_scb_entry_ret_cor        := io.mul1_ex_wb_entry.ret_cor
      wb_scb_entry_target_pc      := io.mul1_ex_wb_entry.target_pc
    } .elsewhen(hindex === index_mul2){
      wb_scb_entry_instr          := io.mul2_ex_wb_entry.instr
      wb_scb_entry_pc             := io.mul2_ex_wb_entry.pc
      wb_scb_entry_reg_wb_en      := io.mul2_ex_wb_entry.reg_wb_en
      wb_scb_entry_reg_wb_addr    := io.mul2_ex_wb_entry.reg_wb_addr
      wb_scb_entry_reg_wb_data    := io.mul2_ex_wb_entry.reg_wb_data
      wb_scb_entry_trans_id       := io.mul2_ex_wb_entry.trans_id
      wb_scb_entry_dcache_rd_en   := io.mul2_ex_wb_entry.dcache_rd_en
      wb_scb_entry_dcache_rd_addr := io.mul2_ex_wb_entry.dcache_rd_addr
      wb_scb_entry_dcache_wb_en   := io.mul2_ex_wb_entry.dcache_wb_en
      wb_scb_entry_dcache_wb_sel  := io.mul2_ex_wb_entry.dcache_wb_sel
      wb_scb_entry_dcache_wb_addr := io.mul2_ex_wb_entry.dcache_wb_addr
      wb_scb_entry_dcache_wb_data := io.mul2_ex_wb_entry.dcache_wb_data
      wb_scb_entry_branch_cor     := io.mul2_ex_wb_entry.branch_cor
      wb_scb_entry_call_cor       := io.mul2_ex_wb_entry.call_cor
      wb_scb_entry_ret_cor        := io.mul2_ex_wb_entry.ret_cor
      wb_scb_entry_target_pc      := io.mul2_ex_wb_entry.target_pc
    } .elsewhen(hindex === index_divu){
      wb_scb_entry_instr          := io.divu_ex_wb_entry.instr
      wb_scb_entry_pc             := io.divu_ex_wb_entry.pc
      wb_scb_entry_reg_wb_en      := io.divu_ex_wb_entry.reg_wb_en
      wb_scb_entry_reg_wb_addr    := io.divu_ex_wb_entry.reg_wb_addr
      wb_scb_entry_reg_wb_data    := io.divu_ex_wb_entry.reg_wb_data
      wb_scb_entry_trans_id       := io.divu_ex_wb_entry.trans_id
      wb_scb_entry_dcache_rd_en   := io.divu_ex_wb_entry.dcache_rd_en
      wb_scb_entry_dcache_rd_addr := io.divu_ex_wb_entry.dcache_rd_addr
      wb_scb_entry_dcache_wb_en   := io.divu_ex_wb_entry.dcache_wb_en
      wb_scb_entry_dcache_wb_sel  := io.divu_ex_wb_entry.dcache_wb_sel
      wb_scb_entry_dcache_wb_addr := io.divu_ex_wb_entry.dcache_wb_addr
      wb_scb_entry_dcache_wb_data := io.divu_ex_wb_entry.dcache_wb_data
      wb_scb_entry_branch_cor     := io.divu_ex_wb_entry.branch_cor
      wb_scb_entry_call_cor       := io.divu_ex_wb_entry.call_cor
      wb_scb_entry_ret_cor        := io.divu_ex_wb_entry.ret_cor
      wb_scb_entry_target_pc      := io.divu_ex_wb_entry.target_pc
    } .elsewhen(hindex === index_bju){
      wb_scb_entry_instr          := io.bju_ex_wb_entry.instr
      wb_scb_entry_pc             := io.bju_ex_wb_entry.pc
      wb_scb_entry_reg_wb_en      := io.bju_ex_wb_entry.reg_wb_en
      wb_scb_entry_reg_wb_addr    := io.bju_ex_wb_entry.reg_wb_addr
      wb_scb_entry_reg_wb_data    := io.bju_ex_wb_entry.reg_wb_data
      wb_scb_entry_trans_id       := io.bju_ex_wb_entry.trans_id
      wb_scb_entry_dcache_rd_en   := io.bju_ex_wb_entry.dcache_rd_en
      wb_scb_entry_dcache_rd_addr := io.bju_ex_wb_entry.dcache_rd_addr
      wb_scb_entry_dcache_wb_en   := io.bju_ex_wb_entry.dcache_wb_en
      wb_scb_entry_dcache_wb_sel  := io.bju_ex_wb_entry.dcache_wb_sel
      wb_scb_entry_dcache_wb_addr := io.bju_ex_wb_entry.dcache_wb_addr
      wb_scb_entry_dcache_wb_data := io.bju_ex_wb_entry.dcache_wb_data
      wb_scb_entry_branch_cor     := io.bju_ex_wb_entry.branch_cor
      wb_scb_entry_call_cor       := io.bju_ex_wb_entry.call_cor
      wb_scb_entry_ret_cor        := io.bju_ex_wb_entry.ret_cor
      wb_scb_entry_target_pc      := io.bju_ex_wb_entry.target_pc
    } .elsewhen(hindex === index_lsu){
      wb_scb_entry_instr          := io.lsu_ex_wb_entry.instr
      wb_scb_entry_pc             := io.lsu_ex_wb_entry.pc
      wb_scb_entry_reg_wb_en      := io.lsu_ex_wb_entry.reg_wb_en
      wb_scb_entry_reg_wb_addr    := io.lsu_ex_wb_entry.reg_wb_addr
      wb_scb_entry_reg_wb_data    := io.lsu_ex_wb_entry.reg_wb_data
      wb_scb_entry_trans_id       := io.lsu_ex_wb_entry.trans_id
      wb_scb_entry_dcache_rd_en   := io.lsu_ex_wb_entry.dcache_rd_en
      wb_scb_entry_dcache_rd_addr := io.lsu_ex_wb_entry.dcache_rd_addr
      wb_scb_entry_dcache_wb_en   := io.lsu_ex_wb_entry.dcache_wb_en
      wb_scb_entry_dcache_wb_sel  := io.lsu_ex_wb_entry.dcache_wb_sel
      wb_scb_entry_dcache_wb_addr := io.lsu_ex_wb_entry.dcache_wb_addr
      wb_scb_entry_dcache_wb_data := io.lsu_ex_wb_entry.dcache_wb_data
      wb_scb_entry_branch_cor     := io.lsu_ex_wb_entry.branch_cor
      wb_scb_entry_call_cor       := io.lsu_ex_wb_entry.call_cor
      wb_scb_entry_ret_cor        := io.lsu_ex_wb_entry.ret_cor
      wb_scb_entry_target_pc      := io.lsu_ex_wb_entry.target_pc
    } .elsewhen(hindex === index_mul1){
      wb_scb_entry_instr          := io.csr_ex_wb_entry.instr
      wb_scb_entry_pc             := io.csr_ex_wb_entry.pc
      wb_scb_entry_reg_wb_en      := io.csr_ex_wb_entry.reg_wb_en
      wb_scb_entry_reg_wb_addr    := io.csr_ex_wb_entry.reg_wb_addr
      wb_scb_entry_reg_wb_data    := io.csr_ex_wb_entry.reg_wb_data
      wb_scb_entry_trans_id       := io.csr_ex_wb_entry.trans_id
      wb_scb_entry_dcache_rd_en   := io.csr_ex_wb_entry.dcache_rd_en
      wb_scb_entry_dcache_rd_addr := io.csr_ex_wb_entry.dcache_rd_addr
      wb_scb_entry_dcache_wb_en   := io.csr_ex_wb_entry.dcache_wb_en
      wb_scb_entry_dcache_wb_sel  := io.csr_ex_wb_entry.dcache_wb_sel
      wb_scb_entry_dcache_wb_addr := io.csr_ex_wb_entry.dcache_wb_addr
      wb_scb_entry_dcache_wb_data := io.csr_ex_wb_entry.dcache_wb_data
      wb_scb_entry_branch_cor     := io.csr_ex_wb_entry.branch_cor
      wb_scb_entry_call_cor       := io.csr_ex_wb_entry.call_cor
      wb_scb_entry_ret_cor        := io.csr_ex_wb_entry.ret_cor
      wb_scb_entry_target_pc      := io.csr_ex_wb_entry.target_pc
    } .elsewhen(hindex === index_mul1){
      wb_scb_entry_instr          := io.nopu_ex_wb_entry.instr
      wb_scb_entry_pc             := io.nopu_ex_wb_entry.pc
      wb_scb_entry_reg_wb_en      := io.nopu_ex_wb_entry.reg_wb_en
      wb_scb_entry_reg_wb_addr    := io.nopu_ex_wb_entry.reg_wb_addr
      wb_scb_entry_reg_wb_data    := io.nopu_ex_wb_entry.reg_wb_data
      wb_scb_entry_trans_id       := io.nopu_ex_wb_entry.trans_id
      wb_scb_entry_dcache_rd_en   := io.nopu_ex_wb_entry.dcache_rd_en
      wb_scb_entry_dcache_rd_addr := io.nopu_ex_wb_entry.dcache_rd_addr
      wb_scb_entry_dcache_wb_en   := io.nopu_ex_wb_entry.dcache_wb_en
      wb_scb_entry_dcache_wb_sel  := io.nopu_ex_wb_entry.dcache_wb_sel
      wb_scb_entry_dcache_wb_addr := io.nopu_ex_wb_entry.dcache_wb_addr
      wb_scb_entry_dcache_wb_data := io.nopu_ex_wb_entry.dcache_wb_data
      wb_scb_entry_branch_cor     := io.nopu_ex_wb_entry.branch_cor
      wb_scb_entry_call_cor       := io.nopu_ex_wb_entry.call_cor
      wb_scb_entry_ret_cor        := io.nopu_ex_wb_entry.ret_cor
      wb_scb_entry_target_pc      := io.nopu_ex_wb_entry.target_pc
    }.otherwise{
      wb_scb_entry_instr := INSTR_TAB(hindex)
      wb_scb_entry_pc := PC_TAB(hindex)
      wb_scb_entry_reg_wb_en := REG_WEN_TAB(hindex)
      wb_scb_entry_reg_wb_addr := REG_WADDR_TAB(hindex)
      wb_scb_entry_reg_wb_data := REG_WDATA_TAB(hindex)
      wb_scb_entry_trans_id := U"0"@@hindex
      wb_scb_entry_dcache_rd_en := DCACHE_REN_TAB(hindex)
      wb_scb_entry_dcache_rd_addr := DCACHE_RADDR_TAB(hindex)
      //io.ex_wb_entry.dcache_rd_data := wb_scb_entry_dcache_rd_data
      wb_scb_entry_dcache_wb_en := DCACHE_WEN_TAB(hindex)
      wb_scb_entry_dcache_wb_sel := DCACHE_WSEL_TAB(hindex)
      wb_scb_entry_dcache_wb_addr := DCACHE_WADDR_TAB(hindex)
      wb_scb_entry_dcache_wb_data := DCACHE_WDATA_TAB(hindex)
      wb_scb_entry_branch_cor := BRANCH_COR_TAB(hindex)
      wb_scb_entry_call_cor := CALL_COR_TAB(hindex)
      wb_scb_entry_ret_cor  := RET_COR_TAB(hindex)
      wb_scb_entry_target_pc  := TARGET_PC_TAB(hindex)
  }

  io.wb_scb_entry.instr := wb_scb_entry_instr
  io.wb_scb_entry.pc := wb_scb_entry_pc
  io.wb_scb_entry.reg_wb_en := wb_scb_entry_reg_wb_en
  io.wb_scb_entry.reg_wb_addr := wb_scb_entry_reg_wb_addr
  io.wb_scb_entry.reg_wb_data := wb_scb_entry_reg_wb_data
  io.wb_scb_entry.trans_id := wb_scb_entry_trans_id
  io.wb_scb_entry.dcache_rd_en :=wb_scb_entry_dcache_rd_en
  io.wb_scb_entry.dcache_rd_addr := wb_scb_entry_dcache_rd_addr
  //io.ex_wb_entry.dcache_rd_data := io.wb_scb_entry.dcache_rd_data
  io.wb_scb_entry.dcache_wb_en := wb_scb_entry_dcache_wb_en
  io.wb_scb_entry.dcache_wb_sel := wb_scb_entry_dcache_wb_sel
  io.wb_scb_entry.dcache_wb_addr := wb_scb_entry_dcache_wb_addr
  io.wb_scb_entry.dcache_wb_data := wb_scb_entry_dcache_wb_data
  io.wb_scb_entry.branch_cor := wb_scb_entry_branch_cor
  io.wb_scb_entry.call_cor := wb_scb_entry_call_cor
  io.wb_scb_entry.ret_cor := wb_scb_entry_ret_cor
  io.wb_scb_entry.target_pc := wb_scb_entry_target_pc

  io.alu_ex_wb_entry.commit_ack := alu_commit_req_ack
  io.alu_ex_wb_entry.recv_id    := alu_recv_id
  io.mul1_ex_wb_entry.commit_ack := mul1_commit_req_ack
  io.mul1_ex_wb_entry.recv_id    := mul1_recv_id
  io.mul2_ex_wb_entry.commit_ack := mul2_commit_req_ack
  io.mul2_ex_wb_entry.recv_id    := mul2_recv_id
  io.divu_ex_wb_entry.commit_ack := divu_commit_req_ack
  io.divu_ex_wb_entry.recv_id    := divu_recv_id
  io.bju_ex_wb_entry.commit_ack := bju_commit_req_ack
  io.bju_ex_wb_entry.recv_id    := bju_recv_id
  io.lsu_ex_wb_entry.commit_ack := lsu_commit_req_ack
  io.lsu_ex_wb_entry.recv_id    := lsu_recv_id
  io.csr_ex_wb_entry.commit_ack := csr_commit_req_ack
  io.csr_ex_wb_entry.recv_id    := csr_recv_id
  io.nopu_ex_wb_entry.commit_ack := nop_commit_req_ack
  io.nopu_ex_wb_entry.recv_id    := nop_recv_id

}
*/

/*
class alu_unit extends Component with Global_parameter with Interface_MS {
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
            alu_res_shift := rs1 |<< rs2
          }
          is(SRL) {
            alu_res_shift := rs1 |>> rs2
          }
          is(SLLI) {
            alu_res_shift := rs1 |<< shamt
          }
          is(SRLI) {
            alu_res_shift := rs1 |>> shamt
          }
          is(SRA) {
            alu_res_shift := rs1 >> rs2
          }
          is(SRAI) {
            alu_res_shift := rs1 >> shamt
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
            alu_res_shift := U(imm) |<<12 + pc
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
*/

class mul_unit extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val dec_entry = slave(decorder_entry(CoreConfig())) // from issue stage
    val ex_operand_entry = slave(operand_entry(CoreConfig()))  // from issue
    //val mul_res = Bits(RegDataBus bits) // mul to commit
    val mul_ex_entry = master(mul_res_entry(CoreConfig()))  // to commit
  }
  // todo 不急
}

class div_unit extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val dec_entry = slave(decorder_entry(CoreConfig())) // from issue stage
    val ex_operand_entry = slave(operand_entry(CoreConfig()))  // from issue
    //val div_res = Bits(RegDataBus bits) // mul to commit
    val div_ex_entry = master(div_res_entry(CoreConfig()))  // to commit
  }
  // todo 不急
}

/*
class bju_unit extends Component with Global_parameter with Interface_MS {
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

  io.bju_branch_predict.setAsReg()
  io.bju_branch_predict.branch_valid := io.ex_branch_predict.branch_valid
  io.bju_branch_predict.branch_taken := branch_taken
  io.bju_branch_predict.branch_target := target_pc
  io.bju_branch_predict.is_branch := io.ex_branch_predict.is_branch
  io.bju_branch_predict.is_call := io.ex_branch_predict.is_call
  io.bju_branch_predict.is_ret := io.ex_branch_predict.is_ret
  io.bju_branch_predict.pc := io.ex_branch_predict.pc

}
*/

/*
class lsu_unit extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val dec_entry = slave(decorder_entry(CoreConfig())) // from issue stage
    val ex_operand_entry = slave(operand_entry(CoreConfig()))  // from issue
    val lsu_ex_entry = master(lsu_res_entry(CoreConfig()))  // to commit
    // todo with lsu //
    val lsu_ack = out Bool()  // to scb
  }
  val load_raddr = Reg(UInt(DataAddrBus bits)) init(0)
  val load_rden = Reg(Bool()) init(False)
  val load_byte = Reg(Bits(4 bits)) init(B"1111")
  val load_sign = Reg(Bool()) init(False)
  val load_result = Reg(UInt(DataBus bits)) init(0)
  val store_waddr = Reg(UInt(DataAddrBus bits)) init(0)
  val store_wten = Reg(Bool()) init(False)
  val store_byte = Reg(Bits(4 bits)) init(B"1111")
  val store_wdata = Reg(UInt(DataBus bits)) init(0)
  val ex_operand_entry_instr = Reg(UInt(InstBus bits)) init(0)
  val ex_operand_entry_trans_id = Reg(UInt(SCB_ID_WIDTH bits)) init(SCB_IU_DEEPTH)
  val ex_operand_entry_pc = Reg(UInt(InstAddrBus bits)) init(0)

  val rs1_data = io.ex_operand_entry.rs1_data
  val rs2_data = io.ex_operand_entry.rs2_data
  val imm = io.ex_operand_entry.imm
  val rd_addr = io.ex_operand_entry.rd_addr

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


  io.lsu_ex_entry.store_wb_en := store_wten
  io.lsu_ex_entry.store_wb_addr := store_waddr
  io.lsu_ex_entry.store_wb_data := store_wdata
  io.lsu_ex_entry.store_wb_byte := store_byte

  io.lsu_ex_entry.instr := ex_operand_entry_instr
  io.lsu_ex_entry.trans_id := ex_operand_entry_trans_id
  io.lsu_ex_entry.pc := ex_operand_entry_pc

  io.lsu_ex_entry.load_rd_en := load_rden   // to dcache
  io.lsu_ex_entry.load_rd_addr := load_raddr  // to dcache
  io.lsu_ex_entry.load_rd_byte := load_byte
  io.lsu_ex_entry.result := load_result // from dcache

  when(io.ex_operand_entry.dec_valid){
    io.ex_operand_entry.busy := True
    ex_operand_entry_instr := io.ex_operand_entry.instr
    ex_operand_entry_trans_id := io.ex_operand_entry.trans_id
    ex_operand_entry_pc := io.ex_operand_entry.pc
  } . otherwise{
    io.ex_operand_entry.busy := False
  }

  when(io.ex_operand_entry.busy){
    switch(io.ex_operand_entry.instr){
      is(LB){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"0001"
        load_sign := True
        load_result := U(DataBus bits,default->io.lsu_ex_entry.load_rd_data(DataBus-1),(7 downto 0)->io.lsu_ex_entry.load_rd_data(7 downto 0))
      }
      is(LH){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"0011"
        load_sign := True
        load_result := U(DataBus bits,default->io.lsu_ex_entry.load_rd_data(DataBus-1),(15 downto 0)->io.lsu_ex_entry.load_rd_data(15 downto 0))
      }
      is(LW){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"1111"
        load_sign := True
        load_result := io.lsu_ex_entry.load_rd_data
      }
      is(LBU){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"0001"
        load_sign := False
        load_result := io.lsu_ex_entry.load_rd_data(7 downto 0).resized
      }
      is(LHU){
        load_raddr := rs1_data+U(imm)
        load_rden := True
        load_byte := B"1111"
        load_sign := False
        load_result := io.lsu_ex_entry.load_rd_data(15 downto 0).resized
      }
      is(SB){
        store_waddr := rs1_data+U(imm)
        store_wten := True
        store_byte := B"0001"
        store_wdata := rs2_data
      }
      is(SH){
        store_waddr := rs1_data+U(imm)
        store_wten := True
        store_byte := B"0011"
        store_wdata := rs2_data
      }
      is(SW){
        store_waddr := rs1_data+U(imm)
        store_wten := True
        store_byte := B"1111"
        store_wdata := rs2_data
      }
      default{
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
*/

/*
class csr_unit extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    //val dec_entry = slave(decorder_entry(CoreConfig())) // from issue stage
    val ex_operand_entry = slave(operand_entry(CoreConfig()))  // from issue
    //val csr_res = Bits(RegDataBus bits) // mul to commit
    val csr_ex_entry = master(csr_res_entry(CoreConfig()))  // to commit
  }
  // todo csr buffer
}
*/

class regfile extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val write_interface = slave(wregfile_interface(CoreConfig()))  // from commit stage
    //val readop_ctrl = in Vec(Bits(2 bit),REG_NUM) // from scb
    val readop_entry = out Vec(UInt(RegDataBus bits),REG_NUM) // to scb
    //val regfile_req = in Bool()
    //val regfile_ack = out Bool()
  }
  val REG_FILE = Vec(Reg(UInt(RegDataBus bits)) init(0),REG_NUM)
  io.readop_entry := REG_FILE
  when(io.write_interface.reg_wen){
    REG_FILE(io.write_interface.reg_waddr) := io.write_interface.reg_wdata
  } .otherwise{ }
}

class regfile_wb extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val write_interface = slave(wregfile_interface(CoreConfig()))  // from commit stage
    //val readop_ctrl = in Vec(Bits(2 bit),REG_NUM) // from scb
    val readop_entry = out Vec(UInt(RegDataBus bits),REG_NUM) // to scb
    //val regfile_req = in Bool()
    //val regfile_ack = out Bool()
  }
  val REG_FILE = Vec(Reg(UInt(RegDataBus bits)) init(0),REG_NUM)
  io.readop_entry := REG_FILE
  when(io.write_interface.reg_wen){
    REG_FILE(io.write_interface.reg_waddr) := io.write_interface.reg_wdata
  } .otherwise{ }
}

class csr_regfile extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val write_interface = slave(wcsr_interface(CoreConfig())) // from commit stage
    val read_addr = in UInt(CSRAddrBus bits)  // from scb,与csr读指令相关
    val read_data = out UInt(CSRDataBus bits) // to scb,与csr读指令相关
    //val readop_entry = out Vec(UInt(CSRDataBus bits),CSR_NUM) // to scb
    val csr_exc_entry = slave(csr_except_entry(CoreConfig()))  // 发生异常时，同时更新异常相关的特殊csr寄存器
    val csr_exc_readout = master(csr_except_entry(CoreConfig()))  // handle异常时，需要读出异常相关特殊csr寄存器
  }
  // todo
  val CSR_FILE = Vec(Reg(UInt(CSRDataBus bits)) init(0),CSR_NUM)
  //io.readop_entry := CSR_FILE
  io.read_data := CSR_FILE(io.read_addr)
  when(io.write_interface.reg_wen){
    CSR_FILE(io.write_interface.reg_waddr) := io.write_interface.reg_wdata
  } .otherwise{ }
  when(io.csr_exc_entry.csr_except_wen === True){
    // todo 恢复
    CSR_FILE(CSR.MCAUSE)  := io.csr_exc_entry.mcause
    CSR_FILE(CSR.MEPC)    := io.csr_exc_entry.mepc
    CSR_FILE(CSR.MTVAL)   := io.csr_exc_entry.mtval   // 老版本叫MBADADDR，新版本叫MTVAL，S和U也一样，找def的时候需要注意 //
    CSR_FILE(CSR.MSTATUS) := io.csr_exc_entry.mstatus
  } .otherwise{ }
  io.csr_exc_readout.mcause := CSR_FILE(CSR.MCAUSE)
  io.csr_exc_readout.mepc := CSR_FILE(CSR.MEPC)
  io.csr_exc_readout.mtval := CSR_FILE(CSR.MTVAL)
  io.csr_exc_readout.mstatus := CSR_FILE(CSR.MSTATUS)
  io.csr_exc_readout.mtvec := CSR_FILE(CSR.MTVEC)
}

class csr_regfile_wb extends Component with Global_parameter with Interface_MS {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val write_interface = slave(wcsr_interface(CoreConfig())) // from commit stage
    val read_addr = in UInt(CSRAddrBus bits)
    val read_data = out UInt(CSRDataBus bits)
    //val readop_entry = out Vec(UInt(CSRDataBus bits),CSR_NUM) // to scb
    val csr_exc_entry = slave(csr_except_entry(CoreConfig()))  // 发生异常时，同时更新异常相关的特殊csr寄存器
  }
  // todo with except handle
  val CSR_FILE = Vec(Reg(UInt(CSRDataBus bits)) init(0),CSR_NUM)
  //io.readop_entry := CSR_FILE
  io.read_data := CSR_FILE(io.read_addr)
  when(io.write_interface.reg_wen){
    CSR_FILE(io.write_interface.reg_waddr) := io.write_interface.reg_wdata
  } .otherwise{ }
  when(io.csr_exc_entry.csr_except_wen === True){
    // todo 恢复
    CSR_FILE(CSR.MCAUSE)  := io.csr_exc_entry.mcause
    CSR_FILE(CSR.MEPC)    := io.csr_exc_entry.mepc
    CSR_FILE(CSR.MTVAL)   := io.csr_exc_entry.mtval   // 老版本叫MBADADDR，新版本叫MTVAL，S和U也一样，找def的时候需要注意 //
    CSR_FILE(CSR.MSTATUS) := io.csr_exc_entry.mstatus
  } .otherwise{ }
}

class cutecore_logic extends Component with Global_parameter with Interface_MS{
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
    val icache_rdy = in Bool()  // from top
    val icache_entry = master(icache_interface(CoreConfig())) // to top
    val dcache_rdy = in Bool()  // from top
    val dcache_entry = master(dcache_interface(CoreConfig())) // to top
  }
  // 实例化模块 //
  // pipeline-1 : pc_gen stage //
  val pc_gen = new pc_gen

  // pipeline-2 : if stage //
  val instr_realign = new instr_realign
  val instr_queue = new instr_queue
  val bht = new bht
  val btb = new btb
  val ras = new ras

  // pipeline-3 : id stage //
  val decorder = new decorder
  val id2issue = new id2issue

  // pipeline-4 : issue stage //
  val scoreboard = new scoreboard

  // pipeline-5 : ex stage //
  val alu_unit = new alu_unit
  val mul1_unit = new mul_unit
  val mul2_unit = new mul_unit
  val div_unit = new div_unit
  val bju_unit = new bju_unit
  val lsu_unit = new lsu_unit
  val csr_unit = new csr_unit
  val nop_unit = new nop_unit

  // pipeline-6 : commit stage //
  val wb = new wb
  val commit = new commit
  val regfile = new regfile
  val csr_regfile = new csr_regfile
  val regfile_wb = new regfile_wb
  val csr_regfile_wb = new csr_regfile_wb
  val exc_arbit = new exc_arbit


  // 建立连接关系 //
  pc_gen.io.clk := io.clk
  pc_gen.io.rstn := io.rstn
  pc_gen.io.flush := commit.io.flush
  pc_gen.io.flush_mis_predict := commit.io.flush_mis_predict
  pc_gen.io.flush_mis_predict_target_pc := commit.io.flush_mis_predict_target_pc
  pc_gen.io.flush_except := commit.io.flush_except // todo
  pc_gen.io.trap_entry := csr_regfile.io.csr_exc_readout.mtvec // todo
  //pc_gen.io.epc := 0 // todo
  //pc_gen.io.csr_epc1 := 0 // todo
  //pc_gen.io.csr_epc2 := 0 // todo
  //pc_gen.io.icache_rdy := io.icache_rdy
  pc_gen.io.icache_rdy := ~instr_queue.io.stall_push
  pc_gen.io.ras_target := ras.io.ras_target
  pc_gen.io.predict_btb_entry connect btb.io.predict_btb_entry
  pc_gen.io.predict_bht_entry connect bht.io.predict_bht_entry
  pc_gen.io.if_branch_predict connect btb.io.if_branch_predict
  pc_gen.io.if_branch_predict connect bht.io.if_branch_predict
  pc_gen.io.if_branch_predict connect instr_queue.io.if_branch_predict_entry
  pc_gen.io.instr_realign connect instr_realign.io.instr_realign
  pc_gen.io.ex_branch_predict connect bju_unit.io.bju_mispredict
  pc_gen.io.bju_branch_predict connect bju_unit.io.bju_branch_predict
  pc_gen.io.resolved_bht_entry connect bht.io.resolved_bht_entry
  pc_gen.io.resolved_btb_entry connect btb.io.resolved_btb_entry
  ras.io.mispredict_entry connect bju_unit.io.bju_mispredict
  btb.io.pc_valid := pc_gen.io.pc_valid
  bht.io.pc_valid := pc_gen.io.pc_valid

  //instr_realign.io.icache_entry connect mmu.io.icache_entry // todo with MMU
  instr_realign.io.icache_entry connect io.icache_entry
  //instr_realign.io.icache_rdy := io.icache_rdy
  instr_realign.io.icache_rdy := ~instr_queue.io.stall_push
  instr_realign.io.pc := pc_gen.io.pc
  btb.io.clk := io.clk
  btb.io.rstn := io.rstn
  bht.io.clk := io.clk
  bht.io.rstn := io.rstn
  ras.io.clk := io.clk
  ras.io.rstn := io.rstn
  instr_realign.io.clk := io.clk
  instr_realign.io.rstn := io.rstn
  instr_queue.io.clk := io.clk
  instr_queue.io.rstn := io.rstn
  instr_queue.io.flush := commit.io.flush
  decorder.io.clk := io.clk
  decorder.io.rstn := io.rstn
  id2issue.io.clk := io.clk
  id2issue.io.rstn := io.rstn
  id2issue.io.flush := commit.io.flush // todo
  scoreboard.io.clk := io.clk
  scoreboard.io.rstn := io.rstn
  scoreboard.io.flush := commit.io.flush
  alu_unit.io.clk := io.clk
  alu_unit.io.rstn := io.rstn
  mul1_unit.io.clk := io.clk
  mul1_unit.io.rstn := io.rstn
  mul2_unit.io.clk := io.clk
  mul2_unit.io.rstn := io.rstn
  div_unit.io.clk := io.clk
  div_unit.io.rstn := io.rstn
  bju_unit.io.clk := io.clk
  bju_unit.io.rstn := io.rstn
  lsu_unit.io.clk := io.clk
  lsu_unit.io.rstn := io.rstn
  csr_unit.io.clk := io.clk
  csr_unit.io.rstn := io.rstn
  nop_unit.io.clk := io.clk
  nop_unit.io.rstn := io.rstn
  wb.io.clk := io.clk
  wb.io.rstn := io.rstn
  commit.io.clk := io.clk
  commit.io.rstn := io.rstn
  exc_arbit.io.clk := io.clk
  exc_arbit.io.rstn := io.rstn

  //instr_queue.io.if_intr_entry connect instr_realign.io.instr_realign
  instr_queue.io.if_intr_entry.pc := pc_gen.io.pc
  instr_queue.io.if_intr_entry.valid := instr_realign.io.instr_realign.valid
  instr_queue.io.if_intr_entry.inst := instr_realign.io.instr_realign.inst
  instr_queue.io.if2id_instr_entry connect decorder.io.id_instr_entry
  instr_queue.io.if2id_branch_predict_entry connect id2issue.io.id_branch_predict_entry
  instr_queue.io.stall_pop := scoreboard.io.scb_full
  decorder.io.id_dec_entry connect id2issue.io.id_dec_entry
  id2issue.io.icache_rdy := ~instr_queue.io.stall_pop
  // alu
  //id2issue.io.id2issue_dec_entry connect alu_unit.io.dec_entry
  scoreboard.io.alu_oprand_entry connect alu_unit.io.ex_operand_entry
  scoreboard.io.alu_ex_entry connect alu_unit.io.alu_ex_entry
  // mul1
  //id2issue.io.id2issue_dec_entry connect mul1_unit.io.dec_entry
  scoreboard.io.mul1_oprand_entry connect mul1_unit.io.ex_operand_entry
  scoreboard.io.mul1_ex_entry connect mul1_unit.io.mul_ex_entry
  // mul2
  //id2issue.io.id2issue_dec_entry connect mul2_unit.io.dec_entry
  scoreboard.io.mul2_oprand_entry connect mul2_unit.io.ex_operand_entry
  scoreboard.io.mul2_ex_entry connect mul2_unit.io.mul_ex_entry
  // div
  //id2issue.io.id2issue_dec_entry connect div_unit.io.dec_entry
  scoreboard.io.div_oprand_entry connect div_unit.io.ex_operand_entry
  scoreboard.io.div_ex_entry connect div_unit.io.div_ex_entry
  // todo with bju
  //id2issue.io.id2issue_dec_entry connect bju_unit.io.dec_entry
  scoreboard.io.bju_oprand_entry connect bju_unit.io.ex_operand_entry
  scoreboard.io.bju_ex_entry connect bju_unit.io.bju_ex_entry
  scoreboard.io.scb_branch_predict_entry connect bju_unit.io.ex_branch_predict
  scoreboard.io.issue_branch_predict_entry connect id2issue.io.id2issue_branch_predict_entry
  scoreboard.io.bju_mis_predict_entry connect bju_unit.io.bju_mispredict
  // todo with lsu
  //id2issue.io.id2issue_dec_entry connect lsu_unit.io.dec_entry
  scoreboard.io.lsu_oprand_entry connect lsu_unit.io.ex_operand_entry
  scoreboard.io.lsu_ex_entry connect lsu_unit.io.lsu_ex_entry
  // todo with csr
  //id2issue.io.id2issue_dec_entry connect csr_unit.io.dec_entry
  scoreboard.io.csr_oprand_entry connect csr_unit.io.ex_operand_entry
  scoreboard.io.csr_ex_entry connect csr_unit.io.csr_ex_entry
  // todo with nop
  scoreboard.io.nopu_oprand_entry connect nop_unit.io.ex_operand_entry
  scoreboard.io.nopu_ex_entry connect nop_unit.io.nopu_ex_entry

  scoreboard.io.alu_ex_wb_entry connect wb.io.alu_ex_wb_entry
  scoreboard.io.mul1_ex_wb_entry connect wb.io.mul1_ex_wb_entry
  scoreboard.io.mul2_ex_wb_entry connect wb.io.mul2_ex_wb_entry
  scoreboard.io.divu_ex_wb_entry connect wb.io.divu_ex_wb_entry
  scoreboard.io.bju_ex_wb_entry connect wb.io.bju_ex_wb_entry
  scoreboard.io.lsu_ex_wb_entry connect wb.io.lsu_ex_wb_entry
  scoreboard.io.csr_ex_wb_entry connect wb.io.csr_ex_wb_entry
  scoreboard.io.nopu_ex_wb_entry connect wb.io.nopu_ex_wb_entry
  scoreboard.io.issue_dec_entry connect id2issue.io.id2issue_dec_entry
  scoreboard.io.wb_commit_entry connect commit.io.ex_commit_entry
  scoreboard.io.scb_readop_i := regfile.io.readop_entry
  scoreboard.io.scb_readop_wb_i := regfile_wb.io.readop_entry
  scoreboard.io.csr_wb_read_data := csr_regfile_wb.io.read_data // todo
  csr_regfile_wb.io.read_addr := scoreboard.io.csr_wb_read_addr // todo
  scoreboard.io.wb_scb_entry connect wb.io.wb_scb_entry

  wb.io.wb_regfile_interface connect regfile_wb.io.write_interface
  //wb.io.wb_csr_interface connect csr_regfile_wb.io.write_interface
  wb.io.head_ptr := scoreboard.io.head_ptr

  commit.io.wb_regfile_interface connect regfile.io.write_interface
  commit.io.wb_csr_interface connect csr_regfile.io.write_interface // todo
  //commit.io.wb_dacahe_interfacec connect mmu.io.dcache_store_interface // todo with MMU
  commit.io.wb_dacahe_interfacec connect io.dcache_entry
  commit.io.bju_mis_predict connect bju_unit.io.bju_mispredict
  commit.io.exc_entry connect exc_arbit.io.exc_entry
  commit.io.csr_exc_entry connect csr_regfile.io.csr_exc_entry  // todo
  commit.io.csr_exc_entry connect csr_regfile_wb.io.csr_exc_entry  // todo
  commit.io.exc_commit_entry connect exc_arbit.io.exc_commit_entry
}

class cutecore extends Component {
  val io = new Bundle {
    val clk = in Bool()
    val rstn = in Bool()
  }
  // 实例化模块 //
  val cutecore_logic = new cutecore_logic
  val icache_inst = new icache
  val dcache_inst = new dcache

  // 建立连接关系 //
  cutecore_logic.io.clk := io.clk
  cutecore_logic.io.rstn := io.rstn
  cutecore_logic.io.icache_entry connect icache_inst.io.icache_entry
  cutecore_logic.io.dcache_entry connect dcache_inst.io.dcache_entry
  cutecore_logic.io.icache_rdy := icache_inst.io.icache_rdy
  cutecore_logic.io.dcache_rdy := dcache_inst.io.dcache_rdy

}

trait Interface_MS extends Global_parameter {
  // instr_entry interface
  case class instr_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val pc = UInt(InstAddrBus bits)
    val inst = UInt(InstBus bits)
    val valid = Bool()

    override def asMaster(): Unit = {
      out(pc, inst, valid)
    }
  }

  // bht_predict_entry interface
  case class bht_predict_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val pc = UInt(InstAddrBus bits)
    val bht_taken = Bool()
    val bht_valid = Bool()

    override def asMaster(): Unit = {
      out(pc, bht_taken, bht_valid)
    }
  }

  // btb_predict_entry interface
  case class btb_predict_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val pc = UInt(InstAddrBus bits)
    val btb_taken = Bool()
    val btb_target = UInt(InstAddrBus bits)
    val btb_valid = Bool()

    override def asMaster(): Unit = {
      out(pc, btb_taken, btb_target,btb_valid)
    }
  }

  // branch_predict_entry interface
  case class branch_predict_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val pc = UInt(InstAddrBus bits)
    val branch_taken = Bool()
    val branch_target = UInt(InstAddrBus bits)
    val branch_valid = Bool()
    val is_branch = Bool()
    val is_call = Bool()
    val is_ret = Bool()

    override def asMaster(): Unit = {
      out(pc, branch_taken, branch_target,branch_valid,is_branch,is_call,is_ret)
    }
  }

  // branch_mispredict_entry interface
  case class branch_mispredict_entry(config: CoreConfig) extends Bundle with IMasterSlave{
    val branch_cor = Bool()
    val call_cor = Bool()
    val ret_cor = Bool()
    val target_pc = UInt(InstAddrBus bits)

    override def asMaster(): Unit = {
      out(branch_cor, call_cor, ret_cor,target_pc)
    }
  }

  // decorder_entry interface
  case class decorder_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val pc = UInt(InstAddrBus bits)
    val alu_sel = ALU_UNIT_SEL()  // Enum
    val op_type = OP_TYPE()
    //val op = EXE_OP()
    val instr = UInt(InstBus bits)
    val rs1_entry = master(register_entry(CoreConfig()))
    val rs2_entry = master(register_entry(CoreConfig()))
    val rd_entry = master(register_entry(CoreConfig()))
    val csr_entry = master(csr_register_entry(CoreConfig()))
    val imm = SInt(ImmBus bits)
    val dec_valid = Bool()
    val predict_flag = Bool()

    override def asMaster(): Unit = {
      out(pc,alu_sel,op_type,instr,rs1_entry,rs2_entry,rd_entry,csr_entry,imm,dec_valid,predict_flag)
    }
  }

  // register_entry interface
  case class register_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val reg_addr = UInt(RegAddrBus bits)
    val reg_wten = Bool()
    val reg_rden = Bool()

    override def asMaster(): Unit = {
      out(reg_addr,reg_wten,reg_rden)
    }
  }

  // csr_register_entry interface
  case class csr_register_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val reg_addr = UInt(CSRAddrBus bits)
    val reg_wten = Bool()
    val reg_rden = Bool()

    override def asMaster(): Unit = {
      out(reg_addr,reg_wten,reg_rden)
    }
  }

  // regfile_interface interface
  case class regfile_interface(config: CoreConfig) extends Bundle with IMasterSlave {
    val reg_addr = UInt(RegAddrBus bits)
    val reg_wen = Bool()
    val reg_wdata = UInt(RegDataBus bits)
    val reg_rdata = UInt(RegDataBus bits)

    override def asMaster(): Unit = {
      out(reg_addr,reg_wen,reg_wdata)
      in(reg_rdata)
    }
  }
  // wregfile_interface interface
  case class wregfile_interface(config: CoreConfig) extends Bundle with IMasterSlave {
    val reg_waddr = UInt(RegAddrBus bits)
    val reg_wen = Bool()
    val reg_wdata = UInt(RegDataBus bits)

    override def asMaster(): Unit = {
      out(reg_waddr,reg_wen,reg_wdata)
    }
  }

  // wcsr_interface interface
  case class wcsr_interface(config: CoreConfig) extends Bundle with IMasterSlave {
    val reg_waddr = UInt(CSRAddrBus bits)
    val reg_wen = Bool()
    val reg_wdata = UInt(CSRDataBus bits)

    override def asMaster(): Unit = {
      out(reg_waddr,reg_wen,reg_wdata)
    }
  }

  // csr_except_entry interface
  case class csr_except_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val csr_except_wen = Bool()
    val mcause = UInt(CSRDataBus bits)
    val mepc = UInt(CSRDataBus bits)
    val mtval = UInt(CSRDataBus bits)
    val mstatus = UInt(CSRDataBus bits)
    val mtvec = UInt(CSRDataBus bits)
    // todo with other mode csr exception registers
    override def asMaster(): Unit = {
      out(csr_except_wen,mcause,mepc,mtval,mstatus,mtvec)
    }
  }

  // operand_entry interface
  case class operand_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val rs1_data = UInt(RegDataBus bits)
    val rs2_data = UInt(RegDataBus bits)
    val rd_addr = UInt(RegAddrBus bits)
    val rd_wten = Bool()
    val imm = SInt(ImmBus bits)
    val busy = Bool()
    val instr = UInt(InstBus bits)
    val op_type = OP_TYPE()
    val dec_valid = Bool()
    val trans_id = UInt(SCB_ID_WIDTH bits)
    val pc = UInt(InstAddrBus bits)

    override def asMaster(): Unit = {
      out(rs1_data,rs2_data,rd_addr,rd_wten,imm,instr,op_type,dec_valid,trans_id,pc)
      in(busy)
    }
  }

  // alu_res_entry interface
  case class alu_res_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val result = SInt(RegDataBus bits)
    val instr = UInt(InstBus bits)
    val trans_id = UInt(SCB_ID_WIDTH bits)
    val pc = UInt(InstAddrBus bits)

    override def asMaster(): Unit = {
      out(result,instr,trans_id,pc)
    }
  }

  // mul_res_entry interface
  case class mul_res_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val result = UInt(RegDataBus bits)
    val instr = UInt(InstBus bits)
    val trans_id = UInt(SCB_ID_WIDTH bits)
    val pc = UInt(InstAddrBus bits)

    override def asMaster(): Unit = {
      out(result,instr,trans_id,pc)
    }
  }

  // div_res_entry interface
  case class div_res_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val result = UInt(RegDataBus bits)
    val instr = UInt(InstBus bits)
    val trans_id = UInt(SCB_ID_WIDTH bits)
    val pc = UInt(InstAddrBus bits)

    override def asMaster(): Unit = {
      out(result,instr,trans_id,pc)
    }
  }

  // bju_res_entry interface
  case class bju_res_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val result = UInt(RegDataBus bits)
    val instr = UInt(InstBus bits)
    val trans_id = UInt(SCB_ID_WIDTH bits)
    val pc = UInt(InstAddrBus bits)

    override def asMaster(): Unit = {
      out(result,instr,trans_id,pc)
    }
  }

  // lsu_res_entry interface
  case class lsu_res_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val result = UInt(RegDataBus bits)  // load result to rd
    val instr = UInt(InstBus bits)
    val trans_id = UInt(SCB_ID_WIDTH bits)
    val store_wb_en = Bool()
    val store_wb_addr = UInt(DataAddrBus bits)
    val store_wb_data = UInt(DataBus bits)
    val store_wb_byte = Bits(4 bits)
    val load_rd_en = Bool()
    val load_rd_addr = UInt(DataAddrBus bits)
    val load_rd_data = UInt(DataBus bits)
    val load_rd_byte = Bits(4 bits)
    val pc = UInt(InstAddrBus bits)

    override def asMaster(): Unit = {
      out(result,instr,trans_id,store_wb_en,store_wb_addr,store_wb_data,store_wb_byte,load_rd_en,load_rd_addr,load_rd_byte,pc)
      in(load_rd_data)
    }
  }

  // csr_res_entry interface
  case class csr_res_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val result = UInt(RegDataBus bits)
    val instr = UInt(InstBus bits)
    val trans_id = UInt(SCB_ID_WIDTH bits)
    val pc = UInt(InstAddrBus bits)
    val result_csr = UInt(CSRDataBus bits)

    override def asMaster(): Unit = {
      out(result,instr,trans_id,pc,result_csr)
    }
  }

  // nop_res_entry interface
  case class nop_res_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val instr = UInt(InstBus bits)
    val trans_id = UInt(SCB_ID_WIDTH bits)
    val pc = UInt(InstAddrBus bits)

    override def asMaster(): Unit = {
      out(instr,trans_id,pc)
    }
  }

  // commit_entry interface
  case class commit_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val reg_wb_addr = UInt(RegAddrBus bits)
    val reg_wb_data = UInt(RegDataBus bits)
    val reg_wb_en = Bool()
    val csr_wb_addr = UInt(CSRAddrBus bits)
    val csr_wb_data = UInt(CSRDataBus bits)
    val csr_wb_en = Bool()
    val dcache_wb_en = Bool()
    val dcache_wb_addr = UInt(DataAddrBus bits)
    val dcache_wb_data = UInt(DataBus bits)
    val dcache_rd_en = Bool()
    val dcache_rd_addr = UInt(DataAddrBus bits)
    val dcache_rd_data = UInt(DataBus bits)
    val dcache_wb_sel = Bits(4 bits)
    val commit_req = Bool()
    val commit_ack = Bool()
    val instr = UInt(InstBus bits)
    val trans_id = UInt(SCB_ID_WIDTH bits)
    val recv_id = UInt(SCB_ID_WIDTH bits)
    val pc = UInt(InstAddrBus bits)
    val branch_cor = Bool()
    val call_cor = Bool()
    val ret_cor = Bool()
    val target_pc = UInt(InstAddrBus bits)
    val dec_valid = Bool()
    //val alusel = Bits(ALU_UNIT_SEL().getBitsWidth bits)

    override def asMaster(): Unit = {
      out(reg_wb_addr,reg_wb_data,reg_wb_en,csr_wb_addr,csr_wb_data,csr_wb_en,dcache_wb_en,dcache_wb_addr,dcache_wb_data,dcache_wb_sel,dcache_rd_en,dcache_rd_addr,commit_req,instr,trans_id,pc,branch_cor,call_cor,ret_cor,target_pc,dec_valid)
      in(commit_ack,recv_id,dcache_rd_data)
    }
  }

  // except_commit_entry interface
  case class except_commit_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val reg_wb_addr = UInt(RegAddrBus bits)
    val reg_wb_en = Bool()
    val csr_wb_addr = UInt(CSRAddrBus bits)
    val csr_wb_en = Bool()
    val dcache_wb_en = Bool()
    val dcache_wb_addr = UInt(DataAddrBus bits)
    val dcache_rd_en = Bool()
    val dcache_rd_addr = UInt(DataAddrBus bits)
    val commit_req = Bool()
    val instr = UInt(InstBus bits)
    val pc = UInt(InstAddrBus bits)
    val target_pc = UInt(InstAddrBus bits)
    val dec_valid = Bool()
    //val alusel = Bits(ALU_UNIT_SEL().getBitsWidth bits)

    override def asMaster(): Unit = {
      out(reg_wb_addr,reg_wb_en,csr_wb_addr,csr_wb_en,dcache_wb_en,dcache_wb_addr,dcache_rd_en,dcache_rd_addr,commit_req,instr,pc,target_pc,dec_valid)
    }
  }

  // except_entry interface
  case class except_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val exc_req = Bool()  // 异常标志
    val exc_cause = UInt(CSRDataBus bits)// 异常原因
    val exc_pc = UInt(CSRDataBus bits)  // 精确异常引发pc地址
    val exc_val = UInt(CSRDataBus bits)  // 非法指令编码值or非法访存地址 // 位宽todo

    override def asMaster(): Unit = {
      out(exc_req,exc_cause,exc_pc,exc_val)
    }
  }

  // int_entry interface
  case class int_entry(config: CoreConfig) extends Bundle with IMasterSlave {
    val int_req = Bool()  // 中断申请
    val int_source = UInt(CSRDataBus bits) // 中断源
    val int_clear = Bool()  // 清除中断

    override def asMaster(): Unit = {
      out(int_req,int_source)
      in(int_clear)
    }
  }

  // cache interface
  case class icache_interface(config: CoreConfig) extends Bundle with IMasterSlave {
    val addr = UInt(InstAddrBus bits)
    val data = UInt(InstBus bits)
    val valid = Bool()

    override def asMaster(): Unit = {
      out(addr, valid)
      in(data)
    }
  }

  // dcache interface
  case class dcache_interface(config: CoreConfig) extends Bundle with IMasterSlave {
    val waddr = UInt(DataAddrBus bits)
    val we = Bool()
    val wdata = UInt(DataBus bits)
    val raddr = UInt(DataAddrBus bits)
    val re = Bool()
    val rdata = UInt(DataBus bits)
    val sel = UInt(MemSelBus bits)

    override def asMaster(): Unit = {
      out(waddr,we,wdata,raddr,re,sel)
      in(rdata)
    }
  }
}

trait Global_parameter {
  // 全局参数
  val InstAddrBus = 32
  val InstBus = 32
  val DataAddrBus   = 32
  val DataBus       = 32
  val InstLen = 4
  val BTB_PAGE_NUM = 8
  val BTB_TARGET_NUM = 32
  val RegAddrBus = 5
  val RegDataBus = 32
  val ImmBus = 32
  val Instr_FIFO_DEEPTH = 16
  val RAS_STACK_DEEPTH = 8
  val RAS_PTR_WIDTH = 3
  val SCB_INSTR_DEEPTH = 16
  val SCB_INSTR_WIDTH = 4
  val SCB_IU_DEEPTH = 16
  val SCB_ID_WIDTH = 5
  val REG_NUM = 32
  val CSRAddrBus = 4  //12
  val CSRDataBus = 32
  val CSR_NUM = 16  //理论上最多可以支持2^12=4096个csr寄存器
  //val CSRValidAddrBus = 5 // 必须是$clog2(CSR_NUM)

  val InstMemNum = 128  // ROM实际大小：64KB
  val InstMemNumLog2 = 7  // ROM实际使用的地址宽度,即 2^16=65536，PC计数为32bits，实际上对于ROM储存区只用到了16bits即可
  val DataMemNum    = 32  // // 单个RAM实际大小：64KB
  val DataMemNumLog2  = 5  // 单个RAM实际使用的地址宽度,即 2^16=65536
  val ByteWidth       = 8
  val MemSelBus = 4 // mem的字节选择宽度（0000-1111）
  val MemSelZero = U"4'b0000"  // 内存字节的初始化选择为第0位
  val MemSelOne = U"4'b1111"  // 内存字节的初始化选择为无效的全1
  val Memsel_1 = U"4'b1000"  // 内存返回字的第一个字节（首字节）
  val Memsel_2 = U"4'b0100"
  val Memsel_3 = U"4'b0010"
  val Memsel_4 = U"4'b0001"
  val Memsel_1H = U"4'b1100"  // 第一个半字
  val Memsel_2H = U"4'b0011"
  val Memsel_maskL1 = U"4'b0111"  // SWL 一个字节空悬的的掩码
  val Memsel_maskL2 = U"4'b0011"
  val Memsel_maskL3 = U"4'b0001"
  val Memsel_maskR1 = U"4'b1110"  // SWR 一个字节空悬的的掩码
  val Memsel_maskR2 = U"4'b1100"
  val Memsel_maskR3 = U"4'b1000"

}

case class CoreConfig(){
  // TODO as Global Parameters //
}

//Generate the MyTopLevel's Verilog
object cuteriscv {
  def main(args: Array[String]) {
    SpinalVerilog(new cutecore)
  }
}
object CoreConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))
//Generate the MyTopLevel's Verilog using the above custom configuration.
object cuteriscvVerilog {
  def main(args: Array[String]) {
    CoreConfig
      .generateVerilog(new cutecore)  // module name
  }
}