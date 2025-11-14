package mainelib
import spinal.core._
import spinal.lib._

/** RVC (16b) -> 32b 等价指令的前端解压器
 * 输入：预译码32b指令（16b压缩码已零扩展，高16位为0；32b正常指令保持不变）
 * 输出：展开成32b基础编码，便于后端统一用 32b 掩码 M"..." 做匹配
 */
object RvcDecompressor {
  def apply(instrPre: Bits, xlen: Int = 32): Bits = {
    val out = Bits(32 bits)

    // 是否为压缩指令：低2位 != 0b11
    val isC   = instrPre(1 downto 0) =/= B"2'b11"
    val cin   = instrPre(15 downto 0)      // 原始16b
    val q     = cin(1 downto 0)            // quadrant
    val f3    = cin(15 downto 13)          // funct3

    // ------------------ 小工具 ------------------
    def UConst(v: BigInt, w: Int) = U(v, w bits)
    def BConst(v: String)         = B(v)

    // 有符号扩展：把 fromWidth 扩到 toWidth（UInt 语义）
    def sext(u: UInt, fromWidth: Int, toWidth: Int): UInt = {
      val sign = u(fromWidth-1)
      val upper = sign ? ~U(0, (toWidth - fromWidth) bits) | U(0, (toWidth - fromWidth) bits)
      (upper ## u.asBits).asUInt
    }

    // 压缩寄存器 r'(3b) → x8..x15 的 5b UInt
    def xr(rp: UInt): UInt = (U(8, 5 bits) + rp.resized).resized

    // ------ 构造 32b 指令（按 RISC-V 基础6种格式 Cat 出来）------
    def makeR(funct7: Bits, rs2: UInt, rs1: UInt, funct3: Bits, rd: UInt, opcode: Bits): Bits =
      Cat(funct7, rs2.asBits.resize(5), rs1.asBits.resize(5), funct3, rd.asBits.resize(5), opcode)

    def makeI(imm12: UInt, rs1: UInt, funct3: Bits, rd: UInt, opcode: Bits): Bits =
      Cat(imm12.asBits.resize(12), rs1.asBits.resize(5), funct3, rd.asBits.resize(5), opcode)

    def makeS(imm12: UInt, rs2: UInt, rs1: UInt, funct3: Bits, opcode: Bits): Bits = {
      val imm11_5 = imm12(11 downto 5)
      val imm4_0  = imm12(4 downto 0)
      Cat(imm11_5, rs2.asBits.resize(5), rs1.asBits.resize(5), funct3, imm4_0, opcode)
    }

    // B型：传入的是已 <<1、已符号扩展到13位的 offset（imm[12:0]，LSB=0）
    def makeB(imm13: UInt, rs2: UInt, rs1: UInt, funct3: Bits, opcode: Bits): Bits = {
      val b12   = imm13(12)
      val b10_5 = imm13(10 downto 5)
      val b4_1  = imm13(4 downto 1)
      val b11   = imm13(11)
      Cat(b12, b10_5, rs2.asBits.resize(5), rs1.asBits.resize(5), funct3, b4_1, b11, opcode)
    }

    // U型
    def makeU(imm20: UInt, rd: UInt, opcode: Bits): Bits =
      Cat(imm20.asBits.resize(20), rd.asBits.resize(5), opcode)

    // J型：传入的是已 <<1、已符号扩展到21位的 offset（imm[20:0]，LSB=0）
    def makeJ(imm21: UInt, rd: UInt, opcode: Bits): Bits = {
      val j20    = imm21(20)
      val j10_1  = imm21(10 downto 1)
      val j11    = imm21(11)
      val j19_12 = imm21(19 downto 12)
      Cat(j20, j19_12, j11, j10_1, rd.asBits.resize(5), opcode)
    }

    // 常用 opcode / funct3 / funct7（Bits常量）
    val OP      = B"7'b0110011"
    val OPIMM   = B"7'b0010011"
    val LOAD    = B"7'b0000011"
    val STORE   = B"7'b0100011"
    val BRANCH  = B"7'b1100011"
    val OPC_JAL = B"7'b1101111"
    val OPC_JALR= B"7'b1100111"
    val OPC_LUI = B"7'b0110111"
    val OPC_AUIPC=B"7'b0010111" // 目前未用到

    // funct3/funct7 片段
    val F3_ADDI = B"3'b000"
    val F3_ANDI = B"3'b111"
    val F3_SL   = B"3'b001"
    val F3_SR   = B"3'b101"
    val F3_LW   = B"3'b010"
    val F3_SW   = B"3'b010"
    val F3_BEQ  = B"3'b000"
    val F3_BNE  = B"3'b001"

    val F7_ADD  = B"7'b0000000"
    val F7_SUB  = B"7'b0100000"
    val F7_SRLI = B"7'b0000000"
    val F7_SRAI = B"7'b0100000"

    // ------------------ RVC 各类立即数拼接 ------------------

    // CI: imm[5|4:0]，符号位在 insn[12]，目标 I型需要 12b
    val ciImm6  = (cin(12) ## cin(6 downto 2)).asUInt // 6b，含符号位
    def immCI_12(): UInt = sext(ciImm6, 6, 12)

    // CI 移位型（SRLI/SRAI/SLLI）shamt：无符号，(insn[12] 作为高位)
    def shamtCI(): UInt = ((cin(12) ## cin(6 downto 2)).asUInt).resize(6) // 最多 6b，RV32 仅低5b有效

    // CIW: C.ADDI4SPN 的 nzimm: {insn[10:7],insn[12:11],insn[5],insn[6]} << 2
    def immCIW_ADDI4SPN(): UInt = {
      val raw = (cin(10 downto 7) ## cin(12 downto 11) ## cin(5) ## cin(6) ## B"2'b00").asUInt
      raw.resize(12) // I型 imm 宽度够用
    }

    // CL/CS: C.LW/C.SW 的字节偏移（4字节对齐）
    def offCL_LW(): UInt = {
      // uimm[6:2] = {insn[5],insn[12:10],insn[6]}, <<2
      val u = (cin(5) ## cin(12 downto 10) ## cin(6) ## B"2'b00").asUInt
      u.resize(12)
    }
    def offCS_SW(): UInt = {
      val u = (cin(5) ## cin(12 downto 10) ## cin(6) ## B"2'b00").asUInt
      u.resize(12)
    }

    // CLSP/CSS: C.LWSP/C.SWSP 偏移
    def offCLWSP(): UInt = {
      // {insn[3:2],insn[12],insn[6:4]} << 2
      val u = (cin(3 downto 2) ## cin(12) ## cin(6 downto 4) ## B"2'b00").asUInt
      u.resize(12)
    }
    def offCSWSP(): UInt = {
      // 规范等价表述：{insn[8:7],insn[12:9],insn[6:2]} << 2
      val u = (cin(8 downto 7) ## cin(12 downto 9) ## cin(6 downto 2) ## B"2'b00").asUInt
      u.resize(12)
    }

    // C.ADDI16SP: 仅 rd=x2；imm = {insn[12],insn[4:3],insn[5],insn[2],insn[6]} << 4
    def immAddi16sp(): UInt = {
      val raw = (cin(12) ## cin(4 downto 3) ## cin(5) ## cin(2) ## cin(6) ## B"4'b0000").asUInt // 10b含符号
      sext(raw, 10, 12)
    }

    // CJ: C.J/C.JAL 偏移 (<<1)，拼成 12b 含符号，再扩到 21b 供 J型
    def immCJ_21(): UInt = {
      // imm[11|4|9:8|10|6|7|3:1|5] << 1
      val i12 = (cin(12) ## cin(8) ## cin(10 downto 9) ## cin(11) ##
        cin(6) ## cin(7) ## cin(3 downto 1) ## cin(5) ## B"1'b0").asUInt // 12b(含对齐0)
      sext(i12, 12, 21)
    }

    // CB: C.BEQZ/C.BNEZ 偏移 (<<1)，9b 含符号，扩到 13b 供 B型
    def immCB_13(): UInt = {
      // {insn[12],insn[6:5],insn[2],insn[11:10],insn[4:3],0}
      val i9 = (cin(12) ## cin(6 downto 5) ## cin(2) ## cin(11 downto 10) ##
        cin(4 downto 3) ## B"1'b0").asUInt
      sext(i9, 9, 13)
    }

    // ------------------ 默认：透传32b正常指令 ------------------
    out := instrPre

    when(isC) {
      // 提取共用小字段
      val rd   = (cin(11 downto 7)).asUInt
      val rs1  = (cin(11 downto 7)).asUInt
      val rs2  = (cin(6  downto 2)).asUInt
      val rs1p = xr((cin(9 downto 7)).asUInt)  // 压缩寄存器
      val rs2p = xr((cin(4 downto 2)).asUInt)
      val rd_p = xr((cin(4 downto 2)).asUInt)

      switch(q){
        // ---------------- Quadrant 0 (00): CIW/CL/CS ----------------
        is(B"2'b00"){
          switch(f3){
            is(B"3'b000"){ // C.ADDI4SPN -> ADDI rd', x2, nzimm
              val rdPrime = rd_p
              val imm     = immCIW_ADDI4SPN()
              out := makeI(imm, U(2, 5 bits), F3_ADDI, rdPrime, OPIMM)
            }
            is(B"3'b010"){ // C.LW -> LW rd', off(rs1')
              out := makeI(offCL_LW(), rs1p, F3_LW, rd_p, LOAD)
            }
            is(B"3'b110"){ // C.SW -> SW rs2', off(rs1')
              out := makeS(offCS_SW(), rs2p, rs1p, F3_SW, STORE)
            }
          }
        }

        // ---------------- Quadrant 1 (01): CI/CJ/CB/算术 ----------------
        is(B"2'b01"){
          switch(f3){
            is(B"3'b000"){ // C.ADDI / NOP
              val imm = immCI_12()
              // NOP 情况 (rd==x0 && imm==0) 也会得到 ADDI x0,x0,0
              out := makeI(imm, rd, F3_ADDI, rd, OPIMM)
            }
            is(B"3'b001"){ // RV32: C.JAL -> JAL x1, off ; RV64+: C.ADDIW
              if(xlen == 32){
                out := makeJ(immCJ_21(), U(1,5 bits), OPC_JAL)
              } else {
                val imm = immCI_12()
                out := makeI(imm, rd, F3_ADDI, rd, B"7'b0011011") // ADDIW
              }
            }
            is(B"3'b010"){ // C.LI -> ADDI rd, x0, imm
              out := makeI(immCI_12(), U(0,5 bits), F3_ADDI, rd, OPIMM)
            }
            is(B"3'b011"){ // C.ADDI16SP / C.LUI
              when(rd === U(2,5 bits)){ // x2
                out := makeI(immAddi16sp(), U(2,5 bits), F3_ADDI, U(2,5 bits), OPIMM)
              } elsewhen(rd =/= U(0,5 bits)){
                // C.LUI: uimm = {insn[12],insn[6:2]} << 12
                val uimm20 = (cin(12) ## cin(6 downto 2) ## B"12'b000000000000").asUInt.resize(20)
                out := makeU(uimm20, rd, OPC_LUI)
              } otherwise {
                // illegal：保持 out 不变或置成 ADDI x0,x0,0
                out := B"32'h00000013"
              }
            }
            is(B"3'b100"){ // C.SRLI / C.SRAI / C.ANDI / C.SUB/XOR/OR/AND
              val subop = cin(11 downto 10)
              switch(subop){
                is(B"2'b00"){ // C.SRLI
                  val sh = shamtCI().resize(12)
                  out := makeI(sh, rs1p, F3_SR, rs1p, OPIMM) // SRLI
                }
                is(B"2'b01"){ // C.SRAI
                  val sh = shamtCI().resize(12) | U(0x400, 12 bits) // 0100000 <<5 进去
                  out := makeI(sh, rs1p, F3_SR, rs1p, OPIMM) // SRAI
                }
                is(B"2'b10"){ // C.ANDI
                  out := makeI(immCI_12(), rs1p, F3_ANDI, rs1p, OPIMM)
                }
                is(B"2'b11"){ // C.SUB/C.XOR/C.OR/C.AND
                  val funct2 = cin(6 downto 5)
                  val rs2p_5 = rs2p
                  val rd_5   = rs1p
                  switch(funct2){
                    is(B"2'b00"){ // SUB
                      out := makeR(F7_SUB, rs2p_5, rs1p, B"3'b000", rd_5, OP)
                    }
                    is(B"2'b01"){ // XOR
                      out := makeR(F7_ADD, rs2p_5, rs1p, B"3'b100", rd_5, OP)
                    }
                    is(B"2'b10"){ // OR
                      out := makeR(F7_ADD, rs2p_5, rs1p, B"3'b110", rd_5, OP)
                    }
                    is(B"2'b11"){ // AND
                      out := makeR(F7_ADD, rs2p_5, rs1p, B"3'b111", rd_5, OP)
                    }
                  }
                }
              }
            }
            is(B"3'b101"){ // C.J -> JAL x0, off
              out := makeJ(immCJ_21(), U(0,5 bits), OPC_JAL)
            }
            is(B"3'b110"){ // C.BEQZ -> BEQ rs1', x0, off
              out := makeB(immCB_13(), U(0,5 bits), rs1p, F3_BEQ, BRANCH)
            }
            is(B"3'b111"){ // C.BNEZ -> BNE rs1', x0, off
              out := makeB(immCB_13(), U(0,5 bits), rs1p, F3_BNE, BRANCH)
            }
          }
        }

        // ---------------- Quadrant 2 (10): CI/CLSP/CSS/寄存器跳转 ----------------
        is(B"2'b10"){
          switch(f3){
            is(B"3'b000"){ // C.SLLI -> SLLI rd, rd, shamt
              when(rd === U(0,5 bits)){
                out := B"32'h00000013" // illegal -> ADDI x0,x0,0
              } otherwise {
                out := makeI(shamtCI().resize(12), rd, F3_SL, rd, OPIMM)
              }
            }
            is(B"3'b010"){ // C.LWSP -> LW rd, off(x2)
              when(rd === U(0,5 bits)){
                out := B"32'h00000013"
              } otherwise {
                out := makeI(offCLWSP(), U(2,5 bits), F3_LW, rd, LOAD)
              }
            }
            is(B"3'b100"){ // C.JR / C.JALR / C.MV / C.ADD / C.EBREAK
              val rs2z = rs2 === U(0,5 bits)
              when(rs2z && (rd =/= U(0,5 bits))){ // C.JR -> JALR x0, 0(rd)
                out := makeI(U(0,12 bits), rd, B"3'b000", U(0,5 bits), OPC_JALR)
              } elsewhen(rs2z && (rd === U(0,5 bits))){ // C.EBREAK
                out := B"32'h00100073"
              } elsewhen(!rs2z && (rd === U(0,5 bits))){ // C.MV -> ADD rd, x0, rs2  (这里 rd 实际是显示的 rd 字段)
                // 注意 C.MV 编码里 rd!=0；如果你的前端零扩扩展，这里保持等价 ADD rd, x0, rs2
                out := makeR(F7_ADD, rs2, U(0,5 bits), B"3'b000", rs2, OP)
              } otherwise { // C.ADD -> ADD rd, rd, rs2
                out := makeR(F7_ADD, rs2, rd, B"3'b000", rd, OP)
              }
            }
            is(B"3'b110"){ // C.SWSP -> SW rs2, off(x2)
              out := makeS(offCSWSP(), rs2, U(2,5 bits), F3_SW, STORE)
            }
          }
        }
      }
    }

    out
  }
}

