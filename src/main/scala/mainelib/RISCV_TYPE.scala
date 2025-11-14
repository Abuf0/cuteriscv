package mainelib
import spinal.core._

//object RISCV_TYPE {

  // PIPELINE STATE
  object PP_STATE extends SpinalEnum(
    defaultEncoding = binaryOneHot
  ){
    val IF1_PC, IF2_TM, IF3_FM, ID, ISSUE, EXE, WB, COMMIT = newElement()
  }

  // EX stage Units List
  object ALU_UNIT_SEL extends SpinalEnum(defaultEncoding = binarySequential) {
    val ALU, MUL, DIV, BJU, LSU, CSR, FPU, NOP = newElement()
  }

  object FU_ID extends SpinalEnum(defaultEncoding = binarySequential) {
    case class Def(
                    ALU: Seq[E],
                    MUL: Seq[E],
                    DIV: Seq[E],
                    BJU: Seq[E],
                    LSU: Seq[E],
                    CSR: Seq[E],
                    FPU: Seq[E],
                    NOP: Seq[E]
                  )

    private var built: Option[Def] = None

    def build(issueW: Int): Def = {
      require(built.isEmpty, "FU_ID.build() must be called only once")

      def mk(prefix: String, n: Int): Seq[E] =
        Seq.tabulate(n)(i => newElement(s"${prefix}${i}"))  // ← 关键：显式命名

      val nalu = math.max(2, (issueW/2.0).ceil.toInt)
      val nmul = math.ceil(issueW.toFloat/3).toInt
      val ndiv = 1
      val nbju = 1
      val nlsu = math.max(1, math.round(issueW.toFloat/2))
      val ncsr = 1
      val nfpu = 1
      val nnop = 1
      val alu = mk("ALU", nalu)
      val mul = mk("MUL", nmul)
      val div = mk("DIV", ndiv)
      val bju = mk("BJU", nbju)
      val lsu = mk("LSU", nlsu)
      val csr = mk("CSR", ncsr)
      val fpu = mk("FPU", nfpu)
      val nop = mk("NOP", nnop)

      val d = Def(alu, mul, div, bju, lsu, csr, fpu, nop)
      built = Some(d)
      println(s"\n---------- Function Unit ------------")
      println(s"|\tissuewidth = $issueW\n|\tALU = $nalu\n|\tMUL = $nmul\n|\tDIV = $ndiv\n|\tBJU = $nbju\n|\tLSU = $nlsu\n|\tCSR = $ncsr\n|\tFPU = $nfpu\n|\tNOP = $nnop")
      println(s"-------------------------------------\n")
      d
    }

    def enums: Def =
      built.getOrElse(sys.error("Call FU_ID.build(issueW) before creating signals"))
  }

  // dec option types
  object OP_TYPE extends SpinalEnum(defaultEncoding = binarySequential) {
    val OP_LOGIC, OP_ARITHMETIC, OP_SHIFT, OP_MOVE, OP_MUL, OP_DIV, OP_JUMP_BRANCH, OP_LOAD, OP_STORE, OP_NOP = newElement()
    //println(s"OP_TYPE Count=${elements.size}, OP_TYPE Width=${this.defaultEncoding.getWidth(this)}")
  }

  // Exception cause List // todo: 包含所有异常【包含中断】，有待后续完善
  //val exception_encoding = SpinalEnumEncoding("dynamicEncoding", _ + 1)
  object EXC_CAUSE extends SpinalEnum(defaultEncoding = binarySequential) {
    val ILEGAL_CODING, ILEGAL_PC, ILEGAL_ACCESS, NON_ALIGNED, EBREAK  = newElement()
  }


//}
