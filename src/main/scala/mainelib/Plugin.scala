package mainelib
import scala.io.Source

/*
object Plugin {

  /** 读取每行 4 字节的 .pat 文件，返回整个镜像的字节数组（小端：按出现顺序写入） */
  def loadPat4B(path: String): Array[Byte] = {
    def parseLine(line: String): Array[Byte] = {
      val body = line.takeWhile(c => c != '#' && c != '/')
      val toks = body.split("""[\s,]+""").map(_.trim).filter(_.nonEmpty)

      def parseHexByte(tok: String): Byte = {
        val t = tok.replace("_","").replace("0x","").replace("0X","")
        Integer.parseInt(t, 16).toByte
      }

      toks.length match {
        case 1 =>
          // 8 hex chars, e.g. "12345678"
          val t = toks(0).replace("_","").replace("0x","").replace("0X","")
          require(t.length == 8, s"expect 8 hex chars per word, got: $t")
          // 小端：低地址字节在前 -> 78 56 34 12
          Array(
            Integer.parseInt(t.substring(6,8),16).toByte,
            Integer.parseInt(t.substring(4,6),16).toByte,
            Integer.parseInt(t.substring(2,4),16).toByte,
            Integer.parseInt(t.substring(0,2),16).toByte
          )
        case 4 =>
          // 4 个字节 token，按出现顺序输出（已是小端顺序）
          toks.map(parseHexByte)
        case 0 =>
          Array.emptyByteArray
        case n =>
          throw new RuntimeException(s"Bad line (need 1 word or 4 bytes, got $n): $line")
      }
    }

    val src = Source.fromFile(path)
    try src.getLines().flatMap(parseLine).toArray
    finally src.close()
  }


}
*/

object Plugin {

  /** 读取每行可能包含地址标记和4字节数据的.pat文件，返回整个镜像的字节数组（小端：按出现顺序写入） */
  def loadPat4B(path: String): Array[Byte] = {
    def parseLine(line: String): Array[Byte] = {
      // 去除注释内容（#或/之后的部分）
      val body = line.takeWhile(c => c != '#' && c != '/').trim
      if (body.isEmpty) return Array.emptyByteArray

      // 分割所有token（处理空格、逗号分隔，支持地址标记@开头的格式）
      val toks = body.split("""[\s,]+""").map(_.trim).filter(_.nonEmpty)
      if (toks.isEmpty) return Array.emptyByteArray

      // 过滤掉地址标记（以@开头的token）
      val dataToks = toks.filterNot(_.startsWith("@"))

      def parseHexByte(tok: String): Byte = {
        val t = tok.replace("_", "").replace("0x", "").replace("0X", "")
        Integer.parseInt(t, 16).toByte
      }

      dataToks.length match {
        case 0 =>
          // 只有地址标记，无数据
          Array.emptyByteArray
        case n if n >= 1 =>
          // 处理每个数据token（每个token是8位16进制数，代表4字节）
          dataToks.flatMap { tok =>
            val t = tok.replace("_", "").replace("0x", "").replace("0X", "")
            require(t.length == 8, s"expect 8 hex chars per word, got: $t in line: $line")
            // 小端处理：低地址字节在前（如"12345678" -> 78 56 34 12）
            Array(
              Integer.parseInt(t.substring(0, 2), 16).toByte,
              Integer.parseInt(t.substring(2, 4), 16).toByte,
              Integer.parseInt(t.substring(4, 6), 16).toByte,
              Integer.parseInt(t.substring(6, 8), 16).toByte,
//              Integer.parseInt(t.substring(6, 8), 16).toByte,
//              Integer.parseInt(t.substring(4, 6), 16).toByte,
//              Integer.parseInt(t.substring(2, 4), 16).toByte,
//              Integer.parseInt(t.substring(0, 2), 16).toByte
            )
          }
        case _ =>
          throw new RuntimeException(s"Bad line (invalid data tokens): $line")
      }
    }

    val src = Source.fromFile(path)
    try src.getLines().flatMap(parseLine).toArray
    finally src.close()
  }
}