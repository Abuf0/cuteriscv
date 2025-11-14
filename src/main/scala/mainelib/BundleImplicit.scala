package mainelib

import spinal.core._   // IMasterSlave

// 隐式类，协助Master、Slave在不同类的实例中的连接
object BundleImplicit{
  implicit class autoConnect(bus:Bundle){
    def connect(srcBus:Bundle):Unit={
      bus.connectWithSrc(srcBus)
    }
    def connectWithSrc(srcBus:Bundle):Unit={
      for((name,element)<-bus.elements){
        val nameOfBundle1 = srcBus.getName()
        val nameOfBundle2 = bus.getName()
        println(Console.GREEN+s"connecting Bundle " + Console.YELLOW + s"$nameOfBundle1"
          + Console.GREEN + s" & " + Console.YELLOW + s"$nameOfBundle2" + Console.GREEN +s" with port "
          + Console.MAGENTA + s"$name"+Console.RESET)
        val srcPort = srcBus.find(name)
        if(srcPort!=null){
          element match {
            case b:Bundle => b.connect(srcPort.asInstanceOf[Bundle])
            case _ =>{
              (element.getDirection,srcPort.getDirection) match {
                case (`out`,`in`)  => assignWithAdapt(element,srcPort)
                case(`out`,null)   => assignWithAdapt(element,srcPort)
                case(`in`,`out`)   => assignWithAdapt(srcPort,element)
                case(`in`,null)    => assignWithAdapt(srcPort,element)
                case(null,`out`)   => assignWithAdapt(srcPort,element)
                case(null,`in`)    => assignWithAdapt(element,srcPort)
                case (`in`,`in`)  => assignWithAdapt(element,srcPort)   // 为模块嵌套做准备，即子模块的输入也作为其上主模块的输入
                case (`out`,`out`)  => assignWithAdapt(element,srcPort)  //  // 为模块嵌套做准备，即子模块的输出也作为其上主模块的输出
                case _  if element.isAnalog && srcPort.isAnalog => assignWithAdapt(element,srcPort)
                case _             => LocatedPendingError(s"Direction Error")
              }
            }
          }
        }
      }
    }
    def assignWithAdapt(dst:Data,src:Data):Unit={
      if(dst.getBitsWidth != src.getBitsWidth){
        println(Console.RED+s"$dst width is different with $src, auto resize."+Console.RESET)
        dst <> src.resized
      }else
        dst <> src
    }
  }
}
