package p5.examples

import p5._


object EEE_example {
  
  import P5_lib.lib
  import P5_lib.lib._
  import P5_lib.lib.implicits._
  
  val _1: Parser[String,Substring[String]] = lib.terminals.a("1")
  val eps: Parser[String,Substring[String]] = lib.terminals.a("")
  
  val p:Parser[String,Int] = ((p~p~p) ^^ { case x~y~z => x+y+z }) |
  (_1 ^^ ( (_) => 1)) |
  (eps ^^ ( (_) => 0))
  
  def main(args:Array[String]) {
    def do_work = {
      val s = "1"*100
      println("EEE example:"+lib.run_parser_string(s,p))
    }
    Timing.time(10,do_work)
    
  }
  
}
