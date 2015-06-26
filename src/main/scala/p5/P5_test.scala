package p5

object P5_test {
  
  object example_parser_2 {
    import P5_lib.lib._
    import P5_lib.lib.implicits._
    
    val raw_a1 = (x:Substring[String]) => x match { case (s,i,j) => 
      if (i < j && s(i)=='1') List(i+1) else List()}

    val _1 = mktmparser[String,Int](raw_a1)((ss:Substring[String]) => 
      if (content(ss) == "1") List(1) else throw new Exception("_1"))
    
    val raw_eps = (x:Substring[String]) => x match { case (s,i,j) => 
      if (i <= j) List(i) else List()}
    
    val eps = mktmparser[String,Int](raw_eps)((ss:Substring[String]) => ss match { 
          case (s,i,j) => if (i==j) List(0) else {println("!"); List()} // this should not be called if the oracle is right
        })
    
    
    val e2:Parser[String,Int] = {
      ((e2 ~ e2 ~ e2) ^^  { _ match { case x1 ~ x2 ~ x3 => x1+x2+x3} }) |
        _1 |
        eps
    }
    
    def test() = {
      val s = "1"*10
      val len = s.length()
      println(run_parser(s,len,e2))
    }
  }
  
  def main(args: Array[String]) {
    example_parser_2.test()
    import P5_lib.lib
    import P5_lib.lib._
    {
      val p = lib.terminals.parse_RE("x*y".r)
      println(lib.run_parser("xxxy", 4, p))
      println(("x*y".r).findFirstMatchIn("xxxyz"))
    }
    {
      val p1 = lib.terminals.until_a("b")
      val p2 = lib.terminals.a("bc")
      import P5_lib.lib._
      import P5_lib.lib.implicits._
      //val q:Rhs[String,Any] = p1~p2
      //val p:Parser[String,Any] = p1~p2
      val s = "abc"
      println("abc: "+lib.run_parser(s, s.length(), p1~p2)) 
    }
  }
  
}
