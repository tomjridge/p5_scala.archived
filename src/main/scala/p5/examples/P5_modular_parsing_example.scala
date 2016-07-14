package p5.examples

import p5._

object P5_modular_parsing_example {
  
  //def println(x:Any) = {} // turn off debugging
  
  import P5_lib.lib
  import P5_lib.lib._
  import P5_lib.lib.implicits._
  
  // we really want to change p~q to mean "parse p, optionally followed by ws, followed by q"; but with the current setup 
  // this is a bit fiddly, 
  // since ~ is a method on an object, rather than an infix function
  
  /*
  implicit def parser2richrhs[S,A](p:Parser[S,A]) : RichRhs[S,A] = {
    implicits.parser2richrhs(p)
  }
  * 
  */

  val w = lib.terminals.parse_RE(" *".r) // 0 or more spaces
  
  sealed abstract class Exp
  case class Plus(l:Exp,r:Exp) extends Exp
  case class Minus(l:Exp,r:Exp) extends Exp
  case class Num(r:Int) extends Exp
  
  val parse_num : Parser[String,Int] = lib.terminals.parse_RE("[0-9]+".r) ^^ ((x:Substring[String]) => content(x).toInt)
  def a(s:String) = lib.terminals.a(s)
  
  val never:Parser[String,Nothing] = {
    val rp = (x:Substring[String]) => List()
    val ract = (x:Substring[String]) => List()
    mktmparser(rp)(ract)
  }
  
  
  trait Parse_A { 
    val parse_h: Parser[String,Exp]
    val pa:Parser[String,Exp] = {
      ((pa ~w ~"+" ~w ~pa) ^^ {case x1~_~_~_~x2 => Plus(x1,x2):Exp } ) | // FIXME note explicit annotation(
      ((pa ~w ~"-" ~w ~pa) ^^ {case x1~_~_~_~x2 => Minus(x1,x2) } ) |(
      parse_num ^^ ((x:Int) => Num(x))) |(
      {(a("("))~w~pa ~w ~")"} ^^ {case _~_~x~_~_ => x} ) | // FIXME note how we need to explicitly convert the first string
      parse_h
    }
  }
  
  val arith_parser = new Parse_A {
    val parse_h = never
  }
  
  case class Bool(b:Boolean) extends Exp
  case class IfThenElse(c:Exp,p1:Exp,p2:Exp) extends Exp
  case class Lt(p1:Exp,p2:Exp) extends Exp
  
  trait Parse_B {
    val parse_h: Parser[String,Exp]
    val pb:Parser[String,Exp] = {
      ((a("true")) ^^ (_ => Bool(true):Exp)) |
      ((a("false")) ^^ (_ => Bool(false))) |(
      ((a("if"))~w~pb ~w ~"then" ~w ~pb ~w ~"else" ~w ~pb) ^^ 
        { case i~_~c~_~t~_~p1~_~e~_~p2 => IfThenElse(c,p1,p2) } )  |(
      (pb ~w ~"<" ~w ~pb) ^^ { case p1~_~_~_~p2 => Lt(p1,p2) } ) |
      parse_h
    }
  }
  
  val bool_parser = new Parse_B {
    val parse_h = never
  }
  
  case class Var(s:String) extends Exp
  case class App(e1:Exp,e2:Exp) extends Exp
  case class Lam(x:String,body:Exp) extends Exp
  
  trait Parse_L {
    val parse_h: Parser[String,Exp]
    val pvar:Parser[String,String] = lib.terminals.parse_RE("[xyz]".r) ^^ ( (x) => content(x))
    val pl:Parser[String,Exp] = {
      (((a("""\"""))~w~pvar~w~pl) ^^ { case _~_~x~_~p1 => Lam(x,p1):Exp }) |(
      (pl ~w ~pl) ^^ {case p1~_~p2 => App(p1,p2)}) |(
      pvar ^^ ( (x) => Var(x))) |(
      {(a("("))~w~pl ~w ~")"} ^^ {case _~_~x~_~_ => x} ) |
      parse_h
    }
  }
  
  val lam_parser = new Parse_L {
    val parse_h = never
  }
  
  object combined_parser extends Parse_A with Parse_B with Parse_L {
    val parse_h:Parser[String,Exp] = pa | pb | pl
  }
  
  def main(args:Array[String]) = {
    
    val num_loops = 100
    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      val diff = t1 - t0
      val in_secs = diff / 1000000000.0
      val avg = in_secs / num_loops
      println(s"Elapsed time: $diff ns ($in_secs secs) (avg: $avg)")
      result
    }
    
    {
      val p = arith_parser.pa
      val s = "1+2"
      println(lib.run_parser(s, s.length(), p))
    }
    
    {
      val p = bool_parser.pb
      val s = "if true < false then false else true"
      println(lib.run_parser(s, s.length(), p))
    }
    
    {
      val p = lam_parser.pl
      val s = "\\ x \\ y (x y)"
      println(lib.run_parser(s, s.length(), p))
    }
    
    {
      val p = combined_parser.parse_h
      val s = "\\ x \\ y if x < y then true else (1+(x+y))"
      time {
        for(i<-1 to num_loops) {
          println(lib.run_parser(s, s.length(), p))
        }
      }
      
    }
    
    /*
    {
      println("70 start")
      println(parse_num)
      val x = a("x") // FIXME call by name, but we are not using lazy to ensure that only evaluated once!
      println("x is: "+x)
      val p:Parser[String,Any] = parse_num~x
      println(p)
      println("70 2")
      println(p)
      val s = "123x"
      println("70: "+lib.run_parser(s, s.length(), p)) // works OK
    }
    
    {
      val x = a("x") // FIXME call by name, but we are not using lazy to ensure that only evaluated once! 
      val p:Parser[String,Any] = (parse_num~x)~parse_num
      println(s"75: $p")
      println("grammar: "+P3_core.code.sym_plus_to_grammar(lib.get_p3_parser(p).spl))
      println(s"76: $p")
      val s = "123x456"
      println(lib.run_parser(s, s.length(), p)) // doesn't work
      println(lib.run_parser(s, s.length(), p)) // doesn't work, doesn't generate any nts
    }
    */
    
    /*
    {
      val p = (p~w~"+"~w~p) ^^ (_ match {case x1~_~_~_~x2 => Plus(x1,x2):Exp } ))
      val s = "1+2"
      println(lib.run_parser(s, s.length(), p))
    }
    * 
    */
    
  }
  
}
