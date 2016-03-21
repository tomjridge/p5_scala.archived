package p5

object P5_lib {
  
  def println(x:String) = {} // turn off debugging
  
  trait Lib {
    
    type Substring[S] = (S,Int,Int)
    def content(x:Substring[String]) : String
    
    type Raw_parser[S] = Substring[S] => List[Int]
    type Raw_action[S,A] = Substring[S] => List[A]
    
    type Parser[S,+A]
    type Rhs[S,+A]
    
    def mktmparser[S,A](rp:Raw_parser[S]): (Raw_action[S,A]) => Parser[S,A]
        
    case class ~[+a, +b](_1: a, _2: b) {
      override def toString = "("+ _1 +"~"+ _2 +")"
    }
    
    trait RichRhs[S,A] { // implicit convert rhs to...
      def ~ [B](q:Parser[S,B]):Rhs[S,~[A,B]]
      def | (q:Rhs[S,A]): List[Rhs[S,A]]
      def ^^ [B](f:A=>B): Rhs[S,B] 
    }
    
    trait RichListRhs[S,A] { // implicit convert list(rhs) to...
      def | (q:Rhs[S,A]) : List[Rhs[S,A]]
    }
    
    trait Implicits {
      implicit def parser2rhs[S,A](p:Parser[S,A]) : Rhs[S,A]
      implicit def r2rr[S,A](xs:Rhs[S,A]) : RichRhs[S,A]
      implicit def parser2richrhs[S,A](p:Parser[S,A]) : RichRhs[S,A]
      implicit def lr2rlr[S,A](xs:List[Rhs[S,A]]) : RichListRhs[S,A]
      implicit def lr2p[S,A](xs: => List[Rhs[S,A]]) : Parser[S,A]
      implicit def r2p[S,A](r: => Rhs[S,A]) : Parser[S,A]
      implicit def s2p(s:String) : Parser[String,Substring[String]]
      import scala.util.matching._
      implicit def re2p(r:Regex) : Parser[String,Substring[String]]
    }
    
    val implicits: Implicits
    
    def run_parser[S,A](s:S,len:Int,p:Parser[S,A]): List[A]
    
    def run_parser_string[A](s:String,p:Parser[String,A]) = {
      run_parser(s,s.length(),p)
    }
    
    trait Terminals {
      import scala.util.matching._
      def parse_EOF[S](): Parser[S,Substring[S]]
      val parse_RE: (Regex) => Parser[String,Substring[String]]
      val a: (String) => Parser[String,Substring[String]]
      def until_a(s:String) : Parser[String,Substring[String]]
    }
    
    val terminals: Terminals
    
    def get_p3_parser[S,A](p:Parser[S,A]) : P3_core.code.Parser[S,A]
  }
  
  
  private object lib_impl extends Lib {
    
    override type Substring[S] = P3_core.Substr[S]
    
    def content(x:Substring[String]) = {
      x._1.substring(x._2, x._3)
    }
    
    import P3_core.code
    
    type Parser[S,+A] = code.Parser[S,A]
    
    type Rhs[S,+A] = code.Rhs[S,A]
    
    import P3_core.code
    
    def seq2[S,A,B](r:Rhs[S,A],p:Parser[S,B]): Rhs[S,~[A,B]] = {
      val r2 = code.seq(r,p)
      val r3 = code.set_act_rhs(r2,(x:(A,B)) => new ~(x._1,x._2))
      r3
    }
    
    class RichRhs_impl[S,A](val r:code.Rhs[S,A]) extends RichRhs[S,A] {
      def ~ [B](q:Parser[S,B]):Rhs[S,~[A,B]] = {
        seq2(r,q)
      }
      
      def | (q:Rhs[S,A]) : List[Rhs[S,A]] = {
        r::List(q)
      } 
      
      def ^^ [B](f:A => B): Rhs[S,B] = {
        code.set_act_rhs(r,f)
      }
    }
    
    class RichListRhs_impl[S,A](val r:List[Rhs[S,A]]) extends RichListRhs[S,A] {
      def | (q:Rhs[S,A]) : List[Rhs[S,A]] = {
        r :+ q
      }
    }
    
    // introduce objects to scope implicits before including them all later
    object not_implicit {
      def parser2rhs[S,A](p:Parser[S,A]) = code.rhs(p)
      def r2rr[S,A](r:Rhs[S,A]) = new RichRhs_impl(r)
      def parser2richrhs[S,A](p:Parser[S,A]) = r2rr(parser2rhs(p))
      def lr2rlr[S,A](xs:List[Rhs[S,A]]) = new RichListRhs_impl(xs)
      def lr2p[S,A](xs: => List[Rhs[S,A]]) = P5_memo.memo_p(code.mkntparser(code.mkntparser_1(code.alts(xs))))
      def r2p[S,A](r: => Rhs[S,A]) = lr2p(List(r))
      def s2p(s:String) = terminals.a(s)
      import scala.util.matching._
      def re2p(r:Regex) = terminals.parse_RE(r)
    }
    
    class Implicits_impl extends Implicits {
      implicit def parser2rhs[S,A](p:Parser[S,A]) = not_implicit.parser2rhs(p)
      implicit def r2rr[S,A](r:Rhs[S,A]) = not_implicit.r2rr(r)
      implicit def parser2richrhs[S,A](p:Parser[S,A]) = not_implicit.parser2richrhs(p)
      implicit def lr2rlr[S,A](xs:List[Rhs[S,A]]) = not_implicit.lr2rlr(xs)
      implicit def lr2p[S,A](xs: => List[Rhs[S,A]]) = not_implicit.lr2p(xs)
      implicit def r2p[S,A](r: => Rhs[S,A]) = not_implicit.r2p(r)
      implicit def s2p(s:String) = not_implicit.s2p(s)
      import scala.util.matching._
      implicit def re2p(r:Regex) = not_implicit.re2p(r)
    }
    val implicits = new Implicits_impl
    
    def mktmparser[S,A](rp:Raw_parser[S]): (Raw_action[S,A]) => Parser[S,A] = code.mktmparser(rp)
    
    def run_parser[S,A](s:S,len:Int,p:Parser[S,A]) = {
      import P5_e3_ds.Fix_e3_core
      val e3_core = new Fix_e3_core(s)
      e3_core.run_parser(s, len, p)
    }
    
    class Terminals_impl extends Terminals {
      
      def parse_EOF[S]() = {
        val rp = (s:Substring[S]) => s match { case (s,i,j) =>
          if (i==j) List(j) else List()}
        val ract = (x:Substring[S]) => List(x)
        mktmparser[S,Substring[S]](rp)(ract)
      }
      
      import scala.util.matching._
      
      val parse_RE = {
        val f = (r:Regex) => {
        val rp = (s:Substring[String]) => s match { case (s,i,j) =>
          println(s"parse_RE: $r, $s $i $j")
          val m = r.findPrefixMatchOf(s.subSequence(i, j))
          println(s"parse_RE: m: $m")
          m match {
            case None => List()
            case Some(m) => {
              val end = i+m.end
              println(s"parse_RE: m.end: $end")
              List(end) 
            }
          }
        }
        val ract = (x:Substring[String]) => {
          println("ract 150 called")
          List(x) 
        }
        mktmparser[String,Substring[String]](rp)(ract)
        }
        val tbl = scala.collection.mutable.Map[Regex,Parser[String,Substring[String]]]()
        P5_memo.simple_memo(tbl)(f)
      }
       
      val a = {
        val tbl = scala.collection.mutable.Map[String,Parser[String,Substring[String]]]()
        val f = (s:String) => parse_RE(java.util.regex.Pattern.quote(s).r) // hope this matches up with Scala regular expression syntax (it does according to current scala doc) 
        P5_memo.simple_memo(tbl)(f)
      }
      
      def until_a(s0:String) = {
        val rp = (s:Substring[String]) => s match { case (s,i,j) =>
          val r = java.util.regex.Pattern.quote(s0).r
          val m = r.findFirstMatchIn(s.subSequence(i, j))
          m match {
            case None => List(j) // consume till end of string if no match
            case Some(m) => {
              val end = i+m.end
              println((s"until_a: $s0")+(end-1))
              List(end-1)
            }
          }
        }
        val ract = (x:Substring[String]) => List(x)
        mktmparser[String,Substring[String]](rp)(ract)
      }
      
    }
    
    val terminals = new Terminals_impl()
    
    def get_p3_parser[S,A](p:Parser[S,A]) : P3_core.code.Parser[S,A] = p
    
  } // lib_impl
  
  val lib : Lib = lib_impl
  
  
}
