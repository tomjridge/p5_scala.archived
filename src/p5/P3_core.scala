package p5

import e3.E3_substring

object P3_core {
  
  def println(x:Any) = {} // turn off debugging

  import P3_core_aux._

  //class Intrep[T] // a construct such that it is possible to get an int, but the underlying type is T

  type Substr[A] = E3_substring.Substring[A]
  //type raw_parser[A] = sp.t[A] => List[Int]
    type raw_parser[A] = Substr[A] => List[Int]
    //type raw_act[S,A] = sp.t[S] => List[A]
    type raw_act[S,A] = Substr[S] => List[A]
    
  trait code_trait {
    val gg : Gensym
    val bb : Box
    val sp : Span
    
    type term = gg.t // odd
    type nonterm = gg.t // even
    
    case class Context(x:Set[(nonterm,sp.t[bb.key])]) // note we use keys, not the actual strings, for performance reasons
    
    sealed abstract class Symbol
    case class NT(x:nonterm) extends Symbol // could override hashcode, but compiler will make this efficient anyway?
    case class TM(x:term) extends Symbol
    def mk_NT() = gg.gen_even()
    def mk_TM() = gg.gen_odd()
    def dest_NT(sym:Symbol) = sym match {
      case NT(nt) => nt
      case _ => throw new Exception("dest_NT")
    }
    
    /*
    trait Pre_ref_equality {
      def canEqual( that:Any ) = that.isInstanceOf[Pre_ref_equality]
    }
    trait Reference_equality extends Pre_ref_equality { 
      override def equals( that:Any ) = that match {
        case that:Reference_equality => ( that eq this) // reference identity
        case _ => false
      }
      override def canEqual( that:Any ) = that.isInstanceOf[Reference_equality]
    }
    * 
    */
    
    case class Tm_plus[S](val tp_tm:term, val tp_raw_parser:raw_parser[S]) { // extends Reference_equality {
      override def toString = { "TM_plus("+tp_tm.toString()+","+this.hashCode()+")"}
    }
    
    object Tm_plus_mk {
      def apply[S](tp_tm:term, tp_raw_parser:raw_parser[S]) = {
        println(s"Tm_plus_mk: $tp_tm $tp_raw_parser")
        new Tm_plus(tp_tm,tp_raw_parser)
      }
    }
    
    // note that lists which contain the same sym_pluses may be identified (particularly the empty list)
    case class Ty_rhs_syms_list[S](val xs:List[Sym_plus[S]]) // extends Reference_equality
      
    object Ty_rhs_syms_list_mk {
      def apply[S](xs:List[Sym_plus[S]]) = {
        println("Ty_rhs_syms_list_mk: "+xs)
        new Ty_rhs_syms_list(xs)
      }
    }
    
    case class Nt_plus[S](val np_nt:nonterm, val np_rhss:() => List[Ty_rhs_syms_list[S]]) { //extends Reference_equality {
      override def toString = { "Nt_plus("+np_nt.toString()+")"}
    }
    
    object Nt_plus_mk {
      def apply[S](np_nt:nonterm, np_rhss:() => List[Ty_rhs_syms_list[S]]) = {
        println("Nt_plus_mk: "+np_nt)
        new Nt_plus(np_nt,np_rhss)
      }
    }
    
    sealed abstract class Sym_plus[S] // extends Reference_equality
    case class NP[S](x:Nt_plus[S]) extends Sym_plus[S] {
      override def toString = { "NP("+x+","+this.hashCode()+")"}
    }
    case class TP[S](x:Tm_plus[S]) extends Sym_plus[S] {
      override def toString = { "TP("+x+","+this.hashCode()+")"}
    }
    
    
    type ty_oracle[S] = (Ty_rhs_syms_list[S],Sym_plus[S]) => (Int,Int) => List[Int]
    
    type ty_tmoracle[S] = (Tm_plus[S]) => (Int,Int) => Boolean
    
    class inr[S](
      val ss4:sp.t[S],
      val box4:bb.t[S],
      val lc4:Context,
      val oracle4:ty_oracle[S],
      val tmoracle4:ty_tmoracle[S]) {
      def with_ss4(x:sp.t[S]) = {
        new inr[S](x,box4,lc4,oracle4,tmoracle4)
      }
      def with_lc4(x:Context) = {
        new inr[S](ss4,box4,x,oracle4,tmoracle4)
      }
    }
    
    type outr[+A] = List[A]
    
    class Parser[S,+A](val spl:Sym_plus[S], val act:inr[S] => outr[A]) {
      
      //      def with_act[B](f:A => B) : Parser[S,B] = {
      //        new Parser(this.spl,(i) => this.act(i).map(f))
      //      }
      def with_act[B](act: inr[S] => outr[B]) = {
        new Parser[S,B](this.spl,act) // do we need to create another spl? probably not
      }
      override def toString = { spl.toString() }
      
    }
   
    def ^^ [S,A,B](p:Parser[S,A])(f: A => B): Parser[S,B] = { // construct a new parser
      new Parser(p.spl,(i) => p.act(i).map(f) )
    }
    
    
    
    
    case class Rhs[S,+A](rhs_syms:() => Ty_rhs_syms_list[S], rhs_act: inr[S] => outr[A])
    
    case class Alts[S,+A](alts_rhss: () => List[Ty_rhs_syms_list[S]], alts_act: inr[S] => outr[A])
    
    def rhs[S,A](p:Parser[S,A]) : Rhs[S,A] = { // FIXME really want [S,A]
      // but then we also seem to want to have a representation for the empty rhs
      val r = Rhs(
        rhs_syms=(() => Ty_rhs_syms_list_mk(List(p.spl))),
        rhs_act=p.act)
      r
    }
    
    def seq[S,A,B](r:Rhs[S,A], p:Parser[S,B]) = {
      val syms = r.rhs_syms()
      val syms2 = Ty_rhs_syms_list_mk(syms.xs ++ List(p.spl))
      val act = (i0:inr[S]) => {
        sp.dest_SS(i0.ss4) match {
          case (s,i,j) => {
            val ks = i0.oracle4(syms,p.spl)(i,j)
            val f1 = (k:Int) => {
              val rs1 = r.rhs_act(i0.with_ss4(sp.mk_SS(s, i, k)))
              val rs2 = p.act(i0.with_ss4(sp.mk_SS(s, k, j)))
              val to_return = rs1.flatMap { x => rs2.map { y => (x,y) } } // all pairs
              to_return
            }
            ks.flatMap(f1)
          }
        } 
      }
      Rhs(() => syms2, act)
    }
    
    def set_act_rhs[S,A,B](rhs:Rhs[S,A],act:A => B) : Rhs[S,B] = {
      val f = rhs.rhs_act.andThen { x => x.map(act) }
      Rhs(rhs.rhs_syms,f)      
    }
    
    def set_act[S,A,B](p:Parser[S,A],act:A => B) : Parser[S,B] = {
      new Parser(p.spl,p.act.andThen { x => x.map(act) })
    }
    
    def alts[S,A](rhss:List[Rhs[S,A]]) : Alts[S,A] = {
      rhss match {
        case Nil => throw new Exception("alts: []")
        case rhs::Nil => {
          val syms = () => List(rhs.rhs_syms()) // FIXME do we want to eagerly evaluate rhs_syms?
          val act = rhs.rhs_act
          Alts(syms,act)
        }
        case rhs::rest => {
          val rest2 = alts(rest)
          val syms = () => (rhs.rhs_syms()) :: (rest2.alts_rhss()) // FIXME eagerly evaluate?
          val act = (i:inr[S]) => ((rhs.rhs_act(i)) ++ (rest2.alts_act(i))).distinct
          Alts(syms,act)
        }
      } 
    }
    
    def mkntparser_1[S,A](alts2: => Alts[S,A]) = {
      val i = gg.gen_even()
      lazy val alts4 = alts2
      lazy val alts3_rhss = alts4.alts_rhss()
      val nt = Nt_plus_mk(i,() => alts3_rhss) // FIXME eagerly evaluate alts_rhss? or lazily? probably lazily
      new Parser(NP(nt),(i:inr[S]) => alts4.alts_act(i))
    }
    
    // note we give an interface in terms of E3_substring so that types are inferred
    //def mktmparser[S,A](rp:raw_parser[S]) = (ract:raw_act[S,A]) => {
    def mktmparser[S,A](rp:E3_substring.Substring[S] => List[Int]) = (ract:E3_substring.Substring[S] => List[A]) => {
      val tp_tm = mk_TM()
      val tp_raw_parser = rp
      val tp = Tm_plus_mk[S](tp_tm,tp_raw_parser) // NB the annotation with type [S] appears crucial if we want types inferred
      val act = (i0:inr[S]) => {
        sp.dest_SS(i0.ss4) match {
          case (s,i,j) => {
            if (i0.tmoracle4(tp)(i,j)) (ract(s,i,j))
            else List()
          }
        }
      }
      new Parser(TP(tp),act)
    }
    
    def run_parser_1[S,A](p:Parser[S,A],txt:S,len:Int,otmo:(ty_oracle[S],ty_tmoracle[S])) : outr[A] = {
      otmo match {
        case (o,tmo) => {
          val ss = sp.mk_SS(txt,0,len)
          val inr = new inr(ss,bb.box(txt),Context(Set()),o,tmo)
          val r:outr[A] = p.act(inr)
          r
        }
      }
    }
    
    // context definitions; a context is a set; irrelevant parts are removed
    def replace_string_with_box_key[S](i0:inr[S],ss:sp.t[S]) = {
      sp.dest_SS(ss) match {
        case (s,l,h) => sp.mk_SS(bb.box_get_key(i0.box4), l, h)
      }
    }
    
    def normalize_context[S](i0:inr[S],lc:Context,ss:sp.t[S]) : Context = {
      val ss2 = replace_string_with_box_key(i0,ss)
      lc match {
        case Context(xs) => Context(xs.filter({ case (nt,ss3) => ss3 == ss2 }))
      }
    }
    
    def update_context[S](i0:inr[S],lc:Context,x:(nonterm,sp.t[S])) = {
      val lc2 = normalize_context(i0,lc,x._2) match { case Context(lc) => lc }
      val new_elt = replace_string_with_box_key(i0,x._2)
      Context(lc2 + ((x._1,new_elt))) // note double bracket
    }
    
    def context_contains[S](i0:inr[S],lc:Context,x:(nonterm,sp.t[S])) = {
      val ss2 = replace_string_with_box_key(i0,x._2)
      lc match {
        case Context(lc) => lc.contains((x._1,ss2))
      }
    }
    
    
    def update_lc4[S,A](nt:nonterm,h:inr[S]=>outr[A],i0:inr[S]) = {
      h(i0.with_lc4(update_context(i0,i0.lc4,(nt,i0.ss4))))
    }
    
    def check_and_upd_lc4[S,A](p:Parser[S,A]) = {
      p.spl match {
        case TP(_) => p
        case NP(np) => {
          val h = p.act
          val h2 = (i:inr[S]) => {
            val nt = np.np_nt
            val should_trim = context_contains(i,i.lc4,(nt,i.ss4))
            if (should_trim) List()
            else update_lc4(nt,h,i)
          }
          p.with_act(h2)
        }
      }
    }
    
    def mkntparser[S,A](p: => Parser[S,A]) = { // FIXME this should be call by name, mkntparser_1 should nto be?
      check_and_upd_lc4(p)
    }
    
    // external versions of nt_items_for_nt

    
  }  // trait code
  
  
  val code = new code_trait {
    val gg = default_gensym
    val bb = new Default_box()
    val sp = new Default_span
    
    
    // a few auxiliary functions, exposing an "external" interface in terms of Substring
    import E3_substring.Substring
    def nt_items_for_nt[S](nt:Nt_plus[S]) = (ss:Substring[S]) => {
          val rhss = nt.np_rhss()
          val (s,i,j) = ss
          val rs = rhss.map( (rhs:Ty_rhs_syms_list[S]) =>
            (nt,
              Ty_rhs_syms_list_mk[S](List()),
              rhs,
              j,j))
          rs
        }
    def p_of_tm[S](tm:Tm_plus[S]) = (ss:Substring[S]) => {
          //println(tm)
          val (s,i,j) = ss
          //println(ss)
          val rs = tm.tp_raw_parser(sp.mk_SS(s, i, j))
          //println(rs)
          rs
        }
    
    // utility
    type Grammar[S] = List[(Nt_plus[S],List[Sym_plus[S]])]
    def sym_plus_to_grammar[S](sym:Sym_plus[S]) : Grammar[S] = {
      def f1(g:Grammar[S],syms:List[Sym_plus[S]]) : Grammar[S] = {
        syms match {
          case Nil => g
          case sym::syms => {
            sym match {
              case NP(ntp) => {
                val nts_of_g :List[Nt_plus[S]] = g.map( (x) => x._1)
                if (nts_of_g.contains(ntp)) f1(g,syms) else {
                  val rhs = ntp.np_rhss()
                  val list_syms = rhs.map { x => x.xs }
                  val rules = list_syms.map( (x) => (ntp,x) )
                  f1(g++rules,syms++(list_syms.flatten))
                }
              }
              case TP(x) => f1(g,syms)
            }
          } 
        }
      } // f1
      f1(List(),List(sym))
    }
  }
  

}
