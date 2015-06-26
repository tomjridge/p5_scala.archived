package p5

object P3_context {

  trait P3_context { 
  
  type BB[S] // bb.t[S]
  type Nt
  
  type S_rep // int
  
  def map[S](inr:BB[S],s:S) : S_rep
  
  type T[A] = (A,Int,Int)
  
  case class Context(xs:Set[(Nt,T[S_rep])])
  
  // context definitions; a context is a set; irrelevant parts are removed
    def replace_string_with_box_key[S](i0:BB[S],ss:T[S]): T[S_rep] = {
      ss match {
        case (s,l,h) => (map(i0,s), l, h)
      }
    }
  
    def normalize_context[S](i0:BB[S],lc:Context,ss:T[S]) : Context = {
      val ss2 = replace_string_with_box_key(i0,ss) 
      lc match {
        case Context(xs) => Context(xs.filter({ case (nt,ss3) => ss3 == ss2 }))
      }
    }
  
    def update_context[S](i0:BB[S],lc:Context,x:(Nt,T[S])) : Context = {
      val lc2 = normalize_context(i0,lc,x._2) match { case Context(lc) => lc }
      val new_elt = replace_string_with_box_key(i0,x._2)
      Context(lc2 + ((x._1,new_elt))) // note double bracket
    }
    
    def context_contains[S](i0:BB[S],lc:Context,x:(Nt,T[S])) : Boolean = {
      val ss2 = replace_string_with_box_key(i0,x._2)
      lc match {
        case Context(lc) => lc.contains((x._1,ss2))
      }
    }
  }
  
  trait P3_context_comp {
    val p3_context_comp : P3_context
  }
    
}
