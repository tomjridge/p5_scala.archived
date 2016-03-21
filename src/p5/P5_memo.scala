package p5

object P5_memo {
  
  import scala.collection.mutable
  
  // generic memo fun
  def memo[K,T,S](tbl:mutable.Map[K,T])(key_of_i:S=>Option[K])(f:S => T) = (i:S) => { 
    val k = key_of_i(i)
    k match {
      case None => (f(i))
      case Some(k) => {
        tbl.get(k) match {
          case None => {
            val r = f(i)
            tbl.put(k,r)
            r
          }
          case Some(r) => r
        }
      }
    } 
  }
  
  // simplified, no key_of_i
  def simple_memo[K,T](tbl:mutable.Map[K,T])(f:K => T) = (i:K) => { 
    memo[K,T,K](tbl)((x:K)=>Some(x))(f)(i) 
  }
  
  import P3_core.code
  type P3_key = (code.Context,code.bb.key,Int,Int)
  
  def key_of_input[S](i0:code.inr[S]) :Option[P3_key] = {
    val ss = i0.ss4
    val lc = code.normalize_context(i0, i0.lc4, ss)
    code.sp.dest_SS(ss) match {
      case (s,l,h) => {
        val k = (lc,code.bb.box_get_key(i0.box4),l,h)
        Some(k:P3_key)
      }
    }
  }
  
  
  def memo_tbl_p[S,A](tbl:mutable.Map[P3_key,code.outr[A]],p:code.Parser[S,A]) = {
    val act = p.act
    val act2 = memo[P3_key,code.outr[A],code.inr[S]](tbl)(key_of_input)(act)
    p.with_act { act2 }
  }
  
  def memo_p[S,A](p: => code.Parser[S,A]) = {
    lazy val p2 = { 
      val tbl = mutable.Map[P3_key,code.outr[A]]()
      memo_tbl_p(tbl,p)
    }
    p2
  }
  
}
