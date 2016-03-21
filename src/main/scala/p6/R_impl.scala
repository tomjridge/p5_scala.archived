package p6

object R_impl extends R {

  case class sym[S](alts:() => alts[S])
  type rhs[S] = List[sym[S]]
  type alts[S] = List[rhs[S]]
  class rhs_obj2[S](val rhs: rhs[S]) extends rhs_obj[S] {
    def ~(x: sym[S]): rhs[S] = (rhs :+ x)
  }

  class alts_obj2[S](val alts: alts[S]) extends alts_obj[S] {
    def |(x: rhs[S]): alts[S] = (alts :+ x)
  }

  //val tys = tys0
  //implicit def nt_to_sym[S](x:nt[S]) : sym[S] = x
  /*
  implicit def xnt_to_rhs_obj[S](x:nt[S]) : rhs_obj[S] = {
    val rhs :rhs[S] = List(x)
    val r : rhs_obj[S] = new rhs_obj[S](rhs)
    r
  }
  * 
  */

  implicit def rhs_to_rhs_obj[S](x: rhs[S]): rhs_obj[S] = {
    new rhs_obj2[S](x)
  }

  implicit def sym_to_rhs_obj[S](x: sym[S]): rhs_obj[S] = {
    rhs_to_rhs_obj[S](List(x))
  }

  implicit def alts_to_alts_obj[S](x: alts[S]): alts_obj[S] = {
    new alts_obj2[S](x)
  }
  
  implicit def rhs_to_alts_obj[S](x:rhs[S]) :alts_obj[S] = {
    alts_to_alts_obj(List(x))
  }
  
  def alts_to_sym[S](alts: () => alts[S]) : sym[S] = {
    sym[S](alts)
  }
  
  object test {
    val e: sym[Any] = alts_to_sym(() => (e ~ e ~ e | e ~ e))
  }
  
  def main(args:Array[String]) {
    println(test.e)
  }
}