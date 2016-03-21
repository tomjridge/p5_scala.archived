package p6

trait R {

  //type nt[S]

  type sym[S]

  type rhs[S] // = List[sym[S]]

  // a rhs is a list of sym
  abstract class rhs_obj[S] {
    def ~(x: sym[S]): rhs[S]
  }

  // alts are lists of rhs
  type alts[S] // = List[rhs[S]] // List[rhs]

  abstract class alts_obj[S] {
    def |(x: rhs[S]): alts[S]
  }


  //implicit def nt_to_sym[S](x:nt[S]) : sym[S]
  //implicit def nt_to_rhs_obj[S](x:nt[S]) : rhs_obj[S]
  
  
  // inject sym to rhs
  //implicit def sym_to_rhs[S](x:sym[S]) : rhs[S] // need to move from sym to rhs_obj
  implicit def rhs_to_rhs_obj[S](x: rhs[S]): rhs_obj[S]  
  implicit def sym_to_rhs_obj[S](x:sym[S]) : rhs_obj[S]
  
  
  implicit def alts_to_alts_obj[S](x:alts[S]) :alts_obj[S]
  //implicit def rhs_to_alts[S](x:rhs[S]) :alts[S]
  implicit def rhs_to_alts_obj[S](x:rhs[S]) :alts_obj[S]
  
  
  def alts_to_sym[S](alts: () => alts[S]) : sym[S]  
  // what about the operations?

}


trait test {
  val r:R
  import r._
  val e : sym[Any] = alts_to_sym(() => (e ~ e ~ e | e ~ e))
}
