package p6

trait R_t {

  type nt_id // Int

  type sym
  type nt <: sym
  type tm <: sym

  type rhs // = List[sym]
  
  // alts are lists of rhs
  type alts // = List[rhs] 

  // following implicits 

  abstract class rhs_obj {
    def ~(x: sym): rhs
  }

  abstract class alts_obj {
    def |(x: rhs): alts
  }

  implicit def rhs_to_obj(x: rhs): rhs_obj
  implicit def alts_to_obj(x: alts): alts_obj

  // lift all types to "larger" types
  implicit def sym_to_rhs(x:sym) : rhs
  implicit def sym_to_rhs_obj(x: sym): rhs_obj
  implicit def sym_to_alts(x:sym) : alts
  implicit def sym_to_alts_obj(x:sym) : alts_obj
  
  implicit def rhs_to_alts(x:rhs) : alts
  implicit def rhs_to_alts_obj(x: rhs): alts_obj

  def alts_to_nt(i: nt_id, alts: () => alts): nt

}

