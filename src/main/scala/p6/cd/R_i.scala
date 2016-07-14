package p6.cd

import p6.bc._

// FIXME these R classes aren't doing much
object R_i extends R_t {

  type nt_id = Int

  type tm_id = Int
  
  object d { // debugging
    var id = 0  // to track object creation
  }

  sealed abstract class sym
  case class tm(tm_id:tm_id) extends sym {
    override def toString = s"""TM($tm_id)"""
  }
  case class nt(nt_id: nt_id, alts: () => alts) extends sym {
    val did = d.id
    d.id = d.id + 1
    override def toString() : String = s"""NT($nt_id/$did)"""
  }
  type rhs = List[sym] // non-empty
  type alts = List[rhs] // may be empty
  
  class rhs_obj2(val rhs: rhs) extends rhs_obj {
    def ~(x: sym): rhs = (rhs :+ x)
  }

  class alts_obj2(val alts: alts) extends alts_obj {
    def |(x: rhs): alts = (alts :+ x)
  }
  
  implicit def rhs_to_obj(x: rhs): rhs_obj = {
    new rhs_obj2(x)
  }
  
  implicit def alts_to_obj(x: alts): alts_obj = {
    new alts_obj2(x)
  }

  implicit def sym_to_rhs(x:sym):rhs = List(x)
  implicit def sym_to_rhs_obj(x:sym) : rhs_obj = sym_to_rhs(x)
  implicit def sym_to_alts(x:sym) : alts = rhs_to_alts(x)
  implicit def sym_to_alts_obj(x:sym) : alts_obj = sym_to_alts(x)

  implicit def rhs_to_alts(x: rhs): alts = List(x)
  implicit def rhs_to_alts_obj(x: rhs): alts_obj = rhs_to_alts(x)


  def alts_to_nt(i: nt_id, alts: () => alts): nt = {
    nt(i, alts)
  }

}