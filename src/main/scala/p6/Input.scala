package p6

import p6.cd.R_i

object Input {

  import R_i.sym
  import A_i.input

  type oracle_t = (Int,List[sym], sym,Int) => List[Int] // FIXME belongs here?
  
  case class in[S](
    i: Int,
    j: Int,
    c: Ctxt.Ctxt,
    s: S,
    o: oracle_t)
      extends input[S] {
    def split(i:Int,syms:List[sym], sym: sym,j:Int): List[Int] = o(i,syms,sym,j)
    def with_i(i: Int): input[S] = this.copy(i=i)
    def with_j(j: Int): input[S] = this.copy(j=j)
    def with_c(c: Ctxt.Ctxt): input[S] = this.copy(c=c)
  }
  
  def mk_input(s:String,o:oracle_t) : input[String] = {
    new in[String](0,s.length(),Ctxt.mk_ctxt,s,o)
  }
}