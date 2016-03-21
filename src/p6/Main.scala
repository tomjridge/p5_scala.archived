package p6

/*
object Main {

  import A_i._
  val r0 = R_i

  type oracle_t = {
    def split(ss: List[r0.sym], s: r0.sym): List[Int]
  }

  case class input[S](s: S, i: Int, j: Int, o: oracle_t, c: Ctxt.Ctxt) extends A_i.input[S] {
    def split(ss: List[r0.sym], s: r0.sym): List[Int] = {
      o.split(ss, s)
    }
    def with_c(c:Ctxt.Ctxt) :input[S] = {
      this.copy(c=c)
    }
    def with_i(i: Int): input[S] = {
      this.copy(i = i)
    }
    def with_j(j: Int): input[S] = {
      this.copy(j = j)
    }
  }

  /*
  def print_sym_rules[S,A](x:sym[S,A]) = {
    val xs = x.alts()
    (s"""(${r0.print_sym(x.r_sym)} -> ${print_alts(xs)})""")
  }
  
  def print_alts[S,A](x:alts[S,A]) = {
    x.alts.map(print_rhs(_))
  }
  
  def print_rhs[S,A](x:rhs[S,A]) = {
    x.r_rhs().map { r0.print_sym(_) }
  }
  
  def sym_debug[S,A](x:sym[S,A]) = {
    (x,x.alts().alts.map( _.r_rhs()))
  }
  * 
  */
  
  /*
  val p_1:sym[String,Int] = new sym[String,Int] {
    def apply_actions[
  }
  * 
  */

  val e: sym[String,Int] = alts_to_sym(0,  () => {
        val r1 = (e ~ e ~ e) ^^ ((_) => 1)
        val r2 = (e ~ e) ^^ ((_) => 1)
        r1 | r2 })

  def main(args: Array[String]) {

    /* so does this
    object d {
      val e: sym[String,Int] = alts_to_sym(0,  () => {
        val r1 = (e ~ e ~ e) ^^ ((_) => 1)
        val r2 = (e ~ e) ^^ ((_) => 1)
        r1 | r2 })
      /*
      val e: sym[String,Int] = alts_to_sym(0,  () => {
        val r1 = (d.this.e ~ d.this.e ~ d.this.e) ^^ ((_) => 1)
        val r2 = (d.this.e ~ d.this.e) ^^ ((_) => 1)
        r1 | r2 })
        * 
        */
    }
        val e = d.e
    
    * 
    */
    


    println(e)
    println(e)
    println(print_sym_rules(e))
    println(print_sym_rules(e))
    val u = sym_debug(e)
    println(u)
  }

}
*/