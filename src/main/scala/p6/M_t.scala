package p6


// trait combining rules and actions
trait M_t {

  import Squiggle._

  type nt_id
  
  type input[S]

  type sym[S, A]//  = P[S, A]

  type P[S, A] = sym[S,A] // parsers on input S, producing List[A]
  
  type rhs[S, A]

  abstract class rhs_obj[S, A] {
    def ~[B](x: sym[S, B]): rhs[S, ~[A, B]]
    def ^^[B](f: A => B): rhs[S, B]
  }

  type alts[S, A]

  abstract class alts_obj[S, A] {
    def |(x: rhs[S, A]): alts[S, A]
  }

  implicit def rhs_to_obj[S, A](x: rhs[S, A]): rhs_obj[S, A]
  implicit def sym_to_rhs_obj[S, A](x: sym[S, A]): rhs_obj[S, A]

  implicit def alts_to_obj[S, A](x: alts[S, A]): alts_obj[S, A]
  implicit def rhs_to_alts_obj[S, A](x: rhs[S, A]): alts_obj[S, A]

  def alts_to_sym[S, A](x:nt_id, alts: () => alts[S, A]): sym[S, A]

  //val parse_1: P[String, Int]
  //val parse_eps: P[String, Unit]

}

trait test_a {
  import Squiggle._

  val a: M_t
  import a._
  lazy val i: nt_id = (throw new Exception())
  val e: sym[String, Int] = alts_to_sym(i,
    () => {
      val r1 = (e ~ e ~ e) ^^ ((_) => 1)
      val r2 = (e ~ e) ^^ ((_) => 1)
      r1 | r2
    })

}