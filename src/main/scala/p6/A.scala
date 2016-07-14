package p6

// actions trait
trait A {

  import Squiggle._
  import Span._
  import Raw_parser._

  type nt_id
  type tm_id
  
  type sym[S, A] 
  type nt[S,A] <: sym[S,A]
  type tm[S,A] <: sym[S,A] 
  
  // NB not just spans
  def mk_tm[S,A](tm_id:tm_id,r:raw_parser[S,A]) : tm[S,A]  
  
  
  //type P[S, A] = sym[S,A] // parsers on input S, producing List[A]
  
  type rhs[S, A] 

  abstract class rhs_obj[S, A] {
    def ~[B](x: sym[S, B]): rhs[S, ~[A, B]]
    def ^^[B](f: A => B): rhs[S, B]
  }

  type alts[S, A] 

  abstract class alts_obj[S, A] {
    def |(x: rhs[S, A]): alts[S, A]
  }

  // add extra functionality
  implicit def rhs_to_obj[S, A](x: rhs[S, A]): rhs_obj[S, A]
  implicit def alts_to_obj[S, A](x: alts[S, A]): alts_obj[S, A]
  
  // want to have rhs X -> ... ~ Y
  // implicit def sym_to_rhs[S, A](x: sym[S, A]): rhs[S, A]
  
  
  // want ... | X
  implicit def sym_to_rhs[S,A](x:sym[S,A]) : rhs[S,A]
  // want _ -> X ~ ... 
  implicit def sym_to_rhs_obj[S, A](x: sym[S, A]): rhs_obj[S, A]
  // want ... | X | ...
  implicit def sym_to_alts[S,A](x:sym[S,A]) : alts[S,A]
  // want Y -> X | ...
  implicit def sym_to_alts_obj[S,A](x:sym[S,A]) : alts_obj[S,A]

  // want X -> rhs
  implicit def rhs_to_alts[S,A](x:rhs[S,A]) :alts[S,A]
  // want rhs | ...
  implicit def rhs_to_alts_obj[S, A](x: rhs[S, A]): alts_obj[S, A]
  
  def alts_to_nt[S, A](x:nt_id, alts: () => alts[S, A]): nt[S, A]

  //val parse_1: P[String, Int]
  //val parse_eps: P[String, Unit]

}

