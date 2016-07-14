package p6

import p6.bc.Span
import p6.bc.Raw_parser
import p6.bc.Squiggle

object A_i extends A_t {

  import Squiggle._
  import Span._
  import Raw_parser._

  val r0 = R_i
  type nt_id = r0.nt_id
  type tm_id = r0.tm_id
  
  // what we need from inputs
  trait input[S] {
    def i:Int
    def j:Int
    def c:Ctxt.Ctxt
    def s:S
    def split(i:Int,x:List[r0.sym], y:r0.sym,j:Int): List[Int]
    def with_i(i:Int): input[S]
    def with_j(j:Int): input[S]
    def with_c(c:Ctxt.Ctxt) : input[S]
  }
  
  sealed abstract class sym[S,A] {
    val r_sym : r0.sym
    def apply_actions(i0:input[S]) : List[A]
  }
  case class nt[S, A](nt_id: r0.nt_id, alts: () => alts[S, A]) extends sym[S,A] {
    val r_sym : r0.sym = r0.nt(nt_id, () => alts().r_alts)
    def apply_actions(i0: input[S]): List[A] = {
      alts().apply_actions(i0) 
    }
  }
  // really want to separate the recognition from the generation of A FIXME
  case class tm[S,A](tm_id:r0.tm_id,r:raw_parser[S,A]) extends sym[S,A] {
    val r_sym : r0.sym = r0.tm(tm_id)
    def apply_actions(i0:input[S]) : List[A] = {
      r(Span(i0.i,i0.s,i0.j)).filter(_._2==i0.j).map(_._1) // FIXME inefficient - use tmoracle
    }
  }
  def mk_tm[S,A](tm_id:r0.tm_id,r:raw_parser[S,A]) = tm[S,A](tm_id,r)

  // for the rhs, we just need two sorts of object: one that calls sym.aa, and one that
  // splits and calls
  sealed abstract class rhs[S, A] {
    val r_rhs: r0.rhs
    def apply_actions(i0: input[S]): List[A]
  }
  case class rhs_1[S, A](sym: sym[S, A]) extends rhs[S, A] {
    import r0._
    val r_rhs : r0.rhs = sym.r_sym
    def apply_actions(i0: input[S]) = sym.apply_actions(i0)
  }
  case class rhs_2[S, A, B](rhs: rhs[S, A], sym: sym[S, B]) extends rhs[S, ~[A, B]] {
    val r_rhs : r0.rhs = {
      import r0._
      rhs.r_rhs ~ sym.r_sym
    }
    def apply_actions(i0: input[S]): List[~[A, B]] = {
      // we need to split the input and call the underlying actions and merge them
      val ks = i0.split(i0.i,rhs.r_rhs, sym.r_sym,i0.j)
      val rs = ks.map { k =>
        val rs2 = sym.apply_actions(i0.with_i(k))
        val rs1 = rhs.apply_actions(i0.with_j(k))
        val rs = for { x <- rs1; y <- rs2 } yield (new ~(x, y))
        rs
      }
      rs.flatten
    }
  }

  case class alts[S, A](alts: List[rhs[S, A]]) {
    val r_alts = alts.map(_.r_rhs)
    def apply_actions(i0: input[S]): List[A] = {
      alts.map(rhs => rhs.apply_actions(i0)).flatten
    }
  }

  class rhs_obj2[S, A](val rhs: rhs[S, A]) extends rhs_obj[S, A] {
    def ~[B](x: sym[S, B]): rhs[S, ~[A, B]] = {
      rhs_2[S, A, B](rhs, x)
    }
    def ^^[B](f: A => B): rhs[S, B] = {
      val r = new rhs[S, B] {
        val r_rhs = rhs.r_rhs
        def apply_actions(x: input[S]): List[B] = {
          rhs.apply_actions(x).map(f)
        }
      }
      r
    }
  }

  class alts_obj2[S, A](val alts: alts[S, A]) extends alts_obj[S, A] {
    def |(x: rhs[S, A]): alts[S, A] = { new alts[S, A](alts.alts :+ x) }
  }

  implicit def rhs_to_obj[S, A](x: rhs[S, A]): rhs_obj[S, A] = {
    new rhs_obj2(x)
  }
  implicit def alts_to_obj[S, A](x: alts[S, A]): alts_obj[S, A] = {
    new alts_obj2(x)
  }
  
  implicit def sym_to_rhs[S,A](x:sym[S,A]) : rhs[S,A] = rhs_1(x)
  implicit def sym_to_rhs_obj[S, A](x: sym[S, A]): rhs_obj[S, A] = rhs_to_obj(x)
  implicit def sym_to_alts[S,A](x:sym[S,A]) : alts[S,A] = rhs_to_alts(x)
  implicit def sym_to_alts_obj[S,A](x:sym[S,A]) : alts_obj[S,A] = sym_to_alts(x)
  
  implicit def rhs_to_alts[S,A](x:rhs[S,A]) :alts[S,A] = alts(List(x))
  implicit def rhs_to_alts_obj[S, A](x: rhs[S, A]): alts_obj[S, A] = rhs_to_alts(x)
  


  def alts_to_nt[S, A](nt_id: nt_id, alts: () => alts[S, A]): nt[S, A] = {
    //val nt = new r0.nt(nt_id, () => alts().r_alts)
    //val s1 = new nt[S, A](nt_id, alts)
    val s2 = new nt[S, A](nt_id, alts) {
      override def apply_actions(i0:input[S]) : List[A] = {
        // check context
        val (i,nt,j) = (i0.i,nt_id,i0.j)
        i0.c.contains(i,nt,j) match {
          case true => List()
          case false => {
            val c2 = i0.c.add(i,nt,j)
            super.apply_actions(i0.with_c(c2))
          }
        }
      }
    }
    s2
  }

}

