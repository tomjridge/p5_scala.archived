package p6

object Ctxt {

  type nt = Int

  // interface
  // do we bundle the operations with the object, or not? probably cleaner to bundle?

  trait Ctxt {
    // for memoization
    def normalize(i: Int, j: Int): Ctxt

    // assume we always work with normalized contexts
    def add(i: Int, nt: nt, j: Int): Ctxt

    def contains(i: Int, nt: nt, j: Int): Boolean
  }

  case class Ctxt_ab(c:Set[(Int, nt, Int)]) extends Ctxt {
    // for memoization
    def normalize(i: Int, j: Int): Ctxt = { // FIXME why can't this be Ctxt_ab?
      Ctxt_ab(c.filter({ case (a, nt, b) => { (a, b) == (i, j) } }))
    }

    // assume we always work with normalized contexts?
    def add(i: Int, nt: nt, j: Int): Ctxt = {
      Ctxt_ab(c + ((i, nt, j)))
    }

    def contains(i: Int, nt: nt, j: Int): Boolean = {
      c.contains((i, nt, j))
    }
  }

  def mk_ctxt: Ctxt = {
    (new Ctxt_ab(Set()))
  }

}