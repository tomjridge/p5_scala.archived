package p6

import p6.bc._
import p6.cd._
import p6.de._

object Test {

  // test R
  trait test_ab {
    val r: R_t
    import r._
    val i: nt_id
    val _1: tm
    val eps: tm
    val e: sym = alts_to_nt(i, () => (e ~ e ~ e | _1 | eps))
  }
  object test_bc extends test_ab {
    val r = R_i
    import r._
    val i = 1
    val _1: tm = tm(2)
    val eps: tm = tm(3)
  }

  // test A_t
  trait test_cd {

    import Squiggle._

    val a: A_t { type nt_id = Int }
    import a._
    val i = 0
    val _1: tm[String, Int]
    val eps: tm[String, Unit]
    val e: nt[String, Int] = alts_to_nt(i, () => {
      (e ~ e ~ e) ^^ (_ => 1) |
        _1 |
        eps ^^ (_ => 0)
    })
  }

  // test A_i
  object test_de {
    import Squiggle._
    import A_i._

    val r0 = R_i

    val eps = mk_tm(1, Raw_parser.eps)
    val x = mk_tm(3, Raw_parser.x)
    val e: nt[String, Int] = alts_to_nt(0, () => {
      (e ~ e ~ e) ^^ { case x ~ y ~ z => x + y + z } |
        x |
        eps
    })

    val o: Input.oracle_t = {
      import r0._
      (i: Int, syms: List[sym], sym: sym, j: Int) => {
        (syms, sym) match {
          //case (List(nt(0, _)), nt(0, _)) => ((i) to (j)).toList
          case(List(nt(0,_)),_) => (i to j).toList
          case(_,nt(0,_)) => (i to j).toList
          case _ => {
            assert(false)
            List()
          }
        }
      }
    }
    val i0: input[String] = Input.mk_input("xxx", o)
    def main() {
      println(e.apply_actions(i0))
    }
  }

  def main(args: Array[String]) {
    test_de.main()
  }

}