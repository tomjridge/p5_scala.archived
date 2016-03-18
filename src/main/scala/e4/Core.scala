package e4

import Core_types._
import Core_log._

abstract class Core {
  
  val at0 : All_traits 
    // = Impl.at1
  import at0._
  
  //val c0: Ctxt
  //val init_state: State
  
  def step_k(s00:State) : State = {
    assert(log(P.ab))
    //i=i+1
    //if(i%1000 == 0) System.gc()
    //assert(log(P.ac))
    //debugln("step_k: "+s00.k+" "+s00.todo)
    var s0 : State = s00
    val k = state_to_k(s0)
    //assert(log(P.ae))
    //val bitms_ab = bitms(s0)
    val (nitm,s0_ab) = pop_todo(s0)
    //assert(log(P.am))
    s0 = s0_ab
    //println(to_string(nitm))
    val complete = bs_nil(nitm)
    //debugln("complete: "+complete)
    assert(log(P.bc))
    complete match {
      case true => {
        val (i,x) = (nitm_dot_i(nitm),nitm_dot_nt(nitm))
        val already_done = mem_ixk_done((i,x),s0)  // FIXME combine with subsequent add to cut time in half
        //debugln("already_done: "+already_done)
        assert(log(P.cd))
        already_done match {
          case true => {
            s0
          }
          case false => {
            s0 = add_ixk_done((i,x),s0)
            def fm(s1: State, bitm: nt_item) = {
              add_todo(cut(bitm, k), s1)
            }
            val bitms_bc = bitms(s0, (i, x))
            val r = nt_item_set_fold(fm,s0,bitms_bc)
            assert(log(P.de))
            r
          }
        } // already_done
      } // true
      case false => {
        val bitm = nitm
        val s = hd_bs(bitm)
        //debugln("sym_case(s): "+sym_case(s))
        sym_case(s) match {
          case NT(y) => {
            assert(log(P.ef))
            val bitms_ab = bitms(s0,(k,y))
            val bitms_empty = nt_item_set_is_empty(bitms_ab)
            s0 = add_bitm_at_k(bitm,y,s0)
            assert(log(P.fg))
            bitms_empty match {
              case false => {
                mem_ixk_done((k,y),s0) match {
                  case true => (add_todo(cut(bitm,k),s0))
                  case false => s0
                }
              }
              case true => {
                val new_itms = c0.g0.nt_items_for_nt(y,(c0.i0.str,k))
                def fn(s1:State,nitm:nt_item) = (add_todo(nitm,s1))
                val r = new_itms.foldLeft(s0)(fn)
                assert(log(P.gh))
                r
              }
            }
          } // NT
          case TM(t) => {
            val ktjs = find_ktjs(t,s0)
            assert(log(P.hi))
            val (js,s0_bc) = {
              ktjs match {
                case None => {
                  val js = c0.g0.p_of_tm(t)(to_substring(c0.i0.str,k,c0.i0.len))
                  //debugln("js: "+js)
                  val s0_cd = add_ktjs(t,js,s0)
                  (js,s0_cd)
                }
                case Some(js) => (js,s0)
              }
            }
            s0 = s0_bc
            assert(log(P.ij))
            def fo(s1:State,j:Int) = add_todo(cut(bitm,j),s1)
            val r = js.foldLeft(s0)(fo)
            assert(log(P.jk))
            r
          } // TM
        } // sym_case
      } // false
    } // complete match
  } // step_k

  def loop_k(s0: State): State = {
    //debugln("loop_k: " + state_to_k(s0))
    var s = s0
    while (!(todo_is_empty(s))) {
      s = step_k(s)
    }
    s
  }
  
  def loop(s0: State): State = {
    var s = s0
    while (!(state_to_k(s) > c0.i0.len + 1)) {
      s = loop_k(s)
      s = inc_k(s)
    }
    s
  }
  
  def fg_earley(nt:nt): State = {
    loop(init_state(nt))
  }
  

} // Core
