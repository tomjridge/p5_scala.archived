package e4.core

import Core_types._

object Core {
	
  import Core_log._
  
  var i = 0
  
  trait Core_t { self: Substring_t with Symbol_t with Item_t with Input_t with Grammar_t with Ctxt_t with Earley_state_t =>
    
  	@inline final def fm(k:k_t) = (s1:State,bitm:nt_item) => {
  	  add_todo(cut(bitm,k),s1)
  	}
  	
    def step_k(c0:Ctxt) : State => State = (s00:State) => {
    	assert(log(P.ab))
    	//i=i+1
    	//if(i%1000 == 0) System.gc()
    	//assert(log(P.ac))
    	//debugln("step_k: "+s00.k+" "+s00.todo)
    	var s0 : State = s00
      val k = s0.k
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
          debugln("already_done: "+already_done)
          assert(log(P.cd))
          already_done match {
            case true => {
              s0
            }
            case false => {
              s0 = add_ixk_done((i,x),s0)
              /*
              def fm(s1:State,bitm:nt_item) = {
                add_todo(cut(bitm,k),s1)
              }
              * 
              */
              val fn = fm(k)
              val bitms_bc = bitms(s0,(i,x))
              val r = nt_item_set_fold(fn,s0,bitms_bc)
              assert(log(P.de))
              r
            }
          } // already_done
        } // true
        case false => {
          val bitm = nitm
          val s = hd_bs(bitm)
          debugln("sym_case(s): "+sym_case(s))
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
                    debugln("js: "+js)
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

    
    def loop_k(c0:Ctxt) : State => State = {
    	val sk = step_k(c0)
    	(s0:State) => {
    		debugln("loop_k: "+s0.k)
    		var s = s0
    		while(!(todo_is_empty(s.todo))) {
    			s = sk(s)
    		}
    		s
    	}
    }
    	
    
    def loop(c0:Ctxt,s0:State) : State = {
    	var s = s0
    	val lk = loop_k(c0)
    	while(!(s.k > c0.i0.len + 1)) {
    		s = lk(s)
    		val old_k =s.k
    		val k = s.k+1
    		val new_itms : nt_item_set_t = todo_gt_k_find(k,s.todo_gt_k)
    		val todo_done = nt_item_set_to_done(new_itms) 
    		val todo = nt_item_set_to_list(new_itms)
    		val todo_gt_k = s.todo_gt_k // FIXME remove old entries
    		val ixk_done = ixk_set_empty
    		val ktjs = ktjs_empty
    		val bitms_lt_k = bitms_lt_k_add(s.bitms_lt_k,old_k,s.bitms_at_k)
    		val bitms_at_k = bitms_at_k_empty
    		s = State(k,todo,todo_done,todo_gt_k,ixk_done,ktjs,bitms_lt_k,bitms_at_k)
    	}
    	s
    }
    
    def init_state(c0:Ctxt,nt:nt) : State
    
    def fg_earley(c0:Ctxt,nt:nt): State = {
      loop(c0,init_state(c0,nt))
    }
    
  } // Core_t

} // Core
