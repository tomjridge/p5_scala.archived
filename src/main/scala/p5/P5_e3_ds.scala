package p5

import e3._

object P5_e3_ds {
  
  def println(x:Any) = {} // turn off debugging
  
  import E3_core_types._
  
  class Impl_e3_core[S] extends E3_core {
    
    class Impl_ops_component extends Ops_component {
      import P3_core.code._
      type nt = Nt_plus[S]
      type tm = Tm_plus[S]
      type sym = Sym_plus[S]
      type sym_list = Ty_rhs_syms_list[S]
      type sym_item = (sym,Int,Int)
      type tm_item = (tm,Int)
      type nt_item = (nt,sym_list/* reversed! */,sym_list,Int,Int)
      type item = Item
      
      def nt_item_to_string(x:nt_item) = { "FIXME" }
      
      val sym_case = (x:sym) => x match {
        case NP(np) => NT(np)
        case TP(tp) => TM(tp)
      }
      val sym_of_tm = (x:tm) => TP(x)
      val mk_tm_coord = (tm:tm,i:Int) => (tm,i):tm_item
      val tm5 = (x:tm_item) => x._1
      val mk_sym_coord = (s:sym,i:Int,j:Int) => (s,i,j):sym_item
      val sym6 = (sij:sym_item) => sij._1
      val nt2 = (x:nt_item) => NP(x._1)
      val shift_a2_b2_c2 = (x:nt_item) => x match {
        case (nt,as,bs,i,j) => {
          bs match {
            case Ty_rhs_syms_list(xs) => {
              xs match {
                case Nil => throw new Exception("shift_a2_b2_c2")
                case x::xs => as match {
                  case Ty_rhs_syms_list(ys) => {
                    (nt,Ty_rhs_syms_list(x::ys),Ty_rhs_syms_list(xs),i,j) // NB first arg is reversed; must be accounted for in oracle
                  }
                }
              }
            }
          }
        }
      }
      val b2_nil = (x:nt_item) => x._3 match {
        case Ty_rhs_syms_list(Nil) => true
        case _ => false
      }
      val a2 = (x:nt_item) => x._2
      val hd_b2 = (x:nt_item) => x match {
        case (nt,as,bs,i,j) => {
          bs match {
            case Ty_rhs_syms_list(x::xs) => x
            case Ty_rhs_syms_list(Nil) => throw new Exception("hd_b2")
          }
        }
      }
      val mk_item = (x:Item) => (x:item)
      val dest_item = (x:item) => (x:Item)
      val tm_dot_i9 = (x:tm_item) => x._2
      val sym_dot_i9 = (x:sym_item) => x._2
      val sym_dot_j9 = (x:sym_item) => x._3
      val nt_dot_i9 = (x:nt_item) => x._4
      val nt_dot_j9 = (x:nt_item) => x._5
      val with_j9 = ((x:nt_item) => (j:Int) => x match {
        case (nt,as,bs,i,_) => (nt,as,bs,i,j)
      })
      
      
    }
    
    
    trait Impl_has_ops_component extends Has_ops_component {
      val ops5 = new Impl_ops_component
    }
    
    // now need to refine has_sets_component
    trait Impl_has_sets_component extends Has_sets_component { this: Impl_has_ops_component =>
      object impl extends set_todo_done {
        type t = Set[ops5.item]
        val std_empty = (_:Unit) => (Set()):t
        val std_add = (x:elt) => (s:t) => s + x
        val std_mem = (x:elt) => (s:t) => s(x)
      }
      val sets = new ctxt_set {
        val set_todo_done = impl
      }
    }
    
    trait Impl_has_maps_component extends Has_maps_component { this:Impl_has_ops_component =>
      val mbk_impl = new E3_map_set_types.Default_map_impl with map_blocked_key
      val mck_impl = new E3_map_set_types.Default_map_impl with map_complete_key
      val mssii_impl = new E3_map_set_types.Default_map_impl with map_sym_sym_int_int
      val mti_impl = new E3_map_set_types.Default_map_impl with map_tm_int

      object maps extends ctxt_map {
        val map_blocked_key = mbk_impl
        val map_complete_key = mck_impl
        val map_sym_sym_int_int = mssii_impl
        val map_tm_int = mti_impl
      }
    }
    
    class Default_ctxt_component extends Ctxt_component with Impl_has_ops_component with Impl_has_sets_component with Impl_has_maps_component
    
    val ctxt = new Default_ctxt_component
    

  } // Impl_e3_core
  
  class Fix_e3_core[S](val dummy:S) {
    
    val impl_e3_core = new Impl_e3_core[S]
    
    import P3_core.code
    type Earley_result = impl_e3_core.Ty_loop2
    type Oracles = (code.ty_oracle[S],code.ty_tmoracle[S])
    
    def sops[A](s:S,len:Int,p:P3_core.code.Parser[S,A]) : impl_e3_core.sops_class.Sops[S] = {
      val sops2 = {
        import P3_core.code._
        //import impl_e3_core._
        import E3_substring.Substring
        val nt_items_for_nt = code.nt_items_for_nt[S] _
        val p_of_tm = code.p_of_tm[S] _
        val r = impl_e3_core.sops_class.Sops(
          s,
          len,
          p.spl,
          nt_items_for_nt,
          p_of_tm
        )
        r
      }
      sops2
    }
    

    
    def run_earley[A](s:S,len:Int,p:P3_core.code.Parser[S,A]) :Earley_result = {
      val sops2 = sops(s,len,p)
      val r = impl_e3_core.earley(sops2)
      //println("run_earley: "+r)
      r
    }
    
    
    def transform_earley_result_to_oracles(r:Earley_result) : Oracles = {  // FIXME this is all very inefficient
      import P3_core.code._
      val o : ty_oracle[S] =
        (syms:Ty_rhs_syms_list[S],sym:Sym_plus[S]) => (i:Int,j:Int) => {
          import impl_e3_core.ctxt
          println(s"o called: $syms $sym $i $j")
          val syms_rev = syms match { case Ty_rhs_syms_list(xs) => Ty_rhs_syms_list(xs.reverse)}
          val rs = ctxt.maps.map_sym_sym_int_int.mssii_elts_cod(syms_rev,sym,i,j)(r.oracle5)
          println(s"o returning: $rs")
          rs
        }
      val tmo: ty_tmoracle[S] = (tm:Tm_plus[S]) => (i:Int,j:Int) => {
        import impl_e3_core.ctxt
        val b = ctxt.maps.map_tm_int.map_find_cod((tm,i))(j)(r.tmoracle5)
        b
      }
      (o,tmo)
    }
    
    
    def run_actions[A](s:S,len:Int,p:code.Parser[S,A],otmo: Oracles) = {
      code.run_parser_1(p, s, len, otmo)
    }
    
    
    def run_parser[A](s:S,len:Int,p:code.Parser[S,A]) : List[A] = {
      val earley_result = run_earley(s,len,p)
      println(earley_result)
      val otmo = transform_earley_result_to_oracles(earley_result)
      val r = run_actions(s,len,p,otmo)
      r
    }
  }
  
}
