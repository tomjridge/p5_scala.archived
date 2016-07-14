
package e3


object E3_simple_impl {

  import E3_core_types._
  
  object e3_core_impl extends E3_core {

    
    // following needs nt_items_for_nt and p_of_tm
    class Default_ops extends Ops_component {
      type nt = Int // assumed even
      type tm = Int // assumed odd
      type sym = Int
      type tm_item = (tm,Int)
      type sym_item = (sym,Int,Int)
      type sym_list = List[sym]
      type nt_item = (nt,sym_list,sym_list,Int,Int)
      
    def is_NT(x:sym) = (x % 2 == 0)  
    def sym_to_string(x:sym) = {
			if (is_NT(x)) s"NT $x" else s"TM $x"
		}
		def sym_list_to_string(x:List[sym]) = {
			x.map(sym_to_string _).mkString("[", ",", "]")
		}

		
		  def nt_item_to_string(x:nt_item) : String = {
      	s"""(${x._1} ${x._4} ${sym_list_to_string(x._2)} ${x._5} ${sym_list_to_string(x._3)})"""
      }
      
      type item = Item
      val sym_case = (s:sym) => (if (s % 2 == 0) (NT(s)) else (TM(s))):Sym
      val sym_of_tm = (s:tm) => s
      val mk_tm_coord = (tm:tm,i:Int) => (tm,i)
      val tm5 = (i:tm_item) => i._1
      val mk_sym_coord = (x:sym,i:Int,j:Int) => (x,i,j)
      val sym6 = (x:sym_item) => x._1
      val nt2 = (x:nt_item) => x._1
      val shift_a2_b2_c2 = (x:nt_item) => x match {
        case (nt,as,b::bs,i,j) => (nt,b::as,bs,i,j) // NB "as" stored in reverse order
        case (nt,as,Nil,i,j) => throw new Exception("impossible")
      }
      val b2_nil = (x:nt_item) => x match {
        case (nt,as,bs,i,j) => bs.equals(Nil)
      }
      val a2 = (x:nt_item) => x match {
        case (nt,as,bs,i,j) => as
      }
      val hd_b2 =  (x:nt_item) => x match {
        case (nt,as,bs,i,j) => bs.head
      }
      val mk_item = (x:Item) => x
      val dest_item = (x:item) => x
      val tm_dot_i9 = (x:tm_item) => x._2
      val sym_dot_i9 = (x:sym_item) => x._2
      val sym_dot_j9 = (x:sym_item) => x._3
      val nt_dot_i9 = (x:nt_item) => x match {
        case (nt,as,bs,i,j) => i
      }
      val nt_dot_j9 = (x:nt_item) => x match {
        case (nt,as,bs,i,j) => j
      }
      val with_j9 = (x:nt_item) => (j:Int) => x match {
        case (nt,as,bs,i,_) => (nt,as,bs,i,j)
      }
    }

    // the following classes incrementally add the different values we require
    class Default_has_ops_component extends Has_ops_component {
      val ops5 = new Default_ops
    }
    
    // FIXME none of the set and map defns depend on the nature of the types in ops; ideally these defns should be made using abstract repns and mixed in at the end
    trait Ctxt_set_comp extends Has_sets_component { this:Has_ops_component =>
      object impl extends set_todo_done {
        type t = Set[ops5.item]
        val std_empty = (_:Unit) => (Set()):t
        val std_add = (x:elt) => (s:t) => s + x
        val std_mem = (x:elt) => (s:t) => s(x)
      };
      val sets = new ctxt_set {
        val set_todo_done = impl
      }
    }
    
    trait Ctxt_map_comp extends Ctxt_set_comp with Has_maps_component { this:Has_ops_component =>
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
    
    /*
     class Ty_ctxt_comp extends Ctxt_map_comp with Ctxt_component {
     val s = "1" * 100
     val string5=s
     val length5=s.length()
     }
     * 
     */
    
    class Default_ctxt_component extends Default_has_ops_component with Ctxt_map_comp with Ctxt_component
    
    val ctxt = new Default_ctxt_component
    
  } // e3_core_impl
  
}
