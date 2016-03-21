package e3



object E3_core_types { // we define as a trait so we can have declared but undefined members
  
  
    trait Ops_component {
      type nt
      type tm
      type sym
      type nt_item  
      def nt_item_to_string(x:nt_item): String
      type tm_item
      type sym_item
      type sym_list
      type item
      sealed abstract class Sym
      case class NT(nt:nt) extends Sym
      case class TM(tm:tm) extends Sym
      
      sealed abstract class Item
      case class NTITM(x:nt_item) extends Item
      case class TMITM(x:tm_item) extends Item

      val sym_case: sym => Sym
      val sym_of_tm: tm => sym
      val mk_tm_coord: (tm,Int) => tm_item
      val tm5: tm_item => tm
      val mk_sym_coord: (sym,Int,Int) => sym_item
      val sym6: sym_item => sym
      val nt2: nt_item => sym
      val shift_a2_b2_c2: nt_item => nt_item
      val b2_nil: nt_item => Boolean
      val a2: nt_item => sym_list
      val hd_b2: nt_item => sym
      val mk_item: Item => item
      val dest_item: item => Item
      val tm_dot_i9: tm_item => Int
      val sym_dot_i9: sym_item => Int
      val sym_dot_j9: sym_item => Int
      val nt_dot_i9: nt_item => Int
      val nt_dot_j9: nt_item => Int
      val with_j9: nt_item => Int => nt_item

    }

    
    trait Has_ops_component {
    val ops5: Ops_component
    }
    
    /*
  trait External_ops_component extends Ops_component with Has_string_component {
  val string5: String_t
    val length5: Int{
      override def 
      override def hashCode = tp_tm.hashCode()
    }
    val nt_items_for_nt: nt => string_component.Substring => List[nt_item]
    val p_of_tm: tm => string_component.Substring => List[Int]
  }


  trait Has_external_ops_component {
    val ops5: External_ops_component
  }
  * 
  */


  trait Has_sets_component { this:Has_ops_component =>
    import E3_map_set_types._
    // refine set type
    trait set_todo_done extends std {
      type elt = ops5.item
    }
    trait ctxt_set {
      val set_todo_done: set_todo_done
    }
    val sets: ctxt_set
  }

  
  
  trait Has_maps_component { this:Has_ops_component =>
    import E3_map_set_types._
    // refine the map types
    trait map_blocked_key extends mbk {
      type key = (Int,ops5.sym)
      type value = ops5.nt_item
    }
    trait map_complete_key extends mck {
      type key = (Int,ops5.sym)
      type value = Int
    }
    trait map_sym_sym_int_int extends mssii {
      type key = (ops5.sym_list,ops5.sym,Int,Int)
      type value = Int
    }
    trait map_tm_int extends mti {
      type key = (ops5.tm,Int)
      type value = Int
    }
    trait ctxt_map {
      val map_blocked_key: map_blocked_key
      val map_complete_key: map_complete_key
      val map_sym_sym_int_int: map_sym_sym_int_int
      val map_tm_int: map_tm_int
    }
    val maps: ctxt_map
  }  

  trait Ctxt_component 
    extends Has_ops_component with Has_sets_component with Has_maps_component { 
    
  }
  
  trait Has_ctxt_component {
    val ctxt: Ctxt_component
  }
  


}
