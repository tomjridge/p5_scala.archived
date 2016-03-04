package e4.core

object Core_types {
	
	def debugln(s:String) {}
	
	type i_t = Int
	type j_t = Int
	type k_t = Int
	
	trait Substring_t {
		type string_t
		type substring
		def to_substring(x:string_t,z:Int,y:Int) : substring
	}
	
	trait Symbol_t {
		type nt
		type tm
		type sym
		sealed abstract class sym_ab
		case class NT(val nt:nt) extends sym_ab
		case class TM(val tm:tm) extends sym_ab
		
		def sym_case(x:sym) : sym_ab
		def sym_of_tm(x:tm) : sym
	}
	
	trait Item_t { self: Symbol_t =>
		type nt_item
		def bs_nil(x:nt_item): Boolean
		def hd_bs(x:nt_item) : sym
		def nitm_dot_i(x:nt_item): i_t
		def nitm_dot_k(x:nt_item): k_t
		def nitm_dot_nt(x:nt_item): nt
		
		def to_string(nitm:nt_item): String
	}
	
	trait Input_t { self: Substring_t =>
		case class Input(str:string_t, len:Int)
	}
	
	
	trait Grammar_t { self: Substring_t with Symbol_t with Item_t =>
		case class Grammar(
				nt_items_for_nt: (nt,(string_t,Int)) => List[nt_item],
				p_of_tm: (tm => substring => List[Int]))
		
	}
	
	trait Ctxt_t { self: Input_t with Grammar_t => 
		case class Ctxt(g0:Grammar,i0:Input)
	}
	
	
	trait Earley_state_t { self: Symbol_t with Item_t =>
  	type nt_item_set_t
		def nt_item_set_fold[A](f:(A,nt_item) => A,y:A,x:nt_item_set_t) : A
		def nt_item_set_is_empty(x:nt_item_set_t) : Boolean
		def nt_item_set_to_list(x:nt_item_set_t) : todo_t
		def nt_item_set_to_done(x:nt_item_set_t): todo_done_t

		type todo_t     // = List[nt_item]
		def todo_is_empty(x:todo_t) : Boolean

		type todo_done_t    // = nt_item_set_t
		type todo_gt_k_t
		
		type ixk_t = (i_t,nt) 
		type ixk_done_t
		val ixk_set_empty: ixk_done_t
		
		type ktjs_t
		val ktjs_empty: ktjs_t

		type bitms_lt_k_t
		type bitms_at_k_t
		val bitms_at_k_empty: bitms_at_k_t
		def bitms_lt_k_add(x:bitms_lt_k_t, k:k_t, y:bitms_at_k_t) : bitms_lt_k_t		

		
	  case class State(
			var k: Int,
			var todo:todo_t,
			var todo_done: todo_done_t,
			var todo_gt_k:todo_gt_k_t,
			var ixk_done:ixk_done_t,
			var ktjs:ktjs_t,
			var bitms_lt_k: bitms_lt_k_t,
			var bitms_at_k: bitms_at_k_t)
		
		
		def bitms(x:State,y:(k_t,nt)) : nt_item_set_t 
	  def add_bitm_at_k(x:nt_item,y:nt,z:State) : State
	  def pop_todo(x:State) : (nt_item,State)
	  def cut(x:nt_item,y:j_t) : nt_item
	  def add_todo(x:nt_item,y:State) : State
	  
	  def add_ixk_done(x:ixk_t,y:State) : State
	  def mem_ixk_done(x:ixk_t,y:State) : Boolean
	  
	  def find_ktjs(x:tm,y:State) : Option[List[Int]]
		def add_ktjs(tm:tm,js:List[Int],x:State): State
		
		def todo_gt_k_find(x:k_t,y:todo_gt_k_t): nt_item_set_t 
		//def nt_item_set_to_list(x:nt_item_set_t) : List[nt_item]
		//def todo_to_done(x:todo_t) : todo_done_t
	}
}