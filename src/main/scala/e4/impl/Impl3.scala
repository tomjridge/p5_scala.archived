package e4.impl

import scala.collection.immutable.Map
import scala.collection.immutable.Set

import e4.core.Core_types._
import e4.core.Core._

object Impl3 {
	

	
	class Impl_t 
		extends Substring_t with Symbol_t with Item_t 
	    with Input_t with Grammar_t with Ctxt_t with Earley_state_t with Core_t
	{   
		type string_t = String
		type substring = (String,Int,Int)
		def to_substring(s:string_t,i:Int,j:Int) = ((s,i,j):substring)
		
		type nt = Int // even
		type tm = Int // odd
		//sealed abstract class sym_ab
		//case class NT(nt:nt) extends sym_ab
		//case class TM(tm:tm) extends sym_ab
		type sym = Int
		def is_NT(x:sym) = (x % 2 == 0)
		
		def sym_case(x:sym) = {
			if(is_NT(x)) NT(x) else TM(x)
		}
		def sym_of_tm(x:tm) = x
		
		type nt_item = (nt,List[sym],List[sym],i_t,k_t)
		@inline final def bs_nil(x:nt_item) = x._3.isEmpty
		def hd_bs(x:nt_item) = x._3.head
		def nitm_dot_i(x:nt_item) = x._4
		def nitm_dot_k(x:nt_item) = x._5
		def nitm_dot_nt(x:nt_item) = x._1
		def sym_to_string(x:sym) = {
			if (is_NT(x)) s"NT $x" else s"TM $x"
		}
		def sym_list_to_string(x:List[sym]) = {
			x.map(sym_to_string _).mkString("[", ",", "]")
		}
		def to_string(x:nt_item) = {
			s"""(${x._1} ${x._4} ${sym_list_to_string(x._2)} ${x._5} ${sym_list_to_string(x._3)})"""
		}
		
		type nt_item_set_t = Set[nt_item]
		val nt_item_set_empty : nt_item_set_t = Set()
		def nt_item_set_fold[A](f:(A,nt_item) => A,y:A,x:nt_item_set_t) : A = x.foldLeft(y)(f)
		def nt_item_set_is_empty(x:nt_item_set_t) : Boolean = x.isEmpty
		def nt_item_set_to_done(x:nt_item_set_t) : todo_done_t = x
		def nt_item_set_to_list(x:nt_item_set_t) : todo_t = x.toList
		
		type todo_t = List[nt_item]
		def todo_is_empty(x:todo_t) = x.isEmpty
		
		type todo_done_t = Set[nt_item]
		
		type todo_gt_k_t = Map[Int,nt_item_set_t]
		
		type ixk_done_t = Set[ixk_t]
		
		val ixk_set_empty: ixk_done_t = Set()
		
		type ktjs_t = Map[tm,Option[List[Int]]]
		
		val ktjs_empty : ktjs_t = Map().withDefaultValue(None)
		
		type bitms_lt_k_t = Map[(k_t,nt),nt_item_set_t]
		
		type bitms_at_k_t = Map[nt,nt_item_set_t]
		
		val bitms_at_k_empty: bitms_at_k_t = Map().withDefaultValue(nt_item_set_empty)
		
		def bitms_lt_k_add(x:bitms_lt_k_t, k:k_t, y:bitms_at_k_t) : bitms_lt_k_t = {
			def f(x:bitms_lt_k_t,y:(nt,nt_item_set_t)) = {
				val (nt,nts) = y
				val key = (k,nt)
				x + (key -> nts)
				/*
				val s : nt_item_set_t = x.apply(key)
				val s2 = s union nts
				val x2 : bitms_lt_k_t = x + (key -> s2)
				x2
				* 
				*/
			}
			y.foldLeft(x)(f)
		}
		
		def bitms(s0:State,y:(k_t,nt)) : nt_item_set_t = {
			val (k,x) = y
			(k==s0.k) match {
				case true => s0.bitms_at_k(x)
				case false => s0.bitms_lt_k((k,x))
			}
		}
		
	  def add_bitm_at_k(nitm:nt_item,x:nt,s0:State) : State = {
	  	val m = s0.bitms_at_k
	  	val s = m(x)
	  	val s2 = s + nitm
	  	val m2 = m + (x -> s2)
	  	//s0.copy(bitms_at_k=m2)
	  	s0.bitms_at_k=m2
	  	s0 
	  }
	  
	  @inline final def pop_todo(s0:State) : (nt_item,State) = {
	  	s0.todo match {
	  		case Nil => (throw new Exception("pop_todo"))
	  		case x::xs => {
	  			//s0.copy(todo=xs)
	  			s0.todo=xs
	  			(x,s0)
	  		}
	  	}
	  }
	  
	  def cut(x:nt_item,j:j_t) : nt_item = {
	  	x match {
	  		case (nt,as,s::bs,i,k) => {
	  			(nt,s::as,bs,i,j)
	  		}
	  		case _ => (throw new Exception("cut"))
	  	}
	  }
	  
	  def add_todo(nitm:nt_item,s0:State) : State = {
	  	debugln("add_todo: "+nitm)
	  	val k = s0.k
	  	val nitm_k = nitm._5
	  	(nitm_k > k) match {
	  		case true => {
	  			val m = s0.todo_gt_k
	  			val s = m.apply(nitm_k)
	  			val s2 = s+nitm
	  			val m2 = m + (nitm_k -> s2)
	  			//s0.copy(todo_gt_k=m2)
	  			s0.todo_gt_k=m2
	  			s0
	  		}
	  		case false => {
	  			s0.todo_done.contains(nitm) match {
	  				case true => s0
	  				case false => {
	  					//s0.copy(todo=nitm::s0.todo, todo_done = s0.todo_done + nitm)
	  					s0.todo=nitm::s0.todo
	  					s0.todo_done = s0.todo_done + nitm
	  					s0
	  				}
	  			}
	  		}
	  	}
	  }
	  
	  def add_ixk_done(x:ixk_t,s0:State) : State = {
	  	debugln("add_ixk_done: "+x)
	  	//s0.copy(ixk_done = s0.ixk_done + x)
	  	s0.ixk_done = s0.ixk_done + x
	  	s0
	  }
	  def mem_ixk_done(x:ixk_t,s0:State) : Boolean = {
	  	s0.ixk_done.contains(x)
	  }
	  
	  def find_ktjs(x:tm,s0:State) : Option[List[Int]] = {
	  	s0.ktjs(x)
	  }
		def add_ktjs(t:tm,js:List[Int],s0:State): State = {
			//s0.copy(ktjs=s0.ktjs + (t -> Some(js)))
			s0.ktjs=s0.ktjs + (t -> Some(js))
			s0
		}
		
		def todo_gt_k_find(x:k_t,y:todo_gt_k_t): nt_item_set_t = {
			y.apply(x)
		}
		
		def init_state(c0:Ctxt,nt:nt) :State = {
			val (i,k) = (0,0)
			val init = (nt,List(),List(nt),i,k)
			val todo = List(init)
			val todo_done = nt_item_set_empty
			val todo_gt_k : todo_gt_k_t = Map().withDefaultValue(nt_item_set_empty)
			val ixk_done : ixk_done_t = Set()
			val ktjs : ktjs_t = Map().withDefaultValue(None)
			val bitms_lt_k : bitms_lt_k_t = Map().withDefaultValue(nt_item_set_empty)
			val bitms_at_k : bitms_at_k_t = Map().withDefaultValue(nt_item_set_empty)
			State(k,todo,todo_done,todo_gt_k,ixk_done,ktjs,bitms_lt_k,bitms_at_k)
		}
		
		
	}
	
	def main(args:Array[String]) {
		val i_ab = new Impl_t()
		import i_ab._
		val eps = 1
		val parse_eps: substring => List[Int] = (s:substring) =>  {
			val (i,j) = (s._2,s._3)
			if (i<=j) List(i) else List() 
		}
		val x = 3
		val parse_x: substring => List[Int] = (s0:substring) =>  {
			val (s,i,j) = (s0._1,s0._2,s0._3)
			if (i<j /* && i < s.length() */ && s(i) == 'x') List(i+1) else List() 
		}
		def p_of_tm(tm:tm) = {
			if (tm == eps) parse_eps
			else if (tm == x) parse_x
			else (throw new Exception("p_of_tm"))
		}
		
		def i0(str:string_t) = Input(str,str.length())
		
		val e = 2
		
		def nt_items_for_nt(nt:nt,si:(string_t,Int)) : List[nt_item] = {
			val (s,i) = si
			val as = List()
			val k = i
			List(
					(nt,as,List(e,e,e),i,k),
					(nt,as,List(x),i,k),
					(nt,as,List(eps),i,k))
		}
		
		val g0 = Grammar(nt_items_for_nt, p_of_tm)
		
		def c0(str:string_t) = Ctxt(g0,i0(str))
		
		import p5.examples.Timing
		
		val s = "x"*100
		val c = c0(s)
		def do_work = {
			println("Start")
			val r = fg_earley(c,e)
			println("Finished")
    }
		//do_work
    Timing.time(4,do_work)
    e4.core.Core_log.ts = List()
    Timing.time(1,do_work)
    e4.core.Core_log.print_logs()
    // length 100, Elapsed time: 14127525272 ns (14.127525272 secs) (avg: 1.4127525271999999)
    // 200, impl2: Elapsed time: 40120022336 ns (40.120022336 secs) (avg: 10.030005584)
	}
  
}