package e3

trait E3_core {
  
  import E3_core_types._
  import E3_substring._
  
  // want to fix the context types, because ty_loop2 is dependent on these
  val ctxt: Ctxt_component
  
  object sops_class {
    import ctxt.ops5._
    case class Sops[S](
      val string5: S,
      val length5: Int,
      val init_sym: sym,
      val nt_items_for_nt: nt => Substring[S] => List[nt_item],
      val p_of_tm: tm => Substring[S] => List[Int])
  }
  
  case class Ty_loop2 (
    val todo_done5: ctxt.sets.set_todo_done.t,
    val todo5: List[ctxt.ops5.item],
    val oracle5: ctxt.maps.map_sym_sym_int_int.t,
    val tmoracle5: ctxt.maps.map_tm_int.t,
    val blocked5: ctxt.maps.map_blocked_key.t,
    val complete5: ctxt.maps.map_complete_key.t
  ) {
    override def toString = {
      s"""Ty_loop2(
todo_done5 = $todo_done5,
todo5 = $todo5,
oracle5 = $oracle5,
tmoracle5 = $tmoracle5,
blocked5 = $blocked5,
complete5 = $complete5)
"""
    }
  }

  val update_oracle = (m: ctxt.maps.map_sym_sym_int_int.t) =>
  (itm:ctxt.ops5.nt_item,l:Int) => {
    // val ops = item_ops5 this causes problems with type equality
    val (syms1,sym2) = (ctxt.ops5.a2(itm),ctxt.ops5.hd_b2(itm))
    val (i,k,j) = (ctxt.ops5.nt_dot_i9(itm),ctxt.ops5.nt_dot_j9(itm),l)
    val key = (syms1,sym2,i,j)
    val m0 = ctxt.maps.map_sym_sym_int_int.map_add_cod(key)(k)(m)
    m0
  }
  
  val update_tmoracle = (m:ctxt.maps.map_tm_int.t) => (tmij:(ctxt.ops5.tm,Int,Int)) => {
    val (tm,i,j) = tmij
    val key = (tm,i)
    val m0 = ctxt.maps.map_tm_int.map_add_cod(key)(j)(m)
    m0
  }
  
  val todo_is_empty = (s0:Ty_loop2) => {
    s0.todo5.isEmpty
  }
  
  val add_todo = (s0:Ty_loop2) => (itm:ctxt.ops5.item) => {
    //println("add_todo: "+itm)
    val s1 = s0.copy(todo5=(itm::s0.todo5))
    val s2 = s1.copy(todo_done5=ctxt.sets.set_todo_done.std_add(itm)(s1.todo_done5))
    s2
  }
  
  val pop_todo = (s0:Ty_loop2) => {
    s0.todo5 match {
      case Nil => throw new Exception("pop_todo")
      case x::xs => {
        val s1 = s0.copy(todo5=xs)
        (s1,x)
      }
    }
  }
  
  val cut = (bitm:ctxt.ops5.nt_item) => (citm_j:Int) => (s0:Ty_loop2) => {
    import ctxt.ops5
    val nitm = ops5.mk_item(ops5.NTITM({
      val f2 = ((x:ops5.nt_item) => ops5.with_j9(x)(citm_j))
      (ops5.shift_a2_b2_c2 andThen f2)(bitm)
    }))
    val s1 = (
      if (ctxt.sets.set_todo_done.std_mem(nitm)(s0.todo_done5)) s0 else
        add_todo(s0)(nitm)
    )
    s1
  }
  
  
  def loop2[S](sops:sops_class.Sops[S]) = (s0:Ty_loop2) => {
    import ctxt._
    import ctxt.maps._
    //import ctxt.{ops5 => ops}
    val (s00,itm0) = pop_todo(s0)
      (ops5.dest_item(itm0)) match {
      case (ops5.NTITM(nitm)) => {
        val complete = ops5.b2_nil(nitm)
        complete match {
          case true => {
            val citm_j = ops5.nt_dot_j9(nitm)
            val citm = ops5.mk_sym_coord(ops5.nt2(nitm),ops5.nt_dot_i9(nitm),citm_j)
            val k : (Int,ops5.sym) = (ops5.sym_dot_i9(citm), ops5.sym6(citm))
            val f1 = (bitm:ops5.nt_item) => (s1:Ty_loop2) => (cut(bitm)(citm_j)(s1))
            var s1 = s00
            s1 = (map_blocked_key.map_fold_cod(k)(f1)(s1.blocked5)(s1))
            // want to update the complete5 component of s01
            s1 = s1.copy(complete5 = (map_complete_key.map_add_cod(k)(citm_j)(s1.complete5)))
            val f2 = (bitm:ops5.nt_item) => (s1:Ty_loop2) => {
              val s11 = s1.copy(oracle5 = update_oracle(s1.oracle5)(bitm,ops5.sym_dot_j9(citm)))
              s11
            }
            s1 = map_blocked_key.map_fold_cod(k)(f2)(s1.blocked5)(s1)
            s1
          }
          case false => {
            val bitm = nitm
            val sym = ops5.hd_b2(bitm)
            val k = (ops5.nt_dot_j9(bitm),sym)
            val new_key = map_blocked_key.map_cod_empty(k)(s00.blocked5)
            var s1 = s00
            s1 = s1.copy(blocked5 = map_blocked_key.map_add_cod(k)(bitm)(s1.blocked5))
            val f3 = (citm_j:Int) => (s1:Ty_loop2) => cut(bitm)(citm_j)(s1)
            s1 = map_complete_key.map_fold_cod(k)(f3)(s1.complete5)(s1)
            val f4 = (citm_j:Int) => (s1:Ty_loop2) => {
              val s2 = s1.copy(oracle5=update_oracle(s1.oracle5)(bitm,citm_j))
              s2
            }
            s1 = map_complete_key.map_fold_cod(k)(f4)(s1.complete5)(s1)
            if (new_key) {
              ops5.sym_case(sym) match {
                case ops5.NT(nt) => {
                  val rs = sops.nt_items_for_nt(nt)(sops.string5,ops5.nt_dot_i9(nitm),ops5.nt_dot_j9(nitm))
                  val f1 = (s1:Ty_loop2, pnitm:ops5.nt_item) => {
                    val nitm = ops5.mk_item(ops5.NTITM(pnitm))
                    if (sets.set_todo_done.std_mem(nitm)(s1.todo_done5)) s1 else
                      add_todo(s1)(nitm)
                  }
                  val s03 = rs.foldLeft(s1)(f1)
                  s03
                }
                case ops5.TM(tm) => {
                  val titm = ops5.mk_item(ops5.TMITM(ops5.mk_tm_coord(tm,ops5.nt_dot_j9(nitm))))
                  if(sets.set_todo_done.std_mem(titm)(s1.todo_done5)) s1 else
                    add_todo(s1)(titm)
                }
              }
            } else {
              s1
            }
          }
        }
      }
      case ops5.TMITM(titm) => {
        val tm = ops5.tm5(titm)
        val p = sops.p_of_tm(tm)
        val i = ops5.tm_dot_i9(titm)
        val rs = p((sops.string5,i,sops.length5))
        val sym = ops5.sym_of_tm(tm)
        val k = (i,sym)
        val f5 = (s1:Ty_loop2,citm_j:Int) => {
          val s2 = s1.copy(complete5 = map_complete_key.map_add_cod(k)(citm_j)(s1.complete5))
          s2
        }
        var s1 = s00
        s1 = rs.foldLeft(s1)(f5)
        val f8 = (s1:Ty_loop2,citm_j:Int) => {
          val i = ops5.tm_dot_i9(titm)
          val f6 = (bitm:ops5.nt_item) => (s1:Ty_loop2) => cut(bitm)(citm_j)(s1)
          val s11 = map_blocked_key.map_fold_cod(k)(f6)(s1.blocked5)(s1)
          val f7 = (bitm:ops5.nt_item) => (s1:Ty_loop2) => {
            val s2 = s1.copy(oracle5 = update_oracle(s1.oracle5)(bitm,citm_j))
            s2
          }
          val s12 = map_blocked_key.map_fold_cod(k)(f7)(s11.blocked5)(s11)
          val s13 = s12.copy(tmoracle5 = update_tmoracle(s12.tmoracle5)(tm,i,citm_j))
          s13
        }
        s1 = rs.foldLeft(s1)(f8)
        s1
      }
    }
  } // loop2
  
  //@scala.annotation.tailrec // not respected?
  def earley_loop[S](sops:sops_class.Sops[S]): Ty_loop2 => Ty_loop2 = (s0:Ty_loop2) => {
    var s00 = s0
    val loop = loop2(sops)
    while(!todo_is_empty(s00)) {
      s00 = loop(s00)
    }
    s00
  }
  
  def mk_init_state[S](sops:sops_class.Sops[S]) = {
    import ctxt.ops5
    val init_items = (ops5.sym_case(sops.init_sym) match {
      case ops5.NT(nt) => (
        sops.nt_items_for_nt(nt)((sops.string5,0,0)).map( (x) => ctxt.ops5.mk_item(ctxt.ops5.NTITM(x))))
      case ops5.TM(tm) => (
          List(ops5.mk_item(ops5.TMITM(ops5.mk_tm_coord(tm,0)))))
    })
        
      val s0 = Ty_loop2(
        todo_done5 = ctxt.sets.set_todo_done.std_empty(),
        todo5 = init_items, // List(ctxt.ops5.NTITM((nt,List(),List(nt),0,0))),
        oracle5 = ctxt.maps.map_sym_sym_int_int.map_empty(),
        tmoracle5 = ctxt.maps.map_tm_int.map_empty(),
        blocked5 = ctxt.maps.map_blocked_key.map_empty(),
        complete5 = ctxt.maps.map_complete_key.map_empty()
      )
      s0
    }

  def earley[S](sops:sops_class.Sops[S]): Ty_loop2 = {
    val init_loop = mk_init_state(sops)
    earley_loop(sops)(init_loop)
  }
}
