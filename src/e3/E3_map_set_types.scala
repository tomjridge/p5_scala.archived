

package e3

object E3_map_set_types {
  
  // set type
    trait std  {
      type elt
      type t
      val std_empty: Unit => t
      val std_add: elt => t => t
      val std_mem: elt => t => Boolean
    }
    
  /* example instantiation
  val x = new ctxt_set_component {
    val sets = new ctxt_set {
      val set_todo_done = new std {
        type elt = Int
        type t = Int
        val std_empty = (x:Unit) => 1
        val std_add = (x:elt) => (t:t) => t
        val std_mem = (x:elt) => (t:t) => true
      }
    }
  } 
  */
  
  // maps to *sets* of values
  trait my_map {
    type t
    type key
    type value
    val map_empty: Unit => t
    val map_add_cod: key => value => t => t
  }

  trait mfc extends my_map {
    //val map_fold_cod: key => (value => Any => Any) => t => Any => Any // really Any is forall quantified; how to express this?
    def map_fold_cod[B]: key => (value => B => B) => t => B => B // really Any is forall quantified; how to express this?def f[T]: T => Boolean
  }

  trait mce extends my_map {
    val map_cod_empty: key => t => Boolean
  }

  trait mbk extends mfc with mce

  trait mck extends mfc

  trait mti extends my_map {
    val map_find_cod: key => value => t => Boolean
  }

  trait mssii extends my_map {
    val mssii_elts_cod: key => t => List[value]
  }

  class Default_map_impl extends mbk with mti with mssii {
        type t = scala.collection.immutable.Map[key,Set[value]]
        val map_apply: t => key => Set[value] = (t:t) => (k:key) => t.applyOrElse(k, default = (_:key) => Set())
        val map_empty: Unit=>t = (_:Unit) => Map()
        val map_add_cod: key => value => t => t = (k:key) => (v:value) => (t:t) => t + (k -> (map_apply(t)(k) + v))
        def map_fold_cod[B]: key => (value => B => B) => t => B => B = (k:key) => (f:value => B => B) => (t:t)
        => (b0:B) => {
          val s:Set[value] = map_apply(t)(k)
          val f1 = (b:B,v:value) => {
            f(v)(b)
          }
          s.foldLeft(b0)(f1)
        }
        val map_cod_empty: key => t => Boolean = (k:key) => (t:t) => map_apply(t)(k).isEmpty
        val map_find_cod: key => value => t => Boolean = (k:key) => (v:value) => (t:t) => map_apply(t)(k).contains(v)
        val mssii_elts_cod: key => t => List[value] = (k:key) => (t:t) => map_apply(t)(k).toList
      }
}