package p5

import e3._

object P3_core_aux {
  
  def println(x:Any) = {} // turn off debugging
  
  trait Gensym {
    type t
    def gen_int() : t
    def gen_even() : t
    def gen_odd() : t
    def t_to_int(x:t) : Int
  }
  
  class Default_gensym(val id:String) extends Gensym {
    type t = Int
    var counter = 0
    def gen_int() = {
      val r = counter
      counter = counter+1
      println(s"gen_int $id: $r")
      r
    }
    def gen_even() = {
      val x = if (counter%2==0) gen_int()
        else {gen_int(); gen_int()}
      println(s"gen_even $id: $x")
      x
    }
    def gen_odd() = {
      val x = if (counter%2==1) gen_int()
        else {gen_int(); gen_int()}
      println(s"gen_odd $id: $x")
      x
    }
    def t_to_int(x:t) = {
      x
    }
  }
  
  val default_gensym = new Default_gensym("default_gensym")

  trait Box {
    type key
    type t[A]
    def box[A](x:A) : t[A]
    def box_even[A](x:A) : t[A]
    def box_odd[A](x:A) : t[A]
    def unbox[A](x:t[A]):  A
    def box_get_key[A](x:t[A]) : key
  }

  /*
   * <console>:19: warning: higher-kinded type should be enabled
by making the implicit value scala.language.higherKinds visible.
This can be achieved by adding the import clause 'import scala.language.higherKinds'
or by setting the compiler option -language:higherKinds.
See the Scala docs for value scala.language.higherKinds for a discussion
why the feature should be explicitly enabled.
           type t[A]
   * 
   * 
   */
  
  
  class Default_box extends Box {
    val gensym = new Default_gensym("box")
    type key = gensym.t
    case class W(k:key,x:Any)
    type t[A] = W
    def box[A](x:A) = {
      W(gensym.gen_int(),x)
    }
    def box_even[A](x:A) = {
      W(gensym.gen_even(),x)
    }
    def box_odd[A](x:A) = {
      W(gensym.gen_odd(),x)
    }
    def unbox[A](x:t[A]) = {
      x match {
        case W(k,x) => x.asInstanceOf[A] // safe?
      }
    }
    def box_get_key[A](x:t[A]) = {
      x match {
        case W(k,x) => k
      }
    }
  }
  
  trait Span {
    type t[A]
    def dest_SS[A](x:t[A]): (A,Int,Int)
    def mk_SS[A](x:A,i:Int,j:Int): t[A]
    def content(x:t[String]) : String
    def concatenate_two[A](x:t[A],y:t[A]) : Option[t[A]]
    def concatenate_list[A](xs:List[t[A]]) : Option[t[A]]
  }

  /*
  class Default_span extends Span {
    case class SS[A](x:A,i:Int,j:Int)
    type t[A] = SS[A]
    def dest_SS[A](x:t[A]): (A,Int,Int) = {
      x match {
        case SS(x:A,i,j) => (x,i,j) // warning can be ignored - parameter must be of type t[A]
      }
    }
    def mk_SS[A](s:A,i:Int,j:Int) = {
      SS(s,i,j)
    }
    def content(x:t[String]) = {
      x match {
        case SS(x:String,i,j) => x.substring(i, j) 
      }
    }
    def concatenate_two[A](x:t[A],y:t[A]) = {
      // assume the underlying strings are the same
      (dest_SS(x),dest_SS(y)) match {
        case ((x,i,k),(y,k2,j)) if (k==k2) => Some(SS[A](x,i,j))
        case _ => None
      }
    }
    def concatenate_list[A](xs:List[t[A]]) = {
      xs match {
        case Nil => None
        case x::xs => {
          val f = (acc:Option[t[A]],x:t[A]) => acc match {
            case None => None
            case Some(acc) => concatenate_two(acc,x)
          }
          val z:Option[t[A]] = Some(x)
          xs.foldLeft(z)(f)
        }
      }
    }
  }
  * 
  */
  

  class Default_span extends Span {
    type t[A] = E3_substring.Substring[A]
    def dest_SS[A](x:t[A]): (A,Int,Int) = {
      x match {
        case (x,i,j) => (x,i,j)
      }
    }
    def mk_SS[A](s:A,i:Int,j:Int) = (s,i,j)
    def content(x:t[String]) = {
      x match {
        case (x:String,i,j) => x.substring(i, j) 
      }
    }
    def concatenate_two[A](x:t[A],y:t[A]) = {
      // assume the underlying strings are the same
      (x,y) match {
        case ((x,i,k),(y,k2,j)) if (k==k2) => Some(x,i,j)
        case _ => None
      }
    }
    def concatenate_list[A](xs:List[t[A]]) = {
      xs match {
        case Nil => None
        case x::xs => {
          val f = (acc:Option[t[A]],x:t[A]) => acc match {
            case None => None
            case Some(acc) => concatenate_two(acc,x)
          }
          val z:Option[t[A]] = Some(x)
          xs.foldLeft(z)(f)
        }
      }
    }
  }

  
  
}
