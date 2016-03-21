

package e3

object E3_test {
  
  def main(args: Array[String]) {
    val E = 2
    val eps = 1
    val _1 = 3
    
    val s = "1"*100
    val num_loops = 5
    
    import E3_simple_impl.e3_core_impl
      
    val sops = {
      import e3_core_impl.ctxt.ops5._
      val nt_items_for_nt = (x:nt) => (ss:E3_substring.Substring[String]) => {
        val (s,i,j) = ss
        // E -> EEE | "1" | eps
        val eee = (E,Nil,List(E,E,E),j,j)
        val one = (E,Nil,List(_1),j,j)
        val eps0 = (E,Nil,List(eps),j,j)
          (List(eee,one,eps0)):List[nt_item]
      }
      val p_of_tm = (x:tm) => (ss:E3_substring.Substring[String]) => {
        val (s,i,j) = ss
        x match {
          case _ if x==_1 => {
            //print("s is: "+s)
            if (i < s.length() && s.charAt(i)=='1') List(i+1) else List()
          }
          case _ if x==eps => {
            List(i)
          }
          case _ => throw new Exception("p_of_tm")
        }
      }
      val init_items = (nt_items_for_nt(E)((s,0,0))).map( (x) => NTITM(x))
      e3_core_impl.sops_class.Sops(
        s,
        s.length,
        E,
        nt_items_for_nt,
        p_of_tm
      )
    }
    
    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      val diff = t1 - t0
      val in_secs = diff / 1000000000.0
      val avg = in_secs / num_loops
      println(s"Elapsed time: $diff ns ($in_secs secs) (avg: $avg)")
      result
    }
    time {
      for(i<-1 to num_loops) {
        e3_core_impl.earley(sops)
      }
    }
    //println(e3_core_impl.earley(sops))
  }

}