package e4.core

object Core_log {
  
  object P {
    val ab = 1
    val ac = 13
    val ae = 11
    val am = 12
    val bc = 2
    val cd = 3
    val de = 4
    val ef = 5
    val fg = 6
    val gh = 7
    val hi = 8
    val ij = 9
    val jk = 10

    def p_to_string(i:Int) = {
      i match {
        case _ if i == ab => "ab"
        case _ if i == ac => "ac"
        case _ if i == ae => "ae"
        case _ if i == am => "am"
        case _ if i == bc => "bc"
        case _ if i == cd => "cd"
        case _ if i == de => "de"
        case _ if i == ef => "ef"
        case _ if i == fg => "fg"
        case _ if i == gh => "gh"
        case _ if i == hi => "hi"
        case _ if i == ij => "ij"
        case _ if i == jk => "jk"
        case _ => (throw new Exception("p_to_string"))
      }
    }
  }
  
  def now() = {
  	System.nanoTime()
  }
  
  type loc_t = Int
  
  type ts_t = (Int,Long)
	
  var ts : List[ts_t] = List()
  
  def log(p:loc_t) = {
  	ts=(p,now())::ts
  	true
  }
  
  def print_logs() {
  	def f(last:ts_t,prev:ts_t) = {
  		val (p2,t2) = last
  		val (p1,t1) = prev
  		val d = t2 - t1
  		val s = s"""(${P.p_to_string(p1)},${P.p_to_string(p2)}) $d"""
  		println(s)
  		prev
  	}
  	ts.foldLeft(ts.head)(f)
  }
  
}