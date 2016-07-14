package p6

import Span._

object Raw_parser {
  //type raw_parser[S,A] 
  type raw_parser[S, A] = Span[S] => List[(A, Int)]

  // consume an 'x' and return a 1
  val x: raw_parser[String, Int] = (s: Span[String]) => {
    if (s.i < s.j &&
      s.i < s.s.length &&
      s.s.charAt(s.i) == 'x')
      List((1, s.i + 1))
    else
      List()
  }
  
  // consume the empty string and return a 0
  val eps: raw_parser[String, Int] = (s: Span[String]) => {
    if (s.i <= s.j)
      List((0, s.i ))
    else
      List()
  }
}