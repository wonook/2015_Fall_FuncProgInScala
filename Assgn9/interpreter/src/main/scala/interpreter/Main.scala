package interpreter

import Lisp._

object Main extends App {
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))
  // TODO: Insert code for the REPL
  print("lisp> ")
  while(true) {
    val str = in.readLine()
    println(lisp2string(evaluate(str)))
    print("lisp> ")
  }
}

object LispCode {
  // TODO: implement the function `reverse` in Lisp.
  // From a list (a, b, c, d) it should compute (d, c, b, a)
  // Write it as a String, and test it in your REPL
  val reverse = """
  (def (reverse L acc) (if (null? L) nil (cons (reverse (cdr L) acc) (car L)) ) )
  """

  // TODO: implement the function `differences` in Lisp.
  // From a list (a, b, c, d ...) it should compute (a, b-a, c-b, d-c ...)
  // You might find useful to define an inner loop def
  val differences = """
  (def (differences L) (def (innerloop lst a) (if (null? lst) nil (cons (- a (car lst)) (innerloop (cdr lst) (car lst))))) (cons (car L) (innerloop (cdr L) (car L))))
  """
  val rebuildList = """
  (def (rebuildList L) (def (innerloop lst a) (if (null? lst) nil (cons (+ a (car lst)) (innerloop (cdr lst) (car lst))))) (cons (car L) (innerloop (cdr L) (car L))))
  """

  val withDifferences: String => String =
    (code: String) => "(" + reverse + " (" + differences + " (" + rebuildList + " " + code + ")))"
}
