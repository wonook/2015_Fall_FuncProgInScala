package interpreter

import Lisp._

object Main extends App {
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))
  // TODO: Insert code for the REPL
  while(true) {
    print("lisp> ")
    val str = in.readLine()
    try {
      println(lisp2string(evaluate(str)))
    } catch {
      case e: Error => println("Error: " + e)
      case e: Exception => println("Exception: " + e)
    }
  }
}

object LispCode {
  // TODO: implement the function `reverse` in Lisp.
  // From a list (a, b, c, d) it should compute (d, c, b, a)
  // Write it as a String, and test it in your REPL
  val reverse = """
  def (reverse L acc) (if (null? L) acc (reverse (cdr L) (cons (car L) acc)) )
  """

  // TODO: implement the function `differences` in Lisp.
  // From a list (a, b, c, d ...) it should compute (a, b-a, c-b, d-c ...)
  // You might find useful to define an inner loop def
  val differences = """
  def (differences L) (def (innerloop lst a acc) (if (null? lst) acc (innerloop (cdr lst) (car lst) (cons (- (car lst) a) acc)) ) (reverse (innerloop L 0 nil) nil))
  """
  val rebuildList = """
  def (rebuildList L) (def (innerloop lst a acc) (if (null? lst) acc (innerloop (cdr lst) (+ a (car lst)) (cons (+ (car lst) a) acc)) ) (reverse (innerloop L 0 nil) nil))
  """

  val withDifferences: String => String =
    (code: String) => "(" + reverse + " (" + differences + " (" + rebuildList + " " + code + ")))"
}
