package interpreter

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Lisp._

@RunWith(classOf[JUnitRunner])
class LispSuite extends FunSuite {
  val expr1 = "(+ 1 2)"
  test("addition") {
    assert(string2lisp(expr1).toString === "List('+, 1, 2)")
    assert(evaluate(expr1) === 3)
  }

  def testLisp(
      testName: String,
      expectedStructure: String,
      expectedResult: Data,
      expr: String) {
    test(testName + " / string2lisp") {
      assert(expectedStructure === string2lisp(expr).toString)
    }
    test(testName + " / evaluate") {
      assert(expectedResult === evaluate(expr))
    }
  }
  def testLisp(
      testName: String,
      expectedResult: Data,
      expr: String) {
    test(testName + " / evaluate") {
      assert(expectedResult === evaluate(expr))
    }
  }




  val expr2 = "((lambda(x) (+ x 1)) 41)"
  testLisp("expr1",
    "List(List('lambda, List('x), List('+, 'x, 1)), 41)",
    42, expr2)

  val expr3 =
    "(val g (lambda (x) (x 2))"+
    " (val f (lambda (x) (g (lambda(y) (+ x y))))"+
    "  (f 1)))"
  testLisp("expr2",
    "List('val, 'g, List('lambda, List('x), List('x, 2)), List('val, 'f, List('lambda, List('x), List('g, List('lambda, List('y), List('+, 'x, 'y)))), List('f, 1)))",
    3, expr3)

  val expr4 =
    "(val g (lambda (z) (z 2))"+
    " (val f (lambda (x) (g (lambda(y) (+ x y))))"+
    "  (f 1)))"
  testLisp("expr3",
    "List('val, 'g, List('lambda, List('z), List('z, 2)), List('val, 'f, List('lambda, List('x), List('g, List('lambda, List('y), List('+, 'x, 'y)))), List('f, 1)))",
    3, expr4)

  val factDef =
    "def factorial (lambda (n)" +
    "  (if (= n 0)" +
    "      1" +
    "      (* n (factorial (- n 1)))))"

  def expr5 = "("+factDef+"(factorial 4))"

  testLisp("expr4",
    "List('def, 'factorial, List('lambda, List('n), List('if, List('=, 'n, 0), 1, List('*, 'n, List('factorial, List('-, 'n, 1))))), List('factorial, 4))",
    24, expr5)

  val factDefSugar =
    "def (factorial n)" +
    "  (if (= n 0)" +
    "      1" +
    "      (* n (factorial (- n 1))))"

  def expr6 = "("+factDefSugar+"(factorial 4))"

  testLisp("defSugar1", 24, expr6)
  testLisp("defSugar2", 3, "(def (add a b) (+ a b) (add 1 2))")
  testLisp("defSugar3", 1, "(def (succ x) (+ x 1) (succ 0))")
  testLisp("defSugar4", 0, "(def (foo) 0 (foo))")

  testLisp("caseSugar1", 2, "(case 1 (else 2))")
  testLisp("caseSugar2", 3, "(case 1 (1 3) (else 4))")
  testLisp("caseSugar3", 4, "(case 2 (1 3) (else 4))")
  testLisp("caseSugar4", 12, "(case 2 (1 11) (2 12) (3 13) (else 14))")
  testLisp("caseSugar5", 100, "(case (case 1 (2 50) (3 4) (else 5)) (3 51) (5 100) (else 0))")

  testLisp("sugar1", 1, "(def (yo a) (case a (1 a) (else 1)) (yo 2))")

  testLisp("differencesLisp1", List(), LispCode.withDifferences("(differences nil)"))
  testLisp("differencesLisp2", List(7), LispCode.withDifferences("(differences (cons 7 nil))"))
  testLisp("differencesLisp3", List(0, 0, 0), LispCode.withDifferences("(differences (cons 0 (cons 0 (cons 0 nil))))"))
  testLisp("differencesLisp4", List(1, 1, 1), LispCode.withDifferences("(differences (cons 1 (cons 2 (cons 3 nil))))"))
  testLisp("differencesLisp5", List(4, -3, 5), LispCode.withDifferences("(differences (cons 4 (cons 1 (cons 6 nil))))"))

  testLisp("rebuildListLisp1", List(), LispCode.withDifferences("(rebuildList nil)"))
  testLisp("rebuildListLisp2", List(7), LispCode.withDifferences("(rebuildList (cons 7 nil))"))
  testLisp("rebuildListLisp3", List(0, 0, 0), LispCode.withDifferences("(rebuildList (cons 0 (cons 0 (cons 0 nil))))"))
  testLisp("rebuildListLisp4", List(1, 2, 3), LispCode.withDifferences("(rebuildList (cons 1 (cons 1 (cons 1 nil))))"))
  testLisp("rebuildListLisp5", List(4, 1, 6), LispCode.withDifferences("(rebuildList (cons 4 (cons -3 (cons 5 nil))))"))

}
