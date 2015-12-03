package constraints

import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._

import scala.annotation.tailrec

/**
  * This object contains utility functions for encoding
  * some arithmetic constraints as boolean ones
  */
object Arithmetic {

  /**
   * Transforms a positive integer in binary form into its integer representation.
   * The `head` element of the input list contains the most
   * significant bit (the list is in big-endian form).
   */
  def binary2int(n: List[Boolean]): Int = { //tailrec?
    @tailrec def binary2intAcc(acc: Int, n: List[Boolean]): Int = {
      if(n.isEmpty) acc
      else binary2intAcc(2*acc + {if(n.head) 1 else 0}, n.tail)
    }
    binary2intAcc(0, n)
  }

  /**
   * Encodes a positive integer number into base 2.
   * The `head` element of the resulting list contains the most significant
   * bit. This function should not return unnecessary leading zeros.
   */
  def int2binary(n: Int): List[Boolean] = { //tailrec?
    @tailrec def int2binaryAcc(acc: List[Boolean], n: Int): List[Boolean] = {
      if(n == 0) acc
      else if(n % 2 == 0) int2binaryAcc(false::acc, n/2)
      else int2binaryAcc(true::acc, n/2)
    }
    if(n == 0) List(false)
    else int2binaryAcc(List(), n)
  }


  /**
   * This function takes two arguments, both representing positive
   * integers encoded in binary as lists of propositional formulas
   * (true for 1, false for 0). It returns
   * a formula that represents a boolean circuit that constraints
   * `n1` to be less than or equal to `n2`
   */
  def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
    if(n1.length < n2.length) true
    else if(n1.length > n2.length) false
    else if(n1.length == 1) !(n1.head && !n2.head)
    else !(n1.head && !n2.head) && ((!n1.head && n2.head) || lessEquals(n1.tail, n2.tail))
  }

  /**
   * A full adder is a circuit that takes 3 one bit numbers, and returns the
   * result encoded over two bits: (cOut, s)
   */
  def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) = {
    ((a && b) || (b && cIn) || (a && cIn), (a xor b xor cIn))
  }

  /**
   * This function takes two arguments, both representing positive integers
   * encoded as lists of propositional variables. It returns a pair.
   *
   * The first element of the pair is a `List[Formula]`, and it represents
   * the resulting binary number.
   * The second element is a set of intermediate constraints that are created
   * along the way.
   *
   */
  def adder(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
    if(n1.length > n2.length) {
      adder(n1, false::n2)
//      val (res, summary) = adder(n1.tail, n2)
//      val (cOut, s) = fullAdder(n1.head, false, res.head)
//
//      val co2: PropVar = propVar()
//      val s2: PropVar = propVar()
//      val cs = (co2 iff cOut)
//      val ss = (s2 iff s)
//      val newSummary = summary + cs + ss
//      (cOut::s::res.tail, newSummary)
    } else if(n1.length < n2.length) {
      adder(false::n1, n2)
//      val (res, summary) = adder(n1, n2.tail)
//      val (cOut, s) = fullAdder(n2.head, false, res.head)
//
//      val co2: PropVar = propVar()
//      val s2: PropVar = propVar()
//      val cs = (co2 iff cOut)
//      val ss = (s2 iff s)
//      val newSummary = summary + cs + ss
//      (cOut::s::res.tail, newSummary)
    } else {
      (n1, n2) match {
        case(h1::Nil, h2::Nil) =>
          val (cOut, s) = fullAdder(h1, h2, false)
          val co2: PropVar = propVar("co2")
          val s2: PropVar = propVar("s2")
          val sum = (co2 iff cOut) && (s2 iff s)
          (cOut::s::Nil, Set[Formula](sum))
        case(h1::h1t, h2::h2t) =>
          val (res, summary) = adder(h1t, h2t)
          val (cOut, s) = fullAdder(h1, h2, res.head)
          val co2: PropVar = propVar("co2")
          val s2: PropVar = propVar("s2")
          val sum = (co2 iff cOut) && (s2 iff s)
          (cOut::s::res.tail, summary + sum)
        case _ => sys.error("Unexpected case")
      }
    }
  }

  /**
   * A helper function that creates a less-equals formula
   * taking an integer and a formula as parameters
   */
  def lessEqualsConst(cst: Int, n: List[Formula]): Formula = {
    lessEquals(int2binary(cst), n)
  }

  /**
   * A helper function that creates a less-equals formula
   * taking a formula and an integer as parameters
   */
  def lessEqualsConst(n: List[Formula], cst: Int): Formula = {
    lessEquals(n, int2binary(cst))
  }


}
