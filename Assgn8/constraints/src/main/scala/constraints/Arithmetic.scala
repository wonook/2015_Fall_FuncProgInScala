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
  def binary2int(n: List[Boolean]): Int = ???

  /**
   * Encodes a positive integer number into base 2.
   * The `head` element of the resulting list contains the most significant
   * bit. This function should not return unnecessary leading zeros.
   */
  def int2binary(n: Int): List[Boolean] = ???


  /**
   * This function takes two arguments, both representing positive
   * integers encoded in binary as lists of propositional formulas
   * (true for 1, false for 0). It returns
   * a formula that represents a boolean circuit that constraints
   * `n1` to be less than or equal to `n2`
   */
  def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = ???

  /**
   * A full adder is a circuit that takes 3 one bit numbers, and returns the
   * result encoded over two bits: (cOut, s)
   */
  def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) = ???

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
  def adder(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = ???

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
