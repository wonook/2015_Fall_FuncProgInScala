package constraints

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._

/**
 * This component implements a constraint solver for assigning time slots to volunteers
 * for various tasks at a festival. A task may require more than one volunteer,
 * and a volunteer can take a limited number of tasks
 */
object Balelec {

  import Arithmetic._

  case class Volunteer(name: String) {
    override def toString = name
  }

  /**
   * A task is represented by its name and
   * its capacity, i.e. the exact number of people
   * required to complete it.
   */
  case class Task(name: String, capacity: Int) {
    override def toString = name
  }

  /**
   * This function schedules volunteers to tasks.
   * It takes as input a list of volunteers and a list of tasks.
   * The `availability` map contains mappings from volunteers to the
   * tasks they are available for.
   * A volunteer can be assigned to several tasks, but only
   * up to a maximum number of task specified by the `maxWorkload` parameter.
   * It is ok to not assign a volunteer to any task.
   *
   * The return value is a list of volunteers assigned to each task. The function only
   * returns a complete valid assignment, if no such assignment exists then the
   * function returns None.
   */
  def schedule(
    volunteers: List[Volunteer],
    tasks: List[Task],
    availability: Map[Volunteer, List[Task]],
    maxWorkload: Int
  ): Option[Map[Task, List[Volunteer]]] = {
    val varsMatrix: Map[(Volunteer, Task), PropVar] =
      volunteers.flatMap({case v@Volunteer(name) =>
        tasks.map(t => (v, t) -> propVar(name))
      }).toMap

    val desirableTasks: Seq[Formula] = {
      val groupByVolunteers = varsMatrix.groupBy(_._1._1).map(_._2)
      val volunteersWithAvailableTasks = groupByVolunteers.map((bm) => bm.filter((s) => availability.apply(s._1._1).contains(s._1._2)))
      val propVarsOfEachVolunteers = volunteersWithAvailableTasks.map(_.map(_._2))
      propVarsOfEachVolunteers.map((e) => e.foldLeft[Formula](false) (_ || _)).toSeq
    }

    val eachVolunteerHasLessThanMaxWorkload: Seq[Formula] = {
      val groupByVolunteers = varsMatrix.groupBy(_._1._1).map(_._2)
      val volunteersWithAvailableTasks = groupByVolunteers.map((bm) => bm.filter((s) => availability.apply(s._1._1).contains(s._1._2)))
      val propVarsOfEachVolunteers = volunteersWithAvailableTasks.map(_.map(_._2))
      val combinationOfTwo = propVarsOfEachVolunteers.map(_.toSet.subsets.toList.filter(_.toList.length == maxWorkload+1))
      combinationOfTwo.map(_.foldLeft[Formula](true)(_ && _.foldLeft[Formula](false)(_ || !_))).toSeq
    }

    val eachTaskDoneOnce: Seq[Formula] = {
      val groupByTasks = varsMatrix.groupBy(_._1._2).map(_._2.map(_._2))
      val combination = groupByTasks.flatMap(_.toSet.subsets.toList.filter(_.toList.length == 2))
      combination.map(_.foldLeft[Formula](false)(_ || !_)).toSeq
    }

    val eachTaskDone: Seq[Formula] = {
      val groupByTasks = varsMatrix.groupBy(_._1._2).map(_._2.map(_._2))
      groupByTasks.map(_.foldLeft[Formula](false)(_ || _)).toSeq
    }

    val allConstraints: Seq[Formula] = desirableTasks ++ eachVolunteerHasLessThanMaxWorkload ++ eachTaskDoneOnce ++ eachTaskDone

    val res = solveForSatisfiability(and(allConstraints:_*))

    res.map(model => {
      tasks.map(task => {
        val assignedVolunteers = volunteers.filter((v) => model(varsMatrix((v, task))))
        (task, assignedVolunteers)
      }).toMap
    })
  }

  /**
   * This function takes a list of constraint, and returns a
   * constraint that is true if and only if at most max
   * of them are true.
   */
  def positivesLessEquals(ns: List[Formula], max: Int): Formula = {
    val (r, c) = countPositives(ns)
    lessEquals(r, int2binary(max)) && and(c.toSeq:_*)
  }

  /**
   * This function takes a list of constraints, and returns a pair.
   * The first element of the pair is a list of constraints representing the bitwise
   * sum of the constraints of `ns`.
   * The second element is a set of additional constraints that have been gathered along
   * the way. Hint: see `adder` for understanding how to use additional constraints
   */
  def countPositives(ns: List[Formula]): (List[Formula], Set[Formula]) = {
    ns.foldLeft((List[Formula](false), Set[Formula]())) { case ((tmpSum, tmpAcc), n) =>
      val (r, c) = adder(tmpSum, List(n))
      (r, tmpAcc ++ c)
    }
  }

}
