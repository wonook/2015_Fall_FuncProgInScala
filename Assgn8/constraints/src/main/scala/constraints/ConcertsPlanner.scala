package constraints

import cafesat.api.Formulas._
import cafesat.api.FormulaBuilder._
import cafesat.api.Solver._

/**
 * This component implements a constraint solver
 * for assigning time slots to bands at a festival
 */
object ConcertsPlanner {

  case class Band(name: String) {
    override def toString = name
  }

  case class Stage(name: String) {
    override def toString = name
  }

  case class Time(time: String) {
    override def toString = time
  }

  type Slot = (Stage, Time)

  /*
   * This function schedules bands to slots. It takes as input
   * a list of preferences, as a map from bands to the list of 
   * slots the band wishes to play in.
   *
   * The result is an `Option[Map[Band, Slot]]`. The function attemps to
   * assign a unique and different slot to each band. It returns None if
   * no complete valid scheduling exists.
   * If the problem has a complete valid assignment, a map from every
   * band to a slot is returned.
   * No partial solution is returned, if only some of the band can be assigned
   * a slot, then None is returned.
   */
  def plan(preferences: Map[Band, List[Slot]]): Option[Map[Band, Slot]] = {
    val bands: Seq[Band] = preferences.keys.toSeq
    val slots: Seq[Slot] = getUniqueSlots(preferences).toSeq
  
    //generates one propositional variable per band/slot combination
    val varsMatrix: Map[(Band, Slot), PropVar] =
      bands.flatMap({ case b@Band(name) =>
        slots.map(s => (b, s) -> propVar(name))
      }).toMap


    //Set of constraints ensuring each band gets a desired slot
    val desirableSlots: Seq[Formula] = {
      val groupByBands = varsMatrix.groupBy(_._1._1).map(_._2)
      val bandsWithDesiredSlots = groupByBands.map((bm) => bm.filter((s) => preferences.apply(s._1._1).contains(s._1._2)))
      val propVarsOfEachBands = bandsWithDesiredSlots.map(_.map(_._2))
      propVarsOfEachBands.map((e) => e.foldLeft[Formula](false) (_ || _)).toSeq
    }

    //A set of constraints ensuring that a band gets at most one slot
    val eachBandPlaysOnce: Seq[Formula] = {
      val groupByBands = varsMatrix.groupBy(_._1._1).map(_._2)
      val bandsWithDesiredSlots = groupByBands.map((bm) => bm.filter((s) => preferences.apply(s._1._1).contains(s._1._2)))
      val propVarsOfEachBands = bandsWithDesiredSlots.map(_.map(_._2))
      val combinationOfTwo = propVarsOfEachBands.map(_.toSet.subsets.toList.filter(_.toList.length == 2))
      combinationOfTwo.map(_.foldLeft[Formula](true)(_ && _.foldLeft[Formula](false)(_ || !_))).toSeq
    }
  
    //A set of constraints ensuring that each slot is used at most once
    val eachSlotUsedOnce: Seq[Formula] = {
      val groupBySlots = varsMatrix.groupBy(_._1._2).map(_._2.map(_._2))
      val combination = groupBySlots.flatMap(_.toSet.subsets.toList.filter(_.toList.length == 2))
      combination.map(_.foldLeft[Formula](false)(_ || !_)).toSeq
    }


    //combining all the constraints together
    val allConstraints: Seq[Formula] = 
      desirableSlots ++ eachBandPlaysOnce ++ eachSlotUsedOnce
  
    //finding a satisfying assignment to the constraints
    val res = solveForSatisfiability(and(allConstraints:_*))
  
    res.map(model => {
      bands.map(band => {
        val assignedSlot = slots.find(slot => model(varsMatrix((band, slot))))
        (band, assignedSlot.get)
      }).toMap
    })
  }

  /**
   * This function takes a preference map, and returns all unique slots that are
   * part of the preferences.
   */
  def getUniqueSlots(preferences: Map[Band, List[Slot]]): Set[Slot] = {
    preferences.toSet.flatMap((e: (Band, List[Slot])) => e._2)
  }

}
