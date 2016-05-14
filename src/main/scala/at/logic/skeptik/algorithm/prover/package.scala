package at.logic.skeptik.algorithm

import at.logic.skeptik.expression.E
import at.logic.skeptik.judgment.Sequent
import at.logic.skeptik.judgment.immutable.SetSequent

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
package object prover {
  def isUnit(clause: Sequent): Boolean = clause.width == 1

  // Assumes, that `isUnit` is true
  def getUnit(clause: Sequent): (Boolean, E) =
    if (clause.ant.size == 1) (true, clause.ant.head) else (false, clause.suc.head)

  // Eliminates one unit clause `negate:unit` in `exp`
  def eliminate(exp: Iterable[Sequent], negated: Boolean, unit: E): Set[Sequent] = {
    val newResult = mutable.Set.empty[Sequent]
    for (clause <- exp) {
      if (!(!negated && clause.sucContains(unit)) && !(negated && clause.antContains(unit))) {
        val newSuc = if (negated) {
          clause.suc.filter(_ != unit).toSet
        } else {
          clause.suc.toSet
        }
        val newAnt = if (negated) {
          clause.ant.toSet
        } else {
          clause.ant.filter(_ != unit).toSet
        }
        newResult += new SetSequent(newAnt, newSuc)
      }
    }
    newResult.toSet
  }
}
