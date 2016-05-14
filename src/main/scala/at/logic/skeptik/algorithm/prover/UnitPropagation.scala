package at.logic.skeptik.algorithm.prover

import at.logic.skeptik.expression.E
import at.logic.skeptik.judgment.Sequent

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

/**
  * Implements Unit Propagation algorithm.
  *
  * @author Daniyar Itegulov
  */
object UnitPropagation {
  /**
    * Applies unit propagation technique to expression.
    *
    * @param exp expression where unit clauses will be resolved
    * @return (set of non-negated literals, which either present as unit clauses in
    *         exp or were derived from exp by unit propagation,
    *         set of negated literals, which either present as unit clauses in
    *         exp or were derived from exp by unit propagation,
    *         resulting expression after applying all possible unit propagation)
    */
  def apply(exp: Iterable[Sequent]): (Seq[E], Seq[E], Set[Sequent]) = {
    val unitClauses = ArrayBuffer.empty[(Boolean, E)]
    val eliminatedUnitClauses = ArrayBuffer.empty[E]
    val eliminatedNegatedUnitClauses = ArrayBuffer.empty[E]
    var result: Set[Sequent] = exp.toSet
    Breaks.breakable {
      while (true) {
        for (clause <- result) if (isUnit(clause))
          unitClauses += getUnit(clause)

        if (unitClauses.isEmpty) Breaks.break

        for (unitClause <- unitClauses) {
          val (negated, unit) = unitClause
          if (negated) {
            if (!eliminatedUnitClauses.contains(unit)) {
              eliminatedNegatedUnitClauses += unit
            }
          } else {
            if (!eliminatedNegatedUnitClauses.contains(unit)) {
              eliminatedUnitClauses += unit
            }
          }
          result = eliminate(result, negated, unit)
        }
        unitClauses.clear()
      }
    }
    (eliminatedUnitClauses, eliminatedNegatedUnitClauses, result)
  }
}
