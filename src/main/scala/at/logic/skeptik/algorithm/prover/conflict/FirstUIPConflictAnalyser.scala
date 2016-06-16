package at.logic.skeptik.algorithm.prover.conflict

import at.logic.skeptik.algorithm.prover._
import at.logic.skeptik.algorithm.prover.structure.mutable
import at.logic.skeptik.algorithm.prover.util.DecisionLevel

/**
  * @author Daniyar Itegulov
  */
object FirstUIPConflictAnalyser extends ConflictAnalyser {
  override def learnConflictClause(cnf: mutable.CNF, conflictLiterals: Seq[Literal], levels: Seq[DecisionLevel]): Clause = {
    val allLiterals = cnf.assignment.toSet
    def uip(current: Literal): Set[Literal] = {
      if (current == levels.last.literal) {
        Set(current)
      } else {
        val dominants = cnf.implicationGraph(current).map { case (lit, _) => uip(lit) }
        (allLiterals /: dominants )(_ intersect _)
      }
    }

    val conflictUip = conflictLiterals.map(uip(_)).reduce(_ intersect _)
    def firstUip(current: Literal, currentClause: Clause): Option[Clause] = {
      if (conflictUip contains current) {
        Some(currentClause)
      } else {
        cnf.implicationGraph(current).map {
          case (lit, clause) => firstUip(lit, currentClause.resolve(clause).get)
        }.find(_.isDefined).flatten
      }
    }
    firstUip(conflictLiterals.head, conflictLiterals.to[Clause])
  }
}
