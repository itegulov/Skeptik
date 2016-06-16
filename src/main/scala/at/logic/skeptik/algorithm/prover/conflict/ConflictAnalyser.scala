package at.logic.skeptik.algorithm.prover.conflict

import at.logic.skeptik.algorithm.prover._
import at.logic.skeptik.algorithm.prover.structure.mutable
import at.logic.skeptik.algorithm.prover.util.DecisionLevel

/**
  * Represents general way to learn a conflict clause.
  *
  * @author Daniyar Itegulov
  */
trait ConflictAnalyser {
  /**
    * Predicts the best conflict clause.
    *
    * @param levels decision levels
    * @return learnt clause
    */
  def learnConflictClause(cnf: mutable.CNF, conflictLiterals: Seq[Literal], levels: Seq[DecisionLevel]): Clause
}
