package at.logic.skeptik.algorithm.prover.choosing

import at.logic.skeptik.algorithm.prover._
import at.logic.skeptik.algorithm.prover.structure.mutable.CNF
import at.logic.skeptik.expression.E

/**
  * Very dumb algorithm, which just pick some random undecided variable.
  *
  * @author Daniyar Itegulov
  */
object SimpleLiteralChooser extends LiteralChooser {
  override def chooseLiteral(cnf: CNF): Option[Literal] =
    cnf.variables.find(variable =>
      !cnf.assignment.contains(variable) && !cnf.assignment.contains(!varToLit(variable))
    ).map(varToLit(_))

  override def clauseAdded(clause: Clause): Unit = {}

  override def unsetLiteral(literal: (E, Boolean)): Unit = {}
}
