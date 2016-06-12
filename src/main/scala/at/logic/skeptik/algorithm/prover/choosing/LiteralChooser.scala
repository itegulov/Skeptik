package at.logic.skeptik.algorithm.prover.choosing

import at.logic.skeptik.algorithm.prover._
import at.logic.skeptik.algorithm.prover.structure.mutable.CNF

/**
  * Represents general way to choose next literal.
  *
  * @author Daniyar Itegulov
  */
trait LiteralChooser {
  /**
    * Tries to choose best literal to make decision on.
    *
    * @param cnf which has some undecided literals
    * @return None, if all literals have decisions
    *         Some(literal), if literal should be decided
    */
  def chooseLiteral(cnf: CNF): Option[Literal]

  /**
    * Handle the addition of a new clause
    *
    * @param clause to be added
    */
  def clauseAdded(clause: Clause): Unit

  /**
    * Handle the unsetting of a literal
    *
    * @param literal to be unsetted
    */
  def unsetLiteral(literal: Literal): Unit
}
