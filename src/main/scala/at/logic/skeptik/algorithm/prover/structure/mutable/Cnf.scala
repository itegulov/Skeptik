package at.logic.skeptik.algorithm.prover.structure.mutable

import at.logic.skeptik.algorithm.prover._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Represents propositional CNF.
  *
  * @author Daniyar Itegulov
  */
class CNF(val clauses: ArrayBuffer[Clause]) {
  /**
    * Shows which literals are set to be true.
    */
  val assignment = mutable.Set.empty[Literal]

  /**
    * Just all variables, contained in CNF.
    */
  val variables = clauses.flatMap(_.literals.map(_.unit))

  /**
    * Shows what literals were used to achieve this
    */
  val implicationGraph = mutable.Map.empty[Literal, Seq[(Literal, Clause)]]

  /**
    * Represents two-watched literal scheme:
    * for each literal we know what clauses have watchers set
    * to this literal.
    */
  val sentinels: Map[Literal, mutable.Set[Clause]] = {
    val sentinels = variables.flatMap(variable =>
      Seq(
        varToLit(variable) -> mutable.Set.empty[Clause],
        !varToLit(variable) -> mutable.Set.empty[Clause]
      )
    ).toMap
    for (clause <- clauses) if (clause.width >= 2) {
      sentinels(clause.first) += clause
      sentinels(clause.last) += clause
    }
    sentinels
  }

  def +=(that: Clause): CNF = {
    if (that.width >= 1) {
      sentinels(that.first) += that
      sentinels(that.last) += that
    }
    clauses += that
    this
  }

  def -=(that: Clause): CNF = {
    if (clauses.contains(that) && that.width >= 1) {
      sentinels(that.first) -= that
      sentinels(that.last) -= that
    }
    clauses -= that
    this
  }

  private def clauseIsSatisfied(clause: Clause): Boolean = clause.literals.exists(assignment.contains)

  /**
    * Ensures that provided literal is true and returns sequence
    * of literals that should also be true.
    *
    * @param literal which should be true
    * @return sequence of literals that also should be true
    */
  def assignLiteral(literal: Literal): Seq[Literal] = {
    assignment += literal
    val result = ArrayBuffer.empty[Literal]
    for (clause <- sentinels(!literal)) if (!clauseIsSatisfied(clause)) {
      val otherLiterals = clause.literals.filter(_ != !literal)
      otherLiterals.find(!sentinels(_).contains(clause)) match {
        case Some(nonSentinelLiteral) =>
          sentinels(!literal) -= clause
          sentinels(nonSentinelLiteral) += clause
        case None =>
          otherLiterals.find(sentinels(_) contains clause) match {
            case Some(sentinel) =>
              result += sentinel
              implicationGraph(sentinel) = otherLiterals.zip(Stream.continually(clause))
            case None =>
          }
      }
    }
    result
  }
}
