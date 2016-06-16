package at.logic.skeptik.algorithm.prover

import at.logic.skeptik.algorithm.prover.choosing.{LiteralChooser, SimpleLiteralChooser}
import at.logic.skeptik.algorithm.prover.conflict.{ConflictAnalyser, SimpleConflictAnalyser}
import at.logic.skeptik.algorithm.prover.util.DecisionLevel

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @author Daniyar Itegulov
  */
object CDCL {
  def isSatisfiable(immutableCnf: CNF,
                    literalChooser: LiteralChooser = SimpleLiteralChooser,
                    conflictAnalyser: ConflictAnalyser = SimpleConflictAnalyser): Boolean = {
    val cnf = immutableCnf.toMutableCnf
    val levels = ArrayBuffer[DecisionLevel]() // Stack of decision levels
    val unitPropagationQueue = mutable.Queue.empty[Literal] // Queue of literals that should be propagated

    for (clause <- cnf.clauses) if (clause.isUnit) {
      unitPropagationQueue += clause(0) // Unit clauses should be propagated right here
    }

    /**
      * Reverts last decision level for cnf.
      */
    def undo(): Unit = {
      for (literal <- levels.last.varAssessment) {
        literalChooser.unsetLiteral(literal)
        cnf.assignment -= literal
      }
      levels.remove(levels.size - 1)
    }

    /**
      * Ensures that provided literal is true.
      *
      * @param literal which should be true
      */
    def assignLiteral(literal: Literal): Unit = {
      levels.lastOption.foreach(_.varAssessment += literal)
      unitPropagationQueue ++= cnf.assignLiteral(literal)
    }

    /**
      * Applies unit propagation technique to elements in unitPropagationQueue.
      *
      * @return None, if there is no conflicts after propagating all unit clauses
      *         Some(literal), if there is a conflict derived from literal and !literal
      */
    def propagate(): Option[Seq[Literal]] = {
      while (unitPropagationQueue.nonEmpty) {
        val propagationLiteral = unitPropagationQueue.dequeue()
        if (cnf.assignment contains !propagationLiteral) {
          // Found a conflict
          unitPropagationQueue.clear()
          return Some(propagationLiteral)
        } else {
          assignLiteral(propagationLiteral) // Ensure that unit clause literal is true
        }
      }
      None
    }

    if (propagate().isDefined) {
      return false // If there is a conflict from the very start -> CNF is unsatisfiable
    }
    var chosen = literalChooser.chooseLiteral(cnf)
    while (true) {
      chosen = chosen match {
        case Some(literal) =>
          levels += new DecisionLevel(literal)
          assignLiteral(literal)
          propagate() match {
            case Some(conflictLiteral) =>
              while (levels.last.literal.negated) { // Find first level which hasn't been processed twice
                undo()
                if (levels.isEmpty) {
                  return false // All previous levels where processed twice -> CNF is unsatisfiable
                }
              }

              val newClause = conflictAnalyser.learnConflictClause(cnf, conflictLiteral, levels)
              literalChooser.clauseAdded(newClause)
              cnf += newClause

              val flipLiteral = !levels.last.literal
              undo()
              levels += new DecisionLevel(flipLiteral)
              Some(flipLiteral)
            case None =>
              literalChooser.chooseLiteral(cnf)
          }
        case None => return true // There is no literal which can be chose -> CNF is satisfiable
      }
    }
    true
  }
}
