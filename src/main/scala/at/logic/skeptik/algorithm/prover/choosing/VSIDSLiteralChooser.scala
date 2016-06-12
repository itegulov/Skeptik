package at.logic.skeptik.algorithm.prover.choosing

import at.logic.skeptik.algorithm.prover._
import at.logic.skeptik.algorithm.prover.structure.mutable.CNF
import at.logic.skeptik.algorithm.prover.structure.immutable
import at.logic.skeptik.expression.E

import scala.collection.mutable

/**
  * VSIDS heuristic described in Handbook of Sat §4.5.3.
  *
  * @author Daniyar Itegulov
  */
class VSIDSLiteralChooser(val cnf: immutable.CNF) extends LiteralChooser {
  val literalScore = mutable.Map.empty[Literal, Int]
  val varNumber = mutable.Map.empty[Literal, Int]
  cnf.variables.zipWithIndex.foreach { case (v, i) =>
    val literal: Literal = v
    varNumber += literal -> (i + 1)
    varNumber += !literal -> -(i + 1)
  }

  var literalsTree = mutable.SortedSet.empty[(Int, Literal)](new Ordering[(Int, Literal)] {
    override def compare(x: (Int, Literal), y: (Int, Literal)): Int = y._1 compare x._1 match {
      case 0 => varNumber(x._2) compare varNumber(y._2)
      case o => o
    }
  })

  cnf.variables.foreach(v => {
    val literal: Literal = v
    literalScore += literal -> 0
    literalScore += !literal -> 0
  })
  cnf.clauses.flatMap(c => c.literals).foreach(l => literalScore(l) += 1)
  cnf.variables.foreach(v => {
    val literal: Literal = v
    literalsTree += ((literalScore(literal), literal))
    literalsTree += ((literalScore(!literal), !literal))
  })

  override def chooseLiteral(cnf: CNF): Option[(E, Boolean)] = {
    literalsTree = literalsTree.dropWhile { case (_, literal) =>
      (cnf.assignment contains literal) || (cnf.assignment contains !literal)
    }
    if (literalsTree.isEmpty) {
      None
    } else {
      val (_, literal) = literalsTree.firstKey
      Some(literal)
    }
  }

  override def clauseAdded(clause: Clause): Unit =
    clause.literals.foreach(l => literalScore(l) += 1)

  override def unsetLiteral(literal: Literal): Unit = {
    literalsTree += ((literalScore(literal), literal))
    literalsTree += ((literalScore(!literal), !literal))
  }
}
