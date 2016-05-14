package at.logic.skeptik.algorithm.prover

import at.logic.skeptik.expression.E
import at.logic.skeptik.judgment.Sequent
import at.logic.skeptik.judgment.immutable.{SeqSequent, SetSequent}

/**
  * Implements DPLL algorithm.
  *
  * See Handbook of Satisfiability 3.5.
  *
  * @author Daniyar Itegulov
  */
object DPLL {
  // TODO: current implementation is pretty dumb and uses straightforward literal choosing
  private def chooseLiteral(goal: Iterable[Sequent]): E = {
    val sequent = goal.head
    (sequent.ant ++ sequent.suc).head
  }

  /**
    * Solve CNF-SAT problem using DPLL algorithm.
    *
    * @param goal CNF-SAT formula which should be proved
    * @return None, if this formula is unsatisfiable
    *         Some(result) if this formula is satisfiable with literals from result.suc
    *           set to true and literals from result.ant set to false
    */
  def prove(goal: Iterable[Sequent]): Option[Sequent] = {
    val (literals, literalsNegated, exp) = UnitPropagation(goal)
    if (exp.isEmpty) {
      Some(new SeqSequent(literalsNegated, literals))
    } else if (exp.contains(new SetSequent(Set.empty, Set.empty))) {
      None
    } else {
      val literal = chooseLiteral(goal)
      prove(eliminate(exp, negated = false, literal)) match {
        case Some(result) =>
          val ant = (result.ant ++ literalsNegated).toSeq
          val suc = literal +: (result.suc ++ literals).toSeq
          Some(new SeqSequent(ant, suc))
        case _ =>
          prove(eliminate(exp, negated = true, literal)) match {
            case Some(result) =>
              val ant = literal +: (result.ant ++ literalsNegated).toSeq
              val suc = (result.suc ++ literals).toSeq
              Some(new SeqSequent(ant, suc))
            case _ => None
          }
      }
    }
  }
}
