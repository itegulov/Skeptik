package at.logic.skeptik.algorithm.prover

import at.logic.skeptik.expression.E
import at.logic.skeptik.judgment.Sequent
import at.logic.skeptik.judgment.immutable.SetSequent

/**
  * Implements DPLL algorithm.
  *
  * See Handbook of Satisfiability 3.5.
  *
  * @author Daniyar Itegulov
  */
object DPLL {
  // TODO: current implementation is pretty dumb and uses straightforward literal choosing
  private def chooseLiteral(goal: Set[Sequent]): E = {
    val sequent = goal.head
    (sequent.ant ++ sequent.suc).head
  }

  /**
    * Solve CNF-SAT problem using DPLL algorithm.
    *
    * @param goal CNF-SAT formula which should be proved
    * @return None, if this formula is unsatisfiable
    *         Some(lit, negLit) if this formula is satisfiable with literals from lit
    *           set to true and literals from negLit set to false
    */
  def prove(goal: Set[Sequent]): Option[(Seq[E], Seq[E])] = {
    val (literals, literalsNegated, exp) = UnitPropagation(goal)
    if (exp.isEmpty) {
      Some((literals, literalsNegated))
    } else if (exp.contains(new SetSequent(Set.empty, Set.empty))) {
      None
    } else {
      val literal = chooseLiteral(goal)
      prove(eliminate(exp, negated = false, literal)) match {
        case Some((rLiterals, rLiteralsNegated)) =>
          Some(literal +: (rLiterals ++ literals), rLiteralsNegated ++ literalsNegated)
        case _ =>
          prove(eliminate(exp, negated = true, literal)) match {
            case Some((rLiterals, rLiteralsNegated)) =>
              Some(rLiterals ++ literals, literal +: (rLiteralsNegated ++ literalsNegated))
            case _ => None
          }
      }
    }
  }
}
