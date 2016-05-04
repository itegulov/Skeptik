package at.logic.skeptik.algorithm.prover

import at.logic.skeptik.expression.{Var, i}
import at.logic.skeptik.judgment.immutable.SetSequent
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import at.logic.skeptik.algorithm.prover.DPLL._
import at.logic.skeptik.judgment.Sequent

/**
  * @author Daniyar Itegulov
  */
@RunWith(classOf[JUnitRunner])
class DPLLSpecification extends SpecificationWithJUnit {
  val a = new Var("A", i)
  val b = new Var("B", i)
  val c = new Var("C", i)
  val d = new Var("D", i)

  "DPLL" should {
    "prove successfully" in {
      val exp: Set[Sequent] = Set(
        new SetSequent(Set(a), Set(b)),
        new SetSequent(Set(b, c), Set.empty),
        new SetSequent(Set(d), Set(c))
      )
      prove(exp) shouldEqual Some(Seq(a, b), Seq(c, d))
    }

    "not prove" in {
      val exp: Set[Sequent] = Set(
        new SetSequent(Set.empty, Set(a)),
        new SetSequent(Set(a), Set.empty)
      )
      prove(exp) shouldEqual None
    }
  }
}
