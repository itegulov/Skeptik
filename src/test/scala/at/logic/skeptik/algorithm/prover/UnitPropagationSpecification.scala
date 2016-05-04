package at.logic.skeptik.algorithm.prover

import at.logic.skeptik.expression.{Var, i}
import at.logic.skeptik.judgment.Sequent
import at.logic.skeptik.judgment.immutable.SetSequent
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner

/**
  * @author Daniyar Itegulov
  */
@RunWith(classOf[JUnitRunner])
class UnitPropagationSpecification extends SpecificationWithJUnit {
  val a = Var("A", i)
  val b = Var("B", i)
  val c = Var("C", i)
  val d = Var("D", i)

  "UnitPropagation" should {
    "eliminate everything" in {
      val exp: Set[Sequent] = Set(
        new SetSequent(Set(a, b), Set.empty),
        new SetSequent(Set.empty, Set(b, c)),
        new SetSequent(Set(c), Set(d)),
        new SetSequent(Set.empty, Set(a))
      )
      UnitPropagation(exp) shouldEqual (Seq(a, c, d), Seq(b), Set())
    }

    "eliminate half" in {
      val exp: Set[Sequent] = Set(
        new SetSequent(Set(a, b), Set.empty),
        new SetSequent(Set.empty, Set(b, c)),
        new SetSequent(Set(c), Set(d)),
        new SetSequent(Set.empty, Set(c))
      )
      UnitPropagation(exp) shouldEqual (Seq(c, d), Seq(), Set(new SetSequent(Set(a, b), Set.empty)))
    }

    "eliminate in false" in {
      val exp: Set[Sequent] = Set(
        new SetSequent(Set(a), Set.empty),
        new SetSequent(Set.empty, Set(a))
      )
      UnitPropagation(exp) shouldEqual (Seq(), Seq(a), Set(new SetSequent(Set.empty, Set.empty)))
    }
  }
}
