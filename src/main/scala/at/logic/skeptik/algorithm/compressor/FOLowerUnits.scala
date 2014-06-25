package at.logic.skeptik.algorithm.compressor

import at.logic.skeptik.proof.sequent.SequentProofNode
import at.logic.skeptik.proof.sequent.lk.Axiom
import at.logic.skeptik.proof.sequent.resolution.UnifyingResolution
import at.logic.skeptik.proof.sequent.resolution.CanRenameVariables
import at.logic.skeptik.judgment.immutable.{ SeqSequent => Sequent }
import collection.mutable.{ Queue, HashMap => MMap }
import at.logic.skeptik.proof.Proof
import at.logic.skeptik.expression._
import collection.mutable.{ HashSet => MSet }
import at.logic.skeptik.algorithm.unifier.{ MartelliMontanari => unify }

object FOLowerUnits
  extends (Proof[SequentProofNode] => Proof[SequentProofNode]) with CanRenameVariables {

  def isUnitClause(s: Sequent) = s.ant.length + s.suc.length == 1

  // ToDo: optimize this by interlacing collectUnits and fixProofNodes

  //I think this method is okay with FOL proofs (nothing changes),
  //but somewhere after this is called we should check the unifiability constraints?
  //or in here as we collect them?
  private def collectUnits(proof: Proof[SequentProofNode]) = {
    var vars = MSet[Var]()
    val unitsList = (proof :\ (Nil: List[SequentProofNode])) { (node, acc) =>
      if (isUnitClause(node.conclusion) && proof.childrenOf(node).length > 1) {
        vars = vars union getSetOfVars(node)
        node :: acc
      } else {
        vars = vars union getSetOfVars(node)
        acc
      }
    }
    (unitsList, vars)
  }

  private def checkListUnifiability(list: Option[List[Sequent]], vars: MSet[Var]) = list match {
    case Some(l) => checkListUnif(l, vars)
    case None => false
  }

  private def checkListUnif(l: List[Sequent], vars: MSet[Var]): Boolean = {
    if (l.length > 1) {
      val first = l.head
      val second = l.tail.head
      //TODO: these are definitely wrong. need to pass the aux formulas from the premise I think
      val mgu = unify((first.ant.head, second.suc.head) :: Nil)(vars) match {
        case None => {
          false
        }
        case Some(u) => {
          true
        }
      }
      if (mgu) {
        checkListUnif(l.tail, vars)
      } else {
        false
      }
    } else {
      true
    }
  }

  private def checkUnifiability(proof: Proof[SequentProofNode], vars: MSet[Var]) = {
    var premiseMap = MMap[SequentProofNode, List[Sequent]]()

    //traverse the proof &
    // collect clauses being unified against units

    //TODO: why do I need fixedPremises?
    def visitForUnifiability(node: SequentProofNode, fixedPremises: Seq[Any]) = node match {
      //TODO: does this check if it is an MRR node?
      case UnifyingResolution(left, right, _, _) => processResolution(left, right, premiseMap)
    }

    proof.foldDown(visitForUnifiability)

    for (k <- premiseMap.keysIterator) {
      if (!checkListUnifiability(premiseMap.get(k), vars)){
    	  premiseMap.put(k, Nil)
      }
    }
    premiseMap
  }

  private def processResolution(left: SequentProofNode, right: SequentProofNode, map: MMap[SequentProofNode, List[Sequent]]) = {
    if (isUnitClause(left.conclusion)) {
      map.put(left, left.conclusion :: map.getOrElse(left, Nil))
    }
    if (isUnitClause(right.conclusion)) {
      map.put(right, right.conclusion :: map.getOrElse(right, Nil))
    }
  }

  private def fixProofNodes(unitsSet: Set[SequentProofNode], proof: Proof[SequentProofNode], vars: MSet[Var]) = {
    val fixMap = MMap[SequentProofNode, SequentProofNode]()

    def visit(node: SequentProofNode, fixedPremises: Seq[SequentProofNode]) = {
      lazy val fixedLeft = fixedPremises.head;
      lazy val fixedRight = fixedPremises.last;

      //TODO: does this check if it is an MRR node?
      val fixedP = node match {
        case Axiom(conclusion) => node
        case UnifyingResolution(left, right, _, _) if unitsSet contains left => fixedRight
        case UnifyingResolution(left, right, _, _) if unitsSet contains right => fixedLeft
        case UnifyingResolution(left, right, _, _) => UnifyingResolution(fixedLeft, fixedRight)(vars)
        case _ => node
      }
      if (node == proof.root || unitsSet.contains(node)) fixMap.update(node, fixedP)
      fixedP
    }
    proof.foldDown(visit)
    fixMap
  }

  def apply(proof: Proof[SequentProofNode]) = {
    val collected = collectUnits(proof)
    val units = collected._1
    var vars = collected._2
    val premiseMap = checkUnifiability(proof, vars)
    

    var toRemove = MSet[SequentProofNode]()
    for (k <- premiseMap.keysIterator) {
      if (premiseMap.get(k) == Nil){
        toRemove.add(k)
      }
    }
    val unitsClean = units.filter(toRemove.contains(_))
    
    val fixMap = fixProofNodes(unitsClean.toSet, proof, vars)

    val root = unitsClean.map(fixMap).foldLeft(fixMap(proof.root))((left, right) => try { UnifyingResolution(left, right)(vars) } catch { case e: Exception => left })
    Proof(root)
  }

}
