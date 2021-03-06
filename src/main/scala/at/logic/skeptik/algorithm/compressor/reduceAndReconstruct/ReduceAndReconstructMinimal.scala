package at.logic.skeptik.algorithm.compressor.reduceAndReconstruct

import at.logic.skeptik.proof.Proof
import at.logic.skeptik.proof.sequent.SequentProofNode
import at.logic.skeptik.proof.sequent.lk.R

import baseRules._

abstract class ReduceMin
extends Reduce(Seq(b3,b1))

class ReduceAndReconstructMin(val timeout: Int)
extends ReduceMin with TimeoutTermination


// Variants

object RRMinSimpleTermination
extends ReduceMin with SimpleTermination

object RRMinRandomTermination
extends ReduceMin with RandomTermination
