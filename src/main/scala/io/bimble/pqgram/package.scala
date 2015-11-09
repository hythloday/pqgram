package io.bimble

import scalax.collection.constrained.{CompanionAlias, dagConstraint}
import scalax.collection.edge.LkDiEdge


package object pqgram {
  /**
    * Typeclass for extracting characteristic labels from nodes
    */
  trait Labelled[A] {
    def label(a: A): String
  }

  /**
    * Typeclass for indicating that a type has a supremum and an infimum, which compare greater-than and less-than
    * with all inhabitatnts of the type.
    */
  trait Bounded[A] extends Ordering[A] {
    def infimum: A
    def supremum: A

    def compareInner(x: A, y: A): Int

    override def compare(x: A, y: A): Int =
      if (x == infimum) -1
      else if (y == infimum) 1
      else if (x == supremum) 1
      else if (y == supremum) -1
      else compareInner(x, y)
  }

  /** Default (immutable) directed acyclic `Graph`. */
  type MultiDAG[N] = scalax.collection.constrained.Graph[N,LkDiEdge]
  /** Companion module for default (immutable) directed acyclic `Graph`. */
  object MultiDAG extends CompanionAlias[LkDiEdge](dagConstraint)
}
