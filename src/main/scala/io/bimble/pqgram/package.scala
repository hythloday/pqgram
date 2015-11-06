package io.bimble


package object pqgram {
  /**
    * Typeclass for extracting characteristic labels from nodes
    */
  trait Labelled[A] {
    def label(a: A): String
  }

  /**
    * Typeclass for suggesting that elements can be compared and created with lt/gt semantics
    */
  // TODO - eliminate and replace with BoundedT (with Infimum/T/Supremum inhabitants)
  trait CreatableOrdering[N] extends Ordering[N] {
    override def compare(x: N, y: N): Int
    def createLessThan(x: N): N
    def createGreaterThan(x: N): N
    def create: N
  }

}
