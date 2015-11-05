package io.bimble


package object pqgram {
  /**
    * Typeclass for extracting characteristic labels from nodes
    */
  trait Labelled[A] {
    def label(a: A): String
  }
}
