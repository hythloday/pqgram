package io.bimble.pqgram

import org.scalacheck.Prop
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

class BoundingLaws extends PropSpec with Checkers {

  implicit object BoundedString extends Bounded[String] {
    override val infimum: String = "__INFIMUM"
    override val supremum: String = "__SUPREMUM"
    override def compareInner(x: String, y: String): Int = x compare y
  }

  property("infimum < *") {
    check { (s: String) =>
      Prop(implicitly[Bounded[String]].compare(implicitly[Bounded[String]].infimum, s) < 0)
    }
  }

  property("* > infimum") {
    check { (s : String) =>
      Prop(implicitly[Bounded[String]].compare(s, implicitly[Bounded[String]].infimum) > 0)
    }
  }

  property("supremum > *") {
    check { (s: String) =>
      Prop(implicitly[Bounded[String]].compare(implicitly[Bounded[String]].supremum, s) > 0)
    }
  }

  property("* < supremum") {
    check { (s: String) =>
      Prop(implicitly[Bounded[String]].compare(s, implicitly[Bounded[String]].supremum) < 0)
    }
  }

  property("* <=> *") {
    check { (s1: String, s2: String) =>
      Prop.classify(s1 == s2, "==") { implicitly[Bounded[String]].compare(s1, s2) == 0 } ||
        Prop.classify(s1 < s2, "<") { implicitly[Bounded[String]].compare(s1, s2) < 0 } ||
        Prop.classify(s1 > s2, ">") { implicitly[Bounded[String]].compare(s1, s2) > 0 }
    }
  }

}