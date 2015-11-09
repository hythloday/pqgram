package io.bimble.pqgram

import java.util.UUID

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

import scalax.collection.GraphEdge
import scalax.collection.GraphEdge._
import scalax.collection.constrained._
import scalax.collection.GraphPredef._

case class Label(uuid: UUID, i: Int)

class PQGramLaws extends PropSpec with Checkers {

  implicit object NodeLabeller extends Labelled[Label] {
    override def label(a: Label): String = a.i.toString
  }

  val genLabel = for {
    uuid <- Gen.uuid
    i <- Gen.posNum[Int]
  } yield Label(uuid, i)

  type G = Graph[Label, DiEdge]
  implicit val tree: Gen[G] = Gen.sized { height =>
    def makeChildren = for {
      xs <- Gen.oneOf(0, 2, 3, 4)
      is <- Gen.containerOfN[Set, Label](xs, genLabel)
    } yield is.toSeq.sortBy(_.i)

    lazy val rowStream: Stream[Seq[Seq[Label]]] = Stream.cons(
      Seq(Seq(genLabel.sample.get)),
      rowStream.map(nodes => nodes.flatten.map(_ => makeChildren.sample.get)))

    def sliding2[N](xs: Seq[N]): Seq[(N, N)] = xs.sliding(2).map { case Seq(a, b) => (a, b) }.toSeq

    val nodes = sliding2(rowStream.take(height + 2))
    nodes.foldLeft(Graph.empty[Label, DiEdge]) {
      case (g, (upper, lower)) =>
        val pairs: Seq[(Label, Seq[Label])] = upper.flatten.zip(lower)
        val marginal: Graph[Label, DiEdge] = Graph.from(upper.flatten ++ lower.flatten, pairs.flatMap { case (l, rs) =>
          rs.map(r => l ~> r)
        })
        g ++ marginal
    }
  }

  implicit val arbTree: org.scalacheck.Arbitrary[Graph[Label,GraphEdge.DiEdge]] = Arbitrary(tree)

  implicit val boundedLabel = new Bounded[Label] {
    override def infimum: Label = new Label(UUID.randomUUID(), Int.MinValue)
    override def supremum: Label = new Label(UUID.randomUUID(), Int.MaxValue)
    override def compareInner(l: Label, r: Label): Int = l.i compare r.i
  }

  case class PQ(p: Int, q: Int)
  implicit val arbPQ = Arbitrary(for { p <- Gen.choose(2, 3); q <- Gen.choose(2, 3)} yield PQ(p, q))

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 2, maxSize = 4)

  property("symmetry") {
    check { (t1: DAG[Label], t2: DAG[Label], pq: PQ) =>
      val p1 = PqExtended(t1, pq.p, pq.q).profile
      val p2 = PqExtended(t2, pq.p, pq.q).profile
      if (p1.distance(p2).isNaN) {
        println(p1.distance(p2), p2.distance(p1))
      }
      Prop(p1.distance(p2) == p2.distance(p1))
    }
  }

  property("metric bounds") {
    check { (t1: DAG[Label], t2: DAG[Label], pq: PQ) =>
      val distance = PqExtended.distance(t1, t2, pq.p, pq.q)
      Prop(distance >= 0 && distance <= 1)
    }
  }

  property("identity") {
    check { (t1: DAG[Label], t2: DAG[Label], pq: PQ) =>
      val p1 = PqExtended(t1, pq.p, pq.q).profile
      Prop(p1.distance(p1) == 0)
    }
  }

  property("triangle inequality") {
    check { (t1: DAG[Label], t2: DAG[Label], t3: DAG[Label], pq: PQ) =>
      val p1 = PqExtended(t1, pq.p, pq.q).profile
      val p2 = PqExtended(t2, pq.p, pq.q).profile
      val p3 = PqExtended(t3, pq.p, pq.q).profile

      Prop(p1.distance(p3) <= p1.distance(p2) + p2.distance(p3))
    }
  }
}
