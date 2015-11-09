package io.bimble.pqgram

import org.scalatest.{FlatSpec, Matchers}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.constrained._
import scalax.collection.constrained.constraints.{Acyclic, Connected}
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LkDiEdge

sealed trait TestNode {
  def order: Int
  def label: String
}
case class LabelledNode(order: Int, label: String) extends TestNode
class NullNode(val order: Int) extends TestNode { val label = "*" }

object toDot {
  def dump[N, E[X] <: EdgeLikeIn[X]](d: scalax.collection.constrained.Graph[N, E], name: String): String = {
    import scalax.collection.Graph
    import scalax.collection.io.dot._
    import implicits._

    val root = DotRootGraph(directed = true, id = Some(name))
    def edgeTransformer(innerEdge: Graph[N, E]#EdgeT): Option[(DotGraph,DotEdgeStmt)] = innerEdge.edge match {
      case DiEdge(source, target) =>
        Some((root, DotEdgeStmt(source.toString, target.toString, List(DotAttr("id", s"${source.toString} ~> ${target.toString}")))))
      case LkDiEdge(source, target, key) =>
        Some((root, DotEdgeStmt(source.toString, target.toString, List(DotAttr("id", key.toString)))))
    }
    def nodeTransformer(innerNode: Graph[N, E]#NodeT): Option[(DotGraph, DotNodeStmt)] = innerNode.value match {
      case l@LabelledNode(order, label) => Some(root, DotNodeStmt(innerNode.toString, Seq(DotAttr("label", label))))
      case n: NullNode => Some(root, DotNodeStmt(innerNode.toString, Seq(DotAttr("label", "*"))))
    }
    val str = new Export(d).toDot(root, edgeTransformer, cNodeTransformer = Some(nodeTransformer), iNodeTransformer = Some(nodeTransformer))
    import java.nio.charset.StandardCharsets
    import java.nio.file.{Files, Paths}

    Files.write(Paths.get(s"$name.dot"), str.getBytes(StandardCharsets.UTF_8))

    str
  }
}

class PQGramTests extends FlatSpec with Matchers {

  import Implicits._

  implicit object TestNodeLabeller extends Labelled[TestNode] {
    override def label(a: TestNode): String = a.label
  }
  implicit object LabelledNodeLabeller extends Labelled[LabelledNode] {
    override def label(a: LabelledNode): String = a.label
  }

  implicit object StringLabeller extends Labelled[String] {
    override def label(a: String): String = a
  }

  implicit object BoundedStrings extends Bounded[String] {
    override val infimum = "__INFIMUM"
    override val supremum = "__SUPREMUM"
    override def compareInner(x: String, y: String): Int = x compare y
  }

  implicit object BoundedTestNode extends Bounded[TestNode] {
    override def infimum: TestNode = new NullNode(Int.MinValue)
    override def supremum: TestNode = new NullNode(Int.MaxValue)
    override def compareInner(x: TestNode, y: TestNode): Int = x.order compare y.order
  }

  implicit object BoundedLabelledNode extends Bounded[LabelledNode] {
    override def infimum: LabelledNode = new LabelledNode(Int.MinValue, "*")
    override def supremum: LabelledNode = new LabelledNode(Int.MaxValue, "*")
    override def compareInner(x: LabelledNode, y: LabelledNode): Int = x.order compare y.order
  }

  implicit val conf: Config = Connected && Acyclic

  "pqgram-extend" should "give the same result as the paper" in {
    val (v1, v2, v3, v4, v5, v6) = (LabelledNode(1, "a"), LabelledNode(2, "a"), LabelledNode(3, "e"),
      LabelledNode(4, "b"), LabelledNode(5, "b"), LabelledNode(6, "c"))

    val t = DAG.from[TestNode](
      List(v1, v2, v3, v4, v5, v6),
      List(v1 ~> v2, v1 ~> v5, v1 ~> v6, v2 ~> v3, v2 ~> v4)
    )
    val tExtended = PqExtended(t, 2, 3)

    val (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20, o21) = (
      LabelledNode(1, "*"), LabelledNode(2, "*"), LabelledNode(3, "*"), LabelledNode(4, "*"), LabelledNode(5, "*"),
      LabelledNode(6, "*"), LabelledNode(7, "*"), LabelledNode(8, "*"), LabelledNode(9, "*"), LabelledNode(10, "*"),
      LabelledNode(11, "*"), LabelledNode(12, "*"), LabelledNode(13, "*"), LabelledNode(14, "*"), LabelledNode(15, "*"),
      LabelledNode(16, "*"), LabelledNode(17, "*"), LabelledNode(18, "*"), LabelledNode(19, "*"), LabelledNode(20, "*"),
      LabelledNode(21, "*")
    )
    val expected = DAG.from(
      List(
        v1, v2, v3, v4, v5, v6,
        o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20, o21
      ),
      List(
        o1 ~> v1,
        v1 ~> o2, v1 ~> o3, v1 ~> v2, v1 ~> v5, v1 ~> v6, v1 ~> o20, v1 ~> o21,
        v2 ~> o4, v2 ~> o5, v2 ~> v3, v2 ~> v4, v2 ~> o12, v2 ~> o13,
        v3 ~> o6, v3 ~> o7, v3 ~> o8,
        v4 ~> o9, v4 ~> o10, v4 ~> o11,
        v5 ~> o14, v5 ~> o15, v5 ~> o16,
        v6 ~> o17, v6 ~> o18, v6 ~> o19
      )
    )

    tExtended.t.order shouldEqual expected.order
    tExtended.t.size shouldEqual expected.size
  }

  "pqgram-pattern" should "recognize a tree of the right shape" in {
    val (p1, p2, p3, p4, p5) = ("p1", "p2", "p3", "p4", "p5")

    val t = MultiDAG.from(
      List(p1, p2, p3, p4, p5),
      List((p1 ~+#> p2)(0), (p2 ~+#> p3)(0), (p2 ~+#> p4)(0), (p2 ~+#> p5)(0))
    )

    PQGram(t, 2, 3).subgraphs shouldEqual Seq(t)
  }

  "pqgram-labeltuple" should "create the right label tuples" in {
    val (o1, v1, o2, o3, v2) = (LabelledNode(1, "*"), LabelledNode(2, "a"), LabelledNode(3, "*"), LabelledNode(4, "*"),
      LabelledNode(5, "a"))

    val t = MultiDAG.from(
      List(o1, v1, o2, o3, v2),
      List((o1 ~+#> v1)(0), (v1 ~+#> o2)(0), (v1 ~+#> o3)(0), (v1 ~+#> v2)(0))
    )

    PqExtended.labelTuple(t).mkString shouldEqual "*a**a"
  }

  it should "give the same result on t1" in {
    val (v1, v2, v3, v4, v5, v6) = (LabelledNode(1, "a"), LabelledNode(2, "a"), LabelledNode(3, "e"),
      LabelledNode(4, "b"), LabelledNode(5, "b"), LabelledNode(6, "c"))

    val t1 = DAG.from[TestNode](
      List(v1, v2, v3, v4, v5, v6),
      List(v1 ~> v2, v1 ~> v5, v1 ~> v6, v2 ~> v3, v2 ~> v4)
    )

    PqExtended(t1, 2, 3).profile.labelTuples.sorted shouldEqual List(
      "*a**a", "aa**e", "ae***", "aa*eb", "ab***", "aaeb*", "aab**", "*a*ab", "ab***", "*aabc", "ac***", "*abc*", "*ac**"
    ).sorted
  }

  it should "give the same results on t2" in {
    val (w5, w1, w7, w9, w3, w6) = (LabelledNode(5, "a"), LabelledNode(2, "a"), LabelledNode(7, "e"),
      LabelledNode(9, "b"), LabelledNode(3, "b"), LabelledNode(6, "x"))

    val t2 = DAG.from[TestNode](
      List(w5, w1, w7, w9, w3, w6),
      List(w5 ~> w1, w5 ~> w3, w5 ~> w6, w1 ~> w7, w1 ~> w9)
    )

    PqExtended(t2, 2, 3).profile.labelTuples.sorted shouldEqual List(
      "*a**a", "aa**e", "ae***", "aa*eb", "ab***", "aaeb*", "aab**", "*a*ab", "ab***", "*aabx", "ax***", "*abx*", "*ax**"
    ).sorted
  }

  "pqgram-distance" should "calculate same distance as paper" in {

    import Implicits._

    val (v1, v2, v3, v4, v5, v6) = (LabelledNode(1, "a"), LabelledNode(2, "a"), LabelledNode(3, "e"),
      LabelledNode(4, "b"), LabelledNode(5, "b"), LabelledNode(6, "c"))

    val t1 = DAG.from[TestNode](
      List(v1, v2, v3, v4, v5, v6),
      List(v1 ~> v2, v1 ~> v5, v1 ~> v6, v2 ~> v3, v2 ~> v4)
    )

    val (w5, w1, w7, w9, w3, w6) = (LabelledNode(5, "a"), LabelledNode(2, "a"), LabelledNode(7, "e"),
      LabelledNode(9, "b"), LabelledNode(3, "b"), LabelledNode(6, "x"))

    val t2 = DAG.from[TestNode](
      List(w5, w1, w7, w9, w3, w6),
      List(w5 ~> w1, w5 ~> w3, w5 ~> w6, w1 ~> w7, w1 ~> w9)
    )

    val p1 = PqExtended(t1, 2, 3).profile
    val p2 = PqExtended(t2, 2, 3).profile

    p1.distance(p2) shouldBe > (0.305)
    p1.distance(p2) shouldBe <= (0.31)
  }
}
