package io.bimble.pqgram

import scala.reflect.runtime.universe._
import scalax.collection.config.GraphConfig
import scalax.collection.constrained._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LkDiEdge


object Implicits {
  implicit val config: GraphConfig = dagConstraint

  implicit class RichDag[N](d: MultiDAG[N]) {
    def root: N = d.nodes.filter(_.diPredecessors.isEmpty).head.value
  }
}

case class PQGram[N : TypeTag : Bounded : Labelled](t: MultiDAG[N], p: Int, q: Int) {
  import Implicits._

  /**
    * Definition 4.3 (pq-Gram) For p > 0 and q > 0, a pq-gram of a tree T is defined as a subtree of the
    * extended tree Tpq that is isomorphic to the pq-gram pattern.
    * @return
    */
  def subgraphs: Seq[MultiDAG[N]] = t.nodes.toSeq.flatMap { node =>
    for {
      ancs <- node.diPredecessors.toSeq.sortBy(_.value).sliding(p - 1)
      descs <- node.diSuccessors.toSeq.sortBy(_.value).sliding(q)
    } yield {
      val nodes = (ancs.toSet ++ descs.toSet + node).map(_.value)
      val edges = ancs.map(a => (a.value ~+#> node.value)(0)) ++ descs.map(d => (node.value ~+#> d.value)(0))
      MultiDAG.from(nodes, edges)
    }
  }

  def profile = PQProfile(subgraphs.map(g => PqExtended.labelTuple(g).mkString("")))
}

/**
  * Definition 4.5 (pq-Gram Profile) For p > 0 and q > 0, the pq-gram profile, Pp,q(T), of a tree T is
  *  defined as the bag of label-tuples l(Gi) of all pq-grams Gi of T.
  */
case class PQProfile(labelTuples: Seq[String]) {

  private lazy val kv = labelTuples.groupBy(identity).mapValues(_.size)
  /**
    *
    * Definition 4.6 (pq-Gram Distance) For p > 0 and q > 0, the pq-gram distance, ∆p,q(T1, T2), between two trees
    * T1 and T2 is defined as follows:
    *
    * ∆p,q(T1, T2) = 1 − 2 |Pp,q(T1) ∩ Pp,q(T2)|
    *                      |Pp,q(T1) ∪ Pp,q(T2)|
    */
  def distance(other: PQProfile): Double = {
    val unionSize = (kv.keySet & other.kv.keySet).toSeq.map(k => math.min(kv(k), other.kv(k))).sum
    val intersectionSize = labelTuples.size + other.labelTuples.size
    1 - 2 * (unionSize.toDouble / intersectionSize.toDouble)
  }
}

trait PqGramOps {
  private def extendRootNode[N : Bounded](root: N, p: Int) = for {
    i <- 0 until p - 1
  } yield (implicitly[Bounded[N]].infimum ~+#> root)(i)

  private def extendLeafNode[N : Bounded](leaf: N, q: Int) = for {
    i <- 0 until q
  } yield (leaf ~+#> implicitly[Bounded[N]].supremum)(i)

  private def extendNonLeafNode[N : Bounded](node: DAG[N]#NodeT, q: Int): List[LkDiEdge[N]] = for {
    side <- List("before", "after")
    i <- 0 until q - 1
  } yield (side, node.diSuccessors.toSeq) match {
    case ("before", xs) => (node.value ~+#> implicitly[Bounded[N]].infimum)(i)
    case ("after", xs) => (node.value ~+#> implicitly[Bounded[N]].supremum)(i)
  }

  /**
    * Definition 4.1 (pq-Extended Tree) Let T be a tree, and p > 0 and q > 0
    * be two integers. The pqextended tree, Tpq, is constructed from T by
    * adding p−1 ancestors to the root node, inserting q−1 children before
    * the first and after the last child of each non-leaf node, and adding q
    * children to each leaf of T. All newly inserted nodes are null nodes that
    * do not occur in T.

    * @param tree the tree to extend
    * @param p insert p-1 ancestors to the root node
    * @param q add q children to each leaf node in t
    * @return the extended tree
    */
  def apply[N : Labelled : TypeTag : Bounded](tree: DAG[N], p: Int, q: Int): PQGram[N] = {
    import Implicits._
    val nTree = MultiDAG.from[N](Nil, tree.edges.toSeq.map{ e =>
      (e.source.value ~+#> e.target.value)(0).edge
    })

    PQGram(nTree ++ tree.nodes.foldLeft(List.empty[LkDiEdge[N]]) {
      case (nodes, root) if root.diPredecessors.isEmpty =>
        nodes ++ extendRootNode(root.value, p) ++ extendNonLeafNode(root, q)
      case (nodes, leaf) if leaf.diSuccessors.isEmpty =>
        nodes ++ extendLeafNode(leaf.value, q)
      case (nodes, node) =>
        nodes ++ extendNonLeafNode(node, q)
    }, p, q)
  }

  /**
    * Definition 4.4 (Label-tuple) Let G be a pq-gram with the nodes V (G) = {v1, ... , vp, vp+1, ... , vp+q}, where vi
    * is the i-th node in preorder. The tuple l(G) = (l(v1), ... , l(vp), l(vp+1), ... , l(vp+q)) is called the
    * label-tuple of G.
    */
  def labelTuple[N : Labelled : Bounded](tree: MultiDAG[N]): List[String] = {
    import Implicits._

//    def edgeCompare(e1: MultiDAG[N]#EdgeT, e2: MultiDAG[N]#EdgeT): Int = {
//      val ord = implicitly[Ordering[N]]
//      val (s1, s2, t1, t2) = (e1.edge.source.value, e2.edge.source.value, e1.edge.target.value, e2.edge.target.value)
//      (ord.compare(s1, s2), ord.compare(t1, t2)) match {
//        case (cmp1, _) if cmp1 != 0 => cmp1
//        case (_, cmp2) => cmp2
//      }
//    }
//
//    def edgeOrdering = tree.EdgeOrdering(edgeCompare)
//    val traverser = tree.innerEdgeTraverser(tree get tree.root).withOrdering(edgeOrdering)
//    val nodesOrdered = traverser.filter(edg => edg.source.diPredecessors.isEmpty).map(_.source.value).toList :::
//      traverser.filter(edg => edg.source.diPredecessors.isEmpty).map(_.target.value).toList :::
//      traverser.filter(edg => edg.target.diPredecessors.isEmpty).map(_.target.value).toList
//    nodesOrdered.map(implicitly[Labelled[N]].label)

    def nodeOrdering = tree.NodeOrdering((l, r) => implicitly[Ordering[N]].compare(l.value, r.value))
    val traverser2 = tree.outerNodeTraverser(tree get tree.root).withOrdering(nodeOrdering)
    traverser2.toList.map(implicitly[Labelled[N]].label)
  }

  def distance[N : Labelled : TypeTag : Bounded](t1: DAG[N], t2: DAG[N], p: Int, q: Int) =
    apply(t1, p, q).profile.distance(apply(t2, p, q).profile)
}

object PqExtended extends PqGramOps
