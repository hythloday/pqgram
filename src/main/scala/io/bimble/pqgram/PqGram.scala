package io.bimble.pqgram

import scala.reflect.runtime.universe._
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.config.GraphConfig
import scalax.collection.constrained._

object Implicits {
  implicit val config: GraphConfig = dagConstraint

  implicit class RichDag[N](d: DAG[N]) {
    def root: N = d.nodes.filter(_.diPredecessors.isEmpty).head.value
  }
}

case class PQGram[N : CreatableOrdering : TypeTag : Labelled](t: DAG[N], p: Int, q: Int) {
  import Implicits._

  /**
    * Definition 4.3 (pq-Gram) For p > 0 and q > 0, a pq-gram of a tree T is defined as a subtree of the
    * extended tree Tpq that is isomorphic to the pq-gram pattern.
    * @return
    */
  def subgraphs: Seq[DAG[N]] = t.nodes.toSeq.flatMap { node =>
    for {
      ancs <- node.diPredecessors.toSeq.sortBy(_.value).sliding(p - 1)
      descs <- node.diSuccessors.toSeq.sortBy(_.value).sliding(q)
    } yield {
      val nodes = (ancs.toSet ++ descs.toSet + node).map(_.value)
      val edges = ancs.map(_.value ~> node.value) ++ descs.map(node.value ~> _.value)
      DAG.from(nodes, edges)
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
  private def extendRootNode[N : CreatableOrdering](root: N, p: Int) = for {
    i <- 0 until p - 1
  } yield implicitly[CreatableOrdering[N]].create ~> root

  private def extendLeafNode[N : CreatableOrdering](leaf: N, q: Int) = for {
    i <- 0 until q
  } yield leaf ~> implicitly[CreatableOrdering[N]].create

  private def extendNonLeafNode[N : CreatableOrdering](node: DAG[N]#NodeT, q: Int) = for {
    side <- List("before", "after")
    i <- 0 until q - 1
  } yield (side, node.diSuccessors.toSeq) match {
    case ("before", Seq()) => node.value ~> implicitly[CreatableOrdering[N]].create
    case ("before", xs) => node.value ~> implicitly[CreatableOrdering[N]].createLessThan(xs.map(_.value).min)
    case ("after", Seq()) => node.value ~> implicitly[CreatableOrdering[N]].create
    case ("after", xs) => node.value ~> implicitly[CreatableOrdering[N]].createGreaterThan(xs.map(_.value).max)
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
  def apply[N : Labelled : CreatableOrdering : TypeTag](tree: DAG[N], p: Int, q: Int): PQGram[N] = PQGram(tree ++ tree.nodes.foldLeft(List.empty[DiEdge[N]]) {
    case (nodes, root) if root.diPredecessors.isEmpty =>
      nodes ++ extendRootNode(root.value, p) ++ extendNonLeafNode(root, q)
    case (nodes, leaf) if leaf.diSuccessors.isEmpty =>
      nodes ++ extendLeafNode(leaf.value, q)
    case (nodes, node) =>
      nodes ++ extendNonLeafNode(node, q)
  }, p, q)

  /**
    * Definition 4.4 (Label-tuple) Let G be a pq-gram with the nodes V (G) = {v1, ... , vp, vp+1, ... , vp+q}, where vi
    * is the i-th node in preorder. The tuple l(G) = (l(v1), ... , l(vp), l(vp+1), ... , l(vp+q)) is called the
    * label-tuple of G.
    */
  def labelTuple[N : Labelled : CreatableOrdering](tree: DAG[N]): List[String] = {
    import Implicits._
    def nodeOrdering = tree.NodeOrdering((l, r) => implicitly[Ordering[N]].compare(l.value, r.value))
    val traverser = tree.outerNodeTraverser(tree get tree.root).withOrdering(nodeOrdering)
    traverser.toList.map(implicitly[Labelled[N]].label)
  }

  def distance[N : Labelled : CreatableOrdering : TypeTag](t1: DAG[N], t2: DAG[N], p: Int, q: Int) =
    apply(t1, p, q).profile.distance(apply(t2, p, q).profile)
}

object PqExtended extends PqGramOps
