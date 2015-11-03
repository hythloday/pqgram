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

trait CreatableOrdering[N] extends Ordering[N] {
  override def compare(x: N, y: N): Int
  def createLessThan(x: N): N
  def createGreaterThan(x: N): N
  def create: N
}

case class PQGram[N](t: DAG[N], p: Int, q: Int) {
  import Implicits._
  def subgraphs(implicit tt: TypeTag[N], ord: Ordering[N]): Seq[DAG[N]] = t.nodes.toSeq.flatMap { node =>
    for {
      ancs <- node.diPredecessors.toSeq.sortBy(_.value).sliding(p - 1)
      descs <- node.diSuccessors.toSeq.sortBy(_.value).sliding(q)
    } yield {
      val nodes = (ancs.toSet ++ descs.toSet + node).map(_.value)
      val edges = ancs.map(_.value ~> node.value) ++ descs.map(node.value ~> _.value)
      DAG.from(nodes, edges)
    }
  }
}

/**
  *
  * @tparam N the node type of the tree
  */
trait PqExtender[N] {
  private def extendRootNode(root: N, p: Int)(implicit nullNodes: CreatableOrdering[N]) = for {
    i <- 0 until p - 1
  } yield nullNodes.create ~> root

  private def extendLeafNode(leaf: N, q: Int)(implicit nullNodes: CreatableOrdering[N]) = for {
    i <- 0 until q
  } yield leaf ~> nullNodes.create

  private def extendNonLeafNode(node: DAG[N]#NodeT, q: Int)(implicit nullNodes: CreatableOrdering[N]) = for {
    side <- List("before", "after")
    i <- 0 until q-1
  } yield if (side == "before") { node.value ~> nullNodes.createLessThan(node.diSuccessors.map(_.value).min) }
          else                  { node.value ~> nullNodes.createGreaterThan(node.diSuccessors.map(_.value).max) }

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
  def extend(tree: DAG[N], p: Int, q: Int)(implicit nullNodes: CreatableOrdering[N]): PQGram[N] = PQGram(tree ++ tree.nodes.foldLeft(List.empty[DiEdge[N]]) {
    case (nodes, root) if root.diPredecessors.isEmpty =>
      nodes ++ extendRootNode(root, p) ++ extendNonLeafNode(root, q)
    case (nodes, leaf) if leaf.diSuccessors.isEmpty =>
      nodes ++ extendLeafNode(leaf, q)
    case (nodes, node) =>
      nodes ++ extendNonLeafNode(node, q)
  }, p, q)
}

trait PqSubgraph[N] {
  import Implicits._
  /**
    * Definition 4.3 (pq-Gram) For p > 0 and q > 0, a pq-gram of a tree T is defined as a subtree of the
    * extended tree Tpq that is isomorphic to the pq-gram pattern.
    *
    * @param pq the PQGram to retrieve all subtrees of
    * @return all subtrees of the PQGram
    */
  def subgraphs(pq: PQGram[N])(implicit tt: TypeTag[N], ord: Ordering[N]): Seq[DAG[N]] = pq.t.nodes.toSeq.flatMap { node =>
    for {
      ancs <- node.diPredecessors.toSeq.sortBy(_.value).sliding(pq.p - 1)
      descs <- node.diSuccessors.toSeq.sortBy(_.value).sliding(pq.q)
    } yield {
      val nodes = (ancs.toSet ++ descs.toSet + node).map(_.value)
      val edges = ancs.map(_.value ~> node.value) ++ descs.map(node.value ~> _.value)
      DAG.from(nodes, edges)
    }
  }
}

trait PqGramLabelTuple[N] {
  /**
    * Definition 4.4 (Label-tuple) Let G be a pq-gram with the nodes V (G) = {v1, ... , vp, vp+1, ... , vp+q}, where vi
    * is the i-th node in preorder. The tuple l(G) = (l(v1), ... , l(vp), l(vp+1), ... , l(vp+q)) is called the
    * label-tuple of G.
    */
  def labelTuple(tree: DAG[N], root: N)(implicit ord: CreatableOrdering[N]): List[N] = {
    def nodeOrdering = tree.NodeOrdering((l, r) => ord.compare(l.value, r.value))
    val traverser = tree.outerNodeTraverser(tree get root).withOrdering(nodeOrdering)
    traverser.toList
  }
}

trait PqGramDistance[N] {
  /**
    *
    * Definition 4.6 (pq-Gram Distance) For p > 0 and q > 0, the pq-gram distance, ∆p,q(T1, T2), between two trees
    * T1 and T2 is defined as follows:
    *
    * ∆p,q(T1, T2) = 1 − 2 |Pp,q(T1) ∩ Pp,q(T2)|
    *                      |Pp,q(T1) ∪ Pp,q(T2)|
    */
  // TODO O(n^2) implementation, could be O(n)
  def distance(lt1: Seq[String], l2: Seq[String]): Double = {
    val unionSize = lt1 count (l2 contains _)
    val intersectionSize = lt1.size + l2.size
    val d = 1 - 2 * (unionSize.toDouble / intersectionSize.toDouble)
    d
  }
}

case class PqOps[N]() extends PqExtender[N] with PqSubgraph[N] with PqGramLabelTuple[N] with PqGramDistance[N]
