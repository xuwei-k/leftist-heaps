package com.qtamaki.lh

import scala.math.Ordered._

sealed abstract class Heap[T]
case class Empty[T <% Ordered[T]] extends Heap[T]
case class Tree[T <% Ordered[T]](rank: Int, node: T, left: Heap[T], right: Heap[T]) extends Heap[T]

object Heap {
  private def rank[T](h: Heap[T]): Int = h match {
    case Empty() => 0
    case Tree(x, _, _, _) => x
    case _ => throw new Exception()
  }
  private def makeTree[T <% Ordered[T]](node: T, h1: Heap[T], h2: Heap[T]): Heap[T] = if (rank(h1) >= rank(h2)) {
    Tree(rank(h2) + 1, node, h1, h2)
  } else {
    Tree(rank(h1) + 1, node, h2, h1)
  }
  def merge[T <% Ordered[T]](heap1: Heap[T], heap2: Heap[T]): Heap[T] = (heap1, heap2) match {
    case (Empty(), h) => h
    case (h, Empty()) => h
    //    case (h1 @ Tree(_, x, a1, b1), h2 @ Tree(_, y, a2, b2)) if x < y => makeTree(x, a1, merge(b1, h2))
    //    case (h1 @ Tree(_, x, a1, b1), h2 @ Tree(_, y, a2, b2)) => makeTree(x, a2, merge(b2, h1))
    case (h1: Tree[T], h2: Tree[T]) => {
      if (h1.node < h2.node) {
        makeTree(h1.node, h1.left, merge(h1.right, h2))
      } else {
        makeTree(h2.node, h2.left, merge(h2.right, h1))
      }
    }
    case _ => throw new Exception()
  }
  def insert[T<% Ordered[T]](node: T, heap: Heap[T]): Heap[T] = {
    merge(Tree(1, node, Empty(), Empty()), heap)
  }
  def findMin[T<% Ordered[T]](h: Heap[T]): T = h match {
    case t: Tree[T] => t.node
    case _ => throw new Exception()
  }
  def deleteMin[T<% Ordered[T]](h: Heap[T]): Heap[T] = h match {
    case Tree(_, x, a, b) => merge(a, b)
    case _ => throw new Exception()
  }
  def listToHeap[T<% Ordered[T]](xs: List[T]) = xs.foldRight[Heap[T]](Empty())(insert _)
  def heapToList[T<% Ordered[T]](h: Heap[T]): List[T] = h match {
    case Empty() => List()
    //case t:Tree[T] => f(implicit c:ClassManifest[T])indMin(t) :: heapToList(deleteMin(t))
    case t: Tree[T] => {
      val n: T = findMin(t)
      val list: List[T] = heapToList(deleteMin(t))
      n :: list
    }
    case _ => throw new Exception()
  }
}