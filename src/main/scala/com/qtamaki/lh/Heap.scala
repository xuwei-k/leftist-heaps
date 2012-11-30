package com.qtamaki.lh

sealed abstract class Heap
case class Empty extends Heap
case class Tree[T](rank: Int, node: Ordered[T], left: Heap, Right: Heap) extends Heap

object Heap {
  private def rank(h: Heap): Int = h match {
    case Empty() => 0
    case Tree(x, _, _, _) => x
    case _ => throw new Exception()
  }
  private def makeTree[T](node: Ordered[T], h1: Heap, h2: Heap): Heap = if (rank(h1) >= rank(h2)) {
    Tree(rank(h2) + 1, node, h1, h2)
  } else {
    Tree(rank(h1) + 1, node, h2, h1)
  }
  def merge(heap1: Heap, heap2: Heap): Heap = (heap1, heap2) match {
    case (Empty(), h) => h
    case (h, Empty()) => h
    case (h1 @ Tree(_, x, a1, b1), h2 @ Tree(_, y, a2, b2)) if x < y => makeTree(x, a1, merge(b1, h2))
    case (h1 @ Tree(_, x, a1, b1), h2 @ Tree(_, y, a2, b2)) => makeTree(x, a2, merge(b2, h1))
    case _ => throw new Exception()
  }
  def insert[T](node: Ordered[T], heap: Heap): Heap = {
    merge(Tree(1, node, Empty(), Empty()), heap)
  }
  def findMin[T](h: Heap): Ordered[T] = h match {
    case t: Tree[T] => t.node
    case _ => throw new Exception()
  }
  def deleteMin[T](h: Heap): Heap = h match {
    case Tree(_, x, a, b) => merge(a, b)
    case _ => throw new Exception()
  }
  def listToHeap[T](xs: List[Ordered[T]]) = xs.foldRight[Heap](Empty())(insert _)
  def heapToList[T](h: Heap): List[Ordered[T]] = h match {
    case Empty() => List()
    //case t:Tree[T] => findMin(t) :: heapToList(deleteMin(t))
    case t: Tree[T] => {
      val n: Ordered[T] = findMin(t)
      val list: List[Ordered[T]] = heapToList(deleteMin(t))
      n :: list
    }
    case _ => throw new Exception()
  }
}