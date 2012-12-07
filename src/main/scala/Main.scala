import com.qtamaki.lh.Heap
import com.qtamaki.lh.Empty

object Main {

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8)
    val h1 = Heap.merge(Empty, Empty)
    val h2 = Heap.insert(1, Empty)
    val h3 = Heap.listToHeap(list)
    println("hello")
  }

}
