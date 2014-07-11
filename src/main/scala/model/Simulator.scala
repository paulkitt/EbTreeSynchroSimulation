package model

import src.main.scala.model.EbTree
/**
 * Created by prototyp on 12.06.14.
 */
/**
 * Created by prototyp on 11.06.14.
 */
object Simulator {
  def main(args: Array[String]) {
    val tree: EbTree[Int] = new EbTree[Int]
    tree.put(1000L, 100)
    tree.put(1001L, 102)
    tree.put(1023L, 111)
    tree.put(1312L, 123123)
    tree.put(1024L,1234)
    tree.put(1025L,2222)
    tree.put(1026L,222)

print(tree.firstKey())
print(tree.lastKey())

  }
}


