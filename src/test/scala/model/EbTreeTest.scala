package model

import org.scalatest._
import src.main.scala.model.EbTree
import src.main.scala.model.EbTree.Leaf


/**
 * Test class for testing EbTree functionality
 * Created by prototyp on 09.06.14.
 */

class EbTreeTest extends FunSuite{

  test("findLeaf is invoked on a empty EbTree"){
    val tree = new EbTree[Int]
    val leaf = tree.findLeaf(2)
     assert(leaf.isEmpty)
  }
  test("findLeaf is invoked on EbTree with one element"){
    val tree = new EbTree[Int]
    tree.myRoot =  tree.Node[Int](64)
    tree.myRoot.setChild(1,tree.Leaf[Int](1,100))

    val leaf = tree.findLeaf(2)
    assert(leaf.isEmpty)
  }





  test("remove is invoked on EbTree with one leaf"){
    val tree = new EbTree[Int]
    tree.myRoot =  tree.Node[Int](64)
    tree.myRoot.setChild(1,tree.Leaf[Int](1,100))

  }
}
