package model

import org.scalatest._
import src.main.scala.model.{Child, Leaf, EbTree, Node}


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
    tree.myRoot =  Some(Node[Int](64))
    tree.myRoot.get.setChild(1,Leaf[Int](1,100))

    val leaf = tree.findLeaf(2)
    assert(leaf.isEmpty)
  }


  test("remove is invoked on EbTree with one leaf"){
    val tree = new EbTree[Int]
    tree.myRoot =  Some(Node[Int](64))
    tree.myRoot.get.setChild(1,Leaf[Int](1,100))

  }


  test("isLeaf is invoked with Node "){
    val tree = new EbTree[Int]
    val treeItem:Child[Int] = Node[Int](10)
    assert(tree.isLeaf(treeItem) == false)
  }
  test("isLeaf is invoked with Leaf"){
    val tree = new EbTree[Int]
    val treeItem:Child[Int] = Leaf[Int](1000,1)
    assert(tree.isLeaf(treeItem) == true)
  }
  test("getDeltaByBit is invoked with correct bitAddress"){
    val tree = new EbTree[Int]
    tree.put(1000L, 100)
    tree.put(1001L, 102)
    tree.put(1023L, 111)
    tree.put(1312L, 123123)
    tree.put(1025L,2222)
    tree.put(1026L,222)
    tree.put(1033L,222)
    tree.put(1032L,222)
    tree.put(1045L,222)
    tree.put(1800L,222)
    val bitAddress:List[Int] = List(1,0,0)
    val delta:(Long,(Boolean,Long),(Boolean,Long)) = tree.getDeltaByBit(bitAddress)
    assert(delta==(1047,(false,2),(true,1045)))
  }
  test("getDeltaByBit is invoked with incorrect bitAddress"){
    val tree = new EbTree[Int]
    tree.put(1000L, 100)
    tree.put(1001L, 102)
    tree.put(1023L, 111)
    tree.put(1312L, 123123)
    tree.put(1025L,2222)
    tree.put(1026L,222)
    tree.put(1033L,222)
    tree.put(1032L,222)
    tree.put(1045L,222)
    tree.put(1800L,222)
    val bitAddress:List[Int] = List(1,0,0,1)
    val delta:(Long,(Boolean,Long),(Boolean,Long)) = tree.getDeltaByBit(bitAddress)
    assert(delta==null)
  }
}
