package model

import org.scalatest.FunSuite
import src.main.scala.model.{Delta, EbTree}
import scala.util.control._

/**
 * Created by prototyp on 12.08.14.
 */
class EbTreeSynchro extends FunSuite {

  val startDelta = Delta(0, 64, -1, -1, -2)

  test("nextDelta is invoked on empty tree") {
    val tree = new EbTree[Int]
    val nextDelta = tree.nextDelta(Delta(0, 4, 3, 1000, 1001))
    assert(nextDelta == null)
  }

  test("nextDelta is invoked on tree with one element") {
    val tree = new EbTree[Int]
    tree.put(1000L, 100)
    val nextDelta = tree.nextDelta(Delta(0, 4, 3, 1000, 1001))
    assert(nextDelta == null)
  }
  test("nextDelta is invoked with Delta leading to synchronized state") {
    //case A
    val tree: EbTree[Int] = new EbTree[Int]
    List(1000L, 1001L, 1024L, 1023L, 1025L, 1026L, 1032L, 1033L, 1045L, 1312L, 1800L).foreach(x => tree.put(x, 1))
    val nextDelta = tree.nextDelta(Delta(0, 64, 0, -2, -2))
    assert(tree.nextDelta(nextDelta) == null)
  }
  test("nextDelta is invoked with Delta locating diff leaf on left side") {
    // case B
    val tree: EbTree[Int] = new EbTree[Int]
    tree.put(1000L, 1)
    tree.put(1001L, 1)
    tree.put(1023L, 2)

    val nextDelta = tree.nextDelta(Delta(0, 64, 4, 1, 1024))
    assert(nextDelta == Delta(16, 4, -1, 1023, 1023))
  }

  test("nextDelta is invoked on tree with bit small bitLVL gap and one item to synchronize") {
    // case D
    val treeA: EbTree[Int] = new EbTree[Int]
    treeA.put(1000L, 1)
    treeA.put(1001L, 1)
    val treeB: EbTree[Int] = new EbTree[Int]
    treeB.put(1023L, 2)
    treeB.put(1000L, 1)
    treeB.put(1001L, 2)

    val reply = findUniqueLeaf(treeA, treeB)
    assert(reply == Delta(-1, -1, -1, 1023, 1023))
  }

  //F) -> D)
  test("nextDelta is invoked on tree with bit small bitLVL gap left and big diff on right side") {
    val treeA: EbTree[Int] = new EbTree[Int]
    List(1023L, 1000L, 1001L).foreach(x => treeA.put(x, 1))
    val treeB = new EbTree[Int]
    List(1000L, 1001L, 1024L, 1023L, 1025L, 1026L, 1032L, 1033L, 1045L, 1312L, 1800L).foreach(x => treeB.put(x, 1))

    assert(findUniqueLeaf(treeA, treeB) == Delta(-1, -1, -1, 1024, 1024))
  }

  //G) - > E*)
  test("nextDelta is invoked on trees  G) - > E*)") {
    val treeA: EbTree[Int] = new EbTree[Int]
    List(1033L, 1045L).foreach(x => treeA.put(x, 1))
    val treeB: EbTree[Int] = new EbTree[Int]
    List(1025L, 1026L, 1026L, 1032L, 1033L, 1312L, 1800L).foreach(x => treeB.put(x, 1))


    val reply = findUniqueLeaf(treeA, treeB)
    assert(reply == Delta(-1, -1, -1, 1025, 1025))
  }
  //G) -> E**)
  test("nextDelta is invoked on trees G) -> E**)") {
    val treeA: EbTree[Int] = new EbTree[Int]
    List(1025L, 1026L).foreach(x => treeA.put(x, 1))
    val treeB: EbTree[Int] = new EbTree[Int]
    List(1024L, 1025L, 1026L, 1032L, 1033L, 1045L, 1312L, 1800L).foreach(x => treeB.put(x, 1))


    val reply = findUniqueLeaf(treeA, treeB)
    assert(reply == Delta(-1, -1, -1, 1024, 1024))
  }
  //G) - > E***)
  test("nextDelta is invoked on trees  G) - > E***)") {
    val treeA: EbTree[Int] = new EbTree[Int]
    List(1028L, 1029L).foreach(x => treeA.put(x, 1))
    val treeB: EbTree[Int] = new EbTree[Int]
    List(1025L, 1026L, 1032L, 1033L, 1312L, 1800L).foreach(x => treeB.put(x, 1))

    val reply = findUniqueLeaf(treeA, treeB)
    assert(reply == Delta(-1, -1, -1, 1025, 1025))
  }

  //B*
  test("nextDelta is invoked at case B*") {
    val treeA: EbTree[Int] = new EbTree[Int]
    List(1000L, 1001L, 1025L).foreach(x => treeA.put(x, 1))
    val treeB: EbTree[Int] = new EbTree[Int]
    List(1000L, 1001L, 1024L, 1025L, 1800L).foreach(x => treeB.put(x, 1))


    val reply = findUniqueLeaf(treeA, treeB)
    assert(reply == Delta(-1, -1, -1, 1024, 1024))
  }
  //B**
  test("nextDelta is invoked at case B**") {
    val treeA: EbTree[Int] = new EbTree[Int]
    List(1000L, 1001L, 1025L).foreach(x => treeA.put(x, 1))
    val treeB: EbTree[Int] = new EbTree[Int]
    List(1000L, 1001L, 1024L, 1026L, 1800L).foreach(x => treeB.put(x, 1))


    val reply = findUniqueLeaf(treeA, treeB)
    assert(reply == Delta(-1, -1, -1, 1025, 1025))
  }
  //B***
  test("nextDelta is invoked at case B***") {
    val treeA: EbTree[Int] = new EbTree[Int]
    List(1000L, 1001L, 1025L).foreach(x => treeA.put(x, 1))
    val treeB: EbTree[Int] = new EbTree[Int]
    List(1000L, 1001L, 1025L, 1026L, 1800L).foreach(x => treeB.put(x, 1))

    val reply = findUniqueLeaf(treeA, treeB)
    assert(reply == Delta(-1, -1, -1, 1026, 1026))
  }

  def findUniqueLeaf(treeA: EbTree[Int], treeB: EbTree[Int]): Delta = {
    var reply: Delta = startDelta
    val loop = new Breaks;
    loop.breakable {
      while (true) {
        if (reply.isLeaf()) {
          reply = treeA.checkIfLostLeafIsFound(reply)
          loop.break()
        }
        else {
          reply = treeA.nextDelta(reply)
        }
        if (reply.isLeaf()) {
          reply = treeB.checkIfLostLeafIsFound(reply)
          loop.break()
        } else {
          reply = treeB.nextDelta(reply)
        }
      }
    }
    reply
  }
}
