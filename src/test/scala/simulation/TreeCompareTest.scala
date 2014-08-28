package simulation

import org.scalatest.FunSuite
import src.main.scala.model.EbTree


/**
 * Created by prototyp on 31.07.14.
 */
class TreeCompareTest extends FunSuite {

  test("compareLeaf is invoked on two EbTrees which differ"){
    val treeA:EbTree[Int] = new EbTree[Int]
    List(3,4,5,6,45,48,700,1312).foreach(x => treeA.put(x,1))
    val treeB = new EbTree[Int]
    List(3,5,6,7,9,45,46,700,1000).foreach(x => treeB.put(x,1))
    val treeComp:TreeCompare[Int] = new TreeCompare[Int](null)
    treeComp.firstDB    = (treeA,treeA)
    treeComp.secondDB = (treeB,treeB)
    assert(treeComp.compareLeaf(treeA,treeB)==7)
  }
  test("compareLeaf is invoked on two equal EbTrees"){
    val treeA:EbTree[Int] = new EbTree[Int]
    List(3,4,5,6,45,48,700,1312).foreach(x => treeA.put(x,1))
    val treeComp:TreeCompare[Int] = new TreeCompare[Int](null)
    treeComp.firstDB    = (treeA,treeA)
    treeComp.secondDB = (treeA,treeA)

    assert(treeComp.compareLeaf(treeA,treeA)==0)
  }
  test("compareLeaf is invoked on two EbTrees with first longer"){
    val treeA:EbTree[Int] = new EbTree[Int]
    List(3,4,5,6,45,48,700,1312,5000,6000,7000).foreach(x => treeA.put(x,1))
    val treeB = new EbTree[Int]
    List(3,5,6,7,9,45,46,700,1000).foreach(x => treeB.put(x,1))
    val treeComp:TreeCompare[Int] = new TreeCompare[Int](null)
    treeComp.firstDB    = (treeA,treeA)
    treeComp.secondDB = (treeB,treeB)

    assert(treeComp.compareLeaf(treeA,treeB)==10)
  }
  test("compareLeaf is invoked on two EbTrees with second longer"){
    val treeA:EbTree[Int] = new EbTree[Int]
    List(3,4,5,6,45,48,700,1312).foreach(x => treeA.put(x,1))
    val treeB = new EbTree[Int]
    List(3,5,6,7,9,45,46,700,1000,5000,6000,7000).foreach(x => treeB.put(x,1))
    val treeComp:TreeCompare[Int] = new TreeCompare[Int](null)
    treeComp.firstDB    = (treeA,treeA)
    treeComp.secondDB = (treeB,treeB)

    assert(treeComp.compareLeaf(treeA,treeB)==10)
  }

}
