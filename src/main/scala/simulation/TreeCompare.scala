package simulation

import src.main.scala.model.EbTree
import model.EbTreeDataObject

/**
 * Created by prototyp on 13.07.14.
 */
//TODO make generic
class TreeCompare[T](firstTree:(EbTree[T],EbTree[T]),secondTree:(EbTree[T],EbTree[T])) {

  def paintTrees() = ???

  def compareStructures() = ???

  def compareTrees():(Int,Int) ={
    //compare as lists
    val lostInserts = compareLeaf(firstTree._1,secondTree._1)// get number off lost inserts
    val lostUpdates = compareLeaf(firstTree._2,secondTree._2)// get number off lost updates
    (lostInserts,lostUpdates)
  }

  def compareLeaf(treeA:EbTree[T], treeB:EbTree[T]):Int = {
    var diff = 0
    var keyA = treeA.firstKey()
    var keyB = treeB.firstKey()
    while(keyA!=0 || keyB!=0){
      if(keyA==0){
        diff += 1
        keyB = treeB.next(keyB)
      }else if(keyB==0){
        diff += 1
        keyA = treeA.next(keyA)
      }
      if(keyA!=0 && keyB!=0){
        if(keyA < keyB){
          diff += 1
          keyA = treeA.next(keyA)
        }else if(keyB < keyA){
          diff += 1
          keyB = treeB.next(keyB)
        }else if(keyA == keyB){
          keyA = treeA.next(keyA)
          keyB = treeB.next(keyB)
        }
      }
    }
    diff
  }
}
