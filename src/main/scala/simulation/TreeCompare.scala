package simulation

import src.main.scala.model.EbTree
import model.{GetChangeIdTreeRequest, GetuIdTreeRequest, Tree, EbTreeDataObject}
import akka.actor.{ActorRef,ActorSystem}

import scala.concurrent.{Future,Await}
import scala.util.{Failure, Success}
import akka.pattern.ask
import scala.concurrent.duration._

import akka.util.Timeout
import org.slf4j.LoggerFactory

/**
 * Created by prototyp on 13.07.14.
 */
//TODO make generic
class TreeCompare[T](system:ActorSystem) {
  var firstDBActor:ActorRef = _
  var secondDBActor:ActorRef = _
  var firstDB:(EbTree[T],EbTree[T])   = _
  var secondDB:(EbTree[T],EbTree[T])  = _
  val log = LoggerFactory.getLogger(classOf[TreeCompare[T]])

  def this (firstDBActorInc:ActorRef, secondDBActorInc:ActorRef,system:ActorSystem){
    this(system)
    firstDBActor = firstDBActorInc
    secondDBActor = secondDBActorInc
    getTrees()
  }
  def getTrees(){
    firstDB = getTreesOfCell(firstDBActor)
    secondDB = getTreesOfCell(secondDBActor)
    //log.info("TreeSize" +firstDB._1.size() +"/" + secondDB._1.size())
  }

  def printTreeItems(){
    val a:List[Long] = printTreeItems(firstDB._2,firstDB._2.firstKey(),List())
    val b:List[Long] = printTreeItems(secondDB._2,secondDB._2.firstKey(),List())
//    log.info("TreeA: "+ a)
//    log.info("TreeB: "+ b)
//    log.info("DIFF A B: "+a.filterNot(b.contains))
  }
  def printTreeItems(tree:EbTree[T],key:Long,items:List[Long]):List[Long] = key match{
    case 0 => items
    case x =>  printTreeItems(tree,tree.next(key),x :: items)
  }

//  def check(x:List[Long]){
//    getTrees()
//    var found = 0
//    var noFound = 0
//    x.foreach( id =>{
//      val data  = firstDB._1.get(id)
//      if(data != None){
//        found += 1
//      }else{noFound +=1}
//    })
//    log.info("found "+found)
//    log.info("no found "+noFound)
//  }

  def getTreeDiff():(Int,Int) ={
    getTrees()
    //compare as lists
    val lostInserts = compareLeaf(firstDB._1,secondDB._1)// get number off lost inserts
    val lostUpdates = compareLeaf(firstDB._2,secondDB._2)// get number off lost updates
    (lostInserts,lostUpdates)
  }
  def getTreeDiffInPercent():Double = {

    val diff = getTreeDiff()
    val maxDiff = firstDB._2.size() + secondDB._2.size()
    val onePerc = 100.0/maxDiff
    diff._2.toDouble * onePerc
  }

  def compareLeaf(treeA:EbTree[T], treeB:EbTree[T]):Int = {
    var diff = 0
    var keyA = treeA.firstKey()
    var keyB = treeB.firstKey()
    if(keyA== -1 || keyB== -1){
      log.error("[TreeCompare] Compare but trees empty")
      EventLogging.addEvent("[TreeCompare] Compare but trees empty")
      Thread.sleep(100)
      keyA = treeA.firstKey()
      keyB = treeB.firstKey()
    }

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
  def getTreesOfCell(treeActor:ActorRef):(EbTree[T],EbTree[T]) = {
    var receivedAnswerUiDTree:Boolean = false
    var receivedAnswerCiDTree:Boolean = false
    var uIdTreeReceived:EbTree[T] = null
    var cIdTreeReceived:EbTree[T] = null
    implicit val timeout = Timeout(60.second)
    implicit val ec = system.dispatcher

    val uIdTreeAnswer:Future[Any] = treeActor ? GetuIdTreeRequest
    val answerUID = Await.result(uIdTreeAnswer,timeout.duration)
    answerUID match {
      case Tree(uIdTree:EbTree[Any]) => uIdTreeReceived = uIdTree.asInstanceOf[EbTree[T]]
      case _ =>  log.error("[Failure]!Get uIdTree failed!")
    }

    val cIdTreeAnswer:Future[Any] = treeActor ? GetChangeIdTreeRequest
    val answerCID = Await.result(cIdTreeAnswer,timeout.duration)
    answerCID match {
      case Tree(cIdTree:EbTree[Any]) => cIdTreeReceived = cIdTree.asInstanceOf[EbTree[T]]
      case _ =>  log.error("[Failure]!Get ChangeIdTree failed!")
    }

    //log.info("trees received")
    (uIdTreeReceived,cIdTreeReceived)
  }
}
