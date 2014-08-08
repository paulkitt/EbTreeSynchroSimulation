package simulation

import akka.actor.{ActorRef, Props, ActorSystem}
import model._

import scala.collection.immutable.HashMap
import model.SetTreeActors
import model.InsertNewObject
import akka.event.Logging
import com.typesafe.config.ConfigFactory
import scala.concurrent.Future
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure,Success}
import src.main.scala.model.EbTree
import akka.util.Timeout
import akka.pattern.ask

case object Clock
object Constant { final val CLOCK_SPEED = 1000
                  final val STEP_CLOCK_KEY_WISE = false}

/**
 * Created by prototyp on 19.06.14.
 */
object SimulationMaster extends App{

  //init the Simulator

  val system = ActorSystem("EBTreeSimulation", ConfigFactory.load)
  val log = Logging.getLogger(system,this)
  val communication = system.actorOf(Props[CommunikationLayer[Int]],"communication")
  val actorList = List(system.actorOf(Props(new EbTreeDatabase[Int](communication)),"nodeA"),
                       system.actorOf(Props(new EbTreeDatabase[Int](communication)),"nodeB"))
  val accessLayer:AccessLayer[Int] = new AccessLayer[Int](communication,actorList)
  // sends to each tree managing actor the references of the others that they can add routing information
  actorList.foreach(f=> f ! SetTreeActors(actorList))
  // send all tree actor references to the communication layer
  communication ! SetTreeActors(actorList)
  log.info("Done init start Simulation")


  // basic test: send some values
  val testValues = List(10,11,12,13)
  testValues.foreach(x => accessLayer.insertNewObject(x))
  //TODO save inserted uid
  //TODO generated change sequence (e.g.in range 1 to 100 changeRate(length))
  //this.wait(1000)


  sendClock()
  sendClock()

  //val IDTreesOfnodeA = getTreesOfCell(actorList.head)
  //val IDTreesOfnodeB = getTreesOfCell(actorList.tail.head)
  //diffTrees()
  actorList(0) ! StartSynchronisation
  actorList(1) ! StartSynchronisation
  //shutDownAllActors()




  def sendClock() {
    if(Constant.STEP_CLOCK_KEY_WISE){
      log.info("Waiting for key to send Clock")
      readLine()
      //TODO read int steps
      log.info("Got key! Send Clock")
    }else{
      Thread.sleep(Constant.CLOCK_SPEED)
    }
    communication ! Clock
  }
  def diffTrees(){
    val treeComp = new TreeCompare[EbTreeDataObject[Int]](getTreesOfCell(actorList.head),getTreesOfCell(actorList.tail.head))
    treeComp.compareTrees
  }
 //TODO change type to T
  def getTreesOfCell(treeActor:ActorRef):(EbTree[EbTreeDataObject[Int]],EbTree[EbTreeDataObject[Int]]) = {
    var receivedAnswerUiDTree:Boolean = false
    var receivedAnswerCiDTree:Boolean = false
    var uIdTreeReceived:EbTree[EbTreeDataObject[Int]] = null
    var cIdTreeReceived:EbTree[EbTreeDataObject[Int]] = null
    implicit val timeout = Timeout(2.second)
    implicit val ec = system.dispatcher

    val uIdTreeAnswer:Future[Any] = treeActor ? GetuIdTreeRequest
    uIdTreeAnswer.onComplete{
      case Success(Tree(uIdTree:EbTree[Any])) => receivedAnswerUiDTree=true;uIdTreeReceived = uIdTree.asInstanceOf[EbTree[EbTreeDataObject[Int]]]
      case Failure(_)                         => receivedAnswerCiDTree=true; log.error("[Failure]!Get uIdTree failed!")
      case _                                  => receivedAnswerCiDTree=true; log.error("[_]StrangeAnswer! Get ChangeIdTree failed!")
    }
    val cIdTreeAnswer:Future[Any] = treeActor ? GetChangeIdTreeRequest
    cIdTreeAnswer.onComplete{
      case Success(Tree(cIdTree:EbTree[Any])) => receivedAnswerCiDTree=true; cIdTreeReceived = cIdTree.asInstanceOf[EbTree[EbTreeDataObject[Int]]]
      case Failure(_)                         => receivedAnswerCiDTree=true; log.error("[Failure]!Get ChangeIdTree failed!")
      case _                                  => receivedAnswerCiDTree=true; log.error("[_]StrangeAnswer! Get ChangeIdTree failed!")
    }
    while(!receivedAnswerUiDTree || !receivedAnswerCiDTree){println("Waiting!")}
    log.info("trees received")
    (uIdTreeReceived,cIdTreeReceived)
  }

  def shutDownAllActors(){
    communication!ShutDownActorRequest
    actorList.foreach(x => x !ShutDownActorRequest)
  }
}
//class SimulationMaster
//Questions:
// 1. Bilden der uID/ChangeID in Access Layer
// 2. Bilden NodeIds EBTree.122
// 3. adress node bitwise

//---------------------------------------------------------------------------------------
//TODO select logging pattern
//TODO implement tree id xor

//TODO
//TODO implement tree diff: logical and graphical

//TODO implement better simulation master(with key stop/start)

//TODO actor failure handling
//TODO ask for newest version of ebTree


//TODO write tests , akka-testkit
//TODO write paper implementation
//TODO write doku

// implement clock actor
