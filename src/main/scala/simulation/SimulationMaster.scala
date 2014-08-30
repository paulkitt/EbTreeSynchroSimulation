package simulation

import akka.actor.{ActorRef, Props, ActorSystem}
import model._
import akka.event.Logging
import com.typesafe.config.ConfigFactory
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.util.control._

import scala.concurrent.duration._
import scala.util.{Failure, Success}
import src.main.scala.model.{Delta, EbTree}
import model.SetTreeActors
import scala.util.{Failure, Success}
import akka.pattern.ask
import scala.concurrent.duration._
import akka.util.Timeout


case object Clock

object Constant {
  final val CLOCK_SPEED = 1000
  final val STEP_CLOCK_KEY_WISE = false
}

/**
 * Created by prototyp on 19.06.14.
 */
object SimulationMaster extends App {

  //init the Simulator

  val system = ActorSystem("EBTreeSimulation", ConfigFactory.load)
  val log = Logging.getLogger(system, this)
  val communication = system.actorOf(Props(new CommunikationLayer(0)), "communication")
  val actorList = List(system.actorOf(Props(new EbTreeDatabase[Int](communication)), "nodeA"),
    system.actorOf(Props(new EbTreeDatabase[Int](communication)), "nodeB"))
  val accessLayer: AccessLayer[Int] = new AccessLayer[Int](communication, actorList)
  //sends to each tree managing actor the references of the others that they can add routing information
  actorList.foreach(f => f ! SetTreeActors(actorList))
  // send all tree actor references to the communication layer
  communication ! SetTreeActors(actorList)
  log.info("Done init start Simulation")

  println("[SimulationMaster] select Simulation: \n" +
    "(1): Paketloss Simulation - Insert and Updates a lost while sending\n" +
    "(2): Random Item Simulation - Both trees are total different\n" +
    "(3): Error")
  readLine().toInt match {
    case 1 => {
      communication ! SetLoss(50)
      val testValues = 1.to(3000) //List(10,11,12,13,1,1,1,1,23,4,5,6,7,8)
      testValues.foreach(x => accessLayer.insertNewObject(x))
    }
    case 2 => {
      println("[SimulationMaster] => Random Item Simulation! Number of Elements:")
      val numItems = readLine().toInt
      val aLay1 = new AccessLayer[Int](communication, List(actorList(0)))
      val aLay2 = new AccessLayer[Int](communication, List(actorList(1)))
      val values: List[Int] = 1.to(numItems).toList
      values.foreach(x => {
        aLay1.insertNewObject(x);
        aLay2.insertNewObject(x)
      })
      log.info("[SimulationMaster] Generation of Data complete!")
    }
    case 3 => {
      val a: List[Long] = List(92332172247042L, 92332172247313L, 92332172247574L, 92332172247846L, 92332172248113L, 92332172248380L, 92332172248644L, 92332172248905L, 92332172249171L, 92332172249435L, 92332172249705L, 92332172249976L, 92332172250242L, 92332172250504L, 92332172250770L, 92332172251036L, 92332172251301L, 92332172251565L, 92332172251838L, 92332172252099L, 92332172252368L, 92332172252630L, 92332172252892L, 92332172253162L, 92332172253433L, 92332172253690L, 92332172312577L, 92332172312849L, 92332172313109L, 92332172313380L, 92332172313643L, 92332172313907L, 92332172314175L, 92332172314438L, 92332172314706L, 92332172314972L, 92332172315241L, 92332172315506L, 92332172315773L, 92332172316041L, 92332172316310L, 92332172316575L, 92332172316837L, 92332172317103L, 92332172382906L, 92332172383170L, 92332172383441L, 92332172383705L, 92332172383966L, 92332172384238L, 92332172384504L, 92332172384764L, 184664344507652L, 184664344508179L, 184664344508698L, 184664344509218L, 184664344509740L, 184664344510266L, 184664344510792L, 184664344511308L, 184664344511829L, 184664344512351L, 184664344512872L, 184664344513402L, 184664344513923L, 184664344514445L)
      val b: List[Long] = List(92332172247042L, 92332172247313L, 92332172247574L, 92332172247846L, 92332172249171L, 92332172249435L, 92332172249705L, 92332172249976L, 92332172250242L, 92332172250504L, 92332172250770L, 92332172251036L, 92332172251301L, 92332172251565L, 92332172251838L, 92332172252099L, 92332172252368L, 92332172252630L, 92332172252892L, 92332172253162L, 92332172253433L, 92332172253690L, 92332172312577L, 92332172312849L, 92332172313109L, 92332172313380L, 92332172313643L, 92332172313907L, 92332172314175L, 92332172314438L, 92332172314706L, 92332172314972L, 92332172315241L, 92332172315506L, 92332172315773L, 92332172316041L, 92332172316310L, 92332172316575L, 92332172316837L, 92332172317103L, 92332172382906L, 92332172383170L, 92332172383441L, 92332172383705L, 92332172383966L, 92332172384238L, 92332172384504L, 92332172384764L, 184664344769806L, 184664344770326L, 184664344770840L, 184664344771367L, 184664344771884L, 184664344772408L, 184664344772937L, 184664344773458L, 184664344773982L, 184664344774501L, 184664344775020L, 184664344775538L, 184664344776060L, 184664344776587L)
      a.foreach(x => actorList(0) ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), Some(communication), actorList(0)))
      b.foreach(x => actorList(1) ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), Some(communication), actorList(1)))
    }
  }




  //TODO save inserted uid
  //TODO generated change sequence (e.g.in range 1 to 100 changeRate(length))



  sendClock()
  Thread.sleep(2000)
  actorList.foreach(x => x ! PaintTree)
  val comp: TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](actorList(0), actorList(1), system)
  var diff: (Int, Int) = comp.compareTrees()
  var oldDiff: (Int, Int) = (0, 0)
  var run: Boolean = true

  while (run) {
    log.info("[SimulationMaster] Diff: " + diff)
    log.info("[SimulationMaster] paused! Enter number of steps or (q)uit: ")
    val command = readLine()
    if (command == "q") {
      run = false
    } else {
      var steps: Int = command.toInt
      val innerloop = new Breaks;
      innerloop.breakable {
        implicit val timeout = Timeout(2.second)
        implicit val ec = system.dispatcher
        while (steps > 0) {
          steps -= 1
          val synchroResponce: Future[Any] = communication ? StartSynchro(Some(actorList(1)), actorList(0))
          val answer = Await.result(synchroResponce, timeout.duration)
          answer.toString match {
            case "SynchroFinished" => {
              if (diff != oldDiff) {
                actorList.foreach(x => x ! PaintTree)
                oldDiff == diff
                diff = comp.compareTrees()
                log.info("[SimulationMaster] old Tree diff: " + oldDiff)
                log.info("[SimulationMaster] Tree diff: " + diff)
              } else if (oldDiff == diff && diff !=(0, 0)) {
                comp.printTreeItems()
                log.info("[SimulationMaster] Tree sync Stuck!")
                innerloop.break()
              } else if (diff ==(0, 0)) {
                log.info("[SimulationMaster] Tree sync complete!")
                innerloop.break()
              }
            }
            case _ =>
              log.error("[SimulationMaster] synchroResponce Failure!")
              innerloop.break()
          }
        }
      }
      


      //              val innerloop = new Breaks;
      //              innerloop.breakable {
      //                while(steps>0){
      //                  steps -= 1
      //                  communication ! StartSynchro(Some(actorList(1)),actorList(0))
      //                  Thread.sleep(100) //
      //                  actorList.foreach(x => x ! PaintTree)
      //                  oldDiff = diff
      //                  diff = comp.compareTrees()
      //                  log.info("[SimulationMaster] old Tree diff: "+oldDiff)
      //                  log.info("[SimulationMaster] Tree diff: "+diff)
      //                  if(oldDiff==diff){
      //                    comp.printTreeItems()
      //                    log.info("[SimulationMaster] Tree sync Stuck!")
      //                    innerloop.break()
      //                  }
      //                }
      //              }
    }
  }


  //  sendClock()
  //
  //  //val IDTreesOfnodeA = getTreesOfCell(actorList.head)
  //  //val IDTreesOfnodeB = getTreesOfCell(actorList.tail.head)
  //  //diffTrees()
  //  //shutDownAllActors()
  //
  def sendClock() {
    if (Constant.STEP_CLOCK_KEY_WISE) {
      log.info("Waiting for key to send Clock")
      readLine()
      //TODO read int steps
      log.info("Got key! Send Clock")
    } else {
      Thread.sleep(Constant.CLOCK_SPEED)
    }
    communication ! Clock
  }

  //  def diffTrees(){
  //    val treeComp = new TreeCompare[EbTreeDataObject[Int]](actorList.head,actorList.tail.head,system)
  //    treeComp.compareTrees
  //  }
  //
  //  def shutDownAllActors(){
  //    communication!ShutDownActorRequest
  //    actorList.foreach(x => x !ShutDownActorRequest)
  //  }
}

//class SimulationMaster
//Questions:
// 1. Bilden der uID/ChangeID in Access Layer
// 2. Bilden NodeIds EBTree.122
// 3. adress node bitwise

//---------------------------------------------------------------------------------------

//TODO
// implement delete

//TODO implement better simulation master(with key stop/start)

//TODO actor failure handling


//TODO write paper implementation
//TODO write doku

