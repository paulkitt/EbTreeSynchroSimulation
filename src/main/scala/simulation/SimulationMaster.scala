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
import java.io._
import java.util.Date
import java.text.SimpleDateFormat

import scala.collection.mutable.Map
case object Clock

object Constant {
  final val CLOCK_SPEED = 1000
  final val STEP_CLOCK_KEY_WISE = false
  final val GRAPHIC_ACTIVE = true
  final val LOG = false
  final val SYNCHRO_LOG = false
  var SYNCHRO_EVENT_LOG = false
  val SYNCHRO_EVENTS = List("[Synchro] SynchroPKT","[Synchro] Go left","[Synchro] Go right","[Synchro] Flip Sides","[Synchro] BitLvlDiff LeftNode equal","[Synchro] BitLvlDiff")
}

/**
 * Created by prototyp on 19.06.14.
 */
object SimulationMaster extends App {

  //init the Simulator

  val system = ActorSystem("EBTreeSimulation", ConfigFactory.load)
  val log = Logging.getLogger(system, this)
  val communication = system.actorOf(Props(new CommunicationLayer(0, 0)), "communication")
  var actorList = List( system.actorOf(Props(new EbTreeDatabase[Int](communication)), "nodeA"),
                        system.actorOf(Props(new EbTreeDatabase[Int](communication)), "nodeB"))
  val accessLayer: AccessLayer[Int] = new AccessLayer[Int](communication, actorList, 64)
  //sends to each tree managing actor the references of the others that they can add routing information
  actorList.foreach(f => f ! SetTreeActors(actorList))
  // send all tree actor references to the communication layer
  //communication ! SetTreeActors(actorList)
  log.info("Done init start Simulation")

  log.info("[SimulationMaster] select Simulation: \n" +
    "(1): Paketloss Simulation - Insert and Updates a lost while sending\n" +
    "(2): Random Item Simulation - Both trees are total different\n" +
    "(3): Test Diff KeyBitLength")
  readLine().toInt match {
    case 1 => {
      communication ! SetLoss(50)
      val testValues = 1.to(3000) //List(10,11,12,13,1,1,1,1,23,4,5,6,7,8)
      testValues.foreach(x => accessLayer.insertNewObject(x))
      sendClock()
      Thread.sleep(100)
      actorList.foreach(x => x ! PaintTree)
      log.info(syncTrees(-1).toString())
    }
    case 2 => {
      log.info("[SimulationMaster] => Random Item Simulation! Number of Elements:")
      val numItems = readLine().toInt
      log.info("[SimulationMaster] => Random Item Simulation! KeyLength(48/56/64):")
      val keyLen = readLine().toInt
      val aLay1 = new AccessLayer[Int](communication, List(actorList(0)), keyLen)
      val aLay2 = new AccessLayer[Int](communication, List(actorList(1)), keyLen)
      val values: List[Int] = 1.to(numItems).toList
      values.foreach(x => {
        aLay1.insertNewObject(x);
        aLay2.insertNewObject(x)
      })
      log.info("[SimulationMaster] Generation of Data complete!")
      sendClock()
      actorList.foreach(x => x ! PaintTree)
      Thread.sleep(1000)
      log.info("[SimulationMaster]" + syncTrees(-1))
      EventLogging.events.foreach(x => log.info(x.toString()))
    }
    case 3 => {
      log.info("[SimulationMaster] => Test Diff KeyBitLength!")
      //val elemNum = List(100,500,1000,2000,4000,6000,8000,10000)
      val elemNum = List(100,500,1000,2000,5000,10000)
      //val elemNum = List(10000)
      val keys = List( 48, 56, 64)
      val repetition = 100.0
      var result: List[String] = List()
      val fileName = "./simulationOutput/"+new SimpleDateFormat("dd.MM.yyyy_hh:mm:ss").format(new Date())+"TestKeyBitLength.csv"
      val file = new File(fileName)
      file.createNewFile()
      val print:PrintWriter = new PrintWriter(new FileWriter(file.getAbsolutePath))
      var heading:String = "keyLen;"
      elemNum.foreach(x => heading = heading + x +";" )
      print.println(heading)
      print.flush()
      keys.foreach(keyLen => {
        log.info("[SimulationMaster] Running at Key: "+keyLen)
        var resByKey: String = keyLen+";"
        var unSuccess:Double = 0
        elemNum.foreach(elems => {
          if(unSuccess!=repetition){
            log.info("[SimulationMaster] Running at ElemNumbers: "+elems)
            var results: List[Int] = List()
            //repetition.foreach(rep => {
            var i = repetition
            while(i>0){
              log.info("[SimulationMaster] Repetition: "+i)
              actorList.foreach(x => x ! ClearTree)
              Thread.sleep(100)
              val aLay1 = new AccessLayer[Int](communication, List(actorList(0)), keyLen)
              val aLay2 = new AccessLayer[Int](communication, List(actorList(1)), keyLen)
              val values: List[Int] = 1.to(elems).toList
              values.foreach(x => aLay1.insertNewObject(x))
              sendClock()
              Thread.sleep(300)
              values.foreach(x => aLay2.insertNewObject(x))
              sendClock()
              Thread.sleep(100)
              val response = syncTrees(-1)
              if((response._1,response._2) ==(false,(0,0)))results = 1 :: results
              else results = 0 :: results
              i -= 1
            }
            unSuccess = results.filter(x => x==0).length
          }
          resByKey = resByKey + ((unSuccess/repetition)*100)+";"
          log.info("[SimulationMaster] Elems Fail Perc:" + resByKey + " unsuccess: "+unSuccess + " results: "+resByKey)
        })
        print.println(resByKey)
        print.flush()
        result = result ::: List(resByKey)
        log.info("[SimulationMaster] Result for Key " +keyLen+": " + result)
      })
      log.info("[SimulationMaster] DONE"+result)
      log.info("[SimulationMaster] Events:")
      EventLogging.events.foreach(x => log.info(x.toString()))
    }
    case 4 => {
      log.info("[SimulationMaster] => Test Measure Traffic!")
      //val elemNum = List(100,500,1000,2000,4000,6000,8000,10000)
      Constant.SYNCHRO_EVENT_LOG = true
      val elemNum = 2000
      val databaseDifference = List(1,10,50,100)
      val repetition = 4
      var result: List[String] = List()
      val fileName = "./simulationOutput/"+new SimpleDateFormat("dd.MM.yyyy_hh:mm:ss").format(new Date())+"TestMeasureTraffic.csv"
      val file = new File(fileName)
      file.createNewFile()
      val print:PrintWriter = new PrintWriter(new FileWriter(file.getAbsolutePath))
      databaseDifference.foreach(diffDB => {
        var i = repetition
        while(i>0){

          val diff = (elemNum/100)*diffDB // number of diff elements
          log.info("[SimulationMaster] Repetition: "+i)
          actorList.foreach(x => x ! ClearTree)
          1.to(elemNum - diff).toList.foreach(data => accessLayer.insertNewObject(data))
          sendClock()

          val aLay1 = new AccessLayer[Int](communication, List(actorList(0)), 64)
          1.to(diff/2).toList.foreach(diffData => aLay1.insertNewObject(diffData))
          sendClock()

          val aLay2 = new AccessLayer[Int](communication, List(actorList(1)), 64)
          1.to(diff/2).toList.foreach(diffData => aLay2.insertNewObject(diffData))
          sendClock()
          Thread.sleep(100)
          var syncResponce:(Boolean,(Int, Int),Int) = syncTrees(1)
          log.info("Diff: "+syncResponce)
          var syncSteps = 1
          while(syncResponce._1!=true && syncResponce._2!=(0,0)){
            syncResponce = syncTrees(1)
            syncSteps += 1
          }
          log.info("[SimulationMaster] Total num of Deltas send: "+EventLogging.getEvent("[Synchro] SynchroPKT").get+" syncSteps: "+syncSteps)
          log.info("[SimulationMaster] Avarge num of Deltas send: "+EventLogging.delEvent("[Synchro] SynchroPKT").get.toDouble/syncSteps.toDouble)
          log.info("[SimulationMaster] Total num of [Synchro] Go left send: "+EventLogging.delEvent("[Synchro] Go left").getOrElse(0).toDouble/syncSteps.toDouble)
          log.info("[SimulationMaster] Total num of [Synchro] Go right send: "+EventLogging.delEvent("[Synchro] Go right").getOrElse(0).toDouble/syncSteps.toDouble)
          log.info("[SimulationMaster] Total num of [Synchro] Flip Sides send: "+EventLogging.delEvent("[Synchro] Flip Sides").getOrElse(0).toDouble/syncSteps.toDouble)
          log.info("[SimulationMaster] Total num of [Synchro] BitLvlDiff LeftNode equal send: "+EventLogging.delEvent("[Synchro] BitLvlDiff LeftNode equal").getOrElse(0).toDouble/syncSteps.toDouble)
          log.info("[SimulationMaster] Total num of [Synchro] BitLvlDiff send: "+EventLogging.delEvent("[Synchro] BitLvlDiff").getOrElse(0).toDouble/syncSteps.toDouble)
          i -= 1
          log.info("[SimulationMaster] Events:")
          EventLogging.events.foreach(x => log.info(x.toString()))
          EventLogging.events = scala.collection.mutable.Map()
        }
      })


    }
    case 5 => {
      log.info("[SimulationMaster] Vergleich identischer Datenbanken")
      val elemNum = List(100,500,1000,2000,5000,10000)
      val repetition = 10
      Constant.SYNCHRO_EVENT_LOG = true
      var result:List[Int] = List()
      elemNum.foreach(num => {
        log.info("Teste mit " + num + " Datensaetzen!")
        1.to(repetition).toList.foreach(x => {
          log.info("Wiederholung: "+x)
          actorList.foreach(x => x ! ClearTree)
          1.to(num).foreach(data => accessLayer.insertNewObject(data))
          sendClock()
          (syncTrees(1) == (false,(0,0),0)&& EventLogging.delEvent("[Synchro] DeltaPKT").getOrElse(0)==2) match {
            case true => 1 :: result ; log.info("Durchgang erfolgreich!")
            case    _ => 0 :: result ; log.info("Durchgang fehlgeschlagen!")
          }
        })
      })
      result.filter(x => x==0).length match {
        case 0 => log.info("Test erfolgreich!")
        case _ => log.info("Test fehlgeschlagen!")
      }
    }
    case 6 => {
      log.info("[SimulationMaster] Synchronisation einer leeren Datenbankreplikation")
      val elemNum = List(100,500,1000,2000,5000,10000)
      val repetition = 10
      Constant.SYNCHRO_EVENT_LOG = true
      var result:List[String] = List()
      val fileName = "./simulationOutput/"+new SimpleDateFormat("dd.MM.yyyy_hh:mm:ss a zzz").format(new Date())+"TestEmptyDBSync.csv"
      val file = new File(fileName)
      file.createNewFile()
      val print:PrintWriter = new PrintWriter(new FileWriter(file.getAbsolutePath))
      print.println("DataAmount;SyncPkt;GoLeft;GoRight;FlipSides;BitLvlDiffLeftNodeEqual;BitLvlDiff")
      elemNum.foreach(num => {
        log.info("Teste mit " + num + " Datensaetzen!")
        var result:Map[String,Double] = Map()
        def addEvent(event:String,amount:Double) = result.remove(event) match{
          case Some(x:Double)   => result += (event -> (x+amount))
          case None             => result += (event -> amount)
        }
        1.to(repetition).toList.foreach(x => {
          var response:(Boolean,(Int, Int),Int) = (false,(0,0),0)
          do{
          log.info("Wiederholung: "+x)
          EventLogging.events =  Map()
          actorList.foreach(x => x ! ClearTree)
          val aLay1 = new AccessLayer[Int](communication, List(actorList(0)), 64)
          1.to(num).foreach(data => aLay1.insertNewObject(data))
          sendClock()
            response = syncTrees(-1)
          }while(response._1==true)
          Constant.SYNCHRO_EVENTS.foreach(event => addEvent(event,{val eventNum:Double = EventLogging.getEvent(event).getOrElse(0).toDouble; if(eventNum==0) 0 else eventNum/response._3}))

        })
        var processedResult:String = num + ";"
        println("Response: " + result)
        Constant.SYNCHRO_EVENTS.foreach(x =>processedResult += BigDecimal(result.remove(x).getOrElse(1).asInstanceOf[Double]/repetition).setScale(2,BigDecimal.RoundingMode.HALF_EVEN)+";" )
        println("Response: " + processedResult)
        print.println(processedResult)
        })
      print.flush()
    }
  }
  def syncTrees(stepsInut: Int): (Boolean,(Int, Int),Int) = {
    val comp: TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](actorList(0), actorList(1), system)
    var diff: (Int, Int) = (0, 0)
    var cycles:Int = 0
    var steps: Int = stepsInut
    if (stepsInut == -1) steps = 1
    //def isStuck():Boolean = (oldDiff == diff && diff !=(0, 0))
    var stuck = false
    val innerloop = new Breaks;
    innerloop.breakable {
      implicit val timeout = Timeout(60.second)
      implicit val ec = system.dispatcher
      while (steps > 0) {
        steps -= 1
        actorList.foreach(x => x ! PaintTree)
        if(stepsInut== -1)steps += 1
        var actors = List(0,1)
        def getRandomItem(x:Int = Random.nextInt(2)):Int = {
          actors = actors.diff(List(x))
          x
        }
        val synchroResponce: Future[Any] = communication ? StartSynchroCycle(Some(actorList(getRandomItem())), actorList(actors.head))
        val answer = Await.result(synchroResponce, timeout.duration)
        answer match {
          case SynchroCycleFinished => {
            cycles += 1
            if(stepsInut== -1)steps += 1
//            if (diff != oldDiff) {
//
//              oldDiff = diff
//              diff = comp.compareTrees()
//              //log.info("[SimulationMaster] old Tree diff: " + oldDiff)
//              //log.info("[SimulationMaster] Tree diff: " + diff)
//            } else if (isStuck()) {
//              log.info("[SimulationMaster] Tree sync Stuck! "  + diff.toString())
//              innerloop.break()
//            } else if (diff ==(0,0)) {
//
//              innerloop.break()
//            }
          }
          case SynchroFinished => {
            diff = comp.compareTrees()
            if(diff !=(0,0)){
              stuck = true
              log.info("[SimulationMaster] Tree sync Stuck! "  + diff.toString())
            }else{
              log.info("[SimulationMaster] Tree sync complete!")
            }
            innerloop.break()
          }
          case _ =>
            log.error("[SimulationMaster] synchroResponce Failure!")
            innerloop.break()
        }
      }
    }
    if(stuck && diff !=(0, 0) ){
      EventLogging.addEvent("Unfinished Synchro!")
    }
    (stuck,diff,cycles)
  }


  //TODO save inserted uid
  //TODO generated change sequence (e.g.in range 1 to 100 changeRate(length))


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

  def shutDownAllActors() {
    communication ! ShutDownActorRequest
    actorList.foreach(x => x ! ShutDownActorRequest)
  }
}

//---------------------------------------------------------------------------------------

//TODO

//TODO implement better simulation master(with key stop/start)

//TODO actor failure handling
//TODO write doku

