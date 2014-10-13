package simulation

import akka.actor.{Props, ActorSystem}
import model._
import akka.event.Logging
import com.typesafe.config.ConfigFactory
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.util.control._

import scala.concurrent.duration._

import akka.pattern.ask
import scala.concurrent.duration._
import akka.util.Timeout
import java.io._
import java.util.Date
import java.text.SimpleDateFormat

import scala.collection.mutable.Map
import scala.Some
import model.SetLoss
import model.EbTreeDataObject
import model.SetTreeActors
import model.StartSynchroCycle

case object Clock

object Constant {
  final val CLOCK_SPEED = 1000
  final val STEP_CLOCK_KEY_WISE = false
  var GRAPHIC_ACTIVE = false
  final val LOG = false
  final val SYNCHRO_LOG = false
  var SYNCHRO_EVENT_LOG = false
  val SYNCHRO_EVENTS = List("[Synchro] SynchroPKT", "[Synchro] Go left", "[Synchro] Go right", "[Synchro] Flip Sides", "[Synchro] BitLvlDiff LeftNode equal", "[Synchro] BitLvlDiff")
}

/**
 * Created by prototyp on 19.06.14.
 */
object SimulationMaster extends App {

  //init the Simulator

  val system = ActorSystem("EBTreeSimulation", ConfigFactory.load)
  val log = Logging.getLogger(system, this)
  val communication = system.actorOf(Props(new CommunicationLayer(0, 0)), "communication")
  var actorList = List(system.actorOf(Props(new EbTreeDatabase[Int](communication)), "nodeA"),
    system.actorOf(Props(new EbTreeDatabase[Int](communication)), "nodeB"))
  val accessLayer: AccessLayer[Int] = new AccessLayer[Int](communication, actorList, 64)
  var comp: TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](actorList(0), actorList(1), system)
  //sends to each tree managing actor the references of the others that they can add routing information
  actorList.foreach(f => f ! SetTreeActors(actorList))
  // send all tree actor references to the communication layer
  //communication ! SetTreeActors(actorList)
  log.info("Done init start Simulation")

  log.info("[SimulationMaster] Simulation auswaehlen: \n" +
    "(1): Paketloss Simulation - Insert and Updates a lost while sending\n" +
    "(2): Random Item Simulation - Both trees are total different\n" +
    "--------------------Statische Tests------------------------------\n" +
    "(3): Verschiedene Schlüsselgrößen und das Erzeugen der Knoten Change-IDs durch XOR!\n" +
    "(4): Vergleich identischer Datenbanken\n" +
    "(5): Synchronisation einer leeren Datenbankreplikation\n" +
    "(6): Synchronisation sich unterscheidender Datenbanken\n" +
    "--------------------Dynamische Tests------------------------------\n" +
    "(7): Bestimmen der benötigten Synchronisationsresourcen im laufenden Betrieb\n")
  val inputSim = readLine()
  log.info("[SimulationMaster] Graphische Visualisierung der Bäume aktivieren (Verlangsamt Simulation): (J)a/(N)ein ")
  val inputGraph = readLine()
  if(inputGraph.length == 1 && (inputGraph(0) == 'J' || inputGraph(0) == 'j')){
    Constant.GRAPHIC_ACTIVE = true
  }else{
    log.info("Ungueltige Eingabe! Starte Simulation ohne Graphische Visualisierung ")
  }

  if(inputSim.length == 1 && Character.isDigit(inputSim(0))){
    inputSim.toInt match {
      case 1 => {
        communication ! SetLoss(50)
        val testValues = 1.to(3000) //List(10,11,12,13,1,1,1,1,23,4,5,6,7,8)
        testValues.foreach(x => accessLayer.insertNewObject(x))
        communication ! Clock
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
//------------------------------------------------------------------------------------------------------------------------------------------------------
      // Statische Tests
      case 3 => {
        log.info("[SimulationMaster] => Verschiedene Schlüsselgrößen und das Erzeugen der Knoten Change-IDs durch XOR!")
        val elemNum = List(100, 500, 1000, 2000, 5000, 10000)
        val keys = List(48, 56, 64)
        val repetition = 100.0
        var result: List[String] = List()
        val file = new File("./simulationOutput/" + new SimpleDateFormat("dd.MM.yyyy_hh:mm:ss").format(new Date()) + "TestKeyBitLength.csv")
        file.createNewFile()
        val print: PrintWriter = new PrintWriter(new FileWriter(file.getAbsolutePath))
        var heading: String = "keyLen;"
        elemNum.foreach(x => heading = heading + x + ";")
        print.println(heading)
        print.flush()
        keys.foreach(keyLen => {
          log.info("[SimulationMaster] Running at Key: " + keyLen)
          var resByKey: String = keyLen + ";"
          var unSuccess: Double = 0
          elemNum.foreach(elems => {
            if (unSuccess != repetition) {
              log.info("[SimulationMaster] Running at ElemNumbers: " + elems)
              var results: List[Int] = List()
              //repetition.foreach(rep => {
              var i = repetition
              while (i > 0) {
                log.info("[SimulationMaster] Repetition: " + i)
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
                if ((response._1, response._2) ==(false, (0, 0))) results = 1 :: results
                else results = 0 :: results
                i -= 1
              }
              unSuccess = results.filter(x => x == 0).length
            }
            resByKey = resByKey + ((unSuccess / repetition) * 100) + ";"
            log.info("[SimulationMaster] Elems Fail Perc:" + resByKey + " unsuccess: " + unSuccess + " results: " + resByKey)
          })
          print.println(resByKey)
          print.flush()
          result = result ::: List(resByKey)
          log.info("[SimulationMaster] Result for Key " + keyLen + ": " + result)
        })
        log.info("[SimulationMaster] DONE" + result)
        log.info("[SimulationMaster] Events:")
        EventLogging.events.foreach(x => log.info(x.toString()))
      }
      case 4 => {
        log.info("[SimulationMaster] Vergleich identischer Datenbanken!")
        val elemNum = List(100, 500, 1000, 2000, 5000, 10000)
        val repetition = 10
        Constant.SYNCHRO_EVENT_LOG = true
        var result: List[Int] = List()
        elemNum.foreach(num => {
          log.info("Teste mit " + num + " Datensaetzen!")
          1.to(repetition).toList.foreach(x => {
            log.info("Wiederholung: " + x)
            actorList.foreach(x => x ! ClearTree)
            1.to(num).foreach(data => accessLayer.insertNewObject(data))
            sendClock()
            (syncTrees(1) ==(false, (0, 0), 0) && EventLogging.delEvent("[Synchro] DeltaPKT").getOrElse(0) == 2) match {
              case true => 1 :: result; log.info("Durchgang erfolgreich!")
              case _ => 0 :: result; log.info("Durchgang fehlgeschlagen!")
            }
          })
        })
        result.filter(x => x == 0).length match {
          case 0 => log.info("Test erfolgreich!")
          case _ => log.info("Test fehlgeschlagen!")
        }
      }
      case 5 => {
        log.info("[SimulationMaster] Synchronisation einer leeren Datenbankreplikation!")
        val elemNum = List(100, 500, 1000, 2000, 5000, 10000)
        val repetition = 10
        Constant.SYNCHRO_EVENT_LOG = true
        var result: List[String] = List()
        val file = new File("./simulationOutput/" + new SimpleDateFormat("dd.MM.yyyy_hh:mm:ss a zzz").format(new Date()) + "TestEmptyDBSync.csv")
        file.createNewFile()
        val printToFile: PrintWriter = new PrintWriter(new FileWriter(file.getAbsolutePath))
        val info = "DataAmount;SyncPkt;GoLeft;GoRight;FlipSides;BitLvlDiffLeftNodeEqual;BitLvlDiff"
        printToFile.println(info)
        result = result ::: List(info)
        elemNum.foreach(num => {
          log.info("Teste mit " + num + " Datensaetzen!")
          var resultByElemNum: Map[String, Double] = Map()
          def addEvent(event: String, amount: Double) = resultByElemNum.remove(event) match {
            case Some(x: Double) => resultByElemNum += (event -> (x + amount))
            case None => resultByElemNum += (event -> amount)
          }
          1.to(repetition).toList.foreach(x => {
            var response: (Boolean, (Int, Int), Int) = (false, (0, 0), 0)
            do {
              log.info("Wiederholung: " + x)
              EventLogging.events = Map()
              actorList.foreach(x => x ! ClearTree)
              val aLay1 = new AccessLayer[Int](communication, List(actorList(0)), 64)
              1.to(num).foreach(data => aLay1.insertNewObject(data))
              sendClock()
              response = syncTrees(-1)
              Thread.sleep(100)
            } while (response._1 == true)
            Constant.SYNCHRO_EVENTS.foreach(event => addEvent(event, {
              val eventNum: Double = EventLogging.getEvent(event).getOrElse(0).toDouble;
              if (eventNum == 0) 0 else eventNum / response._3
            }))
          })
          var processedResult: String = num + ";"
          Constant.SYNCHRO_EVENTS.foreach(x => processedResult += BigDecimal(resultByElemNum.remove(x).getOrElse(1.0) / repetition).setScale(2, BigDecimal.RoundingMode.HALF_EVEN) + ";")
          log.info("Ergebnis bei " + num + " Datensaetzen: " + processedResult)
          printToFile.println(processedResult)
          result = result ::: List(processedResult)
        })
        printToFile.flush()
        log.info("Endergebnis: ")
        result.foreach(log.info)
      }

      case 6 => {
        log.info("[SimulationMaster] => Synchronisation sich unterscheidender Datenbanken!")
        Constant.SYNCHRO_EVENT_LOG = true
        val elemNum = List(100, 500, 1000, 2000, 5000, 10000)
        val databaseDifference = List(1, 10, 50, 100)
        val repetition = 10
        var result: List[String] = List()
        val file = new File("./simulationOutput/" + new SimpleDateFormat("dd.MM.yyyy_hh:mm:ss").format(new Date()) + "TestMeasureTraffic.csv")
        file.createNewFile()
        val printToFile: PrintWriter = new PrintWriter(new FileWriter(file.getAbsolutePath))
        val info = "DataAmount;SyncPkt;GoLeft;GoRight;FlipSides;BitLvlDiffLeftNodeEqual;BitLvlDiff"

        elemNum.foreach(numOfData => {
          log.info("Teste mit " + numOfData + " Datensaetzen!")
          printToFile.println("AmountOfData: " + numOfData + ";")
          printToFile.println(info)
          result = result ::: List(info)
          databaseDifference.foreach(diffDB => {
            log.info("Teste mit 1/" + diffDB + " % Unterschied!")
            var resultByElemNum: Map[String, Double] = Map()
            def addEvent(event: String, amount: Double) = resultByElemNum.remove(event) match {
              case Some(x: Double) => resultByElemNum += (event -> (x + amount))
              case None => resultByElemNum += (event -> amount)
            }
            1.to(repetition).toList.foreach(x => {
              var response: (Boolean, (Int, Int), Int) = (false, (0, 0), 0)
              do {
                log.info("Wiederholung: " + x)
                EventLogging.events = Map()
                actorList.foreach(x => x ! ClearTree)
                val diff = (numOfData / 100) * diffDB // number of diff elements
                1.to(numOfData - diff).toList.foreach(data => accessLayer.insertNewObject(data))
                sendClock()

                val aLay1 = new AccessLayer[Int](communication, List(actorList(0)), 64)
                1.to(diff / 2).toList.foreach(diffData => aLay1.insertNewObject(diffData))
                sendClock()

                val aLay2 = new AccessLayer[Int](communication, List(actorList(1)), 64)
                1.to(diff / 2).toList.foreach(diffData => aLay2.insertNewObject(diffData))
                sendClock()
                //              Thread.sleep(50)
                response = syncTrees(-1)
              } while (response._1 == true)
              val bla = EventLogging.events
              Constant.SYNCHRO_EVENTS.foreach(event => addEvent(event, {
                val eventNum: Double = EventLogging.getEvent(event).getOrElse(0).toDouble;
                if (eventNum == 0 || response._3 == 0) 0 else eventNum / response._3
              }))
            })
            var processedResult: String = diffDB + ";"
            Constant.SYNCHRO_EVENTS.foreach(x => processedResult += BigDecimal(resultByElemNum.remove(x).getOrElse(1.0) / repetition).setScale(2, BigDecimal.RoundingMode.HALF_EVEN) + ";")
            log.info("Ergebnis bei " + numOfData + " Datensaetzen bei 1/" + diffDB + "Unterschied: " + processedResult)
            printToFile.println(processedResult)
            printToFile.flush()
            result = result ::: List(processedResult)
          })
        })
        printToFile.flush()
        log.info("Endergebnis: ")
        result.foreach(log.info)
      }
//------------------------------------------------------------------------------------------------------------------------------------------------------
      // Dynamische Tests
      case 7 => {
        log.info("[SimulationMaster] => Traffic Analyse im laufendem Betrieb!")
        Constant.SYNCHRO_EVENT_LOG = true
        val numOfData = 5000
        var numOfSyncPkt = -100
        val paketLossPropability = List(1,5,10)

        val numOfChangesPerCycle = 1000
        val repetition = 10 //000
        var ids: List[Long] = List()
        var diffPercent = 0.0
        Constant.SYNCHRO_EVENT_LOG = true
        val file = new File("./simulationOutput/" + new SimpleDateFormat("dd.MM.yyyy_hh:mm:ss").format(new Date()) + "dynTestMeasureTraffic.csv")
        file.createNewFile()
        val printToFile: PrintWriter = new PrintWriter(new FileWriter(file.getAbsolutePath))


        paketLossPropability.foreach(loss =>{
          numOfSyncPkt = -100
          printToFile.println("Verlust an Paketen beim Senden: " + loss)
          log.info("Verlust an Paketen beim Senden: " + loss)
          var pkt = "Pakete;"
          var diff = "Unterschied in %;"
          do {
            diffPercent = 0.0
            numOfSyncPkt += 100
            communication ! SetSyncStop(numOfSyncPkt)
            Thread.sleep(100)
            communication ! SetLoss(0)
            1.to(numOfData).toList.foreach(data => ids = ids ::: List(accessLayer.insertNewObject(data)))
            Thread.sleep(100)
            communication ! Clock
            communication ! SetLoss(loss)
            Thread.sleep(1000)
            var i = 0
            while(i < repetition){
              i += 1
              1.to(numOfChangesPerCycle).toList.foreach(newData => {
                accessLayer.updateObject(ids(Random.nextInt(ids.length)), newData)
              })
              Thread.sleep(100)
              communication ! Clock
              Thread.sleep(100)
              log.info("[SimulationMaster] "+i+" Start Diff: "+comp.getTreeDiff())
              if(syncTrees(-1)._1 == true){
                i -= 1
              }else{
                diffPercent += comp.getTreeDiffInPercent()
                log.info("[SimulationMaster]"+ EventLogging.events.remove("[Synchro] CheckLeaf"))
              }
            }
            diffPercent = BigDecimal(diffPercent/repetition).setScale(2, BigDecimal.RoundingMode.HALF_EVEN).toDouble
            log.info("[SimulationMaster] Unterschied in Prozent: " + diffPercent + " Anzahl SyncPkt: "+numOfSyncPkt)

            Thread.sleep(100)
            actorList.foreach(x => x ! ClearTree)
            ids = List()
            pkt   += numOfSyncPkt +";"
            diff  += diffPercent  +";"
          } while (diffPercent > 0.2)
          printToFile.println(pkt)
          printToFile.println(diff)
          printToFile.flush()
        })
      }
      case _ => log.info("[SimulationMaster] Ungueltige Eingabe!")
    }
  }else{

  }
  //shutDownAllActors()
  Thread.sleep(100)
  system.shutdown()
  System.exit(0)

  def syncTrees(stepsInut: Int): (Boolean, (Int, Int), Int) = {
    //comp = new TreeCompare[EbTreeDataObject[Int]](actorList(0), actorList(1), system)
    var diff: (Int, Int) = (0, 0)
    var cycles: Int = 0
    var steps: Int = stepsInut
    if (stepsInut == -1) steps = 1
    var stuck = false
    val innerloop = new Breaks;
    innerloop.breakable {
      implicit val timeout = Timeout(60.second)
      implicit val ec = system.dispatcher
      while (steps > 0) {
        steps -= 1
        actorList.foreach(x => x ! PaintTree)
        if (Constant.GRAPHIC_ACTIVE == true) Thread.sleep(100)
        if (stepsInut == -1) steps += 1
        var actors = List(0, 1)
        def getRandomItem(x: Int = Random.nextInt(2)): Int = {
          actors = actors.diff(List(x))
          x
        }
        val synchroResponce: Future[Any] = communication ? StartSynchroCycle(Some(actorList(getRandomItem())), actorList(actors.head))
        val answer = Await.result(synchroResponce, timeout.duration)
        answer match {
          case SynchroCycleFinished => {
            cycles += 1
            if (stepsInut == -1) steps += 1
          }
          case SynchroFinished => {
            diff = comp.getTreeDiff()
            if (diff !=(0, 0)) {
              stuck = true
              log.info("[SimulationMaster] Tree sync Stuck! " + diff.toString())
            } else {
              log.info("[SimulationMaster] Tree sync complete!")
            }
            innerloop.break()
          }
          case SynchroStoped => {
            innerloop.break()
          }
          case _ =>
            log.error("[SimulationMaster] synchroResponce Failure!")
            innerloop.break()
        }
      }
    }
    if (stuck && diff !=(0, 0)) {
      EventLogging.addEvent("Unfinished Synchro!")
    }
    (stuck, diff, cycles)
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

