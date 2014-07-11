package simulation

import akka.actor.{Props, ActorSystem}
import model._

import scala.collection.immutable.HashMap
import model.SetTreeActors
import model.InsertNewObject
import akka.event.Logging
import com.typesafe.config.ConfigFactory
import scala.concurrent.Future
import scala.concurrent.ExecutionContext._

case object Clock

/**
 * Created by prototyp on 19.06.14.
 */
object SimulationMaster extends App{
  //init the Simulator

  val system = ActorSystem("EBTreeSimulation", ConfigFactory.load.getConfig("akka"))
  val log = Logging.getLogger(system,this)
  val communication = system.actorOf(Props[CommunikationLayer[Int]],"communication")
  val actorList = List(system.actorOf(Props(new TreeActor[Int](communication)),"nodeA"),
                       system.actorOf(Props(new TreeActor[Int](communication)),"nodeB"))
  val accessLayer:AccessLayer[Int] = new AccessLayer[Int](communication,actorList)
  // sends to each tree managing actor the references of the others that they can add routing information
  actorList.foreach(f=> f ! SetTreeActors(actorList))
  // send all tree actor references to the communication layer
  communication ! SetTreeActors(actorList)
  log.info("[SimulationMaster] Done init start Simulation")


  // basic test: send some values
  val testValues = List(10,11,12,13)
  testValues.foreach(x => accessLayer.insertNewObject(x))

  this.wait(1000)
  val sleeped = Future { Thread.sleep(1000); 42 }
  sleeped.onSuccess(communication ! Clock)


  actorList(0) ! StartSynchronisation
  actorList(1) ! StartSynchronisation
  //shutDownAllActors()



  def shutDownAllActors(){
    communication!ShutDownActorRequest
    actorList.foreach(x => x !ShutDownActorRequest)
  }
}



//TODO select logging pattern -> check stackOverFlow
//TODO implement tree id xor
//TODO implement tree diff: logical and graphical

// implement clock actor
