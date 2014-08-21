package simulation

import akka.actor.{ActorRef, Actor}
import akka.actor.Actor.Receive
import model._
import model.EbTreeDataObject

import model.InsertNewObject
import akka.event.Logging
import src.main.scala.model.Delta


/**
 * Created by prototyp on 19.06.14.
 */
class CommunikationLayer() extends Actor{
  var cells:List[ActorRef] = _ // HashMap name> actRef
  var ebTreeMessageQueue:List[List[EBTreeMessage]] = List(List()) //first list cells, second list clock slots, third list messages for each time slot
//  var ebTreeMessageQueue1:List[List[EBTreeMessage]] = List(List())
//  var ebTreeMessageQueue2:List[List[EBTreeMessage]] = List(List())
  var synchroStart:ActorRef = _
  var log = Logging(context.system, this)

  override def receive: Receive = {
    //init
    case SetTreeActors(actors:List[ActorRef]) =>
      //ebTreeMessageQueue = ebTreeMessageQueue(0) :: List(List())
      //ebTreeMessageQueue = ebTreeMessageQueue(1) :: List(List())
      //actors.foreach(x=>ebTreeMessageQueue :: List(List(Nil)))
      cells = actors


    //basic  operations
    case Clock =>
      log.info("[Clock] received!")
      if(!ebTreeMessageQueue.isEmpty){
        ebTreeMessageQueue.head.foreach(x => x.dataObject.toActorRef ! x) // send all queued messages
        ebTreeMessageQueue = ebTreeMessageQueue.tail//delete head
      }



    case InsertNewObject(newObject: EbTreeDataObject[_])=>
      log.info("[InsertNewObject] received! Placing item in queue")
      //insert new item the head of the queue
      ebTreeMessageQueue = List(ebTreeMessageQueue.head ::: List(InsertNewObject(newObject: EbTreeDataObject[_])) ) ::: ebTreeMessageQueue.tail
      //TODO insert failure => packet loss or delay

    //sync operations
    case x:StartSynchro =>
      log.info("{" + self.path.name + "}" + "[StartSynchro] received! Syncing "+x.fromActorRef)
      synchroStart = sender()
      x.toActorRef ! Synchronize(DeltaObject(Delta(0, 64, -1, -1, -2),Some(x.fromActorRef.get),x.toActorRef))

    case SynchroFinisched =>
      log.info("{" + self.path.name + "}" + "[SynchroStep] fininished! Syncing ")
      sender ! SynchroFinisched

    case Synchronize(delta:DeltaObject) =>
      log.info("{" + self.path.name + "}" + "[Synchronize] Syncing "+x.fromActorRef)
      delta.toActorRef ! Synchronize(delta)

    case x:CheckLeaf[_] =>
      x.data.toActorRef ! x

    case  x:RequestEbTreeDataObject =>
      x.delta.toActorRefInc ! x

    case ShutDownActorRequest => context.system.shutdown()

    case x:_ => log.error("CommunicationActor wrong message received")
  }

}

// for each actor a fifo queeu which gets called via a clock

//Random  seed

// Should system be designed for n treeActors?

// what happens to synchro if tree part changes while synchro