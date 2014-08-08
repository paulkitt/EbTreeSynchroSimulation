package simulation

import akka.actor.{ActorRef, Actor}
import akka.actor.Actor.Receive
import model._
import model.EbTreeDataObject
import model.GetDelta
import model.InsertNewObject
import akka.event.Logging





/**
 * Created by prototyp on 19.06.14.
 */
class CommunikationLayer[T]() extends Actor{
  var cells:List[ActorRef] = _ // HashMap name> actRef
  var ebTreeMessageQueue:List[List[EBTreeMessage]] = List(List()) //first list cells, second list clock slots, third list messages for each time slot
//  var ebTreeMessageQueue1:List[List[EBTreeMessage]] = List(List())
//  var ebTreeMessageQueue2:List[List[EBTreeMessage]] = List(List())
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



    case InsertNewObject(newObject: EbTreeDataObject[T])=>
      log.info("[InsertNewObject] received! Placing item in queue")
      //insert new item the head of the queue
      ebTreeMessageQueue = List(ebTreeMessageQueue.head ::: List(InsertNewObject(newObject: EbTreeDataObject[T])) ) ::: ebTreeMessageQueue.tail
      //TODO insert failure => packet loss or delay




    //sync operations
    case GetDelta(delta:DeltaObject) =>
        log.info("[GetDelta] from "+delta.fromActorRef.get.path.name)
      delta.toActorRefInc ! GetDelta(delta:DeltaObject)


    case ReturnDelta(delta:DeltaObject) =>



    case ShutDownActorRequest => context.system.shutdown()

    case _ => log.error("CommunicationActor wrong message received")
  }

}

// for each actor a fifo queeu which gets called via a clock

//Random  seed

// Should system be designed for n treeActors?

// what happens to synchro if tree part changes while synchro