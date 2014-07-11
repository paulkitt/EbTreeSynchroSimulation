package model

import akka.actor.{ActorRef, Actor}
import simulation.CommunikationLayer
import src.main.scala.model.EbTree
import akka.event.Logging


// define parent class with roting information

trait MessageRoutingInformation{
  var fromActorRef:Option[ActorRef] = _
  var toActorRef:ActorRef = _
}
trait EBTreeMessage{
  var dataObject:MessageRoutingInformation = _
}

case class SetTreeActors(actors:List[ActorRef])

case class EbTreeDataObject[+T](uId: Long, changeId: Long, payload:T, fromActorRefInc:Option[ActorRef],toActorRefInc:ActorRef)
  extends MessageRoutingInformation{fromActorRef = fromActorRefInc;toActorRef = toActorRefInc}

case class InsertNewObject[T](newObject: EbTreeDataObject[T])
  extends EBTreeMessage{dataObject = newObject}

case class UpdateObject[T](changedObject: EbTreeDataObject[T]) extends EBTreeMessage

case class RemoveObject[T](removedObject: EbTreeDataObject[T]) extends EBTreeMessage

case object StartSynchronisation

case class DeltaObject(parentBit:Long,changeIdParent:Long,changeIdZeroChild:Long,changeIdOneChild:Long,fromActorRefInc:Option[ActorRef],toActorRefInc:ActorRef)
  extends MessageRoutingInformation {fromActorRef = fromActorRefInc;toActorRef = toActorRefInc}

case class GetDelta(delta:DeltaObject)
case class ReturnDelta(delta:DeltaObject)

case object GetChangeIdTreeRequest

case object GetuIdTreeRequest

case class Tree[T](tree:EbTree[T])

case object ShutDownActorRequest

/**
 * Created by prototyp on 21.06.14.
 */
class TreeActor[T](communication:ActorRef) extends Actor {//with akka.actor.ActorLogging{
  var cells:List[ActorRef] = _
  var uIdTree = new EbTree[EbTreeDataObject[T]]
  var changeIdTree = new EbTree[EbTreeDataObject[T]]
  val log = Logging(context.system,this)

  override def receive: Receive = {
    //treeActor init
    case SetTreeActors(actors:List[ActorRef]) =>
      cells = actors.filter(x => x!=context.self)
      cells.foreach(x => log.info(self.path.name+": Init with Actor:"+x.path.name))

    //basic tree operations
    case InsertNewObject(newObject: EbTreeDataObject[T]) =>
      log.info(self.path.name+"[InsertNewObject] received! new object:"+ newObject.uId+", "+newObject.changeId+", "+newObject.payload)
      uIdTree.put(newObject.uId, newObject)
      changeIdTree.put(newObject.changeId, newObject)

    case UpdateObject(newObject: EbTreeDataObject[T]) =>
        // get old changeID from uidTree and alter payload
        // remove old changeId from changeIDTree
        // insert changeId
    case RemoveObject(removedObject: EbTreeDataObject[T]) =>
      // get old changeID from uidTree and set payload null


    //sync operations

    case StartSynchronisation => // start synchronisation with sending first delta
      cells.foreach(x => {
        log.info(self.path.name+"[StartSynchronisation] received! Start synchro with sending Delta LvL 0 to "+x.path.name)
        var getDeltaMsg = DeltaObject(0,0,0,0,Some(self),x)
        communication !  GetDelta(getDeltaMsg)
        })



    case GetDelta(delta:DeltaObject) => // answer if newer Delta is present
      println(context.self.path.name+" received GetDelta")
      var newDelta:DeltaObject = DeltaObject(1,1,1,1,Some(self),sender)
      sender ! ReturnDelta(newDelta)


    case ReturnDelta(delta:DeltaObject) =>
      println(context.self.path.name+" received ReturnDelta")



    //TODO synchronized delete of empty objects


    case GetChangeIdTreeRequest => //returns tree reference for tree diff
      sender ! Tree(changeIdTree)


    case GetuIdTreeRequest =>  //returns tree reference for tree diff
      sender ! Tree(uIdTree)

    case ShutDownActorRequest => context.system.shutdown()

    case _ => println("TreeActor wrong message received")
  }
}

//How to start synchronisation -> spawn actor which sleeps given oder rnd and starts the synchro when awaik

// Fehlerbehandlung ?