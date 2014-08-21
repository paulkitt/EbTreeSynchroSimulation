package model

import akka.actor.{ActorRef, Actor}
import simulation.{EventLogging, CommunikationLayer}
import src.main.scala.model.{Delta, EbTree}
import akka.event.Logging


// define parent class with roting information

trait MessageRoutingInformation {
  var fromActorRef: Option[ActorRef] = _
  var toActorRef: ActorRef = _
}

trait EBTreeMessage {
  var dataObject: MessageRoutingInformation = _
}

// Init
case class SetTreeActors(actors: List[ActorRef])

//----------------------------------------------------------------------------------------------
//Basic tree ops
case class EbTreeDataObject[+T](uId: Long, changeId: Long, payload: T, fromActorRefInc: Option[ActorRef], toActorRefInc: ActorRef)
  extends MessageRoutingInformation {
  fromActorRef = fromActorRefInc; toActorRef = toActorRefInc
}

case class InsertNewObject[T](newObject: EbTreeDataObject[T])
  extends EBTreeMessage {
  dataObject = newObject
}

case class UpdateObject[T](changedObject: EbTreeDataObject[T]) extends EBTreeMessage

case class RemoveObject[T](removedObject: EbTreeDataObject[T]) extends EBTreeMessage

//----------------------------------------------------------------------------------------------
// Synchro


case class DeltaObject(delta: Delta, fromActorRefInc: Option[ActorRef], toActorRefInc: ActorRef)
  extends MessageRoutingInformation {
  fromActorRef = fromActorRefInc; toActorRef = toActorRefInc
}

case class StartSynchro()extends MessageRoutingInformation

case object SynchroFinisched

case class Synchronize(delta: DeltaObject)

case class RequestEbTreeDataObject(delta: DeltaObject)

case class CheckLeaf[T](data: EbTreeDataObject[T])

//----------------------------------------------------------------------------------------------
// tree diff
case object GetChangeIdTreeRequest

case object GetuIdTreeRequest

case class Tree[T](tree: EbTree[T])

case object ShutDownActorRequest

/**
 * Created by prototyp on 21.06.14.
 */
class EbTreeDatabase[T](communication: ActorRef) extends Actor {
  //with akka.actor.ActorLogging{
  var cells: List[ActorRef] = _
  var uIdTree = new EbTree[EbTreeDataObject[T]]
  var changeIdTree = new EbTree[EbTreeDataObject[T]]
  val log = Logging(context.system, this)


  override def receive: Receive = {
    //treeActor init
    case SetTreeActors(actors: List[ActorRef]) =>
      cells = actors.filter(x => x != context.self)
      cells.foreach(x => log.info("{" + self.path.name + "}" + ": Init with Actor:" + x.path.name))

    //basic tree operations
    case InsertNewObject(newObject: EbTreeDataObject[T]) =>
      log.info("{" + self.path.name + "}" + "[InsertNewObject] received! new object:" + newObject.uId + ", " + newObject.changeId + ", " + newObject.payload)
      uIdTree.put(newObject.uId, newObject)
      changeIdTree.put(newObject.changeId, newObject)

    case UpdateObject(newObject: EbTreeDataObject[T]) => uIdTree.get(newObject.uId) match {
      case Some(oldObJ: EbTreeDataObject[T]) =>
        uIdTree.remove(oldObJ.uId) // get old object from uidTree and alter payload
        uIdTree.put(newObject.uId, newObject)
        changeIdTree.remove(oldObJ.changeId) // remove old changeId from changeIDTree
        changeIdTree.put(newObject.changeId, newObject) // insert new ChangeId
      case _ =>
    }

    case RemoveObject(removedObject: EbTreeDataObject[T]) =>
    // get old changeID from uidTree and set payload null

    //sync operations

    case Synchronize(dObj: DeltaObject) => // start synchronisation with sending first delta
      if (changeIdTree.size() > 2) {
        log.info("{" + self.path.name + "}" + "[Synchronize] received! Syncing with " + dObj.fromActorRefInc.get.path.name)

        if (dObj.delta.isLeaf()) {
          val uniqueLeaf: Delta = changeIdTree.checkIfUniqueLeafisFound(dObj.delta)
          if (dObj.delta.l == uniqueLeaf.l) { //
            //receive unique leaf id! request all leaf data
            communication ! RequestEbTreeDataObject(DeltaObject(dObj.delta, Some(self), dObj.fromActorRefInc.get))
          } else {
            // found subbranch at leaf position with leaf inside => unique leaf most left in sub branch
            val changeID = uniqueLeaf.l
            val data: EbTreeDataObject[T] = changeIdTree.get(changeID).get
            communication ! CheckLeaf(EbTreeDataObject[T](data.uId, data.changeId, data.payload, Some(self), dObj.fromActorRefInc.get))
          }
        } else {
          val delta: Delta = changeIdTree.nextDelta(dObj.delta)
          communication ! Synchronize(DeltaObject(delta, Some(self), dObj.fromActorRefInc.get))
        }
      }

    case RequestEbTreeDataObject(delta: DeltaObject) => changeIdTree.get(delta.delta.l) match {
      case Some(data:EbTreeDataObject[T]) =>
        communication ! CheckLeaf(EbTreeDataObject[T](data.uId,data.changeId,data.payload,Some(self),delta.fromActorRefInc.get))
      case None => //Todo log error
    }

    case CheckLeaf(data: EbTreeDataObject[T]) => uIdTree.get(data.uId) match {
      case Some(ownData: EbTreeDataObject[T]) =>
        ownData.changeId match {
          case cId: Long if (cId > data.changeId) =>
            communication ! CheckLeaf(EbTreeDataObject[T](ownData.uId, ownData.changeId, ownData.payload, Some(self), data.fromActorRefInc.get))
          case cId: Long if (cId < data.changeId) =>
            self ! UpdateObject(data)
          case cId: Long if (cId == data.changeId) => //TODO log this
            log.error("{" + self.path.name + "}" + "[CheckLeaf] received with equal cID")
            EventLogging.addEvent("CheckLeaf received Duplicate!")
        }
      case _ => log.warning("{" + self.path.name + "}" + "[CheckLeaf] received! found Lost Insert")
        uIdTree.put(data.uId, data)
        changeIdTree.put(data.changeId, data)
    }
    //todo send finish synchro


    //TODO synchronized delete of empty objects



    case GetuIdTreeRequest => //returns tree reference for tree diff
      log.info("{" + self.path.name + "}" + "[GetuIdTreeRequest] received! Sending uIdTree.")
      sender ! Tree(uIdTree)
    case GetChangeIdTreeRequest => //returns tree reference for tree diff
      log.info("{" + self.path.name + "}" + "[GetChangeIdTreeRequest] received! Sending changeIdTree.")
      sender ! Tree(changeIdTree)

    case ShutDownActorRequest =>
      context.system.shutdown()
    case _ => println("TreeActor wrong message received")
  }
}

//How to start synchronisation -> spawn actor which sleeps given oder rnd and starts the synchro when awaik

// Fehlerbehandlung ?