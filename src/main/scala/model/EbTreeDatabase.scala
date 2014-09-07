package model

import akka.actor.{ActorRef, Actor}
import simulation.{TreeFx, TreeView, EventLogging}
import src.main.scala.model.{Delta, EbTree}
import akka.event.Logging


// define parent class with roting information

trait EBTreeMessage {
  var fromActorRef: Option[ActorRef] = _
  var toActorRef: ActorRef = _
}

// Init
case class SetTreeActors(actors: List[ActorRef])
case class SetLoss(loss:Int)

//----------------------------------------------------------------------------------------------
//Basic tree ops
case class EbTreeDataObject[+T](uId: Long, changeId: Long, payload: T)

case class InsertNewObject[T](newObject: EbTreeDataObject[T], from:Option[ActorRef], to:ActorRef) extends EBTreeMessage {
  fromActorRef = from; toActorRef = to
}

case class UpdateObject[T](changedObject: EbTreeDataObject[T], from:Option[ActorRef], to:ActorRef) extends EBTreeMessage {
  fromActorRef = from; toActorRef = to
}

case class RemoveObject[T](removedObject: EbTreeDataObject[T], from:Option[ActorRef], to:ActorRef) extends EBTreeMessage {
  fromActorRef = from; toActorRef = to
}

case class GetObject[T](id:Long)

//----------------------------------------------------------------------------------------------
// Synchro


case class StartSynchro(from:Option[ActorRef], to:ActorRef) extends EBTreeMessage {
fromActorRef = from; toActorRef = to
}

case object SynchroFinished{
  override def toString = {"SynchroFinished"}
}

case class Synchronize(delta: Delta,from:Option[ActorRef], to:ActorRef) extends EBTreeMessage {
  fromActorRef = from; toActorRef = to
}

case class RequestEbTreeDataObject(delta: Delta, from:Option[ActorRef], to:ActorRef) extends EBTreeMessage{
  fromActorRef = from; toActorRef = to
}

case class CheckLeaf[T](data: EbTreeDataObject[T], from:Option[ActorRef], to:ActorRef) extends EBTreeMessage{
  fromActorRef = from; toActorRef = to
}

//----------------------------------------------------------------------------------------------
// tree diff
case object PaintTree

case object GetChangeIdTreeRequest

case object  GetuIdTreeRequest


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
  //var treeV:TreeFx = new TreeFx("")
 //var treeV:TreeView = new TreeView("")
  val log = Logging(context.system, this)



  override def receive: Receive = {
    //treeActor init
    case SetTreeActors(actors: List[ActorRef]) =>
      cells = actors.filter(x => x != context.self)
      cells.foreach(x => log.info("{" + self.path.name + "}" + ": Init with Actor:" + x.path.name))

    case PaintTree =>
     //treeV.setTree(changeIdTree.myRoot.get.myZero)
     //treeV.setTree(changeIdTree.myRoot.get.myZero)

    //basic tree operations
    case obj:InsertNewObject[T]=>
      log.info("{" + self.path.name + "}" + "[InsertNewObject]! new object:" + obj.newObject.uId + ", " + obj.newObject.changeId + ", " + obj.newObject.payload)
      uIdTree.put(obj.newObject.uId, obj.newObject)
      changeIdTree.put(obj.newObject.changeId, obj.newObject)

    case uptObj:UpdateObject[T] => uIdTree.get(uptObj.changedObject.uId) match {
      case Some(oldObJ: EbTreeDataObject[T]) =>
        log.info("{" + self.path.name + "}" + "[UpdateObject]! update object:" + uptObj.changedObject.uId + ", " + uptObj.changedObject.changeId + ", " + uptObj.changedObject.payload)
        uIdTree.remove(oldObJ.uId) // get old object from uidTree and alter payload
        uIdTree.put(uptObj.changedObject.uId, uptObj.changedObject)
        changeIdTree.remove(oldObJ.changeId) // remove old changeId from changeIDTree
        changeIdTree.put(uptObj.changedObject.changeId, uptObj.changedObject) // insert new ChangeId
      case _ =>
    }

    case delObj:RemoveObject[T] => uIdTree.get(delObj.removedObject.uId) match {
      case Some(oldObJ: EbTreeDataObject[T]) =>
        uIdTree.remove(oldObJ.uId) // get old object from uidTree and alter payload
        uIdTree.put(delObj.removedObject.uId, delObj.removedObject)
        changeIdTree.remove(oldObJ.changeId) // remove old changeId from changeIDTree
        changeIdTree.put(delObj.removedObject.changeId, delObj.removedObject) // insert new ChangeId
      case _ => log.info("{" + self.path.name + "}" +"[RemoveObject] object not found!")
    }
    //TODO  set payload null add new ChangeID

    case get:GetObject[T] =>

    //sync operations

    case sync:Synchronize => // start synchronisation with sending first delta
      if (changeIdTree.size() > 1) {
        log.info("{" + self.path.name + "}" + "[Synchronize]! Delta: "+sync.delta+"! Syncing with " +  sync.fromActorRef.get.path.name)
        if (sync.delta.isLeaf()) {
          val uniqueLeaf: Delta = changeIdTree.checkIfLostLeafIsFound(sync.delta)
          if (sync.delta.l == uniqueLeaf.l) { //receive unique leaf id! request all leaf data
            communication ! RequestEbTreeDataObject(sync.delta, Some(self), sync.fromActorRef.get)
          } else {
            // found subbranch at leaf position with leaf inside => unique leaf most left in sub branch
            val changeID = uniqueLeaf.l
            val data: EbTreeDataObject[T] = changeIdTree.get(changeID).get
            communication ! CheckLeaf(EbTreeDataObject[T](data.uId, data.changeId, data.payload), Some(self), sync.fromActorRef.get)
          }
        } else {
          val delta: Delta = changeIdTree.nextDelta(sync.delta)
          if(delta != null){
            communication ! Synchronize(delta, Some(self), sync.fromActorRef.get)
          }else{
            log.info("[Synchronize] Trees in Sync!")
            communication ! SynchroFinished
          }
        }
      }

    case req:RequestEbTreeDataObject => changeIdTree.get(req.delta.l) match {
      case Some(data:EbTreeDataObject[T]) =>
        log.info("{" + self.path.name + "}" + "[RequestEbTreeDataObject]! requested: "+req.delta)
        communication ! CheckLeaf(EbTreeDataObject[T](data.uId,data.changeId,data.payload),Some(self),req.fromActorRef.get)
      case None =>
        log.error("[RequestEbTreeDataObject] but requested leaf not found")
    }

    case checkL:CheckLeaf[T] => uIdTree.get(checkL.data.uId) match {
      case Some(ownData: EbTreeDataObject[T]) =>
        log.info("{" + self.path.name + "}" + "[CheckLeaf]! found Lost Update: "+checkL.data)
        ownData.changeId match {
          case cId: Long if (cId > checkL.data.changeId) =>
            log.info("{" + self.path.name + "}" + "[CheckLeaf]! Own data is mor up to date! sending data")
            communication ! CheckLeaf(EbTreeDataObject[T](ownData.uId, ownData.changeId, ownData.payload), Some(self), checkL.fromActorRef.get)
          case cId: Long if (cId < checkL.data.changeId) =>
            log.info("{" + self.path.name + "}" + "[CheckLeaf]! Own data is old! updating!")
            self ! UpdateObject(checkL.data,Some(self),self)
            communication ! SynchroFinished
          case cId: Long if (cId == checkL.data.changeId) =>
            log.error("{" + self.path.name + "}" + "[CheckLeaf] received with equal cID:"+cId)
            EventLogging.addEvent("[CheckLeaf] RECEIVED DUPLICATE!")
            //val leftItem:EbTreeDataObject[T] = changeIdTree.get(changeIdTree.prev(ownData.changeId)).get
            //communication ! CheckLeaf(leftItem,Some(self),checkL.fromActorRef.get)
            communication ! SynchroFinished
        }
      case _ =>
        log.info("{" + self.path.name + "}" + "[CheckLeaf] ! found Lost Insert: "+checkL.data)
        uIdTree.put(checkL.data.uId, checkL.data)
        changeIdTree.put(checkL.data.changeId, checkL.data)
        communication ! SynchroFinished
    }

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