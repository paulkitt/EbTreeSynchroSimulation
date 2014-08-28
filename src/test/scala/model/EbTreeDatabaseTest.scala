package model


import org.scalatest._
import akka.testkit._
import akka.actor.{ActorSystem}
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import src.main.scala.model.{Delta}


/**
 * Created by prototyp on 16.08.14.
 */
class EbTreeDatabaseTest extends TestKit(ActorSystem("EbTreeDatabaseTest", ConfigFactory.parseString(EbTreeDatabaseTest.config)))
with DefaultTimeout with ImplicitSender with FunSuiteLike with Matchers with BeforeAndAfterAll {


  override def afterAll = TestKit.shutdownActorSystem(system)

  test("EbTreeDatabase InsertNewObject is invoked") {
    val actorRef = TestActorRef(new EbTreeDatabase[Int](self))
    val actor = actorRef.underlyingActor

    val idList = List(1024L, 1025L, 1026L, 1032L, 1033L, 1045L, 1312L, 1800L)
    idList.
      foreach(x => actorRef ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))
    var cursor: Long = actor.uIdTree.firstKey()
    var actorItems: List[Long] = List(cursor)

    while (cursor != actor.uIdTree.lastKey()) {
      cursor = actor.uIdTree.next(cursor)
      actorItems = cursor :: actorItems
    }
    assert(idList.diff(actorItems) == List())
  }

  test("EbTreeDatabase UpdateObject") {
    val actorRef = TestActorRef(new EbTreeDatabase[Int](self))
    val actor = actorRef.underlyingActor

    val idList = List(1024L, 1025L, 1026L, 1032L, 1033L, 1045L, 1312L, 1800L)
    idList.
      foreach(x => actorRef ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    actorRef ! UpdateObject(EbTreeDataObject[Int](1024L, 2024L, 2),None, null)
    assert(actor.uIdTree.get(1024L).get.payload == 2)
    assert(actor.changeIdTree.get(2024L) == actor.uIdTree.get(1024L))

    actorRef ! UpdateObject(EbTreeDataObject[Int](1026L, 2026L, 2),None, null)
    assert(actor.uIdTree.get(1026L).get.payload == 2)
    assert(actor.changeIdTree.get(2026L) == actor.uIdTree.get(1026L))

    actorRef ! UpdateObject(EbTreeDataObject[Int](1032L, 2032L, 2),None, null)
    assert(actor.uIdTree.get(1032L).get.payload == 2)
    assert(actor.changeIdTree.get(2032L) == actor.uIdTree.get(1032L))

  }

  test("EbTreeDatabase CheckLeaf") {
    val actorRef = TestActorRef(new EbTreeDatabase[Int](self))
    val actor = actorRef.underlyingActor

    val idList = List(1024L, 1025L, 1026L, 1032L, 1033L, 1045L, 1312L, 1800L)
    idList.
      foreach(x => actorRef ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))
    actorRef ! UpdateObject(EbTreeDataObject[Int](1024L, 2024L, 2 ),None, null)
    actorRef ! UpdateObject(EbTreeDataObject[Int](1026L, 2026L, 2 ),None, null)
    actorRef ! UpdateObject(EbTreeDataObject[Int](1032L, 2032L, 2),None, null)

    val lostInsert = EbTreeDataObject[Int](2000L, 2500L, 42)
    actorRef ! CheckLeaf(lostInsert,None, null)
    assert(actor.uIdTree.get(2000L).get == lostInsert && actor.changeIdTree.get(2500L).get == lostInsert)

    val lostUpdate = EbTreeDataObject[Int](1024L, 2000L, 42 )
    actorRef ! CheckLeaf(lostUpdate,Some(self), actorRef)
    expectMsg(CheckLeaf(EbTreeDataObject[Int](1024L, 2024L, 2),Some(actorRef), self))

    val lostUpdate2 = EbTreeDataObject[Int](1024L, 3000L, 42)
    actorRef ! CheckLeaf(lostUpdate2, Some(self), actorRef)
    assert(actor.uIdTree.get(1024L).get == lostUpdate2 && actor.changeIdTree.get(3000L).get == lostUpdate2)
  }

  test("EbTreeDatabase Synchronize") {
    val actorRef = TestActorRef(new EbTreeDatabase[Int](self))
    val actor = actorRef.underlyingActor

    val idList = List(1000L, 1001L, 1023L , 1025L, 1026L, 1032L, 1033L, 1045L, 1312L, 1800L)
    idList.
      foreach(x => actorRef ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, self))

    actorRef ! Synchronize(Delta(0, 64, -1, -1, -2),Some(self),actorRef)
    expectMsg(Synchronize(Delta(0,64,10,1022,1599),Some(actorRef),self))

    actorRef ! Synchronize(Delta(1024, 10, 9, 1111, 1800),Some(self),actorRef)
    expectMsg(Synchronize(Delta(1024,9,8,1047,1312),Some(actorRef),self))

    actorRef ! Synchronize(Delta(1040, 4, 3, 1111, 1800),Some(self),actorRef)
    expectMsg(Synchronize(Delta(1040,4,-1,1045,1045),Some(actorRef),self))

    actorRef ! Synchronize(Delta(1032,3,-1,1033,1033),Some(self),actorRef)
    expectMsg( CheckLeaf(EbTreeDataObject(1032,1032,1),Some(actorRef),self))

    actorRef ! Synchronize(Delta(1040,3,-1,1046,1046),Some(self),actorRef)
    expectMsg(RequestEbTreeDataObject(Delta(1040,3,-1,1046,1046), Some(actorRef), self))
  }

  test("EbTreeDatabase RequestEbTreeDataObject") {
    val actorRef = TestActorRef(new EbTreeDatabase[Int](self))
    val actor = actorRef.underlyingActor

    val idList = List(1000L, 1001L, 1023L , 1025L, 1026L, 1032L, 1033L, 1045L, 1312L, 1800L)
    idList.
      foreach(x => actorRef ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    actorRef ! RequestEbTreeDataObject(Delta(1040,3,-1,1045,1045), Some(self),actorRef)
    expectMsg(CheckLeaf(EbTreeDataObject[Int](1045,1045,1),Some(actorRef),self))
  }

}

object EbTreeDatabaseTest {
  // Define your test specific configuration here
  val config = """
    akka {
      loglevel = "WARNING"
    }
               """
}