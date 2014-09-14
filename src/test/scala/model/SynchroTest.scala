package model

import akka.testkit.{TestActorRef, ImplicitSender, DefaultTimeout, TestKit}
import akka.actor.{ActorRef, ActorSystem}
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, FunSuiteLike}
import simulation.{Clock, CommunicationLayer, TreeCompare}
import scala.concurrent.Future
import akka.pattern.ask
import scala.util.{Failure, Success}
import akka.util.Timeout
import scala.util.Random
import org.slf4j.LoggerFactory


/**
 * Created by prototyp on 20.08.14.
 */
class SynchroTest extends TestKit(ActorSystem("SynchroTest", ConfigFactory.parseString(EbTreeDatabaseTest.config)))
with DefaultTimeout with ImplicitSender with FunSuiteLike with Matchers with BeforeAndAfterAll {

  override def afterAll = TestKit.shutdownActorSystem(system)
  val log = LoggerFactory.getLogger(classOf[SynchroTest])



  test("Synchro with case B"){

    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor

    val idList = List(1000L, 1001L, 1023L)
    idList.
      foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    val idList2 = List(1000L, 1001L, 1023L,1024L, 1025L, 1800L)
    idList2.
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
    }
    assert(diff._2==0)
  }

  test("Synchro of LostUpdate"){
    val communication = TestActorRef(new CommunicationLayer(0,0))
    val idList = List(1000L, 1001L, 1023L,1024L, 1025L, 1800L)

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor
    idList.foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), Some(self), db1))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor
    idList.foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), Some(self), db2))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)

    assert(comp.compareTrees()._1==0)

    db2 ! UpdateObject[Int](EbTreeDataObject[Int](1024L,2024L,42), Some(self), db2)
    db2 ! UpdateObject[Int](EbTreeDataObject[Int](1023L,2023L,42), Some(self), db2)
    db2 ! UpdateObject[Int](EbTreeDataObject[Int](1000L,2000L,42), Some(self), db2)

    assert(comp.compareTrees() == (0,6))

    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
    }
    assert(diff==(0,0))
  }


  test("Synchro with case B*"){

    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor

    val idList = List(1000L, 1001L, 1025L)
    idList.
      foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    val idList2 = List(1000L, 1001L,1024L, 1025L, 1800L)
    idList2.
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
    }
    assert(diff._2==0)
  }
  test("Synchro with case B**"){

    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor

    val idList = List(1000L, 1001L, 1025L)
    idList.
      foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    val idList2 = List(1000L, 1001L,1024L, 1026L, 1800L)
    idList2.
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
    }
    assert(diff._2==0)
  }
  test("Synchro with case B***"){

    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor

    val idList = List(1000L, 1001L, 1025L)
    idList.
      foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    val idList2 = List(1000L, 1001L,1025L, 1026L, 1800L)
    idList2.
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
    }
    assert(diff._2==0)
  }
  test("Synchro with case G->E"){

    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor
    List(1025L, 1026L)
      .foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    List(1025L, 1026L, 1032L, 1033L, 1312L, 1800L).
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
     }
    assert(diff._2==0)
  }
  test("Synchro with case G->E*"){

    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor
    List(1033L, 1045L)
    .foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    List(1025L, 1026L, 1026L, 1032L, 1033L, 1312L, 1800L).
   foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
    }
    assert(diff._2==0)
  }
  test("Synchro with case G->E**"){

    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor
    List(1025L, 1026L)
      .foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    List(1024L, 1025L, 1026L, 1032L, 1033L, 1045L, 1312L, 1800L).
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
    }
    assert(diff._2==0)
  }
  test("Synchro with case G->E***"){

    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor
    List(1028L, 1029L)
      .foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    List(1025L, 1027L, 1032L, 1033L, 1312L, 1800L).
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      //readLine()
      diff = comp.compareTrees()
    }
    assert(diff._2==0)
  }

  test("Synchro with case G->E****"){

    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor
    List(1026L, 1027L,1028L, 1029L)
      .foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    List(1026L, 1027L, 1032L, 1033L, 1312L, 1800L).
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
    }
    assert(diff._2==0)
  }
  test("Synchro with both side diff"){
    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor
    List(1211116L, 127L,1280L, 10029L)
      .foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1),None, null))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    List(10260L, 1027L, 10320L, 1033L, 1312L, 1800L).
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1), None, null))

    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff._2!=0){
      communication ! StartSynchroCycle(Some(db1),db2)
      expectMsg(SynchroCycleFinished)
      diff = comp.compareTrees()
    }
    assert(diff._2==0)
  }
  test("Synchro random diffs"){
    val communication = TestActorRef(new CommunicationLayer(0,0))

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor
    val aLay1 = new AccessLayer[Int](communication,List(db1),64)

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor
    val aLay2 = new AccessLayer[Int](communication,List(db2),64)

    communication ! SetTreeActors(List(db1,db2))

    val values:List[Int] =  1.to(40).toList
      values.foreach(x => aLay1.insertNewObject(x))
      Thread.sleep(1000)
      values.foreach(x => aLay2.insertNewObject(x))

    communication ! Clock


    val comp:TreeCompare[EbTreeDataObject[Int]] = new TreeCompare[EbTreeDataObject[Int]](db1,db2,system)
    //assert(comp.compareTrees()==(84,84))
    var oldDiff:(Int,Int)= (0,0)
    var diff:(Int,Int) = comp.compareTrees()
    while(diff!=(0,0)){
      if(oldDiff!= diff){
        communication ! StartSynchroCycle(Some(db1),db2)
        expectMsg(SynchroCycleFinished)
        oldDiff = diff
        diff = comp.compareTrees()
        log.info("Diff "+diff)
        Thread.sleep(500)
        db1 ! PaintTree
        db2 ! PaintTree
      }else{
        comp.printTreeItems()
        Thread.sleep(10000)
      }


    }
    assert(diff._2==0)
  }
  test("Synchro after inserts updates with paketloss"){

  }
}

