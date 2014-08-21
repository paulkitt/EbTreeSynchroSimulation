package model

import akka.testkit.{TestActorRef, ImplicitSender, DefaultTimeout, TestKit}
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, FunSuiteLike}
import simulation.CommunikationLayer
import scala.concurrent.Future
import akka.pattern.ask
import scala.util.{Failure, Success}
import akka.util.Timeout
import scala.concurrent.duration._

/**
 * Created by prototyp on 20.08.14.
 */
class SynchroTest extends TestKit(ActorSystem("SynchroTest", ConfigFactory.parseString(EbTreeDatabaseTest.config)))
with DefaultTimeout with ImplicitSender with FunSuiteLike with Matchers with BeforeAndAfterAll {

  override def afterAll = TestKit.shutdownActorSystem(system)




  test("Synchro with case B"){

    val communication = TestActorRef(new CommunikationLayer)

    val db1 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db1Actor = db1.underlyingActor

    val idList = List(1000L, 1001L, 1023L)
    idList.
      foreach(x => db1 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1, None, null)))

    val db2 = TestActorRef(new EbTreeDatabase[Int](communication))
    val db2Actor = db2.underlyingActor

    val idList2 = List(1000L, 1001L, 1023L,1024L, 1025L, 1800L)
    idList2.
      foreach(x => db2 ! InsertNewObject(EbTreeDataObject[Int](x, x, 1, None, null)))

     var start:StartSynchro = StartSynchro()
    start.toActorRef = db1
    start.fromActorRef = Some(db2)
    communication ! start

    implicit val timeout = Timeout(2.second)
    implicit val ec = system.dispatcher

    val uIdTreeAnswer:Future[Any] = communication ? GetuIdTreeRequest
    uIdTreeAnswer.onComplete{
      case Success(SynchroFinisched) =>
      case Failure(_)                =>
      case _                         =>
    }

  }
}

