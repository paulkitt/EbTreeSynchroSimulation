package model

import akka.actor.ActorRef
import scala.util.Random
import org.slf4j.LoggerFactory

/**
 * Created by prototyp on 23.06.14.
 */
class AccessLayer[T](communicationLayer:ActorRef, actors:List[ActorRef]) {
  var randomRange:Int = 0
  var sequence:Int = 0
  val log = LoggerFactory.getLogger(classOf[AccessLayer[T]])


  def insertNewObject(payload:T):Long = {
    val id = genID()
    log.info("New Object "+payload+" with id:"+id)
    actors.foreach(x =>{
      val newObject: EbTreeDataObject[T] = EbTreeDataObject[T](id,id,payload,None,x)
      communicationLayer ! InsertNewObject(newObject)
    })
    id
  }
  def updateObject(uId:Long,newPayload:T) = ???
  def getObject(uId:Long,changeId:Long) = ???


  def genID():Long = { // TODO WRITE test
    val range = randomRange to randomRange+10
    randomRange +=10
    val random = range(Random.nextInt(range.length))
    val timestamp = System.currentTimeMillis / 1000
    val tmpBin = timestamp.toBinaryString
    val seqBin = toBinary(sequence)
    val rndBin = toBinary(random)
    val binary =(timestamp.toBinaryString + toBinary(sequence) + toBinary(random))
    sequence+=1
    java.lang.Long.parseLong(binary,2)
    // TODO use byte array arrays bytebuffer instead of string
  }

  def toBinary(i: Int, digits: Int = 8):String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')

}
