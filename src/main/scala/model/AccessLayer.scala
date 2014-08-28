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
    //log.info("New Object "+payload+" with id:"+id)
    actors.foreach(x =>{
      val newObject: EbTreeDataObject[T] = EbTreeDataObject[T](id,id,payload)
      communicationLayer ! InsertNewObject(newObject,Some(communicationLayer),x)
    })
    id
  }
  def updateObject(uId:Long,newPayload:T):Long = {
    val newChangeID = genID()
    //log.info("New Object "+newPayload+" with id:"+newChangeID)
    actors.foreach(x =>{
      val newObject: EbTreeDataObject[T] = EbTreeDataObject[T](uId,newChangeID,newPayload)
      communicationLayer ! UpdateObject(newObject,Some(communicationLayer),x)
    })
    newChangeID
  }

  def removeObject(id:Long) = ???
  def getObject(uId:Long,changeId:Long) = ???


  def genID():Long = {
    val range = randomRange to randomRange+10
    randomRange +=10
    val random = range(Random.nextInt(range.length))
    val timestamp = (System.currentTimeMillis / 100).toBinaryString
    val seqBin = toBinary(sequence,7)
    val rndBin = toBinary(Random.nextInt(32767),15)

    val binary =(timestamp + seqBin  +  rndBin)
    //val binary =( toBinary(sequence) + toBinary(random))
    sequence+=1
    if(sequence>=255){
      sequence =0
    }

    log.info("[AccessLayer] bin len: "+binary.length )
    if(timestamp.length>39 || seqBin.length>8 || rndBin.length>16){
      log.error("[AccessLayer]: TmpLen: "+timestamp.length+" seqLen: "+seqBin.length+" rndBinLen: "+rndBin.length)
      log.error("[AccessLayer] Binary Error")
    }
    java.lang.Long.parseLong(binary,2)
    // TODO use byte array arrays bytebuffer instead of string
  }

  def toBinary(i: Int, digits: Int):String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')

}
