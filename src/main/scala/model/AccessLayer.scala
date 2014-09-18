package model

import akka.actor.ActorRef
import scala.util.Random
import org.slf4j.LoggerFactory

/**
 * Created by prototyp on 23.06.14.
 */
class AccessLayer[T](communicationLayer:ActorRef, actors:List[ActorRef],keyLength:Int) {

  var sequence:Int = 0
  val log = LoggerFactory.getLogger(classOf[AccessLayer[T]])


  def insertNewObject(payload:T):Long = {
    val id = getID()
    //log.info("New Object "+payload+" with id:"+id)
    actors.foreach(x =>{
      val newObject: EbTreeDataObject[T] = EbTreeDataObject[T](id,id,payload)
      communicationLayer ! InsertNewObject(newObject,Some(communicationLayer),x)
    })
    id
  }
  def updateObject(uId:Long,newPayload:T):Long = {
    val newChangeID = getID()
    //log.info("New Object "+newPayload+" with id:"+newChangeID)
    actors.foreach(x =>{
      val newObject: EbTreeDataObject[T] = EbTreeDataObject[T](uId,newChangeID,newPayload)
      communicationLayer ! UpdateObject(newObject,Some(communicationLayer),x)
    })
    newChangeID
  }

  def removeObject(id:Long) = ???
  def getObject(uId:Long,changeId:Long) = ???

  def getID():Long = keyLength match{
    case 48 => genID48
    case 56 => genID56
    case 64 => genID64
  }

  def genID64():Long = {

    val timestamp = (System.currentTimeMillis / 100).toBinaryString
    val seqBin = toBinary(sequence,7)
    val rndBin = toBinary(Random.nextInt(32767),15)

    val binary =(timestamp + seqBin  +  rndBin)
    sequence+=1
    if(sequence>=255){
      sequence =0
    }
    if(timestamp.length>39 || seqBin.length>8 || rndBin.length>16){
      log.error("[AccessLayer] 64: TmpLen: "+timestamp.length+" seqLen: "+seqBin.length+" rndBinLen: "+rndBin.length)
      log.error("[AccessLayer] Binary Error")
    }
    java.lang.Long.parseLong(binary,2)
  }

  def genID56():Long = {

    val timestamp = (System.currentTimeMillis / 1000).toBinaryString
    val seqBin = toBinary(sequence,7)
    val rndBin = toBinary(Random.nextInt(32767),15)

    val binary =(timestamp + seqBin  +  rndBin)
    sequence+=1
    if(sequence>=255){
      sequence =0
    }
    if(timestamp.length>32 || seqBin.length>8 || rndBin.length>16){
      log.error("[AccessLayer] 64: TmpLen: "+timestamp.length+" seqLen: "+seqBin.length+" rndBinLen: "+rndBin.length)
      log.error("[AccessLayer] Binary Error")
    }
    java.lang.Long.parseLong(binary,2)
  }
  def genID48():Long = {
    val timestamp = (System.currentTimeMillis / 1000).toBinaryString
    val rndBin = toBinary(Random.nextInt(32767),15)

    val binary =(timestamp +  rndBin)
    //val binary =( toBinary(sequence) + toBinary(random))

    if(timestamp.length>32 || rndBin.length>16){
      log.error("[AccessLayer] 48: TmpLen: "+timestamp.length+" rndBinLen: "+rndBin.length)
      log.error("[AccessLayer] Binary Error")
    }
    java.lang.Long.parseLong(binary,2)
  }

  def toBinary(i: Int, digits: Int):String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')

}
