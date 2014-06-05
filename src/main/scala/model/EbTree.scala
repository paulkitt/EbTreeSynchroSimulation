package src.main.scala.model

/**
 * Created by prototyp on 24.05.14.
 */

//TODO check if val should be var and vise versa

class EbTree[T] {
  var mySize:Int  = _
  var myRoot:Node[T] = _



  class Child[T]{
    var myParent:Node[T] = _
  }

  case class Node[T](myBit:Int) extends Child[T]{

    var myZero:Child[T]   = _
    var myOne:Child[T]    = _

    //TODO understand/debug this funktion
    def bitOne(uid:Long):Boolean = ((uid & (1L << myBit)) != 0) && (myBit < 64)

    def getChild(uid:Long):Child[T] =  bitOne(uid) match{
      case true   => myOne;
      case false  => myZero
    }

    def setChild(uid:Long,child:Child[T]) ={
      bitOne(uid) match {
        case true   => myOne = child
        case false  => myZero = child
      }
      child.myParent=this
    }

    override def toString():String = "[" + myZero + "," + myOne +"]"
  }
  case class Leaf[T](myUid:Long,myPayload:T) extends Child[T]{
    override def toString():String = "<" + myUid + ":" + myPayload +">"
  }

  def size():Int = mySize

  def findLeaf(uid:Long):Leaf[T] = {
    def findLeafRec(cursor:Child[T]):Leaf[T] = cursor match {
      case x:Leaf[T] => x
      case y:Node[T] => findLeafRec(y.getChild(uid))
    }
    findLeafRec(myRoot)
  }

  def put()

}




