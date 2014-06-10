package src.main.scala.model

/**
 * Created by prototyp on 24.05.14.
 */
/**
 *
 * @tparam T
 */
//TODO check if val should be var and vise versa

class EbTree[T] {
  var mySize: Int = _
  var myRoot: Node[T] = _


  class Child[T] {
    var myParent: Node[T] = _
  }

  /**
   * Class which represents a Node in the Tree. The Node has a parentNode inherited from Child and hold the references of two Nodes or Leaf
   * @param myBit The binary address of the Node
   * @tparam T The data type the tree is holding
   */
  case class Node[T](myBit: Int) extends Child[T] {

    var myZero: Child[T] = _
    var myOne: Child[T] = _

    //TODO understand/debug this funktion
    def bitOne(uid: Long): Boolean = ((uid & (1L << myBit)) != 0) && (myBit < 64)

    def getChild(uid: Long): Child[T] = bitOne(uid) match {
      case true => myOne;
      case false => myZero
    }

    def setChild(uid: Long, child: Child[T]) = {
      bitOne(uid) match {
        case true => myOne = child
        case false => myZero = child
      }
      child.myParent = this
    }

    override def toString(): String = "[" + myZero + "," + myOne + "]"
  }

  /**
   *
   * @param myUid
   * @param myPayload
   * @tparam T
   */
  case class Leaf[T](myUid: Long, var myPayload: T) extends Child[T] {
    override def toString(): String = "<" + myUid + ":" + myPayload + ">"
  }

  def size(): Int = mySize

  /**
   * Searches a leaf to a given uid
   * @param uid the binary address of the Leaf
   * @return the requested leaf if the leaf is present or None if the leaf is not present
   */
  def findLeaf(uid: Long): Option[Leaf[T]] = {
    def findLeafRec(cursor: Child[T]): Option[Leaf[T]] = cursor match {
      case x: Leaf[T] => Some(x)
      case y: Node[T] => findLeafRec(y.getChild(uid))
      case _ => None
    }
    findLeafRec(myRoot)
  }


  def put(uid: Long, payload: T): Option[T] = findLeaf(uid) match {
    //Tree is empty?
    case None => {
      myRoot = Node[T](64) //defines first root node of the EbTree
      myRoot.setChild(uid, Leaf[T](uid, payload)); //append leaf with payload to the root node
      mySize +=1
      None
    }
    case leaf: Leaf[T] => {
      if (leaf.myUid == uid) { // leaf al ready exists alter payload
        val oldPayload: T = leaf.myPayload
        leaf.myPayload = payload
        Some(oldPayload)
      } else {
        //TODO append leaf to tree
        None
      }
    }
    case _ => None
  }

  def get(uid: Long): Option[T] = findLeaf(uid) match {
    case leaf: Leaf[T] if (leaf.myUid == uid) => Some(leaf.myPayload)
    case _ => None
  }

  def remove(uid:Long):Option[T] = findLeaf(uid) match {
    case leaf: Leaf[T] if(leaf.myUid == uid) => {
      val node = leaf.myParent
      val parent = node.myParent
      if(parent == None) myRoot = _ //TODO check working
      else{
        parent.setChild(uid,node.getChild(~uid))
      }
      mySize -= 1
      Some(leaf.myPayload)
    }
    case _ => None

  }


}




