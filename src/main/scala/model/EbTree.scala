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
  var myRoot: Option[Node[T]] = None


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

  /**
   * @return The size of the tree
   */
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
      case _ => println("findleaf no leaf o Node found super strange"); None //TODO delete
    }
    if (myRoot.isDefined) findLeafRec(myRoot.get) else None
  }


  def put(uid: Long, payload: T): Option[T] = findLeaf(uid) match {
    case Some(leaf: Leaf[T]) => {
      if (leaf.myUid == uid) {
        // leaf al ready exists alter payload
        val oldPayload: T = leaf.myPayload
        leaf.myPayload = payload
        Some(oldPayload)
      } else {
        val xored: Long = uid ^ leaf.myUid // findet hoechstes unterscheidungsbit
        val bit = 63 - java.lang.Long.numberOfLeadingZeros(xored) // ergibt höchste
        var parent = leaf.myParent
        while (parent.myBit < bit) parent = parent.myParent

        val node = Node[T](bit)
        node.setChild(uid, Leaf[T](uid, payload))

        node.setChild(~uid, parent.getChild(uid))
        parent.setChild(uid, node)
        mySize += 1
        None
      }
    }
    case _ => {
      myRoot = Some(Node[T](64)) //defines first root node of the EbTree
      myRoot.get.setChild(uid, Leaf[T](uid, payload)); //append leaf with payload to the root node
      mySize += 1
      None
    }
    //TODO should be null
  }


  /**
   * Returns the payload of a leaf to a given uid
   * @param uid the uid of the leaf
   * @return payload of the leaf or None if the leaf could not been found
   */
  def get(uid: Long): Option[T] = findLeaf(uid).get match {
    case leaf: Leaf[T] if (leaf.myUid == uid) => Some(leaf.myPayload)
    case _ => None //TODO should be null
  }

  /**
   * Removes a leaf to a given ui. If the leaf is found the sibling leaf is pended to the place of the parent node
   * @param uid The uid of the leaf which should be removed
   * @return The payload of the leaf deleted leaf or None if the leaf could not been found
   */
  def remove(uid: Long): Option[T] = findLeaf(uid).get match {
    case leaf: Leaf[T] if (leaf.myUid == uid) => {
      val node = leaf.myParent
      val parent = node.myParent
      if (parent == None) myRoot = None //TODO check working
      else {
        parent.setChild(uid, node.getChild(~uid))
      }
      mySize -= 1
      Some(leaf.myPayload)
    }
    case _ => None //TODO should be null
  }


  def firstKey(): Long = {
    if (myRoot.isEmpty) -1 // here we have the problem, that if -1 is the only single key, it will not work
    findLeaf(0L).get.myUid
  }

  def lastKey(): Long = {
    if (myRoot.isEmpty) 0 // here we have the problem, that if 0 is the only single key, it will not work
    findLeaf(-1L).get.myUid
  }

  def snapUp(uid: Long): Long = findLeaf(uid) match {
    case Some(leaf: Leaf[T]) => {
      if (leaf.myUid != uid) {
        var child: Child[T] = null
        var parent: Node[T] = leaf.myParent

        // 1. go up tireturnll highest difference bit level
        val bit: Int = 63 - java.lang.Long.numberOfLeadingZeros(uid ^ leaf.myUid)

        while (parent.myBit < bit) parent = parent.myParent

        if ((uid & (1L << bit)) != 0) {
          while (parent.bitOne(uid)) parent = parent.myParent
          child = parent.myOne
          if (child == null) return 0
        }
        else {
          child = parent.getChild(uid)
        }

        // 3. go down, keep left
        while (child.isInstanceOf[Node[_]]) {
          child = (child.asInstanceOf[Node[T]]).myZero
        }

        child.asInstanceOf[Leaf[T]].myUid
      } else {
        leaf.myUid
      }
    }
    case _ => 0 //Tree empty
  }

  def snapDown(uid: Long): Long = findLeaf(uid) match {
    case Some(leaf:Leaf[T]) => {
      if (leaf.myUid != uid) {
        var child: Child[T] = null
        var parent: Node[T] = leaf.myParent
        val bit: Int = 63 - java.lang.Long.numberOfLeadingZeros(uid ^ leaf.myUid)
        while (parent.myBit < bit) parent = parent.myParent
        if ((uid & (1L << bit)) == 0) {
          while (!parent.bitOne(uid)) {
            parent = parent.myParent
            if (parent == null) return -1
          }
          child = parent.myZero
        }
        else {
          child = parent.getChild(uid)
        }
        while (child.isInstanceOf[Node[_]]) {
          child = (child.asInstanceOf[Node[T]]).myOne
        }
        child.asInstanceOf[Leaf[T]].myUid
      }else{
        leaf.myUid
      }
    }
    case _ => 0
  }

  def next(uid: Long): Long = {
    if (uid == -1)  0
     snapUp(uid + 1)
  }
  def prev(uid:Long):Long ={
    if (uid == 0)  -1
    snapDown(uid - 1)
  }

  def getDelta(){

  }


  //TODO
  // 1. understand bit magic
  // 2. get delta
  // 3. xor upwoards
  //
}




