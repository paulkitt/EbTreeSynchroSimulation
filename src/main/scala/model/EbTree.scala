package src.main.scala.model

import org.slf4j.LoggerFactory
import simulation.EventLogging


/**
 * Created by prototyp on 24.05.14.
 */

//TODO check if val should be var and vise versa

// inner classes an trait
//------------------------------------------------------------------------------------

trait NodeIf {
def  isLeaf():Boolean
def  getLeft():NodeIf
def  getRight():NodeIf
def getLabel():(String,String)
}

trait Child[T] extends NodeIf{
  var myParent: Node[T] = _
  def getID():Long
  def  isLeaf():Boolean = this match{
    case l:Leaf[T] =>   true
    case _         =>   false
  }
  def  getLeft():NodeIf ={
    if(!isLeaf()){
      return this.asInstanceOf[Node[T]].myZero
    }else{
      return null
    }
  }
  def  getRight():NodeIf= {
    if(!isLeaf()){
      return this.asInstanceOf[Node[T]].myOne
    }else{
      return null
    }
  }
  def getLabel():(String,String) = this match{
    case n:Node[T] => (n.myBit.toString,n.nodeChangeID.toString)
    case l:Leaf[T] => (l.myUid.toString,"")
  }
}


/**
 * Class which represents a Node in the Tree. The Node has a parentNode inherited from Child and hold the references of two Nodes or Leaf
 * @param myBit The binary address of the Node
 * @tparam T The data type the tree is holding
 */
case class Node[T](myBit: Int) extends Child[T] {
  var myZero:       Child[T]  = _
  var myOne:        Child[T]  = _
  var nodeChangeID: Long      = _

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

  override def getID(): Long = nodeChangeID
}

/**
 *
 * @param myUid
 * @param myPayload
 * @tparam T
 */
case class Leaf[T](myUid: Long, var myPayload: T) extends Child[T] {
  override def toString(): String = "<" + myUid + ":" + myPayload + ">"

  override def getID(): Long = myUid
}

object Delta{
  def apply[T](aid:Long, afrom:Int, child:Child[T]):Delta = child match{
    case n:Node[T] => Delta(aid,afrom,n.myBit,n.myZero.getID,n.myOne.getID)
    case l:Leaf[T] => Delta(aid,afrom,-1,l.getID(),l.getID())
  }
}
case class Delta(id:Long,from:Int,to:Int,l:Long,r:Long){
  def isLeaf():Boolean = {
    if( to == -1 && l==r) true
    else false
  }
  def isFinalLeaf():Boolean = {
    if(id == -1 && from == -1 && to == -1 && l==r) true
    else false
  }
}


class EbTree[T] {
  var mySize: Int = 0
  var myRoot: Option[Node[T]] = None
  val log = LoggerFactory.getLogger(classOf[EbTree[T]])

//Basic tree operations
//------------------------------------------------------------------------------------
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
      case _ => log.warn("findleaf no leaf o Node found super strange"); None //TODO delete
    }
    if (myRoot.isDefined) findLeafRec(myRoot.get) else None
  }


  def put(uid: Long, payload: T): Option[T] = findLeaf(uid) match {
    case Some(leaf: Leaf[T]) =>
      if (leaf.myUid == uid) {
        // leaf al ready exists alter payload
        val oldPayload: T = leaf.myPayload
        leaf.myPayload = payload
        //calculateNodeIDs(leaf.myParent)//TODO remove
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
        calculateNodeIDs(node)
        mySize += 1
        None
      }
    case _ =>
      myRoot = Some(Node[T](64)) //defines first root node of the EbTree
      myRoot.get.setChild(uid, Leaf[T](uid, payload)); //append leaf with payload to the root node
      mySize += 1
      None
  }

  /**
   * Returns the payload of a leaf to a given uid
   * @param uid the uid of the leaf
   * @return payload of the leaf or None if the leaf could not been found
   */
  def get(uid: Long): Option[T] = findLeaf(uid).get match {
    case leaf: Leaf[T] if (leaf.myUid == uid) => Some(leaf.myPayload)
    case _ => None
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
      calculateNodeIDs(parent)
      mySize -= 1
      Some(leaf.myPayload)
    }
    case _ => log.error("Tree.remove : No leaf to remove Found!");None //TODO should be null
  }
//Synchronisation
//------------------------------------------------------------------------------------
  def calculateNodeIDs(node:Node[T]):Unit = node.myBit match{
    case 64 => //log.info("(calculateNodeIDs) reached RootNode")
    case _  =>{
      node.nodeChangeID = node.myZero.getID()^node.myOne.getID()
      calculateNodeIDs(node.myParent)
    }
  }
//--------------------------------------------------------------------------------------


  def nextDelta(t:Delta):Delta = {
    if(mySize==0 || mySize==1) return null
    var node:Node[T] = null
    getDeltaByID(myRoot.get.myZero) match{
      case Some(d:Delta) =>  return d
      case _ =>
    }
    def getDeltaByID(cursor:Child[T]):Option[Delta] = cursor match {
      case n:Node[T] if(n.myBit < t.from) => node = n;None
      case n:Node[T] => getDeltaByID(n.getChild(t.id))
      case l:Leaf[T] => Some(Delta(t.id,t.from,cursor)) //Some(Delta(-1,-1,cursor))////=> unique leaf
      case _ => log.warn("getDeltaByID ERROR"); None
    }

    if(t.l == -1 && t.r == -1){ //Optimierung hier abkuerzen -> jump one one over lvl gap, possible loops
      var nextNode:Delta = Delta(t.id,node.myBit,node.myZero)
//      if(nextNode.to < t.to && node.myBit > t.to){ //check if leaf?? // Buggy optimisation
//        nextNode = nextDelta(Delta(t.id,0,0,-1,-1))
//        nextNode = Delta(nextNode.id,node.myBit,-1,nextNode.l,nextNode.r)
//      }
      return nextNode
    }

    if (node.myBit == t.to) { // same bit lvl diff childs
      if (node.myZero.getID() != t.l) {
        return Delta(t.id, t.to, node.myZero) // A)
      } else if(node.myOne.getID() != t.r) {
        return Delta(t.id+(1L<<node.myBit), t.to, node.myOne) // B)
      }else{ // trees synchronized
        log.info("[EbTree] trees synchronized! BitLvLs: " + node.myBit + "/" + t.to +
          "from: "+node.myParent.myBit + "/" + t.from +
          " left: "+node.myZero.getID() + "/" + t.l +
          " right: " +node.myOne.getID() + "/" + t.r)
        //check ob oberster Knoten sonst log error
        if(node.myParent.myBit==64){
          return null
        }else{
          EventLogging.addEvent("[EbTree] Sync Error: "+ node.myBit + "/" + t.to +
            "from: "+node.myParent.myBit + "/" + t.from +
            " left: "+node.myZero.getID() + "/" + t.l +
            " right: " +node.myOne.getID() + "/" + t.r)
         return null
        }
      }
    }
    else if (node.myBit > t.to) {        // flip sides
      return  Delta(t.id, t.from, node); // F) (0000,1,3,l,r)
    }
    else { // node.myBit < t.to
      if (t.l == node.getID()) return new Delta(t.id+(1L<<t.to), 0 , 0, -1, -1) // D) => found triangle go right take most left leaf
      return  Delta(t.id, t.from, node.myBit, -1, -1) //E => request next left triangle
    }
  }
  def checkIfLostLeafIsFound(t:Delta):Delta = {
    val leaf = findLeaf(t.l)
    if(leaf.get.getID()==t.l){ // leaf found search for sub tree
      lazy val node:Node[T] = getNodeAboveLeaf(myRoot.get.myZero)
      def getNodeAboveLeaf(cursor:Child[T]):Node[T] = cursor match {
        case n:Node[T] if(n.myBit < t.from) => n
        case n:Node[T] => getNodeAboveLeaf(n.getChild(t.id))
        case l:Leaf[T] => l.myParent
      }
      var leaf = getMostLeftLeaf(node)
      if(leaf.getID()==t.l){
        leaf = findLeaf(next(leaf.getID())).get
      }
      return Delta(-1,-1,leaf)
    }else{ // request leaf
      return Delta(-1,-1,-1,t.l,t.r)
    }
  }

  def getMostLeftLeaf(cursor:Child[T]):Leaf[T] = cursor match {
    case n:Node[T] => getMostLeftLeaf(n.myZero)
    case l:Leaf[T] => l
  }

//Move inside the tree
//------------------------------------------------------------------------------------
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
        var child: Child[_] = null
        var parent: Node[_] = leaf.myParent

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
        var child: Child[_] = null
        var parent: Node[_] = leaf.myParent
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
}



