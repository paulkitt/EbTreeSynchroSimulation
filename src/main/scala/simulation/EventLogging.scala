package simulation

import scala.collection.mutable.Map

/**
 * Created by prototyp on 20.08.14.
 */
object EventLogging {

  var events:Map[String,Int] = Map()

  def addEvent(event:String) = events.remove(event) match{
    case Some(x:Int)  => events += (event -> (x+1))
    case None         => events += (event -> 1)
  }

  def getEvent(event:String):Option[Int] = events.get(event)

  def getAllAsTuples() = ???
}
