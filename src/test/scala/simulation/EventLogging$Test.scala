package simulation

import org.scalatest.FunSuite

/**
 * Created by prototyp on 20.08.14.
 */
class EventLogging$Test extends FunSuite {

  test("EventLogging add Event"){
    EventLogging.addEvent("Event Test")

    assert(EventLogging.events.size==1 && EventLogging.events.get("Event Test")==Some(1))

    EventLogging.addEvent("Event Test")
    assert(EventLogging.events.size==1 && EventLogging.events.get("Event Test")==Some(2))

    EventLogging.addEvent("Another Event Test")
    assert(EventLogging.events.size==2 && EventLogging.events.get("Event Test")==Some(2))
  }


}
