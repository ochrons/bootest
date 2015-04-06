package boo

case class TestEvent(name:String, timeStamp:Int, isFatal:Boolean)

case class TestData(id:String, counter:Int, events:Seq[TestEvent])
