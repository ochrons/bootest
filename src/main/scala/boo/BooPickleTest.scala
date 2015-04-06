package boo

import java.nio.ByteBuffer

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

class BooPickleTest extends TestCase {

  def encode(data: Seq[TestData]): ByteBuffer = {
    val bb = ByteBuffer.allocateDirect(100000)

    def encodeString(str: String): Unit = {
      val strBytes = str.getBytes("UTF-8")
      bb.putInt(strBytes.length)
      bb.put(strBytes)
    }
    bb.putInt(data.size)
    data.foreach { d =>
      encodeString(d.id)
      bb.putInt(d.counter)
      bb.putInt(d.events.size)
      d.events.foreach { e =>
        encodeString(e.name)
        bb.putInt(e.timeStamp)
        bb.put(if (e.isFatal) 1: Byte else 0: Byte)
      }
    }
    bb.flip.asInstanceOf[ByteBuffer]
  }

  def decode(data: ByteBuffer): Seq[TestData] = {
    val len = data.getInt
    // println(s"Decoded size $len")
    val td = new ArrayBuffer[TestData](len)

    def decodeString: String = {
      val len = data.getInt
      val strBytes = new Array[Byte](len)
      data.get(strBytes)
      new String(strBytes, "UTF-8")
    }

    var idx = 0
    while (idx < len) {
      val id = decodeString
      // println(s"id = $id")
      val counter = data.getInt
      val eventCount = data.getInt
      val events = new ArrayBuffer[TestEvent](eventCount)
      var eIdx = 0
      while (eIdx < eventCount) {
        val name = decodeString
        val timeStamp = data.getInt
        val isFatal = data.get
        events.append(TestEvent(name, timeStamp, isFatal == 1))
        eIdx += 1
      }
      td.append(TestData(id, counter, events))
      idx += 1
    }
    td
  }

  override def run(data: Seq[TestData], count: Int): TestResult = {
    val encoded = encode(data)
    val encSize = encoded.limit()
    println(s"Encoded size $encSize")
    val start = new js.Date().getTime()

    var tmp = 0
    for (i <- 0 until count) {
      val decoded = decode(encoded)
      encoded.rewind()
      // use the decoded data for something to prevent it being optimized away
      tmp += decoded.size
    }

    val time = new js.Date().getTime() - start + (tmp min 0)
    TestResult(time, encSize, decode(encoded))
  }
}

