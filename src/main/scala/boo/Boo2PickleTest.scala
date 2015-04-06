package boo

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.typedarray.{ArrayBuffer => TArrayBuffer}
import scala.scalajs.js.typedarray._

class Boo2PickleTest extends TestCase {

  def encode(data: Seq[TestData]): TArrayBuffer = {
    val buf = new TArrayBuffer(100000)
    val bb = new DataView(buf)
    var ofs = 0

    def encodeString(str: String): Unit = {
      bb.setInt32(ofs, str.length)
      ofs += 4

      str.getBytes("UTF-8").foreach(b => {
        bb.setInt8(ofs, b)
        ofs += 1
      })
    }
    bb.setInt32(ofs, data.size)
    ofs += 4
    data.foreach { d =>
      encodeString(d.id)
      bb.setInt32(ofs, d.counter)
      ofs += 4
      bb.setInt32(ofs, d.events.size)
      ofs += 4
      d.events.foreach { e =>
        encodeString(e.name)
        bb.setInt32(ofs, e.timeStamp)
        ofs += 4
        bb.setUint8(ofs, if (e.isFatal) 1 else 0)
        ofs += 1
      }
    }
    buf.slice(0, ofs)
  }

  def decode(buf: TArrayBuffer): Seq[TestData] = {
    var ofs = 0
    val data = new DataView(buf)
    val data8 = new Int8Array(buf)
    val len = data.getInt32(ofs)
    ofs += 4
    // println(s"Decoded size $len")
    val td = new ArrayBuffer[TestData](len)

    def decodeString: String = {
      val len = data.getInt32(ofs)
      ofs += 4
      val strBytes = new Array[Byte](len)
      for(i <-0 until len) {
        strBytes(i) = data8.get(ofs + i)
      }

      ofs += len
      new String(strBytes, "UTF-8")
    }

    var idx = 0
    while (idx < len) {
      val id = decodeString
      // println(s"id = $id")
      val counter = data.getInt32(ofs)
      ofs += 4
      val eventCount = data.getInt32(ofs)
      ofs += 4
      val events = new ArrayBuffer[TestEvent](eventCount)
      var eIdx = 0
      while (eIdx < eventCount) {
        val name = decodeString
        val timeStamp = data.getInt32(ofs)
        ofs += 4
        val isFatal = data.getInt8(ofs)
        ofs += 1
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
    val encSize = encoded.byteLength
    println(s"Encoded size $encSize")
    val start = new scala.scalajs.js.Date().getTime()

    var tmp = 0
    for (i <- 0 until count) {
      val decoded = decode(encoded)
      // use the decoded data for something to prevent it being optimized away
      tmp += decoded.size
    }

    val time = new scala.scalajs.js.Date().getTime() - start + (tmp min 0)
    TestResult(time, encSize, decode(encoded))
  }
}

