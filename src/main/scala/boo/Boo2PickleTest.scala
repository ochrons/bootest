package boo

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer => TArrayBuffer, _}

class TextDecoder extends js.Object {
  def decode(data: js.Any): String = js.native
}

class Boo2PickleTest extends TestCase {

  def encode(data: Seq[TestData]): TArrayBuffer = {
    val buf = new TArrayBuffer(100000)
    val bb = new DataView(buf)
    var ofs = 0

    def encodeString(str: String): Unit = {
      val bytes = str.getBytes("UTF-8")
      bb.setInt32(ofs, bytes.length)
      ofs += 4

      bytes.foreach(b => {
        bb.setInt8(ofs, b)
        ofs += 1
      })
    }

    /**
     * Encodes an integer efficiently
     * <pre>
     *   0XXX XXXX                                             = 0 to 127
     *   1000 XXXX  XXXX XXXX                                  = 128 to 4095
     *   1001 XXXX  XXXX XXXX                                  = -1 to -4096
     *   1010 XXXX  XXXX XXXX  XXXX XXXX                       = 4096 to 1048575
     *   1011 XXXX  XXXX XXXX  XXXX XXXX                       = -4097 to -1048576
     *   1100 XXXX  XXXX XXXX  XXXX XXXX  XXXX XXXX            = 1048575 to 268435455
     *   1101 XXXX  XXXX XXXX  XXXX XXXX  XXXX XXXX            = -1048576 to -268435456
     *   1110 0000  XXXX XXXX  XXXX XXXX  XXXX XXXX  XXXX XXXX = anything else
     * </pre>
     * @param i
     */
    def encodeInt(i: Int): Unit = {
      // check for a short number
      if (i >= 0) {
        if (i < 128) {
          bb.setInt8(ofs, i.toByte)
          ofs += 1
        } else if (i < 4096) {
          bb.setUint16(ofs, (0x8000 | i).toShort)
          ofs += 2
        } else if (i < 1048575) {
          bb.setUint16(ofs, (0xA000 | (i >> 8)).toShort)
          ofs += 2
          bb.setInt8(ofs, (i & 0xFF).toByte)
          ofs += 1
        } else if (i < 268435455) {
          bb.setUint32(ofs, 0xC0000000 | i)
          ofs += 4
        } else {
          bb.setInt8(ofs, 0xE0.toByte)
          ofs += 1
          bb.setUint32(ofs, i)
          ofs += 4
        }
      } else {
        if (i >= -4096) {
          bb.setUint16(ofs, 0x9000 | (i & 0x07FF).toShort)
          ofs += 2
        } else if (i >= -1048576) {
          bb.setUint16(ofs, (0xB000 | ((i >> 8) & 0x07FFF)).toShort)
          ofs += 2
          bb.setInt8(ofs, (i & 0xFF).toByte)
          ofs += 1
        } else if (i >= -268435456) {
          bb.setUint32(ofs, 0xD0000000 | (i & 0x07FFFFFF))
          ofs += 4
        } else {
          bb.setInt8(ofs, 0xE0.toByte)
          ofs += 1
          bb.setInt32(ofs, i)
          ofs += 4
        }
      }
    }

    encodeInt(data.size)
    data.foreach { d =>
      encodeString(d.id)
      encodeInt(d.counter)
      encodeInt(d.events.size)
      d.events.foreach { e =>
        encodeString(e.name)
        encodeInt(e.timeStamp)
        bb.setUint8(ofs, if (e.isFatal) 1 else 0)
        ofs += 1
      }
    }
    buf.slice(0, ofs)
  }

  val utf8decoder = new TextDecoder

  def decode(buf: TArrayBuffer): Seq[TestData] = {
    var ofs = 0
    val data = new DataView(buf)
    val data8 = new Int8Array(buf)
    def decodeString: String = {
      val len = data.getInt32(ofs)
      ofs += 4
      val s = utf8decoder.decode(data8.subarray(ofs, ofs + len))
      ofs += len
      s
    }

    def decodeInt: Int = {
      val b = data.getUint8(ofs)
      ofs += 1
      if( (b & 0x80) != 0 ) {
        // special coding, expand sign bit
        val b0 = b & 0xF | (b << 27 >> 27)
        (b >> 4) & ~1 match {
          case 0x8 =>
            val b1 = data.getUint8(ofs)
            ofs += 1
            b0 << 8 | b1
          case 0xA =>
            val b1 = data.getUint16(ofs)
            ofs += 2
            b0 << 16 | b1
          case 0xC =>
            val b1 = data.getInt32(ofs-1) & 0x00FFFFFF
            ofs += 3
            b0 << 24 | b1
          case 0xE =>
            val b1 = data.getInt32(ofs)
            ofs += 4
            b1
        }
      } else {
        b
      }
    }
    val len = decodeInt
    // println(s"Decoded size $len")
    val td = new ArrayBuffer[TestData](len)

    var idx = 0
    while (idx < len) {
      val id = decodeString
      // println(s"id = $id")
      val counter = decodeInt
      val eventCount = decodeInt
      val events = new ArrayBuffer[TestEvent](eventCount)
      var eIdx = 0
      while (eIdx < eventCount) {
        val name = decodeString
        val timeStamp = decodeInt
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

