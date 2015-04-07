package boo

import java.util.UUID

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
    val strCache = new ArrayBuffer[String](16)
    // empty string is always at location 0
    strCache.append("")
    var ofs = 0

    /**
     * Encodes a string using UTF-8 encoding. Supports caching of strings and special coding for certain patterns.
     *
     * When length is negative (or zero), it indicates an index to cache.
     *
     * If first byte is 1111 ????, then special codings are applied.
     * <pre>
     * 1111 0000 i0 i1 i2 i3                     = 128-bit lowercase UUID
     * 1111 0001 i0 i1 i2 i3                     = 128-bit UPPERCASE UUID
     * @param str
     */
    def encodeString(str: String): Unit = {
      // check if it is in the cache already
      val idx = strCache.indexOf(str)
      if (idx >= 0) {
        // use negative length to encode reference to cache, empty string gets encoded as 0-length
        encodeInt(-idx)
      } else if (str.length == 36 && str.matches("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}")) {
        strCache.append(str)
        // special coding for UUID
        // TODO: scala.js has a bug in Long hex parsing (#1582) and is missing Integer.parseUnsignedInt, so hacking it in two pieces
        val nums = str.replace("-", "").sliding(8, 8).map(s => Integer.parseInt(s.take(4), 16) << 16 | Integer.parseInt(s.drop(4), 16) )
        bb.setInt8(ofs, 0xF0.toByte)
        ofs += 1
        nums.foreach { n =>
          bb.setInt32(ofs, n)
          ofs += 4
        }
      } else {
        // only cache relatively short strings
        if (str.length < 64)
          strCache.append(str)
        val bytes = str.getBytes("UTF-8")
        encodeInt(bytes.length)

        bytes.foreach(b => {
          bb.setInt8(ofs, b)
          ofs += 1
        })
      }
    }

    /**
     * Encodes an integer efficiently
     * <pre>
     * 0XXX XXXX                                             = 0 to 127
     * 1000 XXXX  XXXX XXXX                                  = 128 to 4095
     * 1001 XXXX  XXXX XXXX                                  = -1 to -4096
     * 1010 XXXX  XXXX XXXX  XXXX XXXX                       = 4096 to 1048575
     * 1011 XXXX  XXXX XXXX  XXXX XXXX                       = -4097 to -1048576
     * 1100 XXXX  XXXX XXXX  XXXX XXXX  XXXX XXXX            = 1048575 to 268435455
     * 1101 XXXX  XXXX XXXX  XXXX XXXX  XXXX XXXX            = -1048576 to -268435456
     * 1110 0000  XXXX XXXX  XXXX XXXX  XXXX XXXX  XXXX XXXX = anything else
     * 1111 ????                                             = reserved for special codings
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
          bb.setInt32(ofs, 0xC0000000 | i)
          ofs += 4
        } else {
          bb.setInt8(ofs, 0xE0.toByte)
          ofs += 1
          bb.setInt32(ofs, i)
          ofs += 4
        }
      } else {
        if (i >= -4096) {
          bb.setUint16(ofs, 0x9000 | (i & 0x0FFF).toShort)
          ofs += 2
        } else if (i >= -1048576) {
          bb.setUint16(ofs, (0xB000 | ((i >> 8) & 0x0FFFF)).toShort)
          ofs += 2
          bb.setInt8(ofs, (i & 0xFF).toByte)
          ofs += 1
        } else if (i >= -268435456) {
          bb.setInt32(ofs, 0xD0000000 | (i & 0x0FFFFFFF))
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
    val data8 = new Uint8Array(buf)
    val strCache = new ArrayBuffer[String](16)
    strCache.append("")

    def decodeInt: Int = {
      val b = data.getUint8(ofs)
      ofs += 1
      if ((b & 0x80) != 0) {
        // special coding, expand sign bit
        val b0 = b & 0xF | (b << 27 >> 27)
        b >> 4 match {
          case 0x8 | 0x9 =>
            val b1 = data.getUint8(ofs)
            ofs += 1
            b0 << 8 | b1
          case 0xA | 0xB =>
            val b1 = data.getUint16(ofs)
            ofs += 2
            b0 << 16 | b1
          case 0xC | 0xD =>
            val b1 = data.getInt32(ofs - 1) & 0x00FFFFFF
            ofs += 3
            b0 << 24 | b1
          case 0xE =>
            val b1 = data.getInt32(ofs)
            ofs += 4
            b1
          case _ =>
            throw new IllegalArgumentException("Unknown integer coding")
        }
      } else {
        b
      }
    }

    def decodeIntExt: Either[Int, Int] = {
      val b = data.getUint8(ofs)
      ofs += 1
      if ((b & 0x80) != 0) {
        // special coding, expand sign bit
        val b0 = b & 0xF | (b << 27 >> 27)
        b >> 4 match {
          case 0x8 | 0x9 =>
            val b1 = data.getUint8(ofs)
            ofs += 1
            Left(b0 << 8 | b1)
          case 0xA | 0xB =>
            val b1 = data.getUint16(ofs)
            ofs += 2
            Left(b0 << 16 | b1)
          case 0xC | 0xD =>
            val b1 = data.getInt32(ofs - 1) & 0x00FFFFFF
            ofs += 3
            Left(b0 << 24 | b1)
          case 0xE =>
            val b1 = data.getInt32(ofs)
            ofs += 4
            Left(b1)
          case 0xF =>
            Right(b)
        }
      } else {
        Left(b)
      }
    }

    def decodeString: String = {
      decodeIntExt match {
        case Left(strLen) if strLen <= 0 =>
          strCache(-strLen)
        case Left(strLen) =>
          val s = utf8decoder.decode(data8.subarray(ofs, ofs + strLen))
          ofs += strLen
          if (s.length < 64)
            strCache.append(s)
          s

        case Right(code) if code == 0xF0 =>
          // read 128 bits
          val i0 = data.getInt32(ofs)
          val i1 = data.getInt32(ofs+4)
          val i2 = data.getInt32(ofs+8)
          val i3 = data.getInt32(ofs+12)
          ofs += 16
          // convert to UUID format
          @inline def paddedHex8(i: Int): String = {
            val s = Integer.toHexString(i)
            "00000000".substring(s.length) + s
          }

          @inline def paddedHex4(i: Int): String = {
            val s = Integer.toHexString(i)
            "0000".substring(s.length) + s
          }

          val s = paddedHex8(i0) + "-" + paddedHex4(i1 >>> 16) + "-" + paddedHex4(i1 & 0xffff) + "-" +
            paddedHex4(i2 >>> 16) + "-" + paddedHex4(i2 & 0xffff) + paddedHex8(i3)
          strCache.append(s)
          s
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

  def toHex(buf: TArrayBuffer): String = {
    val sb = new StringBuilder
    val v = new Uint8Array(buf)
    for (i <- 0 until buf.byteLength) {
      sb.append("%02x ".format(v.get(i)))
    }
    sb.toString()
  }

  override def run(data: Seq[TestData], count: Int): TestResult = {
    val encoded = encode(data)
    val encSize = encoded.byteLength
    println(toHex(encoded))
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

