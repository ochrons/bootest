package boo

import upickle._

import scala.scalajs.js.{JSON, Date}

class UPickleTest extends TestCase {
  override def run(data: Seq[TestData], count: Int): TestResult = {
    val encoded = write(data)
    val encSize = encoded.length

    val start = new Date().getTime()

    var tmp = 0
    for(i <- 0 until count) {
      val decoded = read[Seq[TestData]](encoded)
      // val decoded = JSON.parse(encoded) // just run JSON parse

      // use the decoded data for something to prevent it being optimized away
      // tmp += decoded.length.asInstanceOf[Int]
      tmp += decoded.size
    }

    val time = new Date().getTime() - start + (tmp min 0)
    TestResult(time, encSize, read[Seq[TestData]](encoded))
    // TestResult(time, encSize, data)
  }
}
