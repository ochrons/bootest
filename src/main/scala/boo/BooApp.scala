package boo

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

case class TestResult(time: Double, size: Int, data: Seq[TestData])

trait TestCase {
  def run(data: Seq[TestData], count: Int): TestResult
}

@JSExport("BooApp")
object BooApp extends js.JSApp {

  @JSExport
  def main(): Unit = {
    val upt = new UPickleTest
    val bpt = new BooPickleTest
    val b2pt = new Boo2PickleTest
    val body = dom.document.body
    val runButton = button("Run tests").render
    val resultDiv = div().render
    body.appendChild(runButton)
    body.appendChild(resultDiv)

    val testData = genTestData
    println(testData)
    runButton.onclick = (e: dom.Event) => {
      // run tests

      val count = 1000
      val booResult = bpt.run(testData, count)
      assert(booResult.data == testData)
      val boo2Result = b2pt.run(testData, count)
      assert(boo2Result.data == testData)
      val upResult = upt.run(testData, count)
      assert(upResult.data == testData)

      resultDiv.innerHTML = ""
      val results = div(
        h2("Results"),
        h4("uPickle"), s"Time: ${upResult.time}", br(), s"Size: ${upResult.size}",
        h4("booPickle"), s"Time: ${booResult.time}", br(), s"Size: ${booResult.size}",
        h4("booPickle (TypedArray, TextDecoder)"), s"Time: ${boo2Result.time}", br(), s"Size: ${boo2Result.size}"
      ).render
      resultDiv.appendChild(results)
    }
  }

  def genTestData = {
    val count = 100
    val eventNames = Seq("MouseUp", "MouseDown", "MouseMove")
    for (i <- 0 until count) yield {
      TestData(s"${i * 1234}äåö" * 40, (i * 7) % 100, Range(1, i % 5 + 1).map(j => TestEvent(eventNames(j % eventNames.size), i * j * 1000, (i + j) % 5 == 0)))
    }
  }
}
