package nodescala

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }
  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  test("A list of Futures should be all complete or fail") {
    val l = List[Future[Int]](Future { 1 }, Future { 2 }, Future { 3 })
    val all = Future.all(l)

    assert(Await.result(all, 250 millis) == List(1, 2, 3))
  }
  test("A list of Futures should fail if one fails") {
    val all = Future.all(List(Future { 1 }, Future { 2 }, Future { throw new Exception }))

    try {
      Await.result(all, 250 millis)
      assert(false)
    } catch {
      case t: Exception => // ok!
    }
  }
  test("A list of Futures should return any result") {
    val any = Future.any(List(Future { 1 }, Future { 2 }, Future { throw new Exception }))

    try {
      assert(List(1, 2).contains(Await.result(any, 100 millis)))
    } catch {
      case t: Exception => // ok!
    }
  }
  test("A delay should happen for t seconds") {
    val delay = Future.delay(1000 millis)
    Thread.sleep(500)
    assert(delay.isCompleted == false)
  }

  test("Now should return the value of an immediate result") {
    val f = Future { 3 }
    assert(f.now == 3)
  }
  test("Now should throw an exception of a non-immediate result") {
    val f = Future { Thread.sleep(100); 3 }
    try {
      f.now
      assert(false)
    } catch {
      case e: NoSuchElementException => // ok!
    }
  }
  test("ContinueWith should concatenate two futures") {
    val f1 = Future { 3 }
    val f2 = Future { 2 }
    val f3 = f1.continueWith(f2 => { "test" })
    assert(Await.result(f3, 100 millis) == "test")
  }
  test("Continue should concatenate the result of a future onto another future") {
    val f1 = Future { 3 }
    val t = Try(2)
    val f2 = f1.continue(t => { "test" })
    assert(Await.result(f2, 500 millis) == "test")
  }
  test("Run should allow a computation to be cancelled") {
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          println("working")
        }
        println("done")
        assert(true)
      }
    }
    Future.delay(250 nanos) onSuccess {
      case _ => working.unsubscribe()
    }
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }
  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }
}
