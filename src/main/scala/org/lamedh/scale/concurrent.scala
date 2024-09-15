package org
package lamedh
package scale

object concurrent {

  import java.util.concurrent.Semaphore
  import scale._

  /**
   * Represents `Future` from stdlib
   * Example:
   * {{{
   * val fut = Fut { Thread.sleep(1000); 5 }
   * val fut2 = fut.map(_ + 1)
   * val fut3 = fut.map(_ + 2)
   * val n6 = fut2.get() // this will block and return 6
   * val n7 = fut3.get() // this won't block anymore, returns 7
   * }}}
    **/
  trait Fut[A] {

    /**
     * Non-blocking way to access the wrapped value, and transform it over function
     * @param f function to transform the value
     * @return new instance of `Fut[B]` with transformed value
    **/
    def map[B](f: A => B): Fut[B]

    def flatMap[B](f: A => Fut[B]): Fut[B]

    def onComplete(f: A => Unit): Unit
    def run(): Unit

    /**
     * Gets the value forcefully. This operation blocks the current thread.
     * @return the wrapped value
    **/
    def get(): A
  }

  /**
   * Mimicks `Future.successful`, wrap over a materialized value
  **/
  case class Done[A](a: A) extends Fut[A] {
    def map[B](f: A => B): Fut[B]          = new Done(f(a))
    def flatMap[B](f: A => Fut[B]): Fut[B] = f(a)
    def onComplete(f: A => Unit): Unit     = f(a)

    def run()    = ()
    def get(): A = a
  }

  /**
   * Put a computation `action` to a thread pool
  **/
  class Spawning[A](action: => A) extends Fut[A] {

    private val executionCtx = scala.concurrent.ExecutionContext.global

    private var completeCallback: Option[A => Unit] = None
    private var result: Option[A]                   = None

    // This must be evaluated inside the execution context or a lazy block
    private lazy val evaluateAction = {
      val act = action
      completeCallback.foreach(f => f(act))
      result = Some(act)
      act
    }

    override def run() = executionCtx.execute(() => evaluateAction)

    override def map[B](f: A => B): Fut[B] = new Spawning[B](f(evaluateAction))

    /**
     * Leaky implementation of flatMap, because it eagerly spawns a computation in the execution context.
     * Below is an example of how flatMap is used:
     * {{{
     * val result = fut.flatMap { a => fut2.map { b => a + b } }
     * }}}
     */
    override def flatMap[B](f: A => Fut[B]): Fut[B] = {
      val promise = new Promis[B]
      executionCtx.execute(() => {
        val fut3 = f(evaluateAction)
        fut3.onComplete { b =>
          promise.complete(b)
        }
        fut3.run()
      })
      promise.future
    }

    override def onComplete(f: A => Unit): Unit = {
      completeCallback = Some(f)
    }

    override def get(): A = {
      result match {
        case Some(value) => value
        case None =>
          val sem = new Semaphore(0)
          executionCtx.execute(() => {
            evaluateAction
            sem.release()
          })
          sem.acquire()
          result.get
      }
    }
  }

  /**
   * Wrap an async operation from another type
  **/
  class Promis[A] {
    private var result: Option[A]           = None
    private var callback: Option[A => Unit] = None
    private var semaphore                   = new Semaphore(0)

    def complete(value: A): Unit = {
      result = Some(value)
      callback.foreach(f => f(value))
      semaphore.release()
    }

    val future: Fut[A] =
      new Fut[A] {
        def map[B](f: A => B): Fut[B]          = new Done(f(get()))
        def flatMap[B](f: A => Fut[B]): Fut[B] = f(get())
        def onComplete(f: A => Unit): Unit     = f(get())

        override def run() = ()
        override def get() = {
          semaphore.acquire()
          result.get
        }
      }
  }

  object Fut {

    def done[A](a: A): Fut[A] = Done(a)

    /**
     * Compute `a` in different thread. Equivalence of `Future.apply`
     * @param  a an expression that will be evaluated in a threadpool
    **/
    def apply[A](a: => A): Fut[A] = new Spawning(a)

    /**
     * Wrap any kind of `Future`-like operation
     * Example for wrapping [[scala.concurrent.Future]]
     * {{{
     * val future: Future[String] = downloadPageAsync("http://wikipedia.com")
     * val promise = Fut.promise[String]
     * future.onComplete {
     *   case Success(html) => promise.success(html)
     * }
     *
     * val htmll = promise
     *   .map(html => "Downloaded page:\n" + html)
     *   .fetch()
     * }}}
    **/
    def promise[A]: Promis[A] = new Promis[A]
  }
}
