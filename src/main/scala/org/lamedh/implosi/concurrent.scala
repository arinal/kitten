package org
package lamedh
package implosi

object concurrent {

  import java.util.concurrent.Semaphore
  import implosi._

  /**
   * Yet or not yet? represents [[scala.concurrent.Future]] from stdlib
   * Example:
   * {{{
   * val notyet = NotYet { Thread.sleep(1000); 5 }
   * val ny1 = notyet.map(_ + 1)
   * val ny2 = notyet.map(_ + 2)
   * val n1 = ny1.run() // this will block and return 6
   * val n2 = ny2.run() // this won't block anymore, returns 7
   * }}}
    **/
  trait YetNot[A] {

    /**
     * Non-blocking way to access the wrapped value, and transform it over function
     * @param f function to transform the value
     * @return new instance of [[YetNot]] with transformed value
    **/
    def map[B](f: A => B): YetNot[B]

    /**
     * Unwrap the value forcefully. This operation blocks the current thread.
     * @return either `Ok[A] or Ko[Throwable]`
    **/
    def fetch(): Oko[Throwable, A]
  }

  /**
   * Mimicks `Future.successful`, wrap over a materialized value
  **/
  final class Yet[A](a: => A) extends YetNot[A] {
    override def fetch(): Oko[Throwable, A]   = Ok(a)
    override def map[B](f: A => B): YetNot[B] = new Yet(f(a))
  }

  /**
   * Put a computation `a` to a thread pool
  **/
  final class NotYet[A](a: => A) extends YetNot[A] {

    private val executionCtx = scala.concurrent.ExecutionContext.Implicits.global
    private val semaphore    = new Semaphore(0)

    private lazy val result: Oko[Throwable, A] = fetch()

    def fetch(): Oko[Throwable, A] = {
      var res: Oko[Throwable, A] = null
      executionCtx.execute { () =>
        res = Oko.doTry(a)
        semaphore.release()
      }
      semaphore.acquire()
      res
    }

    override def map[B](f: A => B): YetNot[B] =
      new NotYet(
        result match {
          case Ok(a) => f(a)
          case Ko(t) => throw t
        }
      )
  }

  /**
   * Wrap an async operation from another type
  **/
  final class Promise[A] extends YetNot[A] {

    private lazy val semaphore = new Semaphore(0)
    private var result: Yet[A] = null

    def success(a: A): Unit = {
      result = new Yet(a)
      semaphore.release()
    }

    def failure(err: Throwable): Unit = {
      result = new Yet(throw err)
      semaphore.release()
    }

    override def map[B](f: A => B): YetNot[B] = {
      lazy val value = {
        semaphore.acquire()
        result.fetch() match {
          case Ok(a)   => f(a)
          case Ko(err) => throw err
        }
      }
      new Yet(value)
    }

    override def fetch(): Oko[Throwable, A] = {
      semaphore.acquire()
      result.fetch()
    }
  }

  object YetNot {

    /**
     * Compute `a` in different thread. Equivalence of `Future.apply`
     * @param  a an expression that will be evaluated in a threadpool
    **/
    def apply[A](a: => A): YetNot[A] = new NotYet(a)

    /**
     * Wrap a value in a [[YetNot]] context. Equivalence with [[Future.successful]]
     * @param  a materialized value to wrap.
     * @return a wrapped value a in a finished [[YetNot]]
    **/
    def yet[A](a: A): YetNot[A] = new Yet(a)

    /**
     * Wrap any kind of `Future`-like operation
     * Example for wrapping [[scala.concurrent.Future]]
     * {{{
     * val future: Future[String] = downloadPageAsync("http://wikipedia.com")
     * val promise = YetNot.promise[String]
     * future.onComplete {
     *   case Success(html) => promise.success(html)
     *   case Failure(err) => promise.failure(err)
     * }
     *
     * val htmll = promise
     *   .map(html => "Downloaded page:\n" + html)
     *   .fetch()
     * }}}
    **/
    def promise[A]: Promise[A]  = new Promise[A]
  }
}
