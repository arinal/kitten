package org
package lamedh
package implosi

object concurrent {

  import java.util.concurrent.Semaphore
  import implosi._

  /**
   * Yet or not yet? represents [[Future]] from stdlib
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
    def map[B](f: A => B): YetNot[B]
    def fetch(): Oko[Throwable, A]
  }

  class Yet[A](a: A) extends YetNot[A] {
    override def fetch(): Oko[Throwable, A]   = Ok(a)
    override def map[B](f: A => B): YetNot[B] = new Yet(f(a))
  }

  class NotYet[A](a: => A) extends YetNot[A] {

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

    override def map[B](f: A => B): YetNot[B] = {
      result match {
        case Ok(a) => new NotYet(f(a))
        case Ko(t) => throw t
      }
    }
  }

  object YetNot {
    def apply[A](a: => A) = new NotYet(a)
    def yet[A](a: A)      = new Yet(a)
  }
}
