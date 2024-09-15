package org
package lamedh

/**
 * Mimicking standard scala library with a small modification in the name
**/
package object scale {

  import scala.util.control.NonFatal

  /**
   * Mimicking `Option[A]` from stdlib
   * {{{
   * val five: Opt[Int] = Som(5)
   * val none: Opt[Int] = Non
   * }}}
  **/
  abstract class Opt[+A]
  final case class Som[A](a: A) extends Opt[A]
  final case object Non         extends Opt[Nothing]

  object list {

    abstract class Lis[+A]
    final case object Nil                           extends Lis[Nothing]
    final case class Cons[A](head: A, tail: Lis[A]) extends Lis[A]

    def prepend[A](a: A, as: Lis[A]): Lis[A] = Cons(a, as)

    def reverse[A](list: Lis[A], acc: Lis[A] = Nil): Lis[A] =
      list match {
        case Nil         => acc
        case Cons(a, as) => reverse(as, prepend(a, acc))
      }

    def union[A](as1: Lis[A], as2: Lis[A]) = {
      def unionRec(sa1: Lis[A], acc: Lis[A]): Lis[A] =
        sa1 match {
          case Nil         => acc
          case Cons(a, sa) => unionRec(sa, prepend(a, acc))
        }
      unionRec(reverse(as1), as2)
    }
  }

  /**
   * Mimicking `Either[A, B]` from stdlib
   * {{{
   * val ok : Res[String, Int]    = Ok(5)
   * val ko1: Res[String, Int]    = Ko("Error occured")
   * val ko2: Res[Throwable, Int] = Res.doTry(throw new Exception("pop!"))
   * }}}
  **/
  abstract class Res[A, B]
  final case class Ok[A, B](b: B) extends Res[A, B]
  final case class Ko[A, B](a: A) extends Res[A, B]

  object Res {
    def doTry[A](a: => A): Res[Throwable, A] =
      try { Ok(a) }
      catch { case NonFatal(err) => Ko(err) }
  }
}
