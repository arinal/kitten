package org.lamedh.scale

import scala.util.control.NonFatal

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
