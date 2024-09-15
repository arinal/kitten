package org.lamedh.scale

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
