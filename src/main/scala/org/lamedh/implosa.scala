package org
package lamedh

/**
 * Represents standard scala(ble) library, but in implosive way, hence implosa
**/
package object implosa {

  /**
   * On or No? Mimicking Option[A] from stdlib
   * {{{
   * val five: Ono[Int] = On(5)
   * val none: Ono[Int] = No
   * }}}
  **/
  abstract class Ono[+A]
  final case class On[A](a: A) extends Ono[A]
  final case object No         extends Ono[Nothing]

  object list {

    abstract class Cats[+A]
    final case object Nil                            extends Cats[Nothing]
    final case class Cons[A](head: A, tail: Cats[A]) extends Cats[A]

    def prepend[A](a: A, as: Cats[A]): Cats[A] = Cons(a, as)

    def reverse[A](list: Cats[A], acc: Cats[A] = Nil): Cats[A] =
      list match {
        case Nil         => acc
        case Cons(a, as) => reverse(as, prepend(a, acc))
      }

    def union[A](as1: Cats[A], as2: Cats[A]) = {
      def unionRec(sa1: Cats[A], acc: Cats[A]): Cats[A] = sa1 match {
        case Nil         => acc
        case Cons(a, sa) => unionRec(sa, prepend(a, acc))
      }
      unionRec(reverse(as1), as2)
    }
  }

  /**
   * Ok or Ko? Mimicking Either[A, B] from stdlib
   * {{{
   * val ok5: Oko[String, Int] = Ok(5)
   * val error: Oko[String, Int] = Ko("Error occured")
   * }}}
  **/
  abstract class Oko[A, B]
  final case class Ko[A, B](a: A) extends Oko[A, B]
  final case class Ok[A, B](b: B) extends Oko[A, B]
}
