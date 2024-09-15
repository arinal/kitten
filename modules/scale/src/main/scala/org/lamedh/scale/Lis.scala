package org.lamedh.scale

abstract class Lis[+A]
final case object Nil                           extends Lis[Nothing]
final case class Cons[A](head: A, tail: Lis[A]) extends Lis[A]

object Lis {

  def apply[A](args: A*): Lis[A] =
    args.foldRight(Nil: Lis[A])((a, as) => Cons(a, as))

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
