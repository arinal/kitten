package org
package lamedh
package kitten
package core

import implosa._
import implosa.list._
import categories._

package object instances {

  type OkoString[A] = Oko[String, A]

  object functors {

    implicit val maybeFunctor = new Functor[Ono] {
      override def map[A, B](fa: Ono[A])(f: A => B): Ono[B] =
        fa match {
          case No    => No
          case On(a) => On(f(a))
        }
    }

    implicit val listFunctor = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa match {
          case Cons(h, t) => Cons(f(h), map(t)(f))
          case Nil        => Nil
        }
    }
  }

  object applies {

    import functors._

    implicit object MaybeApply extends Apply[Ono] {
      override def map[A, B](fa: Ono[A])(f: A => B): Ono[B] = maybeFunctor.map(fa)(f)
      override def ap[A, B](fab: Ono[A => B])(fa: Ono[A]): Ono[B] =
        fab match {
          case No    => No
          case On(f) => map(fa)(f)
        }
    }

    implicit val list = new Apply[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = listFunctor.map(fa)(f)
      override def ap[A, B](fab: List[A => B])(fa: List[A]): List[B] =
        fab match {
          case Nil         => Nil
          case Cons(f, fs) => union(map(fa)(f), ap(fs)(fa))
        }
    }

    implicit val okoApply = new Apply[OkoString] {
      override def map[A, B](fa: OkoString[A])(f: A => B): OkoString[B] =
        fa match {
          case Ko(s) => Ko(s)
          case Ok(a) => Ok(f(a))
        }

      /**
       * {{{
       * ap(Ok(_ + 1))(Ok(5)) == Ok(6)
       * ap(Ko("damn"))(Ok(5)) == Ko("damn")
       * ap(Ko("damn"))(Ko("you")) == Ko("damnyou")
       * }}}
      **/
      override def ap[A, B](fab: OkoString[A => B])(fa: OkoString[A]): OkoString[B] =
        (fab, fa) match {
          case (Ok(f), Ok(a))       => Ok(f(a))
          case (Ko(err1), Ko(err2)) => Ko(err1 + err2)
          case (Ok(_), Ko(err))         => Ko(err)
          case (Ko(err), Ok(_))         => Ko(err)
        }
    }
  }

  object monads {

    import functors._

    implicit val onoMonad = new Monad[Ono] {
      override def pure[A](a: A): Ono[A] = On(a)
      override def flatMap[A, B](fa: Ono[A])(f: A => Ono[B]): Ono[B] =
        fa match {
          case No    => No
          case On(a) => f(a)
        }
    }

    implicit val okoMonad = new Monad[OkoString] {
      override def pure[A](a: A): OkoString[A] = Ok(a)
      override def flatMap[A, B](fa: OkoString[A])(f: A => OkoString[B]): OkoString[B] =
        fa match {
          case Ko(s) => Ko(s)
          case Ok(a) => f(a)
        }
    }
  }
}
