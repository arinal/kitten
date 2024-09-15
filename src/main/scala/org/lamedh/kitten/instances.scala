package org
package lamedh
package kitten

import scale._
import scale.list._
import categories.reducers._
import categories.mappers._

package object instances {

  type ResString[A] = Res[String, A]

  object reducers {

    import functors._

    implicit val stringMonoid: Monoid[String] = new Monoid[String] {
      override def add(a1: String, a2: String): String = a1 + a2
      override def empty: String                       = ""
    }

    implicit val intGroup: Group[Int] = new Group[Int] {
      override def add(a1: Int, a2: Int): Int = a1 + a2
      override def inverse(a: Int): Int       = -a
      override def empty: Int                 = 0
    }

    implicit val listFoldable: Foldable[Lis] = new Foldable[Lis] {

      override def foldLeft[A, B](fa: Lis[A], init: B)(f: (A, B) => B): B =
        fa match {
          case Cons(a, as) => foldLeft(as, f(a, init))(f)
          case Nil         => init
        }

      override def foldRight[A, B](fa: Lis[A], init: B)(f: (A, B) => B): B =
        foldLeft(reverse(fa), init)(f)
    }
  }

  object functors {

    implicit val mayFunctor: Functor[Opt] = new Functor[Opt] {
      override def map[A, B](fa: Opt[A])(f: A => B): Opt[B] =
        fa match {
          case Non    => Non
          case Som(a) => Som(f(a))
        }
    }

    implicit lazy val listFunctor: Functor[Lis] = new Functor[Lis] {
      override def map[A, B](fa: Lis[A])(f: A => B): Lis[B] =
        fa match {
          case Cons(h, t) => Cons(f(h), map(t)(f))
          case Nil        => Nil
        }
    }
  }

  object applies {

    import functors._

    implicit val mayApply: Apply[Opt] = new Apply[Opt] {
      override def map[A, B](fa: Opt[A])(f: A => B): Opt[B] =
        mayFunctor.map(fa)(f)
      override def ap[A, B](fab: Opt[A => B])(fa: Opt[A]): Opt[B] =
        fab match {
          case Non    => Non
          case Som(f) => map(fa)(f)
        }
    }

    implicit val as: Apply[Lis] = new Apply[Lis] {
      override def map[A, B](fa: Lis[A])(f: A => B): Lis[B] =
        listFunctor.map(fa)(f)
      override def ap[A, B](fab: Lis[A => B])(fa: Lis[A]): Lis[B] =
        fab match {
          case Nil         => Nil
          case Cons(f, fs) => union(map(fa)(f), ap(fs)(fa))
        }
    }

    implicit val okoApply: Apply[ResString] = new Apply[ResString] {
      override def map[A, B](fa: ResString[A])(f: A => B): ResString[B] =
        fa match {
          case Ko(s) => Ko(s)
          case Ok(a) => Ok(f(a))
        }

      /** {{{
       * ap(Ok(_ + 1))(Ok(5)) == Ok(6)
       * ap(Ko("damn"))(Ok(5)) == Ko("damn")
       * ap(Ko("damn"))(Ko("you")) == Ko("damnyou")
       * }}}
       */
      override def ap[A, B](
          fab: ResString[A => B]
      )(fa: ResString[A]): ResString[B] =
        (fab, fa) match {
          case (Ok(f), Ok(a))       => Ok(f(a))
          case (Ko(err1), Ko(err2)) => Ko(err1 + err2)
          case (_, Ko(err))         => Ko(err)
          case (Ko(err), _)         => Ko(err)
          case _                    => ??? // unreachable
        }
    }
  }

  object monads {

    import org.lamedh.scale.concurrent.Fut
    import functors._

    implicit val mayMonad: Monad[Opt] = new Monad[Opt] {
      override def pure[A](a: A): Opt[A] = Som(a)
      override def flatMap[A, B](fa: Opt[A])(f: A => Opt[B]): Opt[B] =
        fa match {
          case Som(a) => f(a)
          case Non    => Non
        }
    }

    implicit val resMonad: Monad[ResString] = new Monad[ResString] {
      override def pure[A](a: A): ResString[A] = Ok(a)
      override def flatMap[A, B](fa: ResString[A])(
          f: A => ResString[B]): ResString[B] =
        fa match {
          case Ok(a) => f(a)
          case Ko(s) => Ko(s)
        }
    }

    implicit val futMonad: Monad[Fut] = new Monad[Fut] {
      override def pure[A](a: A): Fut[A] = Fut.done(a)
      override def flatMap[A, B](fa: Fut[A])(f: A => Fut[B]): Fut[B] =
        fa.flatMap(f)
    }
  }
}
