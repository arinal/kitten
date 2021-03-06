package org
package lamedh
package kitten

import implosi._
import implosi.list._
import categories.reducers._
import categories.mappers._

package object instances {

  type OkoString[A] = Oko[String, A]

  object reducers {

    import functors._

    implicit val stringMonoid = new Monoid[String] {
      override def add(a1: String, a2: String): String = a1 + a2
      override def empty: String                       = ""
    }

    implicit val intGroup = new Group[Int] {
      override def add(a1: Int, a2: Int): Int = a1 + a2
      override def inverse(a: Int): Int       = -a
      override def empty: Int                 = 0
    }

    implicit val listFoldable = new Foldable[As] {

      override def foldLeft[A, B](fa: As[A], init: B)(f: (A, B) => B): B =
        fa match {
          case Cons(a, as) => foldLeft(as, f(a, init))(f)
          case Nil         => init
        }

      override def foldRight[A, B](fa: As[A], init: B)(f: (A, B) => B): B =
        foldLeft(reverse(fa), init)(f)
    }
  }

  object functors {

    implicit val maybeFunctor = new Functor[Ono] {
      override def map[A, B](fa: Ono[A])(f: A => B): Ono[B] =
        fa match {
          case No    => No
          case On(a) => On(f(a))
        }
    }

    implicit lazy val listFunctor = new Functor[As] {
      override def map[A, B](fa: As[A])(f: A => B): As[B] =
        fa match {
          case Cons(h, t) => Cons(f(h), map(t)(f))
          case Nil        => Nil
        }
    }
  }

  object applies {

    import functors._

    implicit val onoApply = new Apply[Ono] {
      override def map[A, B](fa: Ono[A])(f: A => B): Ono[B] = maybeFunctor.map(fa)(f)
      override def ap[A, B](fab: Ono[A => B])(fa: Ono[A]): Ono[B] =
        fab match {
          case No    => No
          case On(f) => map(fa)(f)
        }
    }

    implicit val as = new Apply[As] {
      override def map[A, B](fa: As[A])(f: A => B): As[B] = listFunctor.map(fa)(f)
      override def ap[A, B](fab: As[A => B])(fa: As[A]): As[B] =
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
          case (_, Ko(err))         => Ko(err)
          case (Ko(err), _)         => Ko(err)
          case _                    => ??? // unreachable
        }
    }
  }

  object monads {

    import org.lamedh.implosi.concurrent.Yet
    import org.lamedh.implosi.concurrent.YetNot
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

    implicit val yetNotMonad = new Monad[YetNot] {
      override def pure[A](a: A): YetNot[A] = new Yet(a)
      override def flatMap[A, B](fa: YetNot[A])(f: A => YetNot[B]): YetNot[B] = {
        fa.map { a =>
          f(a).fetch() match {
            case Ok(a)   => a
            case Ko(err) => throw err
          }
        }
      }
    }
  }
}
