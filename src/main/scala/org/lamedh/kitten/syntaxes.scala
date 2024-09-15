package org.lamedh
package kitten

package object syntaxes {
  import categories.reducers._
  import categories.mappers._
  import instances._

  import scale._

  implicit class MonoidSyntax[A: Monoid](a: A) {
    def +(b: A): A = Monoid[A].add(a, b)
  }

  implicit class FoldableListSyntax[A](list: Lis[A])(implicit
      F: Foldable[Lis]) {
    def foldLeft[B](init: B)(f: (A, B) => B): B  = F.foldLeft(list, init)(f)
    def foldRight[B](init: B)(f: (A, B) => B): B = F.foldRight(list, init)(f)
    def fold(implicit M: Monoid[A])              = F.foldMap(list)(a => a)
  }

  implicit class MonadSyntax[F[_], A](fa: F[A])(implicit M: Monad[F]) {
    def map[B](f: A => B): F[B]        = M.map(fa)(f)
    def flatMap[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
    def ap[B](fab: F[A => B]): F[B]    = M.ap(fab)(fa)
  }

  // To enable `(a, b).product` syntax
  implicit class ApplyT2Syntax[F[_], A, B](fa: (F[A], F[B]))(implicit
      A: Monad[F]) {
    def product: F[(A, B)]            = A.product(fa._1, fa._2)
    def mapN[Z](f: (A, B) => Z): F[Z] = A.mapN(fa._1, fa._2)(f)
  }

  // To enable `(a, b, c).product` syntax
  implicit class ApplyT3Syntax[F[_], A](fa: (F[A], F[A], F[A]))(implicit
      A: Monad[F]) {
    def product[B]: F[(A, A, A)]            = A.product(fa._1, fa._2, fa._3)
    def mapN[B, Z](f: (A, A, A) => Z): F[Z] = A.mapN(fa._1, fa._2, fa._3)(f)
  }
}
