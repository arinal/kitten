package org
package lamedh
package kitten
package core

package object kernel {
  type Id[A] = A
}

/**
 * All of the categories which method returns a non-container value.
 * e.g. `foldLeft` returns `Int` rather than `Foldable[Int]`
**/
package object reducers {

  trait Semigroup[A] {
    def add(a1: A, a2: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  trait Group[A] extends Monoid[A] {
    def inverse(a: A): A
    def sub(a1: A, a2: A) = add(a1, inverse(a2))
  }

  trait Foldable[F[_]] {

    /**
     * Left associative way of folding
     * @param fa foldable instance to be folded
     * @param init initial result
     * @param f a function with 2 parameters, element `A` and accumulator `B`
     * Example below will return `6`:
     * {{{
     * Foldable[List](List(1, 2, 3), 0)((a: Int, acc: String) => acc + a)
     * }}}
     * @return accumulated value `B`
    **/
    def foldLeft[A, B](fa: F[A], init: B)(f: (A, B) => B): B

    /**
     * Right associative way of folding. Has exactly the same signature with its left associative.
     * This function is not stack safe.
    **/
    def foldRight[A, B](fa: F[A], init: B)(f: (A, B) => B): B

    /**
     * Like foldLeft, but combining the mapped `B` values is provided by `Monoid[B]`
     * @return accumulated value `B`
    **/
    def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
      foldLeft(fa, B.empty)((a, b) => B.add(f(a), b))

    /**
     * Fold left using `Monoid` and not mapping each element.
     * In a list-like container, it means totaling all the elements.
    **/
    def fold[A: Monoid](fa: F[A]) = foldMap(fa)(identity)
  }

  object Semigroup { def apply[A: Semigroup]   = implicitly[Semigroup[A]] }
  object Monoid    { def apply[A: Monoid]      = implicitly[Monoid[A]]    }
  object Group     { def apply[A: Group]       = implicitly[Group[A]]     }
  object Foldable  { def apply[F[_]: Foldable] = implicitly[Foldable[F]]  }
}

package object mappers {

  trait Invariant[F[_]] {
    def imap[A, B](fa: F[A])(f: A => B, g: B => A): F[B]
  }

  /**
   * Full named as contravariant functor, unfortunatelly the name 'functor' always refers
   * to its sibling, covariant functor.
   * Apparently `contramap` has a weird signature and doesn't go well in the market.
  **/
  trait Contravariant[F[_]] extends Invariant[F] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
    def imap[A, B](fa: F[A])(f: A => B, g: B => A): F[B] = contramap(fa)(g)
  }

  /**
   * Functor is a famous nick-name compared to its less fortunate technical name, covariant functor.
  **/
  trait Functor[F[_]] extends Invariant[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def imap[A, B](fa: F[A])(f: A => B, g: B => A): F[B] = map(fa)(f)
  }

  trait Apply[F[_]] extends Functor[F] {
    def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      ap(map(fa)(a => (b: B) => (a, b)))(fb)

    def product[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] = {
      val fab = ap(map(fa)(a => (b: B) => (a, b)))(fb)
      ap(map(fab)(ab => (c: C) => (ab._1, ab._2, c)))(fc)
    }

    def map[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]                 = map(product(fa, fb))(f.tupled)
    def map[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] = map(product(fa, fb, fc))(f.tupled)
  }

  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](a: A): F[A]
  }

  /**
   * The star of this show, **drumrolls** `Monad`
  **/
  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    // Monad is not a friendly category, it needs to redefine everything in flatMap
    override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = flatMap(ff)(f => map(fa)(f))
    override def map[A, B](fa: F[A])(f: A => B): F[B]    = flatMap(fa)(a => pure(f(a)))
  }

  trait Traverse[F[_]] extends Functor[F] {

    def traverse[G[_], A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_], A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

    override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[kernel.Id, A, B](fa)(f)
  }

  object Functor     { def apply[F[_]: Functor]     = implicitly[Functor[F]]     }
  object Apply       { def apply[F[_]: Apply]       = implicitly[Apply[F]]       }
  object Applicative { def apply[F[_]: Applicative] = implicitly[Applicative[F]] }
  object Monad       { def apply[F[_]: Monad]       = implicitly[Monad[F]]       }
}
