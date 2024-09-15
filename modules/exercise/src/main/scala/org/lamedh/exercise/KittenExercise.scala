package org.lamedh

import kitten.categories.reducers.Monoid
import scale._

object KittenExercise {

  trait TimeUnit
  case object Seconds extends TimeUnit
  case object Minutes extends TimeUnit
  case class Time(amount: Int, unit: TimeUnit) {
    def toSeconds: Int =
      unit match {
        case Seconds => amount
        case Minutes => amount * 60
      }
  }

  implicit val timeMonoid: Monoid[Time] = new Monoid[Time] {
    override def add(a1: Time, a2: Time): Time =
      Time(a1.toSeconds + a2.toSeconds, Seconds)
    override def empty: Time = Time(0, Seconds)
  }

  /*************************************************************************************************
   * Exercise 0: Use `Lis` from `scale` package to create list of integers
   ************************************************************************************************/

  def exercise0 = {
    // Create a list that contains [1, 2, 3], use `Cons` and `Nil`
    val list1 = Cons(1, Cons(2, Cons(3, Nil)))
    // Create a list that contains [1, 2, 3], use `Lis.apply`
    val list2 = Lis(1, 2, 3)

    println(list1)
    println(list2)
  }

  /*************************************************************************************************
   * Exercise 1: Use `Monoid` to add two integers
   ************************************************************************************************/

  def exercise1 = {
    // Use `Monoid` to add two integers
    import kitten.instances.reducers.intGroup
    val res1 = Monoid[Int].add(1, 2)
    //
    // Use `Monoid` to add two strings
    import kitten.instances.reducers.stringMonoid
    val res2 = Monoid[String].add("Hello", " World")

    println(res1)
    println(res2)
  }

  /*************************************************************************************************
   * Exercise 1b: Add two `Time` instances using `Monoid`
   ************************************************************************************************/

  def exercise1b = {
    // Use `Monoid` to add two `Time` instances
    val t1 = Monoid[Time].add(Time(1, Seconds), Time(2, Minutes))

    // Use `Monoid` to add two `Time` instances, using `+` syntax
    import kitten.syntaxes._
    val t2 = Time(1, Seconds) + Time(2, Minutes)

    println(t1)
    println(t2)
  }

  /*************************************************************************************************
   * Exercise 2a: Sum a list of integers using `Foldable`. Use `foldLeft`, `foldRight` and `fold`
   ************************************************************************************************/

  def exercise2a = {
    val F    = kitten.instances.reducers.listFoldable
    val list = Lis(1, 2, 3)
    println(F.foldLeft(list, 0)(_ + _))
    println(F.foldRight(list, 0)(_ + _))

    import kitten.instances.reducers.intGroup
    println(F.fold(list))
  }

  /*************************************************************************************************
   * Exercise 2b: Same with `exercise2a` but using `Foldable` syntax
   ************************************************************************************************/

  def exercise2b = {
    val list = Lis(1, 2, 3)
    import kitten.instances.reducers._
    import kitten.syntaxes._
    println(list.foldLeft(0)(_ + _))
    println(list.foldRight(0)(_ - _))

    import kitten.instances.reducers.intGroup
    println(list.fold)
  }

  /*************************************************************************************************
   * Exercise 2c: Sum a list of `Time` using `Foldable.fold`
   ************************************************************************************************/

  def exercise2c = {
    val times = Lis(Time(1, Seconds), Time(2, Minutes), Time(3, Seconds))
    import kitten.instances.reducers._
    import kitten.syntaxes._
    println(times.fold)
  }

  /*************************************************************************************************
   * Exercise 3: Play around with `Functor` using `map` and `lift`
   ************************************************************************************************/

  def exercise3 = {
    import kitten.instances.monads._
    import kitten.categories.mappers._
    import kitten.syntaxes._
    import scale._

    val some1: Opt[Int] = Som(1)
    println(some1.map(_ + 1))
    // println(Lis(1, 2).map(_ + 1))

    def inc(a: Int): Int                = a + 1
    val incLifted: Opt[Int] => Opt[Int] = Functor[Opt].lift(inc)

    println(incLifted(Som(1)))
    println(incLifted(Non))
  }

  /*************************************************************************************************
   * Exercise 4: Compose multiple containers using `map`, `flatMap` and `for`
   ************************************************************************************************/

  def exercise4 = {
    val o1: Opt[Int] = Som(1)
    val o2: Opt[Int] = Som(2)
    val o3: Opt[Int] = Som(2)

    val match2 = (o1, o2) match {
      case (Som(a), Som(b)) => Som(a + b)
      case _                => Non
    }

    import kitten.instances.monads._
    import kitten.syntaxes._
    val map2  = o1.map(a => o2.map(b => a + b))
    val fmap2 = o1.flatMap(a => o2.map(b => a + b))
    val fmap3 = o1.flatMap(a => o2.flatMap(b => o3.map(c => a + b + c)))

    val formap = for {
      a <- o1
      b <- o2
      c <- o3
    } yield a + b + c

    println(match2)
    println(map2)
    println(fmap2)
    println(fmap3)
    println(formap)
  }

  /*************************************************************************************************
   * Exercise 5: Compose multiple containers using `Applicative`
   ************************************************************************************************/

  def exercise5a = {
    import kitten.instances.monads._
    import kitten.syntaxes._
    val o1: Opt[Int] = Som(1)
    val o2: Opt[Int] = Som(2)
    val o3: Opt[Int] = Som(3)

    val prod2 = (o1, o2).product
    val map2  = (o1, o2).mapN(_ + _)

    val prod3 = (o1, o2, o3).product
    val map3  = (o1, o2, o3).mapN(_ + _ + _)

    println(prod2)
    println(map2)
    println(prod3)
    println(map3)
  }

  /*************************************************************************************************
   * Exercise 5b: The intuition behind `Applicative.ap` composability
   ************************************************************************************************/

  def exercise5b = {
    val o1: Opt[Int] = Som(1)
    val o2: Opt[Int] = Som(2)
    val o3: Opt[Int] = Som(3)

    import kitten.instances.monads._
    import kitten.categories.mappers._

    val A = Applicative[Opt]

    // `(o1, o2, o3).product` in slow-motion
    val of1: Opt[Int => (Int, Int)] = A.map(o1)(a => (b: Int) => (a, b))
    val o12: Opt[(Int, Int)]        = A.ap(of1)(o2)
    val of2: Opt[(Int) => (Int, Int, Int)] =
      A.map(o12)(ab => (c: Int) => (ab._1, ab._2, c))
    val o123: Opt[(Int, Int, Int)] = A.ap(of2)(o3)

    // import syntaxes._
    // val of1: Opt[Int => Int] = o1.map(a => (b: Int) => (a, b))
    // val o12: Opt[Int]        = o2.ap(of1)
    // val of2: Opt[Int => Int] = o12.map(ab => (c: Int) => (ab._1, ab._2, c))
    // val o123: Opt[Int]       = o3.ap(of2)

    println(o123)
  }

  /*************************************************************************************************
   * Exercise 6a: The difference between monadic and applicative composition
   ************************************************************************************************/

  def exercise6a = {
    import kitten.instances._
    import kitten.instances.applicatives._

    case class Person(name: String, age: Int)

    def checkName(name: String): ResString[String] =
      if (name.isEmpty) Ko("name is empty") else Ok(name)
    def checkAge(age: Int): ResString[Int] =
      if (age < 0) Ko("age is negative") else Ok(age)

    def validateAppl(person: Person): ResString[Person] = {
      import kitten.instances.applicatives._
      import kitten.categories.mappers._
      Applicative[ResString].mapN(checkName(person.name), checkAge(person.age))(
        (n, a) => Person(n, a))
    }

    def validateMon(person: Person): ResString[Person] = {
      import kitten.instances.monads.resMonad
      import kitten.syntaxes._
      (checkName(person.name), checkAge(person.age)).mapN((n, a) =>
        Person(n, a))
      // for {
      //   n <- checkName(person.name)
      //   a <- checkAge(person.age)
      // } yield Person(n, a)
    }

    val v1a = validateAppl(Person("Doe", 30))
    val v2a = validateAppl(Person("", 30))
    val v3a = validateAppl(Person("Doe", -1))
    val v4a = validateAppl(Person("", -1))

    val v1m = validateMon(Person("Doe", 30))
    val v2m = validateMon(Person("", 30))
    val v3m = validateMon(Person("Doe", -1))
    val v4m = validateMon(Person("", -1))

    println("v1a: " + v1a)
    println("v2a: " + v2a)
    println("v3a: " + v3a)
    println("v4a: " + v4a)

    println("v1m: " + v1m)
    println("v2m: " + v2m)
    println("v3m: " + v3m)
    println("v4m: " + v4m)
  }

  /*************************************************************************************************
   * Exercise 6b: Design a `Validation` data structure to validate a generic `A` type.
   * The `Validation` should be able to accumulate errors.
   ************************************************************************************************/

  def exercise6b = {
    import kitten.categories.mappers._

    trait Validation[+A]
    case class Valid[A](value: A)           extends Validation[A]
    case class Invalid(error: List[String]) extends Validation[Nothing]

    implicit val valApplicative = new Applicative[Validation] {
      override def pure[A](a: A): Validation[A] = Valid(a)

      override def map[A, B](fa: Validation[A])(f: A => B): Validation[B] =
        fa match {
          case Valid(a)      => Valid(f(a))
          case Invalid(errs) => Invalid(errs)
        }

      override def ap[A, B](fab: Validation[A => B])(
          fa: Validation[A]): Validation[B] =
        (fab, fa) match {
          case (Valid(f), Valid(a))             => Valid(f(a))
          case (Invalid(errs1), Invalid(errs2)) => Invalid(errs1 ++ errs2)
          case (Invalid(errs), _)               => Invalid(errs)
          case (_, Invalid(errs))               => Invalid(errs)
        }
    }

    case class Person(name: String, age: Int)
    def checkName(name: String): Validation[String] =
      if (name.isEmpty) Invalid(List("name is empty")) else Valid(name)
    def checkAge(age: Int): Validation[Int] =
      if (age < 0) Invalid(List("age is negative")) else Valid(age)

    def validate(person: Person): Validation[Person] = {
      // (checkName(person.name), checkAge(person.age)).mapN((n, a) => Person(n, a))
      Applicative[Validation].mapN(checkName(person.name),
                                   checkAge(person.age))((n, a) => Person(n, a))
    }

    val v1 = validate(Person("Doe", 30))
    val v2 = validate(Person("", 30))
    val v3 = validate(Person("Doe", -1))
    val v4 = validate(Person("", -1))

    println(v1)
    println(v2)
    println(v3)
    println(v4)
  }

  /*************************************************************************************************
   * Exercise 7a: Use `Traverse` to transform a `Lis[Opt[Int]]` into an `Opt[Lis[Int]]`
   ************************************************************************************************/

  def exercise7a = {
    import kitten.categories.mappers._
    import kitten.instances.traversables._
    import kitten.instances.applicatives._

    val opts: Lis[Opt[Int]] = Lis(Som(1), Som(2), Som(3))
    println(Traverse[Lis].sequence(opts))
  }

  /*************************************************************************************************
   * Exercise 7b: Similar to `exercise8a` but with `Fut[Int]`
   ************************************************************************************************/

  def exercise7b = {
    import kitten.instances.traversables._
    import kitten.instances.monads._
    import kitten.categories.mappers._
    import org.lamedh.scale.Fut

    val futs: Lis[Fut[Int]] = Lis(Fut.done(1), Fut.done(2))
    val listOfFut           = Traverse[Lis].traverse(futs)(a => a.map(_ + 1))

    println(listOfFut)
  }
}
