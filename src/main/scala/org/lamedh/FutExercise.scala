package org.lamedh

import scale.concurrent.Fut

object FutExercises {

  def printThread(exercise: Int) =
    println(s"ex$exercise: ${Thread.currentThread().getName()}")

  /*************************************************************************************************
   * Exercise 1: Create a future that returns a value 1.
   ************************************************************************************************/

  def exercise1 = Fut.done(1)

  /*************************************************************************************************
   * Exercise 2: Returns these two on a tuple:
   *   - Create a future that returns a value 1, and the computation must be
   *     done in a different thread. You might also put sleep and thread name
   *     inside the computation.
   *   - Get the result of the future.
   ************************************************************************************************/

  def exercise2: (Fut[Int], Int) = {
    val fut = Fut { Thread.sleep(10); 1 }
    (fut, fut.get())
  }

  /*************************************************************************************************
   * Exercise 3: Create a future that perform computation. Which thread does
   * the callback and the computation run on?
   ************************************************************************************************/

  def exercise3 = {
    def computation(): Int = {
      printThread(3)
      1
    }
    def callback(n: Int) = {
      println("ex3: Callback: result: $n")
      printThread(3)
    }
    val fut = Fut { computation() }
    fut.onComplete(callback)
    fut.run()
  }

  /*************************************************************************************************
   * Exercise 4: Create a spawnable future that returns a value 1. Using map,
   * create a new future that returns the value plus 1. This new future must
   * have the value 2.
   ************************************************************************************************/

  def exercise4 = {
    val fut  = Fut { printThread(4); 1 }
    val fut2 = fut.map(n => { printThread(4); n + 1 })
    val fut3 = fut2.map(n => { printThread(4); n + 1 })
    fut3
  }

  /*************************************************************************************************
   * Exercise 5: Create three futures that return 1, 2, and 3. Combine them to
   * return the sum of the three values, which is 6.
   *   - Avoid using the `get` method.
   *   - Can we use `map` for this?
   ************************************************************************************************/

  lazy val exercise5 = {
    val fut1 = Fut { printThread(5); 1 }
    val fut2 = Fut { printThread(5); 2 }
    val fut3 = Fut { printThread(5); 3 }
    fut1.flatMap(a => fut2.flatMap(b => fut3.map(c => a + b + c)))
  }

  /*************************************************************************************************
   * Exercise 6: Similar to exercise 5, but use a for comprehension syntax.
   ************************************************************************************************/

  def exercise6 =
    for {
      a <- Fut { printThread(6); 1 }
      b <- Fut { printThread(6); 2 }
      c <- Fut { printThread(6); 3 }
    } yield { a + b + c }

  /*************************************************************************************************
   * Exercise 7: Wrap the Java `CompletableFuture` into a `Fut`. Java library
   * has `HttpClient` to perform asynchronous HTTP requests. Below is the code
   * to perform an asynchronous HTTP request.
   * {{{
   * val client = HttpClient.newHttpClient()
   * val request = HttpRequest
   *   .newBuilder()
   *   .uri(java.net.URI.create(url))
   *   .build()
   * val javaFut = client.sendAsync(request, BodyHandlers.ofString())
   * }}}
   * Wrap the `javaFut` into a `Fut` and return the response body.
   ************************************************************************************************/

  def exercise7 = {

    import java.net.http.HttpClient
    import java.net.http.HttpRequest
    import java.net.http.HttpResponse.BodyHandlers

    def getAsync(url: String): Fut[String] = {
      val client = HttpClient.newHttpClient()
      val request = HttpRequest
        .newBuilder()
        .uri(java.net.URI.create(url))
        .build()
      val javaFut = client.sendAsync(request, BodyHandlers.ofString())

      val promise = Fut.promise[String]
      javaFut.whenComplete((response, _) => {
        promise.complete(response.body())
      })
      promise.future
    }

    getAsync("https://www.example.com")
  }

  /*************************************************************************************************
   * Exercise 8: Traverse Implement the `traverse` method that takes a list of
   * futures and returns a future of a list.
   ************************************************************************************************/

  def exercise8 = {
    val listOfFut                 = List(Fut { 1 }, Fut { 2 }, Fut { 3 })
    val futOfList: Fut[List[Int]] = Fut.traverse(listOfFut)
    futOfList.map(_.sum)
  }
}
