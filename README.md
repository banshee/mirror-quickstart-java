Mirror API Quickstart for Scala and Scalaz
========================

This is a Scala port of Google's Quickstart for Java code (https://developers.google.com/glass/quickstart/java).

My suggestion is to start reading the code in src/main/scala/com/seattleglassware/ServersAndFilters.scala.  That
has the servers for /main, /oauth2callback, /notify and /attachmentproxy.

You'll notice that the code makes heavy use of Scalaz, including EitherT and the State monad.  Most of the functions
are for-comprehensions that look like this:

```scala
  def insertSubscription = for {
    userId <- getUserId
    credential <- getCredential
    collection <- getParameter("collection")
    callbackUrl <- getGenericUrlWithNewPath("/notify")
    subscription <- m.insertSubscription(credential, callbackUrl.toString, userId, collection)
    _ <- addGlasswareEffect(AddMessage("Application is now subscribed to updates."))
  } yield subscription
```

The left side of the arrows (<-) are all successful results.  The right side of the arrows are all functions that can 
do a few things:

* Return a successful result, in which case the computation just keeps going
* Return a failure result, in which case the computation ends and returns that failure

Threaded through those successes and failures are additions to state (that''s the state monad), so you can do things
like have a function return a success and at the same time push an operation (like set a session variable)
onto the state object.
