Google Mirror API's Quickstart for Java
========================

This is a Scala port of Google's Quickstart for Java code (https://developers.google.com/glass/quickstart/java).

My suggestion is to start reading the code in src/main/scala/com/seattleglassware/ServersAndFilters.scala.  That
has the servers for /main, /oauth2callback, /notify and /attachmentproxy.

You'll notice that the code makes heavy use of Scalaz, including EitherT and the State monad transformer.  The servers 
themselves are all for-comprehensions that look like this:

  def insertSubscription = for {
    userId <- getUserId
    credential <- getCredential
    collection <- getParameter("collection")
    callbackUrl <- getGenericUrlWithNewPath("/notify")
    subscription <- m.insertSubscription(credential, callbackUrl.toString, userId, collection)
    _ <- addGlasswareEffect(AddMessage("Application is now subscribed to updates."))
  } yield subscription

