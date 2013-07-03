package com.seattleglassware

import com.escalatesoft.subcut.inject._
import com.escalatesoft.subcut.inject.NewBindingModule._
import com.google.api.client.extensions.appengine.http.UrlFetchTransport
import com.google.api.client.json.jackson.JacksonFactory

object BindingIdentifiers {
  object OAuthPropertiesFileLocation extends BindingId
  object ApplicationName extends BindingId
}

object ProjectConfiguration {
  implicit val configuration = UniversalBindings.configuration
}

import BindingIdentifiers._

object UniversalBindings {
  implicit val configuration = newBindingModule { module =>
    import module._ // can now use bind directly

    bind[String] idBy OAuthPropertiesFileLocation toSingle "oauth.properties"
    bind[UrlFetchTransport] toSingle (new UrlFetchTransport)
    bind[JacksonFactory] toSingle (new JacksonFactory)
    //    bind[Z] toProvider { codeToGetInstanceOfZ() }
    //    bind[A] toProvider { implicit module => new AnotherInjectedClass(param1, param2) } // module singleton
    //    bind[B] to newInstanceOf[Fred] // create a new instance of Fred every time - Fred require injection
    //    bind[C] to moduleInstanceOf[Jane] // create a module scoped singleton Jane that will be used
    //    bind[Int] idBy PoolSize to 3 // bind an Int identified by PoolSize to constant 3
    //    bind[String] idBy ServerURL to "http://escalatesoft.com"
  }
}