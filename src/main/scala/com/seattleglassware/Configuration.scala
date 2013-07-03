package com.seattleglassware

import com.escalatesoft.subcut.inject._
import com.escalatesoft.subcut.inject.NewBindingModule._
import com.google.api.client.extensions.appengine.http.UrlFetchTransport
import com.google.api.client.json.jackson.JacksonFactory
import BindingIdentifiers._
import java.util.Properties
import com.google.api.client.auth.oauth2.CredentialStore
import com.google.glassware.ListableAppEngineCredentialStore

object BindingIdentifiers {
  object OAuthPropertiesFileLocation extends BindingId
  object ApplicationName extends BindingId
}

object ProjectConfiguration {
  implicit val configuration = UniversalBindings.configuration
}

object UniversalBindings {
  implicit val configuration = newBindingModule { module =>
    import module._ // can now use bind directly

    bind[String] idBy OAuthPropertiesFileLocation toSingle "oauth.properties"
    bind[String] idBy ApplicationName toSingle "Seattle Art Tracker"
    bind[UrlFetchTransport] toSingle (new UrlFetchTransport)
    bind[JacksonFactory] toSingle (new JacksonFactory)
    bind[CredentialStore] toSingle (new ListableAppEngineCredentialStore)
    //    bind[Z] toProvider { codeToGetInstanceOfZ() }
    //    bind[A] toProvider { implicit module => new AnotherInjectedClass(param1, param2) } // module singleton
    //    bind[B] to newInstanceOf[Fred] // create a new instance of Fred every time - Fred require injection
    //    bind[C] to moduleInstanceOf[Jane] // create a module scoped singleton Jane that will be used
    //    bind[Int] idBy PoolSize to 3 // bind an Int identified by PoolSize to constant 3
    //    bind[String] idBy ServerURL to "http://escalatesoft.com"
  }
}
