package com.seattleglassware

import com.escalatesoft.subcut.inject._
import com.escalatesoft.subcut.inject.NewBindingModule._
import com.google.api.client.extensions.appengine.http.UrlFetchTransport
import com.google.api.client.json.jackson.JacksonFactory
import BindingIdentifiers._
import java.util.Properties
import com.google.api.client.auth.oauth2.CredentialStore
import com.google.glassware.ListableAppEngineCredentialStore
import com.google.api.client.auth.oauth2.Credential.AccessMethod
import com.seattleglassware.MirrorOps

object BindingIdentifiers {
  object OAuthPropertiesFileLocation extends BindingId
  object GlassScope extends BindingId
  object AuthenticationClock extends BindingId
  object TokenServerEncodedUrl extends BindingId
  object ApplicationName extends BindingId
}

class BoundValuesx(implicit val bindingModule: BindingModule) extends Injectable {
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
    bind[String] idBy GlassScope toSingle "https://www.googleapis.com/auth/glass.timeline " +
      "https://www.googleapis.com/auth/glass.location " +
      "https://www.googleapis.com/auth/userinfo.profile"
    bind[MirrorOps] toSingle (new MirrorOps()(module))
  }
}

