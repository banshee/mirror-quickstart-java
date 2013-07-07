package com.seattleglassware

import java.io.FileInputStream
import java.io.InputStream
import java.util.Properties

import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import scala.util.control.Exception.ultimately
import scala.PartialFunction._

import com.escalatesoft.subcut.inject.BindingModule
import com.escalatesoft.subcut.inject.Injectable
import com.escalatesoft.subcut.inject.bindingIdToString
import com.google.api.client.auth.oauth2.AuthorizationCodeFlow
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.auth.oauth2.CredentialStore
import com.google.api.client.extensions.appengine.http.UrlFetchTransport
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.http.GenericUrl
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.util.ByteStreams
import com.google.api.services.mirror.Mirror
import com.seattleglassware.BindingIdentifiers.ApplicationName
import com.seattleglassware.BindingIdentifiers.OAuthPropertiesFileLocation

import JavaInterop.asInstanceOfNotNull
import JavaInterop.safelyCall
import javax.servlet.Filter
import javax.servlet.FilterChain
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import scalaz.Lens
import scalaz.Monoid
import scalaz.EitherT
import scalaz.Applicative
import scalaz.MonadTrans
import scalaz.Scalaz._
import scalaz.State
import scalaz.\/
import scalaz.-\/
import scalaz.\/-

import com.seattleglassware.EitherTWithState._

object EitherTWithState {
  case class StateGenerator[StateType, FailureType] {
    type StateWithFixedStateType[+A] = State[StateType, A]
    type EitherTWithFailureType[F[+_], A] = EitherT[F, FailureType, A]
    type CombinedStateAndFailure[A] = EitherTWithFailureType[StateWithFixedStateType, A]

    def liftStateA[A](s: StateWithFixedStateType[FailureType \/ A]): CombinedStateAndFailure[A] = EitherT(s)

    implicit class HasLiftFromStateWithFixedStateType[A](s: StateWithFixedStateType[FailureType \/ A]) {
      def liftState: CombinedStateAndFailure[A] = EitherT(s)
    }

    implicit class HasLiftFromEitherOfFailureTypeOrA[A](s: FailureType \/ A) {
      def liftState: CombinedStateAndFailure[A] = EitherT(Applicative[StateWithFixedStateType].point(s))
    }

    implicit class HasLiftFromAnswerType[A](s: A) {
      def liftState: CombinedStateAndFailure[A] = (s.right[FailureType]).liftState
    }

    implicit class HasLiftFromFailureType[A](s: FailureType) {
      def liftState: CombinedStateAndFailure[A] = (s.left[A]).liftState
    }

    implicit class HasLiftFromStateWithoutFailure[A](s: State[StateType, A]) {
      def liftState: CombinedStateAndFailure[A] = sublift(s)
      private[this] def sublift[A](st: StateWithFixedStateType[A]): CombinedStateAndFailure[A] = MonadTrans[EitherTWithFailureType].liftM(st)
    }
  }
}
