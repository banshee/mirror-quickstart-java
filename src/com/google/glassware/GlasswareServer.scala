package com.google.glassware

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import scalaz._
import scalaz.Scalaz._
import scalaz.State
import scalaz.{ \/ => \/ }

class GlasswareServer {
  sealed abstract class EarlyReturn
  case class NoSuchParameter( name: String ) extends EarlyReturn

  case class InternalState( req: HttpServletRequest ) {
    def getParameter( s: String ) = Option( req.getParameter( s ) )
  }

  val stategen = StateGenerator[ InternalState, EarlyReturn ]
  import stategen._

  class AttachmentProxyServlet extends HttpServlet {
    def getParameter( parameterName: String ) =
      State[ InternalState, EarlyReturn \/ String ] {
        case s =>
          val parameterValue = s.getParameter( parameterName )
          val result = parameterValue match {
            case Some( t ) => t.right
            case None      => NoSuchParameter( parameterName ).left
          }
          ( s, result )
      }

    def getParameterWithDefault( parameterName: String )( defaultValue: String ) =
      State[ InternalState, EarlyReturn \/ String ] {
        case s =>
          val parameterValue = s.getParameter( parameterName )
          ( s, parameterValue.getOrElse( defaultValue ).right )
      }

    def go = {
      val fn = for {
        attachmentId <- getParameter( "attachment" ).liftState
        timelineItemId <- getParameter( "timelineItem" ).liftState
      } yield ( attachmentId, timelineItemId )
    }
  }
}

trait HttpRequestWrapper {
  import HttpRequestWrapper._

  def getParameter( s: String ): Option[ String ]
  def getParameterWithDefault( parameterName: String )( defaultValue: String ) =
    getParameter( parameterName ) | defaultValue
  def requestType: HttpRequestType = getScheme match {
    case Some( "http" )  => Http
    case Some( "https" ) => Https
    case None            => Missing
    case _               => Other
  }
  def getScheme: Option[ String ]
}
object HttpRequestWrapper {
  sealed abstract class HttpRequestType
  case object Http extends HttpRequestType
  case object Https extends HttpRequestType
  case object Missing extends HttpRequestType
  case object Other extends HttpRequestType

  implicit class HttpServletRequestWrapper( r: HttpServletRequest ) extends HttpRequestWrapper {
    def getParameter( s: String ) = Option( r.getParameter( s ) )
    def getScheme = Option(r.getScheme()) map {_.toLowerCase}
  }
}

case class StateGenerator[ StateType, FailureType ] {
  type StateWithFixedStateType[ +A ] = State[ StateType, A ]
  type EitherTWithFailureType[ F[ +_ ], A ] = EitherT[ F, FailureType, A ]
  type CombinedStateAndFailure[ A ] = EitherTWithFailureType[ StateWithFixedStateType, A ]

  implicit class HasLiftFromStateWithFixedStateType[ A ]( s: StateWithFixedStateType[ FailureType \/ A ] ) {
    def liftState: CombinedStateAndFailure[ A ] = EitherT( s )
  }

  implicit class HasLiftFromEitherOfFailureTypeOrA[ A ]( s: FailureType \/ A ) {
    def liftState: CombinedStateAndFailure[ A ] = EitherT( Applicative[ StateWithFixedStateType ].point( s ) )
  }

  implicit class HasLiftFromAnswerType[ A ]( s: => A ) {
    def liftState: CombinedStateAndFailure[ A ] = ( s.right[ FailureType ] ).liftState
  }

  implicit class HasLiftFromFailureType[ A ]( s: => FailureType ) {
    def liftState: CombinedStateAndFailure[ A ] = ( s.left[ A ] ).liftState
  }

  implicit class HasLiftFromStateWithoutFailure[ A ]( s: State[ StateType, A ] ) {
    def liftState: CombinedStateAndFailure[ A ] = sublift( s )
    private[ this ] def sublift[ A ]( st: StateWithFixedStateType[ A ] ): CombinedStateAndFailure[ A ] = MonadTrans[ EitherTWithFailureType ].liftM( st )
  }
}

object EitherTWithState {
  case class EarlyExit()

  val fx = State[ Int, EarlyExit \/ String ] {
    case s if s < 2 => ( s + 1, "foo".right )
    case s          => ( s + 100, EarlyExit().left )
  }

  type StateA[ +A ] = State[ Int, A ]

  val result = for {
    a <- EitherT[ StateA, EarlyExit, String ]( fx )
    _ = println( s"a is $a" )
    b <- EitherT[ StateA, EarlyExit, String ]( fx )
    _ = println( s"b is $b" )
    c <- EitherT[ StateA, EarlyExit, String ]( fx )
    _ = println( s"c is $c" )
  } yield ( a, b, c )

  val ( finalState, finalResult ) = result.run( 0 )

  println( finalState.toString() )
  println( finalResult.toString() )
}

object GoForIt extends App {
  val x = EitherTWithState
  println( "sharkbait" )
}