package com.seattleglassware

import scalaz._
import scalaz.Scalaz._

object EitherTWithState {
  case class StateGenerator[StateType, FailureType]() {
    type StateWithFixedStateType[+A] = State[StateType, A]
    type EitherTWithFailureType[F[+_], A] = EitherT[F, FailureType, A]
    type CombinedStateAndFailure[A] = EitherTWithFailureType[StateWithFixedStateType, A]

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
      def liftState: CombinedStateAndFailure[A] = MonadTrans[EitherTWithFailureType].liftM(s: StateWithFixedStateType[A])
    }
  }
}