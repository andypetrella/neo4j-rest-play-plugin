package be.nextlab.play.neo4j.rest

import play.api.libs.concurrent.Promise
import scalaz.{Success => OK, Failure => KO, _}
import scalaz.Scalaz._

case class ValidationPromised[E, A](promised: Promise[Validation[E, A]]) {
    import ValidationPromised._

    def /~> [EE >: E, B](f: Validation[E, A] => ValidationPromised[EE, B]): ValidationPromised[EE, B] = 
        promised flatMap { valid => 
            f(valid).promised
        } transformer

    def map[B](f: A => B): ValidationPromised[E, B] = 
        promised map { valid => 
            valid.fold(
                fail => KO(fail),
                suc => OK(f(suc))
            )
        } transformer


    def /~~>[EE >: E, B](f: A => ValidationPromised[EE, B]): ValidationPromised[EE, B] = 
        promised flatMap { valid => 
            valid.fold (
                bad => Promise.pure(KO(bad)),
                good => f(good).promised
            )
        } transformer

    def flatMap[B](f: A => ValidationPromised[E, B]): ValidationPromised[E, B] = /~~>(f)

}

object ValidationPromised {

    case class PromiseValidationW[E, A] (promised:Promise[Validation[E, A]]) {
        def transformer:ValidationPromised[E, A] = ValidationPromised(promised)
    }

    implicit def pimpPromiseValidation[E,A](p:Promise[Validation[E, A]]):PromiseValidationW[E, A] = 
        PromiseValidationW(p)

    implicit def pimpValidationPromised[E,A](p:ValidationPromised[E, A]):Promise[Validation[E, A]] = 
        p.promised

}

