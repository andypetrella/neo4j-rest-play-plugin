package be.nextlab.play.neo4j.rest

import play.api.libs.concurrent.Promise
import scalaz.{Success => OK, Failure => KO, _}
import scalaz.Scalaz._

case class ValidationPromised[+E, +A](promised: Promise[Validation[E, A]]) {

    def /~> [EE >: E, B](f: Validation[E, A] => ValidationPromised[EE, B]): ValidationPromised[EE, B] = 
        promised flatMap { valid => 
            f(valid).promised
        }

    def /~~>[EE >: E, B](f: A => ValidationPromised[EE, B]): ValidationPromised[EE, B] = 
        promised flatMap { valid => 
            valid.fold (
                bad => Promise.pure(KO(bad)),
                good => f(good).promised
            )
        }

}

object ValidationPromised {

    implicit def fromPromiseValidation[E, A](p:Promise[Validation[E, A]]):ValidationPromised[E, A] = ValidationPromised[E, A](p)
    implicit def fromValidationPromised[E, A](p:ValidationPromised[E, A]):Promise[Validation[E, A]] = p.promised

}