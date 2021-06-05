package pl.snipersoft.cats.datamanipulation

import cats.data.Validated
import cats.syntax.validated._

import scala.util.Try

object DataValidation extends App {

  introduction()
  eitherValidation()
  catsValidation()
  operations()
  formValidationExercise()

  def introduction(): Unit = {
    val ok: Validated[String, Int] = Validated.valid(42)
    val ok2: Validated[String, Int] = 42.valid
    val nok: Validated[String, Int] = Validated.invalid("NOK!")
    val nok2: Validated[String, Int] = "NOK!".invalid
    val test: Validated[String, Int] = Validated.cond(42>40, 42, "Nok!")
  }

  def eitherValidation() {
    def test(n: Int): Either[List[String], Int] = {
      def helper(condition: Boolean, newError: String, errors: List[String]): List[String] =
        if (!condition) newError :: errors else errors

      val e1 = helper(n>= 0, s"$n is negative", List())
      val e2 = helper(n <= 100, s"$n is greater than 100", e1)
      val e3 = helper(n%2 == 0, s"$n is not even", e2)

      if (e3.isEmpty) Right(n)
      else Left(e3)
    }

    println(test(103))
    println(test(50))
  }

  def catsValidation(): Unit = {
    def test(n: Int): Validated[List[String], Int] =
      Validated.cond(n>= 0, n, List(s"$n is negative"))
        .combine(Validated.cond(n <= 100, n, List(s"$n is greater than 100")))
        .combine(Validated.cond(n%2 == 0, n, List(s"$n is not even")))

    println(test(103))
    println(test(50))
  }

  def operations(): Unit = {
    val ok: Validated[String, Int] = Validated.valid(42)
    val nok: Validated[List[String], Int] = Validated.invalid(List("NOK!"))

    ok.andThen(_ => nok)
    ok.ensure(List("Another NOK!"))(_%2 != 0)
    ok.map(_*2)
    nok.leftMap(_.length)
    ok.bimap(_.length, _+1)

    val fromEither: Validated[List[String], Int] = Validated.fromEither(Right(25))
    val fromOption: Validated[List[String], Int] = Validated.fromOption(Some(25), List("NOK!"))
    val fromTry: Validated[Throwable, Int] = Validated.fromTry(Try(25))

    ok.toOption
    ok.toEither
  }

  def formValidationExercise(): Unit = {
    object FormValidation {
      type FormValidation[T] = Validated[List[String], T]

      def validateForm(form: Map[String, String]): FormValidation[String] = {
        def getValue(fieldName: String): FormValidation[String] =
          Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified"))

        def nonBlank(value: String, fieldName: String): FormValidation[String] =
          Validated.cond(value.length >= 0, value, List(s"The field $fieldName must not be blank"))

        def emailProperForm(email: String): FormValidation[String] =
          Validated.cond(email.contains("@"), email, List("E-mail is invalid"))

        def checkPassword(password: String): FormValidation[String] =
          Validated.cond(password.length >= 10, password, List("Password is too weak"))

        getValue("name").andThen(name => nonBlank(name, "name"))
          .combine(getValue("mail").andThen(emailProperForm))
          .combine(getValue("password").andThen(checkPassword))
          .map(_ => "Success!")
      }
    }

    println(FormValidation.validateForm(Map("mail" -> "myMail", "password" -> "pass")))
    println(FormValidation.validateForm(Map("name" -> "Janek", "mail" -> "myMail@mail.com", "password" -> "myBestPassword")))
  }

}
