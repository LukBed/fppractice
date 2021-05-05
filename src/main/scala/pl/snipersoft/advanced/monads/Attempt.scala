package pl.snipersoft.advanced.monads

  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    //by name - it can be exception
    def apply[A](a: => A): Attempt[A] = {
      try {
        Success(a)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] = try {
      f(value)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  case class Failure[A](e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }