package pl.snipersoft.advanced.typesystem

object Reflection extends App {
  import scala.reflect.runtime. {universe => ru}
  val mirror = ru.runtimeMirror (getClass.getClassLoader)

  case class Person(name: String) {
    def sayMyName(): Unit = println(s"Hi, my name is $name")
  }
  class MyMap[K, V]
  class Animal
  class Dog extends Animal

  createAnInstance()
  useMethod()
  typeErasureProblems()
  typeTags()
  useMethodWithTypeTags()

  def createAnInstance(): Unit = {
    val classSymbol = mirror.staticClass ("pl.snipersoft.advanced.typesystem.Reflection.Person") //description of class
    val classMirror = mirror.reflectClass (classSymbol) //can do things
    val constructorSymbol = classSymbol.primaryConstructor.asMethod
    val constructorMirror = classMirror.reflectConstructor (constructorSymbol)
    val instance = constructorMirror.apply("Janek")
    println (instance)
  }

  def useMethod(): Unit = {
    val person = Person("Janek")
    val reflected = mirror.reflect(person)
    val methodSymbol = ru.typeOf[Person].decl(ru.TermName("sayMyName")).asMethod
    val methodMirror = reflected.reflectMethod(methodSymbol)
    methodMirror.apply()
  }

  //clearing generics
  def typeErasureProblems(): Unit = {
    //differentiate types at runtime
    val numbers = List(1, 2, 3)
    numbers match {
      case l: List[String] => println("tekst") //it'll be executed
      case l: List[Int] => println("numery")
    }

    //limitations on overloads
    def doSth(list: List[String]): Unit = ???
    //    def doSth(list: List[Int]): Unit = ???
  }

  def typeTags(): Unit = {
    import ru._

    def getTypeArguments[T](value: T)(implicit typeTag: TypeTag[T]) = typeTag.tpe match {
      case TypeRef(_, _, typeArguments) => typeArguments
      case _ => List()
    }

    val myMap = new MyMap[Int, String]
    val typeArgs = getTypeArguments(myMap)
    //implicits are in compile time
    println(typeArgs) //List(Int, String)

    def isSubtype[A, B](implicit ttagA: TypeTag[A], ttagB: TypeTag[B]): Boolean =
      ttagA.tpe <:< ttagB.tpe

    println(isSubtype[Dog, Animal])
  }

  def useMethodWithTypeTags(): Unit = {
    import ru._

    val person = Person("Janek")
    val reflected = mirror.reflect(person)
    val methodSymbol = typeTag[Person].tpe.decl(ru.TermName("sayMyName")).asMethod
    val methodMirror = reflected.reflectMethod(methodSymbol)
    methodMirror.apply()
  }
}
