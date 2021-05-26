package pl.snipersoft.advanced.typesystem

object SelfTypes extends App {

  selfTypes()
  dependencyInjection()
  cakePattern()
  serverSideRenderingExercise()

  def selfTypes() {
    //requiring a type to be mixed in
    trait Instrumentalist {
      def play(): Unit
    }

    //whoever implements Singer to implement Instrumentalist also
    //this is a new feature, not lambda
    //any word, ex. self, this, whatever
    //use case: Singer and Instrumentalist are separated concepts
    //Singer requires an Instrumentalist, not Singer is an Instrumentalist
    trait Singer {
      self: Instrumentalist =>
      def sing(): Unit
    }

    class LeadSinger extends Singer with Instrumentalist {
      override def sing(): Unit = ???

      override def play(): Unit = ???
    }

    //not compiling
    //  class Vocalist extends Singer {
    //    override def sing(): Unit = ???
    //  }

    val jamesHetfield = new Singer with Instrumentalist {
      override def sing(): Unit = ???

      override def play(): Unit = ???
    }
  }

  def dependencyInjection(): Unit = {
    class Component {
      //API
    }
    class ComponentA extends Component
    class ComponentB extends Component
    class DependentComponent(val component: Component)
  }

  def cakePattern(): Unit = {
    //"cake pattern" - dependency injection
    //last layer of abstraction - like baking a cake
    //checked in compilation time
    //cyclical dependencies are possibly
    trait Component {
      //API
    }

    trait DependentComponent { self: Component =>
    }

    trait Application { self: DependentComponent => }

    //server side rendering

    //layer 1 - small components
    trait Picture extends Component
    trait Stats extends Component

    //layer 2 - compose components
    trait Profile extends DependentComponent with Picture
    trait Analytics extends DependentComponent with Stats

    //layer 3 - app
    trait AnalyticsApp extends Application with Analytics
  }

  def serverSideRenderingExercise(): Unit = {

  }

}
