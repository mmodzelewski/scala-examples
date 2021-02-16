case class Config(name: String, age: Int)

case class Name(firstName: String, lastName: String)

case class Age(age: Int)

case class Person(name: Name, age: Age)

object Imperative {

  import Configs._
  import Exceptions._

  def readName: Possibly[Configured[Name]] = {
    val parts = config.name.split(" ")
    require(parts.length >= 2)
    Name(parts(0), parts.tail.mkString(" "))
  }

  def readAge: Possibly[Configured[Age]] = {
    val age = config.age
    require(1 <= age && age <= 150)
    Age(age)
  }

  def readPerson: Configured[Option[Person]] = {
    attempt(
      Some(Person(readName, readAge))
    ).onError(None)
  }

  @main def hello: Unit = {
    println(readPerson(using Config("John Doe", 20)))
    println(readPerson(using Config("Incognito", 99)))
  }

}

object Configs {
  type Configured[T] = Config ?=> T

  def config: Configured[Config] = summon[Config]
}

object Exceptions {

  private class E extends Exception

  class CanThrow private[Exceptions]() {
    private[Exceptions] def throwE() = throw new E
  }

  type Possibly[T] = CanThrow ?=> T

  def require(p: Boolean)(using ct: CanThrow): Unit =
    if (!p) ct.throwE()

  def attempt[T](op: Possibly[T]) = OnError(op)

  class OnError[T](op: Possibly[T]) {
    def onError(fallback: => T): T =
      try op(using new CanThrow)
      catch {
        case ex: E => fallback
      }
  }

}
