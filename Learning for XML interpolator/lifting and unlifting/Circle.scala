import scala.math._
import scala.reflect.runtime.universe._

case class Circle(radius: Double)

object Circle {
    
    implicit val area = Liftable[Circle] { c =>
        q"_root_.Circle(${Pi * c.radius * c.radius})"
    }

    implicit val radius = Unliftable[Circle] {
        case q"_root_.Circle(${area: Double})" => Circle(sqrt(area / Pi))
    }

    def main (args: Array[String]) {
        val x = Circle(2.5)
        println(q"$x")

        x match {
            case Circle(radius) => println(radius)
        }
    }
}