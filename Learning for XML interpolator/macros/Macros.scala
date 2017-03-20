import scala.language.experimental.macros
import scala.reflect.macros._

object test {
    def maximum(a: Int, b: Int): Int = macro maximumMacro

    def maximumMacro(c: Context)(a: c.Tree, b: c.Tree): c.Tree = {
        import c.universe._
        val temp1 = a
        val temp2 = b
        q"if ($temp1 > $temp2) $temp1 else $temp2"
    }
}