object Math {
    // type class
    trait NumberLike[T] {
        def add(x: T, y: T): T
        def minus(x: T, y: T): T
        def divide(x: T, y: Int): T
    }

    // companion object for providing default implicits
    object NumberLike {
        implicit object NumberLikeInt extends NumberLike[Int] {
            def add(x: Int, y: Int): Int = x + y
            def minus(x: Int, y: Int): Int = x - y
            def divide(x: Int, y: Int): Int = x / y
        }

        implicit object NumberLikeDouble extends NumberLike[Double] {
            def add(x: Double, y: Double): Double = x + y
            def minus(x: Double, y: Double): Double = x - y
            def divide(x: Double, y: Int): Double = x / y
        }
    }
}

object Statistics {
    import Math.NumberLike

    // using implicit
    def mean[T](xs: Vector[T])(implicit ev: NumberLike[T]): T = ev.divide(xs.reduce(ev.add(_, _)), xs.size)

    // Context bound
    def median[T : NumberLike](xs: Vector[T]): T = xs(xs.size / 2)

    // Context bound
    def quartile[T : NumberLike](xs: Vector[T]): (T, T, T) = (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))

    // Context bound
    def inq[T : NumberLike](xs : Vector[T]): T = quartile(xs) match {
        case (lowerQuartile, _, upperQuartile) => implicitly[NumberLike[T]].minus(lowerQuartile, upperQuartile)
    }
}


object typeClasses {
    def main(args: Array[String]): Unit = {
        val meanInt: Int = Statistics.mean(Vector(1, 3, 4, 6, 8, 10))
        val meanDouble: Double = Statistics.mean(Vector(1, 3, 4, 6, 8, 10))
        val medianInt: Int = Statistics.median(Vector(1, 3, 4, 6, 8, 10))
        val medianDouble: Double = Statistics.median(Vector(1, 3, 4, 6, 8, 10))
        val quartileInt: (Int, Int, Int) = Statistics.quartile(Vector(1, 3, 4, 6, 8, 10))
        val quartileDouble: (Double, Double, Double) = Statistics.quartile(Vector(1, 3, 4, 6, 8, 10))
        val inqInt: Int = Statistics.inq(Vector(1, 3, 4, 6, 8, 10))
        val inqDouble: Double = Statistics.inq(Vector(1, 3, 4, 6, 8, 10))

        println(meanInt)
        println(meanDouble)
        println(medianInt)
        println(medianDouble)
        println(quartileInt)
        println(quartileDouble)
        println(inqInt)
        println(inqDouble)
    }
}