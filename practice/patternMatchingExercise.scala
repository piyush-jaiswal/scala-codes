package week4

object exprs {
    def show(e: Expr): String = e match {
        case Number(x) => x.toString
        case Var(x) => x.toString
        case Sum(l, r) => show(l) + " + " + show(r)
        case Prod(x: Sum(a, b), y: Sum(c, d)) => "(" + show(x) + ") * (" + show(y) + ")"
        case Prod(x: Sum(a, b), y) => "(" + show(x) + ") * " + show(y)
        case Prod(x, y: Sum(c, d)) => show(x) + " * (" + show(y) + ")"
        case Prod(l, r) => show(l) + " * " + show(r)
    }

    show(Sum(Number(1), Number(44)))
}