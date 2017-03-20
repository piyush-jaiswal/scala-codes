import patmat.Huffman._

object Main extends App {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    println(quickEncode(t1)("ab".toList))
}