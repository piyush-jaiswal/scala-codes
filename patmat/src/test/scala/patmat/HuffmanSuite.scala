package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times1") {
    assert(times(string2Chars("abbcdaabcdd")) === List(('a', 3), ('b', 3), ('c', 2), ('d', 3)))
  }

  test("times2") {
    assert(times(string2Chars("aaaaaaaa")) === List(('a', 8)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode") {
    assert(decode(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), List(1)) === "t".toList)
  }

  test("decode2") {
    new TestTrees {
      assert(decode((t2), List(1,0,0,1)) === "dad".toList)
    }
  }

  test("encode") {
    new TestTrees {
      assert(quickEncode(t2)(string2Chars("dad")) === List(1,0,0,1))
    }
  }

  test("mergeCodeTables") {
    new TestTrees {
      assert(mergeCodeTables(List(('a', List(0))), List(('1', List(1)))) === List(('a'), List(1,0)))
    }
  }

  test("convert") {
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('1', List(1))))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}