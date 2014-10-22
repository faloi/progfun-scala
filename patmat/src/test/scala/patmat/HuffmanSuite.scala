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

  test("weight of a leaf") {
    assert(weight(Leaf('a', 8)) === 8)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a leaf") {
    assert(chars(Leaf('a', 8)) === List('a'))
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("makeCodeTree can create a CodeTree") {
    new TestTrees {
      val t2WithMake = makeCodeTree(
        makeCodeTree(Leaf('a', 2), Leaf('b', 3)),
        Leaf('d', 4)
      )

      assert(t2WithMake == t2)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times returns an empty list for an empty list") {
    assert(times(string2Chars("")) === List())
  }

  test("times returns the count of each character on the list") {
    assert(times(string2Chars("aabbbc")) === List(('c', 1), ('b', 3), ('a', 2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton of empty list is false") {
    assert(!singleton(List()))
  }

  test("singleton of list of N is false") {
    new TestTrees {
      assert(!singleton(List(t1, t2)))
    }
  }

  test("singleton of list of 1 is true") {
    new TestTrees {
      assert(singleton(List(t1)))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of singleton list returns the same list") {
    val leaflist = List(Leaf('e', 2))
    assert(combine(leaflist) === leaflist)
  }

  test("until reduces a list of trees to a tree") {
    assert(
      until(singleton, _.tail)(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4)))
      === List(Leaf('x', 4))
    )
  }

  test("createCodeTree makes the corresponding Huffman tree") {
    assert(createCodeTree(string2Chars("babcaa")) === makeCodeTree(Leaf('a', 3), makeCodeTree(Leaf('c', 1), Leaf('b', 2))))
  }

  test("decode makes a text from a bit array and a code tree") {
    new TestTrees {
      assert(decode(t1, List(1, 0)) === "ba".toList)
    }
  }

  test("decoded secret is really cool!") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("encode makes a bit array from a text and a code tree") {
    new TestTrees {
      assert(encode(t1)("ba".toList) === List(1, 0))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits returns a bitarray from a codeTable and a char") {
    val table = List(('d', List(0)), ('b', List(1, 1)), ('a', List(1, 0)))
    assert(codeBits(table)('b') === List(1, 1))
  }

  test("convert creates a CodeTable from a CodeTree") {
    new TestTrees {
      assert(convert(t2) == List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }
}
