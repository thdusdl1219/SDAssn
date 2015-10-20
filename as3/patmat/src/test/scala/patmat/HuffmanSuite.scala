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
  test("createCodeTree & decode & encode test") {
    val l = string2Chars("hello, world")
    val r = createCodeTree(l)
    assert(decode(r,(encode(r)(l))) === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))

  }



  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("frenchtest"){
    new TestTrees{
      assert(encode(frenchCode)(decodedSecret) === secret)
    }
  }
  test ("Given Decoding Test") {
    assert(Huffman.decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }
  test("quicktest") {
    new TestTrees {
      val tree = createCodeTree("ThisisTest".toList)
      assert(decode(tree, quickEncode(tree)("ThisisTest".toList)) === "ThisisTest".toList)
    }
  }
  test("encode, quickencode test"){//park sy's case has more steps than others' frenchCode case
    assert(quickEncode(frenchCode)(decodedSecret) === List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1))
    assert(encode(frenchCode)(decodedSecret) === List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1))
  }


  val codelorem = createCodeTree(string2Chars("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed iaculis, odio sed consequat porttitor, lorem lacus varius odio, quis elementum diam metus sit amet est. Morbi dignissim semper velit, at viverra massa facilisis eu. Ut eget varius risus, vel cursus sem. Cras odio elit, semper eu ipsum nec, ornare consectetur velit. Morbi fermentum ipsum vel velit sodales luctus. Fusce posuere in arcu at ullamcorper. Vestibulum sed mauris finibus, finibus lectus eu, vestibulum nulla. Donec eu enim iaculis odio tristique finibus vitae in metus. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus velit dolor, vestibulum sit amet rutrum sit amet, suscipit in leo. Nunc ultrices a mauris volutpat congue. Quisque blandit urna id venenatis eleifend j k w x y z"))

  test ("Given Decode and encoding - hardtoeatandlive") {
    assert (decode(frenchCode,encode(frenchCode)("hardtoeatandlive".toList)) === "hardtoeatandlive".toList)
  }
  test ("Given Decode and quickencoding - hardtoeatandlive") {
    assert (decode(frenchCode,quickEncode(frenchCode)("hardtoeatandlive".toList)) === "hardtoeatandlive".toList)
  }
  test("lorem testing both quick and normal - hardtoeatandlive"){
    assert (decode(codelorem,encode(codelorem)("hardtoeatandlive".toList)) === "hardtoeatandlive".toList)
    assert (decode(codelorem,quickEncode(codelorem)("hardtoeatandlive".toList)) === "hardtoeatandlive".toList)
  }
  //end of sldhsmf's testcase
}
