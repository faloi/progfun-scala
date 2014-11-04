package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(3,9)), "3,9")
      assert(!terrain(Pos(3,10)), "3,10")
      assert(!terrain(Pos(6,4)), "6,4")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("Block: isStanding") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isStanding, "Standing")
      assert(!Block(Pos(1,2), Pos(1,3)).isStanding, "Not standing")
    }
  }

  test("Block: isLegal") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isLegal, "Start point is legal")
      assert(!Block(Pos(3,9), Pos(3,10)).isLegal, "One cube outside the terrain is ilegal")
    }
  }

  test("GameDef: startBlock") {
    new Level1 {
      assert(startBlock === Block(Pos(1,1), Pos(1,1)))
    }
  }

  test("Block: neighbors") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).neighbors === List(
        (Block(Pos(1, -1), Pos(1, 0)), Left),
        (Block(Pos(1, 2), Pos(1, 3)), Right),
        (Block(Pos(-1, 1), Pos(0, 1)), Up),
        (Block(Pos(2, 1), Pos(3, 1)), Down)
      ))
    }
  }

  test("Block: legalNeighbors") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).legalNeighbors === List(
        (Block(Pos(1, 2), Pos(1, 3)), Right),
        (Block(Pos(2, 1), Pos(3, 1)), Down)
      ))
    }
  }

  ignore("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  ignore("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
