import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.io.Source

object Solver {

  def main(args: Array[String]) {
    readFile(args(0)) match {
      case Success(arr) => solve(arr)
      case Failure(ex) => println(ex.getMessage)
    }
    // println(isSolved(Map(0->1,1->2)))
  }

  def readFile(filename: String): Try[List[List [String]]] = {
    val lines = Source.fromURL(getClass().getResource(filename)).getLines
    val res = lines.map(_.split("").drop(1).toList)
    Try(res.toList)
  }

  def presolve(grid: List[List[String]]): List[List[String]] {

    null
  }

  def solve(grid: List[List[String]]) = {
    println("is solved: " + isSolved(grid))
    printGrid(grid)
    // go through the grid and generate all possible lines
    val grids = grid.zipWithIndex.map{lineAndIndex =>
      val line = lineAndIndex._1
      println(s"solving: $line")
      val numbers = (1 to 9).filterNot(line.filterNot(_ == ".").map(_.toInt).contains(_))
      println(s"possible numbers: $numbers")
      val arrangements = numbers.permutations.toSeq
      val arrangementsSize = arrangements.size
      println(s"possible arrangements: $arrangementsSize")
      val newarr = arrangements.map{arrangement =>
        val it = arrangement.iterator
        line.map{elem => if (elem != ".") elem else it.next}
      }.filter{line =>
        println(line)
        // filter lines which are not possible from the given grid
        line.zipWithIndex.foldLeft(true){(r,c) =>
          // check every number in the possible line for matches in the grid
          val gridWithoutCurrentLine = grid.patch(lineAndIndex._2, Nil, 1);
          val res = gridWithoutCurrentLine.map{lineFromGrid =>
            lineFromGrid(c._2) != c._1.toString
          }
          res.reduce(_&&_)
        }
      }
      val sizeofarr = newarr.size
      println(s"filled arrangements: $sizeofarr")
      newarr
    }
    grids.foreach{elem => println(elem.size)}
    val n = grids.foldLeft(1.toLong){(r,c) => if(c.size == 0) r else r*c.size}
    println(s"possible arrangements: $n")
  }

  def isSolved(grid: List[List[String]]): Boolean = {
    val u1 = isUniqueGrid(grid)

    val rotatedGrid = rotate2dList(grid)
    val u2 = isUniqueGrid(rotatedGrid)

    val smallGrids = splitGrids(grid, 3)
    val u3 = smallGrids.map(isUniqueGrid).reduce(_&&_)
    println(smallGrids)

    u1 && u2 && u3
  }

  def rotate2dList(grid: List[List[Any]]): List[List[Any]] = {
    grid.zipWithIndex.map{ x =>
      val verticalLine = x._1.zipWithIndex.map{ y =>
        grid(y._2)(x._2)
      }
      verticalLine.toList
    }
  }

  def splitGrids(grid: List[List[Any]], size: Int): List[List[List[Any]]] = {
    (0 until grid.size by size).map{y =>
      (0 until grid(y).size by size).map{x =>
        (y until y+size).map{y2 =>
          (x until x+size).map{x2 =>
            grid(y2)(x2)
          }.toList
        }.toList
      }
    }.reduce(_++_).toList
  }

  def isUniqueGrid(grid: List[List[Any]]): Boolean = {
    grid.map{line => line.distinct.size == line.size}.reduce(_&&_)
  }

  def printGrid(grid: List[List[Any]]) = {
    grid.foreach{line =>
      line.foreach{char =>
        print(char + ",")
      }
      println()
    }
  }
}
