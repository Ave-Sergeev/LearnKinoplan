
object sj extends App{
  val grid2 = Array(
    Array(1, 1, 0, 0, 0),
    Array(1, 0, 0, 0, 0),
    Array(0, 0, 0, 0, 0),
    Array(0, 0, 0, 1, 1)
  )

  println(s"Task 15 = ${solution15(grid2)}")
  // Task 15 = 3


  case class Cord(x: Int, y: Int)

  //площадь острова(сколько содержит единиц)
  def solution15(grid: Array[Array[Int]]): Int = {
    var bestMaxArea = 0
    var currentArea = 0
    val xLen = grid.length
    val yLen = grid.headOption.map(_.length).getOrElse(0)
    var allLandCords = scala.collection.mutable.Set.empty[Cord]
    val directions = Seq(Cord(0,1),Cord(0,-1),Cord(1,0),Cord(-1,0))
    def isLandAndNew(x: Int, y: Int) =
      !(x < 0 || y < 0 || x >= xLen || y >= yLen) &&  // is inbounds
        grid(x)(y) == 1 && // is Land
        !allLandCords(Cord(x, y)) // is first seen Land by cords

    def dfs(x: Int, y: Int): Unit = {
      allLandCords.add(Cord(x, y))
      currentArea += 1
      directions.foreach { dir =>
        if (isLandAndNew(x + dir.x, y + dir.y)) dfs(x + dir.x, y + dir.y)
      }
    }

    (0 until yLen).foreach { y =>
      (0 until xLen).foreach { x =>
        if (isLandAndNew(x,y)) {
          dfs(x,y)
          if (currentArea > bestMaxArea) bestMaxArea = currentArea
          currentArea = 0
        }
      }
    }

    bestMaxArea
  }
}
