import scala.math._

sealed trait Figure

case class Circle(x: Double, y: Double, r: Double) extends Figure
case class Rectangle(x1: Double, y1: Double, x2: Double, y2: Double) extends Figure
case class Triangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) extends Figure

object FigureUtils {

  // a. Вычисление площади фигуры
  def area(f: Figure): Double = f match {
    case Circle(_, _, r) => Pi * pow(r, 2)
    case Rectangle(x1, y1, x2, y2) => abs((x2 - x1) * (y2 - y1))
    case Triangle(x1, y1, x2, y2, x3, y3) =>
      val a = sqrt(pow(x2 - x1, 2) + pow(y2 - y1, 2))
      val b = sqrt(pow(x3 - x2, 2) + pow(y3 - y2, 2))
      val c = sqrt(pow(x3 - x1, 2) + pow(y3 - y1, 2))
      val p = (a + b + c) / 2
      sqrt(p * (p - a) * (p - b) * (p - c))
  }

  // b. Выбор только прямоугольников
  def getRectangles(figures: List[Figure]): List[Rectangle] =
    figures.collect { case r: Rectangle => r }

  // c. Ограничивающий прямоугольник для одной фигуры
  def getBound(f: Figure): Rectangle = f match {
    case Circle(x, y, r) => Rectangle(x - r, y - r, x + r, y + r)
    case r: Rectangle => r
    case Triangle(x1, y1, x2, y2, x3, y3) =>
      Rectangle(min(min(x1, x2), x3), min(min(y1, y2), y3), max(max(x1, x2), x3), max(max(y1, y2), y3))
  }

  // d. Ограничивающий прямоугольник для списка фигур
  def getBounds(figures: List[Figure]): Option[Rectangle] = {
    val bounds = figures.map(getBound)
    if (bounds.isEmpty) None
    else Some(Rectangle(
      bounds.map(_.x1).min, bounds.map(_.y1).min,
      bounds.map(_.x2).max, bounds.map(_.y2).max
    ))
  }

  // e. Поиск первой фигуры, содержащей точку в ее bounding box
  def getFigure(figures: List[Figure], px: Double, py: Double): Option[Figure] =
    figures.find(f => {
      val r = getBound(f)
      px >= r.x1 && px <= r.x2 && py >= r.y1 && py <= r.y2
    })

  // f. Сдвиг фигуры на вектор (dx, dy)
  def move(f: Figure, dx: Double, dy: Double): Figure = f match {
    case Circle(x, y, r) => Circle(x + dx, y + dy, r)
    case Rectangle(x1, y1, x2, y2) => Rectangle(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
    case Triangle(x1, y1, x2, y2, x3, y3) =>
      Triangle(x1 + dx, y1 + dy, x2 + dx, y2 + dy, x3 + dx, y3 + dy)
  }
}

// Тестовые данные
object Main extends App {
  val figures: List[Figure] = List(
    Circle(3, 3, 2),
    Rectangle(1, 1, 4, 4),
    Triangle(0, 0, 2, 3, 4, 1)
  )

  println("Areas:")
  figures.foreach(f => println(s"${f}: ${FigureUtils.area(f)}"))

  println("\nOnly Rectangles:")
  println(FigureUtils.getRectangles(figures))

  println("\nBounds of all figures:")
  println(FigureUtils.getBounds(figures))

  println("\nFind figure containing point (2,2):")
  println(FigureUtils.getFigure(figures, 2, 2))

  println("\nMove figures by (2,3):")
  println(figures.map(f => FigureUtils.move(f, 2, 3)))
}
