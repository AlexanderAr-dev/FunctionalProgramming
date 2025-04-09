import scala.annotation.tailrec
import scala.math._

object RawLib {
  def myExpNeg(x: Double, e: Double): (Double, Int) = {

    // Функция для вычисления факториала с хвостовой рекурсией
    @tailrec
    def factorial(n: Int, acc: BigInt = 1): BigInt = {
      if (n <= 1) acc
      else factorial(n - 1, acc * n)
    }

    @tailrec
    def trLoop(res: Double, i: Int): (Double, Int) = {
      val term = pow(x, i) / factorial(i).toDouble  // Используем факториал в знаменателе
      if (i >= 500 || abs(term) < e) return (res, i)

      val sign = if (i % 2 == 0) 1 else -1  // Чередуем знаки
      trLoop(res + sign * term, i + 1)
    }

    trLoop(1.0, 1)  // Начинаем с 1, так как первый член ряда — 1
  }
}

object Main extends App {

  val x1 = -0.9; val x2 = 0.9; val step = 0.1

  def roundAt(p: Int)(n: Double): Double = { val s = pow (10, p); round(n * s) / s }
  println(f"  x         f(x)        Taylor(x)  TI (Taylor Iterations)")

  @tailrec
  def trLoop(x: Double) : Unit = {
    val funRes = scala.math.exp(-x)
    val rawRes = RawLib.myExpNeg(x, 0.000001)
    println(f"$x% 2.2f $funRes% 2.10f ${rawRes._1}% 2.10f ${rawRes._2}%3d")
    if ( x > x2 ) return
    trLoop(roundAt(4)(x + step))
  }

  trLoop(x1)
}
