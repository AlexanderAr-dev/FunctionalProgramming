import scala.io.Source
import scala.util.{Try, Success, Failure}

object WordFilterApp extends App {
  // Проверка параметров командной строки
  if (args.length != 2) {
    Console.err.println("Ошибка: требуется два параметра: путь к файлу и буква для фильтрации.")
    System.exit(1)
  }

  val filePath = args(0)
  val filterLetter = args(1)

  // Проверка, что вторая строка - это одиночная буква
  if (filterLetter.length != 1 || !filterLetter.matches("[a-zA-Z]")) {
    Console.err.println("Ошибка: второй параметр должен быть одной буквой.")
    System.exit(1)
  }

  // Функция для чтения файла
  def readFile(filePath: String): Try[String] = {
    Try {
      Source.fromFile(filePath).getLines().mkString("\n")
    }
  }

  // Функция для получения слов, заканчивающихся на заданную букву
  def getWordsEndingWithLetter(text: String, letter: String): List[String] = {
    val words = text.split("\\s+").filter(word => word.nonEmpty && word.endsWith(letter))
    words.map(_.toLowerCase).toList.sorted
  }

  // Чтение и обработка файла
  readFile(filePath) match {
    case Success(text) =>
      val words = getWordsEndingWithLetter(text, filterLetter.toLowerCase)
      if (words.isEmpty) {
        Console.err.println(s"Нет слов, заканчивающихся на букву '$filterLetter'.")
        System.exit(1)
      } else {
        words.foreach(println)
      }
    case Failure(exception) =>
      Console.err.println(s"Ошибка при чтении файла: ${exception.getMessage}")
      System.exit(1)
  }
}
