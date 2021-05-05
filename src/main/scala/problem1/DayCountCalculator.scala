package problem1

import java.time.{DayOfWeek, LocalDateTime}
import scala.annotation.tailrec

object DayCountCalculator extends App {

  val century = 1901 to 2000

  @tailrec
  def calculateDayOfWeekFor(century: Range, numDay: Int = 0): Int = {
    if (century.isEmpty) {
      numDay
    } else {
      calculateDayOfWeekFor(century.tail, numDay + countWeekOfDayFor(century.head))
    }
  }

  def countWeekOfDayFor(year: Int): Int = {
    val months = 1 to 12
    months.count(month => isFirstDayOfMonthSunday(month, year))
  }

  def isFirstDayOfMonthSunday(month: Int, year: Int): Boolean = {
    val day = 1
    LocalDateTime.of(year, month, day, 0, 0).getDayOfWeek == DayOfWeek.SUNDAY
  }

  println(s"Number of Sunday as first day of month in $century is ${calculateDayOfWeekFor(century)}")
}
