
object TimeCalc {
  case class ClockTime24(hour: Int, minute: Int)

  val totalHoursInADay = 24
  val totalMinutesInAnHour = 60
  val totalMinutesInADay = totalHoursInADay * totalMinutesInAnHour

  def formatClockTime24(clockTime24 : ClockTime24) : String = {
    val minuteStr = if (clockTime24.minute < 10) s"0${clockTime24.minute}" else s"${clockTime24.minute}"
    if( clockTime24.hour >= 0 && clockTime24.hour < 12){
      s"${clockTime24.hour}:$minuteStr AM"
    }
    else{
      if(clockTime24.hour==12) s"${clockTime24.hour}:${minuteStr} PM"
      else s"${clockTime24.hour-12}:${minuteStr} PM"
    }
  }

  def AddMinutes(timeStr: String, minutes: Int) : String = {
    //asserting minutes will always be a positive number
    assert(minutes >= 0)
    val clockTime24format = parseClockTime(timeStr)
    val truncatedMinutes = minutes % totalMinutesInADay
    val hoursToAdd = truncatedMinutes / totalMinutesInAnHour
    val minutesToAdd = truncatedMinutes % totalMinutesInAnHour
    val newClockTime24format = ClockTime24((clockTime24format.hour + hoursToAdd) % 24, clockTime24format.minute + minutesToAdd)
    formatClockTime24(newClockTime24format)
  }

  def parseClockTime(strTime: String): ClockTime24 = {
    val splittedString = strTime.split("[\\s:]+")
    //precondition that we need both the hour, minute , and ampm part of the string
    assert(splittedString.length == 3)

    var hour = splittedString(0).toInt
    val minute = splittedString(1).toInt
    val ampm = splittedString(2)

    assert(hour >= 0 && hour <= 12)
    assert(minute >= 0 && minute < 60)
    assert(ampm == "AM" || ampm == "PM")
    //change 12AM into 0 AM instead.
    if(ampm == "AM" && hour == 12) hour = 0

    if (ampm == "PM" && hour < 12) {
      val pmHour = hour + 12
      ClockTime24(pmHour, minute)
    } else { //AM case
      ClockTime24(hour, minute)
    }
  }

  def main(args: Array[String]): Unit = {
    val time0 = "9:13 AM"
    println("initial Time = " + time0)
    val minutes0 = 200
    println("minutes we add = " + minutes0)
    val timeRes0 = AddMinutes(time0, minutes0)
    println("result time = " + timeRes0)

    val time1 = "9:13 PM"
    println("initial Time = " + time1)
    val minutes1 = 200
    println("minutes we add = " + minutes1)
    val timeRes1 = AddMinutes(time1, minutes1)
    println("result time = " + timeRes1)

    val time2 = "12:13 PM"
    println("initial Time = " + time2)
    val minutes2 = 200
    println("minutes we add = " + minutes2)
    val timeRes2 = AddMinutes(time2, minutes2)
    println("result time = " + timeRes2)

    val time3 = "0:13 AM"
    println("initial Time = " + time3)
    val minutes3 = 200
    println("minutes we add = " + minutes3)
    val timeRes3 = AddMinutes(time3, minutes3)
    println("result time = " + timeRes3)

    val time4 = "0:00 AM"
    println("initial Time = " + time4)
    val minutes4 = 1660
    println("minutes we add = " + minutes4)
    val timeRes4 = AddMinutes(time4, minutes4)
    println("result time = " + timeRes4)

    val time5 = "0:00 AM"
    println("initial Time = " + time5)
    val minutes5 = 1440
    println("minutes we add = " + minutes5)
    val timeRes5 = AddMinutes(time5, minutes5)
    println("result time = " + timeRes5)

    val time6 = "12:00 PM"
    println("initial Time = " + time6)
    val minutes6 = 1440
    println("minutes we add = " + minutes6)
    val timeRes6 = AddMinutes(time6, minutes6)
    println("result time = " + timeRes6)

    val time7 = "0:00 AM"
    println("initial Time = " + time7)
    val minutes7 = 1
    println("minutes we add = " + minutes7)
    val timeRes7 = AddMinutes(time7, minutes7)
    println("result time = " + timeRes7)

    println("end")

  }
}
