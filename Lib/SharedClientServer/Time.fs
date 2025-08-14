module Lib.Time

open System

[<Measure>]
type minute

[<Measure>]
type second

module DateTime =
   let isToday (date: DateTime) =
      let date = date.ToLocalTime().Date
      let today = DateTime.Now.Date
      date = today

   let isThisMonth (date: DateTime) =
      let date = date.ToLocalTime()
      let today = DateTime.Now
      (today.Month, today.Year) = (date.Month, date.Year)

   let parseOptional (date: string) =
      try
         Some
         <| DateTime.Parse(
            date,
            null,
            Globalization.DateTimeStyles.AdjustToUniversal
         )
      with _ ->
         None

   let numberToDisplayMonth =
      Map [
         1, "Jan"
         2, "Feb"
         3, "Mar"
         4, "Apr"
         5, "May"
         6, "Jun"
         7, "Jul"
         8, "Aug"
         9, "Sep"
         10, "Oct"
         11, "Nov"
         12, "Dec"
      ]

   /// 5 = 5th; 31 = 31st
   let dayWithOrdinal (day: int) =
      let suffix =
         match day with
         | 11
         | 12
         | 13 -> "th"
         | _ ->
            match day % 10 with
            | 1 -> "st"
            | 2 -> "nd"
            | 3 -> "rd"
            | _ -> "th"

      string day + suffix

   let dayOfWeekDisplay (dayOfWeek: System.DayOfWeek) =
      match dayOfWeek with
      | DayOfWeek.Sunday -> "Sunday"
      | DayOfWeek.Monday -> "Monday"
      | DayOfWeek.Tuesday -> "Tuesday"
      | DayOfWeek.Wednesday -> "Wednesday"
      | DayOfWeek.Thursday -> "Thursday"
      | DayOfWeek.Friday -> "Friday"
      | _ -> "Saturday"

   let dayOfWeekDisplayShort = dayOfWeekDisplay >> _.Substring(0, 3)

   let formatShort (date: DateTime) =
      $"{numberToDisplayMonth[date.Month]} {date.Day}"

   let formatShortWithDayOfWeek (date: DateTime) =
      let day = dayOfWeekDisplay date.DayOfWeek |> _.Substring(0, 3)
      formatShort date + $" {day}"

   let format (date: DateTime) = formatShort date + $", {date.Year}"

   let formatRangeShort (dateStart: DateTime) (dateEnd: DateTime) =
      let withYear =
         dateStart.Year <> dateEnd.Year
         || dateStart.Year <> DateTime.UtcNow.Year
         || dateEnd.Year <> DateTime.UtcNow.Year

      let format (date: DateTime) =
         let formatted = $"{date.Month}/{date.Day}"
         if withYear then $"{formatted}/{date.Year}" else formatted

      format dateStart, format dateEnd

   let asEndOfDayUtc (date: DateTime) =
      let utc = date.ToUniversalTime()
      DateTime(utc.Year, utc.Month, utc.Day, 23, 59, 59, DateTimeKind.Utc)

   let futureTimeUIFriendly (futureDate: DateTime) =
      let time = futureDate.ToUniversalTime() - DateTime.UtcNow
      let futureDate = futureDate.ToLocalTime()

      if time.TotalDays >= 1.0 then
         if time.TotalDays < 7.0 then
            $"{floor time.TotalDays} days"
         else
            formatShort futureDate
      elif time.TotalHours >= 1.0 then
         let hours = int time.TotalHours
         let timeUnit = if hours > 1 then "hours" else "hour"
         $"{hours} {timeUnit}"
      elif time.TotalMinutes >= 1.0 then
         let minutes = int time.TotalMinutes
         let timeUnit = if minutes > 1 then "minutes" else "minute"
         $"{minutes} {timeUnit}"
      elif time.TotalSeconds >= 1.0 then
         let seconds = int time.TotalSeconds
         let timeUnit = if seconds > 1 then "seconds" else "second"
         $"{seconds} {timeUnit}"
      else
         "now"
