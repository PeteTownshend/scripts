import org.joda.time.{DateTime, DateTimeZone}

sealed trait Load
case object Peak extends Load
case object Base extends Load
case object Nope extends Load

val in = new Service("prd", Some(dsPrd), Some(cdsPrd)) with Containers with Markets with Lim2
val out = new Service("uat", Some(dsUat), Some(cdsUat)) with Containers with Markets with Lim2
implicit val wb = RISK_CALCULATOR

def isHoliday(day: Day): Boolean = day.isWeekend || List(
  Day(2022, 1, 3),
  Day(2022, 4, 14),
  Day(2022, 4, 15),
  Day(2022, 4, 18),
  Day(2022, 5, 2),
  Day(2022, 5, 17),
  Day(2022, 5, 26),
  Day(2022, 6, 2),
  Day(2022, 6, 3),
  Day(2022, 6, 6),
  Day(2022, 8, 29),
  Day(2022, 10, 3),
  Day(2022, 10, 31),
  Day(2022, 12, 26),
  Day(2022, 12, 27),
  Day(2023, 1, 2),
  Day(2023, 4, 6),
  Day(2023, 4, 7),
  Day(2023, 4, 10),
  Day(2023, 5, 1),
  Day(2023, 5, 17),
  Day(2023, 5, 18),
  Day(2023, 5, 29),
  Day(2023, 8, 28),
  Day(2023, 10, 3),
  Day(2023, 12, 25),
  Day(2023, 12, 26)
).contains(day)

def formerWorkingDay(day: Day): Day = {
  var d = day
  while (isHoliday(d)) d -= 1
  d
}

val tradingDate = formerWorkingDay(yesterday)
val tradingMonth = Month(tradingDate)
val horizon = tradingDate + 59.months
val golden = out.Masterdataprices
val inOverrideMode = true

def v[T](ot: Option[T])(error: String): V[Throwable, T] =
  ot.fold(new Throwable(error).failure[T])(_.success[Throwable])

def isPeak(dt: DateTime): Boolean = {
  val hour = Hour(dt).hour
  val day = Day(dt)
  !day.isWeekend && 8 <= hour && hour < 20
}

def vUpdate(goldenSource: DataTable, prices: Series[Month, Double]): V[Throwable, DataTable] = V {
  val row = goldenSource.newRow()
  row.setValue(1, tradingDate.toString("yyyy-MM-dd"))
  prices.values.zipWithIndex foreach { case (price, column) => row.setValue(column + 2, price) }
  goldenSource.addRow(row)
  goldenSource
}

def formerPrices(goldenSource: DataTable): V[Throwable, (Day, Series[Month, Double])] = V {
  val row = goldenSource.last
  val td = Day(row.getValue("TRADED_DATE").asInstanceOf[String])
  if (td < formerWorkingDay(tradingDate)) (td, Series.empty[Month, Double]) else {
    val m00 = Month(td)
    (td, (0 to 59).toSeries(c =>
      row.getValue(s"""M${"0" * (2 - c.toString.length)}$c""").asInstanceOf[Double]
    ).mapTime(m00 + _))
  }
}

if (isHoliday(tradingDate))
  log info s"golden sources won't be updated because $tradingDate is not a trading date"
else {
  log info s"updating golden sources for trading date $tradingDate"

  List(

    ("ng_ttf_fc", "mid", Markets.DUTCH, Nope, "ttfgaspriceshaped"),
    ("ng_ttf_fc", "mid_flat", Markets.DUTCH, Nope, "ttfgaspriceunshaped"),
    ("power_deu_fc", "mid", Markets.GERMANY, Base, "deupowpricebaseshaped"),
    ("power_deu_fc", "mid", Markets.GERMANY, Peak, "deupowpricepeakshaped"),
    ("power_deu_fc", "mid_flat", Markets.GERMANY, Base, "deupowpricebaseunshaped"),
    ("power_deu_fc", "mid_flat", Markets.GERMANY, Peak, "deupowpricepeakunshaped")

  ) foreach { case (containerName, bucketName, market, load, goldenSourceName) =>

    val vResult = for {

      masterdataprices <- v(golden get goldenSourceName)(s"golden source $goldenSourceName not available")
      source <- v(masterdataprices.dataTable)(s"data table of golden source $goldenSourceName is missing")
      _ <- V(log info s"got data table of golden source $goldenSourceName")
      (lastTradingDate, lastPrices) <- formerPrices(source)
      _ <- V(log info s"got last entry $lastTradingDate of golden source $goldenSourceName")
      _ <- V { if (lastTradingDate == tradingDate && !inOverrideMode) throw new Throwable("traded day exists already") }
      container <- v(in get containerName)(s"forward curve container $containerName not available")
      bucket <- v(container get s"${bucketName}_$tradingDate")(s"forward curve bucket $bucketName not available")
      dataTable <- v(bucket.dataTable)(s"forward curve data table $bucketName not available")
      _ <- V(log info s"got data table of forward curve ${containerName}.$bucketName")
      prices <- V {
        val zone = DateTimeZone.forID(market.timeZoneName)
        val ts = Series(
          dataTable.toArray map { row =>
            val t = row.getValue("START_DATE").asInstanceOf[DateTime].withZone(zone)
            val x = row.getValue("VALUE").asInstanceOf[Double]
            (t, x)
          }
        )
        val loaded = load match {
          case Peak => ts.filterTime(isPeak)
          case _ => ts
        }
        val mthly = loaded.rollBy({ case (t, _) => Month(t) })(_.mean)
        val oMthly = (tradingMonth to horizon).toSeries(mthly.get)
        val tail = Interpolate.flatRight(oMthly)
        oMthly.takeWhile(_._2.isEmpty).time.toList match {

          case Nil =>
            tail

          case m00 :: Nil if lastPrices.isDefinedAt(m00) =>
            Series(m00 -> lastPrices(m00)) ++ tail

          case m00 :: rem if lastPrices.isDefinedAt(m00) =>
            Series(m00 -> lastPrices(m00)) ++ rem.toSeries(_ => tail.firstValue) ++ tail

          case _ =>
            throw new Throwable(s"as fall back no former M00 given from former trading date")
        }
      }
      _ <- V(log info "derived monthly prices from forward curve container")
      newSource <- vUpdate(source, prices)
      result <- V(masterdataprices.writeToCds(newSource))

    } yield result

    vResult match {

      case Success(_) =>
        log info s"updated $goldenSourceName"

      case Failure(throwable: Throwable) =>
        log error s"failed to update $goldenSourceName due to ${throwable.getMessage}"
    }
    
    log info "----------------------------------------------------------------------"
  }
}