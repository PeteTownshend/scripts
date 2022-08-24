import org.joda.time.{DateTime, DateTimeZone}

sealed trait Load
case object Peak extends Load
case object Base extends Load
case object Nope extends Load

val in = new Service("prd", Some(dsPrd), Some(cdsPrd)) with Containers with Markets with Lim2
val out = new Service("uat", Some(dsUat), Some(cdsUat)) with Containers with Markets with Lim2
implicit val wb = RISK_CALCULATOR
val cet = DateTimeZone.forID("Europe/Berlin")
val layout = "----------------------------------------------------------------------"

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
  var d = day - 1
  while (isHoliday(d)) d -= 1
  d
}

val tradingDate = formerWorkingDay(Day(now.withZone(cet)))
log info s"updating golden sources for trading date $tradingDate"
log info layout

val tradingMonth = Month(tradingDate)
val horizon = tradingDate + 59.months
val golden = out.Masterdataprices

def v[T](ot: Option[T])(error: String): V[Throwable, T] =
  ot.fold(new Throwable(error).failure[T])(_.success[Throwable])

def isPeak(dt: DateTime): Boolean = {
  val hour = Hour(dt).hour
  val day = Day(dt)
  !day.isWeekend && 8 <= hour && hour < 20
}

def vUpdate(goldenSource: DataTable, prices: Series[Month, Double]): V[Throwable, DataTable] =
  if (prices.isEmpty)
    Failure(new Throwable("no golden update because no proper prices provided"))
  else V {
    val row = goldenSource.newRow()
    row.setValue(1, tradingDate.toString("yyyy-MM-dd"))
    prices.values.zipWithIndex foreach { case (price, column) => row.setValue(column + 2, price) }
    goldenSource.addRow(row)
    goldenSource
  }

def vFormerPrices(goldenSource: DataTable): V[Throwable, (Day, Series[Month, Double])] = {
  val formerTradingDate = formerWorkingDay(tradingDate)
  if (goldenSource.isEmpty)
    (formerTradingDate, Series.empty[Month, Double]).success
  else V {
    val row = goldenSource.last
    val td = Day(row.getValue("TRADED_DATE").asInstanceOf[String])
    if (td < formerTradingDate) (td, Series.empty[Month, Double]) else {
      val m00 = Month(td)
      (td, (0 to 59).toSeries(c =>
        row.getValue(s"""M${"0" * (2 - c.toString.length)}$c""").asInstanceOf[Double]
      ).mapTime(m00 + _))
    }
  }
}

List(

  ("ng_ttf_fc", "mid", Markets.DUTCH, Nope, "ttfgaspriceshaped"),
  ("ng_ttf_fc", "mid_flat", Markets.DUTCH, Nope, "ttfgaspriceunshaped"),
  ("power_deu_fc", "mid", Markets.GERMANY, Base, "deupowpricebaseshaped"),
  ("power_deu_fc", "mid", Markets.GERMANY, Peak, "deupowpricepeakshaped"),
  ("power_deu_fc", "mid_flat", Markets.GERMANY, Base, "deupowpricebaseunshaped"),
  ("power_deu_fc", "mid_flat", Markets.GERMANY, Peak, "deupowpricepeakunshaped")

) foreach { case (containerName, bucketName, market, load, goldenSourceName) =>

  val zone = DateTimeZone.forID(market.timeZoneName)

  val vResult = for {

    masterdataprices <- v(golden get goldenSourceName)(s"golden source $goldenSourceName not available")
    source <- v(masterdataprices.dataTable)(s"data table of golden source $goldenSourceName is missing")
    _ <- V(log info s"got data table of golden source $goldenSourceName")
    (lastTradingDate, lastPrices) <- vFormerPrices(source)
    _ <- V(log info s"got last entry $lastTradingDate of golden source $goldenSourceName")
    _ <- V { if (lastTradingDate == tradingDate) throw new Throwable("traded day exists already") }
    container <- v(in get containerName)(s"forward curve container $containerName not available")
    bucket <- v(container get s"${bucketName}_$tradingDate")(s"forward curve bucket $bucketName not available")
    dataTable <- v(bucket.dataTable)(s"forward curve data table $bucketName not available")
    _ <- V(log info s"got data table of forward curve ${containerName}.$bucketName")
    oMthly <- V {
      val ts = Series(
        dataTable.toArray map { row =>
          val t = row.getValue("START_DATE").asInstanceOf[DateTime].withZone(zone)
          val x = row.getValue("VALUE").asInstanceOf[Double]
          (t, x)
        }
      )
      log info s"fwd prices starting at: ${ts.head}"
      log info s"           ending   at: ${ts.last}"
      log info s"           are regular: ${ts.mapTime(_.getMillis).isRegular}"
      val loaded = load.asInstanceOf[Load] match {
        case Peak =>
          log info "filtering for peak hours"
          ts.filterTime(isPeak)
        case _ =>
          ts
      }
      log info s"loaded prices starting at: ${loaded.head}"
      log info s"               ending  at: ${loaded.last}"
      val mthly = loaded.rollBy({ case (t, _) => Month(t) })(_.mean)
      log info s"monthly prices starting at: ${mthly.head}"
      log info s"               ending   at: ${mthly.last}"
      (tradingMonth to horizon) toSeries { t =>
        mthly.get(t) match {

          case None =>
            log warn s"got no price for $t"
            None

          case some =>
            some
        }
      }
    }
    if oMthly.exists(_._2.isDefined)
    _ <- V(log info s"got price update")
    _ <- V(if (true) oMthly.foreach(println))
    tail <- V(Interpolate.flatRight(oMthly))
    _ <- V(log info s"got prices flatRight")
    prices <- V {

      oMthly.takeWhile(_._2.isEmpty).time.toList match {

        case Nil =>
          log info "M00 provided by forward curve"
          tail

        case m00 :: Nil if lastPrices.isDefinedAt(m00) =>
          val lst = lastPrices(m00)
          log warn s"only M00 was missing and got assigned by fallback from former trading date: $m00 -> $lst"
          Series(m00 -> lst) ++ tail

        case m00 :: rem if lastPrices.isDefinedAt(m00) =>
          val lst = lastPrices(m00)
          log warn s"M00 was missing and got assigned by fallback from former trading date: $m00 -> $lst"
          log warn "additional values got filled up from right"
          Series(m00 -> lst) ++ rem.toSeries(_ => tail.firstValue) ++ tail

        case _ =>
          throw new Throwable(s"as fall back no former M00 given from former trading date ")
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

  log info layout
}