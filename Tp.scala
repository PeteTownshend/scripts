import scala.collection.SortedSet

// ###############################################################################################
// ### SETTINGS
// ###

val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim

val asof = current[Month]

implicit val wb = FO_SUPPORT
val layout = "#" * 80

import srv.Markets._

val globalBankHolidays = SortedSet.empty[Day]

// ###############################################################################################
// ### CURVES
// ###

object srv_ {

  object NgNcgFc extends srv.Container("NG_NCG_FC") with srv.ForwardCurveContainer {

    object HerenMid extends ForwardCurveStream[Day, Day]("HEREN_MID_%s")
  }

  object NgPegFc extends srv.Container("NG_PEG_FC") with srv.ForwardCurveContainer {

    object NordHerenMid extends ForwardCurveStream[Day, Day]("NORD_HEREN_MID_%s")
  }

  object PowerDeuFc extends srv.Container("POWER_DEU_FC") with srv.ForwardCurveContainer {

    object EexMid extends ForwardCurveStream[Day, Hour]("EEX_MID_%s")
  }

  object PowerFraFc extends srv.Container("POWER_FRA_FC") with srv.ForwardCurveContainer {

    object EexMid extends ForwardCurveStream[Day, Hour]("EEX_MID_%s")
  }

  object TransferpricesCont extends srv.Container("TRANSFERPRICES_CONT") with srv.ForwardCurveContainer {

    object EurUsd extends ForwardCurveStream[Month, Day ]("EUR_USD_MID_%s")
    object EuaSpr extends ForwardCurveStream[Month, Day ]("SPREAD_EUA_%s")
    object DeuSpr extends ForwardCurveStream[Month, Hour]("SPREAD_DEU_%s")
    object NcgSpr extends ForwardCurveStream[Month, Day ]("SPREAD_NCG_%s")
    object FraSpr extends ForwardCurveStream[Month, Hour]("SPREAD_FRA_%s")
    object ApiAsk extends ForwardCurveStream[Month, Day ]("COAL_API2_ASK_EUR_%s")
    object EuaAsk extends ForwardCurveStream[Month, Day ]("CARBON_EUA_ASK_%s")
    object DeuBid extends ForwardCurveStream[Month, Hour]("POWER_DEU_BID_%s")
    object NcgAsk extends ForwardCurveStream[Month, Day ]("NG_NCG_ASK_%s")
    object FraBid extends ForwardCurveStream[Month, Hour]("POWER_FRA_BID_%s")
    object PegAsk extends ForwardCurveStream[Month, Day ]("NG_PEG_NORD_ASK_%s")
  }
}

// ###############################################################################################
// ### HELPER
// ###

def tryOption[T](t: => T): Option[T] = try Some(t) catch {

  case t: Throwable =>
    log error t.getMessage
    None
}

def calcTp[T <: DateTimeLike[T]](
                                  tpName: String,
                                  market: MarketLike with Market
                                )(addBH: SortedSet[Day] = SortedSet.empty[Day])(
                                  seriesStream: Day => Option[Series[T, Double]]
                                ): Option[Series[T, Double]] = try {

  log info layout
  log info "### %s".format(tpName)
  log info "###"

  val holidays = market.specialDays ++ globalBankHolidays ++ addBH

  def mean(month: Month, nbrOfDays: Int) = {
    val wds = month.toDays filterNot { _.isWeekend } diff holidays takeRight nbrOfDays
    val tss = (wds :\ List.empty[Series[T, Double]]) {
      (wd, l) => seriesStream(wd) match {
        case Some(ts) => ts :: l
        case None =>
          log error "%s is missing".format(wd)
          l
      }
    }
    tss.reduceLeft{ (_: Series[T, Double]) + (_: Series[T, Double]) } / tss.size.toDouble
  }

  Some {
    mean(asof - 2.months, 4).filterTime(Month(_) == asof - month) ++
      mean(asof - 1.months, 31).filterTime(Month(_) >= asof)
  }

} catch { case ex: Throwable =>

  log error "%s failed due to %s".format(tpName, ex.getMessage)
  None
}

def hourly(base: Double, peak: Double, hrs: SortedSet[Hour]) = {
  def isPeak(h: Hour) = !Day(h).isWeekend && 8 <= h.hour && h.hour < 20
  val nbrPeak = hrs count isPeak
  val nbrBase = hrs.size
  val offPeak = (nbrBase * base - nbrPeak * peak) / (nbrBase - nbrPeak)
  hrs toSeries { h => if (isPeak(h)) peak else offPeak }
}

def fillFront(series: Series[Day, Double]): Series[Day, Double] = {
  val const = series.firstValue
  series ++ (Month(series.start).firstOfMonth until series.start).toSeries(_ => const)
}

// ###############################################################################################
// ### FX
// ###

lazy val eurusd = calcTp("EUR/USD", GERMANY)() { srv.Reuters1730.EurUsdMid.get(_).flatMap(_.series) }

// ###############################################################################################
// ### SPREADS
// ###

lazy val euaSpr = tryOption {

  val start = asof - month

  (Day(start) to Day(Year(start) + 4.years) - 1) toSeries {

    case d if Year(d) <= Year(start) => 0.02
    case d if Year(d) == Year(start) + 1.years => 0.03
    case d if Year(d) <= Year(start) + 2.years => 0.04
    case d if Year(d) <= Year(start) + 5.years => 0.10
  }
}

lazy val deuSpr = tryOption {

  val start = asof - month

  (start until start + 5.years).foldLeft(Series.empty[Hour, Double]) {


    case (acc, m) if m <= asof + 1.months =>
      acc ++ hourly(0.15, 0.35, Hour(m) until Hour(m + 1.months))

    case (acc, m) if m == asof + 2.months =>
      acc ++ hourly(0.25, 0.50, Hour(m) until Hour(m + 1.months))

    case (acc, m) if m == asof + 3.months =>
      acc ++ hourly(0.35, 0.75, Hour(m) until Hour(m + 1.months))

    case (acc, m) if Quarter(m) == 2020.Q3 =>
      acc ++ hourly(0.10, 0.40, Hour(Quarter(m)) until Hour(Quarter(m) + 1.quarters))

    case (acc, m) if Quarter(m) == 2020.Q4 =>
      acc ++ hourly(0.15, 0.50, Hour(Quarter(m)) until Hour(Quarter(m) + 1.quarters))

    case (acc, m) if Year(m) <= Year(2021) =>
      acc ++ hourly(0.05, 0.25, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2022) =>
      acc ++ hourly(0.12, 0.38, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2023) =>
      acc ++ hourly(0.22, 0.48, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2024) =>
      acc ++ hourly(0.25, 0.50, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2025) =>
      acc ++ hourly(0.25, 0.50, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2026) =>
      acc ++ hourly(0.25, 0.50, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, _) =>
      acc ++ Series.empty[Hour, Double]
  }
}

lazy val fraSpr = tryOption {

  val start = asof - month

  (start until start + 5.years).foldLeft(Series.empty[Hour, Double]) {

    case (acc, m) if m <= asof + 1.months =>
      acc ++ hourly(0.25, 0.75, Hour(m) until Hour(m + 1.months))

    case (acc, m) if m == asof + 2.months =>
      acc ++ hourly(0.40, 2.00, Hour(m) until Hour(m + 1.months))

    case (acc, m) if m == asof + 3.months =>
      acc ++ hourly(1.00, 3.00, Hour(m) until Hour(m + 1.months))

    case (acc, m) if Quarter(m) == 2020.Q3 =>
      acc ++ hourly(0.35, 1.25, Hour(Quarter(m)) until Hour(Quarter(m) + 1.quarters))

    case (acc, m) if Quarter(m) == 2020.Q4 =>
      acc ++ hourly(0.40, 1.50, Hour(Quarter(m)) until Hour(Quarter(m) + 1.quarters))

    case (acc, m) if Year(m) <= Year(2021) =>
      acc ++ hourly(0.25, 1.00, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2022) =>
      acc ++ hourly(0.35, 0.85, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2023) =>
      acc ++ hourly(0.50, 1.25, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2024) =>
      acc ++ hourly(0.50, 1.25, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2025) =>
      acc ++ hourly(0.50, 1.25, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, m) if Year(m) <= Year(2026) =>
      acc ++ hourly(0.50, 1.25, Hour(Year(m)) until Hour(Year(m) + 1.years))

    case (acc, _) =>
      acc ++ Series.empty[Hour, Double]
  }
}

lazy val ncgSpr = tryOption {

  val start = asof - month

  (Day(start) until Day(start) + 56.months) toSeries {

    case m if Month(m) <= start + 1.months  => 0.35
    case m if Month(m) == start + 2.months  => 0.55
    case m if Month(m) == start + 3.months  => 0.65
    case m if Quarter(m) == 2020.Q3         => 0.60
    case m if Quarter(m) == 2020.Q4         => 0.70
    case m if Year(m) <= Year(2021)         => 0.50
    case m if Year(m) <= Year(2022)         => 0.55
    case m if Year(m) <= Year(2023)         => 0.65
    case m if Year(m) <= Year(2024)         => 0.65
    case m if Year(m) <= Year(2025)         => 0.65
    case m if Year(m) <= Year(2026)         => 0.65
  }
}

// ###############################################################################################
// ### TRANSFERPRICE CALCULATIONS
// ###

lazy val apiAsk = calcTp("API2_ASK", UK)() {
  (day: Day) => for {
    ask    <- srv.CoalApi2Fc.Ask.get(day).flatMap(_.series)
    eurusd <- srv.Reuters1730.EurUsdBid.get(day).flatMap(_.series).map(fillFront)
  } yield ask / eurusd
}

lazy val euaAsk = for {
  spread <- euaSpr
  mid <- calcTp("EUA ask", GERMANY)(SortedSet(Day(2019, 8, 26))) { srv.CarbonEuaFc.EcxMid.get(_).flatMap(_.series) }
} yield mid + spread / 2d

lazy val deuBid = for {
  spread <- deuSpr
  mid   <- calcTp("DEU_BID", GERMANY)(SortedSet(Day(2019, 8, 26))) { srv_.PowerDeuFc.EexMid.get(_).flatMap(_.series) }
} yield mid - spread / 2d

lazy val ncgAsk = for {
  spread <- ncgSpr
  mid    <- calcTp("NCG_ASK", GERMANY)(SortedSet(Day(2019, 8, 26))) { srv_.NgNcgFc.HerenMid.get(_).flatMap(_.series) }
} yield mid + spread / 2d

lazy val pegAsk = for {
  spread <- ncgSpr
  mid <- calcTp("PEG_ASK", FRANCE)(SortedSet(Day(2019, 8, 26))) { srv_.NgPegFc.NordHerenMid.get(_).flatMap(_.series) }
} yield mid + spread / 2d

lazy val fraBid = for {
  spread <- fraSpr
  mid    <- calcTp("FRA", FRANCE)(SortedSet(Day(2019, 8, 26))) { srv_.PowerFraFc.EexMid.get(_).flatMap(_.series) }
} yield mid - spread / 2.0

// ###############################################################################################
// ### MAIN
// ###

val t = asof - 1

eurusd map { srv_.TransferpricesCont.EurUsd.getOrCreate(t) <-- _ }
euaSpr map { srv_.TransferpricesCont.EuaSpr.getOrCreate(t) <-- _ }
deuSpr map { srv_.TransferpricesCont.DeuSpr.getOrCreate(t) <-- _ }
ncgSpr map { srv_.TransferpricesCont.NcgSpr.getOrCreate(t) <-- _ }
fraSpr map { srv_.TransferpricesCont.FraSpr.getOrCreate(t) <-- _ }

apiAsk map { srv_.TransferpricesCont.ApiAsk.getOrCreate(t) <-- _ }
euaAsk map { srv_.TransferpricesCont.EuaAsk.getOrCreate(t) <-- _ }
deuBid map { srv_.TransferpricesCont.DeuBid.getOrCreate(t) <-- _ }
ncgAsk map { srv_.TransferpricesCont.NcgAsk.getOrCreate(t) <-- _ }
fraBid map { srv_.TransferpricesCont.FraBid.getOrCreate(t) <-- _ }
pegAsk map { srv_.TransferpricesCont.PegAsk.getOrCreate(t) <-- _ }