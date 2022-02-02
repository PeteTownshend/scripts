import math.{log => _, _}
import org.joda.time.DateTime
import scala.collection.SortedSet

val svIn = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim
val svOut = svIn
val asof = yesterday //running at 3.50 for previous day
implicit val wb = FO_SUPPORT

// ###############################################################################################
// ### SETTINGS
// ###

val inputDir = "//u-dom1.u-ssi.net/DFSRoot34000/SPECIAL/Forward Curves/TP_Input/TPGas/"
val mtpEnd = Day(2016, 12, 31)
def lookUp = asof.iterator(-5 days)

implicit def implicitLim = svIn
implicit def horizon = asof.iterator(-36 months)
def govDutyIter = (Month(asof).firstOfMonth - 1.days).iterator(-20 quarters)
def trendIter = (Day(Quarter(asof)) - 1.days).iterator(-26 quarters)
def indexIter = trendIter

val cp = svOut.ContractPrice
val cpPlus10Pct = svOut.ContractPricePlus10Pct
val cpMinus10Pct = svOut.ContractPriceMinus10Pct

// ###############################################################################################
// ### HELPER
// ###

def tryOption[T](t: => T): Option[T] = try Some(t) catch {

  case t: Throwable =>
    log error t.getMessage
    None
}

lazy val foGovDuty = new svIn.Index.ForwardCurveStream[Day,Quarter]("FUELOILGOVERNMENTDUTY")
lazy val goGovDuty = new svIn.Index.ForwardCurveStream[Day,Quarter]("GASOILGOVERNMENTDUTY")
lazy val ppi = new svIn.Index.ForwardCurveStream[Day,Quarter]("PRODUCTPRICEINDEX")
lazy val rpi = new svIn.Index.ForwardCurveStream[Day,Quarter]("RETAILPRICEINDEX")
lazy val iei = new svIn.Index.ForwardCurveStream[Day,Quarter]("INDUSTRIALELECTRICITYINDEX")

lazy val Gmv = new svIn.Container("Gmv") with svIn.ForwardCurveContainer
lazy val fo35FobAraBargesGmv = new Gmv.ForwardCurveStream[Day, Year]("FO_3_5_FOB_ARA_BARGES")
lazy val datedBrentSwapGmv = new Gmv.ForwardCurveStream[Day, Year]("DATED_BRENT_SWAP")
lazy val go01FobAraBargesGmv = new Gmv.ForwardCurveStream[Day, Year]("GO_0_1_FOB_ARA_BARGES")
lazy val nbpMidGmv = new Gmv.ForwardCurveStream[Day, Month]("NBP_MID")
lazy val eurUsdMidGmv = new Gmv.ForwardCurveStream[Day, Year]("EUR_USD_MID")
lazy val eurGbpMidGmv = new Gmv.ForwardCurveStream[Day, Year]("Eur_GBP_MID")

def cut[T <: DateTimeLike[T]](s: Series[T, Double]) = (change(s) to Month).from(Month(asof))

def hist(symbol: String, field: String*)(implicit lim: Lim, iter: Iterator[Day]) = Interpolate.omit(
  field.toList match {

    case field :: Nil =>
      lim(iter, symbol, field)

    case field1 :: field2 :: Nil =>
      lim(iter, symbol, (field1, field2))()
  }
)

def concat[T <: DateTimeLike[T]](
                                  fwd: Series[T, Double],
                                  hist: Series[Day, Double])(
                                  implicit dtToT: org.joda.time.DateTime => T
                                ): Series[Month, Double] = {

  val mfwd = change(fwd) to Month
  val mhist = change(hist) to Month

  (mhist.end, mfwd.start, hist.end) match {

    case (h, f, lastDay) if h == f =>
      val days = h.toDays filter { !_.isWeekend }
      val avg = (mhist.last._2 * days.filter(_ <= lastDay).size + mfwd.head._2 * days.filter(_ > lastDay).size) / days.size
      (mhist ++ mfwd).updated((h, avg))

    case _ =>
      mhist ++ mfwd
  }
}

def gapped[T <: DateTimeLike[T], C <: DateTimeLike[C]](companion: DateTimeLikeCompanion[C], mtp: Series[T, Double], forecast: Series[T, Double]) = {
  val gap = companion(mtp.end) + 1 until companion(forecast.start)

  (Series(companion(mtp.end) -> (Some(mtp.lastValue): Option[Double]))
    ++ Series(gap map (e => e -> (None: Option[Double])))
    ++ Series(companion(forecast.start) -> (Some(forecast.firstValue): Option[Double])))
}

def quarterlyGapped(mtp: Series[Month, Double], forecast: Series[Month, Double]): Series[Quarter, Option[Double]] =
  gapped(Quarter, mtp, forecast)

import java.io.{
  BufferedReader,
  FileReader
}
import scala.collection.mutable.ListBuffer

class reader(val bfr: BufferedReader, uri: Option[String] = None) extends Logging {
  def apply[T](extractor: ((String*) => T)): List[T] = using(bfr) { bfr =>
    val content = new ListBuffer[T]
    doWhile(bfr.ready) { content += extractor(bfr.readLine().split("[,!]").toList: _*) }
    content.toList
  }

  def apply() = using(bfr) { bfr =>
    val content = new ListBuffer[String]
    doWhile(bfr.ready) { content += bfr.readLine }
    content.toList
  }
}

object reading extends Logging {
  def apply[T](dirName: String, fileName: String) = tryOption {
    new BufferedReader(new FileReader(dirName + fileName))
  } map { new reader(_, Some(dirName + fileName)) }
}

trait FileQuotes {
  this: Product =>

  def fileName: String
  def inputDir: String
  var lastObservation = 0.0
  def fallBackProduct = Some(lastObservation)
  lazy val products = {
    def extractor(s: String*) = (s.head, s.tail match {
      case t if t.length > 0 => Some(t.head.toDouble)
      case _ => None
    }
    )
    reading(inputDir, fileName) map { _(extractor) } match {
      case Some(input) => (Map.empty[String, Double] /: input) { (m, p) =>
        p._2 match {
          case Some(product) =>
            lastObservation = product
            m + (p._1 -> product)
          case _ => m
        }
      }
      case None =>
        log fatal "could not read input file from %s %s".format(inputDir, fileName)
        Map.empty[String, Double]
    }
  }
}

trait ProductStructure {
  def extractor[T <: DateTimeLike[T]]: PartialFunction[(T, Day, Map[String, Double]), String]
}

object EmptyStructure extends ProductStructure {
  def extractor[T <: DateTimeLike[T]]: PartialFunction[(T, Day, Map[String, Double]), String] = Map.empty
}

object WithinMonth {
  def unapply[T <: DateTimeLike[T]](dtAsofProducts: (T, Day, Map[String, Double])): Option[String] = dtAsofProducts match {
    case (dt, asof, products) =>
      if (Month(asof) == (dt: Month)) Some("M+")
      else None
  }
}
object WithinQuarter {
  def unapply[T <: DateTimeLike[T]](dtAsofProducts: (T, Day, Map[String, Double])) = dtAsofProducts match {
    case (dt, asof, products) =>
      if (((Month(asof) + 1): Quarter).contains(dt) == Some(true))
        Some("M+" + ((dt: Month) - Month(asof)))
      else None
  }
}
object AsQuarter {
  def unapply[T <: DateTimeLike[T]](dtAsofProducts: (T, Day, Map[String, Double])) = dtAsofProducts match {
    case (dt, asof, products) =>
      if (
        Quarter(asof.year, 1).contains(asof) == Some(false) && (
          Year(asof).contains(dt) == Some(true) || (Year(asof) + 1).contains(dt) == Some(true)
        )
      ) Some("Q+" + ((dt: Quarter) - (asof: Quarter)))
      else None
  }
}
object WithinSeason {
  def unapply[T <: DateTimeLike[T]](dtAsofProducts: (T, Day, Map[String, Double])) = dtAsofProducts match {
    case (dt, asof, products) =>
      if (Season((Quarter((Month(asof) + 1)) + 1)).contains(dt) == Some(true))
        Some("Q+" + ((dt: Quarter) - (asof: Quarter)))
      else None
  }
}
object AsSeason {
  def unapply[T <: DateTimeLike[T]](dtAsofProducts: (T, Day, Map[String, Double])) = dtAsofProducts match {
    case (dt, asof, products) =>
      if (products.contains("S+" + ((dt: Season) - Season(asof))))
        Some("S+" + ((dt: Season) - Season(asof)))
      else None
  }
}
object WithinYear {
  def unapply[T <: DateTimeLike[T]](dtAsofProducts: (T, Day, Map[String, Double])) = dtAsofProducts match {
    case (dt, asof, products) =>
      if (Year((Month(asof) + 1)).contains(dt) == Some(true))
        Some("Q+" + (Quarter(dt) - Quarter(asof)))
      else None
  }
}
object AsCalendar {
  def unapply[T <: DateTimeLike[T]](dtAsofProducts: (T, Day, Map[String, Double])) = dtAsofProducts match {
    case (dt, asof, products) =>
      if (products.contains("Cal+" + (Year(dt) - Year(asof))))
        Some("Cal+" + (Year(dt) - Year(asof)))
      else None
  }
}

object GasStructure extends ProductStructure {
  def extractor[T <: DateTimeLike[T]]: PartialFunction[(T, Day, Map[String, Double]), String] = {
    case WithinMonth(quote) => quote + "1"
    case WithinQuarter(quote) => quote
    case WithinSeason(quote) => quote
    case AsSeason(quote) => quote
    case AsCalendar(quote) => quote
  }
}

object CoalStructure extends ProductStructure {
  def extractor[T <: DateTimeLike[T]]: PartialFunction[(T, Day, Map[String, Double]), String] = {
    case WithinMonth(quote) => quote + "0"
    case WithinQuarter(quote) => quote
    case AsQuarter(quote) => quote
    case WithinYear(quote) => quote
    case AsCalendar(quote) => quote
  }
}

abstract case class Product(asof: Day, structure: ProductStructure = EmptyStructure) extends Logging {
  def products: Map[String, Double]
  def fallBackProduct: Option[Double]

  def apply[T <: DateTimeLike[T]](dt: T): Option[Double] =
    if (structure.extractor.isDefinedAt((dt, asof, products)))
      tryOption { products(structure.extractor((dt, asof, products))) }
    else
      fallBackProduct

  def apply[T <: DateTimeLike[T]](timeLine: SortedSet[T]): Option[Series[T, Double]] = {
    if (products.isEmpty && !fallBackProduct.isDefined) None
    else {
      ((List.empty[(T, Double)] /: timeLine) { (l, t) =>
        this.apply(t) match {
          case Some(product) => l :+ (t, product)
          case _ => l
        }
      }) match {
        case l if (l.size > 0) => Some(Series(l))
        case _ => None
      }
    }
  }
}

object FileQuotes {
  def apply[T <: ProductStructure](asof: Day, _inputDir: String, _fileName: String, structure: T) = new Product(asof, structure) with FileQuotes {
    val inputDir = _inputDir
    val fileName = _fileName
  }
}

trait FoEnergyTrendParam {
  val x1 = 1.136965547
  val x2 = 0.284635241
}

object FoEnergyTrendParam extends FoEnergyTrendParam

def avgToQuarter(qrt: Int, _s: Series[Quarter, Double])(implicit f: Series[Quarter, Double] => Double = _.firstValue) = {
  val s_ = projective.mean(qrt.quarters, quarter)(_s)
  projective.average(Quarter, quarter)(s_) { s => (s.start, f(s)) }
}

object FoEnergyTrendFormula {
  def apply[T <: DateTimeLike[T]](
                                   param: FoEnergyTrendParam)(
                                   _fx: Series[T, Double],
                                   _fo: Series[T, Double],
                                   _govDuty: Series[T, Double]) = {
    val fo = change(_fo / _fx) to Quarter
    val govDuty = change(_govDuty) to Quarter
    val foDelta = avgToQuarter(4, fo) - fo

    import param._
    ((fo + govDuty) * x1 + foDelta * x2) * 0.967
  }
}

trait GoEnergyTrendParam {
  val x1 = 1.048733362
  val x2 = 0.221846093
}

object GoEnergyTrendParam extends GoEnergyTrendParam

object GoEnergyTrendFormula {
  def apply[T <: DateTimeLike[T]](
                                   param: GoEnergyTrendParam)(
                                   _fx: Series[T, Double],
                                   _go: Series[T, Double],
                                   _govDuty: Series[T, Double]) = {
    val go = change(_go / _fx) to Quarter
    val govDuty = change(_govDuty) to Quarter
    val goDelta = avgToQuarter(4, go) - go

    import param._
    val s1 = (go + govDuty) * x1
    val s2 = goDelta * x2
    (s1 + s2) * 0.985
  }
}

// ###############################################################################################
// ### FX
// ###
lazy val eurgbp                       = svIn.Reuters1730.EurGbpMid
lazy val eurusd                       = svIn.Reuters1730.EurUsdMid.fallBackSeries(lookUp) map {concat(_, hist("ECBUSD", "Spot"))}
lazy val gbpusd                       = svIn.Reuters1730.GbpUsdMid.fallBackSeries(lookUp) map {concat(_, hist("USDGBP_BOE", "Spot"))}
lazy val gbpForecast                  = for {
  eurUsd <- eurUsdMidGmv.get(today) flatMap { _.series }
  eurGbp <- eurGbpMidGmv.get(today) flatMap { _.series }
} yield eurUsd / eurGbp

lazy val gbpExtended                  = for {
  fx    <- gbpusd map {change(_) to Month} map { _ to Month(mtpEnd) }
  fxGmv <- gbpForecast map { change(_) to Month }
  gab = change(Interpolate.linear(quarterlyGapped(fx, fxGmv))) to Month
} yield fx ++ gab ++ fxGmv

// ###############################################################################################
// ### INDICES
// ###
lazy val productPriceIndex          = for {
  fwd <- ppi.get(today) flatMap { _.series }
  hist = change(Interpolate.omit(svIn(indexIter, "UKNS.PPI.JVZ7", "MonthlyOutput"))) to Quarter
} yield hist ++ fwd.from(hist.end + 1)

lazy val retailPriceIndex             = for {
  fwd <- rpi.get(today) flatMap { _.series }
  hist = change(Interpolate.omit(svIn(indexIter, "UKNS.RPI.MONTHLY.CHAW", "IndexPrice"))) to Quarter
} yield hist ++ fwd.from(hist.end + 1)

lazy val industrialElectricityIndex   = for {
  fwd <- iei.get(today) flatMap { _.series }
  hist = change(Interpolate.omit(svIn(indexIter, "DECC.GB.ELECTRICITY.LARGE", "Rate"))) to Quarter
} yield hist ++ fwd.from(hist.end + 1)

// ###############################################################################################
// ### GASOIL
// ###
/**
  * Mid curve plus half bid-ask spread, since forward position is short gas oil
  */
lazy val go01FobAraBGcrgAsk           = svIn.OilEodFc.Go01FobAraBGcrgAsk.get(asof) flatMap {
  _.series} map {
  change(_) to Day} map {
  concat(_, hist("AAYWT00", "High", "Low"))
}
lazy val iceGo                        = svIn.OilEodFc.IceGasoilSwap.get(asof) flatMap {
  _.series} map {
  concat(_, hist("ICEOTC.UUB", "Close"))
}
lazy val iceGoForecast                = for {
  settlement <- svIn.Settlement.NymexWqFobAraBDiff.fallBackSeries(lookUp)
  spread <- tryOption { settlement.lastValue }
  gmv <- go01FobAraBargesGmv.get(today) flatMap { _.series }
} yield gmv - spread

lazy val gasOilEnergyTrendHistory     = change(Interpolate.omit(svIn(trendIter, "DECC.GB.GASOIL.LARGE", "Rate") )) to  Quarter

lazy val gasOilGovernmentDuty         = for {
  fwd <- goGovDuty.get(today) flatMap { _.series} map { change(_) to Month }
  hist = change(Interpolate.omit(svIn(govDutyIter, "HMRC.HO.GAS.OIL.REBATED.OILS.FILL", "Rate"))) to Month
} yield (hist * 11.56) ++ fwd.from(hist.end + 1)

lazy val gasOilExtended               = for {
  go    <- iceGo map {change(_) to Month} map {_ to Month(mtpEnd) }
  goGmv <- iceGoForecast map {change(_) to Month}
  gab = change(Interpolate.linear(quarterlyGapped(go, goGmv))) to Month
} yield go ++ gab ++ goGmv

// ###############################################################################################
// ### FUELOIL
// ###
/**
  * Mid curve plus half bid-ask spread, since forward position is short fuel oil
  */
lazy val fo1FobAraBGcrgAsk            = svIn.OilEodFc.Fo1FobAraBGcrgAsk.get(asof) flatMap {
  _.series} map {
  change(_) to Day} map {
  concat(_, hist("PUAAP00", "High", "Low"))
}
lazy val fo1FobNwe                    = svIn.OilEodFc.Fo1FobNweCargoes.get(asof) flatMap {
  _.series} map {
  concat(_, hist("PUAAM00", "High","Low"))
}
lazy val fo1FobNweForecast            = for {
  settlement <- svIn.Settlement.NymexFsHiloSpread.fallBackSeries(lookUp)
  spread <- tryOption { settlement.lastValue }
  gmv <- fo35FobAraBargesGmv.get(today) flatMap { _.series }
} yield gmv + spread


lazy val fuelOilEnergyTrendHistory    = change(Interpolate.omit(svIn(trendIter, "DECC.GB.HFO.LARGE", "Rate") )) to  Quarter
lazy val fuelOilGovernmentDuty        = for {
  fwd <- foGovDuty.get(today) flatMap { _.series } map { change(_) to Month }
  hist = change(Interpolate.omit(svIn(govDutyIter, "HMRC.HO.FUEL.OIL.REBATED.OILS.FILL", "Rate"))) to Month
} yield  (hist * 10.11) ++ fwd.from(hist.end + 1)

lazy val fuelOilExtended              = for {
  fo    <- fo1FobNwe map {change(_) to Month} map {_ to Month(mtpEnd)}
  foGmv <- fo1FobNweForecast map {change(_) to Month}
  gab = change(Interpolate.linear(quarterlyGapped(fo, foGmv))) to Month
} yield fo ++ gab ++ foGmv

// ###############################################################################################
// ### OIL
// ###
lazy val iceBrent                     = svIn.OilEodFc.IceBrentSwap.get(asof) flatMap {
  _.series} map {
  concat(_, hist("AAYES00","Close"))
}

lazy val iceBrentForecast             = for {
  settlement <- svIn.Settlement.NymexFyDflBrentDiff.fallBackSeries(lookUp)
  spread <- tryOption { settlement.lastValue }
  gmv <- datedBrentSwapGmv.get(today) flatMap { _.series }
} yield gmv - spread

lazy val iceBrentExtended             = for {
  co    <- iceBrent map {change(_) to Month} map {_ to Month(mtpEnd)}
  coGmv <- iceBrentForecast map {change(_) to Month}
  gab =  change(Interpolate.linear(quarterlyGapped(co, coGmv))) to Month
} yield co ++ gab ++ coGmv

// ###############################################################################################
// ### GAS
// ###
lazy val ttfSpread                    = FileQuotes(asof, inputDir, "SpreadInputTTF.csv", GasStructure)
lazy val ttfMid                       = svIn.NgTtfFc.Mid.get(asof) flatMap {_.series}

lazy val zeeSpread                    = FileQuotes(asof, inputDir, "SpreadInputZEE.csv", GasStructure)
lazy val zeeMid                       = svIn.NgZeeFc.Mid.get(asof) flatMap {_.series}

lazy val nbpSpread                    = FileQuotes(asof, inputDir, "SpreadInputNBP.csv", GasStructure)
lazy val nbpMid                       = svIn.NgNbpFc.Mid.get(asof) flatMap {_.series}
lazy val nbpForecast                  = nbpMidGmv.get(today) flatMap { _.series }

// ###############################################################################################
// ### COLA
// ###
lazy val api2Spread                   = FileQuotes(asof, inputDir, "SpreadInputAPI2.csv", CoalStructure)
lazy val api2Mid                      = svIn.CoalApi2Fc.Mid.get(asof) flatMap {_.series}
/**
  * Mid curve plus half bid-ask spread, since forward position is short coal
  */
lazy val coalApi2FcAsk                = svIn.CoalApi2Fc.TrayportAsk.get(asof) flatMap {
  _.series} map {
  change(_) to Day} map {
  concat(_, hist("PA0007773.0.0", "Index"))
}

val BritanniaParam = {

  val IP = 0.5869 * 29.3071 // p/therm
  val PPI = 0.4
  val PPIo = 66.8311
  val FO = 0.12
  val FOo = 68.075
  val GO = 0.34
  val GOo = 137.45
  val IEI = 0.04
  val IEIo = 3.7425
  val CO = 0.05
  val COo = 18.9533
  val RPI = 0.05
  val RPIo = 139.55
}

trait BazeParam {
  val goAlpha = 0.0036474
  val goBeta = -156.28977
  val goAdditional = 43.24

  val foAlpha = 0.0022536
  val foBeta = -144.64

  //fo vs coal
  val hfo1Alpha = 0.0003521
  val hfo1Beta = -78.113469
  val hfo1FoAlpha = 1.028
  val hfo1FoAdditional = 32.71
  val hfo1CoalAlpha = 0.011085 / 0.007825
  val hfo1CoalBeta = -43.5
  val hfo1CoalAdditional = 99.753

  //fo vs index
  val hfo2Alpha = hfo1Alpha
  val hfo2Beta = hfo1Beta
  val hfo2FoAlpha = hfo1FoAlpha
  val hfo2FoAdditional = hfo1FoAdditional
  val hfo2IndexAlpha = 99.753 / 88.800272

  //transportation discount
  val t1 = 1 / 3.0 * 0.0774 + 0.0946

  //goPD is price dependent
  val goPDAlpha = 0.036
  val goPDBeta = 43.24
  val goPDAdditional = -179.0

  //goPA is price dependent
  val p0 = 0.68639
  def inc: Series[Month, Double]
}

object BazeFormula {
  def apply[T <: DateTimeLike[T]](
                                   param: BazeParam)(
                                   _go: Series[T, Double],
                                   _fo: Series[T, Double],
                                   _coal: Series[T, Double]) = {
    val go = {
      val go_ = projective.mean(9.months, month)(change(_go) to Month).lag(-1)
      projective.average(Quarter, quarter)(go_) { s => (s.start, s.firstValue) }
    }
    val fo = {
      val fo_ = projective.mean(6.months, month)(change(_fo) to Month).lag(-1)
      projective.average(Quarter, quarter)(fo_) { s => (s.start, s.firstValue) }
    }
    val coal = {
      val coal_ = projective.mean(3.months, month)(change(_coal) to Month).lag(-6)
      projective.average(Quarter, quarter)(coal_) { s => (s.start, s.firstValue) }
    }

    import param._

    val GO = (go + goAdditional + goBeta) * goAlpha
    val FO = (fo + foBeta) * foAlpha

    val foCoalMin = (fo * hfo1FoAlpha + hfo1FoAdditional) min ((coal + hfo1CoalBeta) * hfo1CoalAlpha + hfo1CoalAdditional)
    val HFO1 = (foCoalMin + hfo1Beta) * hfo1Alpha

    val index = {
      val _index = projective.mean(6.months, month)(inc.lag(-1))
      projective.average(Quarter, quarter)(_index) { s => (s.start, s.firstValue) }
    }
    val foIndexMin = (fo * hfo2FoAlpha + hfo2FoAdditional) min ((change(index) to fo.firstKey.companion) * hfo2IndexAlpha)
    val HFO2 = (foIndexMin + hfo2Beta) * hfo2Alpha

    val PD = ((go + goPDBeta + goPDAdditional) / 46.0 max 0.0 min 1.0) * goPDAlpha

    val PA = (go + 43.24) mapValues {
      case x if x < 182 => 0.0
      case x if x < 260 => 0.00043 * (x - 182)
      case x if x < 335 => 0.0335 + 0.00069 * (x - 260)
      case x if x < 802 => 0.0852 + 0.00046 * (x - 335)
      case x => 0.3 + 0.00023 * (x - 802)
    }

    (GO + p0 + FO + HFO2 + HFO1 - PD - PA - t1) * 10.0
  }
}

trait GasTerraParam {
  def Io: Double
  def I: Double
  def addOn: Double
}

object GasTerraFormula {
  def apply[T <: DateTimeLike[T]](
                                   param: GasTerraParam)(
                                   _go: Series[T, Double],
                                   _fo: Series[T, Double],
                                   _coal: Series[T, Double]) = {
    val go = projective.mean(9.months, month)(change(_go) to Month)

    val fo = projective.mean(9.months, month)(change(_fo) to Month)

    val coal = {
      val coal_ = projective.mean(3.months, month)(change(_coal) to Month).lag(-6)
      projective.average(Quarter, quarter)(coal_) { s => (s.start, s.firstValue) }
    }

    import param._

    val HPA = go mapValues { value =>
      if (value <= 177.7606) 0.0
      else 0.004885 * (value - 177.7606)
    }
    val HPABracket = go mapValues { value =>
      if (value <= 350.7018) 0.0
      else if (value <= 738.9371) 0.003774 * (value - 350.7018)
      else 1.465380 + 0.003460 * (value - 738.9371)
    }

    val HFOC = (fo + 19.59) min (103 * (0.75 + 0.25 * I / Io))

    val P0 = 6.9360363
    val g1 = (go - 138.3491) * 0.61 * 0.82 * 0.0743
    val g2 = (go - 138.3491) * 0.05 * 0.90 * 0.0743
    val c = ((change(coal) to Month) - 38.6588) * 0.05 * 0.90 * 0.1080
    val hfoc = (HFOC - 124.6274) * 0.15 * 0.90 * 0.0768
    val f = (fo - 105.0374) * 0.14 * 0.90 * 0.0768

    val AP = g1 + P0 + g2 + c + hfoc + f - HPA + HPABracket - 0.15339 + addOn

    AP * 10.0 / 9.7692
  }
}

trait TrollShellParam {
  val X = 0.2
  val Y = 0.13
  val Z = 0.67
  val P0 = 9.98
  val A0 = 65.4
  val B0 = 80.3
  val C0 = 1.6064
  val D0 = 75.4
  val E0 = 85.2
  val F0 = 77.4

  def incA: Series[Month, Double]
  def incB: Series[Month, Double]
  def incD: Series[Month, Double]
  def incE: Series[Month, Double]
  def incF: Series[Month, Double]
}

object TrollShellFormula {
  def apply[T <: DateTimeLike[T]](
                                   param: TrollShellParam)(
                                   _coal: Series[T, Double]) = {
    val coal = {
      val coal_ = projective.mean(6.months, month)(change(_coal) to Month)
      projective.average(Quarter, quarter)(coal_) { s => (s.start, s.firstValue) }
    }
    def pa(inc: Series[Month, Double]) = {
      val _index = projective.mean(3.months, month)(inc.lag(-3))
      projective.average(Quarter, quarter)(_index) { s => (s.start, s.firstValue) }
    }

    import param._

    val A = pa(incA)
    val B = pa(incB)
    val C = coal / 25.104 + 0.14
    val D = pa(incD)
    val E = pa(incE)
    val F = pa(incF)

    val I = A * 0.5 / A0 + B / B0 * 0.5
    val L = D * 0.85 / D0 + (E * 0.5 / E0 + F * 0.5 / F0) * 0.15

    (I * X + L * Y + C * Z / C0) * P0 * 10.0 / 9.7694
  }
}

trait StatoilHydroParam {
  val X = 0.2
  val Y = 0.13
  val Z = 0.67
  val P0 = 9.98
  val A0 = 65.3
  val B0 = 79.7
  val C0 = 1.6064
  val D0 = 75.2
  val E0 = 81.5
  val F0 = 78.2

  def incA: Series[Month, Double]
  def incB: Series[Month, Double]
  def incD: Series[Month, Double]
  def incE: Series[Month, Double]
  def incF: Series[Month, Double]
}

object StatoilHydroFormula {
  def apply[T <: DateTimeLike[T]](
                                   param: StatoilHydroParam)(
                                   _coal: Series[T, Double]) = {
    val coal = {
      val coal_ = projective.mean(3.months, month)(change(_coal) to Month)
      projective.average(Quarter, quarter)(coal_) { s => (s.start, s.firstValue) }
    }

    def pa(inc: Series[Month, Double]) = {
      val _index = projective.mean(3.months, month)(inc.lag(-3))
      projective.average(Quarter, quarter)(_index) { s => (s.start, s.firstValue) }
    }

    import param._

    val A = pa(incA)
    val B = pa(incB)
    val C = coal / 25.121 + (B * 0.5 / 100.8 + 0.5) * 0.0915
    val D = pa(incD)
    val E = pa(incE)
    val F = pa(incF)

    val I = A * 0.5 / A0 + B * 0.5 / B0
    val L = D * 0.85 / D0 + (E * 0.5 / E0 + F * 0.5 / F0) * 0.15

    (I * X + L * Y + C * Z / C0) * P0 * 10.0 / 9.7694
  }
}

trait BhpParam {
  val IP = 11.66644 // p/therm
  val GO = 0.15
  val GOo = 126.975
  val RPI = 0.85
  val RPIo = 120.15
}

object BhpFormula {
  def apply[T <: DateTimeLike[T]](
                                   param: BhpParam)(
                                   _goEnergyTrend: Series[T, Double],
                                   _govDuty: Series[T, Double],
                                   _rpi: Series[T, Double]) = {
    import param._
    val go = change(_goEnergyTrend) to Quarter
    val govDuty = change(_govDuty) to Quarter
    val rpi = avgToQuarter(4, (change(_rpi) to Quarter).lag(-1))
    val goo = avgToQuarter(4, (go - ((govDuty - 13.13) / 2.0).roundTo(1)).lag(-1))

    (rpi * RPI / RPIo + goo * GO / GOo) * IP
  }
}

trait BritanniaParam {
  val IP = 0.5869 * 29.3071 // p/therm
  val PPI = 0.4
  val PPIo = 79.5514
  val FO = 0.12
  val FOo = 68.075
  val GO = 0.34
  val GOo = 137.45
  val IEI = 0.04
  val IEIo = 3.7425
  val CO = 0.05
  val COo = 18.9533
  val RPI = 0.05
  val RPIo = 139.55
}

object BritanniaFormula {
  def apply[T <: DateTimeLike[T]](
                                   param: BritanniaParam)(
                                   _foEnergyTrend: Series[T, Double],
                                   _goEnergyTrend: Series[T, Double],
                                   _co: Series[T, Double],
                                   _rpi: Series[T, Double],
                                   _ppi: Series[T, Double],
                                   _iei: Series[T, Double]) = {
    import param._
    val fo = avgToQuarter(4, (change(_foEnergyTrend) to Quarter).lag(-1))
    val go = avgToQuarter(4, (change(_goEnergyTrend) to Quarter).lag(-1))
    val co = avgToQuarter(4, (change(_co) to Quarter).lag(-1))
    val rpi = avgToQuarter(4, (change(_rpi) to Quarter).lag(-1))
    val ppi = avgToQuarter(4, (change(_ppi) to Quarter).lag(-1))
    val iei = avgToQuarter(4, (change(_iei) to Quarter).lag(-1))

    (ppi * PPI / PPIo + go * GO / GOo + fo * FO / FOo + co * CO / COo + iei * IEI / IEIo + rpi * RPI / RPIo) * IP
  }
}

trait NamParam {
  val flexFee = 1.034 //[€/MWh]
}

object NamFormula {
  def apply(param: NamParam)(_hist: Series[Day, Double]) = {

    def mean(from: Day) = {
      _hist filterTime { t => from <= t && t <= Day(Year(from) + 1) } match {
        case hist if hist.length > 0 => Some(hist.mean)
        case _ => None
      }
    }

    val nam = (Series.empty[Year, Double] /: (_hist.time.groupBy { Year(_) }).keys)((s, y) =>
      (y.year match {
        case 2011 => mean(2011.Aug)
        case year => mean(year.Jul)
      }) match {
        case Some(mean) => s + (y + 1 -> mean)
        case None => s
      }
    )

    import param._
    nam + flexFee roundTo 3
  }
}

def zipList(l: Series[Month, List[Double]], r: Series[Month, List[Double]]): Series[Month, List[Double]] = {
  Series((l.time | r.time).toList map { t =>
    t -> ((l.get(t), r.get(t)) match {
      case (Some(l), Some(r)) => l.union(r)
      case (Some(l), _) => l
      case (_, Some(r)) => r
      case _ => Nil
    })
  })
}

def history[T <: DateTimeLike[T]](asof: Day, daysBack: Int)(curve: Day => Option[Series[T, Double]])(implicit c: DateTime => T) = {
  def getTimeSeries(tradeDate: Day, num: Int): List[Series[Month, List[Double]]] = {
    if (num < daysBack)
      curve(tradeDate) match {
        case Some(s) => getTimeSeries(tradeDate - 1, num + 1) :+ (s.rollBy(e => Month(e._1))(_.mean).from(Month(asof))).mapValues { List(_) }
        case None => getTimeSeries(tradeDate - 1, num)
      }
    else Nil
  }
  getTimeSeries(asof, 0).reduceLeft(zipList)
}

trait LimQuotes {
  this: Product =>
  def quotes: Map[String, (String, (String, String))]
  def fallBackProduct: Option[Double] = None
  def lim: Lim

  lazy val products = quotes mapValues {
    case (symbol, fields) => lim(asof iterator asof, symbol, fields)() firstValue
  } collect {
    case (key, Some(price)) => key -> price
  }
}

object LimQuotes {
  def apply[T <: ProductStructure](asof: Day, _lim: Lim, structure: T)(_quotes: Map[String, (String, (String, String))]) = new Product(asof, structure) with LimQuotes {
    val lim = _lim
    val quotes = _quotes
  }
}

val hiLo = ("High", "Low")
val termToKWh = 29.3071

def logReturns[T <: DateTimeLike[T]](series: Series[T, List[Double]])(implicit c: DateTime => T) =
  series.mapValues {
    l => l.dropRight(1) zip l.drop(1) map (r => math.log(r._2 / r._1))
  }.collect {
    case lr if lr._2.length > 0 => lr
  }

def vola[T <: DateTimeLike[T]](
                                series: Series[T, List[Double]])(
                                implicit c: DateTime => T) = series.collect { case p if p._2.length > 1 => p }.mapValues { l =>
  val mean = l.sum / l.length
  val meansq = l.foldLeft(0.0) { (p, e) => { p + math.pow(e - mean, 2) } }
  math.sqrt(meansq / (l.length - 1))
}

implicit class RichList(l: List[Double]) {
  lazy val mean = l.sum / l.length
  def dev = l.foldLeft(0.0) { (p, e) => { p + pow(e - mean, 2) } }
}

class RichTupleList(x: List[Double], y: List[Double]) {
  lazy val mean = (x zip y map { e => e._1 * e._2 }).mean
  def dev = {
    val xmean = x.mean
    val ymean = y.mean
    x zip y map { e => (e._1 - xmean) * (e._2 - ymean) } sum
  }
}

implicit def richTupleList(xy: (List[Double], List[Double])) = new RichTupleList(xy._1, xy._2)

def correl[T <: DateTimeLike[T]](
                                  x: Series[T, List[Double]],
                                  y: Series[T, List[Double]])(
                                  implicit c: DateTime => T, ord: Ordering[T]) = {
  val s = x.map { e =>
    val t = e._1
    val x = e._2
    y.get(t) match {

      case Some(y) if (y.length == x.length) =>
        t -> Some((x, y).dev / math.sqrt(x.dev * y.dev))

      case _ =>
        t -> None
    }
  }
  //dropping Some(NaN) from the end, refactor!!!
  val reverse = Series(s.toIterable)(ord.reverse)
  Series(reverse.dropWhile(p => p._2.isDefined && p._2.get.isNaN).toIterable)
}

case class Spread[T <: DateTimeLike[T]](mid: Series[T, Double], spread: Product) {
  lazy val bidAsk = spread(mid.time)
  lazy val ask = bidAsk map { s => mid + s * 0.5 }
  lazy val bid = bidAsk map { s => mid - s * 0.5 }
}

// ###############################################################################################
// ### CALCULATION
// ###

def foEnergyTrend(param: FoEnergyTrendParam, shock: Double) = for {
  fx <- gbpExtended
  fo <- fuelOilExtended map {_ * shock}
  govDuty <- fuelOilGovernmentDuty
  etHist = fuelOilEnergyTrendHistory
  etFwd = FoEnergyTrendFormula(param)(fx, fo, govDuty)
} yield etHist ++ etFwd.from(etHist.end + 1)

def foEnergyTrendEndur(param: FoEnergyTrendParam) = for {
  fx <- gbpusd map {change(_) to Month}
  fo <- fo1FobNwe map {change(_) to Month}
  govDuty <- fuelOilGovernmentDuty
  etHist = fuelOilEnergyTrendHistory
  etFwd = FoEnergyTrendFormula(param)(fx, fo, govDuty)
} yield etHist ++ etFwd.from(etHist.end + 1)

def goEnergyTrend(param: GoEnergyTrendParam, shock: Double) = for {
  fx <- gbpExtended
  go <- gasOilExtended map {_ * shock}
  govDuty <- gasOilGovernmentDuty
  etHist = gasOilEnergyTrendHistory
  etFwd = GoEnergyTrendFormula(param)(fx, go, govDuty)
} yield etHist ++ etFwd.from(etHist.end + 1)

def goEnergyTrendEndur(param: GoEnergyTrendParam) = for {
  fx <- gbpusd
  go <- iceGo
  govDuty <- gasOilGovernmentDuty
  etHist = gasOilEnergyTrendHistory
  etFwd = GoEnergyTrendFormula(param)(fx, go, govDuty)
} yield etHist ++ etFwd.from(etHist.end + 1)

def baze(param: BazeParam) = for {
  fx    <- eurusd
  go    <- go01FobAraBGcrgAsk map {g => (g - 10.7267) / fx}
  fo    <- fo1FobAraBGcrgAsk  map {f => (f - 0.5) / fx}
  coal  <- coalApi2FcAsk      map {c => c / fx + 10.0}
} yield BazeFormula(param)(go,fo,coal)

def gasTerra(param: GasTerraParam) = for {
  fx    <- eurusd
  go    <- go01FobAraBGcrgAsk map {_ / fx}
  fo    <- fo1FobAraBGcrgAsk  map {_ / fx}
  coal  <- coalApi2FcAsk      map {c => c / fx + 10.0}
} yield GasTerraFormula(param)(go,fo,coal)

def trollShell(param: TrollShellParam) = for {
  fx    <- eurusd
  coal  <- coalApi2FcAsk map {_ / fx}
} yield TrollShellFormula(param)(coal)

def statoilHydro(param: StatoilHydroParam) = for {
  fx    <- eurusd
  coal  <- coalApi2FcAsk map {_ / fx}
} yield StatoilHydroFormula(param)(coal)

def bhp(param: BhpParam, shock: Double = 1.0) = for {
  go <- goEnergyTrend(GoEnergyTrendParam, shock)
  govDuty <- gasOilGovernmentDuty map {change(_) to Quarter}
  rpi <- retailPriceIndex map {_ * shock}
} yield BhpFormula(param)(go, govDuty, rpi)

def britannia(param: BritanniaParam, shock: Double = 1.0) = for {
  fo <- foEnergyTrend(FoEnergyTrendParam, shock)
  go <- goEnergyTrend(GoEnergyTrendParam, shock)
  co <- iceBrentExtended map {_ * shock} map {change(_) to Quarter}
  rpi <- retailPriceIndex map {_ * shock}
  ppi <- productPriceIndex map {_ * shock}
  iei <- industrialElectricityIndex map {_ * shock}
} yield BritanniaFormula(param)(fo, go, co, rpi, ppi, iei)

def nam(param: NamParam) = {
  Interpolate.omit(
    svIn(asof iterator Day(2011,8,1), "ESGM.TTF", ("FwdBid01cyr","FwdOfr01cyr"))()
  ) match {
    case hist if hist.length > 0 => Some(NamFormula(param)(hist))
    case _ => None
  }
}

lazy val ttfHist = history(asof, 60) { tradeDate: Day =>
  if (tradeDate.isWeekend) None
  else {
    LimQuotes(tradeDate, svIn, GasStructure){
      Map[String, String](
        "M+1" -> "PA0003041.6.1",
        "M+2" -> "PA0003041.6.2",
        "M+3" -> "PA0003041.6.3",
        "Q+1" -> "PA0003042.6.1",
        "Q+2" -> "PA0003042.6.2",
        "Q+3" -> "PA0003042.6.3",
        "S+1" -> "PA0003044.6.1",
        "S+2" -> "PA0003044.6.2",
        "S+3" -> "PA0003044.6.3",
        "S+4" -> "PA0003044.6.4",
        "S+5" -> "PA0003044.6.5",
        "S+6" -> "PA0003044.6.6",
        "S+7" -> "PA0003044.6.7",
        "S+8" -> "PA0003044.6.8"
      ) mapValues { (_, hiLo) }
    }(Month(tradeDate) to Year(tradeDate) + 6)
  }
}
lazy val ttfLogReturns = logReturns(ttfHist)
lazy val ttfVola = vola(ttfLogReturns) mapValues { _ * sqrt(252.0) }

lazy val ncgHist = history(asof, 60) { tradeDate: Day =>
  if (tradeDate.isWeekend) None
  else {
    LimQuotes(tradeDate, svIn, GasStructure) {
      Map[String, String](
        "M+1" -> "PA0004723.6.1",
        "M+2" -> "PA0004723.6.2",
        "M+3" -> "PA0004723.6.3",
        "Q+1" -> "PA0009552.6.1",
        "Q+2" -> "PA0009552.6.2",
        "Q+3" -> "PA0009552.6.3",
        "S+1" -> "PA0009554.6.1",
        "S+2" -> "PA0009554.6.2",
        "S+3" -> "PA0009554.6.3",
        "S+4" -> "PA0009554.6.4",
        "S+5" -> "PA0009554.6.5",
        "S+6" -> "PA0009554.6.6",
        "S+7" -> "PA0009554.6.7",
        "S+8" -> "PA0009554.6.8"
      ) mapValues { (_, hiLo) }
    }(Month(tradeDate) to Year(tradeDate) + 6)
  }
}
lazy val ncgLogReturns = logReturns(ncgHist)
lazy val ncgVola = vola(ncgLogReturns) mapValues { _ * sqrt(252.0) }

lazy val nbpHist = history(asof, 60) { tradeDate: Day =>
  if (tradeDate.isWeekend) None
  else {
    val nbpFlat = LimQuotes(tradeDate, svIn, GasStructure) {
      Map[String, String](
        "M+1" -> "PA0002733.6.1",
        "M+2" -> "PA0002733.6.2",
        "M+3" -> "PA0002733.6.3",
        "Q+1" -> "PA0002739.6.1",
        "Q+2" -> "PA0002739.6.2",
        "Q+3" -> "PA0002739.6.3",
        "S+1" -> "PA0002744.6.1",
        "S+2" -> "PA0002744.6.2",
        "S+3" -> "PA0002744.6.3",
        "S+4" -> "PA0002744.6.4",
        "S+5" -> "PA0002744.6.5",
        "S+6" -> "PA0002744.6.6",
        "S+7" -> "PA0002744.6.7",
        "S+8" -> "PA0002744.6.8"
      ) mapValues { (_, hiLo) }
    }(tradeDate to Year(tradeDate) + 6)

    (nbpFlat, eurgbp(tradeDate).series) match {
      case (Some(underlying), Some(fx)) => Some(underlying / (fx * termToKWh) * 10.0)
      case _ => None
    }
  }
}
lazy val nbpLogReturns = logReturns(nbpHist)
lazy val nbpVola = vola(nbpLogReturns) mapValues { _ * sqrt(252.0) }

lazy val zeeHist = history(asof, 60) { tradeDate: Day =>
  if (tradeDate.isWeekend) None
  else {
    val zeeFlat = LimQuotes(tradeDate, svIn, GasStructure) {
      Map[String, String](
        "M+1" -> "PA0002735.6.1",
        "M+2" -> "PA0002735.6.2",
        "M+3" -> "PA0002735.6.3",
        "Q+1" -> "PA0002740.6.1",
        "Q+2" -> "PA0002740.6.2",
        "Q+3" -> "PA0002740.6.3",
        "S+1" -> "PA0002745.6.1",
        "S+2" -> "PA0002745.6.2",
        "S+3" -> "PA0002745.6.3",
        "S+4" -> "PA0002745.6.4",
        "S+5" -> "PA0002745.6.5",
        "S+6" -> "PA0002745.6.6",
        "S+7" -> "PA0002745.6.7",
        "S+8" -> "PA0002745.6.8"
      ) mapValues { (_, hiLo) }
    }(tradeDate to Year(tradeDate) + 6)

    (zeeFlat, eurgbp(tradeDate).series) match {
      case (Some(underlying), Some(fx)) => Some(underlying / (fx * termToKWh) * 10.0)
      case _ => None
    }
  }
}
lazy val zeeLogReturns = logReturns(zeeHist)
lazy val zeeVola = vola(zeeLogReturns) mapValues { _ * sqrt(252.0) }

lazy val zeeNbpCorrel = Interpolate.flatRight(correl(zeeLogReturns, nbpLogReturns))

object NBPIn extends svIn.Container("NG_NBP_FC") with svIn.ForwardCurveContainer {
  object Mid extends ForwardCurveStream[Day, Day]("Mid_%s")
}

object NBPOut extends svOut.Container("NG_NBP_FC") with svOut.ForwardCurveContainer {
  object MidEur extends ForwardCurveStream[Day, Day]("Mid_Eur_%s")
}

lazy val nbp = NBPIn.Mid.get(asof) flatMap {_.series}

lazy val nbpMidEur = for {
  nbp <- nbp
  eurgbp <- eurgbp.get(asof) flatMap {_.series}
  missingDays = nbp.time.takeWhile(!eurgbp.time.contains(_))
  if(missingDays.size match {
    case 0|1|2|3|4|5 => true
    case i =>
      log fatal ("No. of missing fx (%s) is > 5." format(i))
      false
  }
    )
} yield { missingDays.size match {
  case 0 =>
    nbp / eurgbp / 2.9307
  case _ =>
    val fx = Series(missingDays.map(d => (d, eurgbp.values.head))) ++ eurgbp
    nbp / fx / 2.9307
}
}

// ###############################################################################################
// ### MAIN
// ###
ttfMid map {mid => Spread(mid, ttfSpread)} map { s =>
  s.ask map {svOut.NgTtfFc.TrayportAsk.getOrCreate(asof) <-- _}
  s.bid map { svOut.NgTtfFc.TrayportBid.getOrCreate(asof) <-- _}
  s.bidAsk map { svOut.NgTtfFc.TrayportBidAsk.getOrCreate(asof) <-- _}
}
nbpMid map {mid => Spread(mid, nbpSpread)} map { s =>
  s.ask map {svOut.NgNbpFc.TrayportAsk.getOrCreate(asof) <-- _}
  s.bid map {svOut.NgNbpFc.TrayportBid.getOrCreate(asof) <-- _}
  s.bidAsk map {svOut.NgNbpFc.TrayportBidAsk.getOrCreate(asof) <-- _}
}
zeeMid map {mid => Spread(mid, zeeSpread)} map { s =>
  s.ask map {svOut.NgZeeFc.TrayportAsk.getOrCreate(asof) <-- _}
  s.bid map {svOut.NgZeeFc.TrayportBid.getOrCreate(asof) <-- _}
  s.bidAsk map {svOut.NgZeeFc.TrayportBidAsk.getOrCreate(asof) <-- _}
}
api2Mid map {mid => Spread(mid, api2Spread)} map { s =>
  s.ask map {svOut.CoalApi2Fc.TrayportAsk.getOrCreate(asof) <-- _}
  s.bid map {svOut.CoalApi2Fc.TrayportBid.getOrCreate(asof) <-- _}
  s.bidAsk map {svOut.CoalApi2Fc.TrayportBidAsk.getOrCreate(asof) <-- _}
}

gasOilGovernmentDuty map { s =>
  svOut.Index.Gasoilgovernmentduty.getOrCreate(asof) <-- cut(s)
  svOut.Index.Gasoilgovernmentdutydaily.getOrCreate(asof) <-- (change(cut(s)) to Day)
}

fuelOilGovernmentDuty map { s =>
  svOut.Index.Fueloilgovernmentduty.getOrCreate(asof) <-- cut(s)
  svOut.Index.Fueloilgovernmentdutydaily.getOrCreate(asof) <-- (change(cut(s)) to Day)
}

retailPriceIndex map { svOut.Index.Retailpriceindex.getOrCreate(asof) <-- cut(_) }
productPriceIndex map { svOut.Index.Productpriceindex.getOrCreate(asof) <-- cut(_) }

industrialElectricityIndex map { change(_) to Month from (Month(asof) - 6.months) } map { svOut.Index.Industrialelectricityindex.getOrCreate(asof) <-- _ }
goEnergyTrendEndur(GoEnergyTrendParam) map { change(_) to Month from (Month(asof) - 6.months) } map { svOut.Index.Gasoilenergytrend.getOrCreate(asof) <-- _ }
foEnergyTrendEndur(FoEnergyTrendParam) map { change(_) to Month from (Month(asof) - 6.months) } map { svOut.Index.Fueloilenergytrend.getOrCreate(asof) <-- _ }

nbpMidEur map {NBPOut.MidEur.getOrCreate(asof) <-- _}
