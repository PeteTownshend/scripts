import scala.collection.SortedSet

val svIn = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim
val svOut = svIn
val asof = yesterday //running at 5:00 for previous day
implicit val wb = FO_SUPPORT

// ###############################################################################################
// ### SETTINGS
// ###

val mtpEnd = Day(2016, 12, 31)
log info ("running for " + asof)
def lookUp = asof iterator - 5.days

implicit def implicitLim = svIn
implicit def horizon = asof iterator - 48.months
def govDutyIter = (asof:Month).firstOfMonth - 1.days iterator - 20.quarters
def trendIter = Day((asof:Quarter)) - 1.days iterator - 20.quarters
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
lazy val eurUsdMidGmv = new Gmv.ForwardCurveStream[Day, Year]("EUR_USD_MID")
lazy val eurGbpMidGmv = new Gmv.ForwardCurveStream[Day, Year]("Eur_GBP_MID")

def cut[T<:DateTimeLike[T]](s: Series[T, Double]) =
  (change(s) to Month).from(asof)

def hist(symbol: String, field: String*)(implicit lim: Lim, iter: Iterator[Day]) = Interpolate.omit(
  field.toList match {

    case Nil =>
      lim(iter, symbol, "no field given")

    case field :: Nil =>
      lim(iter, symbol, field)

    case field1 :: field2 :: Nil =>
      lim(iter, symbol, (field1, field2))()

    case _ =>
      lim(iter, symbol, "more than one field given")
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

trait ProductStructure {
  def extractor[T <: DateTimeLike[T]]: PartialFunction[(T, Day, Map[String, Double]), String]
}

object EmptyStructure extends ProductStructure {
  def extractor[T <: DateTimeLike[T]]: PartialFunction[(T, Day, Map[String, Double]), String] = Map.empty
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

trait BhpParam {
  val IP = 11.66644 // p/therm
  val GO = 0.15
  val GOo = 126.975
  val RPI = 0.85
  val RPIo = 120.15
}

object BhpParam extends BhpParam

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

object BritanniaParam extends BritanniaParam

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

// ###############################################################################################
// ### FX
// ###
lazy val gbpusd                       = svIn.Reuters1730.GbpUsdMid.fallBackSeries(lookUp) map {concat(_, hist("USDGBP_BOE", "Spot"))}
lazy val gbpForecast 	                = for {
  eurUsd <- eurUsdMidGmv.get(today) flatMap { _.series }
  eurGbp <- eurGbpMidGmv.get(today) flatMap { _.series }
} yield eurUsd / eurGbp

lazy val gbpExtended 	                = for {
  fx    <- gbpusd map {change(_) to Month} map {_ to mtpEnd}
  fxGmv <- gbpForecast map {change(_) to Month}
  gab = change(Interpolate.linear(quarterlyGapped(fx, fxGmv))) to Month
} yield fx ++ gab ++ fxGmv

// ###############################################################################################
// ### INDICES
// ###
lazy val productPriceIndex 				  = for {
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
lazy val iceGo                        = svIn.OilEodFc.IceGasoilSwap.get(asof) flatMap {
  _.series} map {
  concat(_, hist("AARIN00", "Index"))
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
  go    <- iceGo map {change(_) to Month} map {_ to mtpEnd}
  goGmv <- iceGoForecast map {change(_) to Month}
  gab = change(Interpolate.linear(quarterlyGapped(go, goGmv))) to Month
} yield go ++ gab ++ goGmv

// ###############################################################################################
// ### FUELOIL
// ###
/**
  * Mid curve plus half bid-ask spread, since forward position is short fuel oil
  */
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
  fo    <- fo1FobNwe map {change(_) to Month} map {_ to mtpEnd}
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
  co    <- iceBrent map {change(_) to Month} map {_ to mtpEnd}
  coGmv <- iceBrentForecast map {change(_) to Month}
  gab =  change(Interpolate.linear(quarterlyGapped(co, coGmv))) to Month
} yield co ++ gab ++ coGmv

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

def goEnergyTrend(param: GoEnergyTrendParam, shock: Double) = for {
  fx <- gbpExtended
  go <- gasOilExtended map {_ * shock}
  govDuty <- gasOilGovernmentDuty
  etHist = gasOilEnergyTrendHistory
  etFwd = GoEnergyTrendFormula(param)(fx, go, govDuty)
} yield etHist ++ etFwd.from(etHist.end + 1)

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

// ###############################################################################################
// ### MAIN
// ###

bhp(BhpParam) map {cp.Bhp.getOrCreate(asof) <-- cut(_)}
bhp(BhpParam, 1.1) map {cpPlus10Pct.Bhp.getOrCreate(asof) <-- cut(_)}
bhp(BhpParam, 0.9) map {cpMinus10Pct.Bhp.getOrCreate(asof) <-- cut(_)}

britannia(BritanniaParam) map {cp.Britannia.getOrCreate(asof) <-- cut(_)}
britannia(BritanniaParam, 1.1) map {cpPlus10Pct.Britannia.getOrCreate(asof) <-- cut(_)}
britannia(BritanniaParam, 0.9) map {cpMinus10Pct.Britannia.getOrCreate(asof) <-- cut(_)}