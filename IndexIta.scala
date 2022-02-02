val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim

implicit val wb = FO_SUPPORT
val asof = today // running at 23.50 for current day

val RMDSfldr = "\\\\u-dom1.u-ssi.net\\DFSRoot34000\\PROJECTS\\IT\\MPD\\MPD_PROD\\RMDS_Export\\"  

val lookUp = -5 days
val horizon = -25 months
val length = Year(asof.year + 5)

def toRmds(unit: String) = (bucket: Service#Container#ForwardCurveLike[Day, Month, Double]#ForwardCurve) => bucket.series map {s =>
	//coreName is not available since eet.io 0.8.0
	val coreName = bucket.name.replace(bucket.tradeDate.toString("_yyyyMMdd"),"")
	val rmds = (e: (Month, Double)) =>
		"%s_MPM;%s;m;REF;0;%s;%s" format(coreName, Day(e._1).toString("dd.MM.yyyy"), e._2.toString.replace(".", ","), unit)
	write(RMDSfldr, "RMDS_%s%s.csv" format(coreName, bucket.tradeDate.toString("_yyyy-MM-dd"))) map {_(s.iterator, rmds)(None)}
}
val gas = srv.IndexGasIta
val gasToRmds = toRmds("EUR/m3")
val pow = srv.IndexPowerIta

val powToRmds = toRmds("EUR/MWh")
val go  = srv.GoEndurCurves
val c = 38.1 / 3.6  // MWh/km3
val bblPerMT = 7.4  // bbl/MT

// ###############################################################################################
// ### HELPERS
// ###

def tryOption[T](t: => T): Option[T] = try Some(t) catch {

	case t: Throwable =>
		log error t.getMessage
		None
}

/**
 * moved to 0.18
 */
val gasGrp07G01GasIdx = new gas.ForwardCurveStream[Day,Month]("GRP07_G01_GAS_IDX_%s")
val powItaGas02 = new pow.ForwardCurveStream[Day,Month]("ITA_GAS_02_%s")
val UlsdRemixG01Pi03 = new pow.ForwardCurveStream[Day,Month]("ULSD_REMIX_G01_PI03_%s")
val UlsdItecMsPi04 = new pow.ForwardCurveStream[Day, Month]("ULSD_ITEC_MS_PI04_%s")
val UlsdFcr80Pi09 = new pow.ForwardCurveStream[Day, Month]("ULSD_FCR80_PI09_%s")
val UlsdGofo12mPwr = new pow.ForwardCurveStream[Day, Month]("ULSD_GOFO_12M_PWR_%s")
val UlsdRsgbPi19 = new pow.ForwardCurveStream[Day, Month]("ULSD_RSGB_PI_19_%s")

val UlsdSwap = new go.ForwardCurveStream[Day, Month]("ICE_ULSD_SWAP_%s")
val UlsdCifMed = new go.ForwardCurveStream[Day, Month]("ULSD_10_PPM_CIF_MED_CARGOES_%s") //AAWYZ00 MidPoint
val UlsdFobAra = new go.ForwardCurveStream[Day, Month]("ULSD_10_PPM_FOB_ARA_BARGES_%s") //AAJUS00 MidPoint

lazy val GcItaFc = new srv.Container("GC_ITA_FC") with srv.ForwardCurveContainer
lazy val GmeCvMid = new GcItaFc.ForwardCurveStream[Day, Day]("GME_CV_MID_%s")

//Footnotes
//(1) projection applied to fx-converted underlying is intended

val projective_1_3_9_1 = projective[Month, Double](1, 3, 9, 1) _

val projective_1_1_9_0 = projective[Month, Double](1, 1, 9, 0) _

def iter = asof iterator lookUp

def hist(symbol: String, field: String*) = Interpolate.omit( 
	field.toList match {
		case field::Nil => srv(asof iterator horizon, symbol, field)
		case field1::field2::Nil => srv(asof iterator horizon, symbol, (field1,field2))()
		case _ => sys.error("expecting one or two fields")
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
		case (h,f, lastDay) if(h == f) =>
			val days = h.toDays filter {!_.isWeekend}
			val avg = (mhist.last._2 * days.filter(_ <= lastDay).size + mfwd.head._2 * days.filter(_ > lastDay).size) / days.size
			(mhist ++ mfwd).updated((h,avg))
		case _ => mhist ++ mfwd
	}
}

/**
 * taken from eet.core.collection.algorithms 0.12.0 to handle tuple of option
 */
object flattenLocal {

  trait F[X, R] extends (X => List[R])

  implicit def fltAny[X, Y, R](implicit x: F[X, _ <: R], y: F[Y, _ <: R]): F[(X, Y), R] =
    new F[(X, Y), R] { def apply(xy: (X, Y)) = x(xy._1) ::: y(xy._2) }

  implicit def flt[X, R](implicit f: (X => R)): F[X, R] =
    new F[X, R] { def apply(x: X) = List(f(x)) }

  /** Flattens nested Tuple2s with homogeneous types into plain List */
  def apply[X, R](x: X)(implicit f: F[X, R]) = f(x)

  /** Flattens nested Tuple2s with hoterogeneous types into plain List */
  def to[R] = new { def apply[X](x: X)(implicit f: F[X, R]) = f(x) }
}

// ###############################################################################################
// ### COMMODITIES
// ###

// FX
lazy val usdeur = srv.Reuters1730.EurUsdMid.fallBackSeries(iter) map {concat(_, hist("ECBUSD", "Spot"))} map {1.0 / _}

// GASOIL
lazy val go01CifMedNew = for {
  fwd <- go.Go01CifMedCargoes.fallBackSeries(iter)
  current <- go.Go01CifMedCMonthlyAvgUsd.fallBackSeries(iter)
  history = change(hist("AAVJJ03", "High", "Low")) to Month
} yield fwd ++ current ++ history
lazy val go01CifMed = go.Go01CifMedCargoes.fallBackSeries(iter) 	map {concat(_, hist("AAVJJ00", "High", "Low"))}
lazy val go01FobAra = go.Go01FobAraBarges.fallBackSeries(iter) 		map {concat(_, hist("AAYWT00", "High", "Low"))}
lazy val go02CifMed = go.Go02CifMedCargoes.fallBackSeries(iter)		map {concat(_, hist("AAVJJ00", "High", "Low"))} // refers to 01, not in LIM

// FUELOIL
lazy val fo1CifMedNew  = for {
  fwd <- go.Fo1CifMedCargoes.fallBackSeries(iter)    
  current <- go.Fo1CifMedCMonthlyAvgUsd.fallBackSeries(iter)   
  history = change(hist("PUAAJ03", "High", "Low")) to Month
} yield fwd ++ current ++ history
lazy val fo1CifMed  = go.Fo1CifMedCargoes.fallBackSeries(iter)		map {concat(_, hist("PUAAJ00", "High", "Low"))}
lazy val fo1FobAra  = go.Fo1FobAraBarges.fallBackSeries(iter)			map {concat(_, hist("PUAAP00", "High", "Low"))}
lazy val fo1FobNwe  = go.Fo1FobNweCargoes.fallBackSeries(iter)		map {concat(_, hist("PUAAM00", "High", "Low"))}
lazy val fo1CifNwe  = go.Fo1CifNweCargoes.fallBackSeries(iter)		map {concat(_, hist("PUAAL00", "High", "Low"))}
lazy val fo35CifMed = go.Fo35CifMedCargoes.fallBackSeries(iter)		map {concat(_, hist("PUAAY00", "High", "Low"))}
lazy val fo35FobAra = go.Fo35FobAraBarges.fallBackSeries(iter)		map {concat(_, hist("PUABC00", "High", "Low"))}

// OIL
lazy val dtdBrentNew = for {
  fwd <- go.DatedBrentSwap.fallBackSeries(iter)
  current <- go.DatedBrentMonthlyAvgUsd.fallBackSeries(iter)
  history = change(hist("PCAAS03", "High", "Low")) to Month
} yield fwd ++ current ++ history
lazy val dtdBrent = go.DatedBrentSwap.fallBackSeries(iter)				map {concat(_, hist("PCAAS00", "High", "Low"))}
lazy val iceBrent = go.IceBrentSwap.fallBackSeries(iter)					map {concat(_, hist("FB", "Close"))}
lazy val iceGO    = go.IceGasoilSwap.fallBackSeries(iter)					map {concat(_, hist("FP", "Close"))}
//swap curve but use Morningstar quote for the futures contract, TODO rollover not reflected in ICEOTC.ULS (their is no ExpYrMth field)
lazy val ulsdSwap = UlsdSwap.fallBackSeries(iter)	map change(hist("ICE.ULS", "Close")).to(Month).++
lazy val ulsdCifMed = UlsdCifMed.fallBackSeries(iter) map {concat(_, hist("AAWYZ00", "High", "Low"))}
lazy val ulsdFobAra = UlsdFobAra.fallBackSeries(iter) map {concat(_, hist("AAJUS00", "High", "Low"))}

lazy val fobBreakEven = for {
	ara <- go.CrudeArabianLightCrackNet.fallBackSeries(iter)	map {concat(_, hist("TNABT00", "Index"))}
	ira <- go.CrudeIranianLightCrackNet.fallBackSeries(iter)	map {concat(_, hist("TNAMD00", "Index"))}
	kuw <- go.CrudeKuwaitCrackNet.fallBackSeries(iter)				map {concat(_, hist("TNAMX00", "Index"))}	
	aik = (ara + ira + kuw) / 3.0
	k = go.CrudeKirkukCrackNet.get(asof) 			flatMap {_.series} map {concat(_, hist("TNATU00", "Index"))} getOrElse 1.035 * aik
	m = go.CrudeMurbanCrackNet.get(asof) 			flatMap {_.series} map {concat(_, hist("TNATQ00", "Index"))} getOrElse 1.045 * aik
  s = go.CrudeSaharanCrackNet.get(asof)			flatMap {_.series} map {concat(_, hist("TNATY00", "Index"))} getOrElse 1.112 * aik
  z = go.CrudeZueitinaCrackNet.get(asof)		flatMap {_.series} map {concat(_, hist("TNAUC00", "Index"))} getOrElse 1.115 * aik
  b = go.CrudeBrassRiverCrackNet.get(asof)	flatMap {_.series} map {concat(_, hist("TNATS00", "Index"))} getOrElse 1.138 * aik
} yield (3.0 * aik + k + m + s + z + b) / 8.0 roundTo 3

// POWER
// TODO replace powIta by powIta2, only powIta2 has the correct concatenation logic since power is also quoted on weekends
// hence, there is no gap and concatenating according to the number of weekdays is not correct any more.

// monthly
lazy val powIta = srv.PowerItaFc.Mid.fallBackSeries(iter) map {concat(_, hist("IPEX_SINGLE_NATIONAL_PRICE", "Base"))}
// daily
lazy val powIta2 = for {
	fwd <- srv.PowerItaFc.Mid.fallBackSeries(iter) map (change(_) to Day)
	lim = srv(asof iterator horizon, "IPEX_SINGLE_NATIONAL_PRICE", "Base") from (Day(asof.year, 1, 1)) //daily quotes, even for weekends and holidays
	if(lim.filterNot (_._2.isDefined).isEmpty)
	hist = Interpolate.omit(lim)
} yield hist ++ fwd

// FREIGHT
lazy val bestFreight = for{
  rc4 <- srv.FreightRc4Fc.BalticMid.fallBackSeries(iter) map {hist("BALTIC.CAPESIZE.ROUTE.C4", "Val") ++ _}
  rc7 <- srv.FreightRc7Fc.BalticMid.fallBackSeries(iter) map {hist("BALTIC.CAPESIZE.ROUTE.C7", "Val") ++ _}
  bol <- srv.FreightSynthFc.PanamaxBolivarRdamMid.fallBackSeries(iter) 
  bay <- srv.FreightSynthFc.PanamaxRichbayRdamMid.fallBackSeries(iter)
  ban <- srv.FreightSynthFc.PanamaxBanjarRdamMid.fallBackSeries(iter)
} yield List(rc4, rc7, bol, bay, ban).map{change(_) to Month}.reduceLeft(_.all.zip(_) mapValues {p => flattenLocal(p).flatten.min})

// COAL
lazy val api2    = srv.CoalApi2Fc.MidRmds.fallBackSeries(iter) map {hist("PA0002140.0.0", "Index") ++ _} map{change(_) to Month}
lazy val api2Tfs = srv.CoalApi2Fc.TfsMid.fallBackSeries(iter) 	map {hist("PA0002140.0.0", "Index") ++ _} map{change(_) to Month}
lazy val api4Tfs = srv.CoalApi4Fc.TfsMid.fallBackSeries(iter) 	map {hist("PA0002141.0.0", "Index") ++ _} map{change(_) to Month}
lazy val coalIdx = for {
  best <- bestFreight
  ue   <- usdeur
  api2 <- api2Tfs
  api4 <- api4Tfs
} yield ue * (-0.5 + api2 + 0.6 * (api4 - api2 + best).lag(-1))

// GAS
lazy val psv = srv.NgPsvFc.Mid.fallBackSeries(iter) map {concat(_, hist("ALBA.PSV.DA", "Bid", "Offer"))}


// ###############################################################################################
// ### CALCULATION OF GAS INDICES - all indices in EUR/km3
// ###

lazy val eniRemixGas = for { // Idx 4
	eurusd <- usdeur       map {ue => projective.mean(9.months,month)(0.2 / ue) + 0.8 / ue}
	go     <- go01CifMed   map projective.mean(9.months,month)
	foLow  <- fo1CifMed    map projective.mean(9.months,month)
	foHgh  <- fo35CifMed   map projective.mean(9.months,month)
	oil    <- fobBreakEven map {o => projective.mean(9.months,month)(bblPerMT * o)}
} yield (0.0172 * go + 0.0141 * foLow + 0.0240 * foHgh + 0.0140 * oil) / eurusd roundTo 2
	
// -----------------------------------------------------------------------------------------------

lazy val ulsdEniRemixGas = for { // added on Jan 28, 2014, NPA 647
	eurusd <- usdeur       map {ue => projective.mean(9.months,month)(0.2 / ue) + 0.8 / ue}
	go     <- ulsdCifMed   map projective.mean(9.months,month)
	foLow  <- fo1CifMed    map projective.mean(9.months,month)
	foHgh  <- fo35CifMed   map projective.mean(9.months,month)
	oil    <- fobBreakEven map {o => projective.mean(9.months,month)(bblPerMT * o)}
} yield (0.0172 * go + 0.0141 * foLow + 0.0240 * foHgh + 0.0140 * oil) / eurusd roundTo 2

// -----------------------------------------------------------------------------------------------

lazy val gimp = for {
	eni <- eniRemixGas map {_.lag(-1)}
	val (summer, winter) = 
		if (asof < Day(2011, 10, 1)) 
			(-3.913, -1.411) 
		else 
			(-4.2, -1.8)
	spread <- tryOption{Series(eni.start to eni.end map {m => if (3 < m.month && m.month < 11) m -> summer else m -> winter})}
} yield eni + spread
	
// -----------------------------------------------------------------------------------------------

lazy val grp07G01Gas = for {
	ue  <- usdeur
	go  <- go01CifMed map projective_1_1_9_0 map {_ * ue} 
	fo  <- fo1CifMed map projective_1_1_9_0 map {_ * ue} 
	oil <- dtdBrent map projective_1_1_9_0 map {_ * ue * bblPerMT}
} yield (0.41 * go / 219.14).roundTo(3) + (0.46 * fo / 141.07).roundTo(3) + (0.13 * oil / 182.5).roundTo(3) roundTo 3 

// -----------------------------------------------------------------------------------------------

lazy val grp07G01GasIdx = for { // Idx 5 added on Apr 5, 2013
  ue  <- usdeur
  go  <- go01CifMedNew map projective_1_1_9_0 map {_ * ue}
  fo  <- fo1CifMedNew map projective_1_1_9_0 map {_ * ue}
  oil <- dtdBrentNew map projective_1_1_9_0 map {_ * ue}
  idx = (0.41 * go / 219.14).roundTo(3) + (0.46 * fo / 141.07).roundTo(3) + (0.13 * oil * bblPerMT / 182.5).roundTo(3) roundTo 3 
} yield idx * 0.95 * 120.05

// -----------------------------------------------------------------------------------------------

lazy val noSG01Gas = for { // Idx 10 added on Apr 5, 2013
  ue <- usdeur
  fo <- fo1CifMedNew map {_ * ue} map projective_1_3_9_1
  go <- go01CifMedNew map {_ * ue} map projective_1_3_9_1
  oil <- fobBreakEven map {_ * ue} map projective_1_3_9_1
  lt = go * 0.49 / 219.137 + fo * 0.38 / 141.07 + oil * bblPerMT * 0.13 / 184.792 roundTo 3
} yield lt * 120.05

// -----------------------------------------------------------------------------------------------

lazy val qe8910Gas = for { // Idx 16 added on Apr 5, 2013
  ue <- usdeur
  go <- go01CifMedNew map {_ * ue} map projective_1_3_9_1
  fo <- fo1CifMedNew map {_ * ue} map projective_1_3_9_1 
  oil <- dtdBrentNew map {_ * ue} map projective_1_3_9_1
  qe0 = 7.054 * 0.925 * 38.1 // 38.1 not from spec but according to endur screen shot
  go0 = 55.8337 //Ct/kg
  fo0 = 32.1787 //Ct/kg
  oil0 = 41.5377 //Ct/kg
  lt = (go / 10d roundTo 4) / go0 * 0.41 + (fo / 10d roundTo 4) / fo0 * 0.46 + (oil * bblPerMT / 10d roundTo 4) / oil0 * 0.13 roundTo 3
} yield lt mapValues {v => if(v >= 0.788) v * qe0 else v * (qe0 - 1.181205)}

// -----------------------------------------------------------------------------------------------

lazy val itEdisonAt1011 = for { // Idx 17
	ue    <- usdeur
	go    <- go01CifMed map projective.mean(9.months,month)
	foLow <- fo1CifMed  map projective.mean(9.months,month)
	foHgh <- fo35CifMed map projective.mean(9.months,month)
	oil   <- dtdBrent   map {o => projective.mean(9.months,month)(bblPerMT * o)}
} yield ue * 0.06131 * (0.17 * go + 0.21 * foLow + 0.12 * foHgh + 0.50 * oil) roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val itEdisonAt1213 = for { // added on June 18, 2012
	ue    <- usdeur
	go    <- go01CifMed map projective.mean(9.months,month)
	foLow <- fo1CifMed  map projective.mean(9.months,month)
	foHgh <- fo35CifMed map projective.mean(9.months,month)
	oil   <- dtdBrent   map {o => projective.mean(9.months,month)(bblPerMT * o)}
} yield ue * 0.058174 * (0.19 * go + 0.26 * foLow + 0.05 * foHgh + 0.50 * oil) roundTo 2

// -----------------------------------------------------------------------------------------------

lazy val brent = for { // added on Jan 05, 2012
	ue	<- usdeur		map {ue => (projective.mean(9.months,month)(0.2 * ue) + 0.8 * ue) roundTo 4}	
	oil	<- iceBrent map projective.mean(9.months,month)
} yield 0.618 * bblPerMT * oil * ue roundTo 2 

// -----------------------------------------------------------------------------------------------
	
lazy val gas01 = for { // added on Jan 05, 2012
	ue	 <- usdeur 			map {ue => (projective.mean(9.months,month)(0.2 * ue) + 0.8 * ue) roundTo 4}	
	go	 <- iceGO  			map {go => 	projective.mean(9.months,month)(go) roundTo 3}
	oil	 <- iceBrent 		map {o  => 	projective.mean(9.months,month)(o) roundTo 3}
	fo1	 <- fo1FobNwe 	map {fo => 	projective.mean(9.months,month)(fo) roundTo 3}
	fo35 <- fo35FobAra 	map {fo => 	projective.mean(9.months,month)(fo) roundTo 3}
} yield	(0.172 * go + 0.141 * fo1 + 0.24 * fo35 + 0.14 * bblPerMT * oil) * ue roundTo 2 
	
// ###############################################################################################
// ### CALCULATION OF POWER INDICES - all indices in EUR/MWh
// ###

lazy val brent3MPi07 = for { // Idx 2 (1)
	ue  <- usdeur
	oil <- dtdBrent map {o => projective.mean(3.months,month)(ue * o)}
} yield 0.771265 * oil roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val br601Pwr = for { // Idx 3
	ue  <- usdeur
	oil <- iceBrent map projective.mean(6.months,month)
} yield oil * ue roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val itecMsPi04 = for { // Idx 4 (1)
	ue   <- usdeur     map {_ roundTo 4}
	go   <- go01FobAra map {g => projective.mean(6.months,month)(ue * g)}
	fo   <- fo1FobAra  map {ue * _}
	coal <- api2Tfs    map {c => ue * c.lag(-1)}
	oil  <- dtdBrent   map {o => projective.mean(6.months,month)(bblPerMT * ue * o)}
} yield {	
	val pCoal = (coal + 6.0) / 6.96
	val pOil = (1.061 * fo.lag(-1) + 4.0) / 11.368
	val pGas = 13.749 * (go / 248.49 + projective.mean(6.months,month)(fo) / 128.23 + oil / 208.24) / 3.0 + 2.332
	0.225 / 0.35 * pCoal + 0.08 * pOil / 0.35 + 0.695 * pGas / 0.5 roundTo 2 		
}

// -----------------------------------------------------------------------------------------------

lazy val ulsdItecMsPi04 = for { // added on Jan 28, 2014, NPA 647
	ue   <- usdeur     map {_ roundTo 4}
	go   <- ulsdFobAra map {g => projective.mean(6.months,month)(ue * g)}
	fo   <- fo1FobAra  map {ue * _}
	coal <- api2Tfs    map {c => ue * c.lag(-1)}
	oil  <- dtdBrent   map {o => projective.mean(6.months,month)(bblPerMT * ue * o)}
} yield {	
	val pCoal = (coal + 6.0) / 6.96
	val pOil = (1.061 * fo.lag(-1) + 4.0) / 11.368
	val pGas = 13.749 * (go / 248.49 + projective.mean(6.months,month)(fo) / 128.23 + oil / 208.24) / 3.0 + 2.332
	0.225 / 0.35 * pCoal + 0.08 * pOil / 0.35 + 0.695 * pGas / 0.5 roundTo 2 		
}

// -----------------------------------------------------------------------------------------------

lazy val fcPwr = for { // Idx 5
	coal  <- coalIdx
	remix <- eniRemixGas
	gas = remix collect {
		case (t, x) if (t.month >= 4 && t.month < 11) => (t, x + 2.373)
		case (t, x) => (t, x + 6.727)
	}
} yield 0.75 * 1.933 * gas + 0.25 * 0.42 * coal roundTo 2 
	
// -----------------------------------------------------------------------------------------------

lazy val punMBl = for { // Idx 6
	pow <- powIta
} yield pow roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val fcsPi08 = for { // Idx 7
	ue    <- usdeur
	api2  <- api2Tfs
	api4  <- api4Tfs
	remix <- eniRemixGas
	coal = ue * (0.3 * api2 + 0.003 * api4 - 0.15)
} yield 0.65 * 1.976 / 0.8857 * remix + 0.35 * (coal + 17.77) roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val dtd3mPwr = for { // Idx 8 (1)
	ue  <- usdeur
	oil <- dtdBrent
} yield	projective.mean(3.months,month)(ue * oil) roundTo 2

// -----------------------------------------------------------------------------------------------

lazy val fc2Pwr = for { // Idx 9
	coal <- coalIdx
	remix <- eniRemixGas
	gas = remix collect {
		case (t, x) if (t.month >= 4 && t.month < 11) => (t, x + 2.373)
		case (t, x) => (t, x + 6.727)
	}
} yield 0.84 * 1.933 * remix + 0.16 * 0.42 * coal roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val fcr80Pi09 = for { // Idx 10
	ue    <- usdeur
	api2  <- api2Tfs
	remix <- eniRemixGas
	coal = ue * (0.055 * api2 - 0.1)
} yield	1.55 * (4.4 + remix + coal) roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val ulsdFcr80Pi09 = for { // added on Jan 28, 2014, NPA 647
	ue    <- usdeur
	api2  <- api2Tfs
	remix <- ulsdEniRemixGas
	coal = ue * (0.055 * api2 - 0.1)
} yield	1.55 * (4.4 + remix + coal) roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val fc3Pwr = for { // Idx 11
	ue    <- usdeur
	go    <- go01CifMed   map {g => ue * projective.mean(9.months,month)(g)}
	foLow <- fo1CifMed    map {f => ue * projective.mean(9.months,month)(f)}
	foHgh <- fo35CifMed   map {f => ue * projective.mean(9.months,month)(f)}
	crack <- fobBreakEven map {c => ue * projective.mean(9.months,month)(c)}
	oil   <- dtdBrent     map {o => ue * projective.mean(9.months,month)(o)}
	api2  <- api2Tfs      map {c => ue * c.lag(-1)}
	api4  <- api4Tfs      map {c => ue * c.lag(-1)}
	best  <- bestFreight  map {c => ue * c.lag(-1)}
	coal = -0.150645 * api2 + 0.293578 * api4 + 0.296336 * best
} yield 0.440411 * oil - 0.401823 * crack + 0.036878 * go + 0.125404 * foLow - 0.090523 * foHgh + coal + 3.9125 roundTo 3 

// -----------------------------------------------------------------------------------------------

lazy val bbGas = for { // Idx 12
	gas <- grp07G01Gas
	g = 12.005 * 1.9557 * 0.95 * gas collect {
		case (t, x) if (t.month >= 4 && t.month < 11) => (t, x + 5.4189)
		case (t, x) => (t, x + 11.4347)
	}
} yield	g roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val remixG01Pi03 = for { // Idx 13
	remix <- eniRemixGas
} yield 1.976 * remix roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val ulsdRemixG01Pi03 = for { // added on Jan 28, 2014, NPA 647
	remix <- ulsdEniRemixGas
} yield 1.976 * remix roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val fc20111Gas = for { // Idx 15
	gas <- grp07G01Gas
	g = 12.005 * 0.95 * gas collect {
		case (t, x) if (t.month >= 4 && t.month < 11) => (t, x + 2.8323)
		case (t, x) => (t, x + 5.8273)
	}
} yield 1.96 * g roundTo 2 

// -----------------------------------------------------------------------------------------------
	
lazy val fc20111 = for { // Idx 14
	gas  <- fc20111Gas
	coal <- coalIdx
} yield 0.73 * gas + 0.27 * 0.43 * coal roundTo 3 

// -----------------------------------------------------------------------------------------------

lazy val fc20111Coal = for { // Idx 16
	coal <- coalIdx
} yield 0.43 * coal roundTo 3 

// -----------------------------------------------------------------------------------------------

lazy val itGrp2G01Pwr = for { // Idx 17
	gas <- grp07G01Gas
} yield {
	120.05 * 0.95 * 1.976 / 10 * gas roundTo 3 
}

// -----------------------------------------------------------------------------------------------

lazy val bgf901Pwr = for { // Idx 18
	ue  <- usdeur
	go  <- iceGO      map projective.mean(9.months,month)
	fo  <- fo35FobAra map projective.mean(9.months,month)
	oil <- iceBrent   map projective.mean(9.months,month)
} yield	ue * (0.45 / 7.45 * go + 0.275 * oil + 0.275 / 6.35 * fo) roundTo 3 

// -----------------------------------------------------------------------------------------------

lazy val br901Pwr = for { // Idx 19
	ue  <- usdeur
	oil <- iceBrent map {o => projective.mean(9.months,month)(ue * o)}
} yield	oil roundTo 3 

// -----------------------------------------------------------------------------------------------

lazy val gofo12mPwr = for { // Idx 20 (1)
	ue <- usdeur
	go <- go01CifMed map {g => projective.mean(year,month)(ue * g)}
	fo <- fo1CifMed  map {f => projective.mean(year,month)(ue * f)}
} yield	0.080964 * go + 0.099271 * fo roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val ulsdGofo12mPwr = for { // added on Jan 28, 2014, NPA 647
	ue <- usdeur
	go <- ulsdCifMed map {g => projective.mean(year,month)(ue * g)}
	fo <- fo1CifMed  map {f => projective.mean(year,month)(ue * f)}
} yield	0.080964 * go + 0.099271 * fo roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val consipPwr = for { // Idx 21 (1)
	ue  <- usdeur
	oil <- dtdBrent  map {f => projective.mean(9.months,month)(ue * f)}
	fo  <- fo1CifNwe map {f => projective.mean(9.months,month)(ue * f)}
} yield	0.423 * oil + 0.051 * fo roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val cip6Pun = for { // Idx 22
	power <- punMBl
	it = 57 / 56.34 * (change(power) to Quarter).lag(-1)
} yield change(it) to Month 

// -----------------------------------------------------------------------------------------------

lazy val fcCoalApi2 = for { // Idx 23
	ue   <- usdeur
	coal <- api2
} yield 0.43 * ue * coal 
	
// -----------------------------------------------------------------------------------------------

lazy val rsgbPi19 = for { // Idx 24, added on July 26, taken from itecMsPi04, but go and fo are different
	ue   <- usdeur     map {_ roundTo 4}
	go   <- iceGO			 map {g => projective.mean(6.months,month)(ue * g)}
	fo   <- fo1FobNwe  map {ue * _}
	coal <- api2Tfs    map {c => ue * c.lag(-1)}
	oil  <- dtdBrent   map {o => projective.mean(6.months,month)(bblPerMT * ue * o)}
	pCoal = (coal + 6.0) / 6.96
	pOil = (1.061 * fo.lag(-1) + 4.0) / 11.368
	pGas = 13.749 * (go / 248.49 + projective.mean(6.months,month)(fo) / 128.23 + oil / 208.24) / 3.0 + 2.332
} yield 0.225 / 0.35 * pCoal + 0.08 * pOil / 0.35 + 0.695 * pGas / 0.5 roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val ulsdRsgbPi19 = for { // added on Jan 28, 2014, NPA 647
	ue   <- usdeur     map {_ roundTo 4}
	go   <- ulsdSwap	 map {g => projective.mean(6.months,month)(ue * g)}
	fo   <- fo1FobNwe  map {ue * _}
	coal <- api2Tfs    map {c => ue * c.lag(-1)}
	oil  <- dtdBrent   map {o => projective.mean(6.months,month)(bblPerMT * ue * o)}
	pCoal = (coal + 6.0) / 6.96
	pOil = (1.061 * fo.lag(-1) + 4.0) / 11.368
	pGas = 13.749 * (go / 248.49 + projective.mean(6.months,month)(fo) / 128.23 + oil / 208.24) / 3.0 + 2.332
} yield 0.225 / 0.35 * pCoal + 0.08 * pOil / 0.35 + 0.695 * pGas / 0.5 roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val br301Pwr = for { // Idx 25
	ue  <- usdeur
	oil <- iceBrent map {o => projective.mean(quarter,month)(ue * o)}
} yield oil roundTo 3 

// -----------------------------------------------------------------------------------------------

lazy val itaPsv = psv map { _ roundTo 3 } //added on Feb 1, 2013, NPA 590

// -----------------------------------------------------------------------------------------------

lazy val gas01Pwr = for { //added on Feb 1, 2013, NPA 589
	ue	 <- usdeur 			map {ue => (projective.mean(9.months,month)(0.2 * ue) + 0.8 * ue)}	
	go	 <- iceGO  			map {go => 	projective.mean(9.months,month)(go)}
	oil	 <- iceBrent 		map {o  => 	projective.mean(9.months,month)(o)}
	fo1	 <- fo1FobNwe 	map {fo => 	projective.mean(9.months,month)(fo)}
	fo35 <- fo35FobAra 	map {fo => 	projective.mean(9.months,month)(fo)}
} yield	(0.172 * go + 0.141 * fo1 + 0.24 * fo35 + 0.14 * bblPerMT * oil) * ue * 0.195 roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val gas02Pwr = for { //added on Dec 2, 2013, NPA 673
	ue	 <- usdeur 			map {ue => (projective.mean(9.months,month)(0.2 * ue) + 0.8 * ue)}	
	go	 <- ulsdSwap   map projective.mean(9.months,month)
	oil	 <- iceBrent 		map projective.mean(9.months,month)
	fo1	 <- fo1FobNwe 	map projective.mean(9.months,month)
	fo35 <- fo35FobAra 	map projective.mean(9.months,month)
} yield	(0.172 * go + 0.141 * fo1 + 0.24 * fo35 + 0.14 * bblPerMT * oil) * ue * 0.195 roundTo 2 

// -----------------------------------------------------------------------------------------------

lazy val gmeCv = for {
	pow <- powIta2 map (change(_) to Year)
} yield 0.78 * (change(180d - pow) to Day)

// -----------------------------------------------------------------------------------------------

// INDEX GAS
eniRemixGas    map {_ from asof} map {gas.EniRemixGas.getOrCreate(asof) 			<-- _} // map gasToRmds
gimp		   		 map {_ from asof} map {gas.Gimp.getOrCreate(asof) 							<-- _} // map gasToRmds
grp07G01Gas    map {_ from asof} map {gas.Grp07G01Gas.getOrCreate(asof) 			<-- _} // map gasToRmds
grp07G01GasIdx map {_ from asof} map {gasGrp07G01GasIdx.getOrCreate(asof) 		<-- _} // map gasToRmds
qe8910Gas      map {_ from asof} map {gas.Qe8910Gas.getOrCreate(asof) 				<-- _}
noSG01Gas      map {_ from asof} map {gas.NoSG01Gas.getOrCreate(asof)					<-- _}
itEdisonAt1011 map {_ from asof} map {gas.ItEdisonAt1011.getOrCreate(asof) 		<-- _} // map gasToRmds
itEdisonAt1213 map {_ from asof} map {gas.ItEdisonAt1213.getOrCreate(asof) 		<-- _} // map gasToRmds
brent		   		 map {_ from asof} map {s => gas.GasBrent.getOrCreate(asof) <-- s.until(length)} map gasToRmds
gas01 				 map {_ from asof} map {s => gas.Gas01.getOrCreate(asof) <-- s.until(length)} map gasToRmds

// INDEX POWER
brent3MPi07    map {_ from asof} map {pow.Brent3mPi07.getOrCreate(asof) 	<-- _} map powToRmds
br601Pwr       map {_ from asof} map {pow.Br601Pwr.getOrCreate(asof) 			<-- _} map powToRmds
itecMsPi04     map {_ from asof} map {pow.ItecMsPi04.getOrCreate(asof) 		<-- _} map powToRmds
ulsdItecMsPi04 map {_ from asof} map {UlsdItecMsPi04.getOrCreate(asof) 		<-- _} map powToRmds
fcPwr          map {_ from asof} map {pow.FcPwr.getOrCreate(asof) 				<-- _} map powToRmds
punMBl         map {_ from asof} map {pow.PunMBl.getOrCreate(asof) 				<-- _} map powToRmds
fcsPi08        map {_ from asof} map {pow.FcsPi08.getOrCreate(asof) 			<-- _} map powToRmds
dtd3mPwr       map {_ from asof} map {pow.Dtd3mPwr.getOrCreate(asof) 			<-- _} map powToRmds
fc2Pwr         map {_ from asof} map {pow.Fc2Pwr.getOrCreate(asof) 				<-- _} map powToRmds
fcr80Pi09      map {_ from asof} map {pow.Fcr80Pi09.getOrCreate(asof) 		<-- _} map powToRmds
ulsdFcr80Pi09  map {_ from asof} map {UlsdFcr80Pi09.getOrCreate(asof) 		<-- _} map powToRmds
fc3Pwr         map {_ from asof} map {pow.Fc3Pwr.getOrCreate(asof) 				<-- _} map powToRmds
bbGas          map {_ from asof} map {pow.Bbgas.getOrCreate(asof) 				<-- _} map powToRmds
remixG01Pi03   map {_ from asof} map {pow.RemixG01Pi03.getOrCreate(asof) 	<-- _} map powToRmds
ulsdRemixG01Pi03 map {_ from asof} map {UlsdRemixG01Pi03.getOrCreate(asof) 	<-- _} map powToRmds
fc20111        map {_ from asof} map {pow.Fc20111.getOrCreate(asof) 			<-- _} map powToRmds
fc20111Gas     map {_ from asof} map {pow.Fc20111Gas.getOrCreate(asof) 		<-- _} map powToRmds
fc20111Coal    map {_ from asof} map {pow.Fc20111Coal.getOrCreate(asof) 	<-- _} map powToRmds
itGrp2G01Pwr   map {_ from asof} map {pow.ItGrp2G01Pwr.getOrCreate(asof) 	<-- _} map powToRmds
bgf901Pwr      map {_ from asof} map {pow.Bgf901Pwr.getOrCreate(asof) 		<-- _} map powToRmds
br901Pwr       map {_ from asof} map {pow.Br901Pwr.getOrCreate(asof) 			<-- _} map powToRmds
gofo12mPwr     map {_ from asof} map {pow.Gofo12mPwr.getOrCreate(asof) 		<-- _} map powToRmds
ulsdGofo12mPwr map {_ from asof} map {UlsdGofo12mPwr.getOrCreate(asof) 		<-- _} map powToRmds
consipPwr      map {_ from asof} map {pow.ConsipPwr.getOrCreate(asof) 		<-- _} map powToRmds
cip6Pun        map {_ from asof} map {pow.Cip6Pun.getOrCreate(asof) 			<-- _} map powToRmds
fcCoalApi2     map {_ from asof} map {pow.FcCoalApi2.getOrCreate(asof) 		<-- _} map powToRmds
rsgbPi19	   	 map {_ from asof} map {pow.RsgbPi19.getOrCreate(asof) 			<-- _} map powToRmds
ulsdRsgbPi19	 map {_ from asof} map {UlsdRsgbPi19.getOrCreate(asof) 			<-- _} map powToRmds
br301Pwr	   	 map {_ from asof} map {pow.Br301Pwr.getOrCreate(asof) 			<-- _} map powToRmds
itEdisonAt1011 map {_ * 1.95 from asof} map {pow.ItEdisonAt1011.getOrCreate(asof) 		<-- _} map powToRmds
itEdisonAt1213 map {_ * 1.95 from asof} map {pow.ItEdisonAt1213.getOrCreate(asof) 		<-- _} map powToRmds
gas01Pwr			 map {_ from asof} map {pow.ItaGas01.getOrCreate(asof) 	<-- _} map powToRmds
gas02Pwr			 map (_ from asof) map powItaGas02.getOrCreate(asof).<-- map powToRmds
itaPsv 				 map {_ from asof} map {pow.Psv.getOrCreate(asof) <-- _} //map powToRmds
gmeCv 				 map GmeCvMid.getOrCreate(asof).<--