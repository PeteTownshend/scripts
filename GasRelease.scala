val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim

val asof = yesterday // running at 01:45 for the previous day

val horizon = - 12 months

implicit val wb = FO_SUPPORT

def hist(symbol: String, field: String*) = Interpolate.omit( 
	field.toList match {

		case field :: Nil =>
			srv.apply(asof iterator horizon, symbol, field)

		case field1 :: field2 :: Nil =>
			srv(asof iterator horizon, symbol, (field1,field2))()
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
			val days = h.toDays filter {!_.isWeekend}
			val avg = (mhist.last._2 * days.filter(_ <= lastDay).size + mfwd.head._2 * days.filter(_ > lastDay).size) / days.size
			(mhist ++ mfwd).updated((h,avg))

		case _ =>
			mhist ++ mfwd
	}
}

lazy val usdeur = srv.Reuters1730.EurUsdMid								get(asof) flatMap { _.series } map { concat(_, hist("ECBUSD", "Spot")) } map { 1.0 / _ }
lazy val go02CifMed = srv.GoEndurCurves.Go01CifMedCargoes	get(asof) flatMap { _.series } map { concat(_, hist("AAVJJ00", "High", "Low")) - 7.36}
lazy val fo1CifMed = srv.GoEndurCurves.Fo1CifMedCargoes		get(asof) flatMap { _.series } map { concat(_, hist("PUAAJ00", "High", "Low"))}
lazy val dtdBrent = srv.GoEndurCurves.DatedBrentSwap			get(asof) flatMap { _.series } map { concat(_, hist("PCAAS00", "High", "Low"))}

lazy val gasReleaseM3 = for {
	fx  <- usdeur
	go  <- go02CifMed map projective.mean(9.months,month)
	fo  <- fo1CifMed map projective.mean(9.months,month)
	oil <- dtdBrent	map projective.mean(9.months,month)
	regular = 0.0381 * 315.092 * 0.95 * (0.41 * go / 219.14 + 0.46 * fo / 141.07 + 0.13 * 7.4 * oil / 182.50) * fx  + 3.2263154 from asof
	s = regular.end + 1.months
	e = regular.end + math.max(1,75 - regular.length)
	v = regular.lastValue
} yield regular ++ Series(s to e map { _ -> v })

lazy val gasReleaseMwh = gasReleaseM3 map {_ * 0.94487}

gasReleaseM3 map { srv.GoEndurCurves.Gasrelease07M3.getOrCreate(asof) 	<-- _ }
gasReleaseMwh map { srv.GoEndurCurves.Gasrelease07Mwh.getOrCreate(asof) 	<-- _ }
gasReleaseM3 map { srv.OilEodFc.Gasrelease07M3.getOrCreate(asof) 	<-- _ }
gasReleaseMwh map { srv.OilEodFc.Gasrelease07Mwh.getOrCreate(asof) 	<-- _ }