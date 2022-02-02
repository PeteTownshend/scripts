val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim

implicit val wb = FO_SUPPORT

val asof = yesterday// running at 04:00 for the previous day
val endOfPrev = ((asof: Month) - 1).endOfMonth
val endOfCurrent = (asof: Month).endOfMonth
val horizon = ((asof - 18.months):Quarter):Day

def hist(symbol: String, field: String*) = Interpolate.omit( 
	field.toList match {
		case field::Nil => srv(asof iterator horizon, symbol, field)
		case field1::field2::Nil => srv(asof iterator horizon, symbol, (field1,field2))()
	}
)

def concat[T<:DateTimeLike[T]](
	fwd: Series[T,Double],
	hist: Series[Day, Double])(
	implicit dtToT: org.joda.time.DateTime => T
	): Series[Month,Double] = {
	
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

lazy val eurUsd = hist("EURUSD", "Close") to endOfPrev
lazy val api2 = hist("PA0002140.0.0", "Index") to endOfPrev
lazy val carbonEua = srv.CarbonEuaFc.EcxMid get(asof) flatMap { _.series } map { _ filterTime(!_.isWeekend)} map { hist("ECX.CFI.DAILY", "Close") ++ _ }
lazy val carbonFwd = srv.CarbonEuaFc.EcxMid get(asof) flatMap { _.series } map { _ filterTime(!_.isWeekend)} map { hist("ECX.CFI_2018Z", "Close") ++ _ }
lazy val bafaFwd = srv.CoalEodFc.CoalBafaHistEur get(asof) flatMap { _.series }
lazy val bafaHist = hist("BAFA", "Val")

lazy val api2Fixing = {
	val fx = change(eurUsd) to Quarter
	val coal = change(api2) to Quarter
	(change(coal / fx) to Month) to endOfPrev
}

lazy val bafaFixing = for {
	fwd <- bafaFwd map { _ to endOfPrev} map { change(_) to Quarter }
	hist = change(bafaHist) to Quarter
	bafa = hist ++ fwd.from(hist.end + 1) 
} yield change(bafa) to Month to endOfPrev

lazy val carbonFixingM = carbonEua map { _ to endOfCurrent } map { change(_) to Month}

lazy val carbonFixing = carbonEua map { _ to endOfCurrent } map { change(_) to Quarter } map { change(_) to Month to endOfCurrent}

lazy val carbonFwdFixing = for {
	carbon <- carbonFwd map { _ to endOfCurrent } map { change(_) to Quarter }
} yield change(carbon) to Month to endOfCurrent



srv.CoalApi2Fc.LtscFixing.getOrCreate(asof) <-- api2Fixing
carbonFixingM map { srv.CarbonEuaFc.LtscFixingM.getOrCreate(asof) <-- _ }
carbonFixing map { srv.CarbonEuaFc.LtscFixing.getOrCreate(asof) <-- _ }
carbonFwdFixing map { srv.CarbonEuaFc.LtscFixingFwd.getOrCreate(asof) <-- _ }
bafaFixing map { srv.CoalBafaFc.LtscFixing.getOrCreate(asof) <-- _ }