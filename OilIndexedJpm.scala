val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim

val asof = yesterday //running at 00:23 on following day
val lookUp = - 5.days
val horizon = - 13 months
implicit val wb = FO_SUPPORT

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
			val avg = (mhist.lastValue * days.filter(_ <= lastDay).size + mfwd.firstValue * days.filter(_ > lastDay).size) / days.size
			(mhist ++ mfwd).updated((h,avg))
		case _ => mhist ++ mfwd
	}
}

lazy val eurUsd = srv.Reuters1730.EurUsdMid.get(asof) flatMap {_.series} map {concat(_, hist("ECBUSD", "Spot"))}
lazy val go01FobAra = srv.GoEndurCurves.Go01FobAraBarges.get(asof) flatMap {_.series} map {concat(_, hist("AAYWT00", "High", "Low"))}
lazy val fo1FobAra  = srv.GoEndurCurves.Fo1FobAraBarges.get(asof) flatMap {_.series} map {concat(_, hist("PUAAP00", "High", "Low"))}

lazy val oilIndexedJpm = for {
	ue <- eurUsd map { eu	=> 1.0 / eu }
	goLag <- go01FobAra map { g => projective.mean(8.months,month)(g * ue) } map { _.lag(-1) }
	go = goLag from asof map { case p if(p._1.month % 2 == 0) => p._1 -> goLag(p._1 - 1); case p => p }
	fo <-	fo1FobAra map { f	=> projective.mean(4.months,month)(f * ue) }
} yield {
	(22.0 + 0.0330 * (go - 478.811) + 0.0175 * (fo - 214.082)) roundTo 3 from asof
}

oilIndexedJpm map { srv.NgNcgFc.OilIndexedJpm.getOrCreate(asof) <-- _}