val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim

val asof = yesterday //running at 4.30 for previous day
val horizon = - 3 months
implicit val wb = FO_SUPPORT

lazy val nbp = srv.NgNbpFc.Mid.get(asof) map {_.getSeries}
lazy val holidays = srv.Markets.UK.specialDays

lazy val ipe = for {
	fwd <- nbp map {change(_) to Month}
} yield {
	val symbol = "IPE.NBP"
	//does already handle rollover dates for IPE.NBP
	srv(asof iterator horizon, symbol, "Close")
	val hist = Interpolate.omit(srv(asof iterator horizon, symbol, "Close"))	
	//rolled by month at rollover dates
	val rolled = {
		val rolloverDates = srv.rolloverDates(asof iterator horizon, symbol).toList
		def rollator(e:(Day,Double)):Month = {
			rolloverDates.partition(_ < e._1)._1 match {
				case days if days.isEmpty => rolloverDates.head:Month
				case days => (days.last:Month) + 1
			}
		}
		hist.rollBy(rollator)(s => (s.mean,s.size))
	}
	
	val monthAhead = {
		val workingDays = rolled.end.toDays.filter(dt => !dt.isWeekend && !holidays.contains(dt))
		def m(mean:Double, count:Int, quote:String) = srv(asof iterator asof, "ESGM.NBP", quote).head._2 match {
			case Some(ahead) => (mean * count + ahead * (workingDays.size - count)) / workingDays.size.toDouble
			case None =>
				log error "no lim quote for " + symbol
				mean
		}
		rolled.last._2 match {
			case (mean,count) if(count == 1) => m(mean, count, "FwdMid02mo")
			case (mean,count) if(count == workingDays.size) => mean
			case (mean,count) => m(mean, count, "FwdMid01mo")
		}
	}
	
	//replace ipe settlement price by average from heren month ahead and ipe settlement prices
	//shift the values one month further
	val settlement = rolled.mapValues(_._1).lag(-1) ++ Series(rolled.end + 1 -> monthAhead)
	change(fwd ++ settlement) to Day from asof
}

ipe map {s =>
	srv.NgNbpFc.Ipe.getOrCreate(asof) <-- s
	val t = DataTable.fromListOfString(List(CString("TimeStamp")))(List(now.toString("yyyy-MM-dd HH:mm:ss")::Nil))
	srv.NgNbpFc.getOrCreate("Ipe_%s_timestamp".format(asof.toString("yyyyMMdd"))).writeToCds(t)
}