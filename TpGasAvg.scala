import org.joda.time.DateTime

val svIn = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim
val svOut = svIn
implicit val wb = FO_SUPPORT

val asof = current[Month] - 1 // scheduled for the first day of a month to avg the month befor
val backup = "//u-dom1.u-ssi.net/DFSRoot34000/TEAM/M/MP/GRP/23-TP_LTC_Transport_Curves/" + asof.toString("yyyy_MMM") + "/"
val termToKWh = 29.3071
val log = Logger("scriptLogger")

def zipList(l: Series[Month, List[Double]], r: Series[Month, List[Double]]): Series[Month, List[Double]] =
	Series(
		(l.time | r.time).toList map { t =>
			t -> (
				(l.get(t), r.get(t)) match {

					case (Some(l), Some(r)) =>
						l.union(r)

					case (Some(l), _) =>
						l

					case (_, Some(r)) =>
						r

					case _ =>
						Nil
				}
				)
		}
	)

def gasAvg[T <: DateTimeLike[T]](
																	curve: Day => Option[Series[T, Double]],
																	tradeDates: List[Day]
																)(implicit c: DateTime => T): Option[Series[Month, Double]] = {

	val series: List[Series[Month, Double]] = for {
		dt <- tradeDates;
		s = curve(dt)
		if s.isDefined
	} yield {
		s.get.start match {

			case d: Day =>
				s.get.rollBy(_._1: Month)(_.mean)

			case m: Month =>
				s.get.asInstanceOf[Series[Month, Double]]
		}
	}
	if (series.isEmpty)
		None
	else {
		val s: Series[Month, List[Double]] = series.map { s => s.mapValues(List(_)) }.reduceLeft(zipList)
		Some(
			s mapValues { l => l.sum / l.length }
		)
	}
}

lazy val eurgbp   		= svIn.Reuters1730.EurGbpMid
lazy val ttfFlat 			= svIn.NgTtfFc.MidFlat
lazy val ttfMid 			= svIn.NgTtfFc.Mid
lazy val zeeFlat 			= svIn.NgZeeFc.MidFlat
lazy val zeeMid 			= svIn.NgZeeFc.Mid
lazy val zeeNbpSpread = svIn.NgZeeFc.ZeeNbpSpreadQtr
lazy val nbpFlat 			= svIn.NgNbpFc.MidFlat
lazy val nbpMid 			= svIn.NgNbpFc.Mid


type HasName = ({def name:String})
type Names = List[HasName]
implicit def toNames(t:(HasName,HasName,HasName)):String=List(t._1,t._2,t._3) map {_.name} mkString(".")

implicit class RichDays(dts: List[Day]) {
	def avg[T <: DateTimeLike[T]](curve:Day => Option[Series[T,Double]])(implicit c: DateTime => T) =
		gasAvg(curve, dts) map { _.from(asof) }
}

def copy[T <: Service#Container#Bucket](bucket: T) = {
	bucket --> bucket.container.getOrCreate(bucket.name.replace(asof.toString("yyyy_MMM"), asof.endOfMonth.toString("yyyyMMdd")))
	bucket
}

def toFile[T <: Service#Container#Bucket](bucket: T) = {
	bucket --> (backup + "Results\\", (bucket.container.service,bucket.container,bucket))
	val name = bucket.toString + " done "
	val atAll = 80
	val nbr = (atAll - name.length) / 2
	log info ("=" * nbr) + " " + name + ("=" * (atAll - nbr))
	bucket
}

def archive[T <: DateTimeLike[T]](
																	 bucket: ForwardCurveService#ForwardCurveContainer#ForwardCurveStream[Day, T])(
																	 day: Day)(
																	 implicit c: DateTime => T): Option[Series[T, Double]] = bucket.get(day) flatMap { bucket =>
	bucket.series match {
		case Some(ts) => {
			val coreName = bucket.name.replace(bucket.tradeDate.toString("_yyyyMMdd"),"")
			bucket --> (backup + "MarketData\\" + bucket.container.name + "\\" + coreName + "\\", (bucket.container.service,bucket.container,bucket))
			Some(ts)
		}
		case None => None
	}
}

val dts =  asof.toDays filterNot(_.isWeekend) toList

def fxConverted[T <: DateTimeLike[T]](
																			 underlying: ForwardCurveService#ForwardCurveContainer#ForwardCurveStream[Day, T],
																			 fx: ForwardCurveService#ForwardCurveContainer#ForwardCurveStream[Day, T])(
																			 implicit c: DateTime => T) =
	dts avg {
		day => for {
			fwd <- archive(underlying)(day)
			fx 	<- archive(fx)(day)
		} yield fwd / (fx * termToKWh) * 10d
	}

class Market(name: String, timeZoneName: String) extends MarketLike(name, timeZoneName) {
	lazy val specialDays =
		new svIn.Calendar.SeriesBucketLike[Day, String] {
			//eet.config specifiey columnSet as CDate("DATE") and CString("VALUE")
			val columnSet: List[Column[_]] = List(CString("DATE").time, CString("TYPE").value)
			val bucket = new SeriesBucket("SPECIALDAYS_" + name)
			val bankHolidays = bucket.series match {
				case Some(series) => series.time
				case None => scala.collection.SortedSet.empty[Day]
			}
		}.bankHolidays
}

case object UK extends Market("UK", "Europe/London")

//TP
dts avg {archive(svIn.ContractPrice.NgTrollshellGy1112)} map {svOut.ContractPriceAvg.NgTrollshellGy1112.getOrCreate(asof) <-- _} map toFile map copy
dts avg {archive(svIn.ContractPrice.NgTrollshellGy1213)} map {svOut.ContractPriceAvg.NgTrollshellGy1213.getOrCreate(asof) <-- _} map toFile map copy
dts avg {archive(svIn.ContractPrice.NgTrollshellGy1314)} map {svOut.ContractPriceAvg.NgTrollshellGy1314.getOrCreate(asof) <-- _} map toFile map copy
dts avg {archive(svIn.ContractPrice.NgTrollshellGy1415)} map {svOut.ContractPriceAvg.NgTrollshellGy1415.getOrCreate(asof) <-- _} map toFile map copy

dts avg {archive(svIn.ContractPrice.NgTrollstatoilGy1112)} map {svOut.ContractPriceAvg.NgTrollstatoilGy1112.getOrCreate(asof) <-- _} map toFile map copy
dts avg {archive(svIn.ContractPrice.NgTrollstatoilGy1213)} map {svOut.ContractPriceAvg.NgTrollstatoilGy1213.getOrCreate(asof) <-- _} map toFile map copy
dts avg {archive(svIn.ContractPrice.NgTrollstatoilGy1314)} map {svOut.ContractPriceAvg.NgTrollstatoilGy1314.getOrCreate(asof) <-- _} map toFile map copy
dts avg {archive(svIn.ContractPrice.NgTrollstatoilGy1415)} map {svOut.ContractPriceAvg.NgTrollstatoilGy1415.getOrCreate(asof) <-- _} map toFile map copy

//FX
dts avg {archive(eurgbp)} map {svOut.Reuters1730Avg.EurGbpMid.getOrCreate(asof)	<-- _} map toFile map copy

//Ttf
dts avg {archive(svIn.NgTtfFc.TrayportAsk)} map {svOut.NgTtfFcAvg.TrayportAsk.getOrCreate(asof) <-- _} map toFile map copy
dts avg {archive(svIn.NgTtfFc.TrayportBid)} map {svOut.NgTtfFcAvg.TrayportBid.getOrCreate(asof) <-- _} map toFile map copy
dts avg {archive(svIn.NgTtfFc.TrayportBidAsk)} map {svOut.NgTtfFcAvg.TrayportBidAsk.getOrCreate(asof) <-- _} map toFile map copy
dts avg {archive(ttfFlat)} map {svOut.NgTtfFcAvg.MidFlat.getOrCreate(asof) <-- _} map toFile map copy

//Nbp
fxConverted(svIn.NgNbpFc.TrayportAsk, eurgbp) map {svOut.NgNbpFcAvg.TrayportAsk.getOrCreate(asof) <-- _} map toFile map copy
fxConverted(svIn.NgNbpFc.TrayportBid, eurgbp) map {svOut.NgNbpFcAvg.TrayportBid.getOrCreate(asof) <-- _} map toFile map copy
fxConverted(svIn.NgNbpFc.TrayportBidAsk, eurgbp) map {svOut.NgNbpFcAvg.TrayportBidAsk.getOrCreate(asof) <-- _} map toFile map copy
fxConverted(nbpFlat, eurgbp) map {svOut.NgNbpFcAvg.MidFlat.getOrCreate(asof) <-- _} map toFile map copy

//Zee
fxConverted(svIn.NgZeeFc.TrayportAsk, eurgbp) map {svOut.NgZeeFcAvg.TrayportAsk.getOrCreate(asof) <-- _} map toFile map copy
fxConverted(svIn.NgZeeFc.TrayportBid, eurgbp) map {svOut.NgZeeFcAvg.TrayportBid.getOrCreate(asof) <-- _} map toFile map copy
fxConverted(svIn.NgZeeFc.TrayportBidAsk, eurgbp) map {svOut.NgZeeFcAvg.TrayportBidAsk.getOrCreate(asof) <-- _} map toFile map copy
fxConverted(zeeFlat, eurgbp) map {svOut.NgZeeFcAvg.MidFlat.getOrCreate(asof) <-- _} map toFile map copy

//copy last trading to end of month
asof.endOfMonth.find(-10.days){case day if(!UK.specialDays.contains(day) && !day.isWeekend) => day} map {
	lastDay => {
		def copy[T <: DateTimeLike[T]](
																		from: ForwardCurveService#ForwardCurveContainer#ForwardCurveStream[Day,T],
																		to: ForwardCurveService#ForwardCurveContainer#ForwardCurveStream[Day,T]
																	) = from.get(lastDay) map { _ -->to.getOrCreate(asof.endOfMonth) }

		copy(svIn.Volatility.TtfMonthly, svOut.Volatility.TtfMonthly)
		copy(svIn.Volatility.NbpMonthly, svOut.Volatility.NbpMonthly)
		copy(svIn.Volatility.NcgMonthly, svOut.Volatility.NcgMonthly)
		copy(svIn.Volatility.ZeeMonthly, svOut.Volatility.ZeeMonthly)
		copy(svIn.Correlation.NbpZeeMonthly, svOut.Correlation.NbpZeeMonthly)
	}
}
