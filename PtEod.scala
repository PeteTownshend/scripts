val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim

implicit val wb = FO_SUPPORT
val asof = yesterday // running at 04:15

/**
The Oil curves start in the current month when BOM is available. When BOM is unavailable, like today, the curve starts Month Ahead. 
The Coal curves start in M-3, so we will have to be careful about this. If we simply overwrite the eod curve with the intraday, we will replace LIM prices for the last three months with the croco prices. 
*/

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

def start(param: (Int, Int, Int, Int)) = projective(asof: Month)(param._1, param._2, param._3, param._4)

def iter(param: (Int, Int, Int, Int)) = (asof:Month) iterator start(param)

lazy val eurUsd = srv.Reuters1730.EurUsdMid.get(asof) flatMap { _.series }
lazy val eurUsdHist = Interpolate.omit(
	srv(asof iterator -24.months, "ECBUSD", "Spot")
)

/**
 * oil_pft_intraday_fc.hel_rhein_hl_mid_%s
 * STATS.BUNDESAMT.LIGHT.FUELOIL.CONSUMER.RHEINSCHIENE, MthAvg
 * OIL_EOD_FC.HEL_RHEIN
 * HSL Deutschland	€/mt	400	0,0085	1,1,3,0; 1,1,9,0; 1,3,3,1; 1,3,3,3; 1,3,6,1; 1,3,6,3
 * HEL_RHEIN = GO_50_PPM * fx + tax [EUR/mt]
 * HEL_RHEIN_HL = HEL_RHEIN * 0.0845 [EUR/hl]
 * 0.0845 mt = 1 hl
 */
lazy val hel = for { // [EUR/hl]
	helRhein <- srv.OilEodFc.HelRhein.get(asof) flatMap { _.series } // [EUR/mt]
	go50PpmEur <- srv.OilEodFc.Go50PpmFobAraBargesEur.get(asof) flatMap { _.series } // [EUR/mt]
	fwd <- srv.OilEodFc.HelRheinHl.get(asof) flatMap { _.series } // [EUR/hl]
	tax = change(helRhein - go50PpmEur) to Day
	params = List((1,1,3,0), (1,1,9,0), (1,3,3,1), (1,3,3,3), (1,3,6,1), (1,3,6,3))
	start = params map iter map { _.toList.min }
	go50PpmHist = Interpolate.omit(
		srv(asof iterator -36.months, "AAUQC00", ("High", "Low"))()
	)
	hist = change(Interpolate.omit(
		srv(asof iterator -36.months, "STATS.BUNDESAMT.LIGHT.FUELOIL.CONSUMER.RHEINSCHIENE", "MthAvg")
	)) to Month
	//gap only beyond hist
	gap = change(go50PpmHist / eurUsdHist + tax.firstValue) to Month from (hist.end + 1.months)
} yield gap * 0.0845 ++ hist ++ fwd


/**
 * oil_pft_intraday_fc.hsl_deu_mid_%s	
 * STATS.BUNDESAMT.HEAVY.FUELOIL.SUPPLY.DEUTSCHLAND, MthAvg
 * OIL_EOD_FC.HSL_DEU
 * HSL Deutschland	€/mt	400	0,0085	1,1,3,0; 1,1,9,0; 1,3,3,1; 1,3,3,3; 1,3,6,1; 1,3,6,3
 * HSL_DEU = FO_1_FOB * fx + tax [EUR/mt]
 */
lazy val hsl = for { // [EUR/t]
	fwd <- srv.OilEodFc.HslDeu.get(asof) flatMap { _.series }
	fo1FobEur <- srv.OilEodFc.Fo1FobAraBargesEur.get(asof) flatMap { _.series }
	tax = change(fwd - fo1FobEur) to Day
	params = List((1,1,3,0), (1,1,9,0), (1,3,3,1), (1,3,3,3), (1,3,6,1), (1,3,6,3))
	start = params map iter map { _.toList.min }
	fo1FobHist = Interpolate.omit(
		srv(asof iterator -48.months, "PUAAP00", ("High", "Low"))()
	)
	hist = change(Interpolate.omit(
		srv(asof iterator -48.months, "STATS.BUNDESAMT.HEAVY.FUELOIL.SUPPLY.DEUTSCHLAND", "MthAvg")
	)) to Month
	//gap only beyond hist
	gap = change(fo1FobHist / eurUsdHist + tax.firstValue) to Month from (hist.end + 1.months)
} yield gap ++ hist ++ fwd


/**
 * oil_pft_intraday_fc.fo_1_fob_rot_barges_eur_mid_%s 
 * PUAAP00, MidPoint ( High / Low )
 * OIL_EOD_FC.FO_1_FOB_ARA_BARGES_EUR
 * Fuel Oil 1% S, Barges FOB Rotterdam	€/mt	350	0,0085	1,1,1,0; 1,3,3,0; 1,3,3,1; 1,3,3,3; 1,3,6,1
 */
lazy val fo1 = for { // [EUR/t]
	fwd <- srv.OilEodFc.Fo1FobAraBarges.get(asof) flatMap { _.series }
	fx <- eurUsd
	params = List((1,1,1,0), (1,3,3,0), (1,3,3,1), (1,3,3,3), (1,3,6,1))
	start = params map iter map { _.toList.min }
	hist = Interpolate.omit(
		srv(asof iterator start.min, "PUAAP00", ("High","Low"))()
	)
} yield concat(fwd, hist) / change(fx ++ eurUsdHist).to(Month)


/**
 * oil_pft_intraday_fc.go_50_ppm_fob_rot_barges_eur_mid_%s
 * AAUQC00, MidPoint ( High / Low )
 * OIL_EOD_FC.GO_50_PPM_FOB_ARA_BARGES_EUR
 * Gasoil 50 ppm	€/mt	450	0,0077	1,1,1,0; 1,3,3,0; 1,3,3,1; 1,3,3,3; 1,3,6,1
 */
lazy val go50 = for { // [EUR/t]
	fwd <- srv.OilEodFc.Go50PpmFobAraBarges.get(asof) flatMap { _.series }
	fx <- eurUsd
	params = List((1,1,1,0), (1,3,3,0), (1,3,3,1), (1,3,3,3), (1,3,6,1))
	start = params map iter map { _.toList.min }
	hist = Interpolate.omit(
		srv(asof iterator start.min, "AAUQC00", ("High", "Low"))()
	)
} yield concat(fwd, hist) / change(fx ++ eurUsdHist).to(Month)


/**
 * oil_pft_intraday_fc.go_0_1_fob_rot_barges_eur_mid_%s
 * AAYWT00, MidPoint ( High / Low )
 * OIL_EOD_FC.GO_0_1_FOB_ARA_BARGES_EUR
 * Gasoil 0,1% S, Barges FOB Rotterdam	€/mt	450	0,0077	1,1,1,0; 1,3,3,0; 1,3,3,1; 1,3,3,3; 1,3,6,1
 */
lazy val go01 = for { // [EUR/t]
	fwd <- srv.OilEodFc.Go01FobAraBarges.get(asof) flatMap { _.series }
	fx <- eurUsd
	params = List((1,1,1,0), (1,3,3,0), (1,3,3,1), (1,3,3,3), (1,3,6,1), (1,3,6,9))
	start = params map iter map { _.toList.min }
	hist = Interpolate.omit(
		srv(asof iterator start.min, "AAYWT00", ("High", "Low"))()
	)
} yield concat(fwd, hist) / change(fx ++ eurUsdHist).to(Month)

	
/**
 * coal_pft_intraday_fc.coal_api2_eur_mid_%s
 * PA0003254.0.0, AveragePrice
 * COAL_EOD_FC.COAL_API2_MONTHLY_AVG_EUR
 * API 2	€/mt	100	0,016	1,1,1,0; 1,3,3,0
 */
lazy val coal = for { // [EUR/t]
	fwd <- srv.CoalApi2Fc.Mid.get(asof) flatMap { _.series }
	fx <- eurUsd
	params = List((1,1,1,0), (1,3,3,0))
	start = params map iter map { _.toList.min }
	hist = Interpolate.omit(
		srv(asof iterator start.min, "PA0003254.0.0", "AveragePrice")
	)
} yield change(hist ++ fwd).to(Month) / change(fx ++ eurUsdHist).to(Month)


/**
 * coal_pft_intraday_fc.new_bafa_mid_%s
 * BAFA, Val
 * COAL_EOD_FC.COAL_BAFA_EUR
 * Coal Bafa (Drittlandskohle)	€/mt	110	0,016	1,3,3,3; 1,6,6,0
 */
lazy val bafa = for { // [EUR/t]
	fwd <- srv.CoalEodFc.CoalBafaHistEur.get(asof) flatMap { _.series }
	params = List((1,3,3,3), (1,6,6,0))
	start = params map iter map { _.toList.min }
	hist = Interpolate.omit(
		srv(asof iterator start.min, "BAFA", "Val")
	)
} yield fwd ++ (change(hist) to Month)

/**
 * oil_pft_intraday_fc.ulsd_10_ppm_fob_ara_barges_eur_mid%s
 * AAJUS00, MidPoint ( High / Low )
 * OIL_EOD_FC.ULSD_10_PPM_FOB_ARA_BARGES_EUR_MID
 * Gasoil 0,1% S, Barges FOB Rotterdam	€/mt	450	0,0077	1/1/1/0; 1/3/3/0; 1/3/3/1; 1/3/6/1; 1/3/3/3
 */

lazy val ulsd = for { // [EUR/t]
	fwd <- srv.OilEodFc.Ulsd10PpmFobAraBarges.get(asof) flatMap { _.series }
	fx <- eurUsd
	params = List((1,1,1,0), (1,3,3,0), (1,3,3,1), (1,3,3,3), (1,3,6,1))
	start = params map iter map { _.toList.min }
	hist = Interpolate.omit(
		srv(asof iterator start.min, "AAJUS00", ("High", "Low"))()
	)
} yield concat(fwd, hist) / change(fx ++ eurUsdHist).to(Month)

val cOil = srv.OilPtFc
object cOil_ extends srv.Container("OIL_PT_FC") with srv.ForwardCurveContainer {
	object Ulsd10PpmFobAraBargesEurMid extends ForwardCurveStream[Day, Month]("ULSD_10_PPM_FOB_ARA_BARGES_EUR_MID_%s")
}
val cCoal = srv.CoalPtFc
hel map { cOil.HelRheinHlMid.getOrCreate(asof) <-- _ }
hsl map { cOil.HslDeuMid.getOrCreate(asof) <-- _ }
fo1 map { cOil.Fo1FobRotBargesEurMid.getOrCreate(asof) <-- _ }
go50 map { cOil.Go50PpmFobRotBargesEurMid.getOrCreate(asof) <-- _ }
go01 map { cOil.Go01FobRotBargesEurMid.getOrCreate(asof) <-- _ }
ulsd map { cOil_.Ulsd10PpmFobAraBargesEurMid.getOrCreate(asof) <-- _ }
coal map { cCoal.CoalApi2EurMid.getOrCreate(asof) <-- _ }
bafa map { cCoal.NewBafaMid.getOrCreate(asof) <-- _ }