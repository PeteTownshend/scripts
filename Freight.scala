implicit val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim2

val asof = yesterday // TUE-SAT 01-17-00

//fx conversion for port costs paid in eur

implicit val wb = FO_SUPPORT
//implicit val srv2 = service2("sm05780", "PRD")
implicit def limIter = LimIterator(asof iterator projective(2013.Jan)(1,3,3,0))

val projective_1_3_3_0 = projective[Month, Double](1, 3, 3, 0) _

// missing symbols
object ECBUSD extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object PA000214000 extends Symbol[String] { lazy val symbol = "PA0002140.0.0" }

// missing fields
val Index = Numerical("Index")
val Spot = Numerical("Spot")

//TODO, now in 0.17.2, add to regression testing
lazy val panamax_Ustluga_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_USTLUGA_MID_%s")
lazy val panamax_Vancouver_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_VANCOUVER_MID_%s")
lazy val panamax_Vysotsk_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_VYSOTSK_MID_%s")
lazy val panamax_Gogland_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_GOGLAND_MID_%s")
lazy val panamax_Yuzhny_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_YUZHNY_MID_%s")
lazy val panamax_Pecem_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_PECEM_MID_%s")
lazy val panamax_Fob_Big_Sandy_River_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_FOB_BIG_SANDY_RIVER_MID_%s")
lazy val panamax_Bolivar_Portotorres_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_BOLIVAR_PORTOTORRES_MID_%s")
lazy val supramax_Itaqui_Rdam_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("SUPRAMAX_ITAQUI_RDAM_MID_%s")
lazy val panamax_Itaqui_Rdam_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_ITAQUI_RDAM_MID_%s")
lazy val panamax_Lazaro_Cardenas_Rdam_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_LAZARO_CARDENAS_RDAM_MID_%s")
lazy val panamax_Gdansk_Rdam_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_GDANSK_RDAM_MID_%s")
lazy val panamax_Mejillones_Ustluga_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_MEJILLONES_USTLUGA_MID_%s")
lazy val panamax_Izmir_Mid = new srv.CoalApi2Fc.ForwardCurveStream[Day, Day]("PANAMAX_IZMIR_MID_%s")

// new 13-Mar-2018
lazy val panamax_Matsuura_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_MATSUURA_MID_%s")
lazy val panamax_Reihoku_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_REIHOKU_MID_%s")

// new 22-May-2019
lazy val panamax_Uswest_Japan_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_USWEST_JAPAN_MID_%s")
lazy val japan_Mid = new srv.CoalApi6Fc.ForwardCurveStream[Day, Day]("JAPAN_MID_%s")

// new 22-Oct-2014
lazy val supramax_Buenaventura_Kawa_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("SUPRAMAX_BUENAVENTURA_KAWA_MID_%s")
lazy val capesize_Port_Hedland_Kawa_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_PORT_HEDLAND_KAWA_MID_%s")
lazy val capesize_Rc5_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_RC5_MID_%s")
lazy val capesize_Rc5_Mid_Bom = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_RC5_MID_BOM_%s")
lazy val capesize_Rc7_Rc7_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_RC7_RC7_MID_%s")

// new 01-Jun-2016
lazy val capesize_Nacala_Rbct_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_NACALA_RBCT_MID_%s")

// new 02.Dec-2019
lazy val capesize_Bolivar_Rdam_Eco_Mid         = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_BOLIVAR_RDAM_ECO_MID_%s")
lazy val capesize_Bolivar_Rdam_Full_Mid        = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_BOLIVAR_RDAM_FULL_MID_%s")
lazy val panamax_Drummond_Itaqui_Full_Mid      = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_DRUMMOND_ITAQUI_FULL_MID_%s")
lazy val supramax_Drummond_Itaqui_Full_Mid     = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("SUPRAMAX_DRUMMOND_ITAQUI_FULL_MID_%s")
lazy val panamax_Drummond_Pecem_Full_Mid       = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_DRUMMOND_PECEM_FULL_MID_%s")
lazy val capesize_Drummond_Rdam_Eco_Mid        = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_DRUMMOND_RDAM_ECO_MID_%s")
lazy val capesize_Drummond_Rdam_Full_Mid       = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_DRUMMOND_RDAM_FULL_MID_%s")
lazy val panamax_Izmir_Rdam_Diff_Mid           = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_IZMIR_RDAM_DIFF_MID_%s")
lazy val panamax_Jorflasfar_Rdam_Diff_Mid      = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_JORFLASFAR_RDAM_DIFF_MID_%s")
lazy val panamax_Murmansk_Rdam_Full_Mid        = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_MURMANSK_RDAM_FULL_MID_%s")
lazy val capesize_Nacala_Rbct_Diff_Mid         = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_NACALA_RBCT_DIFF_MID_%s")
lazy val panamax_Newcastle_Matsuura_Full_Mid   = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_NEWCASTLE_MATSUURA_FULL_MID_%s")
lazy val panamax_Newcastle_Reihoku_Full_Mid    = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_NEWCASTLE_REIHOKU_FULL_MID_%s")
lazy val panamax_Riga_Rdam_Full_Mid            = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_RIGA_RDAM_FULL_MID_%s")
lazy val capesize_Usec_Rdam_Eco_Mid            = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_USEC_RDAM_ECO_MID_%s")
lazy val capesize_Usec_Rdam_Full_Mid           = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_USEC_RDAM_FULL_MID_%s")
lazy val panamax_Usec_Rdam_Full_Mid            = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_USEC_RDAM_FULL_MID_%s")
lazy val capesize_Usgc_Rdam_Eco_Mid            = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_USGC_RDAM_ECO_MID_%s")
lazy val capesize_Usgc_Rdam_Full_Mid           = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_USGC_RDAM_FULL_MID_%s")
lazy val panamax_Usgc_Rdam_Full_Mid            = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_USGC_RDAM_FULL_MID_%s")
lazy val panamax_Ustluga_Rdam_Full_Mid         = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_USTLUGA_RDAM_FULL_MID_%s")
lazy val panamax_Uswc_Japan_Newcastle_Diff_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_USWC_JAPAN_NEWCASTLE_DIFF_MID_%s")
lazy val panamax_Vysotsk_Rdam_Full_Mid         = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_VYSOTSK_RDAM_FULL_MID_%s")
lazy val panamax_Vysotsk_Rdam_Trans_Mid        = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_VYSOTSK_RDAM_TRANS_MID_%s")
lazy val capesize_Zonguldak_Rdam_Diff_Mid      = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("CAPESIZE_ZONGULDAK_RDAM_DIFF_MID_%s")

object FreightRc5Fc extends srv.Container("FREIGHT_RC5_FC") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )
}
object FreightRc5Fc2 extends srv.Container("FREIGHT_RC5_FC") with srv.ForwardCurveContainer {
  object BalticMidBom extends ForwardCurveStream[Day, Day]("BALTIC_MID_BOM_%s" )
}
object CapeTc5 extends srv.Container("FREIGHT_CAPETC5_FC") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )
}
object Reu1730 extends srv.Container("REUTERS_1730") with srv.ForwardCurveContainer {
  object EurUsdMid extends ForwardCurveStream[Day, Day]("EUR_USD_MID_%s" )
}
object Reu1731 extends srv.Container("REUTERS_1730") with srv.ForwardCurveContainer {
  object RubUsdMid extends ForwardCurveStream[Day, Day]("RUB_USD_MID_%s" )
}
object Reu1732 extends srv.Container("REUTERS_1730") with srv.ForwardCurveContainer {
  object EurUsdMidMonthly extends ForwardCurveStream[Day, Month]("EUR_USD_MID_MONTHLY_AVG_%s" )
}
object Reu1733 extends srv.Container("REUTERS_1730") with srv.ForwardCurveContainer {
  object RubUsdMidMonthly extends ForwardCurveStream[Day, Month]("RUB_USD_MID_MONTHLY_AVG_%s" )
}
object OilEodFc1 extends srv.Container("OIL_EOD_FC") with srv.ForwardCurveContainer {
  object ImoFobAra extends ForwardCurveStream[Day, Month]("MFO_0_5_FOB_ARA_BARGES_%s" )   // changed from "IMO_0_1_FOB_ARA_BARGES_%s"
}
object OilEodFc2 extends srv.Container("OIL_EOD_FC") with srv.ForwardCurveContainer {
  object MgoFobAra extends ForwardCurveStream[Day, Month]("MGO_0_1_FOB_ARA_BARGES_%s" )
}

object Settlement4 extends srv.Container("SETTLEMENT") with srv.ForwardCurveContainer {
  object Mf4 extends ForwardCurveStream[Day, Month]("ICE_MF4_%s" )
}

// addedd 12-May-2015
lazy val panamax_Izmir_Rdam_Mid = new srv.FreightSynthFc.ForwardCurveStream[Day, Day]("PANAMAX_IZMIR_RDAM_MID_%s")

//lazy val eurUsd = srv.Reuters1730.EurUsdMid fallBackSeries lookUp
lazy val eurUsd  = Reu1730.EurUsdMid.get(asof) flatMap {_.series}
lazy val eurUsdM  = Reu1732.EurUsdMidMonthly.get(asof) flatMap {_.series}
//lazy val rubUsd = srv.Reuters1730.RubUsdMid fallBackSeries lookUp
lazy val rubUsd  = Reu1731.RubUsdMid.get(asof) flatMap {_.series}
lazy val rubUsdM  = Reu1733.RubUsdMidMonthly.get(asof) flatMap {_.series}

lazy val panamaxTc = srv.FreightPanamaxtcFc.Baltic.get(asof) flatMap {_.series}
lazy val supramaxTc = srv.FreightSupramaxtcFc.BalticMid.get(asof) flatMap {_.series}
lazy val capeTc = srv.FreightCapetcFc.BalticMid.get(asof) flatMap {_.series}
lazy val capeTc5 =CapeTc5.BalticMid.get(asof) flatMap {_.series}
lazy val handyTc = srv.FreightHandytcFc.BalticMid.get(asof) flatMap {_.series}

//lazy val fo35FobAraBarges = srv.OilEodFc.Fo35FobAraBarges.get(asof) flatMap {_.series} map {change(_) to Day}
lazy val fo35FobAraBarges = srv.OilEodFc.Fo35FobAraBargesMonthlyAvgUsd.get(asof) flatMap {_.series} map {change(_) to Day}
lazy val api2 = srv.CoalApi2Fc.Mid.get(asof) flatMap {_.series}
lazy val api2Rmds = srv.CoalApi2Fc.MidRmds.get(asof) flatMap {_.series}
lazy val api4 = srv.CoalApi4Fc.Mid.get(asof) flatMap {_.series}
lazy val api6 = srv.CoalApi6Fc.Mid.get(asof) flatMap {_.series}
lazy val rc4  = srv.FreightRc4Fc.BalticMid.get(asof) flatMap {_.series}
lazy val rc5  = FreightRc5Fc.BalticMid.get(asof) flatMap {_.series}
lazy val rc5bom  = FreightRc5Fc2.BalticMidBom.get(asof) flatMap {_.series}
lazy val rc7  = srv.FreightRc7Fc.BalticMid.get(asof) flatMap {_.series}

lazy val imo  = OilEodFc1.ImoFobAra.get(asof) flatMap {_.series}
lazy val mgo  = OilEodFc2.MgoFobAra.get(asof) flatMap {_.series}
lazy val ims  = Settlement4.Mf4.get(asof) flatMap {_.series}

//For Solarc only, Morgan API2
lazy val api2Jpm = for { //added on Apr 5, 2013
  fx <- eurUsd map { change(_) to Month }
  fxHist = change(Interpolate.omit(Spot from ECBUSD)) to Month
  coal <- api2Rmds map { change(_) to Month }
  coalHist = change(Interpolate.omit(Index from PA000214000)) to Month
} yield projective_1_3_3_0(coalHist ++ coal) / projective_1_3_3_0(fxHist ++ fx)

//Synthetic Freight Curves

lazy val usgrdam = for {
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
  fx 	<- eurUsd
} yield {
  //(176667.0 + 42667.0 * fx + 1.05 * 39.0 * tc + 1000.0 * fo) / 70500.0
  //(230000.0 + 1.05 * 39.0 * tc + 1000.0 * fo) / 70500.0
  // updated 08-Dec-2015
  //(130000.0 + 66000.0 * fx + 1.05 * 39.0 * tc + 1000.0 * fo) / 70500.0
  // updated 26-Feb-2016
  (130000.0 + 66000.0 * fx + 1.0 * 39.0 * tc + 1000.0 * fo) / 73000.0
}

lazy val usecrdam = for {
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
  fx 	<- eurUsd
} yield {
  //(116667.0 + 42667.0 * fx + 1.05 * 31.0 * tc + 800.0 * fo) / 72000.0
  // (170000.0 + 1.05 * 31.0 * tc + 800.0 * fo) / 72000.0
  // updated 13-Nov-2014
  //(170000.0 + 1.08 * 31.0 * tc + 800.0 * fo) / 80000.0
  // changed on 02-Dec-2015
  //(70000.0 + 66000.0 * fx + 1.08 * 31.0 * tc + 800.0 * fo) / 80000.0
  // updated 26-Feb-2016
  (70000.0 + 66000.0 * fx + 1.0 * 31.0 * tc + 800.0 * fo) / 80000.0
}

lazy val rigaunrestr = for {
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
  fx 	<- eurUsd
} yield {
  //(76667.0 + 138667.0 * fx + 1.1 * 15.4 * tc + 250.0 * fo) / 75000.0
  //(250000.0 + 1.1 * 15.4 * tc + 250.0 * fo) / 75000.0
  // updated 13-Nov-2014
  //(250000.0 + 1.09 * 15.4 * tc + 250.0 * fo) / 78000.0
  //updated 19-Nov-2015
  //(159000.0 * fx + 1.09 * 15.4 * tc + 250.0 * fo) / 78000.0
  // updated 26-Feb-2016
  //(250000.0 + 159000.0 * fx + 1.0 * 16.0 * tc + 250.0 * ( fo + 140.0 )) / 80000.0
  // updated 09-Mar-2016
  (159000.0 * fx + 1.0 * 16.0 * tc + 250.0 * ( fo + 140.0 )) / 80000.0
}

lazy val rigarestricted = for {
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
  fx 	<- eurUsd
} yield {
  //(70000.0 + 128000.0 * fx + 1.1 * 15.25 * tc + 230.0 * fo) / 56500.0
  //(230000.0 + 1.1 * 15.25 * tc + 230.0 * fo) / 56500.0
  //updated 19-Nov-2015
  //(159000.0 * fx + 1.1 * 15.25 * tc + 230.0 * fo) / 56500.0
  // updated 26-Feb-2016
  //(230000.0 + 159000.0 * fx + 1.0 * 16.0 * tc + 230.0 * fo) / 56500.0
  // updated 09-Mar-2016
  (159000.0 * fx + 1.0 * 16.0 * tc + 230.0 * fo) / 56500.0
}

lazy val panamaxRichbayJorflasfar = for { //renamed from jorflasfar on Aug 13, 2012, updated Jul 10, 2014
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  //(125000.0 + 0.0 * 34.0 * tc + 865.0 * fo) / 64000.0
  // updated 13-Nov-2014
  //(125000.0 + 0.0 * 34.0 * tc + 865.0 * fo) / 64000.0
  // updated 26-Feb-2016
  //(125000.0 + 0.3 * 34.0 * tc + 865.0 * fo) / 66000.0
  // updated 3-Mar-2016
  (125000.0 + 0.295 * 34.0 * tc + 865.0 * fo) / 66000.0
}

lazy val maracaibo = for {
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  //(190000.0 + 1.1 * 35.0 * tc + 925.0 * fo) / 52000.0
  // updated 26-Feb-2016
  (190000.0 + 1.0 * 37.0 * tc + 925.0 * fo) / 52000.0
}

lazy val panamaxBolivarRdamMid = for {
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  (141250.0 + 1.1 * 32.3 * tc + 810.0 * fo) / 72000.0
}

lazy val panamaxRichbayRdamMid = for {
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  (139000.0 + 0.55 * 43.75 * tc + 1160.0 * fo) / 72000.0
}

lazy val panamaxBanjarRdamMid = for {
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  (110000.0 + 0.55 * 56.5 * tc + 1515.0 * fo) / 72000.0
}

lazy val capesizeRc5Mid = for { //added on 22-April-2015
  rc <- rc5
  tc  <- capeTc
  fo  <- fo35FobAraBarges
} yield {
  val m0M1M2 = Month(asof) + 3.months
  //val synthetic = (270000.0 + 1.00 *  30.28 * tc + 1255.0 * fo) / 165000.0
  // updated 26-Feb-2016
  val synthetic = (270000.0 + 1.00 *  30.28 * tc + 1255.0 * fo) / 170000.0
  synthetic ++ rc.takeWhile(_._1 < m0M1M2)
}
lazy val capesizeRc5MidBom = for { //added on 22-April-2015
  rc <- rc5bom
  tc  <- capeTc
  fo  <- fo35FobAraBarges
} yield {
  val m0M1M2 = Month(asof) + 3.months
  //val synthetic = (270000.0 + 1.00 *  30.28 * tc + 1255.0 * fo) / 165000.0
  // updated 26-Feb-2016
  val synthetic = (270000.0 + 1.00 *  30.28 * tc + 1255.0 * fo) / 170000.0
  synthetic ++ rc.takeWhile(_._1 < m0M1M2)
}

lazy val capesizeRc7 = for { //added on April 19, 2011
  tc  <- capeTc
  fo  <- fo35FobAraBarges
} yield {
  //(285000.0 + 1.05 * 39.0 * tc + 1500.0 * fo) / 160000.0
  // updated 26-Feb-2016
  (295000.0 + 1.0 * 37.0 * tc + 1500.0 * fo) / 165000.0
}

lazy val capesizeBolivarQdao = for { //added on Mai 25, 2011
  tc  <- capeTc
  fo  <- fo35FobAraBarges
} yield {
  (200000.0 + 1.95 * 70.0 * tc + 2000.0 * fo + 1350.0 * (fo + 30.0)) / 159000.0
}

lazy val capesizeNacalaRbctMid = for { //added on June 1, 2016

  tc  <- capeTc
  fo  <- fo35FobAraBarges
} yield {
  //(6 days x 4TC_Cape + 70 tonnes x HFO (Rotterdam) + 190,000 USD ) / 165,000 tonnes
  (190000.0 + 1.0 * 6.0 * tc + 70.0 * fo) / 165000.0
}

lazy val panamaxBolivarQdao = for { //added on Mai 25, 2011
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  (145000.0 + 1.55 * 67.0 * tc + 1250.0 * fo + 700.0 * (fo + 30.0)) / 72000.0
}

lazy val capesizeRichbayQdao = for { //added on Mai 25, 2011
  tc  <- capeTc
  fo  <- fo35FobAraBarges
} yield {
  //(160000.0 + 0.95 * 57.0 * tc + 2600.0 * (fo + 30.0)) / 167000.0
  // updated 26-Feb-2016
  (160000.0 + 1.0 * 54.0 * tc + 2600.0 * (fo + 30.0)) / 167000.0
}

lazy val panamaxRichbayQdao = for { //added on Mai 25, 2011
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  //(140000.0 + 0.9 * 54.0 * tc + 1550.0 * (fo + 30.0)) / 72000.0
  // updated 26-Feb-2016
  (140000.0 + 1.0 * 49.0 * tc + 1550.0 * (fo + 30.0)) / 72000.0
}

lazy val capesizeTanjungQdao = for { //added on Mai 25, 2011
  tc  <- capeTc
  fo  <- fo35FobAraBarges
} yield {
  //(125000.0 + 0.9 * 35.0 * tc + 1100.0 * (fo + 30.0)) / 162000.0
  // updated 26-Feb-2016
  (125000.0 + 1.0 * 31.0 * tc + 1100.0 * (fo + 30.0)) / 162000.0
}

lazy val panamaxTanjungQdao = for { //added on Mai 25, 2011
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  //(105000.0 + 0.95 * 29.0 * tc + 640.0 * (fo + 30.0)) / 72000.0
  // updated 26-Feb-2016
  (105000.0 + 1.0 * 28.0 * tc + 640.0 * (fo + 30.0)) / 72000.0
}

lazy val capesizeNewcastleQdao = for { //added on Mai 25, 2011
  tc  <- capeTc
  fo  <- fo35FobAraBarges
} yield {
  //(200000.0 + 0.9 * 40.0 * tc + 1750.0 * (fo + 50.0)) / 142000.0
  // updated 3-Mar-2016
  (200000.0 + 1.0 * 40.0 * tc + 1750.0 * (fo + 50.0)) / 142000.0
}

lazy val panamaxNewcastleQdao = for { //added on Mai 25, 2011
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  //(160000.0 + 0.95 * 37.0 * tc + 1000.0 * (fo + 50.0)) / 72000.0
  // updated 3-Mar-2016
  (160000.0 + 1.0 * 37.0 * tc + 1000.0 * (fo + 50.0)) / 72000.0
}

lazy val panamaxCarbosanRdamMid = for { // added on Oct 10, 2011, aka Santa Marta
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
  fx 	<- eurUsd
} yield {
  //(91667.0 + 42667.0 * fx + 1.05 * 37.0 * tc + 950.0 * fo) / 72000.0
  //(145000.0 + 1.05 * 37.0 * tc + 950.0 * fo) / 72000.0
  // updated 19-Nov-2015
  //(45000.0 + 66000.0 * fx + 1.05 * 37.0 * tc + 950.0 * fo) / 72000.0
  // updated 26-Feb-2016
  (45000.0 + 66000.0 * fx + 1.0 * 37.0 * tc + 950.0 * fo) / 72000.0
}

lazy val capesizeDrummondRdamMid = for { //added on Oct 10, 2011
  tc  <- capeTc
  fo  <- fo35FobAraBarges
  fx 	<- eurUsd
} yield {
  //(173333.0 + 85333.0 * fx + 1.05 * 42.5 * tc + 1500.0 * fo) / 165000.0
  //(145000.0 + 0.95 * 39.0 * tc + 950.0 * fo) / 75000.0
  // updated 13-Nov-2014
  //(280000.0 + 0.95 * 42.5 * tc + 1500.0 * fo) / 165000.0
  // updated 18-Nov-2015
  //(280000.0 + 0.95 * 39.0 * tc + 1500.0 * fo) / 165000.0
  // updated 19-Nov-2015
  //(80000.0 + 133000.0 * fx + 0.95 * 39.0 * tc + 1500.0 * fo) / 165000.0
  // updated 26-Feb-2016
  (80000.0 + 133000.0 * fx + 1.0 * 37.0 * tc + 1500.0 * fo) / 165000.0
}

lazy val capesizeBolivarRdamMid = for { //added on Oct 10, 2011
  tc  <- capeTc
  fo  <- fo35FobAraBarges
  fx 	<- eurUsd
} yield {
  //(188333.0 + 85333.0 * fx + 1.05 * 39.0 * tc + 1500.0 * fo) / 160000.0
  //(295000.0 + 1.05 * 39.0 * tc + 1500.0 * fo) / 160000.0
  // updated 13-Nov-2014
  //(295000.0 + 0.95 * 39.0 * tc + 1500.0 * fo) / 160000.0
  // updated 19-Nov-2015
  //(95000.0 + 133000.0 * fx + 0.95 * 39.0 * tc + 1500.0 * fo) / 160000.0
  // updated 26-Feb-2016
  (95000.0 + 133000.0 * fx + 1.0 * 37.0 * tc + 1500.0 * fo) / 163500.0
}

lazy val capesizeBolivarRdamEcoMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {

  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (43.0 * tc * 1.0 * (1.0 - 0.0375) + 1215.0 * im + 165.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (1215.0 + 165.0 + 0.0) + 110000.0 + 160000.0 * fx + 0.0 * fr + 0.0) / 168000.0 / (1.0 - 0.0 - 0.0)
  //	  (43.0 * tc * 1.0 * (1.0 - 0.0375) + 1215.0 * im + 165.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (1215.0 + 165.0 + 0.0) + 110000.0 + 160000.0 * fx + 0.0 * fr + 0.0) / 168000.0
  //	(43.0 * tc * 1.0 * (1.0 - 0.0375) + 1215.0 * 1.1 + 165.0 * 1.1 + 0.0 * 1.1 + (10.0 + 0.0 + 0.0) * (1215.0 + 165.0 + 0.0) + 110000.0 + 160000.0 * 1.1 + 0.0 * 1.1 + 0.0) / 168000.0 / (1 - 0.0 - 0.0)
}

lazy val capesizeBolivarRdamFullMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (38.0 * tc * 1.0 * (1.0 - 0.0375) + 1510.0 * im + 190.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (1510.0 + 190.0 + 0.0) + 110000.0 + 160000.0 * fx + 0.0 * fr + 0.0) / 168000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxDrummondItaquiFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (130000.0 + 20.0 * tp * 1.0 * (1.0 - 0.0375) + 440.0 * im + 0.0 * mg + 0.0 * is + (10.0 + 85.0 + 0.0) * (440.0 + 0.0 + 0.0) + 0.0 * fx + 0.0 * fr + 0.0) / 55000.0 / (1.0 - 0.0 - 0.0)
}

lazy val supramaxDrummondItaquiFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  _ <- panamaxTc
  ts  <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (24.0 * ts * 1.0 * (1.0 - 0.0375) + 470.0 * im + 25.0 * mg + 0.0 * is + (10.0 + 85.0 + 0.0) * (470.0 + 25.0 + 0.0) + 120000.0 + 0.0 * fx + 0.0 * fr + 0.0) / 47000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxDrummondPecemFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (25.0 * tp * 1.0 * (1.0 - 0.0375) + 490.0 * im + 0.0 * mg + 0.0 * is + (10.0 + 85.0 + 0.0) * (490.0 + 0.0 + 0.0) + 140000.0 + 0.0 * fx + 0.0 * fr + 0.0) / 72000.0 / (1.0 - 0.0 - 0.0)
}

lazy val capesizeDrummondRdamEcoMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (44.0 * tc * 1.0 * (1.0 - 0.0375) + 1265.0 * im + 165.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (1265.0 + 165.0 + 0.0) + 90000.0 + 160000.0 * fx + 0.0 * fr + 0.0) / 168000.0 / (1.0 - 0.0 - 0.0)
}

lazy val capesizeDrummondRdamFullMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (39.0 * tc * 1.0 * (1.0 - 0.0375) + 1565.0 * im + 190.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (1565.0 + 190.0 + 0.0) + 90000.0 + 160000.0 * fx + 0.0 * fr + 0.0) / 168000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxIzmirRdamDiffMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (16.0 * tp * 1.0 * (1.0 - 0.0375) + 430.0 * im + 0.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (430.0 + 0.0 + 0.0) + 50000.0 - 85000.0 * fx + 0.0 * fr + 0.0) / 69000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxJorflasfarRdamDiffMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (5.0 * tp * 1.0 * (1.0 - 0.0375) + 150.0 * im + 0.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (150.0 + 0.0 + 0.0) + 70000.0 - 85000.0 * fx + 0.0 * fr + 0.0) / 60000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxMurmanskRdamFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (19.0 * tp * 1.0 * (1.0 - 0.0375) + 210.0 * im + 150.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (210.0 + 150.0 + 0.0) + 0.0 + 85000.0 * fx + 6000000.0 * fr + 0.0) / 72000.0 / (1.0 - 0.0 - 0.0)
}

lazy val capesizeNacalaRbctDiffMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (6.0 * tc * 1.0 * (1.0 - 0.0375) + 0.0 * im + 0.0 * mg + 70.0 * is + (10.0 + 0.0 + 0.0) * (0.0 + 0.0 + 70.0) + 100000.0 + 0.0 * fx + 0.0 * fr + 90000.0) / 167000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxNewcastleMatsuuraFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (40.0 * tp * 1.116 * (1.0 - 0.0375) + 0.0 * im + 0.0 * mg + 1020.0 * is + (10.0 + 10.0 + 0.0) * (0.0 + 0.0 + 1020.0) + 160000.0 + 0.0 * fx + 0.0 * fr + 0.0) / 93500.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxNewcastleReihokuFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (40.0 * tp * 1.14 * (1.0 - 0.0375) + 0.0 * im + 0.0 * mg + 975.0 * is + (10.0 + 10.0 + 0.0) * (0.0 + 0.0 + 975.0) + 120000.0 + 0.0 * fx + 0.0 * fr + 0.0) / 82000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxRigaRdamFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (16.0 * tp * 1.0 * (1.0 - 0.0375) + 0.0 * im + 250.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (0.0 + 250.0 + 0.0) + 0.0 + 185000.0 * fx + 0.0 * fr + 0.0) / 72000.0 / (1.0 - 0.0 - 0.0)
}

lazy val capesizeUsecRdamEcoMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (36.0 * tc * 1.0 * (1.0 - 0.0375) + 525.0 * im + 600.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (520.0 + 600.0 + 0.0) + 125000.0 + 160000.0 * fx + 0.0 * fr + 0.0) / 138000.0 / (1.0 - 0.0 - 0.0)
}

lazy val capesizeUsecRdamFullMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (33.0 * tc * 1.0 * (1.0 - 0.0375) + 650.0 * im + 735.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (650.0 + 735.0 + 0.0) + 125000.0 + 160000.0 * fx + 0.0 * fr + 0.0) / 138000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxUsecRdamFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (30.0 * tp * 1.0 * (1.0 - 0.0375) + 565.0 * im + 155.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (565.0 + 155.0 + 0.0) + 65000.0 + 85000.0 * fx + 0.0 * fr + 0.0) / 72000.0 / (1.0 - 0.0 - 0.0)
}

lazy val capesizeUsgcRdamEcoMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (44.0 * tc * 1.0 * (1.0 - 0.0375) + 1135.0 * im + 400.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (1135.0 + 400.0 + 0.0) + 250000.0 + 160000.0 * fx + 0.0 * fr + 0.0) / 125000.0 / (1.0 - 0.0 - 0.0)
}

lazy val capesizeUsgcRdamFullMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (39.0 * tc * 1.0 * (1.0 - 0.0375) + 1410.0 * im + 490.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (1410.0 + 490.0 + 0.0) + 250000.0 + 160000.0 * fx + 0.0 * fr + 0.0) / 125000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxUsgcRdamFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (37.0 * tp * 1.0 * (1.0 - 0.0375) + 750.0 * im + 210.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (750.0 + 210.0 + 0.0) + 160000.0 + 85000.0 * fx + 0.0 * fr + 0.0) / 72000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxUstlugaRdamFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (17.0 * tp * 1.0 * (1.0 - 0.0375) + 0.0 * im + 270.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (0.0 + 270.0 + 0.0) + 0.0 + 85000.0 * fx + 5500000.0 * fr + 0.0) / 72000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxUswcJapanNewcastleDiffMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (11.0 * tp * 1.0 * (1.0 - 0.0375) + 0.0 * im + 0.0 * mg + 300.0 * is + (10.0 + 0.0 + 0.0) * (0.0 + 0.0 + 300.0) + 100000.0 + 0.0 * fx + 0.0 * fr + 0.0) / 60000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxVysotskRdamFullMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (20.0 * tp * 1.0 * (1.0 - 0.0375) + 0.0 * im + 270.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (0.0 + 270.0 + 0.0) + 0.0 + 70000.0 * fx + 6000000.0 * fr + 0.0) / 55000.0 / (1.0 - 0.0 - 0.0)
}

lazy val panamaxVysotskRdamTransMid = for { //added on 02-Dec-2019
  _ <- capeTc5
  tp  <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (21.0 * tp * 1.0 * (1.0 - 0.0375) + 0.0 * im + 300.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (0.0 + 300.0 + 0.0) + 0.0 + 85000.0 * fx + 7000000.0 * fr + 0.0) / 72000.0 / (1.0 - 0.0 - 0.0)
}

lazy val capesizeZonguldakRdamDiffMid = for { //added on 02-Dec-2019
  tc  <- capeTc5
  _ <- panamaxTc
  _ <- supramaxTc
  im  <- imo map { monthly => change(monthly).to(Day) }
  mg  <- mgo map { monthly => change(monthly).to(Day) }
  is  <- ims map { monthly => change(monthly).to(Day) }
  fx 	<- eurUsdM map { monthly => change(monthly).to(Day) }
  fr  <- rubUsdM map { monthly => change(monthly).to(Day) }
} yield {
  // days * tc * mkt_ratio * (1-addOnTc) + vol_imo * imo + vol_mgo * mgo + vol_ims * ims + (fuel_deliv + fuel_loc + backwd) * (vol_imo + vol_mgo + vol_ims) + port_usd + prt_eur * fx + prt_rub * fr + fixed
  // divided by   / intake / (1-AddOnVC - AddOnBrokerCom )
  (10.0 * tc * 1.0 * (1.0 - 0.0375) + 450.0 * im + 0.0 * mg + 0.0 * is + (10.0 + 0.0 + 0.0) * (450.0 + 0.0 + 0.0) + 25000.0 + 0.0 * fx + 0.0 * fr + 0.0) / 160000.0 / (1.0 - 0.0 - 0.0)
}


lazy val panamaxCarbosanKoperMid = for { //added on Dec 2, 2011
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  (108000.0 + 1.05 * 43.5 * tc + 1150.0 * fo) /70500.0
}

lazy val panamaxRigaBilbaoMid = for { //added on Dec 2, 2011
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  (200000.0 + 1.1 * 17.0 * tc + 320.0 * fo) / 70000.0
}

lazy val panamaxKakinadaRdamMid = for { //added on Jan 17, 2012
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  (170000.0 + 1.0 *  36.0 * tc + 950.0 * (fo + 100.0)) / 65000.0
}

lazy val capesizeZonguldakRdamMid = for { //added on Jan 17, 2012
  tc <- capeTc
  fo <- fo35FobAraBarges
} yield {
  //(24250.0 + 1.05 * 9.0 * tc + 450.0 * fo) / 160000.0
  // updated 26-Feb-2016
  (24250.0 + 1.0 * 9.5 * tc + 450.0 * fo) / 160000.0
}

lazy val handysizeMobileLiverPool = for { //added on Feb 17, 2012
  tc <- handyTc
  fo <- fo35FobAraBarges
} yield (110000.0 + 1.0 * 36.0 * tc + 515.0 * fo) / 27000.0

lazy val supramaxItaquiRdamMid = for { //added on Mar 22, 2012, updated on Dec 10, 2013, updated on Mar 14, 2014
  tc <- supramaxTc
  fo <- fo35FobAraBarges
} yield {
  //(130000.0 + 1.05 *  23.5 * tc + 410.0 * (fo + 80.0)) / 45000.0
  // updated 13-Nov-2014
  //(130000.0 + 1.00 *  23.5 * tc + 410.0 * (fo + 80.0)) / 41000.0
  // updated 26-Feb-2016
  (130000.0 + 1.00 *  23.5 * tc + 410.0 * (fo + 80.0)) / 45000.0
}
lazy val panamaxItaquiRdamMid = for { //added on Apr 26, 2018
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  (140000.0 + 1.00 *  24.0 * tc + 475.0 * (fo + 80.0)) / 65000.0
}
/**
  * @note port costs are negative here (143000 - usecrdam's port costs)
  */
lazy val panamaxIcdasRdamMid = for { //added on Apr 2, 2012
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  (-27000.0 + 1.05 *  10.0 * tc + 230.0 * fo) / 72000.0
}

lazy val panamaxIzmirRdamMid = for { //added on 12-May-2015
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  //(-82000.0 + 1.05 *  14.6 * tc + 430.0 * fo) / 69000.0
  // updated 26-Feb-2016
  (-82000.0 + 1.0 *  15.0 * tc + 430.0 * fo) / 69000.0
}

lazy val panamaxJorflasfar = for { //added on Aug 13, 2012, edited Oct 26, 2012
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
} yield {
  //(90000.0 + 1.05 * -11.0 * tc - 160.0 * fo) / 504000.0
  // updated 26-Feb-2016
  //(90000.0 + 1.0 * -1.38 * tc - 20.0 * fo) / 61000.0
  // updated 21-Nov-2016
  (    0.0 + 1.0 * 5.0 * tc + 150.0 * fo) / 60000.0
}

lazy val capesizeMoneyPointRdamMid = for { //added on Oct 25, 2012
  tc <- capeTc
  fo <- fo35FobAraBarges
  //fx 	<- eurUsd
} yield {
  //(93333.0 + 89667.0 * fx + 1.05 * -5.0 * tc - 130.0 * fo) / 160000.0
  //(30000.0 + 1.05 * -5.0 * tc - 130.0 * fo) / 160000.0
  // updated 26-Feb-2016
  (30000.0 + 1.0 * -5.0 * tc - 130.0 * fo) / 160000.0
}

lazy val capesizeSinesRdamMid = for { //added on Oct 25, 2012, edited Jan 16, 2013
  tc <- capeTc
  fo <- fo35FobAraBarges
  fx <- eurUsd
} yield {
  //(-(47133 + 1/3d) + 1.05 * -6.0 * tc - 180.0 * fo) / 160000.0
  // updated 13-Nov-2014
  //(-(47133 + 1/3d) + 0.95 * -6.0 * tc - 180.0 * fo) / 160000.0
  // updated 04-Dec-2015
  //(-(55000.0 * fx) + 0.95 * -6.0 * tc - 180.0 * fo) / 160000.0
  // updated 26-Feb-2016
  //(-(55000.0 * fx) + 1.0 * -6.0 * tc - 180.0 * fo) / 160000.0
  // updated 3-Mar-2016
  //(-47133.0 -(55000.0 * fx) + 1.0 * -6.0 * tc - 180.0 * fo) / 160000.0
  // updated 09-Mar-2016
  (-(55000.0 * fx) + 1.0 * -6.0 * tc - 180.0 * fo) / 160000.0
}

lazy val panamaxMurmanskMid = for { //added on Jan 16 2013
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  //(180000.0 + 1.1 * 20.0 * tc + 340.0 * fo) / 72000.0
  // updated 26-Feb-2016
  (250000.0 + 1.0 * 21.0 * tc + 340.0 * fo) / 72000.0
}

lazy val panamaxVancouverMid = for { // created 31-May-2017
  tc <- panamaxTc
  fo <- fo35FobAraBarges
  fx <- eurUsd
} yield {

  (85000.0 + 66000.0 * fx + 1.0 * 80.0 * tc + 2500.0 * ( fo + 0.0 )) / 80000.0
}

lazy val panamaxMatsuuraMid = for { // created 13-Mar-2018
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {

  (95000.0 + 1.2 * 39.8 * tc + 1020.0 * ( fo + 0.0 )) / 93500.0
}

lazy val panamaxReihokuMid = for { // created 13-Mar-2018
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {

  (95000.0 + 1.2 * 39.8 * tc + 975.0 * ( fo + 0.0 )) / 82000.0
}

lazy val panamaxUswestJapanMid = for { // created 22-May-2019
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {

  (100000.0 + 1.0 * 11.0 * tc + 300.0 * ( fo + 0.0 )) / 60000.0
}

lazy val japanMid = for { // created 22-May-2019
  ap <- api6
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {

  ap - ((100000.0 + 1.0 * 11.0 * tc + 300.0 * ( fo + 0.0 )) / 60000.0)
}

lazy val panamaxVysotskMid = for { //added on Aug 26 2013
  tc <- panamaxTc
  fo <- fo35FobAraBarges
  fx <- eurUsd
  fy <- rubUsd
} yield {
  //(245000.0 + 1.1  * 20.4  * tc + 300.0 * fo) / 75000.0
  // updated 13-Nov-2014
  //(245000.0 + 1.09 * 20.4  * tc + 300.0 * fo) / 78000.0
  // updated 28-Jul-2015
  //(195000.0 + 1.00 * 17.94 * tc + 284.0 * ( fo + 225.0 ) + 50050.0) / 55000.0
  // updated 19-Nov-2015
  //(102000.0 + 66000.0 * fx + 1.00 * 17.94 * tc + 284.0 * ( fo + 225.0 ) + 50050.0) / 55000.0
  // updated 26-Feb-2016  // 50050.0 is ice surcharge, not needed in the summer
  //(102000.0 + 66000.0 * fx + 1.00 * 19.0 * tc + 284.0 * ( fo + 140.0 ) + 50050.0) / 55000.0
  //(102000.0 + 66000.0 * fx + 1.00 * 19.0 * tc + 284.0 * ( fo + 140.0 )) / 55000.0
  // updated 29-Feb-2016
  (66000.0 * fx + 6500000.0 * fy + 1.0 * 19.0 * tc + 284.0 * ( fo + 140.0 )) / 55000.0
}

lazy val panamaxUstlugaMid = for { //added on Feb 29 2016
  tc <- panamaxTc
  fo <- fo35FobAraBarges
  fx <- eurUsd
  fy <- rubUsd
} yield {
  (66000.0 * fx + 5400000.0 * fy + 1.0 * 16.0 * tc + 250.0 * ( fo + 140.0 )) / 75000.0
}

lazy val panamaxGoglandMid = for { //added on Sep 29 2015
  tc <- panamaxTc
  fo <- fo35FobAraBarges
  fx <- eurUsd
  fy <- rubUsd
} yield {
  //(245050.0 + 1.00 * 20.4 * tc + 300.0 * ( fo + 225.0 )) / 75000.0
  // updated 19-Nov-2015
  //(102000.0 + 66000.0 * fx + 1.00 * 20.4 * tc + 300.0 * ( fo + 225.0 )) / 75000.0
  // updated 26-Feb-2016
  //(102000.0 + 66000.0 * fx + 1.00 * 20.4 * tc + 300.0 * ( fo + 140.0 )) / 80000.0
  // updated 29-Feb-2016
  (66000.0 * fx + 6500000.0 * fy + 1.00 * 20.4 * tc + 300.0 * ( fo + 140.0 )) / 80000.0
}

lazy val panamaxYuzhnyMid = for { //added on Sep 17 2013
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  (256000.0 + 1.0 * 26.7 * tc + 525.0 * (fo + 15d)) / 75000.0
}

lazy val panamaxPecemMid = for { //added on Nov 15 2013, updated on Mar 14, 2014
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  //(130000.0 + 1.05 * 24d * tc + 460d * (fo + 80d)) / 73000.0
  //  updated 13-Nov-2014
  //(130000.0 + 1.00 * 24d * tc + 460d * (fo + 80d)) / 74000.0
  // updated 26-Feb-2016
  (130000.0 + 1.00 * 24d * tc + 460d * (fo + 80d)) / 75000.0
}

lazy val panamaxBolivarPortotorresMid = for { // added on Mar 6, 2014
  tc  <- panamaxTc
  fo  <- fo35FobAraBarges
  fx  <- eurUsd
} yield {
  //(145000.0 + 1.05 * 39.0 * tc + 950.0 * fo) / 75000.0
  // updated 13-Nov-2014
  //(145000.0 + 1.00 * 39.0 * tc + 950.0 * fo) / 78000.0
  // updated 26-Feb-2016
  (95000.0 + 133000.0 * fx + 1.00 * 39.0 * tc + 950.0 * fo) / 75000.0
}

lazy val panamaxLazaroCardenasRdamMid = for { // added on Jun 16, 2014
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  //(465000.0 + 1.05 * 20.0 * tc + 450.0 * (fo + 50.0)) / 55000.0
  // updated 26-Feb-2016
  (465000.0 + 1.0 * 20.0 * tc + 450.0 * (fo + 50.0)) / 55000.0
}

lazy val panamaxGdanskRdamMid = for { // added on Mar 28, 2017
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {
  (175000.0 + 1.05 * 14.0 * tc + 165.0 * (fo + 200.0)) / 72000.0
}

lazy val panamaxMejillonesUstlugaMid = for { // added on Aug 22, 2016
  tc <- panamaxTc
  fo <- fo35FobAraBarges
  fy <- rubUsd
} yield {
  (5400000.0 * fy + 390000.0 + 1.0 * 72.0 * tc + 1800.0 * (fo + 20.0)) / 70000.0
}

lazy val supramaxBuenaventuraKawaMid = for { //added on 22-Oct-2014
  tc <- supramaxTc
  fo <- fo35FobAraBarges
} yield {
  //(100000.0 + 1.55 *  37.5 * tc + 820.0 * (fo + 80.0)) / 48000.0
  // updated 26-Feb-2016
  (100000.0 + 1.55 *  38.5 * tc + 820.0 * (fo + 80.0)) / 48000.0
}

lazy val capesizePortHedlandKawaMid = for { //added on 22-Oct-2014
  tc <- capeTc
  fo <- fo35FobAraBarges
} yield {
  (235000.0 + 1.00 *  29.2 * tc + 1220.0 * (fo + 80.0)) / 167000.0
}

lazy val panamaxFobBigSandyRiverMid = for { //added on 15-Jan-2015
  tc <- panamaxTc
  fo <- fo35FobAraBarges
} yield {									// changed ratio from 1.05 to 1.0 on 05-Feb-2015
  (230000.0 + 1.00 *  39.0 * tc + 1000.0 * fo) / 70500.0
}

lazy val capesizeRc7Rc7Mid = for { //added on 19-Feb-2015  //changed days to 35.62 on 24-Feb-2015
  rc <- rc7
  tc <- capeTc
  fo <- fo35FobAraBarges
  _ <- eurUsd
} yield {
  val m0M1M2 = Month(asof) + 3.months
  //val synthetic = (295000.0 + 1.00 *  35.62 * tc + 1494.87 * fo) / 150000.0
  // updated 02-Dec-2015
  //val synthetic = (80000.0 + 133000.0 * fx + 1.00 *  35.62 * tc + 1494.87 * fo) / 150000.0
  // updated 04-Mar-2016
  val synthetic = (295000.0 + 1.0 * 37.0 * tc + 1500.0 * fo) / 165000.0
  synthetic ++ rc.takeWhile(_._1 < m0M1M2)
}

//TC Differentials

lazy val ventspilsMid = for {
  c  <- api2
  tc <- rigaunrestr
} yield {
  c - tc
}

lazy val usgulfMid = for {
  c  <- api2
  tc <- usgrdam
  _ <- eurUsd
} yield {
  c - tc
}

lazy val tallinMid = for {
  c  <- api2
  tc <- rigarestricted
} yield {
  c - tc
}

lazy val rigarestrictedMid = for {
  c  <- api2
  tc <- rigarestricted
} yield {
  c - tc
}

lazy val rigaunrestrictedMid = for {
  c  <- api2
  tc <- rigaunrestr
} yield {
  c - tc
}

lazy val stpetersburgMid = for {
  c  <- api2
  tc <- rigarestricted
} yield {
  c - tc
}

lazy val northfolkMid = for {
  c  <- api2
  tc <- usecrdam
} yield {
  c - tc
}

lazy val murmansksuekMid = for {
  c  <- api2
  tc <- rigarestricted
} yield {
  c - tc
}

lazy val murmanskccMid = for {
  c  <- api2
  tc <- rigaunrestr
} yield {
  c - tc
}

lazy val maracaiboMid = for {
  c  <- api2
  tc <- maracaibo
} yield {
  c - tc
}

lazy val panamaxRichbayJorflasfarMid = for {
  c  <- api4
  tc <- panamaxRichbayJorflasfar
} yield {
  c + tc
}

lazy val eastcostMid = for {
  c  <- api2
  tc <- usecrdam
} yield {
  c - tc
}

lazy val baltimoreMid = for {
  c  <- api2
  tc <- usecrdam
} yield {
  c - tc
}

lazy val chesapeakeMid = for {
  c  <- api2
  tc <- usecrdam
} yield {
  c - tc
}

lazy val davantidtMid = for {
  c  <- api2
  tc <- usgrdam
} yield {
  c - tc
}

lazy val quindaoMid = for { // added on July 29, 2011
  c <- api4
  tc <- capesizeRichbayQdao
} yield {
  c + tc
}

lazy val panamaxJorflasfarMid = for { //added on Aug 13, 2012
  c  <- api2
  tc <- panamaxJorflasfar
} yield {
  c + tc
}

lazy val panamaxIzmirMid = for { //added on 12-May-2015
  c  <- api2
  tc <- panamaxIzmirRdamMid
} yield {
  c + tc
}

//Fixed Differentials

lazy val portotorresMid = for {
  c  <- api2
} yield {
  c.mapValues{ _ + 6.01}
}

lazy val zonguldakDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 4.0)
}

lazy val zonguldakMid = for { //discharge port
  c  <- api2
  diff <- zonguldakDiff
} yield {
  c + diff
}

lazy val ijmuidenDiff = for { //added on April 29. 2011
  c  <- api2
} yield {
  c.mapValues(_ => 0.0)
}

lazy val ijmuidenMid = for { //added on April 29. 2011
  c  <- api2
  diff <- ijmuidenDiff
} yield {
  c + diff
}

lazy val rietlandenDiff = for { //added on April 29. 2011
  c  <- api2
} yield {
  c.mapValues(_ => 0.0)
}

lazy val rietlandenMid = for { //added on April 29. 2011
  c  <- api2
  diff <- rietlandenDiff
} yield {
  c + diff
}

lazy val belfastDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 11.25)
}

lazy val belfastMid = for { //load port
  c  <- api2
  diff <- belfastDiff
} yield {
  c - diff
}

lazy val indonesiaDiff = for { //added on April 19, 2011
  c  <- api6
} yield {
  c.mapValues(_ => 0.0)
}

lazy val indonesiaMid = for { //added on April 19, 2011
  c  <- api6
  diff <- indonesiaDiff
} yield {
  c - diff
}

lazy val muaraSatuiAnchor = for { //added on Augist 11, 2011
  c <- api6
} yield {
  c
}

lazy val wilhelmshavenDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 1.7)
}

lazy val wilhelmshavenMid = for {
  c  <- api2
  diff <- wilhelmshavenDiff
} yield {
  c + diff
}

lazy val tyneDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 3.75)
}

lazy val tyneMid = for {
  c  <- api2
  diff <- tyneDiff
} yield {
  c + diff
}

lazy val tarragonaDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 5.5)
}

lazy val tarragonaMid = for {
  c  <- api2
  diff <- tarragonaDiff
} yield {
  c + diff
}

lazy val sinesDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => -1.0)
}

lazy val sinesMid = for {
  c  <- api2
  diff <- sinesDiff
} yield {
  c + diff
}

lazy val portburyroyalDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 1.75)
}

lazy val portburyroyalMid = for {
  c  <- api2
  diff <- portburyroyalDiff
} yield {
  c + diff
}

lazy val nordenhamDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 1.33)
}

lazy val nordenhamMid = for {
  c  <- api2
  diff <- nordenhamDiff
} yield {
  c + diff
}

lazy val liverpoolbulkterminalDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 1.75)
}

lazy val liverpoolbulkterminalMid = for {
  c  <- api2
  diff <- liverpoolbulkterminalDiff
} yield {
  c + diff
}

lazy val kielDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 12.5)
}

lazy val kielMid = for {
  c  <- api2
  diff <- kielDiff
} yield {
  c + diff
}

lazy val imminghamidDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 1.35)
}

lazy val imminghamidMid = for {
  c  <- api2
  diff <- imminghamidDiff
} yield {
  c + diff
}

lazy val imminghamhit1Diff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 1.35)
}

lazy val imminghamhit1Mid = for {
  c  <- api2
  diff <- imminghamhit1Diff
} yield {
  c + diff
}

lazy val imminghamhit2Diff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 1.35)
}

lazy val imminghamhit2Mid = for {
  c  <- api2
  diff <- imminghamhit2Diff
} yield {
  c + diff
}

lazy val hamburgDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 0.69)
}

lazy val hamburgMid = for {
  c  <- api2
  diff <- hamburgDiff
} yield {
  c + diff
}

lazy val gijonDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 0.25)
}

lazy val gijonMid = for {
  c  <- api2
  diff <- gijonDiff
} yield {
  c + diff
}

lazy val ghentDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 2.03)
}

lazy val ghentMid = for {
  c  <- api2
  diff <- ghentDiff
} yield {
  c + diff
}

lazy val fosDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 3.59)
}

lazy val fosMid = for {
  c  <- api2
  diff <- fosDiff
} yield {
  c + diff
}

lazy val dunkirkwestDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 1.0)
}

lazy val dunkirkwestMid = for {
  c  <- api2
  diff <- dunkirkwestDiff
} yield {
  c + diff
}

lazy val algecirasDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 2.75)
}

lazy val algecirasMid = for {
  c  <- api2
  diff <- algecirasDiff
} yield {
  c + diff
}

lazy val antwerpDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 3.59)
}

lazy val antwerpMid = for {
  c  <- api2
  diff <- antwerpDiff
} yield {
  c + diff
}

lazy val avonmouthDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 1.75)
}

lazy val avonmouthMid = for {
  c  <- api2
  diff <- avonmouthDiff
} yield {
  c + diff
}

lazy val bremenDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 4.5)
}

lazy val bremenMid = for {
  c  <- api2
  diff <- bremenDiff
} yield {
  c + diff
}

lazy val brunsbuttelDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 4.5)
}

lazy val brunsbuttelMid = for {
  c  <- api2
  diff <- brunsbuttelDiff
} yield {
  c + diff
}

lazy val carbonerasDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 0.0)
}

lazy val carbonerasMid = for {
  c  <- api2
  diff <- carbonerasDiff
}  yield {
  c + diff
}

lazy val corunaDiff = for {
  c  <- api2
} yield {
  c.mapValues(_ => 2.45)
}

lazy val corunaMid = for {
  c  <- api2
  diff <- corunaDiff
} yield {
  c + diff
}

lazy val huangdaoMid = for { // added on July 29 2011
  c <- quindaoMid
  diff = 0.0
} yield {
  c - diff
}

lazy val rizhaoMid = for { // added on July 29 2011
  c <- quindaoMid
  diff = 0.0
} yield {
  c - diff
}

lazy val fangchengMid = for { // added on July 29 2011
  c <- quindaoMid
  diff = 1.0
} yield {
  c - diff
}

lazy val zhoushanMid = for { // added on July 29 2011
  c <- quindaoMid
  diff = 0.3
} yield {
  c - diff
}

lazy val redcarMid = for {
  c <- api2
} yield {
  c mapValues { _ + 1.0}
}

lazy val usgulfFobRiver = for {
  c <- usgulfMid
} yield {
  c mapValues { _ - 28.5}
}

lazy val api4Rc4 = for { // added on Feb 8 2012
  c <- api4
  rc <- rc4
} yield c + rc

//Solarc curve
api2Jpm map {srv.CoalApi2Fc.MidEur1330.getOrCreate(asof) <-- _}

//Synthetic Freight Curves
capesizeRc5Mid map capesize_Rc5_Mid.getOrCreate(asof).<--
capesizeRc5MidBom map capesize_Rc5_Mid_Bom.getOrCreate(asof).<--
capesizeRc7 map {srv.FreightSynthFc.CapesizeRc7.getOrCreate(asof) <-- _}
capesizeRc7Rc7Mid map capesize_Rc7_Rc7_Mid.getOrCreate(asof).<--
capesizeBolivarQdao map {srv.FreightSynthFc.CapesizeBolivarQdaoMid.getOrCreate(asof) <-- _}
capesizeBolivarRdamMid map {srv.FreightSynthFc.CapesizeBolivarRdamMid.getOrCreate(asof) <-- _}
capesizeDrummondRdamMid map {srv.FreightSynthFc.CapesizeDrummondRdamMid.getOrCreate(asof) <-- _}
capesizeMoneyPointRdamMid map {srv.FreightSynthFc.CapesizeMoneyPointRdamMid.getOrCreate(asof) <-- _}
capesizeNewcastleQdao map {srv.FreightSynthFc.CapesizeNewcastleQdaoMid.getOrCreate(asof) <-- _}
//capesizeProdecoRdamMid map {srv.FreightSynthFc.CapesizeProdecoRdamMid.getOrCreate(asof) <-- _}
capesizeRichbayQdao map {srv.FreightSynthFc.CapesizeRichbayQdaoMid.getOrCreate(asof) <-- _}
//capesizeRioCordobaRdamMid map {srv.FreightSynthFc.CapesizeRioCordobaRdamMid.getOrCreate(asof) <-- _}
capesizeSinesRdamMid map {srv.FreightSynthFc.CapesizeSinesRdamMid.getOrCreate(asof) <-- _}
capesizeTanjungQdao map {srv.FreightSynthFc.CapesizeTanjungQdaoMid.getOrCreate(asof) <-- _}
capesizeZonguldakRdamMid map {srv.FreightSynthFc.CapesizeZonguldakRdamMid.getOrCreate(asof) <-- _}
handysizeMobileLiverPool map {srv.FreightSynthFc.HandysizeMobileLiverpoolMid.getOrCreate(asof) <-- _}
maracaibo map {srv.FreightSynthFc.Maracaibo.getOrCreate(asof) <-- _}
panamaxBanjarRdamMid map {srv.FreightSynthFc.PanamaxBanjarRdamMid.getOrCreate(asof) <-- _}
panamaxFobBigSandyRiverMid map panamax_Fob_Big_Sandy_River_Mid.getOrCreate(asof).<--
panamaxBolivarQdao map {srv.FreightSynthFc.PanamaxBolivarQdaoMid.getOrCreate(asof) <-- _}
panamaxBolivarPortotorresMid map panamax_Bolivar_Portotorres_Mid.getOrCreate(asof).<--
panamaxBolivarRdamMid map {srv.FreightSynthFc.PanamaxBolivarRdamMid.getOrCreate(asof) <-- _}
panamaxCarbosanKoperMid map {srv.FreightSynthFc.PanamaxCarbosanKoperMid.getOrCreate(asof) <-- _}
panamaxCarbosanRdamMid map {srv.FreightSynthFc.PanamaxCarbosanRdamMid.getOrCreate(asof) <-- _}
panamaxIcdasRdamMid map {srv.FreightSynthFc.PanamaxIcdasRdamMid.getOrCreate(asof) <-- _}
panamaxIzmirRdamMid map {panamax_Izmir_Rdam_Mid.getOrCreate(asof) <-- _}
panamaxJorflasfar map {srv.FreightSynthFc.PanamaxJorflasfarMid.getOrCreate(asof) <-- _}
panamaxKakinadaRdamMid map {srv.FreightSynthFc.PanamaxKakinadaRdamMid.getOrCreate(asof) <-- _}
panamaxLazaroCardenasRdamMid map panamax_Lazaro_Cardenas_Rdam_Mid.getOrCreate(asof).<--
panamaxGdanskRdamMid map panamax_Gdansk_Rdam_Mid.getOrCreate(asof).<--
panamaxMejillonesUstlugaMid map panamax_Mejillones_Ustluga_Mid.getOrCreate(asof).<--
supramaxBuenaventuraKawaMid map supramax_Buenaventura_Kawa_Mid.getOrCreate(asof).<--
capesizePortHedlandKawaMid map capesize_Port_Hedland_Kawa_Mid.getOrCreate(asof).<--
panamaxMurmanskMid map {srv.FreightSynthFc.PanamaxMurmanskMid.getOrCreate(asof) <-- _}
panamaxNewcastleQdao map {srv.FreightSynthFc.PanamaxNewcastleQdaoMid.getOrCreate(asof) <-- _}
panamaxPecemMid map panamax_Pecem_Mid.getOrCreate(asof).<--
panamaxRichbayJorflasfar map {srv.FreightSynthFc.PanamaxRichbayJorflasfarMid.getOrCreate(asof) <-- _}
panamaxRichbayQdao map {srv.FreightSynthFc.PanamaxRichbayQdaoMid.getOrCreate(asof) <-- _}
panamaxRichbayRdamMid map {srv.FreightSynthFc.PanamaxRichbayRdamMid.getOrCreate(asof) <-- _}
panamaxRigaBilbaoMid map {srv.FreightSynthFc.PanamaxRigaBilbaoMid.getOrCreate(asof) <-- _}
panamaxTanjungQdao map {srv.FreightSynthFc.PanamaxTanjungQdaoMid.getOrCreate(asof) <-- _}
// panamaxVysotskMid map {srv.FreightSynthFc.PanamaxVysotskMid.getOrCreate(asof) <-- _}
panamaxUstlugaMid map {panamax_Ustluga_Mid.getOrCreate(asof) <-- _}
panamaxVancouverMid map {panamax_Vancouver_Mid.getOrCreate(asof) <-- _}
panamaxVysotskMid map {panamax_Vysotsk_Mid.getOrCreate(asof) <-- _}
panamaxGoglandMid map {panamax_Gogland_Mid.getOrCreate(asof) <-- _}
panamaxYuzhnyMid map panamax_Yuzhny_Mid.getOrCreate(asof).<--
rigarestricted map {srv.FreightSynthFc.Rigarestricted.getOrCreate(asof) <-- _}
rigaunrestr map {srv.FreightSynthFc.Rigaunrestr.getOrCreate(asof) <-- _}
supramaxItaquiRdamMid map supramax_Itaqui_Rdam_Mid.getOrCreate(asof).<--
panamaxItaquiRdamMid map panamax_Itaqui_Rdam_Mid.getOrCreate(asof).<--
usecrdam map {srv.FreightSynthFc.Usecrdam.getOrCreate(asof) <-- _}
usgrdam map {srv.FreightSynthFc.Usgrdam.getOrCreate(asof) <-- _}

//TC Differentials
baltimoreMid map {srv.CoalApi2Fc.BaltimoreMid.getOrCreate(asof) <-- _}
chesapeakeMid map {srv.CoalApi2Fc.ChesapeakeMid.getOrCreate(asof) <-- _}
davantidtMid map {srv.CoalApi2Fc.DavantidtMid.getOrCreate(asof) <-- _}
eastcostMid map {srv.CoalApi2Fc.EastcostMid.getOrCreate(asof) <-- _}
maracaiboMid map {srv.CoalApi2Fc.MaracaiboMid.getOrCreate(asof) <-- _}
//both murmansk curves will be removed soon
murmanskccMid map {srv.CoalApi2Fc.MurmanskccMid.getOrCreate(asof) <-- _}
murmansksuekMid map {srv.CoalApi2Fc.MurmansksuekMid.getOrCreate(asof) <-- _}
northfolkMid map {srv.CoalApi2Fc.NorthfolkMid.getOrCreate(asof) <-- _}
panamaxJorflasfarMid map {srv.CoalApi2Fc.PanamaxJorflasfarMid.getOrCreate(asof) <-- _}
panamaxIzmirMid map {panamax_Izmir_Mid.getOrCreate(asof) <-- _}
panamaxRichbayJorflasfarMid map {srv.CoalApi4Fc.PanamaxRichbayJorflasfarMid.getOrCreate(asof) <-- _}
quindaoMid map {srv.CoalApi4Fc.QuindaoMid.getOrCreate(asof) <-- _}
rigarestrictedMid map {srv.CoalApi2Fc.RigarestrictedMid.getOrCreate(asof) <-- _}
rigaunrestrictedMid map {srv.CoalApi2Fc.RigaunrestrictedMid.getOrCreate(asof) <-- _}
stpetersburgMid map {srv.CoalApi2Fc.StpetersburgMid.getOrCreate(asof) <-- _}
tallinMid map {srv.CoalApi2Fc.TallinMid.getOrCreate(asof) <-- _}
usgulfMid map {srv.CoalApi2Fc.UsgulfMid.getOrCreate(asof) <-- _}
ventspilsMid map {srv.CoalApi2Fc.VentspilsMid.getOrCreate(asof) <-- _}

//Fixed Differentials
api4Rc4 map {srv.CoalApi4Fc.Rc4Mid.getOrCreate(asof) <-- _}
algecirasDiff map {srv.CoalApi2Fc.AlgecirasDiff.getOrCreate(asof) <-- _}
algecirasMid map {srv.CoalApi2Fc.AlgecirasMid.getOrCreate(asof) <-- _}
antwerpDiff map {srv.CoalApi2Fc.AntwerpDiff.getOrCreate(asof) <-- _}
antwerpMid map {srv.CoalApi2Fc.AntwerpMid.getOrCreate(asof) <-- _}
avonmouthDiff map {srv.CoalApi2Fc.AvonmouthDiff.getOrCreate(asof) <-- _}
avonmouthMid map {srv.CoalApi2Fc.AvonmouthMid.getOrCreate(asof) <-- _}
belfastDiff map {srv.CoalApi2Fc.BelfastDiff.getOrCreate(asof) <-- _}
belfastMid map {srv.CoalApi2Fc.BelfastMid.getOrCreate(asof) <-- _}
bremenDiff map {srv.CoalApi2Fc.BremenDiff.getOrCreate(asof) <-- _}
bremenMid map {srv.CoalApi2Fc.BremenMid.getOrCreate(asof) <-- _}
brunsbuttelDiff map {srv.CoalApi2Fc.BrunsbuttelDiff.getOrCreate(asof) <-- _}
brunsbuttelMid map {srv.CoalApi2Fc.BrunsbuttelMid.getOrCreate(asof) <-- _}
carbonerasDiff map {srv.CoalApi2Fc.CarbonerasDiff.getOrCreate(asof) <-- _}
carbonerasMid map {srv.CoalApi2Fc.CarbonerasMid.getOrCreate(asof) <-- _}
corunaDiff map {srv.CoalApi2Fc.CorunaDiff.getOrCreate(asof) <-- _}
corunaMid map {srv.CoalApi2Fc.CorunaMid.getOrCreate(asof) <-- _}
dunkirkwestDiff map {srv.CoalApi2Fc.DunkirkwestDiff.getOrCreate(asof) <-- _}
dunkirkwestMid map {srv.CoalApi2Fc.DunkirkwestMid.getOrCreate(asof) <-- _}
fangchengMid map {srv.CoalApi4Fc.FangchengMid.getOrCreate(asof) <-- _}
fosDiff map {srv.CoalApi2Fc.FosDiff.getOrCreate(asof) <-- _}
fosMid map {srv.CoalApi2Fc.FosMid.getOrCreate(asof) <-- _}
ghentDiff map {srv.CoalApi2Fc.GhentDiff.getOrCreate(asof) <-- _}
ghentMid map {srv.CoalApi2Fc.GhentMid.getOrCreate(asof) <-- _}
gijonDiff map {srv.CoalApi2Fc.GijonDiff.getOrCreate(asof) <-- _}
gijonMid map {srv.CoalApi2Fc.GijonMid.getOrCreate(asof) <-- _}
hamburgDiff map {srv.CoalApi2Fc.HamburgDiff.getOrCreate(asof) <-- _}
hamburgMid map {srv.CoalApi2Fc.HamburgMid.getOrCreate(asof) <-- _}
huangdaoMid map {srv.CoalApi4Fc.HuangdaoMid.getOrCreate(asof) <-- _}
ijmuidenDiff map {srv.CoalApi2Fc.IjmuidenDiff.getOrCreate(asof) <-- _}
ijmuidenMid map {srv.CoalApi2Fc.IjmuidenMid.getOrCreate(asof) <-- _}
imminghamhit1Diff map {srv.CoalApi2Fc.Imminghamhit1Diff.getOrCreate(asof) <-- _}
imminghamhit1Mid map {srv.CoalApi2Fc.Imminghamhit1Mid.getOrCreate(asof) <-- _}
imminghamhit2Diff map {srv.CoalApi2Fc.Imminghamhit2Diff.getOrCreate(asof) <-- _}
imminghamhit2Mid map {srv.CoalApi2Fc.Imminghamhit2Mid.getOrCreate(asof) <-- _}
imminghamidDiff map {srv.CoalApi2Fc.ImminghamidDiff.getOrCreate(asof) <-- _}
imminghamidMid map {srv.CoalApi2Fc.ImminghamidMid.getOrCreate(asof) <-- _}
indonesiaDiff map {srv.CoalApi6Fc.IndonesiaDiff.getOrCreate(asof) <-- _}
indonesiaMid map {srv.CoalApi6Fc.IndonesiaMid.getOrCreate(asof) <-- _}
kielDiff map {srv.CoalApi2Fc.KielDiff.getOrCreate(asof) <-- _}
kielMid map {srv.CoalApi2Fc.KielMid.getOrCreate(asof) <-- _}
liverpoolbulkterminalDiff map {srv.CoalApi2Fc.LiverpoolbulkterminalDiff.getOrCreate(asof) <-- _}
liverpoolbulkterminalMid map {srv.CoalApi2Fc.LiverpoolbulkterminalMid.getOrCreate(asof) <-- _}
muaraSatuiAnchor map {srv.CoalApi6Fc.MuaraSatuiAnchor.getOrCreate(asof) <-- _}
nordenhamDiff map {srv.CoalApi2Fc.NordenhamDiff.getOrCreate(asof) <-- _}
nordenhamMid map {srv.CoalApi2Fc.NordenhamMid.getOrCreate(asof) <-- _}
portburyroyalDiff map {srv.CoalApi2Fc.PortburyroyalDiff.getOrCreate(asof) <-- _}
portburyroyalMid map {srv.CoalApi2Fc.PortburyroyalMid.getOrCreate(asof) <-- _}
portotorresMid map {srv.CoalApi2Fc.PortotorresMid.getOrCreate(asof) <-- _}
redcarMid map {srv.CoalApi2Fc.RedcarMid.getOrCreate(asof) <-- _}
rietlandenDiff map {srv.CoalApi2Fc.RietlandenDiff.getOrCreate(asof) <-- _}
rietlandenMid map {srv.CoalApi2Fc.RietlandenMid.getOrCreate(asof) <-- _}
rizhaoMid map {srv.CoalApi4Fc.RizhaoMid.getOrCreate(asof) <-- _}
//both sines curves will be removed sone
sinesDiff map {srv.CoalApi2Fc.SinesDiff.getOrCreate(asof) <-- _}
sinesMid map {srv.CoalApi2Fc.SinesMid.getOrCreate(asof) <-- _}
tarragonaDiff map {srv.CoalApi2Fc.TarragonaDiff.getOrCreate(asof) <-- _}
tarragonaMid map {srv.CoalApi2Fc.TarragonaMid.getOrCreate(asof) <-- _}
tyneDiff map {srv.CoalApi2Fc.TyneDiff.getOrCreate(asof) <-- _}
tyneMid map {srv.CoalApi2Fc.TyneMid.getOrCreate(asof) <-- _}
usgulfFobRiver map {srv.CoalApi2Fc.UsgulfFobRiver.getOrCreate(asof) <-- _}
wilhelmshavenDiff map {srv.CoalApi2Fc.WilhelmshavenDiff.getOrCreate(asof) <-- _}
wilhelmshavenMid map {srv.CoalApi2Fc.WilhelmshavenMid.getOrCreate(asof) <-- _}
zhoushanMid map {srv.CoalApi4Fc.ZhoushanMid.getOrCreate(asof) <-- _}
zonguldakDiff map {srv.CoalApi2Fc.ZonguldakDiff.getOrCreate(asof) <-- _}
zonguldakMid map {srv.CoalApi2Fc.ZonguldakMid.getOrCreate(asof) <-- _}
capesizeNacalaRbctMid map {capesize_Nacala_Rbct_Mid.getOrCreate(asof) <-- _}
panamaxMatsuuraMid map {panamax_Matsuura_Mid.getOrCreate(asof) <-- _}
panamaxReihokuMid map {panamax_Reihoku_Mid.getOrCreate(asof) <-- _}
panamaxUswestJapanMid map {panamax_Uswest_Japan_Mid.getOrCreate(asof) <-- _}
japanMid map {japan_Mid.getOrCreate(asof) <-- _}

capesizeBolivarRdamEcoMid 		map {capesize_Bolivar_Rdam_Eco_Mid.getOrCreate(asof) <-- _}
capesizeBolivarRdamFullMid 		map {capesize_Bolivar_Rdam_Full_Mid.getOrCreate(asof) <-- _}
panamaxDrummondItaquiFullMid 	map {panamax_Drummond_Itaqui_Full_Mid.getOrCreate(asof) <-- _}
supramaxDrummondItaquiFullMid 	map {supramax_Drummond_Itaqui_Full_Mid.getOrCreate(asof) <-- _}
panamaxDrummondPecemFullMid 	map {panamax_Drummond_Pecem_Full_Mid.getOrCreate(asof) <-- _}
capesizeDrummondRdamEcoMid 		map {capesize_Drummond_Rdam_Eco_Mid.getOrCreate(asof) <-- _}
capesizeDrummondRdamFullMid 	map {capesize_Drummond_Rdam_Full_Mid.getOrCreate(asof) <-- _}
panamaxIzmirRdamDiffMid 		map {panamax_Izmir_Rdam_Diff_Mid.getOrCreate(asof) <-- _}
panamaxJorflasfarRdamDiffMid 	map {panamax_Jorflasfar_Rdam_Diff_Mid.getOrCreate(asof) <-- _}
panamaxMurmanskRdamFullMid 		map {panamax_Murmansk_Rdam_Full_Mid.getOrCreate(asof) <-- _}
capesizeNacalaRbctDiffMid 		map {capesize_Nacala_Rbct_Diff_Mid.getOrCreate(asof) <-- _}
panamaxNewcastleMatsuuraFullMid map {panamax_Newcastle_Matsuura_Full_Mid.getOrCreate(asof) <-- _}
panamaxNewcastleReihokuFullMid 	map {panamax_Newcastle_Reihoku_Full_Mid.getOrCreate(asof) <-- _}
panamaxRigaRdamFullMid 			map {panamax_Riga_Rdam_Full_Mid.getOrCreate(asof) <-- _}
capesizeUsecRdamEcoMid 			map {capesize_Usec_Rdam_Eco_Mid.getOrCreate(asof) <-- _}
capesizeUsecRdamFullMid 		map {capesize_Usec_Rdam_Full_Mid.getOrCreate(asof) <-- _}
panamaxUsecRdamFullMid 			map {panamax_Usec_Rdam_Full_Mid.getOrCreate(asof) <-- _}
capesizeUsgcRdamEcoMid 			map {capesize_Usgc_Rdam_Eco_Mid.getOrCreate(asof) <-- _}
capesizeUsgcRdamFullMid 		map {capesize_Usgc_Rdam_Full_Mid.getOrCreate(asof) <-- _}
panamaxUsgcRdamFullMid 			map {panamax_Usgc_Rdam_Full_Mid.getOrCreate(asof) <-- _}
panamaxUstlugaRdamFullMid 		map {panamax_Ustluga_Rdam_Full_Mid.getOrCreate(asof) <-- _}
panamaxUswcJapanNewcastleDiffMid map {panamax_Uswc_Japan_Newcastle_Diff_Mid.getOrCreate(asof) <-- _}
panamaxVysotskRdamFullMid 		map {panamax_Vysotsk_Rdam_Full_Mid.getOrCreate(asof) <-- _}
panamaxVysotskRdamTransMid 		map {panamax_Vysotsk_Rdam_Trans_Mid.getOrCreate(asof) <-- _}
capesizeZonguldakRdamDiffMid 	map {capesize_Zonguldak_Rdam_Diff_Mid.getOrCreate(asof) <-- _}