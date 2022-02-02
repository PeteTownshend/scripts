import scala.math.pow

def tryOption[T](t: => T): Option[T] = try Some(t) catch {

      case t: Throwable =>
            log error t.getMessage
            None
}

implicit val svIn = new Service("script", Some(ds), Some(cds)) with Containers with ForwardCurveService with Markets with Lim2
val svOut = svIn
implicit val wb = FO_SUPPORT
val asof = yesterday

val histStart = Day(Year(asof) - 5)
val fwdEnd = asof.year + 4
val fwdEndDay = Day(fwdEnd, 12, 31)
implicit def limIter = LimIterator(asof iterator histStart)
def limForecastIter = LimIterator((Year(asof) - 4 iterator (Year(asof) + 6)) map (Day(_)))

val thermKWh = 29.3071

val omit = Interpolate.omit[Day, Double] _
val flatRight: Series[Day, Option[Double]] => Series[Day, Double] = os => Interpolate.flatRight(os)

val monthly = omit andThen (change(_) to Month)
val quarterly = omit andThen (change(_) to Quarter)
val yearly = omit andThen (change(_) to Year)

def toDay[A <: DateTimeLike[A]](series: Series[A, Double]) = change(series) to Day
def toMonth[A <: DateTimeLike[A]](series: Series[A, Double]) = change(series) to Month

//def toQuarterToMonth[A <: DateTimeLike[A]](series: Series[A, Double])  = ((change(_:Series[A, Double]) to Quarter) andThen toMonth)(series)
val toQuarterToMonth = (change(_:Series[Month, Double]) to Quarter) andThen toMonth

def concatenate[T<:DateTimeLike[T]](
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

// #############################################################################
// ### MISSING SYMBOLS
object AAJUS00 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object AAUQC03 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object AAVJJ03 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object AAYWS03 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object AAYWT00 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object AAYWT03 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object BAFA extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object ECBGBP extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object ECBUSD extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object GPVTD00 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object PCAAS03 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object PUAAJ03 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object PUAAL03 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object PUAAP00 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object PUAAP03 extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object CBSLABORELECGASWATERSUPPLYCAOWAGESPERMONTHINCLSPECIALPAYMENTSTOTAL2000 extends Symbol[String] { lazy val symbol = "CBS.LABOR.ELEC.GAS.WATER.SUPPLY.CAO.WAGES.PER.MONTH.INCL.SPECIAL.PAYMENTS.TOTAL.2000" }
object CBSPPINLBYEACMANUFACTURINGMONTHLY2010 extends Symbol[String] { lazy val symbol = "CBS.PPINL.BY.EA.C.MANUFACTURING.MONTHLY.2010" }
object CBSPPINLBYPDT25FABRICATEDMETALPRODUCTSEXMACHINERYEQUIPMENTMONTHLY2010 extends Symbol[String] { lazy val symbol = "CBS.PPINL.BY.PDT.25.FABRICATED.METAL.PRODUCTS.EX.MACHINERY.EQUIPMENT.MONTHLY.2010" }
object CBSPPINLBYPDT2711ELECTRICMOTORSGENERATORSTRANSFORMERSMONTHLY2010 extends Symbol[String] { lazy val symbol = "CBS.PPINL.BY.PDT.2711.ELECTRIC.MOTORS.GENERATORS.TRANSFORMERS.MONTHLY.2010" }
object CBSCPINLNETHERLAND_2006 extends Symbol[String] { lazy val symbol = "CBS.CPINL.NETHERLAND_2006" }
object EEXGPLDRPDELIVDATE extends Symbol[String] { lazy val symbol = "EEX.GPL.DRP.DELIV.DATE" }
object EEXNCGDRPDELIVDATE extends Symbol[String] { lazy val symbol = "EEX.NCG.DRP.DELIV.DATE" }
object EEXTTFDRPDELIVDATE extends Symbol[String] { lazy val symbol = "EEX.TTF.DRP.DELIV.DATE" }
object EGIXINDEXMONTHGASPOOL extends Symbol[String] { lazy val symbol = "EGIX.INDEX.MONTH.GASPOOL" }
object EGIXINDEXMONTHNCG extends Symbol[String] { lazy val symbol = "EGIX.INDEX.MONTH.NCG" }
object ESGMBAUM extends Symbol[String] { lazy val symbol = "ESGM.BAUM" }
object ESGMBEB extends Symbol[String] { lazy val symbol = "ESGM.BEB" }
object ESGMEGT extends Symbol[String] { lazy val symbol = "ESGM.EGT" }
object ESGMNBP extends Symbol[String] { lazy val symbol = "ESGM.NBP" }
object ESGMTTF extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object HERENCEGHMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.CEGH.MTH.INDEX" }
object HERENGASPOOLMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.GASPOOL.MTH.INDEX" }
object HERENNBPMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.NBP.MTH.INDEX" }
object HERENNCGDAILYMONTHAHEADINDEX extends Symbol[String] { lazy val symbol = "HEREN.NCG.DAILY.MONTH.AHEAD.INDEX" }
object HERENNCGMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.NCG.MTH.INDEX" }
object HERENPEGNORDMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.PEGNORD.MTH.INDEX" }
object HERENPSVMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.PSV.MTH.INDEX" }
object HERENTTFDAILYMONTHAHEADINDEX extends Symbol[String] { lazy val symbol = "HEREN.TTF.DAILY.MONTH.AHEAD.INDEX" }
object HERENTTFMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.TTF.MTH.INDEX" }
object HERENZEEBRUGGEMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.ZEEBRUGGE.MTH.INDEX" }
object HERINXEGTDAYACUMULATIVE extends Symbol[String] { lazy val symbol = "HERINX.EGT.DAYA.CUMULATIVE" }
object IHSCPINETHERLANDSYEARLY extends Symbol[String] { lazy val symbol = "IHS.CPI.NETHERLANDS.YEARLY" }
object IHSJULCGERMANYYEARLY extends Symbol[String] { lazy val symbol = "IHS.JULC.GERMANY.YEARLY" }
object IHSPPIGERMANYYEARLY extends Symbol[String] { lazy val symbol = "IHS.PPI.GERMANY.YEARLY" }
object IPENBP extends Symbol[String] { lazy val symbol = "IPE.NBP" }
object LEBADAYAHEADEGTINDEX extends Symbol[String] { lazy val symbol = "LEBA.DAYAHEAD.EGT.INDEX" }
object LEBADAYAHEADTTFINDEX extends Symbol[String] { lazy val symbol = "LEBA.DAYAHEAD.TTF.INDEX" }
object LEBADAYAHEADWINDOWEGTINDEX extends Symbol[String] { lazy val symbol = "LEBA.DAYAHEADWINDOW.EGT.INDEX" }
object LEBAMONTHAHEADEGTINDEX extends Symbol[String] { lazy val symbol = "LEBA.MONTHAHEAD.EGT.INDEX" }
object NORDGASNORDICDAYAHEAD extends Symbol[String] { lazy val symbol = "NORD.GAS.NORDIC.DAYAHEAD" }
object PA000214000 extends Symbol[String] { lazy val symbol = "PA0002140.0.0" }
object PA000325400 extends Symbol[String] { lazy val symbol = "PA0003254.0.0" }
object PA000333461 extends Symbol[String] { lazy val symbol = "PA0003334.6.1" }
object PA000333661 extends Symbol[String] { lazy val symbol = "PA0003336.6.1" }
object STATSBUNDEAGREEDHOURLYEARNINGSEXCLBONUSENERGYSUPPLY2010 extends Symbol[String] { lazy val symbol = "STATS.BUNDE.AGREED.HOURLY.EARNINGS.EXCL.BONUS.ENERGY.SUPPLY.2010" }
object STATSBUNDEHEAVYFUELOILSUPPLYDEUTSCHLAND extends Symbol[String] { lazy val symbol = "STATS.BUNDE.HEAVY.FUELOIL.SUPPLY.DEUTSCHLAND" }
object STATSBUNDELIGHTFUELOILCONSUMERDUSSELDORF extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.CONSUMER.DUSSELDORF" }
object STATSBUNDELIGHTFUELOILCONSUMERMANNHEIMLUDWIGSHAFEN extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.CONSUMER.MANNHEIM.LUDWIGSHAFEN" }
object STATSBUNDELIGHTFUELOILCONSUMERFRANKFURTAMMAIN extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.CONSUMER.FRANKFURT.AM.MAIN" }
object STATSBUNDELIGHTFUELOILCONSUMERRHEINSCHIENE extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.CONSUMER.RHEINSCHIENE" }
object STATSBUNDELIGHTFUELOILSUPPLYSTOCKRHEINSCHIENE extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.SUPPLY.STOCK.RHEINSCHIENE" }
object STATSBUNDENEGOTIATEDMONTHLYEARNINGSEXCLBONUSENERGYSUPPLY2010 extends Symbol[String] { lazy val symbol = "STATS.BUNDE.NEGOTIATED.MONTHLY.EARNINGS.EXCL.BONUS.ENERGY.SUPPLY.2010" }
object STATSBUNDEPRODUCTSCAPITALGOODSBASE2010 extends Symbol[String] { lazy val symbol = "STATS.BUNDE.PRODUCTS.CAPITALGOODS.BASE.2010" }
object STATSBUNDEWHOLESALEOVERALLINDEX2010 extends Symbol[String] { lazy val symbol = "STATS.BUNDE.WHOLESALE.OVERALL.INDEX.2010" }
object ECBDKK extends Symbol[String] { lazy val symbol = "ECBDKK" }
object ECBHUF extends Symbol[String] { lazy val symbol = "ECBHUF" }
object BAFAMONTHLYEVOLUTIONBORDERPRICE extends Symbol[String] { lazy val symbol = "BAFA.MONTHLY.EVOLUTION.BORDER.PRICE" }
object HERENESGMCEGHSPOTWEEKENDINDEX extends Symbol[String] { lazy val symbol = "HEREN.ESGM.CEGH.SPOT.WEEKEND.INDEX" }
object PA000169660 extends Symbol[String] { lazy val symbol = "PA0001696.6.0" }
object PA000273761 extends Symbol[String] { lazy val symbol = "PA0002737.6.1" }
object PA000169760 extends Symbol[String] { lazy val symbol = "PA0001697.6.0" }
object EGIXINDEXDAYGASPOOL extends Symbol[String] { lazy val symbol = "EGIX.INDEX.DAY.GASPOOL" }
object EEXBEB_MTH2 extends Symbol[String] { lazy val symbol = "EEX.BEB_MTH2" }
object EEXBEB_Q1 extends Symbol[String] { lazy val symbol = "EEX.BEB_Q1" }
object EEXBEB_Q2 extends Symbol[String] { lazy val symbol = "EEX.BEB_Q2" }
object EEXBEB_Q3 extends Symbol[String] { lazy val symbol = "EEX.BEB_Q3" }
object EEXBEB_Q4 extends Symbol[String] { lazy val symbol = "EEX.BEB_Q4" }
object EEXBEB_SE1 extends Symbol[String] { lazy val symbol = "EEX.BEB_SE1" }
object EEXBEB_SE2 extends Symbol[String] { lazy val symbol = "EEX.BEB_SE2" }
object EEXGPLMW01WDTRADEDAYAHEAD_01 extends Symbol[String] { lazy val symbol = "EEX.GPL.MW01.WD.TRADE.DAYAHEAD_01" }
object EEXGPLMW01WDTRADEDAYAHEAD_02 extends Symbol[String] { lazy val symbol = "EEX.GPL.MW01.WD.TRADE.DAYAHEAD_02" }
object EEXBEB_YR1 extends Symbol[String] { lazy val symbol = "EEX.BEB_YR1" }
object HERINXGASPOOLDAI extends Symbol[String] { lazy val symbol = "HERINX.GASPOOL.DAI" }
object HERENESGMGASPOOLSPOTWEEKENDINDEX extends Symbol[String] { lazy val symbol = "HEREN.ESGM.GASPOOL.SPOT.WEEKEND.INDEX" }
object GBBTD00 extends Symbol[String] { lazy val symbol = "GBBTD00" }
object ICEOTCM extends Symbol[String] { lazy val symbol = "ICEOTC.M" }
object PA000699261 extends Symbol[String] { lazy val symbol = "PA0006992.6.1" }
object EGIXINDEXDAYNCG extends Symbol[String] { lazy val symbol = "EGIX.INDEX.DAY.NCG" }
object EEXEGT_MTH2 extends Symbol[String] { lazy val symbol = "EEX.EGT_MTH2" }
object EEXEGT_Q1 extends Symbol[String] { lazy val symbol = "EEX.EGT_Q1" }
object EEXEGT_Q2 extends Symbol[String] { lazy val symbol = "EEX.EGT_Q2" }
object EEXEGT_Q3 extends Symbol[String] { lazy val symbol = "EEX.EGT_Q3" }
object EEXEGT_Q4 extends Symbol[String] { lazy val symbol = "EEX.EGT_Q4" }
object EEXEGT_SE1 extends Symbol[String] { lazy val symbol = "EEX.EGT_SE1" }
object EEXEGT_SE2 extends Symbol[String] { lazy val symbol = "EEX.EGT_SE2" }
object EEXNCGMW01WDTRADEDAYAHEAD_01 extends Symbol[String] { lazy val symbol = "EEX.NCG.MW01.WD.TRADE.DAYAHEAD_01" }
object EEXNCGMW01WDTRADEDAYAHEAD_02 extends Symbol[String] { lazy val symbol = "EEX.NCG.MW01.WD.TRADE.DAYAHEAD_02" }
object EEXEGT_YR1 extends Symbol[String] { lazy val symbol = "EEX.EGT_YR1" }
object EEXEGT_YR2 extends Symbol[String] { lazy val symbol = "EEX.EGT_YR2" }
object HERINXEGTDAYA extends Symbol[String] { lazy val symbol = "HERINX.EGT.DAYA" }
object HERENESGMNCGSPOTWEEKENDINDEX extends Symbol[String] { lazy val symbol = "HEREN.ESGM.NCG.SPOT.WEEKEND.INDEX" }
object LEBAWEEKENDWINDOWEGTINDEX extends Symbol[String] { lazy val symbol = "LEBA.WEEKENDWINDOW.EGT.INDEX" }
object GERTM00 extends Symbol[String] { lazy val symbol = "GERTM00" }
object NORDGASNORDICWITHINDAY extends Symbol[String] { lazy val symbol = "NORD.GAS.NORDIC.WITHINDAY" }
object ESGMPEG extends Symbol[String] { lazy val symbol = "ESGM.PEG" }
object HERENESGMPSV_DA extends Symbol[String] { lazy val symbol = "HEREN.ESGM.PSV_DA" }
object APXTTFENDOFWORKINGDAYDAFLOW extends Symbol[String] { lazy val symbol = "APX.TTF.END.OF.WORKING.DAY.DA.FLOW" }
object PA000304060 extends Symbol[String] { lazy val symbol = "PA0003040.6.0" }
object ICETTF extends Symbol[String] { lazy val symbol = "ICE.TTF" }
object ICETTF_Q1 extends Symbol[String] { lazy val symbol = "ICE.TTF_Q1" }
object ICETTF_Q2 extends Symbol[String] { lazy val symbol = "ICE.TTF_Q2" }
object ICETTF_Q3 extends Symbol[String] { lazy val symbol = "ICE.TTF_Q3" }
object ICETTF_Q4 extends Symbol[String] { lazy val symbol = "ICE.TTF_Q4" }
object ICETTF_SUM1 extends Symbol[String] { lazy val symbol = "ICE.TTF_SUM1" }
object ICETTF_WIN1 extends Symbol[String] { lazy val symbol = "ICE.TTF_WIN1" }
object ICETTF_YR1 extends Symbol[String] { lazy val symbol = "ICE.TTF_YR1" }
object HERINXTTFDAYACUMULATIVE extends Symbol[String] { lazy val symbol = "HERINX.TTF.DAYA.CUMULATIVE" }
object HERINXTTFDAYA extends Symbol[String] { lazy val symbol = "HERINX.TTF.DAYA" }
object HERINXTTFWEND extends Symbol[String] { lazy val symbol = "HERINX.TTF.WEND" }
object LEBADAYAHEADWINDOWTTFINDEX extends Symbol[String] { lazy val symbol = "LEBA.DAYAHEADWINDOW.TTF.INDEX" }
object LEBAMONTHAHEADTTFINDEX extends Symbol[String] { lazy val symbol = "LEBA.MONTHAHEAD.TTF.INDEX" }
object PA000333360 extends Symbol[String] { lazy val symbol = "PA0003333.6.0" }
object AADOS00 extends Symbol[String] { lazy val symbol = "AADOS00" }
object STATSBUNDELIGHTFUELOILSUPPLYSTOCKDUSSELDORF extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.SUPPLY.STOCK.DUSSELDORF" }
object STATSBUNDELIGHTFUELOILSUPPLYSTOCKFRANKFURTAMMAIN extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.SUPPLY.STOCK.FRANKFURT.AM.MAIN" }
object STATSBUNDELIGHTFUELOILSUPPLYSTOCKHAMBURG extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.SUPPLY.STOCK.HAMBURG" }
object STATSBUNDELIGHTFUELOILCONSUMERHAMBURG extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.CONSUMER.HAMBURG" }
object STATSBUNDELIGHTFUELOILSUPPLYSTOCKMANNHEIMLUDWIGSHAFEN extends Symbol[String] { lazy val symbol = "STATS.BUNDE.LIGHT.FUELOIL.SUPPLY.STOCK.MANNHEIM.LUDWIGSHAFEN" }
object EEXEGT_MTH1 extends Symbol[String] { lazy val symbol = "EEX.EGT_MTH1" }
object EEXBEB_MTH1 extends Symbol[String] { lazy val symbol = "EEX.BEB_MTH1" }
object ECBCZK extends Symbol[String] { lazy val symbol = "ECBCZK" }
object ICEULS extends Symbol[String] { lazy val symbol = "ICE.ULS" }
object NG2 extends eet.io.Symbol[String] { lazy val symbol = "NG" }
object DANSKEBANKEXCHRATEMTHAVGEUR extends eet.io.Symbol[String] { lazy val symbol = "DANSKEBANK.EXCHRATE.MTHAVG.EUR" }
object DANSKEBANKEXCHRATEMTHAVGUSD extends eet.io.Symbol[String] { lazy val symbol = "DANSKEBANK.EXCHRATE.MTHAVG.USD" }
object PA000303960_2 extends eet.io.Symbol[String] { lazy val symbol = "PA0003039.6.0" }
object PA000304161_2 extends eet.io.Symbol[String] { lazy val symbol = "PA0003041.6.1" }

object CEGHBAUMGARTENAT0000A0DGB9SPOT extends eet.io.Symbol[String] { lazy val symbol = "CEGH.BAUMGARTEN.AT0000A0DGB9.SPOT" }
object CEGHBAUMGARTENAT0000A0DGC7SPOT extends eet.io.Symbol[String] { lazy val symbol = "CEGH.BAUMGARTEN.AT0000A0DGC7.SPOT" }
object CEGHBAUMGARTENAT0000A0DGD5SPOT extends eet.io.Symbol[String] { lazy val symbol = "CEGH.BAUMGARTEN.AT0000A0DGD5.SPOT" }
object CEGHBAUMGARTENAT0000A0DGE3SPOT extends eet.io.Symbol[String] { lazy val symbol = "CEGH.BAUMGARTEN.AT0000A0DGE3.SPOT" }
object CEGHBAUMGARTENAT0000A0DGF0SPOT extends eet.io.Symbol[String] { lazy val symbol = "CEGH.BAUMGARTEN.AT0000A0DGF0.SPOT" }
object CEGHBAUMGARTENAT0000A0PQE6SPOT extends eet.io.Symbol[String] { lazy val symbol = "CEGH.BAUMGARTEN.AT0000A0PQE6.SPOT" }
object PNXTGASGPLTRADEDATEWE extends eet.io.Symbol[String] { lazy val symbol = "PNXT.GAS.GPL.TRADEDATE.WE" }
object PNXTGASNCGTRADEDATEWE extends eet.io.Symbol[String] { lazy val symbol = "PNXT.GAS.NCG.TRADEDATE.WE" }
object PNXTGASTTFTRADEDATEWE extends eet.io.Symbol[String] { lazy val symbol = "PNXT.GAS.TTF.TRADEDATE.WE" }
object PNXTGASNCGDA extends eet.io.Symbol[String] { lazy val symbol = "PNXT.GAS.NCG.DA" }
object PNXTGASGPLDA extends eet.io.Symbol[String] { lazy val symbol = "PNXT.GAS.GPL.DA" }
object PNXTGASTTFDA extends eet.io.Symbol[String] { lazy val symbol = "PNXT.GAS.TTF.DA" }
//quoted up until end of Oct 2015
object GASPOOLCOMPENSATIONENERGYPRICESPRICEFORNEGATIVECOMPENSATIONENERGY extends eet.io.Symbol[String] { lazy val symbol = "GASPOOL.COMPENSATION.ENERGYPRICES.PRICE.FOR.NEGATIVE.COMPENSATION.ENERGY" }
object GASPOOLCOMPENSATIONENERGYPRICESPRICEFORPOSITIVECOMPENSATIONENERGY extends eet.io.Symbol[String] { lazy val symbol = "GASPOOL.COMPENSATION.ENERGYPRICES.PRICE.FOR.POSITIVE.COMPENSATION.ENERGY" }
//quoted starting on Oct 1st 2015
object GASPOOLCOMPENSATIONENERGYPRICESNEGATIVEASOFOCT2015 extends eet.io.Symbol[String] { lazy val symbol = "GASPOOL.COMPENSATION.ENERGYPRICES.NEGATIVE.ASOF.OCT.2015" }
object GASPOOLCOMPENSATIONENERGYPRICESPOSITIVEASOFOCT2015 extends eet.io.Symbol[String] { lazy val symbol = "GASPOOL.COMPENSATION.ENERGYPRICES.POSITIVE.ASOF.OCT.2015" }
object PUAAK03 extends eet.io.Symbol[String] { lazy val symbol = "PUAAK03" }
object PUABC03 extends eet.io.Symbol[String] { lazy val symbol = "PUABC03" }
object PUAAZ03 extends eet.io.Symbol[String] { lazy val symbol = "PUAAZ03" }
object AAVJI03 extends eet.io.Symbol[String] { lazy val symbol = "AAVJI03" }
object AAYWR03 extends eet.io.Symbol[String] { lazy val symbol = "AAYWR03" }
object PUAAM03 extends eet.io.Symbol[String] { lazy val symbol = "PUAAM03" }
object STATSBUNDEPRODUCTSCAPITALGOODSTOTALBASE2010 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.PRODUCTS.CAPITALGOODS.TOTAL.BASE.2010" }
object STATSBUNDEPRODUCTSCAPITALGOODSPOWERENERGYSUPPLYSERVICEBASE2010_ANNUAL extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.PRODUCTS.CAPITALGOODS.POWER.ENERGY.SUPPLY.SERVICE.BASE.2010_ANNUAL" }
object STATSSTATSBUNDENEGOTIATEDANNUALEARNINGSENERGYSUPPLYBASE2010 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.NEGOTIATED.ANNUAL.EARNINGS.ENERGY.SUPPLY.BASE.2010" }
object STATSBUNDEAGREEDHOURLYEARNINGSPRIVATESECTORBASE2010 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.AGREED.HOURLY.EARNINGS.PRIVATE.SECTOR.BASE.2010" }
object STATSBUNDEAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2010 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.AGREED.HOURLY.EARNINGS.ENERGY.WATER.RECYCLING.BASE.2010" }
object STATSBUNDEFBGAGREEDHOURLYEARNINGSMININGBASE2010 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.FBG.AGREED.HOURLY.EARNINGS.MINING.BASE.2010" }
object STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2010 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.FBG.AGREED.HOURLY.EARNINGS.ENERGY.WATER.RECYCLING.BASE.2010" }
object STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYSUPPLYBASE2010 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.FBG.AGREED.HOURLY.EARNINGS.ENERGY.SUPPLY.BASE.2010" }
object STATSBUNDEAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2015 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.AGREED.HOURLY.EARNINGS.ENERGY.WATER.RECYCLING.BASE.2015" }
object STATSBUNDEFBGAGREEDHOURLYEARNINGSMININGBASE2015 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.FBG.AGREED.HOURLY.EARNINGS.MINING.BASE.2015" }
object STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2015 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.FBG.AGREED.HOURLY.EARNINGS.ENERGY.WATER.RECYCLING.BASE.2015" }
object STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYSUPPLYBASE2015 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.FBG.AGREED.HOURLY.EARNINGS.ENERGY.SUPPLY.BASE.2015" }
object STATSBUNDENEGOTIATEDANNUALEARNINGSENERGYSUPPLY2015 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.NEGOTIATED.ANNUAL.EARNINGS.ENERGY.SUPPLY.2015" }
object STATSBUNDENEGOTIATEDMONTHLYEARNINGSEXCLBONUSENERGYSUPPLY2015 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.NEGOTIATED.MONTHLY.EARNINGS.EXCL.BONUS.ENERGY.SUPPLY.2015" }
object STATSBUNDEAGREEDHOURLYEARNINGSEXCLBONUSENERGYSUPPLY2015 extends eet.io.Symbol[String] { lazy val symbol = "STATS.BUNDE.AGREED.HOURLY.EARNINGS.EXCL.BONUS.ENERGY.SUPPLY.2015" }

val Vwap = Numerical("Vwap")
val AveragePrice = Numerical("AveragePrice")
val ChangePct = Numerical("ChangePct")
val Close = Numerical("Close")
val CpIndex = Numerical("CpIndex")
val DomesticOutputPrices = Numerical("DomesticOutputPrices")
val FwdBid01mo = Numerical("FwdBid01mo")
val FwdBid1day = Numerical("FwdBid1day")
val FwdMid01cyr = Numerical("FwdMid01cyr")
val FwdMid01mo = Numerical("FwdMid01mo")
val FwdMid01qt = Numerical("FwdMid01qt")
val FwdMid01sn = Numerical("FwdMid01sn")
val FwdMid1day = Numerical("FwdMid1day")
val FwdMid1wkn = Numerical("FwdMid1wkn")
val FwdMidwknd = Numerical("FwdMidwknd")
val FwdOfr01mo = Numerical("FwdOfr01mo")
val FwdOfr1day = Numerical("FwdOfr1day")
val High = Numerical("High")
val Index = Numerical("Index")
val Low = Numerical("Low")
val MonthlyValue = Numerical("MonthlyValue")
val MthAvg = Numerical("MthAvg")
val RefPrice = Numerical("RefPrice")
val Spot = Numerical("Spot")
val Val = Numerical("Val")
val Value = Numerical("Value")
val WtdAverage = Numerical("WtdAverage")
val AnnualAvgPriceTJ = Numerical("AnnualAvgPriceTJ")
val PriceTJ = Numerical("PriceTJ")
val FwdOfr1wkn = Numerical("FwdOfr1wkn")
val FwdOfrwknd = Numerical("FwdOfrwknd")
val FwdBidwknd = Numerical("FwdBidwknd")
val FwdMid04qt = Numerical("FwdMid04qt")
val FwdOfr02sn = Numerical("FwdOfr02sn")
val FwdBid01sn = Numerical("FwdBid01sn")
val Bid = Numerical("Bid")
val Offer = Numerical("Offer")
val FwdOfr01yr = Numerical("FwdOfr01yr")
val FwdBid01yr = Numerical("FwdBid01yr")
val FwdMid01yr = Numerical("FwdMid01yr")
val FwdMid02sn = Numerical("FwdMid02sn")
val DerCPI = Numerical("DerCPI")
val FwdOfr01sn = Numerical("FwdOfr01sn")
val FwdBid02sn = Numerical("FwdBid02sn")

val Concatenated_IndexBrdStabuaGrosshandelVerkaufGesamtBasis2010Percent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_GROSSHANDEL_VERKAUF_GESAMT_BASIS2010_PERCENT_%s")
val Concatenated_FxEcbDkkMidDailyDkkEur = new svOut.Concatenated.ForwardCurveStream[Day, Day]("FX_ECB_DKK_MID_DAILY_DKK_EUR_%s")
val Concatenated_FxEcbGbpMidDailyGbpEur = new svOut.Concatenated.ForwardCurveStream[Day, Day]("FX_ECB_GBP_MID_DAILY_GBP_EUR_%s")
val Concatenated_GasCeghHerenWeekendIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_CEGH_HEREN_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasCeghHerenDaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_CEGH_HEREN_DA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasCeghHerenDaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_CEGH_HEREN_DA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasCeghHerenDaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_CEGH_HEREN_DA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasCeghHerenMaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_CEGH_HEREN_MA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasCeghHerenMaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_CEGH_HEREN_MA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasCeghHerenWeekendAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_CEGH_HEREN_WEEKEND_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasCeghHerenWeekendMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_CEGH_HEREN_WEEKEND_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplArgusDaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_DA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasGplArgusDaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_DA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplArgusMaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_MA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasGplArgusMaIndexBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_MA_INDEX_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplArgusMaIndexAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_MA_INDEX_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasGplArgusWeekendAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_WEEKEND_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasGplArgusWeekendBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_WEEKEND_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexEgixMaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_EGIX_MA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexEgixDaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_EGIX_DA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexQ01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexQ02MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q02_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexQ03MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q03_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexQ04MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q04_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexDa01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_DA01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexDa02MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_DA02_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexCyr01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenDaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_DA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenMaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_MA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenMaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_MA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenWeekendIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenWeekendAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WEEKEND_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenWeekendBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WEEKEND_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenCyr01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_CYR01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplPlattsDaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_Platts_DA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenDaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_DA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenDaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_DA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplHerenDaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_DA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNbpHerenMaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NBP_HEREN_MA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasNbpHerenMaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NBP_HEREN_MA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasNbpIceMaMidDailyGbppcTherm = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NBP_ICE_MA_MID_DAILY_GBPPC_THERM_%s")
val Concatenated_GasNcgArgusMaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_ARGUS_MA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenDayRefpriceDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_DAY_REFPRICE_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexEgixMaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_EGIX_MA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgPnxtDayRefpriceDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_PNXT_DAY_REFPRICE_DAILY_EUR_MWH_%s")
val Concatenated_GasGplPnxtDayRefpriceDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_PNXT_DAY_REFPRICE_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfPnxtDayRefpriceDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_PNXT_DAY_REFPRICE_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexEgixDaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_EGIX_DA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexQ01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexQ02MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q02_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexQ03MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q03_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexQ04MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q04_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexDa01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_DA01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexDa02MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_DA02_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexCyr01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexCyr02MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR02_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenDaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_DA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenDaIndexCumulativeDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_DA_INDEX_CUMULATIVE_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenDaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_DA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenDaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_DA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenMaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_MA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenMaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_MA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenMaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_MA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenQ01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_Q01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenQ04MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_Q04_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenSum01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenWin01AskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN01_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenWin01BidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN01_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenWin01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenWeekendIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenWeekendAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WEEKEND_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenWeekendBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WEEKEND_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgHerenCyrMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_CYR_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgLebaDaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_DA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgLebaDaWindowIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_DA_WINDOW_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgLebaMaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_MA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgLebaWeekendWindowIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_WEEKEND_WINDOW_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgPlattsMaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_Platts_MA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNptfGaspointNordicWithinDayDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NPTF_GASPOINT_NORDIC_WITHIN_DAY_DAILY_EUR_MWH_%s")
val Concatenated_GasPegnHerenDaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_PEGN_HEREN_DA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasPsvPlattsDaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_PSV_PLATTS_DA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfApxDaCloseDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_APX_DA_CLOSE_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfArgusDaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ARGUS_DA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfArgusDaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ARGUS_DA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfArgusMaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ARGUS_MA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfArgusWeekendAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ARGUS_WEEKEND_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfArgusWeekendBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ARGUS_WEEKEND_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfArgusMaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ARGUS_MA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfEex1mwDaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_EEX_1MW_DA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenDaIndexCumulativeDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_DA_INDEX_CUMULATIVE_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenDaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_DA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenDaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_DA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenDaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_DA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenGyr01AskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_GYR01_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenGyr01BidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_GYR01_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenGyr01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_GYR01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenMaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_MA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenMaAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_MA_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenMaBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_MA_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenQ01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_Q01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenQ04MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_Q04_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenSum01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenWin01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenWeekendIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenWeekendAskDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WEEKEND_ASK_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenWeekendBidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WEEKEND_BID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfHerenCyr01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_CYR01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfLebaDaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_DA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfLebaDaWindowIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_DA_WINDOW_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfLebaMaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_MA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasZeeArgusDaIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_ZEE_ARGUS_DA_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasZeePlattsMaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_ZEE_Platts_MA_MID_DAILY_EUR_MWH_%s")
val Concatenated_OilPlattsFo1FobAraAskDailyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Day]("OIL_PLATTS_FO_1_FOB_ARA_ASK_DAILY_USD_T_%s")
val Concatenated_OilPlattsFo1FobAraBidDailyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Day]("OIL_PLATTS_FO_1_FOB_ARA_BID_DAILY_USD_T_%s")
val Concatenated_OilPlattsFo1FobAraMidDailyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Day]("OIL_PLATTS_FO_1_FOB_ARA_MID_DAILY_USD_T_%s")
val Concatenated_OilPlattsGo01FobAraAskDailyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Day]("OIL_PLATTS_GO_0_1_FOB_ARA_ASK_DAILY_USD_T_%s")
val Concatenated_OilPlattsGo01FobAraBidDailyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Day]("OIL_PLATTS_GO_0_1_FOB_ARA_BID_DAILY_USD_T_%s")
val Concatenated_OilPlattsGo01FobAraMidDailyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Day]("OIL_PLATTS_GO_0_1_FOB_ARA_MID_DAILY_USD_T_%s")
val Concatenated_GasPsvHerenDaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_PSV_HEREN_DA_MID_DAILY_EUR_MWH_%s")
val Concatenated_OilPlattsBrentDatedAskMonthlyUsdBbl = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_BRENT_DATED_ASK_MONTHLY_USD_BBL_%s")
val Concatenated_OilPlattsBrentDatedBidMonthlyUsdBbl = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_BRENT_DATED_BID_MONTHLY_USD_BBL_%s")
val Concatenated_OilPlattsFo1CifMedAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_CIF_MED_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo1CifNweAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_CIF_NWE_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo1CifNweBidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_CIF_NWE_BID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo1FobAraAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_FOB_ARA_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo1CifMedBidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_CIF_MED_BID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo1FobMedAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_FOB_MED_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo1FobMedBidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_FOB_MED_BID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo1FobMedMidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_FOB_MED_MID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo1FobNweMidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_FOB_NWE_MID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo35FobAraAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_3_5_FOB_ARA_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo35FobAraBidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_3_5_FOB_ARA_BID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo35FobAraMidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_3_5_FOB_ARA_MID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo35FobMedAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_3_5_FOB_MED_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo35FobMedBidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_3_5_FOB_MED_BID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsFo35FobMedMidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_3_5_FOB_MED_MID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo01CifMedAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_0_1_CIF_MED_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo01CifMedBidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_0_1_CIF_MED_BID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo01CifNweAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_0_1_CIF_NWE_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo01CifNweBidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_0_1_CIF_NWE_BID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo01FobMedAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_0_1_FOB_MED_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo01FobMedBidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_0_1_FOB_MED_BID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo01FobMedMidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_0_1_FOB_MED_MID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo01FobNweMidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_0_1_FOB_NWE_MID_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo50PpmFobAraAskMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_50_PPM_FOB_ARA_ASK_MONTHLY_USD_T_%s")
val Concatenated_OilPlattsGo50PpmFobAraBidMonthlyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_50_PPM_FOB_ARA_BID_MONTHLY_USD_T_%s")
val Concatenated_FxEcbDkkMidMonthlyDkkEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("FX_ECB_DKK_MID_MONTHLY_DKK_EUR_%s")
val Concatenated_FxEcbHufMidMonthlyHufEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("FX_ECB_HUF_MID_MONTHLY_HUF_EUR_%s")
val Concatenated_FxEcbCzkMidMonthlyCzkEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("FX_ECB_CZK_MID_MONTHLY_CZK_EUR_%s")
val Concatenated_GasGplEexMaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_MA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexSum01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_SUM01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasGplEexWin01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_WIN01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexMaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_MA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexSum01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_SUM01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgEexWin01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_WIN01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfIceMaMidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_MA_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfIceQ01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_Q01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfIceQ02MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_Q02_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfIceQ03MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_Q03_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfIceQ04MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_Q04_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfIceSum01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_SUM01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfIceWin01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_WIN01_MID_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfIceCyr01MidDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_CYR01_MID_DAILY_EUR_MWH_%s")
val Concatenated_OilIceUlsMidDailyUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Day]("OIL_ICE_ULS_MID_DAILY_USD_T_%s")
val Concatenated_GasTtfLebaWeekendIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfLebaDaWeekendIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_DA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgLebaWeekendIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgLebaDaWeekendIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_DA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasGplLebaDaWeekendIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_GPL_LEBA_DA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasNcgLebaDaWeekendWindowIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_DA_WEEKEND_WINDOW_INDEX_DAILY_EUR_MWH_%s")
val Concatenated_GasTtfArgusMaMidMthEndMonthlyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_TTF_ARGUS_MA_MID_MTH_END_MONTHLY_EUR_MWH_%s")
val Concatenated_GasTtfHerenDaIndexMthEndShiftMonthlyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_TTF_HEREN_DA_INDEX_MTH_END_SHIFT_MONTHLY_EUR_MWH_%s")
val Concatenated_GasNbpIceMaMthEndMonthlyGbbpcTherm = new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_NBP_ICE_MA_MTH_END_MONTHLY_GBBPC_THERM_%s")
val Concatenated_GasNcgHerenMaIndexMthEndMonthlyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_NCG_HEREN_MA_INDEX_MTH_END_MONTHLY_EUR_MWH_%s")
val Concatenated_GasZeePlattsMaMthEndMonthlyGbbpcTherm = new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_ZEE_PLATTS_MA_MTH_END_MONTHLY_GBBPC_THERM_%s")
val Concatenated_GasBrdBafaBorderPriceMonthlyEurTj= new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_BRD_BAFA_BORDER_PRICE_MONTHLY_EUR_TJ_%s")
val Concatenated_GasBrdBafaBorderPriceYearlyEurTj= new svOut.Concatenated.ForwardCurveStream[Day, Year]("GAS_BRD_BAFA_BORDER_PRICE_YEARLY_EUR_TJ_%s")
val Concatenated_OilPlattsFo1FobAraMidEbv1MonthlyEurT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_FOB_ARA_MID_EBV1_MONTHLY_EUR_T_%s")
val Concatenated_OilPlattsFo1FobAraMidEbv2MonthlyEurT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_FO_1_FOB_ARA_MID_EBV2_MONTHLY_EUR_T_%s")
val Concatenated_OilPlattsGo01FobAraMidEbv1MonthlyEurT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_1_FOB_ARA_MID_EBV1_MONTHLY_EUR_T_%s")
val Concatenated_OilPlattsGo50PpmFobAraMidEbv1MonthlyEurT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_50_PPM_FOB_ARA_MID_EBV1_MONTHLY_EUR_T_%s")
val Concatenated_OilPlattsGo50PpmFobAraMidEbv2MonthlyEurT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_50_PPM_FOB_ARA_MID_EBV2_MONTHLY_EUR_T_%s")
val Concatenated_OilPlattsGo50PpmFobAraMidEbv3MonthlyEurT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_50_PPM_FOB_ARA_MID_EBV3_MONTHLY_EUR_T_%s")
val Concatenated_OilPlattsGo50PpmFobAraMidEbv4MonthlyEurT = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_PLATTS_GO_50_PPM_FOB_ARA_MID_EBV4_MONTHLY_EUR_T_%s")
val Concatenated_GasNbpIcePenultMaAvg8decdigitsMonthlyGbbpcTherm = new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_NBP_ICE_PENULT_MA_AVG_8DECDIGITS_MONTHLY_GBBPC_THERM_%s")
val Concatenated_GasNbpIcePenultMaAvg3decdigitsMonthlyGbbpcTherm = new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_NBP_ICE_PENULT_MA_AVG_3DECDIGITS_MONTHLY_GBBPC_THERM_%s")
val Concatenated_GasHhubNymexMaMidMonthlyUsdMmbtu = new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_HHUB_NYMEX_MA_MID_MONTHLY_USD_MMBTU_%s")
val Concatenated_IndexNlCbsCpiDerivedBasis2006Percent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_NL_CBS_CPI_DERIVED_BASIS2006_PERCENT_%s")
val Concatenated_CoalArgusApi2QuarterlyMidUsdT = new svOut.Concatenated.ForwardCurveStream[Day, Quarter]("COAL_ARGUS_API2_QUARTERLY_MID_USD_T_%s")
val Concatenated_FxDanskebankDkkoMidMonthlyDkkoEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("FX_DANSKEBANK_DKKO_MID_MONTHLY_DKKO_EUR_%s")
val Concatenated_FxDanskebankDkkoMidMonthlyDkkoUsd = new svOut.Concatenated.ForwardCurveStream[Day, Month]("FX_DANSKEBANK_DKKO_MID_MONTHLY_DKKO_USD_%s")
val Concatenated_WagesAvGweKirchmoeser14thSalaryChristmasBonusEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_KIRCHMOESER_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val Concatenated_WagesAvGwe19014thSalaryChristmasBonusEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_1_90_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val Concatenated_WagesAvGwe18214thSalaryChristmasBonusEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_1_82_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val Concatenated_WagesAvGwe78014thSalaryChristmasBonusEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_7_80_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val Concatenated_WagesAvGwe78914thSalaryChristmasBonusEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_7_89_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val Concatenated_WagesAvGweAzv14thSalaryChristmasBonusEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_AZV_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val Concatenated_WagesAvGweSvCharge14thSalaryChristmasBonusEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_SV_CHARGE_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val Concatenated_WagesAvGwe77714thSalaryChristmasBonusEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_7_77_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val Concatenated_WagesAvGwe14thSalaryChristmasBonusEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val Concatenated_GasBrdBnetzaCompensationEnergyPositiveDailyEurcKwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_BRD_BNETZA_COMPENSATION_ENERGY_POSITIV_DAILY_EURC_KWH_%s")
val Concatenated_GasBrdBnetzaCompensationEnergyNegativeDailyEurcKwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_BRD_BNETZA_COMPENSATION_ENERGY_NEGATIVE_DAILY_EURC_KWH_%s")
val Concatenated_GasBrdBnetzaCompensationEnergyExceedingLowerQuantitiesMonthlyEurcKwh = new svOut.Concatenated.ForwardCurveStream[Day, Month]("GAS_BRD_BNETZA_COMPENSATION_ENERGY_EXCEEDING_LOWER_QUANTITIES_MONTHLY_EURC_KWH_%s")
val Concatenated_GasCeghCeghixIndexDailyEurMwh = new svOut.Concatenated.ForwardCurveStream[Day, Day]("GAS_CEGH_CEGHIX_INDEX_DAILY_EUR_MWH_%s")

//priliminary, for UAT
val GoEndurCurves = new svIn.Container("GO_ENDUR_CURVES") with svIn.ForwardCurveContainer
lazy val EonIndexBrdGewerblicheErzeugnisse = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_BRD_GEWERBLICHE_ERZEUGNISSE_%s").get(asof) flatMap { _ series }
lazy val EonIndexBrdGrosshandVerkauf2005 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_BRD_GROSSHAND_VERKAUF_2005_%s").get(asof) flatMap { _ series }
lazy val EonIndexBrdTarifMonatsgehEnergieversMf2010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_BRD_TARIF_MONATSGEH_ENERGIEVERS_MF_2010_%s").get(asof) flatMap { _ series }
lazy val EonIndexBrdTarifStundenEnergievers2010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_BRD_TARIF_STUNDEN_ENERGIEVERS_2010_%s").get(asof) flatMap { _ series }
lazy val EonIndexNlConsumerPriceNder2006 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_CONSUMER_PRICE_NDER_2006_%s").get(asof) flatMap { _ series }
lazy val EonIndexNlPrijsCaoIonenEnergie2000 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_PRIJS_CAO_IONEN_ENERGIE_2000_%s").get(asof) flatMap { _ series }
lazy val EonIndexNlProducerPriceProdcom252010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_PRODUCER_PRICE_PRODCOM_25_2010_%s").get(asof) flatMap { _ series }
lazy val EonIndexNlProducerPriceProdcom27112010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_PRODUCER_PRICE_PRODCOM_2711_2010_%s").get(asof) flatMap { _ series }
lazy val EonIndexNlProducerPriceSbi20082010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_PRODUCER_PRICE_SBI2008_2010_%s").get(asof) flatMap { _ series }
lazy val wagesAvGweKirchmoeser14thSalaryChristmasBonusEurHist = new svIn.GoEndurCurves.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_KIRCHMOESER_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST").get(asof) flatMap { _ series }
lazy val wagesAvGwe19014thSalaryChristmasBonusEurHist = new svIn.GoEndurCurves.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_1_90_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST").get(asof) flatMap { _ series }
lazy val GasTtfLebaWeekendIndexDailyEurMwhHist = new Rdmsmodels.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_WEEKEND_INDEX_DAILY_EUR_MWH_%s").get(asof) flatMap { _ series }
lazy val GasTtfLebaDaWeekendIndexDailyEurMwhHist = new Rdmsmodels.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_DA_WEEKEND_INDEX_DAILY_EUR_MWH_%s").get(asof) flatMap { _ series }
lazy val GasNcgLebaWeekendIndexDailyEurMwhHist = new Rdmsmodels.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_WEEKEND_INDEX_DAILY_EUR_MWH_%s").get(asof) flatMap { _ series }
lazy val GasNcgLebaDaWeekendIndexDailyEurMwhHist = new Rdmsmodels.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_DA_WEEKEND_INDEX_DAILY_EUR_MWH_%s").get(asof) flatMap { _ series }
lazy val GasGplLebaDaWeekendIndexDailyEurMwhHist = new Rdmsmodels.ForwardCurveStream[Day, Day]("GAS_GPL_LEBA_DA_WEEKEND_INDEX_DAILY_EUR_MWH_%s").get(asof) flatMap { _ series } map { _ from histStart }
lazy val GasNcgLebaDaWeekendWindowIndexDailyEurMwhHist = new Rdmsmodels.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_DA_WEEKEND_WINDOW_INDEX_DAILY_EUR_MWH_%s").get(asof) flatMap { _ series }
lazy val wagesAvGwe18214thSalaryChristmasBonusEurHist = new svIn.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_1_82_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST").get(asof) flatMap { _ series }
lazy val wagesAvGwe78014thSalaryChristmasBonusEurHist = new svIn.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_7_80_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST").get(asof) flatMap { _ series }
lazy val wagesAvGwe78914thSalaryChristmasBonusEurHist = new svIn.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_7_89_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST").get(asof) flatMap { _ series }
lazy val wagesAvGwe14thSalaryChristmasBonusEurHist = new svIn.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST").get(asof) flatMap { _ series }
lazy val wagesAvGweSvCharge14thSalaryChristmasBonusEurHist = new svIn.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_SV_CHARGE_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST").get(asof) flatMap { _ series }
lazy val wagesAvGwe77714thSalaryChristmasBonusEurHist = new svIn.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_7_77_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST").get(asof) flatMap { _ series }
lazy val wagesAvGweAzv14thSalaryChristmasBonusEurHist = new svIn.Concatenated.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_AZV_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST").get(asof) flatMap { _ series }
lazy val wagesTvOedKommunallohn54MonthEurHist = new svIn.Concatenated.ForwardCurveStream[Day, Month]("WAGES_TV_OED_KOMMUNALLOHN_5_4_MONTH_HIST_EUR").get(asof) flatMap { _ series }
lazy val wagesTvOedKommunallohn54HourEurHist = new svIn.Concatenated.ForwardCurveStream[Day, Month]("WAGES_TV_OED_KOMMUNALLOHN_5_4_HOUR_HIST_EUR").get(asof) flatMap { _ series }

val Concatenated_IndexBrdStabuaGewerblErzeugnisGesamtBasis2010MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_GEWERBL_ERZEUGNIS_GESAMT_BASIS2010_MONTHLY_PERCENT_%s")
val Concatenated_IndexBrdStabuaMonatsgehaltEnergieWasserBasis2010MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_MONATSGEHALT_ENERGIE_WASSER_BASIS2010_MONTHLY_PERCENT_%s")
val Concatenated_IndexBrdStabuaStundengehaltPrivatwirtschaftBasis2010MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_STUNDENGEHALT_PRIVATWIRTSCHAFT_BASIS2010_MONTHLY_PERCENT_%s")
val Concatenated_IndexBrdStabuaStundengehaltBergebauBasis2010MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_STUNDENGEHALT_BERGEBAU_BASIS2010_MONTHLY_PERCENT_%s")
val Concatenated_IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2010MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_STUNDENGEHALT_FBG_ENERGIE_WASSER_BASIS2010_MONTHLY_PERCENT_%s")
val Concatenated_IndexBrdStabuaStundengehaltFbgEnergieBasis2010MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_STUNDENGEHALT_FBG_ENERGIE_BASIS2010_MONTHLY_PERCENT_%s")
val Concatenated_WagesTvOedKommunallohn54MonthEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_TV_OED_KOMMUNALLOHN_5_4_MONTH_EUR_%s")
val Concatenated_WagesTvOedKommunallohn54HourEur = new svOut.Concatenated.ForwardCurveStream[Day, Month]("WAGES_TV_OED_KOMMUNALLOHN_5_4_HOUR_EUR_%s")
val Concatenated_OilStabuaHelFrank500TMonthlyEurHl = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_STABUA_HEL_FRANK_500_T_MONTHLY_EUR_HL_%s")
val Concatenated_OilStabuaHelHamburg500TMonthlyEurHl = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_STABUA_HEL_HAMBURG_500_T_MONTHLY_EUR_HL_%s")
val Concatenated_OilStabuaHelHamburg4050HlMonthlyEurHl = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_STABUA_HEL_HAMBURG_40_50_HL_MONTHLY_EUR_HL_%s")
val Concatenated_OilStabuaHelMannLud500TMonthlyEurHl = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_STABUA_HEL_MANN_LUD_500_T_MONTHLY_EUR_HL_%s")
val Concatenated_OilStabuaHelDuess500TMonthlyEurHl = new svOut.Concatenated.ForwardCurveStream[Day, Month]("OIL_STABUA_HEL_DUESS_500_T_MONTHLY_EUR_HL_%s")
val Concatenated_PlattsDailyWeightsMonthlyDays = new svOut.Concatenated.ForwardCurveStream[Day, Month]("PLATTS_DAILY_WEIGHTS_MONTHLY_DAYS_%s")

// NEW CURVES
val Concatenated_IndexBrdStabuaStundengehaltFbgEnergieBasis2015MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_STUNDENGEHALT_FBG_ENERGIE_BASIS2015_MONTHLY_PERCENT_%s")
val Concatenated_IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2015MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_STUNDENGEHALT_FBG_ENERGIE_WASSER_BASIS2015_MONTHLY_PERCENT_%s")
val Concatenated_IndexBrdStabuaStundengehaltBergebauBasis2015MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_STUNDENGEHALT_BERGEBAU_BASIS2015_MONTHLY_PERCENT_%s")
val Concatenated_IndexBrdStabuaMonatsgehaltEnergieWasserBasis2015MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STABUA_MONATSGEHALT_ENERGIE_WASSER_BASIS2015_MONTHLY_PERCENT_%s")
val Concatenated_IndexBrdJaehrlicherVerdienstEnergieVersorg2015MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_JAEHRLICHER_VERDIENST_ENERGIE_VERSORGUNG_2015_%s")
val Concatenated_IndexBrdMonatlicherVerdienstEnergieVersorg2015MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_MONATLICHER_VERDIENST_ENERGIE_VERSORGUNG_2015_%s")
val Concatenated_IndexBrdStuendlicherVerdienstEnergieVersorg2015MonthlyPercent = new svOut.Concatenated.ForwardCurveStream[Day, Month]("INDEX_BRD_STUENDLICHER_VERDIENST_ENERGIE_VERSORGUNG_2015_%s")

lazy val GasNbpIcePenultMaAvgMonthlyGbbpcThermHist = new Rdmsmodels.ForwardCurveStream[Day, Month]("GAS_NBP_ICE_PENULT_MA_AVG_MONTHLY_GBBPC_THERM_HIST_%s").get(asof) flatMap { _ series }
lazy val NgBafaFc = new svIn.Container("NG_BAFA_FC") with svIn.ForwardCurveContainer
lazy val Rdmsmodels = new svIn.Container("RDMSMODELS") with svIn.ForwardCurveContainer
lazy val EexContractDetails = svIn("EEX")("CONTRACT_DETAILS")
lazy val BafaGasMidFlat = new NgBafaFc.ForwardCurveStream[Day, Day]("MID_FLAT_%s")

//end of priliminary, for UAT

val BidAskSpreads = new svOut.Container("BID_ASK_SPREADS") with svOut.ForwardCurveContainer

//in
/**USD/T*/
lazy val go_0_1_fob_ara_usd= new BidAskSpreads.ForwardCurveStream[Day, Month]("GO_0_1_FOB_ARA_USD_%s").get(asof) flatMap { _ series }
/**EUR/MWH*/
lazy val gas_ncg_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_NCG_EUR_%s").get(asof) flatMap { _ series }
/**EUR/MWH*/
lazy val gas_pegn_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_PEGN_EUR_%s").get(asof) flatMap { _ series }
/**EUR/MWH*/
lazy val gas_cegh_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_CEGH_EUR_%s").get(asof) flatMap { _ series }
/**EUR/MWH*/
lazy val gas_ttf_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_TTF_EUR_%s").get(asof) flatMap { _ series }
/**EUR/MWH*/
lazy val gas_psv_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_PSV_EUR_%s").get(asof) flatMap { _ series }
/**USD/T*/
lazy val coal_api2_usd= new BidAskSpreads.ForwardCurveStream[Day, Month]("COAL_API2_USD_%s").get(asof) flatMap { _ series }
/**PENCE/THM*/
lazy val gas_nbp_pth= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_NBP_PTH_%s").get(asof) flatMap { _ series }
/**USD/T*/
lazy val fo_1_fob_ara_usd= new BidAskSpreads.ForwardCurveStream[Day, Month]("FO_1_FOB_ARA_USD_%s").get(asof) flatMap { _ series }
/**EUR/MWH*/
lazy val gas_gaspool_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_GASPOOL_EUR_%s").get(asof) flatMap { _ series }
/**PENCE/THM*/
lazy val gas_zee_pth= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_ZEE_PTH_%s").get(asof) flatMap { _ series }
/**USD/BBL*/
lazy val brent_dated_usd= new BidAskSpreads.ForwardCurveStream[Day, Month]("BRENT_DATED_USD_%s").get(asof) flatMap { _ series }

//out
val go_0_1_fob_ara_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("GO_0_1_FOB_ARA_EUR_%s")
val gas_ncg_eurc= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_NCG_EURC_%s")
val gas_pegn_eurc= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_PEGN_EURC_%s")
val gas_cegh_eurc= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_CEGH_EURC_%s")
val gas_ttf_eurc= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_TTF_EURC_%s")
val gas_psv_eurc= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_PSV_EURC_%s")
val coal_api2_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("COAL_API2_EUR_%s")
val gas_nbp_eurc= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_NBP_EURC_%s")
val fo_1_fob_ara_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("FO_1_FOB_ARA_EUR_%s")
val gas_gaspool_eurc= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_GASPOOL_EURC_%s")
val gas_zee_eurc= new BidAskSpreads.ForwardCurveStream[Day, Month]("GAS_ZEE_EURC_%s")
val brent_dated_eur= new BidAskSpreads.ForwardCurveStream[Day, Month]("BRENT_DATED_EUR_%s")

// ###############################################################################################
// ### INPUT
// ###

// FX
lazy val eurGbp = svIn.Reuters1730.EurGbpMid.get(asof) flatMap { _ series } map { _ roundTo 15 }
lazy val eurUsd =  svIn.Reuters1730.EurUsdMid.get(asof) flatMap { _ series } map { _ roundTo 14 }
lazy val eurDkk =  svIn.Reuters1730.EurDkkMid.get(asof) flatMap { _ series } map { _ roundTo 14 }
lazy val eurHuf =  svIn.Reuters1730.EurHufMid.get(asof) flatMap { _ series } map { _ roundTo 14 }
lazy val eurCzk =  svIn.Reuters1730.EurCzkMid.get(asof) flatMap { _ series } map { _ roundTo 14 }

// COAL
lazy val coalApi2 = svIn.GoEndurCurves.CoalApi2Usd.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val coalBafa = svIn.CoalEodFc.CoalBafaHistEur.get(asof) flatMap { _ series } map { _ roundTo 5 }

// GAS
lazy val baumgarten = svIn.NgOmvBaumgartenFc.Mid.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val gaspool = svIn.NgGaspoolFc.Mid.get(asof) flatMap { _ series } map { _ roundTo 5}
lazy val nbp = svIn.NgNbpFc.Mid.get(asof) flatMap { _ series } map { _ roundTo 13 }
lazy val ncg = svIn.NgNcgFc.Mid.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val pegNord = svIn.NgPegFc.NordMid.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val psv = svIn.NgPsvFc.Mid.get(asof) flatMap { _ series }
lazy val ttf = svIn.NgTtfFc.Mid.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val zee = svIn.NgZeeFc.Mid.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val nptf = svIn.NgNptfFc.Mid.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val hhub = svIn.Settlement.IceH.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val bafaGas = BafaGasMidFlat.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val zeeEur = svIn.NgZeeFc.MidEur.get(asof) flatMap { _ series } map { _ roundTo 5 }


// OIL
lazy val datedBrent = svIn.OilEodFc.DatedBrentSwap.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val fo1CifMedCargoes = svIn.OilEodFc.Fo1CifMedCargoes.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val fo1CifNweCargoes = svIn.OilEodFc.Fo1CifNweCargoes.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val fo1FobAraBarges = svIn.OilEodFc.Fo1FobAraBarges.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val go01CifMedCargoes = svIn.OilEodFc.Go01CifMedCargoes.get(asof) flatMap { _ series } map { _ roundTo 3 }
lazy val go01CifNweCargoes = svIn.OilEodFc.Go01CifNweCargoes.get(asof) flatMap { _ series } map { _ roundTo 15 }
lazy val go01FobAraBarges = svIn.OilEodFc.Go01FobAraBarges.get(asof) flatMap { _ series } map { _ roundTo 3 }
lazy val go50Ppm = svIn.OilEodFc.Go50PpmFobAraBarges.get(asof) flatMap { _ series } map { _ roundTo 3 }
lazy val hel = svIn.OilPtFc.HelRheinHlMid.get(asof) flatMap { _ series } map { _ roundTo 6 }
lazy val helDusHl = svIn.OilEodFc.HelDusHl.get(asof) flatMap { _ series } map { _ roundTo 6 }
lazy val helFraHl = svIn.OilEodFc.HelFraHl.get(asof) flatMap { _ series } map { _ roundTo 6 }
lazy val helMannLwghHl = svIn.OilEodFc.HelMannLwghHl.get(asof) flatMap { _ series } map { _ roundTo 6 }
lazy val helRhein500tHl = svIn.OilEodFc.HelRhein500tHl.get(asof) flatMap { _ series } map { _ roundTo 6 }
lazy val hsl = svIn.OilPtFc.HslDeuMid.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val ulsd10Ppm = svIn.GoEndurCurves.Ulsd10PpmFobAraBarges.get(asof) flatMap { _ series }
lazy val fo1FobMedCargoes = svIn.GoEndurCurves.Fo1FobMedCargoes.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val fo1FobNweCargoes = svIn.GoEndurCurves.Fo1FobNweCargoes.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val fo35FobAraBarges = svIn.OilEodFc.Fo35FobAraBarges.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val fo35FobMedCargoes = svIn.OilEodFc.Fo35FobMedCargoes.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val go01FobMedCargoes = svIn.OilEodFc.Go01FobMedCargoes.get(asof) flatMap { _ series } map { _ roundTo 5 }
lazy val go01FobNweCargoes = svIn.OilEodFc.Go01FobNweCargoes.get(asof) flatMap { _ series } map { _ roundTo 5 }

// EEX
lazy val contractDetails = EexContractDetails.dataTable map { table =>
      val iterator = table.iterator
      val builder = List.newBuilder[(String, Day)]
      while(iterator hasNext) {
            val row = iterator.next
            builder += row.getValue("PRODUCT_ID").toString -> Day(row.getValue("EXPIRY_DATE").toString)
      }
      builder.result.groupBy(_._1) map { pair =>
            val monthAndDate = pair._2 map { case (_, settlementDate) => Month(settlementDate) -> settlementDate }
            pair._1 -> monthAndDate.toMap
      }
}

// List(
//   svIn.GoEndurCurves.CoalApi2Usd,
//   svIn.CoalEodFc.CoalBafaHistEur,
//   svIn.GoEndurCurves.Ulsd10PpmFobAraBarges,
//   svIn.NgGaspoolFc.Mid,
//   svIn.NgNbpFc.Mid,
//   svIn.NgNcgFc.Mid,
//   svIn.NgOmvBaumgartenFc.Mid,
//   svIn.NgPegFc.NordMid,
//   svIn.NgPsvFc.Mid,
//   svIn.NgTtfFc.Mid,
//   svIn.NgZeeFc.Mid,
//   svIn.NgNptfFc.Mid,
//   svIn.OilEodFc.DatedBrentSwap,
//   svIn.OilEodFc.Fo1CifMedCargoes,
//   svIn.OilEodFc.Fo1CifNweCargoes,
//   svIn.OilEodFc.Fo1FobAraBarges,
//   svIn.OilEodFc.Go01CifMedCargoes,
//   svIn.OilEodFc.Go01CifNweCargoes,
//   svIn.OilEodFc.Go01FobAraBarges,
//   svIn.OilEodFc.Go50PpmFobAraBarges,
//   svIn.OilPtFc.HelRheinHlMid,
//   svIn.OilEodFc.HelDusHl,
//   svIn.OilEodFc.HelFraHl,
//   svIn.OilEodFc.HelMannLwghHl,
//   svIn.OilEodFc.HelRhein500tHl,
//   svIn.OilPtFc.HslDeuMid,
//   svIn.Reuters1730.EurGbpMid,
//   svIn.Reuters1730.EurUsdMid) map { _(asof) } map { _ -!-> svOut } /*map { crv => s"${crv.container.name}.${crv.name}" -> crv.metaData.get.timeStamp }*/


// ###############################################################################################
// ### Forecasttool
// ###

case class FwdHist(fwd: Option[Series[Month, Double]], hist: Series[Day, Option[Double]]) {

      lazy val overrideFwdQuarterly = for {
            forward <- fwd
            history = change(quarterly(hist)) to Month
            if(!history.isEmpty)
      } yield A (forward ++ history, history.end)

      //type 4
      lazy val overrideFwd = for {
            forward <- fwd
            history = monthly(hist)
            if(!history.isEmpty)
      } yield A (forward ++ history, history.end)

      lazy val overrideFwdRoundTo2 = for {
            forward <- fwd
            history = monthly(hist) roundTo 2
            if(!history.isEmpty)
      } yield A (forward ++ history, history.end)

      //type 2
      lazy val concat = for {
            forward <- fwd
            history = omit(hist)
            if(!history.isEmpty)
      } yield A(concatenate(forward, history), forward.start - 1.months)
}

case class DailyFwdHistWithWeekend(fwd: Option[Series[Day, Double]], hist: Series[Day, Option[Double]], wknd: Series[Day, Option[Double]]) {

      lazy val overrideFwd = for {
            forward <- fwd
            day = omit(hist)
            weekend = flatRight(wknd)
            history = weekend ++ day
            if(!history.isEmpty)
      } yield A(forward ++ history, history.end)
}

case class DailyFwdHist(fwd: Option[Series[Day, Double]], hist: Series[Day, Option[Double]]) {

      lazy val overrideFwdGroupedByMonth = for {
            forward <- fwd map toMonth map (_ lag 1)
            month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
            takeLast = (s: Series[Day, Double]) => tryOption { s.lastValue }
            withLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeLast(series)}
            history = Interpolate.omit(Series(omit(hist) groupBy month map withLast))
            if(!history.isEmpty)
      } yield A(forward ++ history, forward.start - 1.months)

      lazy val overrideFwdGroupedByMonthNolag = for {
            forward <- fwd map toMonth
            month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
            takeLast = (s: Series[Day, Double]) => tryOption { s.lastValue }
            withLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeLast(series)}
            history = Interpolate.omit(Series(omit(hist) groupBy month map withLast))
            if(!history.isEmpty)
      } yield A(forward ++ history, forward.start - 1.months)

      lazy val overrideFwdFlatRight = for {
            forward <- fwd
            history = flatRight(hist)
            if(!history.isEmpty)
      } yield A(forward ++ history, history.end)

      lazy val overrideLaggedFwd = for {
            forward <- fwd map toMonth map (_ lag 1) map toDay
            history = omit(hist)
            //forward.from(history.end) where from is important since history has no values for weekends
      } yield A(change(forward.from(history.end) ++ history) to Month, Month(forward.start) - 1)

      lazy val overrideLaggedFwdKeepDaily = for {
            forward <- fwd map toMonth map (_ lag 1) map toDay
            history = omit(hist)

      } yield A(forward.from(history.end) ++ history, history.end)

      //type 3
      lazy val overrideFwd = for { //note that hist and fwd are not overlapping any more
            A(series, splitAt) <- overrideFwdKeepDaily
      } yield A(change(series) to Month, Month(splitAt + 1 ) - 1 )

      //type 1
      lazy val overrideFwdKeepDaily = for {
            forward <- fwd
            history = omit(hist)
            if(!history.isEmpty)
      } yield A(forward.from(history.end) ++ history, history.end)
}

object A {
      def apply[B <: DateTimeLike[B]](series: Series[B, Double]): A[B] =
            A(series, series.start.companion(Day(1900,1,1)))
}

case class A[B <: DateTimeLike[B]](series: Series[B, Double], splitAt: B) { self =>
      def *(num: Double) = self.copy(series = self.series * num)
      def -(num: Double) = self.copy(series = self.series - num)
      def +(num: Double) = self.copy(series = self.series + num)
      def +(that: Option[A [B]]) =that map { that => self.copy(series = self.series + that.series) }
      def /(num: Double) = self.copy(series = self.series / num)
      def /(that: Option[A[B]]) = that map { that => self.copy(series = self.series / that.series) }
      def roundTo(nbrOfDigits: Int) = self.copy(series = self.series.roundTo(nbrOfDigits))
}

def settlementCurve(settlementSymbol: String, untilRollOver: Series[Day, Option[Double]], afterRollOver: Series[Day, Option[Double]]): Series[Day, Option[Double]] =
      contractDetails flatMap (_.get(settlementSymbol)) map { rollOverDates =>
            val allSettlementDates = untilRollOver.time ++ afterRollOver.time

            val series = for {
                  settlementDate <- allSettlementDates
                  rollOverInThatMonth <- rollOverDates.get(settlementDate: Month)
            } yield {
                  if(settlementDate <= rollOverInThatMonth)
                        settlementDate -> untilRollOver.get(settlementDate).getOrElse(None)
                  else
                        settlementDate -> afterRollOver.get(settlementDate).getOrElse(None)
            }
            val series2 = untilRollOver
            val series3 = Series(series.toSeq)

            series2 ++ series3

      } getOrElse Series.empty

def writeWithTimeStamp[B <: DateTimeLike[B]](fwdCurve: Service#Container#ForwardCurveLike[Day, B, Double])(a: A[B])(implicit b: org.joda.time.DateTime => B){//(series: Series[A, Double]){

      val A(_series,splitAt) = a
      val series = _series roundTo 5
      // cut off everything beyond fwdEndDay
      if (series.nonEmpty) {

            val containerBucket = fwdCurve.getOrCreate(asof)
            val it = series.to(series.start.companion(fwdEndDay))
            val noteColumnOverride = (e: (B, Double)) => if(e._1 <= splitAt) "historical" else "forward"
            def toColumns = fwdCurve.toColumns(containerBucket.tradeDate,it.head._1).take(4) :+ noteColumnOverride

            containerBucket.writeToCds(fwdCurve.columnSet)(toColumns)(it)

            val timeStamp = fwdCurve(asof).metaData.get.timeStamp.toString("yyyy-MM-dd HH:mm:ss")
            val column = CString("TimeStamp")
            val timeStampTable = DataTable.fromIterableRow[String](List((column, (s:String) => s)))(List(timeStamp::Nil))

            fwdCurve(asof).container.getOrCreate(s"${fwdCurve(asof).name}_timestamp").writeToCds(timeStampTable)
      }
}

/////// RDMS

///////// INDEXKURVEN

lazy val grossWagesGermany = {
      implicit def limIter = limForecastIter
      yearly(ChangePct from IHSJULCGERMANYYEARLY) match {
            case series if(!series.isEmpty) => Some(series / 100.0)
            case _ => None
      }
}

lazy val producerPriceIndexGermany = {
      implicit def limIter = limForecastIter
      yearly(ChangePct from IHSPPIGERMANYYEARLY) match {
            case series if(!series.isEmpty) => Some(series / 100.0)
            case _ => None
      }
}

lazy val consumerPriceIndexNl = {
      implicit def limIter = limForecastIter
      yearly(ChangePct from IHSCPINETHERLANDSYEARLY) match {
            case series if(!series.isEmpty) => Some(series / 100.0)
            case _ => None
      }
}

/**
  * NL-CPI DERIVED BASIS 2006
  */
lazy val IndexNlConsumerPriceDer2006 = for {
      index <- consumerPriceIndexNl
      (lastDay, lastValue) = omit(DerCPI from CBSCPINLNETHERLAND_2006).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * DE - Gewerbliche Erzeugnisse
  */
lazy val IndexBrdStabuaGewerblErzeugnisGesamtBasis2010MonthlyPercentFwd = for {
      index <- producerPriceIndexGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEPRODUCTSCAPITALGOODSTOTALBASE2010).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}


/**
  * DE - SALARY MONTHLY ENERGY WATER
  */
lazy val IndexBrdStabuaMonatsgehaltEnergieWasserBasis2010MonthlyPercentFwd = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2010).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * DE - SALARY HOURLY PRIVATE SECTOR
  */
lazy val IndexBrdStabuaStundengehaltPrivatwirtschaftBasis2010MonthlyPercentFwd = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEAGREEDHOURLYEARNINGSPRIVATESECTORBASE2010).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}
/**
  * DE - SALARY HOURLY MINING
  */
lazy val IndexBrdStabuaStundengehaltBergebauBasis2010MonthlyPercentFwd = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSMININGBASE2010).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * DE - SALARY HOURLY ENERGY WATER FBG
  */
lazy val IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2010MonthlyPercentFwd = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2010).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * DE - SALARY HOURLY ENERGY FBG
  */
lazy val IndexBrdStabuaStundengehaltFbgEnergieBasis2010MonthlyPercentFwd = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYSUPPLYBASE2010).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}


/**
  * NEW INDEX CURVES
  */
lazy val IndexBrdStabuaMonatsgehaltEnergieWasserBasis2015MonthlyPercentFwd = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2015).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val IndexBrdStabuaStundengehaltBergebauBasis2015MonthlyPercentFwd = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSMININGBASE2015).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2015MonthlyPercentFwd = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2015).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val IndexBrdStabuaStundengehaltFbgEnergieBasis2015MonthlyPercentFwd = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYSUPPLYBASE2015).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val IndexBrdJaehrlicherVerdienstEnergieVersorg2015 = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDENEGOTIATEDANNUALEARNINGSENERGYSUPPLY2015).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val IndexBrdMonatlicherVerdienstEnergieVersorg2015 = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDENEGOTIATEDMONTHLYEARNINGSEXCLBONUSENERGYSUPPLY2015).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val IndexBrdStuendlicherVerdienstEnergieVersorg2015 = for {
      index <- grossWagesGermany
      (lastDay, lastValue) = omit(Index from STATSBUNDEAGREEDHOURLYEARNINGSEXCLBONUSENERGYSUPPLY2015).last
      lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * PERCENT
  * MONTHLY
  */
lazy val IndexNlCbsCpiDerivedBasis2006Percent = FwdHist(IndexNlConsumerPriceDer2006, DerCPI from CBSCPINLNETHERLAND_2006).overrideFwd

lazy val IndexBrdStabuaGewerblErzeugnisGesamtBasis2010MonthlyPercent = FwdHist(IndexBrdStabuaGewerblErzeugnisGesamtBasis2010MonthlyPercentFwd, Index from STATSBUNDEPRODUCTSCAPITALGOODSTOTALBASE2010).overrideFwd

// lazy val IndexBrdStabuaMonatsgehaltEnergieWasserBasis2010YearlyPercent = for {
// A(series, splitAt) <- FwdHist(IndexNlConsumerPriceDer2006, DerCPI from CBSCPINLNETHERLAND_2006).overrideFwd
// } yield A(change(series) to Year , Year(splitAt) - 1)

lazy val IndexBrdStabuaMonatsgehaltEnergieWasserBasis2010MonthlyPercent = FwdHist(IndexBrdStabuaMonatsgehaltEnergieWasserBasis2010MonthlyPercentFwd, Index from STATSBUNDEAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2010).overrideFwdQuarterly
lazy val IndexBrdStabuaStundengehaltPrivatwirtschaftBasis2010MonthlyPercent = FwdHist(IndexBrdStabuaStundengehaltPrivatwirtschaftBasis2010MonthlyPercentFwd, Index from STATSBUNDEAGREEDHOURLYEARNINGSPRIVATESECTORBASE2010).overrideFwdQuarterly
lazy val IndexBrdStabuaStundengehaltBergebauBasis2010MonthlyPercent = FwdHist(IndexBrdStabuaStundengehaltBergebauBasis2010MonthlyPercentFwd, Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSMININGBASE2010).overrideFwdQuarterly
lazy val IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2010MonthlyPercent = FwdHist(IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2010MonthlyPercentFwd, Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2010).overrideFwdQuarterly
lazy val IndexBrdStabuaStundengehaltFbgEnergieBasis2010MonthlyPercent = FwdHist(IndexBrdStabuaStundengehaltFbgEnergieBasis2010MonthlyPercentFwd, Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYSUPPLYBASE2010).overrideFwdQuarterly

// NEW PERCENT MONTHLY
lazy val IndexBrdStabuaStundengehaltFbgEnergieBasis2015MonthlyPercent = FwdHist(IndexBrdStabuaStundengehaltFbgEnergieBasis2015MonthlyPercentFwd, Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYSUPPLYBASE2015).overrideFwdQuarterly
lazy val IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2015MonthlyPercent = FwdHist(IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2015MonthlyPercentFwd, Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2015).overrideFwdQuarterly
lazy val IndexBrdStabuaStundengehaltBergebauBasis2015MonthlyPercent = FwdHist(IndexBrdStabuaStundengehaltBergebauBasis2015MonthlyPercentFwd, Index from STATSBUNDEFBGAGREEDHOURLYEARNINGSMININGBASE2015).overrideFwdQuarterly
lazy val IndexBrdStabuaMonatsgehaltEnergieWasserBasis2015MonthlyPercent = FwdHist(IndexBrdStabuaMonatsgehaltEnergieWasserBasis2015MonthlyPercentFwd, Index from STATSBUNDEAGREEDHOURLYEARNINGSENERGYWATERRECYCLINGBASE2015).overrideFwdQuarterly
lazy val IndexBrdJaehrlicherVerdienstEnergieVersorg2015MonthlyPercent = FwdHist(IndexBrdJaehrlicherVerdienstEnergieVersorg2015, Index from STATSBUNDENEGOTIATEDANNUALEARNINGSENERGYSUPPLY2015).overrideFwdQuarterly
lazy val IndexBrdMonatlicherVerdienstEnergieVersorg2015MonthlyPercent = FwdHist(IndexBrdMonatlicherVerdienstEnergieVersorg2015, Index from STATSBUNDENEGOTIATEDMONTHLYEARNINGSEXCLBONUSENERGYSUPPLY2015).overrideFwdQuarterly
lazy val IndexBrdStuendlicherVerdienstEnergieVersorg2015MonthlyPercent = FwdHist(IndexBrdStuendlicherVerdienstEnergieVersorg2015, Index from STATSBUNDEAGREEDHOURLYEARNINGSEXCLBONUSENERGYSUPPLY2015).overrideFwdQuarterly

///////// WAGES - FWD

/**
  * Fwd - Wages - Kirchmoeser
  */
lazy val wagesAvGweKirchmoeser14thSalaryChristmasBonusEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesAvGweKirchmoeser14thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}


/**
  * Fwd - Wages - 1-90
  */
lazy val wagesAvGwe19014thSalaryChristmasBonusEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesAvGwe19014thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * Fwd - Wages - 1-82
  */
lazy val wagesAvGwe18214thSalaryChristmasBonusEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesAvGwe18214thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}
/**
  * Fwd - Wages - 7-80
  */
lazy val wagesAvGwe78014thSalaryChristmasBonusEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesAvGwe78014thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}
/**
  * Fwd - Wages - 7-89
  */
lazy val wagesAvGwe78914thSalaryChristmasBonusEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesAvGwe78914thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}
/**
  * Fwd - Wages - 7-77
  */
lazy val wagesAvGwe77714thSalaryChristmasBonusEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesAvGwe77714thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}
/**
  * Fwd - Wages - 14th Salary
  */
lazy val wagesAvGwe14thSalaryChristmasBonusEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesAvGwe14thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}
/**
  * Fwd - Wages - SV CHARGE
  */
lazy val wagesAvGweSvCharge14thSalaryChristmasBonusEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesAvGweSvCharge14thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}
/**
  * Fwd - Wages - AZV
  */
lazy val wagesAvGweAzv14thSalaryChristmasBonusEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesAvGweAzv14thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * Fwd - Wages - KOMMUNALLOHN 5 / 4 MONTH
  */
lazy val wagesTvOedKommunallohn54MonthEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesTvOedKommunallohn54MonthEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * Fwd - Wages - KOMMUNALLOHN 5 / 4 HOUR
  */
lazy val wagesTvOedKommunallohn54HourEurFwd = for {
      index <- grossWagesGermany
      (lastMonth, lastValue) <- wagesTvOedKommunallohn54HourEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
      (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

///// WAGES - CONCATENATIONS

/**
  * Wages - KirchmÃ¶ser
  * MONTHLY
  */
lazy val WagesAvGweKirchmoeser14thSalaryChristmasBonusEur = for {
      fwd <- wagesAvGweKirchmoeser14thSalaryChristmasBonusEurFwd
      hist  <- wagesAvGweKirchmoeser14thSalaryChristmasBonusEurHist
} yield A (fwd ++ hist, hist.end)

/**
  * Wages - 1-90
  * MONTHLY
  */
lazy val WagesAvGwe19014thSalaryChristmasBonusEur = for {
      fwd <- wagesAvGwe19014thSalaryChristmasBonusEurFwd
      hist  <- wagesAvGwe19014thSalaryChristmasBonusEurHist
} yield A (fwd ++ hist, hist.end)

/**
  * Wages - 1-82
  * MONTHLY
  */
lazy val WagesAvGwe18214thSalaryChristmasBonusEur = for {
      fwd <- wagesAvGwe18214thSalaryChristmasBonusEurFwd
      hist  <- wagesAvGwe18214thSalaryChristmasBonusEurHist
} yield A (fwd ++ hist, hist.end)

/**
  * Wages - 7-80
  * MONTHLY
  */
lazy val WagesAvGwe78014thSalaryChristmasBonusEur = for {
      fwd <- wagesAvGwe78014thSalaryChristmasBonusEurFwd
      hist  <- wagesAvGwe78014thSalaryChristmasBonusEurHist
} yield A (fwd ++ hist, hist.end)

/**
  * Wages - 7-89
  * MONTHLY
  */
lazy val WagesAvGwe78914thSalaryChristmasBonusEur = for {
      fwd <- wagesAvGwe78914thSalaryChristmasBonusEurFwd
      hist  <- wagesAvGwe78914thSalaryChristmasBonusEurHist
} yield A (fwd ++ hist, hist.end)

/**
  * Wages - 7-77
  * MONTHLY
  */
lazy val WagesAvGwe77714thSalaryChristmasBonusEur = for {
      fwd <- wagesAvGwe77714thSalaryChristmasBonusEurFwd
      hist  <- wagesAvGwe77714thSalaryChristmasBonusEurHist
} yield A (fwd ++ hist, hist.end)

/**
  * Wages - AZV
  * MONTHLY
  */
lazy val WagesAvGweAzv14thSalaryChristmasBonusEur = for {
      fwd <- wagesAvGweAzv14thSalaryChristmasBonusEurFwd
      hist  <- wagesAvGweAzv14thSalaryChristmasBonusEurHist
} yield A (fwd ++ hist, hist.end)
/**
  * Wages - SV CHARGE
  * MONTHLY
  */
lazy val WagesAvGweSvCharge14thSalaryChristmasBonusEur = for {
      fwd <- wagesAvGweSvCharge14thSalaryChristmasBonusEurFwd
      hist  <- wagesAvGweSvCharge14thSalaryChristmasBonusEurHist
} yield A (fwd ++ hist, hist.end)

/**
  * Wages - 14th Salary
  * MONTHLY
  */
lazy val WagesAvGwe14thSalaryChristmasBonusEur = for {
      fwd <- wagesAvGwe14thSalaryChristmasBonusEurFwd
      hist  <- wagesAvGwe14thSalaryChristmasBonusEurHist
} yield A (fwd ++ hist, hist.end)

/**
  * Wages - KOMMUNALLOHN 5/4 MONTH
  * MONTHLY
  */
lazy val WagesTvOedKommunallohn54MonthEur = for {
      fwd <- wagesTvOedKommunallohn54MonthEurFwd
      hist  <- wagesTvOedKommunallohn54MonthEurHist
} yield A (fwd ++ hist, hist.end)

/**
  * Wages - KOMMUNALLOHN 5/4 HOUR
  * MONTHLY
  */
lazy val WagesTvOedKommunallohn54HourEur = for {
      fwd <- wagesTvOedKommunallohn54HourEurFwd
      hist  <- wagesTvOedKommunallohn54HourEurHist
} yield A (fwd ++ hist, hist.end)

///////// WAGES - FWD



///////// CONCATENATIONS

/**
  *USD_T
  *DAILY
  */
lazy val OilIceUlsMidDailyUsdT = DailyFwdHist(ulsd10Ppm map toDay, Close from ICEULS).overrideFwdKeepDaily
/**
  * DKK_EUR
  * DAILY
  */
lazy val FxEcbDkkMidDailyDkkEur = DailyFwdHist(eurDkk, Spot from ECBDKK).overrideFwdKeepDaily
/**
  * GBP_EUR
  * DAILY
  */
lazy val FxEcbGbpMidDailyGbpEur = DailyFwdHist(eurGbp, Spot from ECBGBP).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasCeghHerenWeekendIndexDailyEurMwh = DailyFwdHist(baumgarten, Close from HERENESGMCEGHSPOTWEEKENDINDEX).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasCeghHerenDaAskDailyEurMwh = DailyFwdHist(baumgarten, FwdOfr1day from ESGMBAUM).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasCeghHerenDaBidDailyEurMwh = DailyFwdHist(baumgarten, FwdBid1day from ESGMBAUM).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasCeghHerenDaMidDailyEurMwh = DailyFwdHist(baumgarten, FwdMid1day from ESGMBAUM).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasCeghHerenMaAskDailyEurMwh = DailyFwdHist(baumgarten, FwdOfr01mo from ESGMBAUM).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasCeghHerenMaBidDailyEurMwh = DailyFwdHist(baumgarten, FwdBid01mo from ESGMBAUM).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasCeghHerenWeekendAskDailyEurMwh = DailyFwdHist(baumgarten, FwdOfr1wkn from ESGMBAUM).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasCeghHerenWeekendMidDailyEurMwh = DailyFwdHist(baumgarten, FwdMid1wkn from ESGMBAUM).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusDaAskDailyEurMwh = DailyFwdHist(gaspool, High from PA000169660).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusDaBidDailyEurMwh = DailyFwdHist(gaspool, Low from PA000169660).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusMaIndexDailyEurMwh = DailyFwdHist(gaspool, Index from PA000333661).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusMaIndexBidDailyEurMwh = DailyFwdHist(gaspool, High from PA000273761).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusMaIndexAskDailyEurMwh = DailyFwdHist(gaspool, Low from PA000273761).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusWeekendAskDailyEurMwh = DailyFwdHist(gaspool, High from PA000169760).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusWeekendBidDailyEurMwh = DailyFwdHist(gaspool, Low from PA000169760).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexEgixMaIndexDailyEurMwh = DailyFwdHist(gaspool, Val from EGIXINDEXMONTHGASPOOL).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexEgixDaIndexDailyEurMwh = DailyFwdHist(gaspool, Val from EGIXINDEXDAYGASPOOL).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ01MidDailyEurMwh = DailyFwdHist(gaspool, settlementCurve("G2BQ", Close from EEXBEB_Q1, Series.empty)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ02MidDailyEurMwh = DailyFwdHist(gaspool, settlementCurve("G2BQ", Close from EEXBEB_Q2, Close from EEXBEB_Q1)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ03MidDailyEurMwh = DailyFwdHist(gaspool, settlementCurve("G2BQ", Close from EEXBEB_Q3, Close from EEXBEB_Q2)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ04MidDailyEurMwh = DailyFwdHist(gaspool, settlementCurve("G2BQ", Close from EEXBEB_Q4, Close from EEXBEB_Q3)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexDa01MidDailyEurMwh = DailyFwdHist(gaspool, Close from EEXGPLMW01WDTRADEDAYAHEAD_01).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexDa02MidDailyEurMwh = DailyFwdHist(gaspool, Close from EEXGPLMW01WDTRADEDAYAHEAD_02).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr01MidDailyEurMwh = DailyFwdHist(gaspool, settlementCurve("G2BY", Close from EEXBEB_YR1, Series.empty)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenDaIndexDailyEurMwh = DailyFwdHist(gaspool, Close from HERINXGASPOOLDAI).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenMaAskDailyEurMwh = DailyFwdHist(gaspool, FwdOfr01mo from ESGMBEB).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenMaBidDailyEurMwh = DailyFwdHist(gaspool, FwdBid01mo from ESGMBEB).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenWeekendIndexDailyEurMwh = DailyFwdHist(gaspool, Close from HERENESGMGASPOOLSPOTWEEKENDINDEX).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenWeekendAskDailyEurMwh = DailyFwdHist(gaspool, FwdOfrwknd from ESGMBEB).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenWeekendBidDailyEurMwh = DailyFwdHist(gaspool, FwdBidwknd from ESGMBEB).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenCyr01MidDailyEurMwh = DailyFwdHist(gaspool, FwdMid01cyr from ESGMBEB).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplPlattsDaMidDailyEurMwh = for {
      hgh <- DailyFwdHist(gaspool, High from GBBTD00).overrideFwdKeepDaily
      sum <- hgh + DailyFwdHist(gaspool, Low  from GBBTD00).overrideFwdKeepDaily
} yield sum / 2

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenDaAskDailyEurMwh = DailyFwdHist(gaspool, FwdOfr1day from ESGMBEB).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenDaBidDailyEurMwh = DailyFwdHist(gaspool, FwdBid1day from ESGMBEB).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenDaMidDailyEurMwh = DailyFwdHist(gaspool, FwdMid1day from ESGMBEB).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNbpHerenMaAskDailyEurMwh = DailyFwdHist(nbp, FwdOfr01mo from ESGMNBP).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNbpHerenMaBidDailyEurMwh = DailyFwdHist(nbp, FwdBid01mo from ESGMNBP).overrideLaggedFwdKeepDaily
/**
  * GBPPC_THERM
  * DAILY
  */
lazy val GasNbpIceMaMidDailyGbppcTherm = DailyFwdHist(nbp, Close from ICEOTCM).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgArgusMaIndexDailyEurMwh = DailyFwdHist(ncg, Index from PA000699261).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenDayRefpriceDailyEurMwh = DailyFwdHist(ncg, RefPrice from EEXNCGDRPDELIVDATE).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgPnxtDayRefpriceDailyEurMwh = {
      val oldeex = RefPrice from EEXNCGDRPDELIVDATE
      val newpnxt = Index from PNXTGASNCGDA
      val pnxtpatch = oldeex.until(Day(2018, 1, 1)) ++ newpnxt.from(Day(2018, 1, 1)) //note that until is exclusive
      DailyFwdHist(ncg, pnxtpatch).overrideFwdKeepDaily
}
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplPnxtDayRefpriceDailyEurMwh = {
      val oldgeex = RefPrice from EEXGPLDRPDELIVDATE
      val newgpnxt = Index from PNXTGASGPLDA
      val pnxtgpatch = oldgeex.until(Day(2018, 1, 1)) ++ newgpnxt.from(Day(2018, 1, 1)) //note that until is exclusive
      DailyFwdHist(gaspool, pnxtgpatch).overrideFwdKeepDaily
}
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfPnxtDayRefpriceDailyEurMwh = {
      val oldteex = RefPrice from EEXTTFDRPDELIVDATE
      val newtpnxt = Index from PNXTGASTTFDA
      val pnxttpatch = oldteex.until(Day(2018, 1, 1)) ++ newtpnxt.from(Day(2018, 1, 1)) //note that until is exclusive
      DailyFwdHist(ttf, pnxttpatch).overrideFwdKeepDaily
}
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexEgixMaIndexDailyEurMwh = DailyFwdHist(ncg, Val from EGIXINDEXMONTHNCG ).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexEgixDaIndexDailyEurMwh = DailyFwdHist(ncg, Val from EGIXINDEXDAYNCG ).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ01MidDailyEurMwh = DailyFwdHist(ncg, settlementCurve("G0BQ", Close from EEXEGT_Q1, Series.empty)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ02MidDailyEurMwh = DailyFwdHist(ncg, settlementCurve("G0BQ", Close from EEXEGT_Q2, Close from EEXEGT_Q1)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ03MidDailyEurMwh = DailyFwdHist(ncg, settlementCurve("G0BQ", Close from EEXEGT_Q3, Close from EEXEGT_Q2)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ04MidDailyEurMwh = DailyFwdHist(ncg, settlementCurve("G0BQ", Close from EEXEGT_Q4, Close from EEXEGT_Q3)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexDa01MidDailyEurMwh = DailyFwdHist(ncg, Close from EEXNCGMW01WDTRADEDAYAHEAD_01).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexDa02MidDailyEurMwh = DailyFwdHist(ncg, Close from EEXNCGMW01WDTRADEDAYAHEAD_02).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr01MidDailyEurMwh = DailyFwdHist(ncg, settlementCurve("G0BY", Close from EEXEGT_YR1, Series.empty)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr02MidDailyEurMwh = DailyFwdHist(ncg, settlementCurve("G0BY", Close from EEXEGT_YR2, Close from EEXEGT_YR1)).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenDaIndexDailyEurMwh = DailyFwdHist(ncg, Close from HERINXEGTDAYACUMULATIVE).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenDaIndexCumulativeDailyEurMwh = DailyFwdHist(ncg, Close from HERINXEGTDAYA).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenDaAskDailyEurMwh = DailyFwdHist(ncg, FwdOfr1day from ESGMEGT).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenDaBidDailyEurMwh = DailyFwdHist(ncg, FwdBid1day from ESGMEGT).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenMaIndexDailyEurMwh = DailyFwdHist(ncg, Close from HERENNCGDAILYMONTHAHEADINDEX).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenMaAskDailyEurMwh = DailyFwdHist(ncg, FwdOfr01mo from ESGMEGT).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenMaBidDailyEurMwh = DailyFwdHist(ncg, FwdBid01mo from ESGMEGT).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenQ01MidDailyEurMwh = DailyFwdHist(ncg, FwdMid01qt from ESGMEGT).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenQ04MidDailyEurMwh = DailyFwdHist(ncg, FwdMid04qt from ESGMEGT).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenSum01MidDailyEurMwh = {
      val a = omit(FwdMid02sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(FwdMid01sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val summer = omit(a) ++ omit(b)
      DailyFwdHist(ncg, summer.mapValues(Some(_))).overrideFwdKeepDaily
}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenWin01AskDailyEurMwh = {
      val a = omit(FwdOfr01sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(FwdOfr02sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val winter = omit(a) ++ omit(b)
      DailyFwdHist(ncg, winter.mapValues(Some(_))).overrideFwdKeepDaily
}



/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenWin01BidDailyEurMwh = {
      val a = omit(FwdBid01sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(FwdBid02sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val winter = omit(a) ++ omit(b)
      DailyFwdHist(ncg, winter.mapValues(Some(_))).overrideFwdKeepDaily
}



/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenWin01MidDailyEurMwh = {
      val a = omit(FwdMid01sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(FwdMid02sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val winter = omit(a) ++ omit(b)
      DailyFwdHist(ncg, winter.mapValues(Some(_))).overrideFwdKeepDaily
}



/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenWeekendIndexDailyEurMwh = DailyFwdHist(ncg, Close from HERENESGMNCGSPOTWEEKENDINDEX).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenWeekendAskDailyEurMwh = DailyFwdHist(ncg, FwdOfrwknd from ESGMEGT).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenWeekendBidDailyEurMwh = DailyFwdHist(ncg, FwdBidwknd from ESGMEGT).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenCyrMidDailyEurMwh = DailyFwdHist(ncg, FwdMid01cyr from ESGMEGT).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgLebaDaIndexDailyEurMwh = DailyFwdHist(ncg, WtdAverage from LEBADAYAHEADEGTINDEX).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgLebaDaWindowIndexDailyEurMwh = DailyFwdHist(ncg, WtdAverage from LEBADAYAHEADWINDOWEGTINDEX).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgLebaMaIndexDailyEurMwh = DailyFwdHist(ncg, WtdAverage from LEBAMONTHAHEADEGTINDEX).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgLebaWeekendWindowIndexDailyEurMwh = DailyFwdHist(ncg, WtdAverage from LEBAWEEKENDWINDOWEGTINDEX).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgPlattsMaMidDailyEurMwh = for {
      hgh <- DailyFwdHist(ncg, High from GERTM00).overrideLaggedFwdKeepDaily
      sum <- hgh + DailyFwdHist(ncg, Low  from GERTM00).overrideLaggedFwdKeepDaily
} yield sum / 2

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNptfGaspointNordicWithinDayDailyEurMwh = DailyFwdHist(nptf, Close from NORDGASNORDICWITHINDAY).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasPegnHerenDaMidDailyEurMwh = DailyFwdHist(pegNord, FwdMid1day from ESGMPEG).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasPsvPlattsDaMidDailyEurMwh = for {
      hgh <- DailyFwdHist(psv, High from GPVTD00).overrideFwdKeepDaily
      sum <- hgh + DailyFwdHist(psv, Low  from GPVTD00).overrideFwdKeepDaily
} yield sum / 2


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfApxDaCloseDailyEurMwh = DailyFwdHist(ttf, Index from APXTTFENDOFWORKINGDAYDAFLOW).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfArgusDaAskDailyEurMwh = DailyFwdHist(ttf, High from PA000303960_2 ).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfArgusDaBidDailyEurMwh = DailyFwdHist(ttf, Low from PA000303960_2 ).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfArgusMaIndexDailyEurMwh = DailyFwdHist(ttf, Index from PA000333461).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfArgusWeekendAskDailyEurMwh = DailyFwdHist(ttf, High from PA000304060).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfArgusWeekendBidDailyEurMwh = DailyFwdHist(ttf, Low from PA000304060 ).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfArgusMaMidDailyEurMwh = for {
      hgh <- DailyFwdHist(ttf, High from PA000304161_2).overrideLaggedFwdKeepDaily
      sum <- hgh + DailyFwdHist(ttf, Low  from PA000304161_2).overrideLaggedFwdKeepDaily
} yield sum / 2


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfEex1mwDaMidDailyEurMwh = /*for {
fwd <- ttf
day = omit(Close from EEXTTFMW01WDDELIVDATE)
weekend = flatRight(Close from PNXTGASTTFTRADEDATEWE)
lim = weekend ++ day
} yield fwd ++ lim*/ DailyFwdHistWithWeekend(ttf, Close from PNXTGASTTFDA,Close from PNXTGASTTFTRADEDATEWE).overrideFwd

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenDaIndexCumulativeDailyEurMwh = DailyFwdHist(ttf, Close from HERINXTTFDAYACUMULATIVE).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenDaIndexDailyEurMwh = DailyFwdHist(ttf, Close from HERINXTTFDAYA).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenDaAskDailyEurMwh = DailyFwdHist(ttf, FwdOfr1day from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenDaBidDailyEurMwh = DailyFwdHist(ttf, FwdBid1day from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenGyr01AskDailyEurMwh = DailyFwdHist(ttf, FwdOfr01yr from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenGyr01BidDailyEurMwh = DailyFwdHist(ttf, FwdBid01yr from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenGyr01MidDailyEurMwh = DailyFwdHist(ttf, FwdMid01yr from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenMaIndexDailyEurMwh = DailyFwdHist(ttf, Close from HERENTTFDAILYMONTHAHEADINDEX).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenMaAskDailyEurMwh = DailyFwdHist(ttf, FwdOfr01mo from ESGMTTF).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenMaBidDailyEurMwh = DailyFwdHist(ttf, FwdBid01mo from ESGMTTF).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenQ01MidDailyEurMwh = DailyFwdHist(ttf, FwdMid01qt from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenQ04MidDailyEurMwh = DailyFwdHist(ttf, FwdMid04qt from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenSum01MidDailyEurMwh = {
      val a = omit(FwdMid02sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(FwdMid01sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val summer = omit(a) ++ omit(b)
      DailyFwdHist(ttf, summer.mapValues(Some(_))).overrideFwdKeepDaily
}
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenWin01MidDailyEurMwh = {
      val a = omit(FwdMid01sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(FwdMid02sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val winter = omit(a) ++ omit(b)
      DailyFwdHist(ttf, winter.mapValues(Some(_))).overrideFwdKeepDaily
}
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenWeekendIndexDailyEurMwh = DailyFwdHist(ttf, Close from HERINXTTFWEND).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenWeekendAskDailyEurMwh = DailyFwdHist(ttf, FwdOfrwknd from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenWeekendBidDailyEurMwh = DailyFwdHist(ttf, FwdBidwknd from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenCyr01MidDailyEurMwh = DailyFwdHist(ttf, FwdMid01cyr from ESGMTTF).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfLebaDaIndexDailyEurMwh = DailyFwdHist(ttf, WtdAverage from LEBADAYAHEADTTFINDEX).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfLebaDaWindowIndexDailyEurMwh = DailyFwdHist(ttf, WtdAverage from LEBADAYAHEADWINDOWTTFINDEX).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfLebaMaIndexDailyEurMwh = DailyFwdHist(ttf, WtdAverage from LEBAMONTHAHEADTTFINDEX).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasZeeArgusDaIndexDailyEurMwh = DailyFwdHist(zeeEur, Index from PA000333360).overrideFwdKeepDaily
/**
  * GBBPC_THERM
  * DAILY
  */
lazy val GasZeePlattsMaMidDailyEurMwh = for {
      hgh <- DailyFwdHist(zee, High from AADOS00).overrideFwdKeepDaily
      sum <- hgh + DailyFwdHist(zee, Low  from AADOS00).overrideFwdKeepDaily
} yield sum / 2
/**
  * USD_T
  * DAILY
  */
lazy val OilPlattsFo1FobAraAskDailyUsdT = DailyFwdHist(fo1FobAraBarges map { _ + 0.25 } map toDay, High from PUAAP00).overrideFwdKeepDaily
/**
  * USD_T
  * DAILY
  */
lazy val OilPlattsFo1FobAraBidDailyUsdT = DailyFwdHist(fo1FobAraBarges map { _ - 0.25 } map toDay, Low from PUAAP00).overrideFwdKeepDaily
/**
  * USD_T
  * DAILY
  */
lazy val OilPlattsFo1FobAraMidDailyUsdT = DailyFwdHist(fo1FobAraBarges map toDay, Close from PUAAP00).overrideFwdKeepDaily
/**
  * USD_T
  * DAILY
  */
lazy val OilPlattsGo01FobAraAskDailyUsdT = DailyFwdHist(go01FobAraBarges map { _ + 0.25 } map toDay, High from AAYWT00).overrideFwdKeepDaily
/**
  * USD_T
  * DAILY
  */
lazy val OilPlattsGo01FobAraBidDailyUsdT = DailyFwdHist(go01FobAraBarges map { _ - 0.25 } map toDay, Low from AAYWT00).overrideFwdKeepDaily
/**
  * USD_T
  * DAILY
  */
lazy val OilPlattsGo01FobAraMidDailyUsdT = DailyFwdHist(go01FobAraBarges map toDay, Close from AAYWT00).overrideFwdKeepDaily

/**
  * USD_BBL
  * MONTHLY
  */
lazy val OilPlattsBrentDatedAskMonthlyUsdBbl = FwdHist(datedBrent map { _ + 0.25 }, High from PCAAS03).overrideFwd
/**
  * USD_BBL
  * MONTHLY
  */
lazy val OilPlattsBrentDatedBidMonthlyUsdBbl = FwdHist(datedBrent map { _ - 0.25 }, Low from PCAAS03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo1CifMedAskMonthlyUsdT = FwdHist(fo1CifMedCargoes map { _ + 0.25 }, High from PUAAJ03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo1CifMedBidMonthlyUsdT = FwdHist(fo1CifMedCargoes map { _ - 0.25 }, Low from PUAAJ03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo1CifNweAskMonthlyUsdT = FwdHist(fo1CifNweCargoes map { _ + 0.25 }, High from PUAAL03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo1CifNweBidMonthlyUsdT = FwdHist(fo1CifNweCargoes map { _ - 0.25 }, Low from PUAAL03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo1FobAraAskMonthlyUsdT = FwdHist(fo1FobAraBarges map { _ + 0.25 }, High from PUAAP03).overrideFwd

/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo1FobMedAskMonthlyUsdT = FwdHist(fo1FobMedCargoes map { _ + 0.25 }, High from PUAAK03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo1FobMedBidMonthlyUsdT = FwdHist(fo1FobMedCargoes map { _ - 0.25 }, Low from PUAAK03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo1FobMedMidMonthlyUsdT = FwdHist(fo1FobMedCargoes, Close from PUAAK03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo1FobNweMidMonthlyUsdT = FwdHist(fo1FobNweCargoes, Close from PUAAM03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo35FobAraAskMonthlyUsdT = FwdHist(fo35FobAraBarges map { _ + 0.25 }, High from PUABC03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo35FobAraBidMonthlyUsdT = FwdHist(fo35FobAraBarges map { _ - 0.25 }, Low from PUABC03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo35FobAraMidMonthlyUsdT = FwdHist(fo35FobAraBarges, Close from PUABC03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo35FobMedAskMonthlyUsdT = FwdHist(fo35FobMedCargoes map { _ + 0.25 }, High from PUAAZ03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo35FobMedBidMonthlyUsdT = FwdHist(fo35FobMedCargoes map { _ - 0.25 }, Low from PUAAZ03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsFo35FobMedMidMonthlyUsdT = FwdHist(fo35FobMedCargoes, Close from PUAAZ03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo01CifMedAskMonthlyUsdT = FwdHist(go01CifMedCargoes  map { _ + 0.25 }, High from AAVJJ03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo01CifMedBidMonthlyUsdT = FwdHist(go01CifMedCargoes  map { _ - 0.25 }, Low from AAVJJ03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo01CifNweAskMonthlyUsdT = FwdHist(go01CifNweCargoes map { _ + 0.25 }, High from AAYWS03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo01CifNweBidMonthlyUsdT = FwdHist(go01CifNweCargoes map { _ - 0.25 }, Low from AAYWS03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo01FobMedAskMonthlyUsdT = FwdHist(go01FobMedCargoes map { _ + 0.25 }, High from AAVJI03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo01FobMedBidMonthlyUsdT = FwdHist(go01FobMedCargoes map { _ - 0.25 }, Low from AAVJI03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo01FobMedMidMonthlyUsdT = FwdHist(go01FobMedCargoes, Close from AAVJI03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo01FobNweMidMonthlyUsdT = FwdHist(go01FobNweCargoes, Close from AAYWR03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo50PpmFobAraAskMonthlyUsdT = FwdHist(go50Ppm map { _ + 0.25 }, High from AAUQC03).overrideFwd
/**
  * USD_T
  * MONTHLY
  */
lazy val OilPlattsGo50PpmFobAraBidMonthlyUsdT = FwdHist(go50Ppm map { _ - 0.25 }, Low from AAUQC03).overrideFwd


/**
  * DKK_EUR
  * MONTHLY
  */
lazy val FxEcbDkkMidMonthlyDkkEur = DailyFwdHist(eurDkk, Spot from ECBDKK).overrideFwd.map(_.roundTo(4))
/**
  * HUF_EUR
  * MONTHLY
  */
lazy val FxEcbHufMidMonthlyHufEur = DailyFwdHist(eurHuf, Spot from ECBHUF).overrideFwd.map(_.roundTo(5))
/**
  * CZK_EUR
  * MONTHLY
  */
lazy val FxEcbCzkMidMonthlyCzkEur = DailyFwdHist(eurCzk, Spot from ECBCZK).overrideFwd.map(_.roundTo(3))
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexMaMidDailyEurMwh = DailyFwdHist(gaspool, settlementCurve("G2BM", Close from EEXBEB_MTH2, Close from EEXBEB_MTH1)).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexSum01MidDailyEurMwh = {

      val sn1 = settlementCurve("G2BS", Close from EEXBEB_SE1, Series.empty)
      val sn2 = settlementCurve("G2BS", Close from EEXBEB_SE2, Close from EEXBEB_SE1)

      val a = omit(sn2) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(sn1) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val summer = omit(a) ++ omit(b)
      DailyFwdHist(gaspool, summer.mapValues(Some(_))).overrideFwdKeepDaily
}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexWin01MidDailyEurMwh = {

      val sn1 = settlementCurve("G2BS", Close from EEXBEB_SE1, Series.empty)
      val sn2 = settlementCurve("G2BS", Close from EEXBEB_SE2, Close from EEXBEB_SE1)

      val a = omit(sn1) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(sn2) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val winter = omit(a) ++ omit(b)
      DailyFwdHist(gaspool, winter.mapValues(Some(_))).overrideFwdKeepDaily
}

/**
  * EUR_MWH
  * DAILY
  */
//lazy val GasNcgEexMaMidDailyEurMwh = DailyFwdHist(ncg, Close from EEXEGT_MTH2).overrideLaggedFwdKeepDaily
//lazy val GasNcgEexMaMidDailyEurMwh = DailyFwdHist(ncg, settlementCurve("G0BM", Close from EEXEGT_MTH2, Close from EEXEGT_MTH1)).overrideLaggedFwdKeepDaily
lazy val GasNcgEexMaMidDailyEurMwh = {
      val oldLogic = settlementCurve("G0BM", Close from EEXEGT_MTH2, Close from EEXEGT_MTH1)

      val mth1 = settlementCurve("G0BM", Close from EEXEGT_MTH1, Series.empty)
      val mth1ByMonth = mth1.groupBy(e => Month(e._1))

      val mth2 = Close from EEXEGT_MTH2 //just to figure out which days are quoted, i.e. which days are either weekend or bank holidays
      val mth2ByMonth = mth2.groupBy(e => Month(e._1))

      val newLogicByMonth = mth1ByMonth map {case (month, mth1) =>
            val (_, lastQuoteInThatMonth) = Interpolate.omit(mth1).last
            val mth2: Series[Day, Option[Double]] = mth2ByMonth(month)
            val mth2WithLastQuote = mth2 map { case (day, None) => day -> None; case (day, Some(_)) => day -> Some(lastQuoteInThatMonth) }
            settlementCurve("G0BM", mth1, mth2WithLastQuote)
      }
      val newLogic = newLogicByMonth.reduceLeft(_ ++ _)

      val patch = oldLogic.until(Day(2015,6,10)) ++ newLogic.from(Day(2015,6,10)) //note that until is exclusive
      DailyFwdHist(ncg, patch).overrideLaggedFwdKeepDaily
}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexSum01MidDailyEurMwh = {
      val sn1 = settlementCurve("G0BS", Close from EEXEGT_SE1, Series.empty)
      val sn2 = settlementCurve("G0BS", Close from EEXEGT_SE2, Close from EEXEGT_SE1)
      // val d = settlementCurve("G0BM", Close from EEXEGT_MTH2, Close from EEXEGT_MTH1)
      //G0BS, blank
      //G0BM, take EEXEGT_MTH1 instead of EEXEGT_MTH2

      val a = omit(sn2) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(sn1) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      //val a = omit(Close from EEXEGT_SE2) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      // val b = omit(Close from EEXEGT_SE1) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val summer = omit(a) ++ omit(b)
      DailyFwdHist(ncg, summer.mapValues(Some(_))).overrideFwdKeepDaily
}
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexWin01MidDailyEurMwh = {

      val sn1 = settlementCurve("G0BS", Close from EEXEGT_SE1, Series.empty)
      val sn2 = settlementCurve("G0BS", Close from EEXEGT_SE2, Close from EEXEGT_SE1)

      val a = omit(sn1) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
      val b = omit(sn2) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
      val winter = omit(a) ++ omit(b)
      DailyFwdHist(ncg, winter.mapValues(Some(_))).overrideFwdKeepDaily
}
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceMaMidDailyEurMwh = DailyFwdHist(ttf, Close from ICETTF).overrideLaggedFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceQ01MidDailyEurMwh = DailyFwdHist(ttf, Close from ICETTF_Q1).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceQ02MidDailyEurMwh = DailyFwdHist(ttf, Close from ICETTF_Q2).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceQ03MidDailyEurMwh = DailyFwdHist(ttf, Close from ICETTF_Q3).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceQ04MidDailyEurMwh = DailyFwdHist(ttf, Close from ICETTF_Q4).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceSum01MidDailyEurMwh = DailyFwdHist(ttf, Close from ICETTF_SUM1).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceWin01MidDailyEurMwh = DailyFwdHist(ttf, Close from ICETTF_WIN1).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceCyr01MidDailyEurMwh = DailyFwdHist(ttf, Close from ICETTF_YR1).overrideFwdKeepDaily
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasPsvHerenDaMidDailyEurMwh = for {
      hgh <- DailyFwdHist(psv, Offer from HERENESGMPSV_DA).overrideFwdKeepDaily
      sum <- hgh + DailyFwdHist(psv, Bid  from HERENESGMPSV_DA).overrideFwdKeepDaily
} yield sum / 2

/**
  * EUR_MWH
  * MONTHLY
  */
lazy val GasTtfArgusMaMidMthEndMonthlyEurMwh = DailyFwdHist(ttf, Index from PA000333461).overrideFwdGroupedByMonth
/**
  * EUR_MWH
  * MONTHLY
  */
lazy val GasTtfHerenDaIndexMthEndShiftMonthlyEurMwh = DailyFwdHist(ttf, Close from HERINXTTFDAYACUMULATIVE).overrideFwdGroupedByMonthNolag
/**
  * GBBPC_THERM
  * MONTHLY
  */
lazy val GasNbpIceMaMthEndMonthlyGbbpcTherm = DailyFwdHist(nbp, Close from IPENBP).overrideFwdGroupedByMonthNolag
/**
  * EUR_MWH
  * MONTHLY
  */
lazy val GasNcgHerenMaIndexMthEndMonthlyEurMwh = DailyFwdHist(ncg, Close from HERENNCGDAILYMONTHAHEADINDEX).overrideFwdGroupedByMonthNolag
/**
  * GBBPC_THERM
  * MONTHLY
  */
lazy val GasZeePlattsMaMthEndMonthlyGbbpcTherm = for {
      hgh <- DailyFwdHist(zee, High from AADOS00).overrideFwdGroupedByMonthNolag
      sum <- hgh + DailyFwdHist(zee, Low  from AADOS00).overrideFwdGroupedByMonthNolag
} yield sum / 2


/**
  * EUR_T
  * MONTHLY
  */
lazy val OilPlattsFo1FobAraMidMonthlyEurT = OilPlattsFo1FobAraMidMonthlyUsdT flatMap  { _ / FxEcbUsdMidMonthlyUsdEur.map(_.roundTo(4)) }
lazy val OilPlattsGo01FobAraMidMonthlyEurT = OilPlattsGo01FobAraMidMonthlyUsdT flatMap  { _ / FxEcbUsdMidMonthlyUsdEur.map(_.roundTo(4)) }
lazy val OilPlattsGo50PpmFobAraMidMonthlyEurT = OilPlattsGo50PpmFobAraMidMonthlyUsdT flatMap  { _ / FxEcbUsdMidMonthlyUsdEur.map(_.roundTo(4)) }

/**
  * EUR_T
  * MONTHLY
  */
lazy val OilPlattsFo1FobAraMidEbv1MonthlyEurT = for {
      A(curve, splitAt) <- OilPlattsFo1FobAraMidMonthlyEurT
      ebv = 3.3
      offset = 0.0
} yield A(curve - ebv - offset, splitAt)

/**
  * EUR_T
  * MONTHLY
  */

lazy val OilPlattsFo1FobAraMidEbv2MonthlyEurT = for {
      A(curve, splitAt) <- OilPlattsFo1FobAraMidMonthlyEurT
      ebv = 3.3
      offset = 0.4
} yield A(curve - ebv - offset, splitAt)

lazy val OilPlattsGo01FobAraMidEbv1MonthlyEurT = for {
      A(curve, splitAt) <- OilPlattsGo01FobAraMidMonthlyEurT
      ebv = 0.23
      offset = 0.0
} yield A(curve - ebv - offset, splitAt)

lazy val OilPlattsGo50PpmFobAraMidEbv1MonthlyEurT = for {
      A(curve, splitAt) <- OilPlattsGo50PpmFobAraMidMonthlyEurT
      ebv = 0.23
      offset = 0.0
} yield A(curve - ebv - offset, splitAt)

lazy val OilPlattsGo50PpmFobAraMidEbv2MonthlyEurT = for {
      A(curve, splitAt) <- OilPlattsGo50PpmFobAraMidMonthlyEurT
      ebv = 0.23
      offset = 17.65
} yield A(curve - ebv - offset, splitAt)

lazy val OilPlattsGo50PpmFobAraMidEbv3MonthlyEurT = for {
      A(curve, splitAt) <- OilPlattsGo50PpmFobAraMidMonthlyEurT
      ebv = 0.23
      offset = 18.0
} yield A(curve - ebv - offset, splitAt)

lazy val OilPlattsGo50PpmFobAraMidEbv4MonthlyEurT = for {
      A(curve, splitAt) <- OilPlattsGo50PpmFobAraMidMonthlyEurT
      ebv = 0.23
      offset = 0.35
} yield A(curve - ebv - offset, splitAt)

/*
      * USD_MMBTU
      * MONTHLY
      */
lazy val GasHhubNymexMaMidMonthlyUsdMmbtu = for {
      a <- FwdHist(hhub, Close from NG2 ).concat
} yield a.copy(series = a.series.lag(-1))

lazy val PlattsDailyWeightsMonthlyDays =  {
      val fwdEnd = (asof.year + 4).year
      val lim = omit(Close from AAJUS00)
      val countDays = (element: (Month, Series[Day, Double])) => element match { case (month, series) => month -> series.size.toDouble}
      val month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
      val numberOfDays = Series(lim groupBy month map countDays)
      val lookupDate = asof.weekday match {
            case Friday => asof + 3
            case Saturday => asof + 2
            case _ => asof + 1
      }
      val lastNumberOfWorkingDays = numberOfDays(Month(lookupDate) - 1.months)
      val series = numberOfDays ++ Series(Month(asof) to Month(fwdEnd, 12) map (_ -> lastNumberOfWorkingDays))
      A(series, Month(lim.end + 1 ) - 1)
}

lazy val GasCeghCeghixIndexDailyEurMwh = for {

      fwd <- baumgarten

      mon = flatRight(Vwap from CEGHBAUMGARTENAT0000A0DGB9SPOT).filter { case (day, _) => day.weekday match {case Monday => true; case _ => false } }
      tue = flatRight(Vwap from CEGHBAUMGARTENAT0000A0DGC7SPOT).filter { case (day, _) => day.weekday match {case Tuesday => true; case _ => false } }
      wed = flatRight(Vwap from CEGHBAUMGARTENAT0000A0DGD5SPOT).filter { case (day, _) => day.weekday match {case Wednesday => true; case _ => false } }
      thu = flatRight(Vwap from CEGHBAUMGARTENAT0000A0DGE3SPOT).filter { case (day, _) => day.weekday match {case Thursday => true; case _ => false } }
      fri = flatRight(Vwap from CEGHBAUMGARTENAT0000A0DGF0SPOT).filter { case (day, _) => day.weekday match {case Friday => true; case _ => false } }
      weekend = flatRight(Vwap from CEGHBAUMGARTENAT0000A0PQE6SPOT)

} yield A(fwd ++ weekend ++ mon ++ tue ++ wed ++ thu ++ fri, fwd.start -1)

lazy val CoalArgusApi2QuarterlyMidUsdT = for {
      fwd <- coalApi2
      lim = monthly(Index from PA000214000)
} yield A( change(fwd ++ lim) to Quarter, Quarter(fwd.start) - 1)





lazy val DkkUsd = for {
      a <- eurDkk
      b <- eurUsd
} yield a / b



/**
  * DKKO_EUR
  * MONTHLY
  */
lazy val FxDanskebankDkkoMidMonthlyDkkoEur = DailyFwdHist(eurDkk map { _ * 100.0 }, Close from DANSKEBANKEXCHRATEMTHAVGEUR).overrideFwd.map(_.roundTo(2))
/**
  * DKKO_USD
  * MONTHLY
  */
lazy val FxDanskebankDkkoMidMonthlyDkkoUsd = DailyFwdHist(DkkUsd  map { _ * 100.0 }, Close from DANSKEBANKEXCHRATEMTHAVGUSD).overrideFwd.map(_.roundTo(2))


// CONCATENATED CURVES DEPENDENT ON INPUT FROM RDMSMODELS SCALA SCRIPT (NEXT 8 CURVES)

/**
  * EUR_MWH
  * DAILY
  */

lazy val GasTtfLebaWeekendIndexDailyEurMwh = for {
      fwd <- ttf
      hist  <- GasTtfLebaWeekendIndexDailyEurMwhHist
} yield A (fwd ++ hist, hist.end)

/**
  * EUR_MWH
  * DAILY
  */

lazy val GasTtfLebaDaWeekendIndexDailyEurMwh = for {
      fwd <- ttf
      hist  <- GasTtfLebaDaWeekendIndexDailyEurMwhHist
} yield A (fwd ++ hist, hist.end)

/**
  * EUR_MWH
  * DAILY
  */

lazy val GasNcgLebaWeekendIndexDailyEurMwh = for {
      fwd <- ncg
      hist  <- GasNcgLebaWeekendIndexDailyEurMwhHist
} yield A (fwd ++ hist, hist.end)

/**
  * EUR_MWH
  * DAILY
  */

lazy val GasNcgLebaDaWeekendIndexDailyEurMwh = for {
      fwd <- ncg
      hist  <- GasNcgLebaDaWeekendIndexDailyEurMwhHist
} yield A (fwd ++ hist, hist.end)
/**
  * EUR_MWH
  * DAILY
  */

lazy val GasGplLebaDaWeekendIndexDailyEurMwh = for {
      fwd <- gaspool
      hist  <- GasGplLebaDaWeekendIndexDailyEurMwhHist
} yield A (fwd ++ hist, hist.end)
/**
  * EUR_MWH
  * DAILY
  */

lazy val GasNcgLebaDaWeekendWindowIndexDailyEurMwh = for {
      fwd <- ncg
      hist  <- GasNcgLebaDaWeekendWindowIndexDailyEurMwhHist
} yield A (fwd ++ hist, hist.end)


/**
  * GBBPC_THERM
  * MONTHLY
  */

lazy val GasNbpIcePenultMaAvg8decdigitsMonthlyGbbpcTherm = for {
      fwd <- nbp map toMonth map { _ roundTo 8 }
      hist  <- GasNbpIcePenultMaAvgMonthlyGbbpcThermHist map { _ roundTo 8 }
} yield A (fwd ++ hist, hist.end)

/**
  * GBBPC_THERM
  * MONTHLY
  */

lazy val GasNbpIcePenultMaAvg3decdigitsMonthlyGbbpcTherm = for {
      fwd <- nbp map toMonth map { _ roundTo 3 }
      hist  <- GasNbpIcePenultMaAvgMonthlyGbbpcThermHist map { _ roundTo 3 }
} yield A ((fwd ++ hist) , hist.end)


/**
  * no concat, coal is a fullmonth curve, fwd override lim
  * USD/MT
  * monthly
  */
// lazy val CoalArgusApi2MonthlyMidUsdT = for {
//  fwd <- coalApi2
//  lim = monthly(AveragePrice from PA000325400) roundTo 2
// } yield fwd ++ lim
lazy val CoalArgusApi2MonthlyMidUsdT = FwdHist(coalApi2, AveragePrice from PA000325400).overrideFwd

/**
  * @todo gap, get approval, gap is already filled in CoalBafaHistEur, lim needs to override fwd here
  * EUR/t SKE
  * monthly
  */
// lazy val CoalBafaDrittlandskohleMidMonthlyEurTske = for {
//  fwd <- coalBafa map {
//     change(_) to Quarter } map {
//      change(_) to Month }
//  lim = monthly(Val from BAFA) roundTo 2
// } yield fwd ++ lim
lazy val CoalBafaDrittlandskohleMidMonthlyEurTske = FwdHist(coalBafa map toQuarterToMonth, Val from BAFA).overrideFwd map { bafa =>
      bafa.copy(series = bafa.series.roundTo(2))
}

/**
  * GBP/EUR
  * monthly
  */
lazy val FxEcbGbpMidMonthlyGbpEur = /*for {
fwd <- eurGbp
lim = omit(Spot from ECBGBP).roundTo(8)
} yield change(fwd ++ lim) to Month*/ DailyFwdHist(eurGbp, Spot from ECBGBP).overrideFwd.map(_.roundTo(8))

/**
  * USD/EUR
  * monthly
  */
lazy val FxEcbUsdMidMonthlyUsdEur = /*for {
fwd <- eurUsd
lim = omit(Spot from ECBUSD) roundTo 8
} yield change(fwd ++ lim) to Month*/ DailyFwdHist(eurUsd, Spot from ECBUSD).overrideFwd.map(_.roundTo(4))

/**
  * EUR/MWh
  * monthly
  */
lazy val GasGplHerenMthindexMonthlyEurMwh = /*for {
fwd <- gaspool map { change(_) to Month }
lim = monthly(Value from HERENGASPOOLMTHINDEX) roundTo 3
} yield fwd ++ lim*/ FwdHist(gaspool map toMonth, Value from HERENGASPOOLMTHINDEX).overrideFwd

/**
  * EUR/MWh
  * daily
  */
lazy val GasGplEex1mwDaMidDailyEurMwh = /*for {
fwd <- gaspool
day = omit(Close from EEXGPLMW01WDDELIVDATE)
weekend = flatRight(Close from PNXTGASGPLTRADEDATEWE)
lim = weekend ++ day
} yield fwd ++ lim*/ DailyFwdHistWithWeekend(gaspool, Close from PNXTGASGPLDA,Close from PNXTGASGPLTRADEDATEWE).overrideFwd

/**
  * TODO define splitAt
  * EUR/MWh
  * monthly
  */
lazy val GasGplEexEgixMaShiftedMidMonthlyEurMwh = {
      val histStart = Day(Year(asof) - 1) - month
      implicit def limIter = LimIterator(asof iterator histStart)
      for {
            fwd <- gaspool map { change(_) to Month }
            lim = omit(Val from EGIXINDEXMONTHGASPOOL)
            month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
            takeSecondLast = (s: Series[Day, Double]) => tryOption { s.dropRight(1).lastValue }
            withSecondLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeSecondLast(series)}
      } yield A(fwd ++ Interpolate.omit(Series(lim groupBy month map withSecondLast).lag(-1)), fwd.start - 1.months)
}
/**
  * pc/therm
  * monthly
  */
lazy val GasNbpHerenMthindexMonthlyGbppcTherm = /*for {
fwd <- nbp map { change(_) to Month }
lim = monthly(Value from HERENNBPMTHINDEX) roundTo 8
} yield fwd ++ lim*/ FwdHist(nbp map toMonth, Value from HERENNBPMTHINDEX).overrideFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasNcgHerenMthindexMonthlyEurMwh = /*for {
fwd <- ncg map { change(_) to Month }
lim = monthly(Value from HERENNCGMTHINDEX) roundTo 8
} yield fwd ++ lim*/ FwdHist(ncg map toMonth, Value from HERENNCGMTHINDEX).overrideFwd

/**
  * EUR/MWh
  * daily
  */
lazy val GasNcgEex1mwDaMidDailyEurMwh = /*for {
fwd <- ncg
day = omit(Close from EEXNCGMW01WDDELIVDATE)
weekend = flatRight(Close from PNXTGASNCGTRADEDATEWE)
lim = weekend ++ day
} yield fwd ++ lim*/ DailyFwdHistWithWeekend(ncg, Close from PNXTGASNCGDA,Close from PNXTGASNCGTRADEDATEWE).overrideFwd

/**
  * TODO define splitAt
  * EUR/MWh
  * monthly
  */
lazy val GasNcgEexEgixMaShiftMidMonthlyEurMwh = {
      val histStart = Day(Year(asof) - 1) - month
      implicit def limIter = LimIterator(asof iterator histStart)
      for {
            fwd <- ncg map { change(_) to Month }
            lim = omit(Val from EGIXINDEXMONTHNCG)
            month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
            takeSecondLast = (s: Series[Day, Double]) => tryOption { s.dropRight(1).lastValue }
            withSecondLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeSecondLast(series)}
      } yield A(fwd ++ Interpolate.omit(Series(lim groupBy month map withSecondLast).lag(-1)),fwd.start - 1.months)
}
/**
  * EUR/MWh
  * monthly
  */
lazy val GasTtfHerenMthindexMonthlyEurMwh = /*for {
fwd <- ttf map { change(_) to Month }
lim = monthly(Value from HERENTTFMTHINDEX) roundTo 8
} yield fwd ++ lim*/ FwdHist(ttf map toMonth, Value from HERENTTFMTHINDEX).overrideFwd

/**
  * USD/BBL
  * monthly
  */
lazy val OilPlattsBrentDatedMidMonthlyUsdBbl = for {
      hgh <- FwdHist(datedBrent, High from PCAAS03).overrideFwd
      sum <- hgh + FwdHist(datedBrent, Low  from PCAAS03).overrideFwd
} yield sum / 2


/**
  * USD/MT
  * monthly
  */
lazy val OilPlattsFo1CifMedMidMonthlyUsdT = for {
      hgh <- FwdHist(fo1CifMedCargoes, High from PUAAJ03).overrideFwd
      sum <- hgh + FwdHist(fo1CifMedCargoes, Low  from PUAAJ03).overrideFwd
} yield sum / 2


/**
  * USD/MT
  * monthly
  */
lazy val OilPlattsFo1CifNweMidMonthlyUsdT = for {
      hgh <- FwdHist(fo1CifNweCargoes, High from PUAAL03).overrideFwd
      sum <- hgh + FwdHist(fo1CifNweCargoes, Low  from PUAAL03).overrideFwd
} yield sum / 2

/**
  * USD/MT
  * monthly
  */
lazy val OilPlattsFo1FobAraMidMonthlyUsdT = for {
      hgh <- FwdHist(fo1FobAraBarges, High from PUAAP03).overrideFwd
      sum <- hgh + FwdHist(fo1FobAraBarges, Low  from PUAAP03).overrideFwd
} yield sum / 2

/**
  * USD/MT
  * monthly
  */
lazy val OilPlattsGo01CifMedMidMonthlyUsdT = for {
      hgh <- FwdHist(go01CifMedCargoes, High from AAVJJ03).overrideFwd
      sum <- hgh + FwdHist(go01CifMedCargoes, Low  from AAVJJ03).overrideFwd
} yield sum / 2

/**
  * USD/MT
  * monthly
  */
lazy val OilPlattsGo01CifNweMidMonthlyUsdT = for {
      hgh <- FwdHist(go01CifNweCargoes, High from AAYWS03).overrideFwd
      sum <- hgh + FwdHist(go01CifNweCargoes, Low  from AAYWS03).overrideFwd
} yield sum / 2

/**
  * USD/MT
  * monthly
  */
lazy val OilPlattsGo01FobAraMidMonthlyUsdT = for {
      hgh <- FwdHist(go01FobAraBarges, High from AAYWT03).overrideFwd
      sum <- hgh + FwdHist(go01FobAraBarges, Low  from AAYWT03).overrideFwd
} yield sum / 2

/**
  * @TODO does fwd have the same uom
  * USD/MT
  * monthly
  */
lazy val OilPlattsGo50PpmFobAraMidMonthlyUsdT = for {
      hgh <- FwdHist(go50Ppm, High from AAUQC03).overrideFwd
      sum <- hgh + FwdHist(go50Ppm, Low  from AAUQC03).overrideFwd
} yield sum / 2


/**
  * fwd fills the gap and has some lim, but doesn't go back long enough
  * HelRheinHl for 40-50 hl per delivery, 3mo is a shorthand for 3 Marktorte/market places
  * review 0.0845 used in fwd curve in OilEodFc.HelRheinHl and also in gap
  * EUR/hl
  * monthly
  */
lazy val OilStabuaHelRhein3mo4050HlMonthlyEurHl = /*for {
fwd <- hel
lim = monthly(MthAvg from STATSBUNDELIGHTFUELOILCONSUMERRHEINSCHIENE) roundTo 2
} yield fwd ++ lim*/ FwdHist(hel, MthAvg from STATSBUNDELIGHTFUELOILCONSUMERRHEINSCHIENE ).overrideFwdRoundTo2

/**
  * fwd fills the gap and has some lim, but doesn't go back long enought
  * EUR/t
  * monthly
  */
lazy val OilStabuaHslDeutschlandMonthlyEurT = /*for {
fwd <- hsl
lim = monthly(MthAvg from STATSBUNDEHEAVYFUELOILSUPPLYDEUTSCHLAND) roundTo 2
} yield fwd ++ lim*/ FwdHist(hsl, MthAvg from STATSBUNDEHEAVYFUELOILSUPPLYDEUTSCHLAND).overrideFwdRoundTo2

/**
  * EUR/MWh
  * daily
  */
lazy val GasTtfEexDayRefpriceDailyEurMwh = {

      val oldteex = RefPrice from EEXTTFDRPDELIVDATE
      val newtpnxt = Index from PNXTGASTTFDA
      val pnxttpatch = oldteex.until(Day(2018, 1, 1)) ++ newtpnxt.from(Day(2018, 1, 1)) //note that until is exclusive
      DailyFwdHist(ttf, pnxttpatch).overrideFwdKeepDaily
}

/**
  * EUR/MWh
  * daily
  */
lazy val GasTtfHerenDayWeekendMidMonthlyEurMwh = /*for {
fwd <- ttf
day = omit(FwdMid1day from ESGMTTF)
weekend = flatRight(FwdMidwknd from ESGMTTF)
lim = weekend ++ day
} yield fwd ++ lim*/ DailyFwdHistWithWeekend(ttf, FwdMid1day from ESGMTTF, FwdMidwknd from ESGMTTF).overrideFwd

/**
  * EUR/MWh
  * daily
  */
lazy val GasNcgEexDayRefpriceDailyEurMwh = {

      val oldeex = RefPrice from EEXNCGDRPDELIVDATE
      val newpnxt = Index from PNXTGASNCGDA
      val pnxtpatch = oldeex.until(Day(2018, 1, 1)) ++ newpnxt.from(Day(2018, 1, 1)) //note that until is exclusive
      DailyFwdHist(ncg, pnxtpatch).overrideFwdKeepDaily
}

/**
  * EUR/MWh
  * daily
  */
lazy val GasNcgHerenDayWeekendMidDailyEurMwh = /*for {
fwd <- ncg
day = omit(FwdMid1day from ESGMEGT)
weekend = flatRight(FwdMidwknd from ESGMEGT)
lim = weekend ++ day
} yield fwd ++ lim*/ DailyFwdHistWithWeekend(ncg, FwdMid1day from ESGMEGT, FwdMidwknd from ESGMEGT).overrideFwd

/**
  * EUR/MWh
  * daily
  */
lazy val GasGplEexDayRefpriceDailyEurMwh = {

      val oldgeex = RefPrice from EEXGPLDRPDELIVDATE
      val newgpnxt = Index from PNXTGASGPLDA
      val pnxtgpatch = oldgeex.until(Day(2018, 1, 1)) ++ newgpnxt.from(Day(2018, 1, 1)) //note that until is exclusive
      DailyFwdHist(gaspool, pnxtgpatch).overrideFwdKeepDaily
}

/**
  * EUR/MWh
  * daily
  */
lazy val GasGplHerenDayWeekendMidDailyEurMwh = /*for {
fwd <- gaspool
day = omit(FwdMid1day from ESGMBEB)
weekend = flatRight(FwdMidwknd from ESGMBEB)
lim = weekend ++ day
} yield fwd ++ lim*/ DailyFwdHistWithWeekend(gaspool, FwdMid1day from ESGMBEB, FwdMidwknd from ESGMBEB).overrideFwd

// from 2nd Iteration
/**
  * EUR/MWh
  * monthly
  */
lazy val GasPsvPlattsDaMidMonthlyEurMwh = for {
      hgh <- DailyFwdHist(psv, High from GPVTD00).overrideFwd
      sum <- hgh + DailyFwdHist(psv, Low  from GPVTD00).overrideFwd
} yield sum / 2


/**
  * EUR/MWh
  * monthly
  */
lazy val GasNcgHerenMaMidMonthlyEurMwh = /*for {
fwd <- ncg map (change(_) to Month) map (_ lag 1)
lim = omit(FwdMid01mo from ESGMEGT)
} yield change((change(fwd) to Day).from(lim.end) ++ lim) to Month*/ DailyFwdHist(ncg, FwdMid01mo from ESGMEGT).overrideLaggedFwd

/**
  * USD/EUR
  * daily
  */
lazy val FxEcbUsdMidDailyUsdEur = /*for {
fwd <- eurUsd
lim = omit(Spot from ECBUSD)
} yield fwd ++ lim*/ DailyFwdHist(eurUsd,Spot from ECBUSD).overrideFwdKeepDaily

/**
  * USD/t
  * monthly
  */
lazy val OilPlattsFo1FobAraBidMonthlyUsdT = /*for {
fwd <- fo1FobAraBarges map { _ - 0.25 }
lim = omit(Low from PUAAP00)
} yield concatenate(fwd, lim)*/ FwdHist(fo1FobAraBarges map { _ - 0.25 }, Low from PUAAP03).overrideFwd

/**
  * EUR/hl
  * monthly
  */
lazy val eonOilHelFrankFull = /*for {
fwd <- OilStabuaHelFrank4050HlMonthlyEurHl
lim = monthly(MthAvg from STATSBUNDELIGHTFUELOILCONSUMERFRANKFURTAMMAIN)
} yield fwd ++ lim*/ FwdHist(OilStabuaHelFrank4050HlMonthlyEurHl map (_.series), MthAvg from STATSBUNDELIGHTFUELOILCONSUMERFRANKFURTAMMAIN).overrideFwdRoundTo2

/**
  * EUR/hl
  * monthly
  */
lazy val eonOilHelMannLudFull = /*for {
fwd <- OilStabuaHelMannLud4050HlMonthlyEurHl
lim = monthly(MthAvg from STATSBUNDELIGHTFUELOILCONSUMERMANNHEIMLUDWIGSHAFEN)
} yield fwd ++ lim*/ FwdHist(OilStabuaHelMannLud4050HlMonthlyEurHl map (_.series), MthAvg from STATSBUNDELIGHTFUELOILCONSUMERMANNHEIMLUDWIGSHAFEN).overrideFwdRoundTo2

/**
  * EUR/hl
  * monthly
  */
lazy val eonOilHelDuessFull = /*for {
fwd <- OilStabuaHelDuess4050HlMonthlyEurHl
lim = monthly(MthAvg from STATSBUNDELIGHTFUELOILCONSUMERDUSSELDORF)
} yield fwd ++ lim*/ FwdHist(OilStabuaHelDuess4050HlMonthlyEurHl map (_.series), MthAvg from STATSBUNDELIGHTFUELOILCONSUMERDUSSELDORF).overrideFwdRoundTo2

/**
  * %
  * monthly
  */
lazy val IndexBrdStabuaGewerblErzeugInvestBasis2010Percent = /*for {
fwd <- EonIndexBrdGewerblicheErzeugnisse
lim = monthly(MthAvg from STATSBUNDEPRODUCTSCAPITALGOODSBASE2010)
} yield fwd ++ lim*/ FwdHist(EonIndexBrdGewerblicheErzeugnisse, MthAvg from STATSBUNDEPRODUCTSCAPITALGOODSBASE2010).overrideFwd

/**
  * %
  * monthly
  */
lazy val IndexBrdStabuaGrosshandelVerkaufGesamtBasis2005Percent = /*for {
fwd <- EonIndexBrdGrosshandVerkauf2005
lim = monthly(Index from STATSBUNDEWHOLESALEOVERALLINDEX2010)
} yield fwd ++ lim*/ FwdHist(EonIndexBrdGrosshandVerkauf2005, Index from STATSBUNDEWHOLESALEOVERALLINDEX2010).overrideFwd

lazy val IndexBrdStabuaGrosshandelVerkaufGesamtBasis2010Percent = IndexBrdStabuaGrosshandelVerkaufGesamtBasis2005Percent

/**
  * %
  * monthly
  * Lim is quoted quarterly
  */
lazy val IndexBrdStabuaMonatsgehhaltTarifD35EnergieversBasis2010Percent = /*for {
fwd <- EonIndexBrdTarifMonatsgehEnergieversMf2010
lim = quarterly(Index from STATSBUNDENEGOTIATEDMONTHLYEARNINGSEXCLBONUSENERGYSUPPLY2010)
} yield fwd ++ change(lim).to(Month)*/ FwdHist(EonIndexBrdTarifMonatsgehEnergieversMf2010, Index from STATSBUNDENEGOTIATEDMONTHLYEARNINGSEXCLBONUSENERGYSUPPLY2010).overrideFwdQuarterly

/**
  * %
  * monthly
  * Lim is quoted quarterly
  */
lazy val IndexBrdStabuaStundengehhaltTarifD35EnergieversBasis2010Percent = /*for {
fwd <- EonIndexBrdTarifStundenEnergievers2010
lim = quarterly(Index from STATSBUNDEAGREEDHOURLYEARNINGSEXCLBONUSENERGYSUPPLY2010)
} yield fwd ++ change(lim).to(Month)*/ FwdHist(EonIndexBrdTarifStundenEnergievers2010, Index from STATSBUNDEAGREEDHOURLYEARNINGSEXCLBONUSENERGYSUPPLY2010).overrideFwdQuarterly

/**
  * %
  * monthly
  */
lazy val IndexNlCbsCpiNonderivedBasis2006Percent = /*for {
fwd <- EonIndexNlConsumerPriceNder2006
lim = monthly(CpIndex from CBSCPINLNETHERLAND_2006)
} yield fwd ++ lim*/ FwdHist(EonIndexNlConsumerPriceNder2006, CpIndex from CBSCPINLNETHERLAND_2006).overrideFwd

/**
  * %
  * monthly
  */
lazy val IndexNlCbsPpiSbi2008CManufacturingDomesticBasis2010Percent = /*for {
fwd <- EonIndexNlProducerPriceSbi20082010
lim = monthly(DomesticOutputPrices from CBSPPINLBYEACMANUFACTURINGMONTHLY2010)
} yield fwd ++ lim*/ FwdHist(EonIndexNlProducerPriceSbi20082010, DomesticOutputPrices from CBSPPINLBYEACMANUFACTURINGMONTHLY2010).overrideFwd

/**
  * %
  * monthly
  */
lazy val IndexNlCbsCaoWagesMonthlySic4041Basis2000Percent = /*for {
fwd <- EonIndexNlPrijsCaoIonenEnergie2000
lim = monthly(MonthlyValue from CBSLABORELECGASWATERSUPPLYCAOWAGESPERMONTHINCLSPECIALPAYMENTSTOTAL2000)
} yield fwd ++ lim*/ FwdHist(EonIndexNlPrijsCaoIonenEnergie2000, MonthlyValue from CBSLABORELECGASWATERSUPPLYCAOWAGESPERMONTHINCLSPECIALPAYMENTSTOTAL2000).overrideFwd

/**
  * %
  * monthly
  */
lazy val IndexNlCbsPpiProdcom25DomesticBasis2010Percent = /*for {
fwd <- EonIndexNlProducerPriceProdcom252010
lim = monthly(DomesticOutputPrices from CBSPPINLBYPDT25FABRICATEDMETALPRODUCTSEXMACHINERYEQUIPMENTMONTHLY2010)
} yield fwd ++ lim*/ FwdHist(EonIndexNlProducerPriceProdcom252010, DomesticOutputPrices from CBSPPINLBYPDT25FABRICATEDMETALPRODUCTSEXMACHINERYEQUIPMENTMONTHLY2010).overrideFwd

/**
  * %
  * monthly
  */
lazy val IndexNlCbsPpiProdcom2711DomesticBasis2010Percent = /*for {
fwd <- EonIndexNlProducerPriceProdcom27112010
lim = monthly(DomesticOutputPrices from CBSPPINLBYPDT2711ELECTRICMOTORSGENERATORSTRANSFORMERSMONTHLY2010)
} yield fwd ++ lim*/ FwdHist(EonIndexNlProducerPriceProdcom27112010, DomesticOutputPrices from CBSPPINLBYPDT2711ELECTRICMOTORSGENERATORSTRANSFORMERSMONTHLY2010).overrideFwd

/**
  * pc/therm
  * monthly
  */
lazy val GasNbpIceMaMidMonthlyGbppcTherm = /*for {
fwd <- nbp map (change(_) to Month) map (_ lag 1)
lim = omit(Close from IPENBP)
} yield change((change(fwd) to Day).from(lim.end) ++ lim) to Month*/ DailyFwdHist(nbp, Close from IPENBP).overrideLaggedFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasNcgHerenDaAskMonthlyEurMwh = /*for {
fwd <- ncg
lim = omit(FwdOfr1day from ESGMEGT)
} yield change(fwd ++ lim) to Month*/ DailyFwdHist(ncg, FwdOfr1day from ESGMEGT).overrideFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasNcgHerenDaBidMonthlyEurMwh = /*for {
fwd <- ncg
lim = omit(FwdBid1day from ESGMEGT)
} yield change(fwd ++ lim) to Month*/ DailyFwdHist(ncg, FwdBid1day from ESGMEGT).overrideFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasNcgHerenMaIndexMonthlyEurMwh = /*for {
fwd <- ncg map (change(_) to Month) map (_ lag 1)
lim = omit(Close from HERENNCGDAILYMONTHAHEADINDEX)
} yield change((change(fwd) to Day).from(lim.end) ++ lim) to Month*/ DailyFwdHist(ncg, Close from HERENNCGDAILYMONTHAHEADINDEX).overrideLaggedFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasNcgHerenMaAskMonthlyEurMwh = /*for {
fwd <- ncg map (change(_) to Month) map (_ lag 1)
lim = omit(FwdOfr01mo from ESGMEGT)
} yield change((change(fwd) to Day).from(lim.end) ++ lim) to Month*/ DailyFwdHist(ncg, FwdOfr01mo from ESGMEGT).overrideLaggedFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasNcgHerenMaBidMonthlyEurMwh = /*for {
fwd <- ncg map (change(_) to Month) map (_ lag 1)
lim = omit(FwdBid01mo from ESGMEGT)
} yield change((change(fwd) to Day).from(lim.end) ++ lim) to Month*/ DailyFwdHist(ncg, FwdBid01mo from ESGMEGT).overrideLaggedFwd

/**
  * USD/t
  * monthly
  */
lazy val OilPlattsGo01FobAraAskMonthlyUsdT = /*for {
fwd <- go01FobAraBarges map { _ + 0.25 }
lim = omit(High from AAYWT00)
} yield concatenate(fwd, lim)*/ FwdHist(go01FobAraBarges map { _ + 0.25 }, High from AAYWT03).overrideFwd

/**
  * USD/t
  * monthly
  */
lazy val OilPlattsGo01FobAraBidMonthlyUsdT = /*for {
fwd <- go01FobAraBarges map { _ - 0.25 }
lim = omit(Low from AAYWT00)
} yield concatenate(fwd, lim)*/ FwdHist(go01FobAraBarges map { _ - 0.25 }, Low from AAYWT03).overrideFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasTtfHerenMaIndexMonthlyEurMwh = /*for {
fwd <- ttf map (change(_) to Month) map (_ lag 1)
lim = omit(Close from HERENTTFDAILYMONTHAHEADINDEX)
} yield change((change(fwd) to Day).from(lim.end) ++ lim) to Month*/ DailyFwdHist(ttf, Close from HERENTTFDAILYMONTHAHEADINDEX).overrideLaggedFwd

for {
      fwd <- ttf map (change(_) to Month) map (_ lag 1)
      lim = omit(Close from HERENTTFDAILYMONTHAHEADINDEX)
} yield change((change(fwd) to Day) ++ lim) to Month


/**
  * EUR/MWh
  * monthly
  */
lazy val GasTtfHerenMaAskMonthlyEurMwh = /*for {
fwd <- ttf map (change(_) to Month) map (_ lag 1)
lim = omit(FwdOfr01mo from ESGMTTF)
} yield change((change(fwd) to Day).from(lim.end) ++ lim) to Month*/ DailyFwdHist(ttf, FwdOfr01mo from ESGMTTF).overrideLaggedFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasTtfHerenMaBidMonthlyEurMwh = /*for {
fwd <- ttf map (change(_) to Month) map (_ lag 1)
lim = omit(FwdBid01mo from ESGMTTF)
} yield change((change(fwd) to Day).from(lim.end) ++ lim) to Month*/ DailyFwdHist(ttf, FwdBid01mo from ESGMTTF).overrideLaggedFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasGplArgusMaIndexMonthlyEurMwh = /*for {
fwd <- gaspool map (change(_) to Month) map (_ lag 1)
lim = omit(Index from PA000333661)
month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
takeLast = (s: Series[Day, Double]) => tryOption { s.lastValue }
withLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeLast(series)}
history = Interpolate.omit(Series(lim groupBy month map withLast)
} yield A(fwd ++ history, history.end)*/ DailyFwdHist(gaspool, Index from PA000333661).overrideFwdGroupedByMonth

/**
  * EUR/MWh
  * monthly
  */
lazy val GasNcgHerenDaIndexMonthlyEurMwh = /*for {
fwd <- ncg
lim = omit(Close from HERINXEGTDAYACUMULATIVE)
} yield change(fwd ++ lim) to Month*/ DailyFwdHist(ncg, Close from HERINXEGTDAYACUMULATIVE).overrideFwdGroupedByMonthNolag

/**
  * EUR/MWh
  * Monthly
  */
lazy val GasTtfArgusMaIndexMonthlyEurMwh = /*for {
fwd <- ttf map (change(_) to Month) map (_ lag 1)
lim = omit(Index from PA000333461)
month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
takeLast = (s: Series[Day, Double]) => tryOption { s.lastValue }
withLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeLast(series)}
} yield fwd ++ Interpolate.omit(Series(lim groupBy month map withLast))*/ DailyFwdHist(ttf, Index from PA000333461).overrideFwdGroupedByMonth

/**
  * EUR/MWh
  * Monthly
  */
lazy val GasTtfLebaDaIndexMonthlyEurMwh = /*for {
fwd <- ttf
lim = omit(WtdAverage from LEBADAYAHEADTTFINDEX)
} yield change(fwd ++ lim) to Month*/ DailyFwdHist(ttf, WtdAverage from LEBADAYAHEADTTFINDEX).overrideFwd

/**
  * EUR/MWh
  * Monthly
  */
lazy val GasNcgLebaDaIndexMonthlyEurMwh = /*for {
fwd <- ncg
lim = omit(WtdAverage from LEBADAYAHEADEGTINDEX)
} yield change(fwd ++ lim) to Month*/ DailyFwdHist(ncg, WtdAverage from LEBADAYAHEADEGTINDEX).overrideFwd

/**
  * EUR/MWh
  * Monthly
  */
lazy val GasNcgLebaDaWindowIndexMonthlyEurMwh = /*for {
fwd <- ncg
lim = omit(WtdAverage from LEBADAYAHEADWINDOWEGTINDEX)
} yield change(fwd ++ lim) to Month*/ DailyFwdHist(ncg, WtdAverage from LEBADAYAHEADWINDOWEGTINDEX).overrideFwd

/**
  * EUR / MWh
  * Monthly
  */
lazy val GasNcgLebaMaIndexMonthlyEurMwh = /*for {
fwd <- ncg map (change(_) to Month) map (_ lag 1)
lim = omit(WtdAverage from LEBAMONTHAHEADEGTINDEX)
} yield change((change(fwd) to Day).from(lim.end) ++ lim) to Month*/  DailyFwdHist(ncg, WtdAverage from LEBAMONTHAHEADEGTINDEX).overrideLaggedFwd

/**
  * USD / t
  * Monthly
  */
lazy val OilPlattsUlsd10PpmFobAraMidMonthlyUsdT = /*for {
fwd <- ulsd10Ppm
lim = omit(Close from AAJUS00)
} yield concatenate(fwd, lim)*/ FwdHist(ulsd10Ppm, Close from AAJUS00).concat

/**
  * EUR/MWh
  * Daily
  */
lazy val GasCeghHerenDayWeekendMidMonthlyEurMwh = /*for {
fwd <- baumgarten
day = omit(FwdMid1day from ESGMBAUM)
weekend = flatRight(FwdMid1wkn from ESGMBAUM)
lim = weekend ++ day
} yield fwd ++ lim*/ DailyFwdHistWithWeekend(baumgarten, FwdMid1day from ESGMBAUM, FwdMid1wkn from ESGMBAUM).overrideFwd

/**
  * EUR/MWh
  * Monthly
  */
lazy val GasCeghHerenMthindexMonthlyEurMwh = /*for {
fwd <- baumgarten map { change(_) to Month }
lim = monthly(Value from HERENCEGHMTHINDEX) roundTo 8
} yield fwd ++ lim*/ FwdHist(baumgarten map toMonth, Value from HERENCEGHMTHINDEX).overrideFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasPegnHerenMthindexMonthlyEurMwh = /*for {
fwd <- pegNord map { change(_) to Month }
lim = monthly(Value from HERENPEGNORDMTHINDEX) roundTo 8
} yield fwd ++ lim*/ FwdHist(pegNord map toMonth, Value from HERENPEGNORDMTHINDEX).overrideFwd

/**
  * EUR/MWh
  * monthly
  */
lazy val GasPsvHerenMthindexMonthlyEurMwh = /*for {
fwd <- psv map { change(_) to Month }
lim = monthly(Value from HERENPSVMTHINDEX) roundTo 8
} yield fwd ++ lim*/ FwdHist(psv map toMonth, Value from HERENPSVMTHINDEX).overrideFwd

/**
  * pc/therm
  * monthly
  */
lazy val GasZeeHerenMthindexMonthlyGbppcTherm = /*for {
fwd <- zee map { change(_) to Month }
lim = monthly(Value from HERENZEEBRUGGEMTHINDEX) roundTo 8
} yield fwd ++ lim*/ FwdHist(zee map toMonth, Value from HERENZEEBRUGGEMTHINDEX).overrideFwd

/**
  * EUR/MWh
  * daily
  */
lazy val gasNptfGaspointnordicDaMidDailyEurMwh = /*for {
fwd <- nptf
lim = omit(Close from NORDGASNORDICDAYAHEAD) roundTo 8
} yield fwd ++ lim*/ DailyFwdHist(nptf, Close from NORDGASNORDICDAYAHEAD).overrideFwdKeepDaily

/**
  * EUR/MWh
  * monthly
  */
lazy val gasNptfGaspointnordicDaMidMonthlyEurMwh = /*for {
fwd <- nptf
lim = omit(Close from NORDGASNORDICDAYAHEAD) roundTo 8
} yield change(fwd ++ lim) to Month*/ DailyFwdHist(nptf, Close from NORDGASNORDICDAYAHEAD).overrideFwd

// ###############################################################################################
// ### EPOS, all in EURc
// ###

/**
  * EURc/KWh
  * monthly
  */
lazy val GasCeghHerenMthindexMonthlyEurcKwh = GasCeghHerenMthindexMonthlyEurMwh map { _ * 0.1 }

/**
  * EURc/KWh
  * monthly
  */
lazy val GasPegnHerenMthindexMonthlyEurcKwh = GasPegnHerenMthindexMonthlyEurMwh map { _ * 0.1 }

/**
  * EURc/KWh
  * monthly
  */
lazy val GasPsvHerenMthindexMonthlyEurcKwh = GasPsvHerenMthindexMonthlyEurMwh map { _ * 0.1 }

/**
  * EURc/KWh
  * monthly
  */
lazy val gasNptfGaspointnordicDaMidMonthlyEurcKwh = gasNptfGaspointnordicDaMidMonthlyEurMwh map { _ * 0.1 }

/**
  * EURc/KWh
  * monthly
  * therm = 0,0293071 MWh
  */
lazy val GasZeeHerenMthindexMonthlyEurcKwh = /*for {
A(zee, _) <- GasZeeHerenMthindexMonthlyGbppcTherm
A(fx, _) <- FxEcbGbpMidMonthlyGbpEur
thermKWh = 29.3071
} yield zee / fx / thermKWh*/ GasZeeHerenMthindexMonthlyGbppcTherm flatMap { _ / FxEcbGbpMidMonthlyGbpEur } map { _ / thermKWh }

/**
  * EURc/t
  * monthly
  */
lazy val OilPlattsGo01FobAraMidMonthlyEurcT = /*for {
A(go, _) <- OilPlattsGo01FobAraMidMonthlyUsdT
A(fx, _) <- FxEcbUsdMidMonthlyUsdEur
} yield go / fx * 100d*/ OilPlattsGo01FobAraMidMonthlyUsdT flatMap { _ / FxEcbUsdMidMonthlyUsdEur } map { _ * 100.0 }

/**
  * EURc/t
  * monthly
  */
lazy val OilPlattsFo1FobAraMidMonthlyEurcT = /*for {
A(fo, _) <- OilPlattsFo1FobAraMidMonthlyUsdT
A(fx, _) <- FxEcbUsdMidMonthlyUsdEur
} yield fo / fx * 100d*/ OilPlattsFo1FobAraMidMonthlyUsdT flatMap { _ / FxEcbUsdMidMonthlyUsdEur } map { _ * 100.0 }

/**
  * EURc/t
  * monthly
  */
lazy val OilPlattsGo01CifMedMidMonthlyEurcT = /*for {
A(go, _) <- OilPlattsGo01CifMedMidMonthlyUsdT
A(fx, _) <- FxEcbUsdMidMonthlyUsdEur
} yield go / fx * 100d*/ OilPlattsGo01CifMedMidMonthlyUsdT flatMap { _ / FxEcbUsdMidMonthlyUsdEur } map { _ * 100.0 }

/**
  * EURc/t
  * monthly
  */
lazy val OilPlattsFo1CifMedMidMonthlyEurcT = /*for {
A(fo, _) <- OilPlattsFo1CifMedMidMonthlyUsdT
A(fx, _) <- FxEcbUsdMidMonthlyUsdEur
} yield fo / fx * 100d*/ OilPlattsFo1CifMedMidMonthlyUsdT flatMap { _ / FxEcbUsdMidMonthlyUsdEur } map { _ * 100.0 }

/**
  * EURc/t
  * monthly
  */
lazy val CoalArgusApi2MonthlyMidEurcT = /*for {
coal <- CoalArgusApi2MonthlyMidUsdT
fx <- FxEcbUsdMidMonthlyUsdEur
} yield coal / fx * 100d*/ CoalArgusApi2MonthlyMidUsdT flatMap { _ / FxEcbUsdMidMonthlyUsdEur } map { _ * 100.0 }

/**
  * EURc/BBL
  * monthly
  */
lazy val OilPlattsBrentDatedMidMonthlyEurcBbl = /*for {
oil <- OilPlattsBrentDatedMidMonthlyUsdBbl
fx <- FxEcbUsdMidMonthlyUsdEur
} yield oil / fx * 100d*/ OilPlattsBrentDatedMidMonthlyUsdBbl flatMap { _ / FxEcbUsdMidMonthlyUsdEur } map { _ * 100.0 }

/**
  * EURc/KWh
  * monthly
  * therm = 0,0293071 MWh
  */
lazy val GasNbpHerenMthindexMonthlyEurcKwh = /*for {
gas <- GasNbpHerenMthindexMonthlyGbppcTherm
fx <- FxEcbGbpMidMonthlyGbpEur
thermKWh = 29.3071
} yield gas / fx / thermKWh*/ GasNbpHerenMthindexMonthlyGbppcTherm flatMap { _ / FxEcbGbpMidMonthlyGbpEur } map { _ / thermKWh }

/**EUR/T*/
lazy val go01FobAraEur = for {
      fwd <- go_0_1_fob_ara_usd //USD/T
      A(fx, _) <- FxEcbUsdMidMonthlyUsdEur //EURUSD
} yield A(fwd / fx )

/**EURC/KWH*/
lazy val gasNcgEurc = for {
      fwd <- gas_ncg_eur//EUR/MWH
} yield A(fwd * 0.1)

/**EURC/KWH*/
lazy val gasPegnEurc = for {
      fwd <- gas_pegn_eur//EUR/MWH
} yield A(fwd * 0.1)

/**EURC/KWH*/
lazy val gasCeghEurc = for {
      fwd <- gas_cegh_eur//EUR/MWH
} yield A(fwd * 0.1)

/**EURC/KWH*/
lazy val gasTtfEurc = for {
      fwd <- gas_ttf_eur//EUR/MWH
} yield A(fwd * 0.1)

/**EURC/KWH*/
lazy val gasPsvEurc = for {
      fwd <- gas_psv_eur//EUR/MWH
} yield A(fwd * 0.1)

/**EUR/T*/
lazy val coalApi2Eur = for {
      fwd <- coal_api2_usd//USD/T
      A(fx, _) <- FxEcbUsdMidMonthlyUsdEur //EURUSD
} yield A(fwd / fx)

/**
  * EURC/KWH
  * therm = 0,0293071 MWh
  */
lazy val gasNbpEurc = for {
      fwd <- gas_nbp_pth//PENCE/THM
      A(fx, _) <- FxEcbGbpMidMonthlyGbpEur
} yield A(fwd / fx / thermKWh)

/**EUR/T*/
lazy val fo1FobAraEur = for {
      fwd <- fo_1_fob_ara_usd//USD/T
      A(fx, _) <- FxEcbUsdMidMonthlyUsdEur //EURUSD
} yield A(fwd / fx)

/**EURC/KWH*/
lazy val gasGaspoolEurc = for {
      fwd  <- gas_gaspool_eur//EUR/MWH
} yield A(fwd * 0.1)

/**
  * EURC/KWH
  * therm = 0,0293071 MWh
  */
lazy val gasZeeEurc = for {
      fwd  <- gas_zee_pth//PENCE/THM
      A(fx, _) <- FxEcbGbpMidMonthlyGbpEur
} yield A(fwd / fx / thermKWh)

/**
  * EUR/BBL
  */
lazy val brentDatedEur = for {
      fwd  <- brent_dated_usd//USD/BBL
      A(fx, _) <- FxEcbUsdMidMonthlyUsdEur //EURUSD
} yield A(fwd / fx)

// ###############################################################################################
// ### ENDUR GO
// ###

lazy val OilStabuaHelMannLud4050HlMonthlyEurHl = for {
      A(helConcat, splitAt) <- OilStabuaHelRhein3mo4050HlMonthlyEurHl
      base <- hel
      man <- helMannLwghHl
      spread = (base - man).firstValue  // 0.15
} yield A(helConcat - spread,  splitAt)

lazy val OilStabuaHelFrank4050HlMonthlyEurHl = for {
      A(helConcat, splitAt) <- OilStabuaHelRhein3mo4050HlMonthlyEurHl
      base <- hel
      fra <- helFraHl
      spread = (fra - base).firstValue  // 0.43
} yield A(helConcat + spread, splitAt)

lazy val OilStabuaHelDuess4050HlMonthlyEurHl = for {
      A(helConcat, splitAt) <- OilStabuaHelRhein3mo4050HlMonthlyEurHl
      base <- hel
      dus <- helDusHl
      spread = (base - dus).firstValue // 0.28
} yield A(helConcat - spread, splitAt)

lazy val OilStabuaHelHamburg4050HlMonthlyEurHl = for {
      // A(helConcat, splitAt) <- OilStabuaHelRhein3mo4050HlMonthlyEurHl
      fwd <- hel
      spread = 0.10 // 0.10
} yield FwdHist(Some(fwd - spread), MthAvg from STATSBUNDELIGHTFUELOILCONSUMERHAMBURG).overrideFwdRoundTo2.get

/**
  * Stefan Schneider provided a spread for 500t < 50hl
  * review 0.0845 used in fwd curve in OilEodFc.HelRheinHl and also in gap
  * the supplay.stock lim symbol is appropriate for 500t delivery
  * the consumer lim symbol used in PtEod.scala is appropriate for 50hl delivery
  * hence, discount has to be applied to the fwd curve only
  * EUR/hl
  * monthly
  */
lazy val OilStabuaHelRhein3mo500TMonthlyEurHl = for {
      fwd <- hel
      fwd500T <- helRhein500tHl
      discount = (fwd - fwd500T).firstValue // 2.60 EUR/hl
} yield FwdHist(Some(fwd - discount), MthAvg from STATSBUNDELIGHTFUELOILSUPPLYSTOCKRHEINSCHIENE).overrideFwdRoundTo2.get

lazy val OilStabuaHelDuess500TMonthlyEurHl = for {
      fwd <- hel
      _ <- helRhein500tHl
      discount = 2.55 // 2.55 EUR/hl
} yield FwdHist(Some(fwd - discount), MthAvg from STATSBUNDELIGHTFUELOILSUPPLYSTOCKDUSSELDORF).overrideFwdRoundTo2.get

lazy val OilStabuaHelFrank500TMonthlyEurHl = for {
      fwd <- hel
      _ <- helRhein500tHl
      discount = 2.35 // 2.35 EUR/hl
} yield FwdHist(Some(fwd - discount), MthAvg from STATSBUNDELIGHTFUELOILSUPPLYSTOCKFRANKFURTAMMAIN).overrideFwdRoundTo2.get

lazy val OilStabuaHelHamburg500TMonthlyEurHl = for {
      fwd <- hel
      _ <- helRhein500tHl
      discount = 2.90 // 2.90 EUR/hl
} yield FwdHist(Some(fwd - discount), MthAvg from STATSBUNDELIGHTFUELOILSUPPLYSTOCKHAMBURG).overrideFwdRoundTo2.get

lazy val OilStabuaHelMannLud500TMonthlyEurHl = for {
      fwd <- hel
      _ <- helRhein500tHl
      discount = 2.43 // 2.43 EUR/hl
} yield FwdHist(Some(fwd - discount), MthAvg from STATSBUNDELIGHTFUELOILSUPPLYSTOCKMANNHEIMLUDWIGSHAFEN).overrideFwdRoundTo2.get

lazy val GasBrdBafaBorderPriceMonthlyEurTj = for {
      fwd <- bafaGas
      conversion = (3.6/1000)
      lim = PriceTJ from BAFAMONTHLYEVOLUTIONBORDERPRICE
      gapFilled = Interpolate.linear(lim ++ fwd.mapValues(value => Some(value / conversion)))
} yield A(change(gapFilled) to Month, Month(lim.end) - 1)

lazy val  GasBrdBafaBorderPriceYearlyEurTj = for {
      A(curve, splitAt) <- GasBrdBafaBorderPriceMonthlyEurTj

      lim = monthly (AnnualAvgPriceTJ from BAFAMONTHLYEVOLUTIONBORDERPRICE)

} yield A(change(curve ++ lim) to Year, Year(splitAt) -1)

lazy val GasBrdBnetzaCompensationEnergyPositiveDailyEurcKwh = for {
      a <- gaspool
      b <- ncg
      c <- ttf
      d <- zeeEur
      secondBestPrice = (a zip b zip c zip d) mapValues {flatten(_).sorted.drop(1).head}
      factor = 1.2
      conversionFactor = 0.1
      positiveUntilOctFrist = Val from GASPOOLCOMPENSATIONENERGYPRICESPRICEFORPOSITIVECOMPENSATIONENERGY until Day(2015, 10, 1)
      positiveFromOctFrist = Val from GASPOOLCOMPENSATIONENERGYPRICESPOSITIVEASOFOCT2015 from Day(2015, 10, 1)
      positive = positiveUntilOctFrist ++ positiveFromOctFrist
} yield DailyFwdHist(Some(secondBestPrice * factor * conversionFactor), positive).overrideFwdKeepDaily.get

lazy val GasBrdBnetzaCompensationEnergyNegativeDailyEurcKwh = for {
      a <- gaspool
      b <- ncg
      c <- ttf
      d <- zeeEur
      secondBestPrice = (a zip b zip c zip d) mapValues {flatten(_).sorted.drop(1).head}
      factor = 0.9
      conversionFactor = 0.1
      negativeUntilOctFrist = Val from GASPOOLCOMPENSATIONENERGYPRICESPRICEFORNEGATIVECOMPENSATIONENERGY until Day(2015, 10, 1)
      negativeFromOctFrist = Val from GASPOOLCOMPENSATIONENERGYPRICESNEGATIVEASOFOCT2015 from Day(2015, 10, 1)
      negative = negativeUntilOctFrist ++ negativeFromOctFrist
} yield DailyFwdHist(Some(secondBestPrice * factor * conversionFactor), negative).overrideFwdKeepDaily.get

lazy val GasBrdBnetzaCompensationEnergyExceedingLowerQuantitiesMonthlyEurcKwh = for {
      A(positive, splitAt) <- GasBrdBnetzaCompensationEnergyPositiveDailyEurcKwh
      A(negative, _) <- GasBrdBnetzaCompensationEnergyNegativeDailyEurcKwh
      monthly = change((positive + negative) / 2.0) to Month
} yield A(monthly, Month(splitAt + 1) - 1)

// ###############################################################################################
// ### Forecasttool
// ###
CoalArgusApi2MonthlyMidUsdT map writeWithTimeStamp(svOut.Concatenated.CoalArgusApi2MonthlyMidUsdT)
CoalBafaDrittlandskohleMidMonthlyEurTske map writeWithTimeStamp(svOut.Concatenated.CoalBafaDrittlandskohleMidMonthlyEurTske)
FxEcbGbpMidMonthlyGbpEur map writeWithTimeStamp(svOut.Concatenated.FxEcbGbpMidMonthlyGbpEur)
FxEcbUsdMidMonthlyUsdEur map writeWithTimeStamp(svOut.Concatenated.FxEcbUsdMidMonthlyUsdEur)
GasGplHerenMthindexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasGplHerenMthindexMonthlyEurMwh)
GasGplEex1mwDaMidDailyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasGplEex1mwDaMidDailyEurMwh)
GasGplEexEgixMaShiftedMidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasGplEexEgixMaShiftedMidMonthlyEurMwh)
GasNbpHerenMthindexMonthlyGbppcTherm map writeWithTimeStamp(svOut.Concatenated.GasNbpHerenMthindexMonthlyGbppcTherm)
GasNcgHerenMthindexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenMthindexMonthlyEurMwh)
GasNcgEex1mwDaMidDailyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgEex1mwDaMidDailyEurMwh)
GasNcgEexEgixMaShiftMidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgEexEgixMaShiftMidMonthlyEurMwh)
GasTtfHerenMthindexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasTtfHerenMthindexMonthlyEurMwh)
OilPlattsBrentDatedMidMonthlyUsdBbl map writeWithTimeStamp(svOut.Concatenated.OilPlattsBrentDatedMidMonthlyUsdBbl)
OilPlattsFo1CifMedMidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsFo1CifMedMidMonthlyUsdT)
OilPlattsFo1CifNweMidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsFo1CifNweMidMonthlyUsdT)
OilPlattsFo1FobAraMidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsFo1FobAraMidMonthlyUsdT)
OilPlattsGo01CifMedMidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsGo01CifMedMidMonthlyUsdT)
OilPlattsGo01CifNweMidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsGo01CifNweMidMonthlyUsdT)
OilPlattsGo01FobAraMidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsGo01FobAraMidMonthlyUsdT)
OilPlattsGo50PpmFobAraMidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsGo50PpmFobAraMidMonthlyUsdT)
OilStabuaHelRhein3mo4050HlMonthlyEurHl map writeWithTimeStamp(svOut.Concatenated.OilStabuaHelRhein3mo4050HlMonthlyEurHl)
OilStabuaHslDeutschlandMonthlyEurT map writeWithTimeStamp(svOut.Concatenated.OilStabuaHslDeutschlandMonthlyEurT)

GasTtfEexDayRefpriceDailyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasTtfEexDayRefpriceDailyEurMwh)
GasTtfHerenDayWeekendMidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasTtfHerenDayWeekendMidMonthlyEurMwh)
GasNcgEexDayRefpriceDailyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgEexDayRefpriceDailyEurMwh)
GasNcgHerenDayWeekendMidDailyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenDayWeekendMidDailyEurMwh)
GasGplEexDayRefpriceDailyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasGplEexDayRefpriceDailyEurMwh)
GasGplHerenDayWeekendMidDailyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasGplHerenDayWeekendMidDailyEurMwh)
FxEcbUsdMidDailyUsdEur map writeWithTimeStamp(svOut.Concatenated.FxEcbUsdMidDailyUsdEur)
GasCeghHerenMthindexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasCeghHerenMthindexMonthlyEurMwh)
GasCeghHerenDayWeekendMidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasCeghHerenDayWeekendMidMonthlyEurMwh)
GasGplArgusMaIndexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasGplArgusMaIndexMonthlyEurMwh)
GasNbpIceMaMidMonthlyGbppcTherm map writeWithTimeStamp(svOut.Concatenated.GasNbpIceMaMidMonthlyGbppcTherm)
GasNcgHerenDaAskMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenDaAskMonthlyEurMwh)
GasNcgHerenDaBidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenDaBidMonthlyEurMwh)
GasNcgHerenDaIndexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenDaIndexMonthlyEurMwh)
GasNcgHerenMaMidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenMaMidMonthlyEurMwh)
GasNcgHerenMaAskMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenMaAskMonthlyEurMwh)
GasNcgHerenMaBidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenMaBidMonthlyEurMwh)
GasNcgHerenMaIndexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenMaIndexMonthlyEurMwh)
GasNcgLebaDaIndexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgLebaDaIndexMonthlyEurMwh)
GasNcgLebaDaWindowIndexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgLebaDaWindowIndexMonthlyEurMwh)
GasNcgLebaMaIndexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNcgLebaMaIndexMonthlyEurMwh)
GasPegnHerenMthindexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasPegnHerenMthindexMonthlyEurMwh)
GasPsvHerenMthindexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasPsvHerenMthindexMonthlyEurMwh)
GasPsvPlattsDaMidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasPsvPlattsDaMidMonthlyEurMwh)
GasTtfArgusMaIndexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasTtfArgusMaIndexMonthlyEurMwh)
GasTtfHerenMaAskMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasTtfHerenMaAskMonthlyEurMwh)
GasTtfHerenMaBidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasTtfHerenMaBidMonthlyEurMwh)
GasTtfHerenMaIndexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasTtfHerenMaIndexMonthlyEurMwh)
GasTtfLebaDaIndexMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasTtfLebaDaIndexMonthlyEurMwh)
GasZeeHerenMthindexMonthlyGbppcTherm map writeWithTimeStamp(svOut.Concatenated.GasZeeHerenMthindexMonthlyGbppcTherm)
IndexBrdStabuaGewerblErzeugInvestBasis2010Percent map writeWithTimeStamp(svOut.Concatenated.IndexBrdStabuaGewerblErzeugInvestBasis2010Percent)
IndexBrdStabuaGrosshandelVerkaufGesamtBasis2005Percent map writeWithTimeStamp(svOut.Concatenated.IndexBrdStabuaGrosshandelVerkaufGesamtBasis2005Percent)
IndexBrdStabuaGrosshandelVerkaufGesamtBasis2010Percent map writeWithTimeStamp(Concatenated_IndexBrdStabuaGrosshandelVerkaufGesamtBasis2010Percent)
IndexBrdStabuaMonatsgehhaltTarifD35EnergieversBasis2010Percent map writeWithTimeStamp(svOut.Concatenated.IndexBrdStabuaMonatsgehhaltTarifD35EnergieversBasis2010Percent)
IndexBrdStabuaStundengehhaltTarifD35EnergieversBasis2010Percent map writeWithTimeStamp(svOut.Concatenated.IndexBrdStabuaStundengehhaltTarifD35EnergieversBasis2010Percent)
IndexNlCbsCpiNonderivedBasis2006Percent map writeWithTimeStamp(svOut.Concatenated.IndexNlCbsCpiNonderivedBasis2006Percent)
IndexNlCbsCaoWagesMonthlySic4041Basis2000Percent map writeWithTimeStamp(svOut.Concatenated.IndexNlCbsCaoWagesMonthlySic4041Basis2000Percent)
IndexNlCbsPpiProdcom25DomesticBasis2010Percent map writeWithTimeStamp(svOut.Concatenated.IndexNlCbsPpiProdcom25DomesticBasis2010Percent)
IndexNlCbsPpiProdcom2711DomesticBasis2010Percent map writeWithTimeStamp(svOut.Concatenated.IndexNlCbsPpiProdcom2711DomesticBasis2010Percent)
IndexNlCbsPpiSbi2008CManufacturingDomesticBasis2010Percent map writeWithTimeStamp(svOut.Concatenated.IndexNlCbsPpiSbi2008CManufacturingDomesticBasis2010Percent)
OilPlattsFo1FobAraBidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsFo1FobAraBidMonthlyUsdT)
OilPlattsGo01FobAraAskMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsGo01FobAraAskMonthlyUsdT)
OilPlattsGo01FobAraBidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsGo01FobAraBidMonthlyUsdT)
eonOilHelDuessFull map writeWithTimeStamp(svOut.Concatenated.OilStabuaHelDuess4050HlMonthlyEurHl)
eonOilHelFrankFull map writeWithTimeStamp(svOut.Concatenated.OilStabuaHelFrank4050HlMonthlyEurHl)
eonOilHelMannLudFull map writeWithTimeStamp(svOut.Concatenated.OilStabuaHelMannLud4050HlMonthlyEurHl)
OilStabuaHelRhein3mo500TMonthlyEurHl map writeWithTimeStamp(svOut.Concatenated.OilStabuaHelRhein3mo500TMonthlyEurHl)
OilPlattsUlsd10PpmFobAraMidMonthlyUsdT map writeWithTimeStamp(svOut.Concatenated.OilPlattsUlsd10PpmFobAraMidMonthlyUsdT)
gasNptfGaspointnordicDaMidDailyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNptfGaspointnordicDaMidDailyEurMwh)
gasNptfGaspointnordicDaMidMonthlyEurMwh map writeWithTimeStamp(svOut.Concatenated.GasNptfGaspointnordicDaMidMonthlyEurMwh)

// ###############################################################################################
// ### EPOS, all in EURc
// ###
GasCeghHerenMthindexMonthlyEurcKwh map writeWithTimeStamp(svOut.Concatenated.GasCeghHerenMthindexMonthlyEurcKwh)
GasPegnHerenMthindexMonthlyEurcKwh map writeWithTimeStamp(svOut.Concatenated.GasPegnHerenMthindexMonthlyEurcKwh)
GasPsvHerenMthindexMonthlyEurcKwh map writeWithTimeStamp(svOut.Concatenated.GasPsvHerenMthindexMonthlyEurcKwh)
GasZeeHerenMthindexMonthlyEurcKwh map writeWithTimeStamp(svOut.Concatenated.GasZeeHerenMthindexMonthlyEurcKwh)
OilPlattsGo01FobAraMidMonthlyEurcT map writeWithTimeStamp(svOut.Concatenated.OilPlattsGo01FobAraMidMonthlyEurcT)
OilPlattsFo1FobAraMidMonthlyEurcT map writeWithTimeStamp(svOut.Concatenated.OilPlattsFo1FobAraMidMonthlyEurcT)
OilPlattsGo01CifMedMidMonthlyEurcT map writeWithTimeStamp(svOut.Concatenated.OilPlattsGo01CifMedMidMonthlyEurcT)
OilPlattsFo1CifMedMidMonthlyEurcT map writeWithTimeStamp(svOut.Concatenated.OilPlattsFo1CifMedMidMonthlyEurcT)
CoalArgusApi2MonthlyMidEurcT map writeWithTimeStamp(svOut.Concatenated.CoalArgusApi2MonthlyMidEurcT)
OilPlattsBrentDatedMidMonthlyEurcBbl map writeWithTimeStamp(svOut.Concatenated.OilPlattsBrentDatedMidMonthlyEurcBbl)
GasNbpHerenMthindexMonthlyEurcKwh map writeWithTimeStamp(svOut.Concatenated.GasNbpHerenMthindexMonthlyEurcKwh)
gasNptfGaspointnordicDaMidMonthlyEurcKwh map writeWithTimeStamp(svOut.Concatenated.GasNptfGaspointnordicDaMidMonthlyEurcKwh)

GasTtfHerenMthindexMonthlyEurMwh map { _ * 0.1 } map writeWithTimeStamp(svOut.Concatenated.GasTtfHerenMthindexMonthlyEurcKwh)
GasNcgHerenMthindexMonthlyEurMwh map { _ * 0.1 } map writeWithTimeStamp(svOut.Concatenated.GasNcgHerenMthindexMonthlyEurcKwh)
GasGplHerenMthindexMonthlyEurMwh map { _ * 0.1 } map writeWithTimeStamp(svOut.Concatenated.GasGplHerenMthindexMonthlyEurcKwh)
OilStabuaHelRhein3mo4050HlMonthlyEurHl map { _ * 100d } map writeWithTimeStamp(svOut.Concatenated.OilStabuaHelRhein3mo4050HlMonthlyEurcHl)
OilStabuaHslDeutschlandMonthlyEurT map { _ * 100d } map writeWithTimeStamp(svOut.Concatenated.OilStabuaHslDeutschlandMonthlyEurcT)
CoalBafaDrittlandskohleMidMonthlyEurTske map { _ * 100d } map writeWithTimeStamp(svOut.Concatenated.CoalBafaDrittlandskohleMidMonthlyEurcTske)

go01FobAraEur  map writeWithTimeStamp(go_0_1_fob_ara_eur)
gasNcgEurc map writeWithTimeStamp(gas_ncg_eurc)
gasPegnEurc map writeWithTimeStamp(gas_pegn_eurc)
gasCeghEurc map writeWithTimeStamp(gas_cegh_eurc)
gasTtfEurc map writeWithTimeStamp(gas_ttf_eurc)
gasPsvEurc map writeWithTimeStamp(gas_psv_eurc)
coalApi2Eur map writeWithTimeStamp(coal_api2_eur)
gasNbpEurc map writeWithTimeStamp(gas_nbp_eurc)
fo1FobAraEur map writeWithTimeStamp(fo_1_fob_ara_eur)
gasGaspoolEurc map writeWithTimeStamp(gas_gaspool_eurc)
gasZeeEurc map writeWithTimeStamp(gas_zee_eurc)
brentDatedEur map writeWithTimeStamp(brent_dated_eur)


FxEcbDkkMidDailyDkkEur map writeWithTimeStamp(Concatenated_FxEcbDkkMidDailyDkkEur)
FxEcbGbpMidDailyGbpEur map writeWithTimeStamp(Concatenated_FxEcbGbpMidDailyGbpEur)
GasCeghHerenWeekendIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasCeghHerenWeekendIndexDailyEurMwh)
GasCeghHerenDaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasCeghHerenDaAskDailyEurMwh)
GasCeghHerenDaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasCeghHerenDaBidDailyEurMwh)
GasCeghHerenDaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasCeghHerenDaMidDailyEurMwh)
GasCeghHerenMaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasCeghHerenMaAskDailyEurMwh)
GasCeghHerenMaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasCeghHerenMaBidDailyEurMwh)
GasCeghHerenWeekendAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasCeghHerenWeekendAskDailyEurMwh)
GasCeghHerenWeekendMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasCeghHerenWeekendMidDailyEurMwh)
GasGplArgusDaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplArgusDaAskDailyEurMwh)
GasGplArgusDaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplArgusDaBidDailyEurMwh)
GasGplArgusMaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplArgusMaIndexDailyEurMwh)
GasGplArgusMaIndexBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplArgusMaIndexBidDailyEurMwh)
GasGplArgusMaIndexAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplArgusMaIndexAskDailyEurMwh)
GasGplArgusWeekendAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplArgusWeekendAskDailyEurMwh)
GasGplArgusWeekendBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplArgusWeekendBidDailyEurMwh)
GasGplEexEgixMaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexEgixMaIndexDailyEurMwh)
GasGplEexEgixDaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexEgixDaIndexDailyEurMwh)
GasGplEexQ01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexQ01MidDailyEurMwh)
GasGplEexQ02MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexQ02MidDailyEurMwh)
GasGplEexQ03MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexQ03MidDailyEurMwh)
GasGplEexQ04MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexQ04MidDailyEurMwh)
GasGplEexDa01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexDa01MidDailyEurMwh)
GasGplEexDa02MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexDa02MidDailyEurMwh)
GasGplEexCyr01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexCyr01MidDailyEurMwh)
GasGplHerenDaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenDaIndexDailyEurMwh)
GasGplHerenMaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenMaAskDailyEurMwh)
GasGplHerenMaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenMaBidDailyEurMwh)
GasGplHerenWeekendIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenWeekendIndexDailyEurMwh)
GasGplHerenWeekendAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenWeekendAskDailyEurMwh)
GasGplHerenWeekendBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenWeekendBidDailyEurMwh)
GasGplHerenCyr01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenCyr01MidDailyEurMwh)
GasGplPlattsDaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplPlattsDaMidDailyEurMwh)
GasGplHerenDaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenDaAskDailyEurMwh)
GasGplHerenDaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenDaBidDailyEurMwh)
GasGplHerenDaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplHerenDaMidDailyEurMwh)
GasNbpHerenMaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasNbpHerenMaAskDailyEurMwh)
GasNbpHerenMaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNbpHerenMaBidDailyEurMwh)
GasNbpIceMaMidDailyGbppcTherm map writeWithTimeStamp(Concatenated_GasNbpIceMaMidDailyGbppcTherm)
GasNcgArgusMaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgArgusMaIndexDailyEurMwh)
GasNcgHerenDayRefpriceDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenDayRefpriceDailyEurMwh)
GasNcgPnxtDayRefpriceDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgPnxtDayRefpriceDailyEurMwh)
GasGplPnxtDayRefpriceDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplPnxtDayRefpriceDailyEurMwh)
GasTtfPnxtDayRefpriceDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfPnxtDayRefpriceDailyEurMwh)
GasNcgEexEgixMaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexEgixMaIndexDailyEurMwh)
GasNcgEexEgixDaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexEgixDaIndexDailyEurMwh)
GasNcgEexQ01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexQ01MidDailyEurMwh)
GasNcgEexQ02MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexQ02MidDailyEurMwh)
GasNcgEexQ03MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexQ03MidDailyEurMwh)
GasNcgEexQ04MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexQ04MidDailyEurMwh)
GasNcgEexDa01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexDa01MidDailyEurMwh)
GasNcgEexDa02MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexDa02MidDailyEurMwh)
GasNcgEexCyr01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexCyr01MidDailyEurMwh)
GasNcgEexCyr02MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexCyr02MidDailyEurMwh)
GasNcgHerenDaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenDaIndexDailyEurMwh)
GasNcgHerenDaIndexCumulativeDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenDaIndexCumulativeDailyEurMwh)
GasNcgHerenDaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenDaAskDailyEurMwh)
GasNcgHerenDaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenDaBidDailyEurMwh)
GasNcgHerenMaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenMaIndexDailyEurMwh)
GasNcgHerenMaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenMaAskDailyEurMwh)
GasNcgHerenMaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenMaBidDailyEurMwh)
GasNcgHerenQ01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenQ01MidDailyEurMwh)
GasNcgHerenQ04MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenQ04MidDailyEurMwh)
GasNcgHerenSum01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenSum01MidDailyEurMwh)
GasNcgHerenWin01AskDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenWin01AskDailyEurMwh)
GasNcgHerenWin01BidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenWin01BidDailyEurMwh)
GasNcgHerenWin01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenWin01MidDailyEurMwh)
GasNcgHerenWeekendIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenWeekendIndexDailyEurMwh)
GasNcgHerenWeekendAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenWeekendAskDailyEurMwh)
GasNcgHerenWeekendBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenWeekendBidDailyEurMwh)
GasNcgHerenCyrMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenCyrMidDailyEurMwh)
GasNcgLebaDaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgLebaDaIndexDailyEurMwh)
GasNcgLebaDaWindowIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgLebaDaWindowIndexDailyEurMwh)
GasNcgLebaMaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgLebaMaIndexDailyEurMwh)
GasNcgLebaWeekendWindowIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgLebaWeekendWindowIndexDailyEurMwh)
GasNcgPlattsMaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgPlattsMaMidDailyEurMwh)
GasNptfGaspointNordicWithinDayDailyEurMwh map writeWithTimeStamp(Concatenated_GasNptfGaspointNordicWithinDayDailyEurMwh)
GasPegnHerenDaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasPegnHerenDaMidDailyEurMwh)
GasPsvPlattsDaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasPsvPlattsDaMidDailyEurMwh)
GasTtfApxDaCloseDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfApxDaCloseDailyEurMwh)
GasTtfArgusDaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfArgusDaAskDailyEurMwh)
GasTtfArgusDaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfArgusDaBidDailyEurMwh)
GasTtfArgusMaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfArgusMaIndexDailyEurMwh)
GasTtfArgusWeekendAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfArgusWeekendAskDailyEurMwh)
GasTtfArgusWeekendBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfArgusWeekendBidDailyEurMwh)
GasTtfArgusMaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfArgusMaMidDailyEurMwh)
GasTtfEex1mwDaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfEex1mwDaMidDailyEurMwh)
GasTtfHerenDaIndexCumulativeDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenDaIndexCumulativeDailyEurMwh)
GasTtfHerenDaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenDaIndexDailyEurMwh)
GasTtfHerenDaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenDaAskDailyEurMwh)
GasTtfHerenDaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenDaBidDailyEurMwh)
GasTtfHerenGyr01AskDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenGyr01AskDailyEurMwh)
GasTtfHerenGyr01BidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenGyr01BidDailyEurMwh)
GasTtfHerenGyr01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenGyr01MidDailyEurMwh)
GasTtfHerenMaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenMaIndexDailyEurMwh)
GasTtfHerenMaAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenMaAskDailyEurMwh)
GasTtfHerenMaBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenMaBidDailyEurMwh)
GasTtfHerenQ01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenQ01MidDailyEurMwh)
GasTtfHerenQ04MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenQ04MidDailyEurMwh)
GasTtfHerenSum01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenSum01MidDailyEurMwh)
GasTtfHerenWin01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenWin01MidDailyEurMwh)
GasTtfHerenWeekendIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenWeekendIndexDailyEurMwh)
GasTtfHerenWeekendAskDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenWeekendAskDailyEurMwh)
GasTtfHerenWeekendBidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenWeekendBidDailyEurMwh)
GasTtfHerenCyr01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenCyr01MidDailyEurMwh)
GasTtfLebaDaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfLebaDaIndexDailyEurMwh)
GasTtfLebaDaWindowIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfLebaDaWindowIndexDailyEurMwh)
GasTtfLebaMaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfLebaMaIndexDailyEurMwh)
GasZeeArgusDaIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasZeeArgusDaIndexDailyEurMwh)
GasZeePlattsMaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasZeePlattsMaMidDailyEurMwh)
OilPlattsFo1FobAraAskDailyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobAraAskDailyUsdT)
OilPlattsFo1FobAraBidDailyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobAraBidDailyUsdT)
OilPlattsFo1FobAraMidDailyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobAraMidDailyUsdT)
// OilPlattsIceGasoilMaMidDailyUsdT map writeWithTimeStamp(Concatenated_OilPlattsIceGasoilMaMidDailyUsdT)
OilPlattsGo01FobAraAskDailyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01FobAraAskDailyUsdT)
OilPlattsGo01FobAraBidDailyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01FobAraBidDailyUsdT)
OilPlattsGo01FobAraMidDailyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01FobAraMidDailyUsdT)
GasBrdBafaBorderPriceMonthlyEurTj map writeWithTimeStamp(Concatenated_GasBrdBafaBorderPriceMonthlyEurTj)
GasBrdBafaBorderPriceYearlyEurTj map writeWithTimeStamp(Concatenated_GasBrdBafaBorderPriceYearlyEurTj)


OilPlattsBrentDatedAskMonthlyUsdBbl map writeWithTimeStamp(Concatenated_OilPlattsBrentDatedAskMonthlyUsdBbl)
OilPlattsBrentDatedBidMonthlyUsdBbl map writeWithTimeStamp(Concatenated_OilPlattsBrentDatedBidMonthlyUsdBbl)
OilPlattsFo1CifMedAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1CifMedAskMonthlyUsdT)
OilPlattsFo1CifMedBidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1CifMedBidMonthlyUsdT)
OilPlattsFo1CifNweAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1CifNweAskMonthlyUsdT)
OilPlattsFo1CifNweBidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1CifNweBidMonthlyUsdT)
OilPlattsFo1FobAraAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobAraAskMonthlyUsdT)
OilPlattsFo1FobMedAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobMedAskMonthlyUsdT)
OilPlattsFo1FobMedBidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobMedBidMonthlyUsdT)
OilPlattsFo1FobMedMidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobMedMidMonthlyUsdT)
OilPlattsFo1FobNweMidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobNweMidMonthlyUsdT)
OilPlattsFo35FobAraAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo35FobAraAskMonthlyUsdT)
OilPlattsFo35FobAraBidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo35FobAraBidMonthlyUsdT)
OilPlattsFo35FobAraMidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo35FobAraMidMonthlyUsdT)
OilPlattsFo35FobMedAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo35FobMedAskMonthlyUsdT)
OilPlattsFo35FobMedBidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo35FobMedBidMonthlyUsdT)
OilPlattsFo35FobMedMidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsFo35FobMedMidMonthlyUsdT)
OilPlattsGo01CifMedAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01CifMedAskMonthlyUsdT)
OilPlattsGo01CifMedBidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01CifMedBidMonthlyUsdT)
OilPlattsGo01CifNweAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01CifNweAskMonthlyUsdT)
OilPlattsGo01CifNweBidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01CifNweBidMonthlyUsdT)
OilPlattsGo01FobMedAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01FobMedAskMonthlyUsdT)
OilPlattsGo01FobMedBidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01FobMedBidMonthlyUsdT)
OilPlattsGo01FobMedMidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01FobMedMidMonthlyUsdT)
OilPlattsGo01FobNweMidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo01FobNweMidMonthlyUsdT)
OilPlattsGo50PpmFobAraAskMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo50PpmFobAraAskMonthlyUsdT)
OilPlattsGo50PpmFobAraBidMonthlyUsdT map writeWithTimeStamp(Concatenated_OilPlattsGo50PpmFobAraBidMonthlyUsdT)

FxEcbDkkMidMonthlyDkkEur map writeWithTimeStamp(Concatenated_FxEcbDkkMidMonthlyDkkEur)
FxEcbHufMidMonthlyHufEur map writeWithTimeStamp(Concatenated_FxEcbHufMidMonthlyHufEur)
FxEcbCzkMidMonthlyCzkEur map writeWithTimeStamp(Concatenated_FxEcbCzkMidMonthlyCzkEur)
GasPsvHerenDaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasPsvHerenDaMidDailyEurMwh)

GasGplEexMaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexMaMidDailyEurMwh)
GasGplEexSum01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexSum01MidDailyEurMwh)
GasGplEexWin01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplEexWin01MidDailyEurMwh)
GasNcgEexMaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexMaMidDailyEurMwh)
GasNcgEexSum01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexSum01MidDailyEurMwh)
GasNcgEexWin01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgEexWin01MidDailyEurMwh)
GasTtfIceMaMidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfIceMaMidDailyEurMwh)
GasTtfIceQ01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfIceQ01MidDailyEurMwh)
GasTtfIceQ02MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfIceQ02MidDailyEurMwh)
GasTtfIceQ03MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfIceQ03MidDailyEurMwh)
GasTtfIceQ04MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfIceQ04MidDailyEurMwh)
GasTtfIceSum01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfIceSum01MidDailyEurMwh)
GasTtfIceWin01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfIceWin01MidDailyEurMwh)
GasTtfIceCyr01MidDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfIceCyr01MidDailyEurMwh)

GasTtfArgusMaMidMthEndMonthlyEurMwh map writeWithTimeStamp(Concatenated_GasTtfArgusMaMidMthEndMonthlyEurMwh)
GasTtfHerenDaIndexMthEndShiftMonthlyEurMwh map writeWithTimeStamp(Concatenated_GasTtfHerenDaIndexMthEndShiftMonthlyEurMwh)
GasNbpIceMaMthEndMonthlyGbbpcTherm map writeWithTimeStamp(Concatenated_GasNbpIceMaMthEndMonthlyGbbpcTherm)
GasNcgHerenMaIndexMthEndMonthlyEurMwh map writeWithTimeStamp(Concatenated_GasNcgHerenMaIndexMthEndMonthlyEurMwh)
GasZeePlattsMaMthEndMonthlyGbbpcTherm map writeWithTimeStamp(Concatenated_GasZeePlattsMaMthEndMonthlyGbbpcTherm)

OilIceUlsMidDailyUsdT map writeWithTimeStamp(Concatenated_OilIceUlsMidDailyUsdT)

OilPlattsFo1FobAraMidEbv1MonthlyEurT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobAraMidEbv1MonthlyEurT)
OilPlattsFo1FobAraMidEbv2MonthlyEurT map writeWithTimeStamp(Concatenated_OilPlattsFo1FobAraMidEbv2MonthlyEurT)
OilPlattsGo01FobAraMidEbv1MonthlyEurT map writeWithTimeStamp(Concatenated_OilPlattsGo01FobAraMidEbv1MonthlyEurT)
OilPlattsGo50PpmFobAraMidEbv1MonthlyEurT map writeWithTimeStamp(Concatenated_OilPlattsGo50PpmFobAraMidEbv1MonthlyEurT)
OilPlattsGo50PpmFobAraMidEbv2MonthlyEurT map writeWithTimeStamp(Concatenated_OilPlattsGo50PpmFobAraMidEbv2MonthlyEurT)
OilPlattsGo50PpmFobAraMidEbv3MonthlyEurT map writeWithTimeStamp(Concatenated_OilPlattsGo50PpmFobAraMidEbv3MonthlyEurT)
OilPlattsGo50PpmFobAraMidEbv4MonthlyEurT map writeWithTimeStamp(Concatenated_OilPlattsGo50PpmFobAraMidEbv4MonthlyEurT)


IndexNlCbsCpiDerivedBasis2006Percent map writeWithTimeStamp(Concatenated_IndexNlCbsCpiDerivedBasis2006Percent)



WagesAvGweKirchmoeser14thSalaryChristmasBonusEur map writeWithTimeStamp(Concatenated_WagesAvGweKirchmoeser14thSalaryChristmasBonusEur)
WagesAvGwe19014thSalaryChristmasBonusEur map writeWithTimeStamp(Concatenated_WagesAvGwe19014thSalaryChristmasBonusEur)
WagesAvGwe18214thSalaryChristmasBonusEur map writeWithTimeStamp(Concatenated_WagesAvGwe18214thSalaryChristmasBonusEur)
WagesAvGwe78014thSalaryChristmasBonusEur map writeWithTimeStamp(Concatenated_WagesAvGwe78014thSalaryChristmasBonusEur)
WagesAvGwe78914thSalaryChristmasBonusEur map writeWithTimeStamp(Concatenated_WagesAvGwe78914thSalaryChristmasBonusEur)
WagesAvGweAzv14thSalaryChristmasBonusEur map writeWithTimeStamp(Concatenated_WagesAvGweAzv14thSalaryChristmasBonusEur)
WagesAvGweSvCharge14thSalaryChristmasBonusEur map writeWithTimeStamp(Concatenated_WagesAvGweSvCharge14thSalaryChristmasBonusEur)
WagesAvGwe77714thSalaryChristmasBonusEur map writeWithTimeStamp(Concatenated_WagesAvGwe77714thSalaryChristmasBonusEur)
WagesAvGwe14thSalaryChristmasBonusEur map writeWithTimeStamp(Concatenated_WagesAvGwe14thSalaryChristmasBonusEur)
WagesTvOedKommunallohn54MonthEur map writeWithTimeStamp(Concatenated_WagesTvOedKommunallohn54MonthEur)
WagesTvOedKommunallohn54HourEur map writeWithTimeStamp(Concatenated_WagesTvOedKommunallohn54HourEur)

GasTtfLebaWeekendIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfLebaWeekendIndexDailyEurMwh)
GasTtfLebaDaWeekendIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasTtfLebaDaWeekendIndexDailyEurMwh)
GasNcgLebaWeekendIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgLebaWeekendIndexDailyEurMwh)
GasNcgLebaDaWeekendIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgLebaDaWeekendIndexDailyEurMwh)
GasGplLebaDaWeekendIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasGplLebaDaWeekendIndexDailyEurMwh)
GasNcgLebaDaWeekendWindowIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasNcgLebaDaWeekendWindowIndexDailyEurMwh)

GasNbpIcePenultMaAvg8decdigitsMonthlyGbbpcTherm map writeWithTimeStamp(Concatenated_GasNbpIcePenultMaAvg8decdigitsMonthlyGbbpcTherm)
GasNbpIcePenultMaAvg3decdigitsMonthlyGbbpcTherm map writeWithTimeStamp(Concatenated_GasNbpIcePenultMaAvg3decdigitsMonthlyGbbpcTherm)

GasHhubNymexMaMidMonthlyUsdMmbtu map writeWithTimeStamp(Concatenated_GasHhubNymexMaMidMonthlyUsdMmbtu)

FxDanskebankDkkoMidMonthlyDkkoEur map writeWithTimeStamp(Concatenated_FxDanskebankDkkoMidMonthlyDkkoEur)
FxDanskebankDkkoMidMonthlyDkkoUsd map writeWithTimeStamp(Concatenated_FxDanskebankDkkoMidMonthlyDkkoUsd)

CoalArgusApi2QuarterlyMidUsdT map writeWithTimeStamp(Concatenated_CoalArgusApi2QuarterlyMidUsdT)

OilStabuaHelFrank500TMonthlyEurHl map writeWithTimeStamp(Concatenated_OilStabuaHelFrank500TMonthlyEurHl)
OilStabuaHelHamburg500TMonthlyEurHl map writeWithTimeStamp(Concatenated_OilStabuaHelHamburg500TMonthlyEurHl)
OilStabuaHelHamburg4050HlMonthlyEurHl map writeWithTimeStamp(Concatenated_OilStabuaHelHamburg4050HlMonthlyEurHl)
OilStabuaHelMannLud500TMonthlyEurHl map writeWithTimeStamp(Concatenated_OilStabuaHelMannLud500TMonthlyEurHl)
OilStabuaHelDuess500TMonthlyEurHl map writeWithTimeStamp(Concatenated_OilStabuaHelDuess500TMonthlyEurHl)

IndexBrdStabuaGewerblErzeugnisGesamtBasis2010MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaGewerblErzeugnisGesamtBasis2010MonthlyPercent)
//error IndexBrdStabuaMonatsgehaltEnergieBasis2010YearlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaMonatsgehaltEnergieBasis2010YearlyPercent)
IndexBrdStabuaMonatsgehaltEnergieWasserBasis2010MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaMonatsgehaltEnergieWasserBasis2010MonthlyPercent)
IndexBrdStabuaStundengehaltPrivatwirtschaftBasis2010MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaStundengehaltPrivatwirtschaftBasis2010MonthlyPercent)
IndexBrdStabuaStundengehaltBergebauBasis2010MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaStundengehaltBergebauBasis2010MonthlyPercent)
IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2010MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2010MonthlyPercent)
IndexBrdStabuaStundengehaltFbgEnergieBasis2010MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaStundengehaltFbgEnergieBasis2010MonthlyPercent)
//error IndexStabuaErzeugerpreiseGewerbProdBasis2010MonthlyPercent map writeWithTimeStamp(Concatenated_IndexStabuaErzeugerpreiseGewerbProdBasis2010MonthlyPercent)


writeWithTimeStamp(Concatenated_PlattsDailyWeightsMonthlyDays)(PlattsDailyWeightsMonthlyDays)

GasBrdBnetzaCompensationEnergyPositiveDailyEurcKwh map writeWithTimeStamp(Concatenated_GasBrdBnetzaCompensationEnergyPositiveDailyEurcKwh)
GasBrdBnetzaCompensationEnergyNegativeDailyEurcKwh map writeWithTimeStamp(Concatenated_GasBrdBnetzaCompensationEnergyNegativeDailyEurcKwh)
GasBrdBnetzaCompensationEnergyExceedingLowerQuantitiesMonthlyEurcKwh map writeWithTimeStamp(Concatenated_GasBrdBnetzaCompensationEnergyExceedingLowerQuantitiesMonthlyEurcKwh)

GasCeghCeghixIndexDailyEurMwh map writeWithTimeStamp(Concatenated_GasCeghCeghixIndexDailyEurMwh)

// NEW MAIN
IndexBrdStabuaStundengehaltFbgEnergieBasis2015MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaStundengehaltFbgEnergieBasis2015MonthlyPercent)
IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2015MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaStundengehaltFbgEnergieWasserBasis2015MonthlyPercent)
IndexBrdStabuaStundengehaltBergebauBasis2015MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaStundengehaltBergebauBasis2015MonthlyPercent)
IndexBrdStabuaMonatsgehaltEnergieWasserBasis2015MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStabuaMonatsgehaltEnergieWasserBasis2015MonthlyPercent)
IndexBrdJaehrlicherVerdienstEnergieVersorg2015MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdJaehrlicherVerdienstEnergieVersorg2015MonthlyPercent)
IndexBrdMonatlicherVerdienstEnergieVersorg2015MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdMonatlicherVerdienstEnergieVersorg2015MonthlyPercent)
IndexBrdStuendlicherVerdienstEnergieVersorg2015MonthlyPercent map writeWithTimeStamp(Concatenated_IndexBrdStuendlicherVerdienstEnergieVersorg2015MonthlyPercent)