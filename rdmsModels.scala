implicit val svIn = new Service("script", Some(ds), Some(cds)) with Containers with ForwardCurveService with Markets with Lim2

val svOut = svIn
implicit val wb = FO_SUPPORT

val asof = yesterday // running at 02:55 yesterday  //

val histStart = Day(Year(asof) - 6) - month
val fwdEnd = asof.year + 4
val fwdEndDay = Day(fwdEnd, 12, 31)
implicit def limIter = LimIterator(asof iterator histStart)

val omit = Interpolate.omit[Day, Double] _
val flatRight: Series[Day, Option[Double]] => Series[Day, Double] = os => {
  if(omit(os).isEmpty)
    Series.empty[Day,Double]
  else
    Interpolate.flatRight(os)
}

val monthly = omit andThen (change(_) to Month)
val quarterly = omit andThen (change(_) to Quarter)

def tryOption[T](t: => T): Option[T] = try Some(t) catch {

  case t: Throwable =>
    log error t.getMessage
    None
}

def writeWithTimeStamp[A<:DateTimeLike[A]](fwdCurve: Service#Container#ForwardCurveLike[Day, A, Double])(series: Series[A, Double])(implicit optSplitAt: Option[A] = None){

  // cut off everything beyond fwdEndDay
  if(series.nonEmpty){

    val splitAt: A = optSplitAt.getOrElse(series.start.companion(asof))

    val containerBucket = fwdCurve.getOrCreate(asof)
    val it = series.to(series.start.companion(fwdEndDay))
    val noteColumnOverride = (e: (A, Double)) => if(e._1 <= splitAt) "historical" else "forward"
    def toColumns = fwdCurve.toColumns(containerBucket.tradeDate,it.head._1).take(4) :+ noteColumnOverride

    containerBucket.writeToCds(fwdCurve.columnSet)(toColumns)(it)

    val timeStamp = fwdCurve(asof).metaData.get.timeStamp.toString("yyyy-MM-dd HH:mm:ss")
    val column = CString("TimeStamp")
    val timeStampTable = DataTable.fromIterableRow[String](List((column, (s:String) => s)))(List(timeStamp::Nil))

    fwdCurve(asof).container.getOrCreate(s"${fwdCurve(asof).name}_timestamp").writeToCds(timeStampTable)
  }
}

// missing symbols
object LEBAWEEKENDBEBINDEX extends eet.io.Symbol[String] { lazy val symbol = "LEBA.WEEKEND.BEB.INDEX" }
object LEBADAYAHEADBEBINDEX extends eet.io.Symbol[String] { lazy val symbol = "LEBA.DAYAHEAD.BEB.INDEX" }
object EEXGERMANYAUSTRIAPEAKLOADDAYAHEAD2 extends eet.io.Symbol[String] { lazy val symbol = "EEX.GERMANY.AUSTRIA.PEAKLOAD.DAYAHEAD" }
object PA000274762 extends eet.io.Symbol[String] { lazy val symbol = "PA0002747.6.2" }
object PA000274763 extends eet.io.Symbol[String] { lazy val symbol = "PA0002747.6.3" }
object PA000274764 extends eet.io.Symbol[String] { lazy val symbol = "PA0002747.6.4" }
object PA000274765 extends eet.io.Symbol[String] { lazy val symbol = "PA0002747.6.5" }
object EEXEGT_SE3 extends eet.io.Symbol[String] { lazy val symbol = "EEX.EGT_SE3" }
object EEXEGT_SE4 extends eet.io.Symbol[String] { lazy val symbol = "EEX.EGT_SE4" }
object EEXBEB_SE3 extends eet.io.Symbol[String] { lazy val symbol = "EEX.BEB_SE3" }
object EEXBEB_SE4 extends eet.io.Symbol[String] { lazy val symbol = "EEX.BEB_SE4" }
object EEXEGT_MTH5 extends eet.io.Symbol[String] { lazy val symbol = "EEX.EGT_MTH5" }
object EEXBEB_MTH5 extends eet.io.Symbol[String] { lazy val symbol = "EEX.BEB_MTH5" }
object APXZTPENDOFWORKINGDAYDAFLOW extends eet.io.Symbol[String] { lazy val symbol = "APX.ZTP.END.OF.WORKING.DAY.DA.FLOW" }
object GASPOOLCOMPENSATIONENERGYPRICESPRICEFORNEGATIVECOMPENSATIONENERGY extends eet.io.Symbol[String] { lazy val symbol = "GASPOOL.COMPENSATION.ENERGYPRICES.PRICE.FOR.NEGATIVE.COMPENSATION.ENERGY" }
object GASPOOLCOMPENSATIONENERGYPRICESPRICEFORPOSITIVECOMPENSATIONENERGY extends eet.io.Symbol[String] { lazy val symbol = "GASPOOL.COMPENSATION.ENERGYPRICES.PRICE.FOR.POSITIVE.COMPENSATION.ENERGY" }
object ICETTF_02 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_02" }
object ICETTF_03 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_03" }
object ICETTF_04 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_04" }
object ICETTF_05 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_05" }
object ICETTF_06 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_06" }
object ICETTF_SE1 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_SE1" }
object ICETTF_SE2 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_SE2" }
object ICETTF_SE3 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_SE3" }
object ICETTF_SE4 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_SE4" }
object ICETTF_SE5 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_SE5" }
object ICETTF_SE6 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_SE6" }
object ICETTF_YR2 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_YR2" }
object ICETTF_YR3 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_YR3" }
object ICETTF_YR4 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_YR4" }
object ICETTF_YR5 extends eet.io.Symbol[String] { lazy val symbol = "ICE.TTF_YR5" }
object EEXGERMANYBASELOADDAYAHEAD extends Symbol[String] { lazy val symbol = "EEX.GERMANY.BASELOAD.DAYAHEAD" }
object EEXGERMANYPEAKLOADDAYAHEAD3 extends Symbol[String] { lazy val symbol = "EEX.GERMANY.PEAKLOAD.DAYAHEAD" }
object ECBUSD extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object EEXGPLDRPDELIVDATE extends Symbol[String] { lazy val symbol = "EEX.GPL.DRP.DELIV.DATE" }
object EEXGPLMW01WDDELIVDATE extends Symbol[String] { lazy val symbol = "EEX.GPL.MW01.WD.DELIV.DATE" }
object EEXNCGDRPDELIVDATE extends Symbol[String] { lazy val symbol = "EEX.NCG.DRP.DELIV.DATE" }
object EEXNCGMW01WDDELIVDATE extends Symbol[String] { lazy val symbol = "EEX.NCG.MW01.WD.DELIV.DATE" }
object EGIXINDEXMONTHGASPOOL extends Symbol[String] { lazy val symbol = "EGIX.INDEX.MONTH.GASPOOL" }
object EGIXINDEXMONTHNCG extends Symbol[String] { lazy val symbol = "EGIX.INDEX.MONTH.NCG" }
object ESGMBEB extends Symbol[String] { lazy val symbol = "ESGM.BEB" }
object ESGMEGT extends Symbol[String] { lazy val symbol = "ESGM.EGT" }
object ESGMTTF extends Symbol[String] { lazy val symbol = "Heren_TTFPrices_continuous/ts?cols=PRICEPOINT&Contract=TTF_001_GASYear" }
object HERENEGTMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.EGT.MTH.INDEX" }
object HERENNCGDAILYMONTHAHEADINDEX extends Symbol[String] { lazy val symbol = "HEREN.NCG.DAILY.MONTH.AHEAD.INDEX" }
object HERENTTFDAILYMONTHAHEADINDEX extends Symbol[String] { lazy val symbol = "HEREN.TTF.DAILY.MONTH.AHEAD.INDEX" }
object HERENTTFMTHINDEX extends Symbol[String] { lazy val symbol = "HEREN.TTF.MTH.INDEX" }
object HERINXEGTDAYACUMULATIVE extends Symbol[String] { lazy val symbol = "HERINX.EGT.DAYA.CUMULATIVE" }
object IPENBP extends Symbol[String] { lazy val symbol = "IPE.NBP" }
object LEBADAYAHEADEGTINDEX extends Symbol[String] { lazy val symbol = "LEBA.DAYAHEAD.EGT.INDEX" }
object LEBADAYAHEADTTFINDEX extends Symbol[String] { lazy val symbol = "LEBA.DAYAHEAD.TTF.INDEX" }
object LEBADAYAHEADWINDOWEGTINDEX extends Symbol[String] { lazy val symbol = "LEBA.DAYAHEADWINDOW.EGT.INDEX" }
object PA000214000 extends Symbol[String] { lazy val symbol = "PA0002140.0.0" }
object PA000169660 extends Symbol[String] { lazy val symbol = "PA0001696.6.0" }
object PA000273761 extends Symbol[String] { lazy val symbol = "PA0002737.6.1" }
object PA000169760 extends Symbol[String] { lazy val symbol = "PA0001697.6.0" }
object EEXBEB_MTH2 extends Symbol[String] { lazy val symbol = "EEX.BEB_MTH2" }
object EEXBEB_Q1 extends Symbol[String] { lazy val symbol = "EEX.BEB_Q1" }
object EEXBEB_Q2 extends Symbol[String] { lazy val symbol = "EEX.BEB_Q2" }
object EEXBEB_Q3 extends Symbol[String] { lazy val symbol = "EEX.BEB_Q3" }
object EEXBEB_Q4 extends Symbol[String] { lazy val symbol = "EEX.BEB_Q4" }
object EEXBEB_SE1 extends Symbol[String] { lazy val symbol = "EEX.BEB_SE1" }
object EEXBEB_SE2 extends Symbol[String] { lazy val symbol = "EEX.BEB_SE2" }
object EEXBEB_YR1 extends Symbol[String] { lazy val symbol = "EEX.BEB_YR1" }
object EEXEGT_MTH2 extends Symbol[String] { lazy val symbol = "EEX.EGT_MTH2" }
object EEXEGT_Q1 extends Symbol[String] { lazy val symbol = "EEX.EGT_Q1" }
object EEXEGT_Q2 extends Symbol[String] { lazy val symbol = "EEX.EGT_Q2" }
object EEXEGT_Q3 extends Symbol[String] { lazy val symbol = "EEX.EGT_Q3" }
object EEXEGT_Q4 extends Symbol[String] { lazy val symbol = "EEX.EGT_Q4" }
object EEXEGT_SE1 extends Symbol[String] { lazy val symbol = "EEX.EGT_SE1" }
object EEXEGT_SE2 extends Symbol[String] { lazy val symbol = "EEX.EGT_SE2" }
object EEXEGT_YR1 extends Symbol[String] { lazy val symbol = "EEX.EGT_YR1" }
object EEXEGT_YR2 extends Symbol[String] { lazy val symbol = "EEX.EGT_YR2" }
object HERINXEGTDAYA extends Symbol[String] { lazy val symbol = "HERINX.EGT.DAYA" }
object LEBAWEEKENDEGTINDEX extends Symbol[String] { lazy val symbol = "LEBA.WEEKEND.EGT.INDEX" }
object LEBAWEEKENDWINDOWEGTINDEX extends Symbol[String] { lazy val symbol = "LEBA.WEEKENDWINDOW.EGT.INDEX" }
object APXTTFENDOFWORKINGDAYDAFLOW extends Symbol[String] { lazy val symbol = "APX.TTF.END.OF.WORKING.DAY.DA.FLOW" }
object ICETTF extends Symbol[String] { lazy val symbol = "ICE.TTF" }
object ICETTF_Q1 extends Symbol[String] { lazy val symbol = "ICE.TTF_Q1" }
object ICETTF_Q2 extends Symbol[String] { lazy val symbol = "ICE.TTF_Q2" }
object ICETTF_Q3 extends Symbol[String] { lazy val symbol = "ICE.TTF_Q3" }
object ICETTF_Q4 extends Symbol[String] { lazy val symbol = "ICE.TTF_Q4" }
object ICETTF_YR1 extends Symbol[String] { lazy val symbol = "ICE.TTF_YR1" }
object HERINXTTFDAYACUMULATIVE extends Symbol[String] { lazy val symbol = "HERINX.TTF.DAYA.CUMULATIVE" }
object HERINXTTFDAYA extends Symbol[String] { lazy val symbol = "HERINX.TTF.DAYA" }
object LEBAWEEKENDTTFINDEX extends Symbol[String] { lazy val symbol = "LEBA.WEEKEND.TTF.INDEX" }
object EEXGERMANYAUSTRIABASELOADDAYAHEAD extends Symbol[String] { lazy val symbol = "EEX.GERMANY.AUSTRIA.BASELOAD.DAYAHEAD" }
object PA000273061 extends Symbol[String] { lazy val symbol = "PA0002730.6.1" }
object PA000273062 extends Symbol[String] { lazy val symbol = "PA0002730.6.2" }
object PA000273762 extends Symbol[String] { lazy val symbol = "PA0002737.6.2" }
object PA000273763 extends Symbol[String] { lazy val symbol = "PA0002737.6.3" }
object PA000274261 extends Symbol[String] { lazy val symbol = "PA0002742.6.1" }
object PA000274262 extends Symbol[String] { lazy val symbol = "PA0002742.6.2" }
object PA000274263 extends Symbol[String] { lazy val symbol = "PA0002742.6.3" }
object PA000274264 extends Symbol[String] { lazy val symbol = "PA0002742.6.4" }
object PA000274265 extends Symbol[String] { lazy val symbol = "PA0002742.6.5" }
object PA000274266 extends Symbol[String] { lazy val symbol = "PA0002742.6.6" }
object PA000274267 extends Symbol[String] { lazy val symbol = "PA0002742.6.7" }
object PA000274268 extends Symbol[String] { lazy val symbol = "PA0002742.6.8" }
object PA000274269 extends Symbol[String] { lazy val symbol = "PA0002742.6.9" }
object PA000274761 extends Symbol[String] { lazy val symbol = "PA0002747.6.1" }
object EGIXINDEXMONTHDE extends Symbol[String] { lazy val symbol = "EGIX.INDEX.MONTH.DE" }
object EEXEGT_MTH1 extends Symbol[String] { lazy val symbol = "EEX.EGT_MTH1" }
object EEXEGT_MTH3 extends Symbol[String] { lazy val symbol = "EEX.EGT_MTH3" }
object EEXEGT_MTH4 extends Symbol[String] { lazy val symbol = "EEX.EGT_MTH4" }
object EEXEGT_Q5 extends Symbol[String] { lazy val symbol = "EEX.EGT_Q5" }
object EEXEGT_YR3 extends Symbol[String] { lazy val symbol = "EEX.EGT_YR3" }
object EEXEGT_YR4 extends Symbol[String] { lazy val symbol = "EEX.EGT_YR4" }
object EEXBEB_MTH1 extends Symbol[String] { lazy val symbol = "EEX.BEB_MTH1" }
object EEXBEB_MTH3 extends Symbol[String] { lazy val symbol = "EEX.BEB_MTH3" }
object EEXBEB_MTH4 extends Symbol[String] { lazy val symbol = "EEX.BEB_MTH4" }
object EEXBEB_YR2 extends Symbol[String] { lazy val symbol = "EEX.BEB_YR2" }
object EEXBEB_YR3 extends Symbol[String] { lazy val symbol = "EEX.BEB_YR3" }
object EEXBEB_YR4 extends Symbol[String] { lazy val symbol = "EEX.BEB_YR4" }

// missing fields
val WtdAverage_TH = Numerical("WtdAverage_TH")
val Close = Numerical("Close")
val FwdMid01cyr = Numerical("FwdMid01cyr")
val FwdMid01mo = Numerical("FwdMid01mo")
val FwdMid01qt = Numerical("FwdMid01qt")
val FwdMid01sn = Numerical("FwdMid01sn")
val FwdMid02mo = Numerical("FwdMid02mo")
val FwdMid1day = Numerical("FwdMid1day")
val FwdMidwknd = Numerical("FwdMidwknd")
val High = Numerical("High")
val Index = Numerical("Index")
val Low = Numerical("Low")
val RefPrice = Numerical("RefPrice")
val Spot = Numerical("Spot")
val Val = Numerical("Val")
val Value = Numerical("Value")
val WtdAverage = Numerical("WtdAverage")
val FwdMid04qt = Numerical("FwdMid04qt")
val FwdOfr02sn = Numerical("FwdOfr02sn")
val FwdBid01sn = Numerical("FwdBid01sn")
val FwdMid01yr = Numerical("FwdMid01yr")
val FwdMid02sn = Numerical("FwdMid02sn")
val FwdOfr01sn = Numerical("FwdOfr01sn")
val FwdOfr03sn = Numerical("FwdOfr03sn")
val FwdBid03sn = Numerical("FwdBid03sn")
val FwdMid03sn = Numerical("FwdMid03sn")
val FwdOfr05sn = Numerical("FwdOfr05sn")
val FwdBid05sn = Numerical("FwdBid05sn")
val FwdMid05sn = Numerical("FwdMid05sn")
val FwdBid02sn = Numerical("FwdBid02sn")
val FwdOfr04sn = Numerical("FwdOfr04sn")
val FwdBid04sn = Numerical("FwdBid04sn")
val FwdMid04sn = Numerical("FwdMid04sn")
val FwdOfr06sn = Numerical("FwdOfr06sn")
val FwdBid06sn = Numerical("FwdBid06sn")
val FwdMid06sn = Numerical("FwdMid06sn")
val FwdMidBlmo = Numerical("FwdMidBlmo")
val FwdMid02cyr = Numerical("FwdMid02cyr")
val FwdMid02yr = Numerical("FwdMid02yr")
val FwdMid03mo = Numerical("FwdMid03mo")
val FwdMid02qt = Numerical("FwdMid02qt")
val FwdMid03qt = Numerical("FwdMid03qt")
val FwdMid05qt = Numerical("FwdMid05qt")
val FwdMid06qt = Numerical("FwdMid06qt")
val FwdMid03cyr = Numerical("FwdMid03cyr")

// in case forward curve needs to be added quickly
// EEX

lazy val contractDetails: Option[Map[String, Map[Month, Day]]] = EexContractDetails.dataTable map { table =>
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

def settlementCurve(settlementSymbol: String, untilRollOver: Series[Day, Option[Double]], afterRollOver: Series[Day, Option[Double]]): Series[Day, Option[Double]] =
  contractDetails flatMap (_.get(settlementSymbol)) map { rollOverDates =>
    val allSettlementDates = untilRollOver.time ++ afterRollOver.time

    val series = for {
      settlementDate <- allSettlementDates
      rollOverInThatMonth <- rollOverDates.get(settlementDate: Month)
    } yield {
      if(settlementDate <= rollOverInThatMonth)
        settlementDate -> untilRollOver.get(settlementDate).flatten
      else
        settlementDate -> afterRollOver.get(settlementDate).getOrElse(None)
    }
    val series2 = untilRollOver
    val series3 =    Series(series.toSeq)

    series2 ++ series3

  } getOrElse Series.empty

lazy val EexContractDetails = svIn("EEX")("CONTRACT_DETAILS")

val container = new svOut.Container("RDMSMODELS") with svOut.ForwardCurveContainer

//// GAS-X BCM
val RDMS_GasNcgEex1mwDaMidDailyEurcKwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_1MW_DA_MID_DAILY_EURC_KWH_%s")
val RDMS_GasGplEex1mwDaMidDailyEurcKwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_1MW_DA_MID_DAILY_EURC_KWH_%s")
val RDMS_GasTtfEndofWorkingDayIndexDailyEurcKwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_APX_END_OF_WORKING_DAY_INDEX_DAILY_EURC_KWH_%s")
val RDMS_GasZeeEndofWorkingDayIndexDailyEurcKwh = new container.ForwardCurveStream[Day, Day]("GAS_ZEE_APX_END_OF_WORKING_DAY_INDEX_DAILY_EURC_KWH_%s")
val RDMS_GasBrdBnetzaStructuralChargeDailyEurcKwh = new container.ForwardCurveStream[Day, Day]("GAS_BRD_BNETZA_STRUCTURAL_CHARGE_DAILY_EURC_KWH_%s")

//// ERDS
val RDMS_PowerGermanyAustriaEexPhelixDayBaseQuarterlyEurMwh = new container.ForwardCurveStream[Day, Quarter]("POWER_GERMANY_AUSTRIA_EEX_PHELIX_DAY_BASE_QUARTERLY_EUR_MWH_%s")
val RDMS_CoalArgusApi2QuarterlyMidUsdT = new container.ForwardCurveStream[Day, Quarter]("COAL_ARGUS_API2_QUARTERLY_MID_USD_T_%s")
val RDMS_FxEcbUsdMidMonthlyUsdEur = new container.ForwardCurveStream[Day, Month]("FX_ECB_USD_MID_MONTHLY_USD_EUR_%s")

//// Leba
val RDMS_GasNcgLebaWeekendIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasNcgLebaDaWeekendIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_DA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasGplLebaWeekendIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_LEBA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasGplLebaDaWeekendIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_LEBA_DA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasNcgLebaDaWeekendWindowIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_DA_WEEKEND_WINDOW_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasTtfLebaWeekendIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasTtfLebaDaWeekendIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_DA_WEEKEND_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasNcgLebaWeekendIndexDailyGbppcTherm = new container.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_WEEKEND_INDEX_DAILY_GBPPC_THERM_%s")
val RDMS_GasNcgLebaDaWeekendIndexDailyGbppcTherm = new container.ForwardCurveStream[Day, Day]("GAS_NCG_LEBA_DA_WEEKEND_INDEX_DAILY_GBPPC_THERM_%s")
val RDMS_GasGplLebaWeekendIndexDailyGbppcTherm = new container.ForwardCurveStream[Day, Day]("GAS_GPL_LEBA_WEEKEND_INDEX_DAILY_GBPPC_THERM_%s")
val RDMS_GasGplLebaDaWeekendIndexDailyGbppcTherm = new container.ForwardCurveStream[Day, Day]("GAS_GPL_LEBA_DA_WEEKEND_INDEX_DAILY_GBPPC_THERM_%s")
val RDMS_GasTtfLebaWeekendIndexDailyGbppcTherm = new container.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_WEEKEND_INDEX_DAILY_GBPPC_THERM_%s")
val RDMS_GasTtfLebaDaWeekendIndexDailyGbppcTherm = new container.ForwardCurveStream[Day, Day]("GAS_TTF_LEBA_DA_WEEKEND_INDEX_DAILY_GBPPC_THERM_%s")

//// Special
val RDMS_GasNbpIcePenultMaAvgMonthlyGbbpcTherm = new container.ForwardCurveStream[Day, Month]("GAS_NBP_ICE_PENULT_MA_AVG_MONTHLY_GBBPC_THERM_HIST_%s")
val RDMS_PowerGermanyAustriaEexPhelixDayBaseMonthlyEurMwh = new container.ForwardCurveStream[Day, Month]("POWER_GERMANY_AUSTRIA_EEX_PHELIX_DAY_BASE_MONTHLY_EUR_MWH_%s")
val RDMS_PowerGermanyAustriaEexPhelixDayPeakMonthlyEurMwh = new container.ForwardCurveStream[Day, Month]("POWER_GERMANY_AUSTRIA_EEX_PHELIX_DAY_PEAK_MONTHLY_EUR_MWH_%s")
val RDMS_GasNcgEexYa01ShiftMonthlyEurMwh = new container.ForwardCurveStream[Day, Month]("GAS_NCG_EEX_YA01_SHIFT_MONTHLY_EUR_MWH_%s")

//// Seasons
val RDMS_GasNcgHerenSum01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenSum02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenSum03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenSum01BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM01_BID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenSum02BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM02_BID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenSum03BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM03_BID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenSum01AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM01_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenSum02AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM02_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenSum03AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM03_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum01BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM01_BID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum02BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM02_BID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum03BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM03_BID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum01AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM01_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum02AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM02_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum03AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM03_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin01BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN01_BID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin02BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN02_BID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin03BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN03_BID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin01AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN01_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin02AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN02_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin03AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN03_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin01BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN01_BID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin02BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN02_BID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin03BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN03_BID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin01AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN01_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin02AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN02_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin03AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN03_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenSum01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_SUM01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenSum02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_SUM02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenSum03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_SUM03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenSum01BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_SUM01_BID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenSum02BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_SUM02_BID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenSum03BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_SUM03_BID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenSum01AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_SUM01_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenSum02AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_SUM02_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenSum03AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_SUM03_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWin01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WIN01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWin02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WIN02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWin03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WIN03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWin01BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WIN01_BID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWin02BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WIN02_BID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWin03BidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WIN03_BID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWin01AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WIN01_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWin02AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WIN02_ASK_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWin03AskDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WIN03_ASK_DAILY_EUR_MWH_%s")

//// iOPT
val RDMS_GasGplArgusDaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_DA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusWeekendMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_WEEKEND_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusCyr01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_CYR01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusCyr02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_CYR02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusMaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_MA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusMa02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_MA02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusMa03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_MA03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusQ01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_Q01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusQ02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_Q02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusQ03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_Q03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusQ04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_Q04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusQ05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_Q05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusQ06MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_Q06_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusQ07MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_Q07_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusQ08MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_Q08_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusQ09MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_Q09_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusSum01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_SUM01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusSum02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_SUM02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusSum03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_SUM03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusWin01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_WIN01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusWin02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_WIN02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplArgusWin03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_ARGUS_WIN03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCurrentMonthMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CURRENT_MONTH_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCurrentMonthMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CURRENT_MONTH_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenDaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_DA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenWeekendMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_WEEKEND_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenBomMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_BOM_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplHerenMaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_HEREN_MA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenQ01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_Q01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenCyr01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_CYR01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenCyr02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_CYR02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenGyr01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_GYR01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenGyr02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_GYR02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenMa02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_MA02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenMa03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_MA03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenQ01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_Q01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenQ02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_Q02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenQ03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_Q03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenQ04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_Q04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenQ05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_Q05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenQ06MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_Q06_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenCyr01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_CYR01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenCyr02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_CYR02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenCyr03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_CYR03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenDaIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_DA_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenMaIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_MA_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenMaIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_MA_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexSum01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_SUM01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexSum02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_SUM02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexWin01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_WIN01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexWin02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_WIN02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexDaRefpriceDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_DA_REFPRICE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexDaRefpriceDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_DA_REFPRICE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexQ01MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q01_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexMaMidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_MA_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenDaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_Heren_DA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWeekendMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WEEKEND_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenDaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_DA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWeekendMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WEEKEND_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenMaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_MA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenMaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_MA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenDaIndexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_DA_INDEX_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexQ05MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q05_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenDaCumulativeIndexMonthEndDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_DA_CUMULATIVE_INDEX_MONTH_END_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenDaCumulativeIndexMonthEndDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_DA_CUMULATIVE_INDEX_MONTH_END_DAILY_EUR_MWH_%s")
val RDMS_GasGermanyEexEgixMaIndexPenultimateMonthEndDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GERMANY_EEX_EGIX_MA_INDEX_PENULTIMATE_MONTH_END_SHIFT_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexEgixMaIndexPenultimateMonthEndDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_EGIX_MA_INDEX_PENULTIMATE_MONTH_END_SHIFT_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexEgixMaIndexPenultimateMonthEndDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_EGIX_MA_INDEX_PENULTIMATE_MONTH_END_SHIFT_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenMthindexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_MTHINDEX_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenMthindexDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_MTHINDEX_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenSum01MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_SUM01_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgHerenWin01MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_HEREN_WIN01_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum01MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM01_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum02MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM02_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenSum03MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_SUM03_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin01MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN01_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin02MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN02_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasTtfHerenWin03MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_HEREN_WIN03_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")

//// PEGASUS
val RDMS_GasNcgEexQ01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexMaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_MA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexMa02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_MA02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexMa03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_MA03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexQ02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexQ03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexQ04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexQ05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCyr01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCyr02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCyr03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCyr04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCyr05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexSe01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_SE01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexSe02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_SE02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexSe03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_SE03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexSe04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_SE04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexSe05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_SE05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexSe01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_SE01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexSe02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_SE02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexSe03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_SE03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexSe04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_SE04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexSe05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_SE05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexMa02MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_MA02_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexMa03MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_MA03_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexQ02MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q02_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexQ03MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q03_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexQ04MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_Q04_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCyr01MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR01_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCyr02MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR02_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCyr03MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR03_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexCyr04MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_CYR04_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexMa04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_MA04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexMa04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_MA04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasNcgEexMa05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_NCG_EEX_MA05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexMa05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_MA05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexMaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_MA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexMa02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_MA02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexMa03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_MA03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexQ01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexQ02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexQ03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexQ04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexQ05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCyr01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCyr02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCyr03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCyr04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCyr05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexMaMidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_MA_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexMa02MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_MA02_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexMa03MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_MA03_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexQ01MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q01_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexQ02MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q02_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexQ03MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q03_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexQ04MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_Q04_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCyr01MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR01_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCyr02MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR02_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCyr03MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR03_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasGplEexCyr04MidFlatrightWeDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_GPL_EEX_CYR04_MID_FLATRIGHT_WE_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceMaMidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_MA_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceMa02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_MA02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceMa03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_MA03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceMa04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_MA04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceMa05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_MA05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceMa06MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_MA06_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceQ01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_Q01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceQ02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_Q02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceQ03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_Q03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceQ04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_Q04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceSe01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_SE01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceSe02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_SE02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceSe03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_SE03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceSe04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_SE04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceSe05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_SE05_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceSe06MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_SE06_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceCyr01MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_CYR01_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceCyr02MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_CYR02_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceCyr03MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_CYR03_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceCyr04MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_CYR04_MID_DAILY_EUR_MWH_%s")
val RDMS_GasTtfIceCyr05MidDailyEurMwh = new container.ForwardCurveStream[Day, Day]("GAS_TTF_ICE_CYR05_MID_DAILY_EUR_MWH_%s")

//////////////// Model Output

val GasTtfLebaWeekendIndexDailyEurMwh = omit(WtdAverage from LEBAWEEKENDTTFINDEX).filter { case (day, _) => day.weekday match {case Friday => true; case _ => false } }

val GasTtfLebaDaWeekendIndexDailyEurMwh = {
  val dayAhead = omit(WtdAverage from LEBADAYAHEADTTFINDEX).lag(-1)

  val flatFriday = flatRight(Series((GasTtfLebaWeekendIndexDailyEurMwh.start to GasTtfLebaWeekendIndexDailyEurMwh.end).map( _ -> None)) ++ GasTtfLebaWeekendIndexDailyEurMwh.mapValues(Some(_)))

  flatFriday ++ dayAhead
}

val GasTtfLebaWeekendIndexDailyGbppcTherm = omit(WtdAverage_TH from LEBAWEEKENDTTFINDEX).filter { case (day, _) => day.weekday match {case Friday => true; case _ => false } }

val GasTtfLebaDaWeekendIndexDailyGbppcTherm = {
  val dayAhead = omit(WtdAverage_TH from LEBADAYAHEADTTFINDEX).lag(-1)

  val flatFriday = flatRight(Series((GasTtfLebaWeekendIndexDailyGbppcTherm.start to GasTtfLebaWeekendIndexDailyGbppcTherm.end).map( _ -> None)) ++ GasTtfLebaWeekendIndexDailyGbppcTherm.mapValues(Some(_)))

  flatFriday ++ dayAhead
}

val GasNcgLebaWeekendIndexDailyEurMwh = omit(WtdAverage from LEBAWEEKENDEGTINDEX).filter { case (day, _) => day.weekday match {case Friday => true; case _ => false } }

val GasNcgLebaDaWeekendIndexDailyEurMwh = {
  val dayAhead = omit(WtdAverage from LEBADAYAHEADEGTINDEX).lag(-1)

  val flatFriday = flatRight(Series((GasNcgLebaWeekendIndexDailyEurMwh.start to GasNcgLebaWeekendIndexDailyEurMwh.end).map( _ -> None)) ++ GasNcgLebaWeekendIndexDailyEurMwh.mapValues(Some(_)))

  flatFriday ++ dayAhead
}
val GasNcgLebaWeekendIndexDailyGbppcTherm = omit(WtdAverage_TH from LEBAWEEKENDEGTINDEX).filter { case (day, _) => day.weekday match {case Friday => true; case _ => false } }

val GasNcgLebaDaWeekendIndexDailyGbppcTherm = {
  val dayAhead = omit(WtdAverage_TH from LEBADAYAHEADEGTINDEX).lag(-1)

  val flatFriday = flatRight(Series((GasNcgLebaWeekendIndexDailyGbppcTherm.start to GasNcgLebaWeekendIndexDailyGbppcTherm.end).map( _ -> None)) ++ GasNcgLebaWeekendIndexDailyGbppcTherm.mapValues(Some(_)))

  flatFriday ++ dayAhead
}

val GasNcgLebaWeekendWindowIndexDailyEurMwh = omit(WtdAverage from LEBAWEEKENDWINDOWEGTINDEX).filter { case (day, _) => day.weekday match {case Friday => true; case _ => false } }

val GasNcgLebaDaWeekendWindowIndexDailyEurMwh = {
  val dayAhead = omit(WtdAverage from LEBADAYAHEADWINDOWEGTINDEX).lag(-1)

  val flatFriday = flatRight(Series((GasNcgLebaWeekendWindowIndexDailyEurMwh.start to GasNcgLebaWeekendWindowIndexDailyEurMwh.end).map( _ -> None)) ++ GasNcgLebaWeekendWindowIndexDailyEurMwh.mapValues(Some(_)))

  flatFriday ++ dayAhead
}

val GasGplLebaWeekendIndexDailyEurMwh = omit(WtdAverage from LEBAWEEKENDBEBINDEX).filter { case (day, _) => day.weekday match {case Friday => true; case _ => false } }

val GasGplLebaDaWeekendIndexDailyEurMwh = {
  val dayAhead = omit(WtdAverage from LEBADAYAHEADBEBINDEX).lag(-1)

  val flatFriday = flatRight(Series((GasGplLebaWeekendIndexDailyEurMwh.start to GasGplLebaWeekendIndexDailyEurMwh.end).map( _ -> None)) ++ GasGplLebaWeekendIndexDailyEurMwh.mapValues(Some(_)))

  flatFriday ++ dayAhead
}

val GasGplLebaWeekendIndexDailyGbppcTherm = omit(WtdAverage_TH from LEBAWEEKENDBEBINDEX).filter { case (day, _) => day.weekday match {case Friday => true; case _ => false } }

val GasGplLebaDaWeekendIndexDailyGbppcTherm = {
  val dayAhead = omit(WtdAverage_TH from LEBADAYAHEADBEBINDEX).lag(-1)

  val flatFriday = flatRight(Series((GasGplLebaWeekendIndexDailyGbppcTherm.start to GasGplLebaWeekendIndexDailyGbppcTherm.end).map( _ -> None)) ++ GasGplLebaWeekendIndexDailyGbppcTherm.mapValues(Some(_)))

  flatFriday ++ dayAhead
}

val GasNbpIcePenultMaAvgMonthlyGbbpcTherm = {
  val lim = omit(Close from IPENBP).lag(-1)
  (change(lim) to Month).lag(-1)
}

////  Seasons SUMMER

val GasNcgHerenSum01MidDailyEurMwh = {
  val a = omit(FwdMid02sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid01sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenSum02MidDailyEurMwh = {
  val a = omit(FwdMid04sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid03sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenSum03MidDailyEurMwh = {

  val a = omit(FwdMid05sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

val GasNcgHerenSum01BidDailyEurMwh = {
  val a = omit(FwdBid02sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid01sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenSum02BidDailyEurMwh = {
  val a = omit(FwdBid04sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid03sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenSum03BidDailyEurMwh = {

  val a  = omit(FwdBid05sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

val GasNcgHerenSum01AskDailyEurMwh = {
  val a = omit(FwdOfr02sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr01sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenSum02AskDailyEurMwh = {
  val a = omit(FwdOfr04sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr03sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenSum03AskDailyEurMwh = {

  val a = omit(FwdOfr05sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

val GasTtfHerenSum01BidDailyEurMwh = {
  val a = omit(FwdBid02sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid01sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum02BidDailyEurMwh = {
  val a = omit(FwdBid04sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid03sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum03BidDailyEurMwh = {
  val a = omit(FwdBid06sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid05sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}
val GasTtfHerenSum01AskDailyEurMwh = {
  val a = omit(FwdOfr02sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr01sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum02AskDailyEurMwh = {
  val a = omit(FwdOfr04sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr03sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum03AskDailyEurMwh = {
  val a = omit(FwdOfr06sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr05sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum01MidDailyEurMwh = {
  val a = omit(FwdMid02sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid01sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum02MidDailyEurMwh = {
  val a = omit(FwdMid04sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid03sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum03MidDailyEurMwh = {
  val a = omit(FwdMid06sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid05sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenSum01MidDailyEurMwh = {
  val a = omit(FwdMid02sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid01sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenSum02MidDailyEurMwh = {
  val a = omit(FwdMid04sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid03sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenSum03MidDailyEurMwh = {

  val a = omit(FwdMid05sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

val GasGplHerenSum01BidDailyEurMwh = {
  val a = omit(FwdBid02sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid01sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenSum02BidDailyEurMwh = {
  val a = omit(FwdBid04sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid03sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenSum03BidDailyEurMwh = {

  val a  = omit(FwdBid05sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

val GasGplHerenSum01AskDailyEurMwh = {
  val a = omit(FwdOfr02sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr01sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenSum02AskDailyEurMwh = {
  val a = omit(FwdOfr04sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr03sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenSum03AskDailyEurMwh = {

  val a = omit(FwdOfr05sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

////  Seasons Winter

val GasNcgHerenWin01MidDailyEurMwh = {
  val a = omit(FwdMid01sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid02sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenWin02MidDailyEurMwh = {
  val a = omit(FwdMid03sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid04sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenWin03MidDailyEurMwh = {

  val a = omit(FwdMid05sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}


val GasNcgHerenWin01BidDailyEurMwh = {
  val a = omit(FwdBid01sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid02sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenWin02BidDailyEurMwh = {
  val a = omit(FwdBid03sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid04sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenWin03BidDailyEurMwh = {
  val a = omit(FwdBid05sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

val GasNcgHerenWin01AskDailyEurMwh = {
  val a = omit(FwdOfr01sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr02sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenWin02AskDailyEurMwh = {
  val a = omit(FwdOfr03sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr04sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenWin03AskDailyEurMwh = {
  val a = omit(FwdOfr05sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

val GasTtfHerenWin01BidDailyEurMwh = {
  val a = omit(FwdBid01sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid02sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenWin02BidDailyEurMwh = {
  val a = omit(FwdBid03sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid04sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenWin03BidDailyEurMwh = {
  val a = omit(FwdBid05sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid06sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}
val GasTtfHerenWin01AskDailyEurMwh = {
  val a = omit(FwdOfr01sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr02sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenWin02AskDailyEurMwh = {
  val a = omit(FwdOfr03sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr04sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenWin03AskDailyEurMwh = {
  val a = omit(FwdOfr05sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr06sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}
val GasTtfHerenWin01MidDailyEurMwh = {
  val a = omit(FwdMid01sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid02sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenWin02MidDailyEurMwh = {
  val a = omit(FwdMid03sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid04sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenWin03MidDailyEurMwh = {
  val a = omit(FwdMid05sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid06sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenWin01MidDailyEurMwh = {
  val a = omit(FwdMid01sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid02sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenWin02MidDailyEurMwh = {
  val a = omit(FwdMid03sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdMid04sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenWin03MidDailyEurMwh = {

  val a = omit(FwdMid05sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

val GasGplHerenWin01BidDailyEurMwh = {
  val a = omit(FwdBid01sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid02sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenWin02BidDailyEurMwh = {
  val a = omit(FwdBid03sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdBid04sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenWin03BidDailyEurMwh = {
  val a = omit(FwdBid05sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

val GasGplHerenWin01AskDailyEurMwh = {
  val a = omit(FwdOfr01sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr02sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenWin02AskDailyEurMwh = {
  val a = omit(FwdOfr03sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = omit(FwdOfr04sn from ESGMBEB) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplHerenWin03AskDailyEurMwh = {
  val a = omit(FwdOfr05sn from ESGMBEB) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a)
}

//// Special
val PowerGermanyAustriaEexPhelixDayBaseMonthlyEurMwh = {
  val oldeex = Index from EEXGERMANYAUSTRIABASELOADDAYAHEAD
  val neweex = Index from EEXGERMANYBASELOADDAYAHEAD
  val splitDate = Day(2018, 10, 1)
  val eexpatch = oldeex.until(splitDate) ++ neweex.from(splitDate)
  monthly(eexpatch)
}

val PowerGermanyAustriaEexPhelixDayPeakMonthlyEurMwh = {

  val series1 = omit(Index from EEXGERMANYAUSTRIAPEAKLOADDAYAHEAD2) filter {

    case (day, _) => List(Monday, Tuesday, Wednesday, Thursday, Friday) contains day.weekday //true
  }

  log debug ">>>>>>>>>>>>>>>>>> old symbol"
  series1 foreach { case (k, v) => log debug (" " + k.toString + " : " + v.toString) }

  val series2 = omit(Index from EEXGERMANYPEAKLOADDAYAHEAD3) filter {

    case (day, _) => List(Monday, Tuesday, Wednesday, Thursday, Friday) contains day.weekday //true
  }

  log debug ">>>>>>>>>>>>>>>>>> new symbol"
  series2 foreach { case (k, v) => log debug (" " + k.toString + " : " + v.toString) }

  val splitDate = Day(2018, 10, 1)

  val series = series1.until(splitDate) ++ series2.from(splitDate)
  log debug ">>>>>>>>>>>>>>>>>> result"
  series foreach { case (k, v) => log debug (" " + k.toString + " : " + v.toString) }


  change(series) to Month
}


val GasNcgEexYa01ShiftMonthlyEurMwh = {
  val lim = omit(Close from EEXEGT_YR1)
  val month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
  val takeFirst = (s: Series[Day, Double]) => tryOption { s.firstValue }
  val withFirst = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeFirst(series)}
  Interpolate.omit(Series(lim groupBy month map withFirst))
}


//// ERDS

val PowerGermanyAustriaEexPhelixDayBaseQuarterlyEurMwh =  quarterly(Index from EEXGERMANYAUSTRIABASELOADDAYAHEAD)
val CoalArgusApi2QuarterlyMidUsdT = quarterly(Index from PA000214000)
val FxEcbUsdMidMonthlyUsdEur = monthly(Spot from ECBUSD)

//// GAS-X BCM

val GasNcgEex1mwDaMidDailyEurcKwh =  {
  val history = omit(Close from EEXNCGMW01WDDELIVDATE)
  val conversion = 0.1 // EUR/MWH in EURC/KWH
  history * conversion
}

val GasGplEex1mwDaMidDailyEurcKwh =  {
  val history = omit(Close from EEXGPLMW01WDDELIVDATE)
  val conversion = 0.1 // EUR/MWH in EURC/KWH
  history * conversion
}

val GasTtfEndofWorkingDayIndexDailyEurcKwh =  {
  val history = omit(Index from APXTTFENDOFWORKINGDAYDAFLOW)
  val conversion = 0.1 // EUR/MWH in EURC/KWH
  history * conversion
}

val GasZeeEndofWorkingDayIndexDailyEurcKwh =  {
  val history = omit(Index from APXZTPENDOFWORKINGDAYDAFLOW)
  val conversion = 0.1 // EUR/MWH in EURC/KWH
  history * conversion
}

val GasBrdBnetzaStructuralChargeDailyEurcKwh = {

  val lim1 = omit(Val from GASPOOLCOMPENSATIONENERGYPRICESPRICEFORNEGATIVECOMPENSATIONENERGY)
  val lim2 = omit(Val from GASPOOLCOMPENSATIONENERGYPRICESPRICEFORPOSITIVECOMPENSATIONENERGY)

  val charge = lim1*0.075 + lim2*0.075

  charge.roundTo(4)
}

//// iOPT

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusDaMidDailyEurMwh = {

  val low = flatRight (Low from PA000169660)
  val high = flatRight (High from PA000169660)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusWeekendMidDailyEurMwh = {

  val low = flatRight (Low from PA000169760)
  val high = flatRight (High from PA000169760)
  val mid = high*0.5 + low*0.5

  mid

}



/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusCyr01MidDailyEurMwh = {

  val low = flatRight (Low from PA000273061)
  val high = flatRight (High from PA000273061)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusCyr02MidDailyEurMwh = {

  val low = flatRight (Low from PA000273062)
  val high = flatRight (High from PA000273062)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusMaMidDailyEurMwh = {

  val low = flatRight (Low from PA000273761)
  val high = flatRight (High from PA000273761)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusMa02MidDailyEurMwh = {

  val low = flatRight (Low from PA000273762)
  val high = flatRight (High from PA000273762)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusMa03MidDailyEurMwh = {

  val low = flatRight (Low from PA000273763)
  val high = flatRight (High from PA000273763)
  val mid = high*0.5 + low*0.5

  mid

}


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusQ01MidDailyEurMwh = {

  val low = flatRight (Low from PA000274261)
  val high = flatRight (High from PA000274261)
  val mid = high*0.5 + low*0.5

  mid

}


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusQ02MidDailyEurMwh = {

  val low = flatRight (Low from PA000274262)
  val high = flatRight (High from PA000274262)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusQ03MidDailyEurMwh = {

  val low = flatRight (Low from PA000274263)
  val high = flatRight (High from PA000274263)
  val mid = high*0.5 + low*0.5

  mid

}


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusQ04MidDailyEurMwh = {

  val low = flatRight (Low from PA000274264)
  val high = flatRight (High from PA000274264)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusQ05MidDailyEurMwh = {

  val low = flatRight (Low from PA000274265)
  val high = flatRight (High from PA000274265)
  val mid = high*0.5 + low*0.5

  mid

}


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusQ06MidDailyEurMwh = {

  val low = flatRight (Low from PA000274266)
  val high = flatRight (High from PA000274266)
  val mid = high*0.5 + low*0.5

  mid

}


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusQ07MidDailyEurMwh = {

  val low = flatRight (Low from PA000274267)
  val high = flatRight (High from PA000274267)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusQ08MidDailyEurMwh = {

  val low = flatRight (Low from PA000274268)
  val high = flatRight (High from PA000274268)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplArgusQ09MidDailyEurMwh = {

  val low = flatRight (Low from PA000274269)
  val high = flatRight (High from PA000274269)
  val mid = high*0.5 + low*0.5

  mid

}

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCurrentMonthMidDailyEurMwh = flatRight (Close from EEXEGT_MTH1)


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ05MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BQ", Close from EEXEGT_Q5, Close from EEXEGT_Q4))



/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCurrentMonthMidDailyEurMwh = flatRight (Close from EEXBEB_MTH1)


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenDaMidDailyEurMwh = flatRight (FwdMid1day from ESGMBEB)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenWeekendMidDailyEurMwh = flatRight (FwdMidwknd from ESGMBEB)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenBomMidDailyEurMwh = flatRight (FwdMidBlmo from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplHerenMaMidDailyEurMwh = flatRight (FwdMid01mo from ESGMBEB)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenQ01MidDailyEurMwh = flatRight (FwdMid01qt from ESGMEGT)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenCyr01MidDailyEurMwh = flatRight (FwdMid01cyr from ESGMEGT)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenCyr02MidDailyEurMwh = flatRight (FwdMid02cyr from ESGMEGT)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenGyr01MidDailyEurMwh = flatRight (FwdMid01yr from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenGyr02MidDailyEurMwh = flatRight (FwdMid02yr from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenMa02MidDailyEurMwh = flatRight (FwdMid02mo from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenMa03MidDailyEurMwh = flatRight (FwdMid03mo from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenQ01MidDailyEurMwh = flatRight (FwdMid01qt from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenQ02MidDailyEurMwh = flatRight (FwdMid02qt from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenQ03MidDailyEurMwh = flatRight (FwdMid03qt from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenQ04MidDailyEurMwh = flatRight (FwdMid04qt from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenQ05MidDailyEurMwh = flatRight (FwdMid05qt from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenQ06MidDailyEurMwh = flatRight (FwdMid06qt from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenCyr01MidDailyEurMwh = flatRight (FwdMid01cyr from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenCyr02MidDailyEurMwh = flatRight (FwdMid02cyr from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenCyr03MidDailyEurMwh = flatRight (FwdMid03cyr from ESGMTTF)


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenDaIndexDailyEurMwh = flatRight (Close from HERINXEGTDAYA)
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenDaIndexDailyEurMwh = flatRight (Close from HERINXTTFDAYA)


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenMaIndexDailyEurMwh = flatRight (Close from HERENNCGDAILYMONTHAHEADINDEX)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenMaIndexDailyEurMwh = flatRight (Close from HERENTTFDAILYMONTHAHEADINDEX)
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexDaRefpriceDailyEurMwh = flatRight (RefPrice from EEXGPLDRPDELIVDATE)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexDaRefpriceDailyEurMwh = flatRight (RefPrice from EEXNCGDRPDELIVDATE)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ01MidDailyEurMwh = omit (settlementCurve("G0BQ", Close from EEXEGT_Q1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ01MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BQ", Close from EEXEGT_Q1, Series.empty))


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexMaMidDailyEurMwh = omit (settlementCurve("G0BM", Close from EEXEGT_MTH2, Close from EEXEGT_MTH1))

lazy val GasNcgEexMaMidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BM", Close from EEXEGT_MTH2, Close from EEXEGT_MTH1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenDaMidDailyEurMwh = flatRight (FwdMid1day from ESGMEGT)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenWeekendMidDailyEurMwh = flatRight (FwdMidwknd from ESGMEGT)


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenDaMidDailyEurMwh = flatRight (FwdMid1day from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenWeekendMidDailyEurMwh = flatRight (FwdMidwknd from ESGMTTF)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenMaMidDailyEurMwh = flatRight (FwdMid01mo from ESGMEGT)

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenMaMidDailyEurMwh = flatRight (FwdMid01mo from ESGMTTF)

lazy val GasTtfHerenDaCumulativeIndexMonthEndDailyEurMwh = {
  val lim = omit(Close from HERINXTTFDAYACUMULATIVE)
  val month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
  val takeLast = (s: Series[Day, Double]) => tryOption { s.lastValue }
  val withLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeLast(series)}
  change(Interpolate.omit(Series(lim groupBy month map withLast))) to Day
}

lazy val GasNcgHerenDaCumulativeIndexMonthEndDailyEurMwh = {
  val lim = omit(Close from HERINXEGTDAYACUMULATIVE)
  val month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
  val takeLast = (s: Series[Day, Double]) => tryOption { s.lastValue }
  val withLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeLast(series)}
  change(Interpolate.omit(Series(lim groupBy month map withLast))) to Day
}

lazy val GasGermanyEexEgixMaIndexPenultimateMonthEndDailyEurMwh = {
  val lim = omit(Val from EGIXINDEXMONTHDE)
  val month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
  val takeSecondLast = (s: Series[Day, Double]) => tryOption { s.dropRight(1).lastValue }
  val withSecondLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeSecondLast(series)}
  change(Interpolate.omit(Series(lim groupBy month map withSecondLast).lag(-1))) to Day
}

lazy val GasNcgEexEgixMaIndexPenultimateMonthEndDailyEurMwh = {
  val lim = omit(Val from EGIXINDEXMONTHNCG)
  val month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
  val takeSecondLast = (s: Series[Day, Double]) => tryOption { s.dropRight(1).lastValue }
  val withSecondLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeSecondLast(series)}
  change(Interpolate.omit(Series(lim groupBy month map withSecondLast).lag(-1))) to Day
}

lazy val GasGplEexEgixMaIndexPenultimateMonthEndDailyEurMwh = {
  val lim = omit(Val from EGIXINDEXMONTHGASPOOL)
  val month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
  val takeSecondLast = (s: Series[Day, Double]) => tryOption { s.dropRight(1).lastValue }
  val withSecondLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeSecondLast(series)}
  change(Interpolate.omit(Series(lim groupBy month map withSecondLast).lag(-1))) to Day
}

val GasGplArgusSum01MidDailyEurMwh = {

  val low2 = flatRight (Low from PA000274762)
  val high2 = flatRight (High from PA000274762)
  val mid2 = high2*0.5 + low2*0.5

  val low1 = flatRight (Low from PA000274761)
  val high1 = flatRight (High from PA000274761)
  val mid1 = high1*0.5 + low1*0.5

  val a = mid2 map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = mid1 map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }

  omit(a) ++ omit(b)
}

val GasGplArgusSum02MidDailyEurMwh = {

  val low2 = flatRight (Low from PA000274764)
  val high2 = flatRight (High from PA000274764)
  val mid2 = high2*0.5 + low2*0.5

  val low1 = flatRight (Low from PA000274763)
  val high1 = flatRight (High from PA000274763)
  val mid1 = high1*0.5 + low1*0.5

  val a = mid2 map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = mid1 map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplArgusSum03MidDailyEurMwh = {

  val low1 = flatRight (Low from PA000274765)
  val high1 = flatRight (High from PA000274765)
  val mid1 = high1*0.5 + low1*0.5

  val a = mid1 map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  flatRight(a)
}


val GasGplArgusWin01MidDailyEurMwh = {

  val low2 = flatRight (Low from PA000274762)
  val high2 = flatRight (High from PA000274762)
  val mid2 = high2*0.5 + low2*0.5

  val low1 = flatRight (Low from PA000274761)
  val high1 = flatRight (High from PA000274761)
  val mid1 = high1*0.5 + low1*0.5


  val a = mid1 map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = mid2 map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplArgusWin02MidDailyEurMwh = {

  val low2 = flatRight (Low from PA000274764)
  val high2 = flatRight (High from PA000274764)
  val mid2 = high2*0.5 + low2*0.5

  val low1 = flatRight (Low from PA000274763)
  val high1 = flatRight (High from PA000274763)
  val mid1 = high1*0.5 + low1*0.5

  val a = mid1 map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = mid2 map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasGplArgusWin03MidDailyEurMwh = {

  val low1 = flatRight (Low from PA000274765)
  val high1 = flatRight (High from PA000274765)
  val mid1 = high1*0.5 + low1*0.5

  val a = mid1 map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  flatRight(a)
}


val GasNcgEexSum01MidDailyEurMwh = {
  val sn1 = settlementCurve("G0BS", Close from EEXEGT_SE1, Series.empty)
  val sn2 = settlementCurve("G0BS", Close from EEXEGT_SE2, Close from EEXEGT_SE1)
  val a = flatRight(sn2) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(sn1) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgEexSum02MidDailyEurMwh = {
  val sn3 = settlementCurve("G0BS", Close from EEXEGT_SE3, Close from EEXEGT_SE2)
  val sn4 = settlementCurve("G0BS", Close from EEXEGT_SE4, Close from EEXEGT_SE3)
  val a = flatRight(sn4) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(sn3) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgEexWin01MidDailyEurMwh = {
  val sn1 = settlementCurve("G0BS", Close from EEXEGT_SE1, Series.empty)
  val sn2 = settlementCurve("G0BS", Close from EEXEGT_SE2, Close from EEXEGT_SE1)
  val a = flatRight(sn1) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(sn2) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgEexWin02MidDailyEurMwh = {
  val sn3 = settlementCurve("G0BS", Close from EEXEGT_SE3, Close from EEXEGT_SE2)
  val sn4 = settlementCurve("G0BS", Close from EEXEGT_SE4, Close from EEXEGT_SE3)
  val a = flatRight(sn3) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(sn4) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}




/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgHerenMthindexDailyEurMwh = {

  val lim = change(omit(Value from HERENEGTMTHINDEX)) to Day
  val month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
  val takeSecondLast = (s: Series[Day, Double]) => tryOption { s.lastValue }
  val withSecondLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeSecondLast(series)}
  change(Interpolate.omit(Series(lim groupBy month map withSecondLast))) to Day
}


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfHerenMthindexDailyEurMwh = {

  val lim = change(omit(Value from HERENTTFMTHINDEX)) to Day
  val month = (element: (Day, Double)) => element match { case (day, _) => Month(day) }
  val takeSecondLast = (s: Series[Day, Double]) => tryOption { s.lastValue }
  val withSecondLast = (element: (Month, Series[Day, Double]))  => element match { case (month, series) => month -> takeSecondLast(series)}
  change(Interpolate.omit(Series(lim groupBy month map withSecondLast))) to Day
}


val GasNcgHerenSum01MidFlatrightWeDailyEurMwh = {
  val a = flatRight(FwdMid02sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(FwdMid01sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasNcgHerenWin01MidFlatrightWeDailyEurMwh = {
  val a = flatRight(FwdMid01sn from ESGMEGT) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(FwdMid02sn from ESGMEGT) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum01MidFlatrightWeDailyEurMwh = {
  val a = flatRight(FwdMid02sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(FwdMid01sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum02MidFlatrightWeDailyEurMwh = {
  val a = flatRight(FwdMid04sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(FwdMid03sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenSum03MidFlatrightWeDailyEurMwh = {
  val a = flatRight(FwdMid06sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(FwdMid05sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenWin01MidFlatrightWeDailyEurMwh = {
  val a = flatRight(FwdMid01sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(FwdMid02sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenWin02MidFlatrightWeDailyEurMwh = {
  val a = flatRight(FwdMid03sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(FwdMid04sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

val GasTtfHerenWin03MidFlatrightWeDailyEurMwh = {
  val a = flatRight(FwdMid05sn from ESGMTTF) map { case (day,value) => if(Season(day).isSummer) day -> Some(value) else day -> None }
  val b = flatRight(FwdMid06sn from ESGMTTF) map { case (day,value) => if(!Season(day).isSummer) day -> Some(value) else day -> None }
  omit(a) ++ omit(b)
}

//// PEGASUS

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexMa02MidDailyEurMwh = omit (settlementCurve("G0BM", Close from EEXEGT_MTH3, Close from EEXEGT_MTH2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexMa03MidDailyEurMwh = omit (settlementCurve("G0BM", Close from EEXEGT_MTH4, Close from EEXEGT_MTH3))
/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexMa04MidDailyEurMwh = omit (settlementCurve("G0BM", Close from EEXEGT_MTH5, Close from EEXEGT_MTH4))


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexMa05MidDailyEurMwh = omit (settlementCurve("G0BM", Series.empty, Close from EEXEGT_MTH5))


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ02MidDailyEurMwh = omit (settlementCurve("G0BQ", Close from EEXEGT_Q2, Close from EEXEGT_Q1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ03MidDailyEurMwh = omit (settlementCurve("G0BQ", Close from EEXEGT_Q3, Close from EEXEGT_Q2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ04MidDailyEurMwh = omit (settlementCurve("G0BQ", Close from EEXEGT_Q4, Close from EEXEGT_Q3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ05MidDailyEurMwh = omit (settlementCurve("G0BQ", Series.empty, Close from EEXEGT_Q4))


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr01MidDailyEurMwh = omit (settlementCurve("G0BY", Close from EEXEGT_YR1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr02MidDailyEurMwh = omit (settlementCurve("G0BY", Close from EEXEGT_YR2, Close from EEXEGT_YR1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr03MidDailyEurMwh = omit (settlementCurve("G0BY", Close from EEXEGT_YR3, Close from EEXEGT_YR2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr04MidDailyEurMwh = omit (settlementCurve("G0BY", Close from EEXEGT_YR4, Close from EEXEGT_YR3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr05MidDailyEurMwh = omit (settlementCurve("G0BY", Series.empty, Close from EEXEGT_YR4))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexMa02MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BM", Close from EEXEGT_MTH3, Close from EEXEGT_MTH2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexMa03MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BM", Close from EEXEGT_MTH4, Close from EEXEGT_MTH3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ02MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BQ", Close from EEXEGT_Q2, Close from EEXEGT_Q1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ03MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BQ", Close from EEXEGT_Q3, Close from EEXEGT_Q2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexQ04MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BQ", Close from EEXEGT_Q4, Close from EEXEGT_Q3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr01MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BY", Close from EEXEGT_YR1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr02MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BY", Close from EEXEGT_YR2, Close from EEXEGT_YR1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr03MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BY", Close from EEXEGT_YR3, Close from EEXEGT_YR2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexCyr04MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G0BY", Close from EEXEGT_YR4, Close from EEXEGT_YR3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexMaMidDailyEurMwh = omit (settlementCurve("G2BM", Close from EEXBEB_MTH2, Close from EEXBEB_MTH1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexMa02MidDailyEurMwh = omit (settlementCurve("G2BM", Close from EEXBEB_MTH3, Close from EEXBEB_MTH2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexMa03MidDailyEurMwh = omit (settlementCurve("G2BM", Close from EEXBEB_MTH4, Close from EEXBEB_MTH3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexMa04MidDailyEurMwh = omit (settlementCurve("G2BM", Close from EEXBEB_MTH5, Close from EEXBEB_MTH4))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexMa05MidDailyEurMwh = omit (settlementCurve("G2BM", Series.empty, Close from EEXBEB_MTH5))



/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ01MidDailyEurMwh = omit (settlementCurve("G2BQ", Close from EEXBEB_Q1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ02MidDailyEurMwh = omit (settlementCurve("G2BQ", Close from EEXBEB_Q2, Close from EEXBEB_Q1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ03MidDailyEurMwh = omit (settlementCurve("G2BQ", Close from EEXBEB_Q3, Close from EEXBEB_Q2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ04MidDailyEurMwh = omit (settlementCurve("G2BQ", Close from EEXBEB_Q4, Close from EEXBEB_Q3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ05MidDailyEurMwh = omit (settlementCurve("G2BQ", Series.empty, Close from EEXBEB_Q4))


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr01MidDailyEurMwh = omit (settlementCurve("G2BY", Close from EEXBEB_YR1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr02MidDailyEurMwh = omit (settlementCurve("G2BY", Close from EEXBEB_YR2, Close from EEXBEB_YR1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr03MidDailyEurMwh = omit (settlementCurve("G2BY", Close from EEXBEB_YR3, Close from EEXBEB_YR2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr04MidDailyEurMwh = omit (settlementCurve("G2BY", Close from EEXBEB_YR4, Close from EEXBEB_YR3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr05MidDailyEurMwh = omit (settlementCurve("G2BY", Series.empty, Close from EEXBEB_YR4))


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexMaMidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BM", Close from EEXBEB_MTH2, Close from EEXBEB_MTH1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexMa02MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BM", Close from EEXBEB_MTH3, Close from EEXBEB_MTH2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexMa03MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BM", Close from EEXBEB_MTH4, Close from EEXBEB_MTH3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ01MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BQ", Close from EEXBEB_Q1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ02MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BQ", Close from EEXBEB_Q2, Close from EEXBEB_Q1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ03MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BQ", Close from EEXBEB_Q3, Close from EEXBEB_Q2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexQ04MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BQ", Close from EEXBEB_Q4, Close from EEXBEB_Q3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr01MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BY", Close from EEXBEB_YR1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr02MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BY", Close from EEXBEB_YR2, Close from EEXBEB_YR1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr03MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BY", Close from EEXBEB_YR3, Close from EEXBEB_YR2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexCyr04MidFlatrightWeDailyEurMwh = flatRight (settlementCurve("G2BY", Close from EEXBEB_YR4, Close from EEXBEB_YR3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexSe01MidDailyEurMwh = omit (settlementCurve("G2BS", Close from EEXBEB_SE1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexSe02MidDailyEurMwh = omit (settlementCurve("G2BS", Close from EEXBEB_SE2, Close from EEXBEB_SE1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexSe03MidDailyEurMwh = omit (settlementCurve("G2BS", Close from EEXBEB_SE3, Close from EEXBEB_SE2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexSe04MidDailyEurMwh = omit (settlementCurve("G2BS", Close from EEXBEB_SE4, Close from EEXBEB_SE3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasGplEexSe05MidDailyEurMwh = omit (settlementCurve("G2BS", Series.empty, Close from EEXBEB_SE4))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexSe01MidDailyEurMwh = omit (settlementCurve("G0BS", Close from EEXEGT_SE1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexSe02MidDailyEurMwh = omit (settlementCurve("G0BS", Close from EEXEGT_SE2, Close from EEXEGT_SE1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexSe03MidDailyEurMwh = omit (settlementCurve("G0BS", Close from EEXEGT_SE3, Close from EEXEGT_SE2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexSe04MidDailyEurMwh = omit (settlementCurve("G0BS", Close from EEXEGT_SE4, Close from EEXEGT_SE3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasNcgEexSe05MidDailyEurMwh = omit (settlementCurve("G0BS", Series.empty, Close from EEXEGT_SE4))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceMaMidDailyEurMwh = omit (settlementCurve("ICEMTHTTF", Close from ICETTF, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceMa02MidDailyEurMwh = omit (settlementCurve("ICEMTHTTF", Close from ICETTF_02, Close from ICETTF))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceMa03MidDailyEurMwh = omit (settlementCurve("ICEMTHTTF", Close from ICETTF_03, Close from ICETTF_02))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceMa04MidDailyEurMwh = omit (settlementCurve("ICEMTHTTF", Close from ICETTF_04, Close from ICETTF_03))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceMa05MidDailyEurMwh = omit (settlementCurve("ICEMTHTTF", Close from ICETTF_05, Close from ICETTF_04))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceMa06MidDailyEurMwh = omit (settlementCurve("ICEMTHTTF", Close from ICETTF_06, Close from ICETTF_05))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceQ01MidDailyEurMwh = omit (settlementCurve("ICEQTTTF", Close from ICETTF_Q1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceQ02MidDailyEurMwh = omit (settlementCurve("ICEQTTTF", Close from ICETTF_Q2, Close from ICETTF_Q1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceQ03MidDailyEurMwh = omit (settlementCurve("ICEQTTTF", Close from ICETTF_Q3, Close from ICETTF_Q2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceQ04MidDailyEurMwh = omit (settlementCurve("ICEQTTTF", Close from ICETTF_Q4, Close from ICETTF_Q3))


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceSe01MidDailyEurMwh = omit (settlementCurve("ICESETTF", Close from ICETTF_SE1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceSe02MidDailyEurMwh = omit (settlementCurve("ICESETTF", Close from ICETTF_SE2, Close from ICETTF_SE1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceSe03MidDailyEurMwh = omit (settlementCurve("ICESETTF", Close from ICETTF_SE3, Close from ICETTF_SE2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceSe04MidDailyEurMwh = omit (settlementCurve("ICESETTF", Close from ICETTF_SE4, Close from ICETTF_SE3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceSe05MidDailyEurMwh = omit (settlementCurve("ICESETTF", Close from ICETTF_SE5, Close from ICETTF_SE4))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceSe06MidDailyEurMwh = omit (settlementCurve("ICESETTF", Close from ICETTF_SE6, Close from ICETTF_SE5))


/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceCyr01MidDailyEurMwh = omit (settlementCurve("ICECYRTTF", Close from ICETTF_YR1, Series.empty))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceCyr02MidDailyEurMwh = omit (settlementCurve("ICECYRTTF", Close from ICETTF_YR2, Close from ICETTF_YR1))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceCyr03MidDailyEurMwh = omit (settlementCurve("ICECYRTTF", Close from ICETTF_YR3, Close from ICETTF_YR2))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceCyr04MidDailyEurMwh = omit (settlementCurve("ICECYRTTF", Close from ICETTF_YR4, Close from ICETTF_YR3))

/**
  * EUR_MWH
  * DAILY
  */
lazy val GasTtfIceCyr05MidDailyEurMwh = omit (settlementCurve("ICECYRTTF", Close from ICETTF_YR5, Close from ICETTF_YR4))

//// LEBA INDICES

if(GasTtfLebaWeekendIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfLebaWeekendIndexDailyEurMwh)(GasTtfLebaWeekendIndexDailyEurMwh)
}

if(GasTtfLebaDaWeekendIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfLebaDaWeekendIndexDailyEurMwh)(GasTtfLebaDaWeekendIndexDailyEurMwh)
}
if(GasNcgLebaWeekendIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgLebaWeekendIndexDailyEurMwh)(GasNcgLebaWeekendIndexDailyEurMwh)
}

if(GasNcgLebaDaWeekendIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgLebaDaWeekendIndexDailyEurMwh)(GasNcgLebaDaWeekendIndexDailyEurMwh)
}

if(GasGplLebaWeekendIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplLebaWeekendIndexDailyEurMwh)(GasGplLebaWeekendIndexDailyEurMwh)
}
if(GasGplLebaDaWeekendIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplLebaDaWeekendIndexDailyEurMwh)(GasGplLebaDaWeekendIndexDailyEurMwh)
}
if(GasNcgLebaDaWeekendWindowIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgLebaDaWeekendWindowIndexDailyEurMwh)(GasNcgLebaDaWeekendWindowIndexDailyEurMwh)
}

if(GasGplLebaWeekendIndexDailyGbppcTherm.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplLebaWeekendIndexDailyGbppcTherm)(GasGplLebaWeekendIndexDailyGbppcTherm)
}
if(GasGplLebaDaWeekendIndexDailyGbppcTherm.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplLebaDaWeekendIndexDailyGbppcTherm)(GasGplLebaDaWeekendIndexDailyGbppcTherm)
}
if(GasTtfLebaWeekendIndexDailyGbppcTherm.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfLebaWeekendIndexDailyGbppcTherm)(GasTtfLebaWeekendIndexDailyGbppcTherm)
}

if(GasTtfLebaDaWeekendIndexDailyGbppcTherm.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfLebaDaWeekendIndexDailyGbppcTherm)(GasTtfLebaDaWeekendIndexDailyGbppcTherm)
}
if(GasNcgLebaWeekendIndexDailyGbppcTherm.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgLebaWeekendIndexDailyGbppcTherm)(GasNcgLebaWeekendIndexDailyGbppcTherm)
}

if(GasNcgLebaDaWeekendIndexDailyGbppcTherm.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgLebaDaWeekendIndexDailyGbppcTherm)(GasNcgLebaDaWeekendIndexDailyGbppcTherm)
}

//// Special


if(GasNbpIcePenultMaAvgMonthlyGbbpcTherm.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNbpIcePenultMaAvgMonthlyGbbpcTherm)(GasNbpIcePenultMaAvgMonthlyGbbpcTherm)(Some(Month(asof + 1) - 1))
}

if(PowerGermanyAustriaEexPhelixDayBaseMonthlyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_PowerGermanyAustriaEexPhelixDayBaseMonthlyEurMwh)(PowerGermanyAustriaEexPhelixDayBaseMonthlyEurMwh)(Some(Month(asof + 1) - 1))
}

if(PowerGermanyAustriaEexPhelixDayPeakMonthlyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_PowerGermanyAustriaEexPhelixDayPeakMonthlyEurMwh)(PowerGermanyAustriaEexPhelixDayPeakMonthlyEurMwh)(Some(Month(asof + 1) - 1))
}

if(GasNcgEexYa01ShiftMonthlyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexYa01ShiftMonthlyEurMwh)(GasNcgEexYa01ShiftMonthlyEurMwh)(Some(Month(asof + 1) - 1))
}



//// Seasons

if(GasNcgHerenSum01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum01MidDailyEurMwh)(GasNcgHerenSum01MidDailyEurMwh)
}

if(GasNcgHerenSum02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum02MidDailyEurMwh)(GasNcgHerenSum02MidDailyEurMwh)
}
if(GasNcgHerenSum03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum03MidDailyEurMwh)(GasNcgHerenSum03MidDailyEurMwh)
}


if(GasNcgHerenSum01BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum01BidDailyEurMwh)(GasNcgHerenSum01BidDailyEurMwh)
}

if(GasNcgHerenSum02BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum02BidDailyEurMwh)(GasNcgHerenSum02BidDailyEurMwh)
}
if(GasNcgHerenSum03BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum03BidDailyEurMwh)(GasNcgHerenSum03BidDailyEurMwh)
}

if(GasNcgHerenSum01AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum01AskDailyEurMwh)(GasNcgHerenSum01AskDailyEurMwh)
}

if(GasNcgHerenSum02AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum02AskDailyEurMwh)(GasNcgHerenSum02AskDailyEurMwh)
}
if(GasNcgHerenSum03AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum03AskDailyEurMwh)(GasNcgHerenSum03AskDailyEurMwh)
}
if(GasTtfHerenSum01BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum01BidDailyEurMwh)(GasTtfHerenSum01BidDailyEurMwh)
}

if(GasTtfHerenSum02BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum02BidDailyEurMwh)(GasTtfHerenSum02BidDailyEurMwh)
}
if(GasTtfHerenSum03BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum03BidDailyEurMwh)(GasTtfHerenSum03BidDailyEurMwh)
}
if(GasTtfHerenSum01AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum01AskDailyEurMwh)(GasTtfHerenSum01AskDailyEurMwh)
}

if(GasTtfHerenSum02AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum02AskDailyEurMwh)(GasTtfHerenSum02AskDailyEurMwh)
}
if(GasTtfHerenSum03AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum03AskDailyEurMwh)(GasTtfHerenSum03AskDailyEurMwh)
}
if(GasTtfHerenSum01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum01MidDailyEurMwh)(GasTtfHerenSum01MidDailyEurMwh)
}

if(GasTtfHerenSum02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum02MidDailyEurMwh)(GasTtfHerenSum02MidDailyEurMwh)
}
if(GasTtfHerenSum03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum03MidDailyEurMwh)(GasTtfHerenSum03MidDailyEurMwh)
}

if(GasNcgHerenWin01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin01MidDailyEurMwh)(GasNcgHerenWin01MidDailyEurMwh)
}
if(GasNcgHerenWin02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin02MidDailyEurMwh)(GasNcgHerenWin02MidDailyEurMwh)
}
if(GasNcgHerenWin03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin03MidDailyEurMwh)(GasNcgHerenWin03MidDailyEurMwh)
}


if(GasNcgHerenWin01BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin01BidDailyEurMwh)(GasNcgHerenWin01BidDailyEurMwh)
}

if(GasNcgHerenWin02BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin02BidDailyEurMwh)(GasNcgHerenWin02BidDailyEurMwh)
}
if(GasNcgHerenWin03BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin03BidDailyEurMwh)(GasNcgHerenWin03BidDailyEurMwh)
}

if(GasNcgHerenWin01AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin01AskDailyEurMwh)(GasNcgHerenWin01AskDailyEurMwh)
}

if(GasNcgHerenWin02AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin02AskDailyEurMwh)(GasNcgHerenWin02AskDailyEurMwh)
}
if(GasNcgHerenWin03AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin03AskDailyEurMwh)(GasNcgHerenWin03AskDailyEurMwh)
}
if(GasTtfHerenWin01BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin01BidDailyEurMwh)(GasTtfHerenWin01BidDailyEurMwh)
}

if(GasTtfHerenWin02BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin02BidDailyEurMwh)(GasTtfHerenWin02BidDailyEurMwh)
}
if(GasTtfHerenWin03BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin03BidDailyEurMwh)(GasTtfHerenWin03BidDailyEurMwh)
}
if(GasTtfHerenWin01AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin01AskDailyEurMwh)(GasTtfHerenWin01AskDailyEurMwh)
}

if(GasTtfHerenWin02AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin02AskDailyEurMwh)(GasTtfHerenWin02AskDailyEurMwh)
}
if(GasTtfHerenWin03AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin03AskDailyEurMwh)(GasTtfHerenWin03AskDailyEurMwh)
}
if(GasTtfHerenWin01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin01MidDailyEurMwh)(GasTtfHerenWin01MidDailyEurMwh)
}

if(GasTtfHerenWin02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin02MidDailyEurMwh)(GasTtfHerenWin02MidDailyEurMwh)
}
if(GasTtfHerenWin03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin03MidDailyEurMwh)(GasTtfHerenWin03MidDailyEurMwh)
}

//// ERDS

if(FxEcbUsdMidMonthlyUsdEur.nonEmpty) {
  writeWithTimeStamp(RDMS_FxEcbUsdMidMonthlyUsdEur)(FxEcbUsdMidMonthlyUsdEur)(Some(Month(asof + 1) - 1))
}

if(CoalArgusApi2QuarterlyMidUsdT.nonEmpty) {
  writeWithTimeStamp(RDMS_CoalArgusApi2QuarterlyMidUsdT)(CoalArgusApi2QuarterlyMidUsdT)(Some(Quarter(asof + 1) - 1))
}

if(PowerGermanyAustriaEexPhelixDayBaseQuarterlyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_PowerGermanyAustriaEexPhelixDayBaseQuarterlyEurMwh)(PowerGermanyAustriaEexPhelixDayBaseQuarterlyEurMwh)(Some(Quarter(asof + 1) - 1))
}
//// GAS-X BCM

if(GasTtfEndofWorkingDayIndexDailyEurcKwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfEndofWorkingDayIndexDailyEurcKwh)(GasTtfEndofWorkingDayIndexDailyEurcKwh)
}

if(GasZeeEndofWorkingDayIndexDailyEurcKwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasZeeEndofWorkingDayIndexDailyEurcKwh)(GasZeeEndofWorkingDayIndexDailyEurcKwh)
}

if(GasGplEex1mwDaMidDailyEurcKwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEex1mwDaMidDailyEurcKwh)(GasGplEex1mwDaMidDailyEurcKwh)
}

if(GasNcgEex1mwDaMidDailyEurcKwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEex1mwDaMidDailyEurcKwh)(GasNcgEex1mwDaMidDailyEurcKwh)
}

if(GasBrdBnetzaStructuralChargeDailyEurcKwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasBrdBnetzaStructuralChargeDailyEurcKwh)(GasBrdBnetzaStructuralChargeDailyEurcKwh)
}

/////  iOPT

if(GasGplArgusDaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusDaMidDailyEurMwh)(GasGplArgusDaMidDailyEurMwh)
}

if(GasGplArgusWeekendMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusWeekendMidDailyEurMwh)(GasGplArgusWeekendMidDailyEurMwh)
}

if(GasGplArgusCyr01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusCyr01MidDailyEurMwh)(GasGplArgusCyr01MidDailyEurMwh)
}

if(GasGplArgusCyr02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusCyr02MidDailyEurMwh)(GasGplArgusCyr02MidDailyEurMwh)
}

if(GasGplArgusMaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusMaMidDailyEurMwh)(GasGplArgusMaMidDailyEurMwh)
}

if(GasGplArgusMa02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusMa02MidDailyEurMwh)(GasGplArgusMa02MidDailyEurMwh)
}

if(GasGplArgusMa03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusMa03MidDailyEurMwh)(GasGplArgusMa03MidDailyEurMwh)
}

if(GasGplArgusQ01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusQ01MidDailyEurMwh)(GasGplArgusQ01MidDailyEurMwh)
}

if(GasGplArgusQ02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusQ02MidDailyEurMwh)(GasGplArgusQ02MidDailyEurMwh)
}

if(GasGplArgusQ03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusQ03MidDailyEurMwh)(GasGplArgusQ03MidDailyEurMwh)
}

if(GasGplArgusQ04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusQ04MidDailyEurMwh)(GasGplArgusQ04MidDailyEurMwh)
}

if(GasGplArgusQ05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusQ05MidDailyEurMwh)(GasGplArgusQ05MidDailyEurMwh)
}

if(GasGplArgusQ06MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusQ06MidDailyEurMwh)(GasGplArgusQ06MidDailyEurMwh)
}

if(GasGplArgusQ07MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusQ07MidDailyEurMwh)(GasGplArgusQ07MidDailyEurMwh)
}

if(GasGplArgusQ08MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusQ08MidDailyEurMwh)(GasGplArgusQ08MidDailyEurMwh)
}

if(GasGplArgusQ09MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusQ09MidDailyEurMwh)(GasGplArgusQ09MidDailyEurMwh)
}

if(GasGplArgusSum01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusSum01MidDailyEurMwh)(GasGplArgusSum01MidDailyEurMwh)
}

if(GasGplArgusSum02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusSum02MidDailyEurMwh)(GasGplArgusSum02MidDailyEurMwh)
}

if(GasGplArgusSum03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusSum03MidDailyEurMwh)(GasGplArgusSum03MidDailyEurMwh)
}

if(GasGplArgusWin01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusWin01MidDailyEurMwh)(GasGplArgusWin01MidDailyEurMwh)
}

if(GasGplArgusWin02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusWin02MidDailyEurMwh)(GasGplArgusWin02MidDailyEurMwh)
}

if(GasGplArgusWin03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusWin03MidDailyEurMwh)(GasGplArgusWin03MidDailyEurMwh)
}

if(GasNcgEexCurrentMonthMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCurrentMonthMidDailyEurMwh)(GasNcgEexCurrentMonthMidDailyEurMwh)
}



if(GasNcgEexQ05MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ05MidFlatrightWeDailyEurMwh)(GasNcgEexQ05MidFlatrightWeDailyEurMwh)
}

if(GasNcgEexQ05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ05MidDailyEurMwh)(GasNcgEexQ05MidDailyEurMwh)
}


if(GasGplEexCurrentMonthMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCurrentMonthMidDailyEurMwh)(GasGplEexCurrentMonthMidDailyEurMwh)
}

if(GasGplHerenDaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenDaMidDailyEurMwh)(GasGplHerenDaMidDailyEurMwh)
}

if(GasGplHerenWeekendMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWeekendMidDailyEurMwh)(GasGplHerenWeekendMidDailyEurMwh)
}

if(GasTtfHerenBomMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenBomMidDailyEurMwh)(GasTtfHerenBomMidDailyEurMwh)
}

if(GasGplHerenMaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenMaMidDailyEurMwh)(GasGplHerenMaMidDailyEurMwh)
}

if(GasNcgHerenQ01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenQ01MidDailyEurMwh)(GasNcgHerenQ01MidDailyEurMwh)
}

if(GasNcgHerenCyr01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenCyr01MidDailyEurMwh)(GasNcgHerenCyr01MidDailyEurMwh)
}

if(GasNcgHerenCyr02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenCyr02MidDailyEurMwh)(GasNcgHerenCyr02MidDailyEurMwh)
}

if(GasTtfHerenGyr01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenGyr01MidDailyEurMwh)(GasTtfHerenGyr01MidDailyEurMwh)
}

if(GasTtfHerenGyr02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenGyr02MidDailyEurMwh)(GasTtfHerenGyr02MidDailyEurMwh)
}

if(GasTtfHerenMa02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenMa02MidDailyEurMwh)(GasTtfHerenMa02MidDailyEurMwh)
}

if(GasTtfHerenMa03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenMa03MidDailyEurMwh)(GasTtfHerenMa03MidDailyEurMwh)
}

if(GasTtfHerenQ01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenQ01MidDailyEurMwh)(GasTtfHerenQ01MidDailyEurMwh)
}

if(GasTtfHerenQ02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenQ02MidDailyEurMwh)(GasTtfHerenQ02MidDailyEurMwh)
}

if(GasTtfHerenQ03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenQ03MidDailyEurMwh)(GasTtfHerenQ03MidDailyEurMwh)
}

if(GasNcgHerenDaIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenDaIndexDailyEurMwh)(GasNcgHerenDaIndexDailyEurMwh)
}
if(GasTtfHerenDaIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenDaIndexDailyEurMwh)(GasTtfHerenDaIndexDailyEurMwh)
}
if(GasNcgHerenMaIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenMaIndexDailyEurMwh)(GasNcgHerenMaIndexDailyEurMwh)
}

if(GasTtfHerenMaIndexDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenMaIndexDailyEurMwh)(GasTtfHerenMaIndexDailyEurMwh)
}

if(!GasTtfHerenDaCumulativeIndexMonthEndDailyEurMwh.isEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenDaCumulativeIndexMonthEndDailyEurMwh)(GasTtfHerenDaCumulativeIndexMonthEndDailyEurMwh)
}
if(!GasNcgHerenDaCumulativeIndexMonthEndDailyEurMwh.isEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenDaCumulativeIndexMonthEndDailyEurMwh)(GasNcgHerenDaCumulativeIndexMonthEndDailyEurMwh)
}


if(!GasGermanyEexEgixMaIndexPenultimateMonthEndDailyEurMwh.isEmpty) {
  writeWithTimeStamp(RDMS_GasGermanyEexEgixMaIndexPenultimateMonthEndDailyEurMwh)(GasGermanyEexEgixMaIndexPenultimateMonthEndDailyEurMwh)
}

if(!GasNcgEexEgixMaIndexPenultimateMonthEndDailyEurMwh.isEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexEgixMaIndexPenultimateMonthEndDailyEurMwh)(GasNcgEexEgixMaIndexPenultimateMonthEndDailyEurMwh)
}

if(!GasGplEexEgixMaIndexPenultimateMonthEndDailyEurMwh.isEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexEgixMaIndexPenultimateMonthEndDailyEurMwh)(GasGplEexEgixMaIndexPenultimateMonthEndDailyEurMwh)
}

if(GasNcgEexSum01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexSum01MidDailyEurMwh)(GasNcgEexSum01MidDailyEurMwh)
}

if(GasNcgEexSum02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexSum02MidDailyEurMwh)(GasNcgEexSum02MidDailyEurMwh)
}

if(GasNcgEexWin01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexWin01MidDailyEurMwh)(GasNcgEexWin01MidDailyEurMwh)
}

if(GasNcgEexWin02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexWin02MidDailyEurMwh)(GasNcgEexWin02MidDailyEurMwh)
}


if(GasGplEexDaRefpriceDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexDaRefpriceDailyEurMwh)(GasGplEexDaRefpriceDailyEurMwh)
}

if(GasNcgEexDaRefpriceDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexDaRefpriceDailyEurMwh)(GasNcgEexDaRefpriceDailyEurMwh)
}

if(GasNcgEexQ01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ01MidDailyEurMwh)(GasNcgEexQ01MidDailyEurMwh)
}

if(GasNcgEexQ01MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ01MidFlatrightWeDailyEurMwh)(GasNcgEexQ01MidFlatrightWeDailyEurMwh)
}


if(GasNcgEexMaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexMaMidDailyEurMwh)(GasNcgEexMaMidDailyEurMwh)
}
if(GasNcgEexMaMidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexMaMidFlatrightWeDailyEurMwh)(GasNcgEexMaMidFlatrightWeDailyEurMwh)
}

if(GasNcgHerenDaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenDaMidDailyEurMwh)(GasNcgHerenDaMidDailyEurMwh)
}

if(GasNcgHerenWeekendMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWeekendMidDailyEurMwh)(GasNcgHerenWeekendMidDailyEurMwh)
}

if(GasTtfHerenDaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenDaMidDailyEurMwh)(GasTtfHerenDaMidDailyEurMwh)
}

if(GasTtfHerenWeekendMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWeekendMidDailyEurMwh)(GasTtfHerenWeekendMidDailyEurMwh)
}

if(GasNcgHerenMaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenMaMidDailyEurMwh)(GasNcgHerenMaMidDailyEurMwh)
}

if(GasTtfHerenMaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenMaMidDailyEurMwh)(GasTtfHerenMaMidDailyEurMwh)
}

if(!GasNcgHerenMthindexDailyEurMwh.isEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenMthindexDailyEurMwh)(GasNcgHerenMthindexDailyEurMwh)
}

if(!GasTtfHerenMthindexDailyEurMwh.isEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenMthindexDailyEurMwh)(GasTtfHerenMthindexDailyEurMwh)
}

if(GasNcgHerenSum01MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenSum01MidFlatrightWeDailyEurMwh)(GasNcgHerenSum01MidFlatrightWeDailyEurMwh)
}
if(GasNcgHerenWin01MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgHerenWin01MidFlatrightWeDailyEurMwh)(GasNcgHerenWin01MidFlatrightWeDailyEurMwh)
}


if(GasTtfHerenSum01MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum01MidFlatrightWeDailyEurMwh)(GasTtfHerenSum01MidFlatrightWeDailyEurMwh)
}

if(GasTtfHerenSum02MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum02MidFlatrightWeDailyEurMwh)(GasTtfHerenSum02MidFlatrightWeDailyEurMwh)
}
if(GasTtfHerenSum03MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenSum03MidFlatrightWeDailyEurMwh)(GasTtfHerenSum03MidFlatrightWeDailyEurMwh)
}

if(GasTtfHerenWin01MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin01MidFlatrightWeDailyEurMwh)(GasTtfHerenWin01MidFlatrightWeDailyEurMwh)
}

if(GasTtfHerenWin02MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin02MidFlatrightWeDailyEurMwh)(GasTtfHerenWin02MidFlatrightWeDailyEurMwh)
}
if(GasTtfHerenWin03MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenWin03MidFlatrightWeDailyEurMwh)(GasTtfHerenWin03MidFlatrightWeDailyEurMwh)
}

if(GasGplArgusDaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplArgusDaMidDailyEurMwh)(GasGplArgusDaMidDailyEurMwh)
}

if(GasTtfHerenCyr01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenCyr01MidDailyEurMwh)(GasTtfHerenCyr01MidDailyEurMwh)
}

if(GasTtfHerenCyr02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenCyr02MidDailyEurMwh)(GasTtfHerenCyr02MidDailyEurMwh)
}
if(GasTtfHerenCyr03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenCyr03MidDailyEurMwh)(GasTtfHerenCyr03MidDailyEurMwh)
}


if(GasTtfHerenQ05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenQ05MidDailyEurMwh)(GasTtfHerenQ05MidDailyEurMwh)
}
if(GasTtfHerenQ06MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenQ06MidDailyEurMwh)(GasTtfHerenQ06MidDailyEurMwh)
}
if(GasTtfHerenQ04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfHerenQ04MidDailyEurMwh)(GasTtfHerenQ04MidDailyEurMwh)
}

//// PEGASUS

if(GasNcgEexMa02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexMa02MidDailyEurMwh)(GasNcgEexMa02MidDailyEurMwh)
}

if(GasNcgEexMa03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexMa03MidDailyEurMwh)(GasNcgEexMa03MidDailyEurMwh)
}

if(GasNcgEexMa04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexMa04MidDailyEurMwh)(GasNcgEexMa04MidDailyEurMwh)
}

if(GasNcgEexMa05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexMa05MidDailyEurMwh)(GasNcgEexMa05MidDailyEurMwh)
}

if(GasNcgEexQ02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ02MidDailyEurMwh)(GasNcgEexQ02MidDailyEurMwh)
}

if(GasNcgEexQ03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ03MidDailyEurMwh)(GasNcgEexQ03MidDailyEurMwh)
}

if(GasNcgEexQ04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ04MidDailyEurMwh)(GasNcgEexQ04MidDailyEurMwh)
}

if(GasNcgEexCyr01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCyr01MidDailyEurMwh)(GasNcgEexCyr01MidDailyEurMwh)
}

if(GasNcgEexCyr02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCyr02MidDailyEurMwh)(GasNcgEexCyr02MidDailyEurMwh)
}

if(GasNcgEexCyr03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCyr03MidDailyEurMwh)(GasNcgEexCyr03MidDailyEurMwh)
}

if(GasNcgEexCyr04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCyr04MidDailyEurMwh)(GasNcgEexCyr04MidDailyEurMwh)
}

if(GasNcgEexCyr05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCyr05MidDailyEurMwh)(GasNcgEexCyr05MidDailyEurMwh)
}

if(GasNcgEexMa02MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexMa02MidFlatrightWeDailyEurMwh)(GasNcgEexMa02MidFlatrightWeDailyEurMwh)
}

if(GasNcgEexMa03MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexMa03MidFlatrightWeDailyEurMwh)(GasNcgEexMa03MidFlatrightWeDailyEurMwh)
}

if(GasNcgEexQ02MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ02MidFlatrightWeDailyEurMwh)(GasNcgEexQ02MidFlatrightWeDailyEurMwh)
}

if(GasNcgEexQ03MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ03MidFlatrightWeDailyEurMwh)(GasNcgEexQ03MidFlatrightWeDailyEurMwh)
}

if(GasNcgEexQ04MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexQ04MidFlatrightWeDailyEurMwh)(GasNcgEexQ04MidFlatrightWeDailyEurMwh)
}

if(GasNcgEexCyr01MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCyr01MidFlatrightWeDailyEurMwh)(GasNcgEexCyr01MidFlatrightWeDailyEurMwh)
}

if(GasNcgEexCyr02MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCyr02MidFlatrightWeDailyEurMwh)(GasNcgEexCyr02MidFlatrightWeDailyEurMwh)
}

if(GasNcgEexCyr03MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCyr03MidFlatrightWeDailyEurMwh)(GasNcgEexCyr03MidFlatrightWeDailyEurMwh)
}

if(GasNcgEexCyr04MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexCyr04MidFlatrightWeDailyEurMwh)(GasNcgEexCyr04MidFlatrightWeDailyEurMwh)
}


if(GasGplEexMaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexMaMidDailyEurMwh)(GasGplEexMaMidDailyEurMwh)
}

if(GasGplEexMa02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexMa02MidDailyEurMwh)(GasGplEexMa02MidDailyEurMwh)
}

if(GasGplEexMa03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexMa03MidDailyEurMwh)(GasGplEexMa03MidDailyEurMwh)
}

if(GasGplEexMa04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexMa04MidDailyEurMwh)(GasGplEexMa04MidDailyEurMwh)
}

if(GasGplEexMa05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexMa05MidDailyEurMwh)(GasGplEexMa05MidDailyEurMwh)
}

if(GasGplEexQ01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexQ01MidDailyEurMwh)(GasGplEexQ01MidDailyEurMwh)
}

if(GasGplEexQ02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexQ02MidDailyEurMwh)(GasGplEexQ02MidDailyEurMwh)
}

if(GasGplEexQ03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexQ03MidDailyEurMwh)(GasGplEexQ03MidDailyEurMwh)
}

if(GasGplEexQ04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexQ04MidDailyEurMwh)(GasGplEexQ04MidDailyEurMwh)
}

if(GasGplEexQ05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexQ05MidDailyEurMwh)(GasGplEexQ05MidDailyEurMwh)
}

if(GasGplEexCyr01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCyr01MidDailyEurMwh)(GasGplEexCyr01MidDailyEurMwh)
}

if(GasGplEexCyr02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCyr02MidDailyEurMwh)(GasGplEexCyr02MidDailyEurMwh)
}

if(GasGplEexCyr03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCyr03MidDailyEurMwh)(GasGplEexCyr03MidDailyEurMwh)
}

if(GasGplEexCyr04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCyr04MidDailyEurMwh)(GasGplEexCyr04MidDailyEurMwh)
}

if(GasGplEexCyr05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCyr05MidDailyEurMwh)(GasGplEexCyr05MidDailyEurMwh)
}


if(GasGplEexMaMidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexMaMidFlatrightWeDailyEurMwh)(GasGplEexMaMidFlatrightWeDailyEurMwh)
}

if(GasGplEexMa02MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexMa02MidFlatrightWeDailyEurMwh)(GasGplEexMa02MidFlatrightWeDailyEurMwh)
}

if(GasGplEexMa03MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexMa03MidFlatrightWeDailyEurMwh)(GasGplEexMa03MidFlatrightWeDailyEurMwh)
}

if(GasGplEexQ01MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexQ01MidFlatrightWeDailyEurMwh)(GasGplEexQ01MidFlatrightWeDailyEurMwh)
}

if(GasGplEexQ02MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexQ02MidFlatrightWeDailyEurMwh)(GasGplEexQ02MidFlatrightWeDailyEurMwh)
}

if(GasGplEexQ03MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexQ03MidFlatrightWeDailyEurMwh)(GasGplEexQ03MidFlatrightWeDailyEurMwh)
}

if(GasGplEexQ04MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexQ04MidFlatrightWeDailyEurMwh)(GasGplEexQ04MidFlatrightWeDailyEurMwh)
}

if(GasGplEexCyr01MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCyr01MidFlatrightWeDailyEurMwh)(GasGplEexCyr01MidFlatrightWeDailyEurMwh)
}

if(GasGplEexCyr02MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCyr02MidFlatrightWeDailyEurMwh)(GasGplEexCyr02MidFlatrightWeDailyEurMwh)
}

if(GasGplEexCyr03MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCyr03MidFlatrightWeDailyEurMwh)(GasGplEexCyr03MidFlatrightWeDailyEurMwh)
}

if(GasGplEexCyr04MidFlatrightWeDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexCyr04MidFlatrightWeDailyEurMwh)(GasGplEexCyr04MidFlatrightWeDailyEurMwh)
}



if(GasGplEexSe01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexSe01MidDailyEurMwh)(GasGplEexSe01MidDailyEurMwh)
}

if(GasGplEexSe02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexSe02MidDailyEurMwh)(GasGplEexSe02MidDailyEurMwh)
}

if(GasGplEexSe03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexSe03MidDailyEurMwh)(GasGplEexSe03MidDailyEurMwh)
}

if(GasGplEexSe04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexSe04MidDailyEurMwh)(GasGplEexSe04MidDailyEurMwh)
}

if(GasGplEexSe05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplEexSe05MidDailyEurMwh)(GasGplEexSe05MidDailyEurMwh)
}

if(GasNcgEexSe01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexSe01MidDailyEurMwh)(GasNcgEexSe01MidDailyEurMwh)
}

if(GasNcgEexSe02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexSe02MidDailyEurMwh)(GasNcgEexSe02MidDailyEurMwh)
}

if(GasNcgEexSe03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexSe03MidDailyEurMwh)(GasNcgEexSe03MidDailyEurMwh)
}

if(GasNcgEexSe04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexSe04MidDailyEurMwh)(GasNcgEexSe04MidDailyEurMwh)
}

if(GasNcgEexSe05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasNcgEexSe05MidDailyEurMwh)(GasNcgEexSe05MidDailyEurMwh)
}



if(GasGplHerenSum01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenSum01MidDailyEurMwh)(GasGplHerenSum01MidDailyEurMwh)
}

if(GasGplHerenSum02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenSum02MidDailyEurMwh)(GasGplHerenSum02MidDailyEurMwh)
}
if(GasGplHerenSum03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenSum03MidDailyEurMwh)(GasGplHerenSum03MidDailyEurMwh)
}


if(GasGplHerenSum01BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenSum01BidDailyEurMwh)(GasGplHerenSum01BidDailyEurMwh)
}

if(GasGplHerenSum02BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenSum02BidDailyEurMwh)(GasGplHerenSum02BidDailyEurMwh)
}
if(GasGplHerenSum03BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenSum03BidDailyEurMwh)(GasGplHerenSum03BidDailyEurMwh)
}

if(GasGplHerenSum01AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenSum01AskDailyEurMwh)(GasGplHerenSum01AskDailyEurMwh)
}

if(GasGplHerenSum02AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenSum02AskDailyEurMwh)(GasGplHerenSum02AskDailyEurMwh)
}
if(GasGplHerenSum03AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenSum03AskDailyEurMwh)(GasGplHerenSum03AskDailyEurMwh)
}





if(GasGplHerenWin01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWin01MidDailyEurMwh)(GasGplHerenWin01MidDailyEurMwh)
}

if(GasGplHerenWin02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWin02MidDailyEurMwh)(GasGplHerenWin02MidDailyEurMwh)
}
if(GasGplHerenWin03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWin03MidDailyEurMwh)(GasGplHerenWin03MidDailyEurMwh)
}


if(GasGplHerenWin01BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWin01BidDailyEurMwh)(GasGplHerenWin01BidDailyEurMwh)
}

if(GasGplHerenWin02BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWin02BidDailyEurMwh)(GasGplHerenWin02BidDailyEurMwh)
}
if(GasGplHerenWin03BidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWin03BidDailyEurMwh)(GasGplHerenWin03BidDailyEurMwh)
}

if(GasGplHerenWin01AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWin01AskDailyEurMwh)(GasGplHerenWin01AskDailyEurMwh)
}

if(GasGplHerenWin02AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWin02AskDailyEurMwh)(GasGplHerenWin02AskDailyEurMwh)
}
if(GasGplHerenWin03AskDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasGplHerenWin03AskDailyEurMwh)(GasGplHerenWin03AskDailyEurMwh)
}


if(GasTtfIceMaMidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceMaMidDailyEurMwh)(GasTtfIceMaMidDailyEurMwh)
}

if(GasTtfIceMa02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceMa02MidDailyEurMwh)(GasTtfIceMa02MidDailyEurMwh)
}

if(GasTtfIceMa03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceMa03MidDailyEurMwh)(GasTtfIceMa03MidDailyEurMwh)
}

if(GasTtfIceMa04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceMa04MidDailyEurMwh)(GasTtfIceMa04MidDailyEurMwh)
}

if(GasTtfIceMa05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceMa05MidDailyEurMwh)(GasTtfIceMa05MidDailyEurMwh)
}

if(GasTtfIceMa06MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceMa06MidDailyEurMwh)(GasTtfIceMa06MidDailyEurMwh)
}

if(GasTtfIceQ01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceQ01MidDailyEurMwh)(GasTtfIceQ01MidDailyEurMwh)
}

if(GasTtfIceQ02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceQ02MidDailyEurMwh)(GasTtfIceQ02MidDailyEurMwh)
}

if(GasTtfIceQ03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceQ03MidDailyEurMwh)(GasTtfIceQ03MidDailyEurMwh)
}

if(GasTtfIceQ04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceQ04MidDailyEurMwh)(GasTtfIceQ04MidDailyEurMwh)
}

if(GasTtfIceSe01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceSe01MidDailyEurMwh)(GasTtfIceSe01MidDailyEurMwh)
}

if(GasTtfIceSe02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceSe02MidDailyEurMwh)(GasTtfIceSe02MidDailyEurMwh)
}

if(GasTtfIceSe03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceSe03MidDailyEurMwh)(GasTtfIceSe03MidDailyEurMwh)
}

if(GasTtfIceSe04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceSe04MidDailyEurMwh)(GasTtfIceSe04MidDailyEurMwh)
}

if(GasTtfIceSe05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceSe05MidDailyEurMwh)(GasTtfIceSe05MidDailyEurMwh)
}

if(GasTtfIceSe06MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceSe06MidDailyEurMwh)(GasTtfIceSe06MidDailyEurMwh)
}

if(GasTtfIceCyr01MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceCyr01MidDailyEurMwh)(GasTtfIceCyr01MidDailyEurMwh)
}

if(GasTtfIceCyr02MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceCyr02MidDailyEurMwh)(GasTtfIceCyr02MidDailyEurMwh)
}

if(GasTtfIceCyr03MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceCyr03MidDailyEurMwh)(GasTtfIceCyr03MidDailyEurMwh)
}

if(GasTtfIceCyr04MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceCyr04MidDailyEurMwh)(GasTtfIceCyr04MidDailyEurMwh)
}

if(GasTtfIceCyr05MidDailyEurMwh.nonEmpty) {
  writeWithTimeStamp(RDMS_GasTtfIceCyr05MidDailyEurMwh)(GasTtfIceCyr05MidDailyEurMwh)
}