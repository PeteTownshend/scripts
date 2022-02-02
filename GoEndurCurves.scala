//more information in http://sm03698.dom1.e-ssi.net/svn/MP/trunk/MPM/Projects/Gaps/

import scala.math.pow

implicit val srv = new Service("script", Some(ds), Some(cds)) with Containers with ForwardCurveService with Markets with Lim2

implicit val wb = FO_SUPPORT

//val asof = yesterday
val asof = today
val histStart = Day(Year(asof) - 6)
val fwdEnd = asof.year + 15
val fwdEndDay = Day(fwdEnd, 12, 31)
implicit def limIter = LimIterator(asof iterator histStart)
def limForecastIter = LimIterator((Year(asof) - 6 iterator (Year(asof) + 16)) map (Day(_)))

val omit = Interpolate.omit[Day, Double] _
val yearly = omit andThen (change(_) to Year)

def writeWithTimeStamp[A <: DateTimeLike[A]](fwdCurve: Service#Container#ForwardCurveLike[Day, A, Double])(series: Series[A, Double])(implicit a: org.joda.time.DateTime => A){

  // cut off everything beyond fwdEndDay
  if(!series.isEmpty) {

    fwdCurve.getOrCreate(asof) <-- series.to(series.start.companion(fwdEndDay))

    val timeStamp = fwdCurve(asof).metaData.get.timeStamp.toString("yyyy-MM-dd HH:mm:ss")
    val column = CString("TimeStamp")
    val timeStampTable = DataTable.fromIterableRow[String](List((column, (s:String) => s)))(List(timeStamp::Nil))

    fwdCurve(asof).container.getOrCreate(s"${fwdCurve(asof).name}_timestamp").writeToCds(timeStampTable)
  }
}

// missing symbols
object CBSCPINLNETHERLAND_2015 extends eet.io.Symbol[String] { lazy val symbol = "CBS.CPINL.NETHERLAND_2015" }
object CBSLABORELECGASWATERSUPPLYCAOWAGESPERMONTHINCLSPECIALPAYMENTSTOTAL2000 extends Symbol[String] { lazy val symbol = "CBS.LABOR.ELEC.GAS.WATER.SUPPLY.CAO.WAGES.PER.MONTH.INCL.SPECIAL.PAYMENTS.TOTAL.2000" }
object CBSPPINLBYEACMANUFACTURINGMONTHLY2010 extends Symbol[String] { lazy val symbol = "CBS.PPINL.BY.EA.C.MANUFACTURING.MONTHLY.2010" }
object CBSPPINLBYPDT25FABRICATEDMETALPRODUCTSEXMACHINERYEQUIPMENTMONTHLY2010 extends Symbol[String] { lazy val symbol = "CBS.PPINL.BY.PDT.25.FABRICATED.METAL.PRODUCTS.EX.MACHINERY.EQUIPMENT.MONTHLY.2010" }
object CBSPPINLBYPDT2711ELECTRICMOTORSGENERATORSTRANSFORMERSMONTHLY2010 extends Symbol[String] { lazy val symbol = "CBS.PPINL.BY.PDT.2711.ELECTRIC.MOTORS.GENERATORS.TRANSFORMERS.MONTHLY.2010" }
object CBSCPINLNETHERLAND_2006 extends Symbol[String] { lazy val symbol = "CBS.CPINL.NETHERLAND_2006" }
object IHSCPINETHERLANDSYEARLY extends Symbol[String] { lazy val symbol = "IHS.CPI.NETHERLANDS.YEARLY" }
object IHSJULCGERMANYYEARLY extends Symbol[String] { lazy val symbol = "IHS.JULC.GERMANY.YEARLY" }
object IHSJULCNETHERLANDSYEARLY extends Symbol[String] { lazy val symbol = "IHS.JULC.NETHERLANDS.YEARLY" }
object IHSPPIGERMANYYEARLY extends Symbol[String] { lazy val symbol = "IHS.PPI.GERMANY.YEARLY" }
object IHSPPINETHERLANDSYEARLY extends Symbol[String] { lazy val symbol = "IHS.PPI.NETHERLANDS.YEARLY" }
object STATSBUNDEAGREEDHOURLYEARNINGSEXCLBONUSENERGYSUPPLY2010 extends Symbol[String] { lazy val symbol = "STATS.BUNDE.AGREED.HOURLY.EARNINGS.EXCL.BONUS.ENERGY.SUPPLY.2010" }
object STATSBUNDENEGOTIATEDMONTHLYEARNINGSEXCLBONUSENERGYSUPPLY2010 extends Symbol[String] { lazy val symbol = "STATS.BUNDE.NEGOTIATED.MONTHLY.EARNINGS.EXCL.BONUS.ENERGY.SUPPLY.2010" }
object STATSBUNDEPRODUCTSCAPITALGOODSBASE2010 extends Symbol[String] { lazy val symbol = "STATS.BUNDE.PRODUCTS.CAPITALGOODS.BASE.2010" }
object STATSBUNDEWHOLESALEOVERALLINDEX2010 extends Symbol[String] { lazy val symbol = "STATS.BUNDE.WHOLESALE.OVERALL.INDEX.2010" }

// missing fields
val ChangePct = Numerical("ChangePct")
val CpIndex = Numerical("CpIndex")
val DomesticOutputPrices = Numerical("DomesticOutputPrices")
val Index = Numerical("Index")
val MonthlyValue = Numerical("MonthlyValue")
val MthAvg = Numerical("MthAvg")

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

lazy val grossWagesGermany = {
  implicit def limIter = limForecastIter
  yearly(ChangePct from IHSJULCGERMANYYEARLY) match {
    case series if(!series.isEmpty) => Some(series / 100.0)
    case _ => None
  }
}

lazy val grossWagesNetherlands = {
  implicit def limIter = limForecastIter
  yearly(ChangePct from IHSJULCNETHERLANDSYEARLY) match {
    case series if(!series.isEmpty) => Some(series / 100.0)
    case _ => None
  }
}
lazy val producerPriceIndexNetherlands = {
  implicit def limIter = limForecastIter
  yearly(ChangePct from IHSPPINETHERLANDSYEARLY) match {
    case series if(!series.isEmpty) => Some(series / 100.0)
    case _ => None
  }
}


//priliminary, for UAT
val GoEndurCurves = new srv.Container("GO_ENDUR_CURVES") with srv.ForwardCurveContainer
val EonIndexBrdGewerblicheErzeugnisse = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_BRD_GEWERBLICHE_ERZEUGNISSE_%s")
val EonIndexBrdGrosshandVerkauf2005 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_BRD_GROSSHAND_VERKAUF_2005_%s")
val EonIndexBrdTarifMonatsgehEnergieversMf2010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_BRD_TARIF_MONATSGEH_ENERGIEVERS_MF_2010_%s")
val EonIndexBrdTarifStundenEnergievers2010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_BRD_TARIF_STUNDEN_ENERGIEVERS_2010_%s")
val EonIndexNlConsumerPriceNder2006 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_CONSUMER_PRICE_NDER_2006_%s")
val EonIndexNlConsumerPriceNder2015 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_CONSUMER_PRICE_NDER_2015_%s")
val EonIndexNlPrijsCaoIonenEnergie2000 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_PRIJS_CAO_IONEN_ENERGIE_2000_%s")
val EonIndexNlProducerPriceProdcom252010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_PRODUCER_PRICE_PRODCOM_25_2010_%s")
val EonIndexNlProducerPriceProdcom27112010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_PRODUCER_PRICE_PRODCOM_2711_2010_%s")
val EonIndexNlProducerPriceSbi20082010 = new GoEndurCurves.EndurCurveStream[Day, Month]("EON_INDEX_NL_PRODUCER_PRICE_SBI2008_2010_%s")

val WagesAvGweKirchmoeser14thSalaryChristmasBonusEur = new GoEndurCurves.EndurCurveStream[Day, Month]("WAGES_AV_GWE_KIRCHMOESER_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val WagesAvGweKirchmoeser14thSalaryChristmasBonusEurHist = new srv.GoEndurCurves.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_KIRCHMOESER_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST")
val WagesAvGwe19014thSalaryChristmasBonusEur = new GoEndurCurves.EndurCurveStream[Day, Month]("WAGES_AV_GWE_1_90_14TH_SALARY_CHRISTMAS_BONUS_EUR_%s")
val WagesAvGwe19014thSalaryChristmasBonusEurHist = new srv.GoEndurCurves.ForwardCurveStream[Day, Month]("WAGES_AV_GWE_1_90_14TH_SALARY_CHRISTMAS_BONUS_EUR_HIST")

//end of priliminary, for UAT

lazy val wagesAvGweKirchmoeser14thSalaryChristmasBonusEurHist = WagesAvGweKirchmoeser14thSalaryChristmasBonusEurHist.get(asof) flatMap { _ series }
lazy val wagesAvGwe19014thSalaryChristmasBonusEurHist = WagesAvGwe19014thSalaryChristmasBonusEurHist.get(asof) flatMap { _ series }

lazy val eonIndexBrdTarifMonatsgehEnergieversMf2010 = for {
  index <- grossWagesGermany
  (lastDay, lastValue) = omit(Index from STATSBUNDENEGOTIATEDMONTHLYEARNINGSEXCLBONUSENERGYSUPPLY2010).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val eonIndexBrdTarifStundenEnergievers2010 = for {
  index <- grossWagesGermany
  (lastDay, lastValue) = omit(Index from STATSBUNDEAGREEDHOURLYEARNINGSEXCLBONUSENERGYSUPPLY2010).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val eonIndexBrdGewerblicheErzeugnisse = for {
  index <- producerPriceIndexGermany
  (lastDay, lastValue) = omit(MthAvg from STATSBUNDEPRODUCTSCAPITALGOODSBASE2010).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val eonIndexBrdGrosshandVerkauf2005 = for {
  index <- producerPriceIndexGermany
  (lastDay, lastValue) = omit(Index from STATSBUNDEWHOLESALEOVERALLINDEX2010).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * NL-B
  */
lazy val eonIndexNlProducerPriceSbi20082010 = for {
  index <- consumerPriceIndexNl
  (lastDay, lastValue) = omit(DomesticOutputPrices from CBSPPINLBYEACMANUFACTURINGMONTHLY2010).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * NL-A
  */
lazy val eonIndexNlConsumerPriceNder2006 = for {
  index <- consumerPriceIndexNl
  (lastDay, lastValue) = omit(CpIndex from CBSCPINLNETHERLAND_2006).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

lazy val eonIndexNlConsumerPriceNder2015 = for {
  index <- consumerPriceIndexNl
  (lastDay, lastValue) = omit(CpIndex from CBSCPINLNETHERLAND_2015).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * NL-D
  */
lazy val eonIndexNlPrijsCaoIonenEnergie2000 = for {
  index <- grossWagesNetherlands
  (lastDay, lastValue) = omit(MonthlyValue from CBSLABORELECGASWATERSUPPLYCAOWAGESPERMONTHINCLSPECIALPAYMENTSTOTAL2000).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * NL-F
  */
lazy val eonIndexNlProducerPriceProdcom27112010 = for {
  index <- producerPriceIndexNetherlands
  (lastDay, lastValue) = omit(DomesticOutputPrices from CBSPPINLBYPDT2711ELECTRICMOTORSGENERATORSTRANSFORMERSMONTHLY2010).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * NL-E
  */
lazy val eonIndexNlProducerPriceProdcom252010 = for {
  index <- producerPriceIndexNetherlands
  (lastDay, lastValue) = omit(DomesticOutputPrices from CBSPPINLBYPDT25FABRICATEDMETALPRODUCTSEXMACHINERYEQUIPMENTMONTHLY2010).last
  lastMonth = Month(lastDay)
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * Wages - Kirchmöser
  */
lazy val wagesAvGweKirchmoeser14thSalaryChristmasBonusEurFwd = for {
  index <- grossWagesGermany
  (lastMonth, lastValue) <- wagesAvGweKirchmoeser14thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * Wages - Kirchmöser
  * monthly
  */
lazy val wagesAvGweKirchmoeser14thSalaryChristmasBonusEur = for {
  fwd <- wagesAvGweKirchmoeser14thSalaryChristmasBonusEurFwd
  hist <- wagesAvGweKirchmoeser14thSalaryChristmasBonusEurHist
} yield fwd ++ hist

/**
  * Wages - 190
  */
lazy val wagesAvGwe19014thSalaryChristmasBonusEurFwd = for {
  index <- grossWagesGermany
  (lastMonth, lastValue) <- wagesAvGwe19014thSalaryChristmasBonusEurHist map { _.last }
} yield (Series(lastMonth -> lastValue) /: (lastMonth + 1 to index.end.Dec)){
  (s, m) => s + (m -> s.lastValue * pow(1 + index(m), 1/12d))
}

/**
  * Wages - 190
  * monthly
  */
lazy val wagesAvGwe19014thSalaryChristmasBonusEur = for {
  fwd <- wagesAvGwe19014thSalaryChristmasBonusEurFwd
  hist <- wagesAvGwe19014thSalaryChristmasBonusEurHist
} yield fwd ++ hist




// ###############################################################################################
// ### ENDUR GO
// ###

eonIndexBrdGewerblicheErzeugnisse map writeWithTimeStamp(EonIndexBrdGewerblicheErzeugnisse)
eonIndexBrdTarifMonatsgehEnergieversMf2010 map writeWithTimeStamp(EonIndexBrdTarifMonatsgehEnergieversMf2010)
eonIndexBrdGrosshandVerkauf2005 map writeWithTimeStamp(EonIndexBrdGrosshandVerkauf2005)
eonIndexBrdTarifStundenEnergievers2010 map writeWithTimeStamp(EonIndexBrdTarifStundenEnergievers2010)
eonIndexNlProducerPriceSbi20082010 map writeWithTimeStamp(EonIndexNlProducerPriceSbi20082010)
eonIndexNlConsumerPriceNder2006 map writeWithTimeStamp(EonIndexNlConsumerPriceNder2006)
eonIndexNlConsumerPriceNder2015 map writeWithTimeStamp(EonIndexNlConsumerPriceNder2015)
eonIndexNlPrijsCaoIonenEnergie2000 map writeWithTimeStamp(EonIndexNlPrijsCaoIonenEnergie2000)
eonIndexNlProducerPriceProdcom27112010 map writeWithTimeStamp(EonIndexNlProducerPriceProdcom27112010)
eonIndexNlProducerPriceProdcom252010 map writeWithTimeStamp(EonIndexNlProducerPriceProdcom252010)
wagesAvGweKirchmoeser14thSalaryChristmasBonusEur map writeWithTimeStamp(WagesAvGweKirchmoeser14thSalaryChristmasBonusEur)
wagesAvGwe19014thSalaryChristmasBonusEur map writeWithTimeStamp(WagesAvGwe19014thSalaryChristmasBonusEur)
