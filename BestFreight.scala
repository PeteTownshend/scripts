val srv  = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim2

//val asof = today-1
val asof = yesterday //running at 4:10 for previous day

implicit val wb = FO_SUPPORT

def iter = asof iterator - 5.days

val capeFloor     = 10811.00 
val panamaxFloor  =  6667.00

// max  ( FREIGHT_CAPETC_FC.BALTIC_MID_%s  , STANDINGDATA.CAPESIZE_TC4_FLOOR_20151001 )
//      FREIGHT_CAPETC_ADJ_FC.BALTIC_MID_%s  
// max  ( FREIGHT_PANAMAXTC_FC.BALTIC_MID_%s  , STANDINGDATA.PANAMAX_TC4_FLOOR_20151001 )
//      FREIGHT_PANAMAXTC_ADJ_FC.BALTIC_MID_%s 

lazy val capeTc = srv.FreightCapetcFc.BalticMid.get(asof) flatMap {_.series}
lazy val panamaxTc = srv.FreightPanamaxtcFc.Baltic.get(asof) flatMap {_.series}
lazy val fo35FobAraBarges = srv.OilEodFc.Fo35FobAraBargesMonthlyAvgUsd.get(asof) flatMap {_.series} map {change(_) to Day}

object CapeTcFloor extends srv.Container("STANDINGDATA") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("CAPESIZE_TC4_FLOOR_20151001" )   
}

object CapeTc5 extends srv.Container("FREIGHT_CAPETC5_FC") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )   
}

object CapeTc5F extends srv.Container("FREIGHT_CAPETC5_FC") with srv.ForwardCurveContainer {
  object BalticMidFlat extends ForwardCurveStream[Day, Day]("BALTIC_MID_FLAT_%s" )   
}

object ImoFobAraBarges extends srv.Container("OIL_EOD_FC") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("IMO_0_1_FOB_ARA_BARGES_%s" )   
}

object UmoFobAraBarges extends srv.Container("OIL_EOD_FC") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("UMO_0_1_FOB_ARA_BARGES_%s" )   
}

object MfoFobAraBarges extends srv.Container("OIL_EOD_FC") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("MFO_0_5_FOB_ARA_BARGES_%s" )   
}

object FreightCapetcAdjFc extends srv.Container("FREIGHT_CAPETC_ADJ_FC") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )   
}

object FreightCapetc5AdjFc extends srv.Container("Freight_CAPETC5_ADJ_FC") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )   
}

object FreightCapetc5AdjFcF extends srv.Container("Freight_CAPETC5_ADJ_FC") with srv.ForwardCurveContainer {
  object BalticMidFlat extends ForwardCurveStream[Day, Day]("BALTIC_MID_FLAT_%s" )   
}

object PanamaxTcFloor extends srv.Container("STANDINGDATA") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("PANAMAX_TC4_FLOOR_20151001" )    
}

object FreightPanamaxtcAdjFc extends srv.Container("FREIGHT_PANAMAXTC_ADJ_FC") with srv.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )   
}

object CapInDB extends srv.Container("CAPACITY") with srv.ForwardCurveContainer {
  object DeuBelDiff extends ForwardCurveStream[Day, Hour]("DEU_BEL_DIFF_%s" )   
}

object CapInBD extends srv.Container("CAPACITY") with srv.ForwardCurveContainer {
  object BelDeuDiff extends ForwardCurveStream[Day, Hour]("BEL_DEU_DIFF_%s" )   
}

object CapInFB extends srv.Container("CAPACITY") with srv.ForwardCurveContainer {
  object FraBelDiff extends ForwardCurveStream[Day, Hour]("FRA_BEL_DIFF_%s" )   
}

object CapInBF extends srv.Container("CAPACITY") with srv.ForwardCurveContainer {
  object BelFraDiff extends ForwardCurveStream[Day, Hour]("BEL_FRA_DIFF_%s" )   
}

object StdIn extends srv.Container("STANDINGDATA") with srv.ForwardCurveContainer {
  object ZeroH extends ForwardCurveStream[Day, Hour]("zeros_h" )    
}

object CapOutDB extends srv.Container("CAPACITY") with srv.ForwardCurveContainer {
  object DeuBel extends ForwardCurveStream[Day, Hour]("DEU_BEL_%s" )    
}

object CapOutBD extends srv.Container("CAPACITY") with srv.ForwardCurveContainer {
  object BelDeu extends ForwardCurveStream[Day, Hour]("BEL_DEU_%s" )    
}

object CapOutFB extends srv.Container("CAPACITY") with srv.ForwardCurveContainer {
  object FraBel extends ForwardCurveStream[Day, Hour]("FRA_BEL_%s" )    
}

object CapOutBF extends srv.Container("CAPACITY") with srv.ForwardCurveContainer {
  object BelFra extends ForwardCurveStream[Day, Hour]("BEL_FRA_%s" )    
}

lazy val capeTcFloor = CapeTcFloor.BalticMid.get(asof) flatMap {_.series}
lazy val panamaxTcFloor = PanamaxTcFloor.BalticMid.get(asof) flatMap {_.series}
lazy val imoFobAraBarges = ImoFobAraBarges.BalticMid.get(asof) flatMap {_.series} map {change(_) to Day}
lazy val umoFobAraBarges = UmoFobAraBarges.BalticMid.get(asof) flatMap {_.series} map {change(_) to Day}
lazy val mfoFobAraBarges = MfoFobAraBarges.BalticMid.get(asof) flatMap {_.series} map {change(_) to Day}
                                   
lazy val capeTc5 = CapeTc5.BalticMid.get(asof) flatMap {_.series}
lazy val capeTc5F = CapeTc5F.BalticMidFlat.get(asof) flatMap {_.series}
lazy val deuBelDiff = CapInDB.DeuBelDiff.get(asof) flatMap {_.series}
lazy val belDeuDiff = CapInBD.BelDeuDiff.get(asof) flatMap {_.series}
lazy val fraBelDiff = CapInFB.FraBelDiff.get(asof) flatMap {_.series}
lazy val belFraDiff = CapInBF.BelFraDiff.get(asof) flatMap {_.series}
lazy val zeroH      = StdIn.ZeroH.get(asof) flatMap {_.series}

lazy val bestFreight = for{
  rc4 <- srv.FreightRc4Fc.BalticMid.fallBackSeries(iter)
  rc7 <- srv.FreightRc7Fc.BalticMid.fallBackSeries(iter)
  bol <- srv.FreightSynthFc.PanamaxBolivarRdamMid.fallBackSeries(iter)
  bay <- srv.FreightSynthFc.PanamaxRichbayRdamMid.fallBackSeries(iter)
  ban <- srv.FreightSynthFc.PanamaxBanjarRdamMid.fallBackSeries(iter)
} yield (rc4 zip rc7 zip bol zip bay zip ban) mapValues {flatten(_).min}

lazy val capeTcAdj = for {
  tc1 <- capeTc
  tc2 <- capeTcFloor.map(change(_).to(Day))
} yield (tc1 zip tc2) mapValues {flatten(_).max}

lazy val capeTc5Adj = for {
  tc <- capeTc5
//fo  <- fo35FobAraBarges.map(change(_).to(Day))
//imo <- imoFobAraBarges.map(change(_).to(Day))
//umo <- umoFobAraBarges.map(change(_).to(Day))
  mfo <- mfoFobAraBarges.map(change(_).to(Day))
} yield {
  (72.0 - 0.02 * tc + 5.65 * mfo)
}  

lazy val capeTc5AdjF = for {
  tcf <- capeTc5F
//umo <- umoFobAraBarges.map(change(_).to(Day))
  mfo <- mfoFobAraBarges.map(change(_).to(Day))
} yield {
  (72.0 - 0.02 * tcf + 5.65 * mfo)
} 

// Cape5tc_ADJ = $72 - 0.02 x 5TC cape price + 5.65 x UMO price 
// UMO = HFO pre  2020 and 
// UMO = IMO post 2020  
lazy val panamaxTcAdj = for {
  tc1 <- panamaxTc
  tc2 <- panamaxTcFloor.map(change(_).to(Day))
} yield (tc1 zip tc2) mapValues {flatten(_).max}

lazy val deuBel = for {
  tc1 <- deuBelDiff
  tc2 <- zeroH  
} yield (tc1 zip tc2) mapValues {flatten(_).max}

lazy val belDeu = for {
  tc1 <- belDeuDiff
  tc2 <- zeroH  
} yield (tc1 zip tc2) mapValues {flatten(_).max}

lazy val fraBel = for {
  tc1 <- fraBelDiff
  tc2 <- zeroH  
} yield (tc1 zip tc2) mapValues {flatten(_).max}

lazy val belFra = for {
  tc1 <- belFraDiff
  tc2 <- zeroH  
} yield (tc1 zip tc2) mapValues {flatten(_).max}

srv.getOrCreate("FREIGHT_CAPETC_ADJ_FC")
srv.getOrCreate("FREIGHT_CAPETC5_ADJ_FC")
srv.getOrCreate("FREIGHT_PANAMAXTC_ADJ_FC")
srv.getOrCreate("CAPACITY")

bestFreight map { srv.FreightSynthFc.BestFreightMid.getOrCreate(asof) <-- _ }
capeTcAdj map { FreightCapetcAdjFc.BalticMid.getOrCreate(asof) <-- _ }
capeTc5Adj map { FreightCapetc5AdjFc.BalticMid.getOrCreate(asof) <-- _ }
capeTc5AdjF map { FreightCapetc5AdjFcF.BalticMidFlat.getOrCreate(asof) <-- _ }        
panamaxTcAdj map { FreightPanamaxtcAdjFc.BalticMid.getOrCreate(asof) <-- _ }
deuBel map { CapOutDB.DeuBel.getOrCreate(asof) <-- _ }
belDeu map { CapOutBD.BelDeu.getOrCreate(asof) <-- _ }
fraBel map { CapOutFB.FraBel.getOrCreate(asof) <-- _ }
belFra map { CapOutBF.BelFra.getOrCreate(asof) <-- _ }