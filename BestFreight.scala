val prd  = new Service("prd", Some(dsPrd), Some(cdsPrd)) with Containers with Markets with Lim2
val int  = new Service("int", Some(dsDev), Some(cdsDev)) with Containers with Markets with Lim2

val asof = yesterday //running at 4:10 for previous day

implicit val wb = FO_SUPPORT

def iter = asof iterator - 5.days

val capeFloor     = 10811.00 
val panamaxFloor  =  6667.00

lazy val capeTc = prd.FreightCapetcFc.BalticMid.get(asof) flatMap {_.series}
lazy val panamaxTc = prd.FreightPanamaxtcFc.Baltic.get(asof) flatMap {_.series}
lazy val fo35FobAraBarges = prd.OilEodFc.Fo35FobAraBargesMonthlyAvgUsd.get(asof) flatMap {_.series} map {change(_) to Day}

object CapeTcFloor extends prd.Container("STANDINGDATA") with prd.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("CAPESIZE_TC4_FLOOR_20151001" )   
}

object CapeTc5 extends prd.Container("FREIGHT_CAPETC5_FC") with prd.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )   
}

object CapeTc5F extends prd.Container("FREIGHT_CAPETC5_FC") with prd.ForwardCurveContainer {
  object BalticMidFlat extends ForwardCurveStream[Day, Day]("BALTIC_MID_FLAT_%s" )   
}

object ImoFobAraBarges extends prd.Container("OIL_EOD_FC") with prd.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("IMO_0_1_FOB_ARA_BARGES_%s" )   
}

object UmoFobAraBarges extends prd.Container("OIL_EOD_FC") with prd.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("UMO_0_1_FOB_ARA_BARGES_%s" )   
}

object MfoFobAraBarges extends prd.Container("OIL_EOD_FC") with prd.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("MFO_0_5_FOB_ARA_BARGES_%s" )   
}

object PanamaxTcFloor extends prd.Container("STANDINGDATA") with prd.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Month]("PANAMAX_TC4_FLOOR_20151001" )    
}

object CapInDB extends prd.Container("CAPACITY") with prd.ForwardCurveContainer {
  object DeuBelDiff extends ForwardCurveStream[Day, Hour]("DEU_BEL_DIFF_%s" )   
}

object CapInBD extends prd.Container("CAPACITY") with prd.ForwardCurveContainer {
  object BelDeuDiff extends ForwardCurveStream[Day, Hour]("BEL_DEU_DIFF_%s" )   
}

object CapInFB extends prd.Container("CAPACITY") with prd.ForwardCurveContainer {
  object FraBelDiff extends ForwardCurveStream[Day, Hour]("FRA_BEL_DIFF_%s" )   
}

object CapInBF extends prd.Container("CAPACITY") with prd.ForwardCurveContainer {
  object BelFraDiff extends ForwardCurveStream[Day, Hour]("BEL_FRA_DIFF_%s" )   
}

object StdIn extends prd.Container("STANDINGDATA") with prd.ForwardCurveContainer {
  object ZeroH extends ForwardCurveStream[Day, Hour]("zeros_h" )    
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
  rc4 <- prd.FreightRc4Fc.BalticMid.fallBackSeries(iter)
  rc7 <- prd.FreightRc7Fc.BalticMid.fallBackSeries(iter)
  bol <- prd.FreightSynthFc.PanamaxBolivarRdamMid.fallBackSeries(iter)
  bay <- prd.FreightSynthFc.PanamaxRichbayRdamMid.fallBackSeries(iter)
  ban <- prd.FreightSynthFc.PanamaxBanjarRdamMid.fallBackSeries(iter)
} yield (rc4 zip rc7 zip bol zip bay zip ban) mapValues {flatten(_).min}

lazy val capeTcAdj = for {
  tc1 <- capeTc
  tc2 <- capeTcFloor.map(change(_).to(Day))
} yield (tc1 zip tc2) mapValues {flatten(_).max}

lazy val capeTc5Adj = for {
  tc <- capeTc5
  mfo <- mfoFobAraBarges.map(change(_).to(Day))
} yield {
  (72.0 - 0.02 * tc + 5.65 * mfo)
}  

lazy val capeTc5AdjF = for {
  tcf <- capeTc5F
  mfo <- mfoFobAraBarges.map(change(_).to(Day))
} yield {
  (72.0 - 0.02 * tcf + 5.65 * mfo)
} 

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

int.getOrCreate("FREIGHT_CAPETC_ADJ_FC")
int.getOrCreate("FREIGHT_CAPETC5_ADJ_FC")
int.getOrCreate("FREIGHT_PANAMAXTC_ADJ_FC")
int.getOrCreate("CAPACITY")

object FreightCapetcAdjFc extends int.Container("FREIGHT_CAPETC_ADJ_FC") with int.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )   
}

object FreightCapetc5AdjFc extends int.Container("Freight_CAPETC5_ADJ_FC") with int.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )   
}

object FreightCapetc5AdjFcF extends int.Container("Freight_CAPETC5_ADJ_FC") with int.ForwardCurveContainer {
  object BalticMidFlat extends ForwardCurveStream[Day, Day]("BALTIC_MID_FLAT_%s" )   
}

object FreightPanamaxtcAdjFc extends int.Container("FREIGHT_PANAMAXTC_ADJ_FC") with int.ForwardCurveContainer {
  object BalticMid extends ForwardCurveStream[Day, Day]("BALTIC_MID_%s" )   
}

object CapOutDB extends int.Container("CAPACITY") with int.ForwardCurveContainer {
  object DeuBel extends ForwardCurveStream[Day, Hour]("DEU_BEL_%s" )    
}

object CapOutBD extends int.Container("CAPACITY") with int.ForwardCurveContainer {
  object BelDeu extends ForwardCurveStream[Day, Hour]("BEL_DEU_%s" )    
}

object CapOutFB extends int.Container("CAPACITY") with int.ForwardCurveContainer {
  object FraBel extends ForwardCurveStream[Day, Hour]("FRA_BEL_%s" )    
}

object CapOutBF extends int.Container("CAPACITY") with int.ForwardCurveContainer {
  object BelFra extends ForwardCurveStream[Day, Hour]("BEL_FRA_%s" )    
}

bestFreight map { int.FreightSynthFc.BestFreightMid.getOrCreate(asof) <-- _ }
capeTcAdj map { FreightCapetcAdjFc.BalticMid.getOrCreate(asof) <-- _ }
capeTc5Adj map { FreightCapetc5AdjFc.BalticMid.getOrCreate(asof) <-- _ }
capeTc5AdjF map { FreightCapetc5AdjFcF.BalticMidFlat.getOrCreate(asof) <-- _ }        
panamaxTcAdj map { FreightPanamaxtcAdjFc.BalticMid.getOrCreate(asof) <-- _ }
deuBel map { CapOutDB.DeuBel.getOrCreate(asof) <-- _ }
belDeu map { CapOutBD.BelDeu.getOrCreate(asof) <-- _ }
fraBel map { CapOutFB.FraBel.getOrCreate(asof) <-- _ }
belFra map { CapOutBF.BelFra.getOrCreate(asof) <-- _ }