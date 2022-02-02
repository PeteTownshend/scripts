trait ForwardCurveService2 { service: Service =>

  trait ForwardCurveContainer2 { container: Container =>

    trait CdsString[S <: DateTimeLike[S]] {
      implicit def toCdsString(s: S): String = s match {
        case q: Quarter => q.toString.replace(".", "_")
        case m: Month => m.toString("yyyy_MMM")
        case d: Day => d.toString("yyyyMMdd")
      }
    }

    class TariffCurveStream[S <: DateTimeLike[S], T <: DateTimeLike[T]: Manifest](val name: String)
      extends ForwardCurveLike[S, T, Double] with CdsString[S] {

      private val endDate: T => Object = manifest[T].runtimeClass match {

        case e if e.equals(classOf[Month]) =>
          startDate => ((startDate + 1): Day) - 1

        case _ =>
          startDate => startDate + 1
      }

      val columnSet: List[Column[_]] = List(
        CDateTime("TRADE_DATE"),
        CDateTime("START_DATE").time,
        CDateTime("END_DATE"),
        CDouble("VALUE").value
      )

      def toColumns(tradeDate: S, startDate: T): List[((T, Double)) => _] = List(
        (_: (T, Double)) => tradeDate,
        (e: (T, Double)) => e._1,
        (e: (T, Double)) => endDate(e._1),
        (e: (T, Double)) => e._2
      )
    }
  }
}

val srv  = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim2
val srv2 = new Service("script2", Some(ds), Some(cds)) with Containers with ForwardCurveService2 with Markets with Lim2

val asof = yesterday

implicit val wb = FO_SUPPORT

lazy val nbpMid = srv.NgNbpFc.Mid.get(asof) flatMap {_.series}
lazy val ncgMid = srv.NgNcgFc.Mid.get(asof) flatMap {_.series}

lazy val EurGbpMid = srv.Reuters1730.EurGbpMid.get(asof) flatMap {_.series}
lazy val EurNokMid = srv.Reuters1730.EurNokMid.get(asof) flatMap {_.series}

lazy val Tariff = new srv2.Container("TARIFF") with srv2.ForwardCurveContainer2
lazy val GasscoExit = new Tariff.TariffCurveStream[Day, Month]("gassco_all_exit_firm_cap_y").get(asof) flatMap { _.series }
lazy val DornumEntry = new Tariff.TariffCurveStream[Day, Month]("oge_h_dornum_ept2_entry_cap_d").get(asof) flatMap { _.series}
lazy val EasingtonEntry = new Tariff.TariffCurveStream[Day, Month]("nts_easington_langld_entry_d").get(asof) flatMap { _.series}
lazy val StandingData = new srv.Container("STANDINGDATA") with srv.ForwardCurveContainer
lazy val varCosts = new StandingData.ForwardCurveStream[Day, Day]("nts_entry_variable_costs").get(asof) flatMap { _.series }

lazy val AreaDWinter = for {
	nbp     <- nbpMid
	EntryUK <- EasingtonEntry map (change(_).to(Day))
	ExitNOK <- GasscoExit map (change(_).to(Day))
	EurGbp  <- EurGbpMid map (change(_).to(Month)) map (change(_).to(Day))
	EurNok  <- EurNokMid map (change(_).to(Month)) map (change(_).to(Day))
} yield nbp / EurGbp / 2.930746461 - (ExitNOK / EurNok / 0.0114 + EntryUK / EurGbp * 41.66666667)

lazy val AreaDSummer = for {
	ncg     <- ncgMid
	EntryDE <- DornumEntry map (change(_).to(Day))
	ExitNOK <- GasscoExit map (change(_).to(Day))
	EurNok  <- EurNokMid map (change(_).to(Month)) map (change(_).to(Day))
} yield ncg - ExitNOK / EurNok / 0.0114 - EntryDE * 41.66666667

lazy val AreaD = for {
	UK <- AreaDWinter
	DE <- AreaDSummer
} yield (UK zip DE) mapValues { eet.core.flatten(_).max }

object AreaDFc extends srv.Container("NG_AREA_D_FC") with srv.ForwardCurveContainer {

  object Mid extends ForwardCurveStream[Day, Day]("Mid_%s")
}

AreaD map {AreaDFc.Mid.getOrCreate(asof) <-- _ }