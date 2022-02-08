val srv = new Service("script", Some(ds), Some(cds)) with Containers
implicit val wb = FO_SUPPORT

object reuters extends srv.Container("REUTERS_1730") with srv.ForwardCurveContainer {

  object lateNight extends ForwardCurveStream[Day, Day]("LATE_NIGHT_%s")
}

val asof = Day(2022, 2, 1)

srv.Reuters1730.EurUsdMid.get(asof).flatMap(_.series) match {

  case Some(series) =>
    change(series) to Year foreach { kv =>

      log info kv
    }
    val newSeries = series * 2d
    reuters.lateNight.getOrCreate(asof) <-- (series * 2d)
    reuters.lateNight.get(asof).flatMap(_.series) match {

      case Some(late) if late == newSeries =>
        log info "round trip works"

      case Some(late) if late.isEmpty =>
        throw new Exception("empty round trip")

      case Some(late) =>
        throw new Exception("curve differs")

      case None =>
        throw new Exception("not instance at all")
    }

  case None =>
    throw new Exception("Housten, we got a problem")
}