val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim

val asof = today //running at 22:00 for current day
implicit val wb = FO_SUPPORT

object srv_Standingdata extends srv.Contner("STANDINGDATA") with srv.ForwardCurveContainer {

  object UkBsuosRcrc extends ForwardCurveStream[Day, Day]("UK_BSUOS_RCRC_%s" )
}

srv.Standingdata.get("UK_RCRC") map { _ --> srv.Standingdata.UkRcrc.getOrCreate(asof) }
srv.Standingdata.get("UK_BSUOS") map { _ --> srv.Standingdata.UkBsuos.getOrCreate(asof) }
srv.Standingdata.get("UK_BSUOS_RCRC") map { _ --> srv_Standingdata.UkBsuosRcrc.getOrCreate(asof) }