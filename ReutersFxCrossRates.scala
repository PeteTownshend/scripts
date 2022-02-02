val srv = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim

val asof 	 = today //running at 17:45 for current day
implicit val wb = FO_SUPPORT

lazy val reuters 	= srv.Reuters1730

lazy val eurUsd = reuters.EurUsdMid.get(asof) map {_.getSeries}

lazy val eurCad = for {
	eurUsd <- eurUsd
	usdCad <- reuters.UsdCadMid.get(asof) map {_.getSeries}
} yield usdCad * eurUsd
 
lazy val eurRon = for {
	eurUsd <- eurUsd
	usdRon <- reuters.UsdRonMid.get(asof) map {_.getSeries}
} yield usdRon * eurUsd

lazy val eurPln = for {
	eurUsd <- eurUsd
	usdPln <- reuters.UsdPlnMid.get(asof) map {_.getSeries}
} yield usdPln * eurUsd

eurCad map { reuters.EurCadMid.getOrCreate(asof) <-- _ }
eurRon map { reuters.EurRonMid.getOrCreate(asof) <-- _ }
eurPln map { reuters.EurPlnMid.getOrCreate(asof) <-- _ }