val srv  = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim2

val asof = yesterday

implicit val wb = FO_SUPPORT

val fo35FobAraBAvg = for {
	foM0  <- srv.GoEndurCurves.Fo35FobAraBMonthlyAvgUsd get asof flatMap { _.series } map { s => Series(s.head) }
	foFwd <- srv.OilEodFc.Fo35FobAraBarges get asof flatMap { _.series } map { _ from (foM0.start + 1) }
} yield foM0 ++ foFwd

val iceBrentAvg = for {
	brentM0  <- srv.GoEndurCurves.IceBrentSwapMonthlyAvgUsd get asof flatMap { _.series } map { s => Series(s.head) }
	brentFwd <- srv.OilEodFc.IceBrentSwap get asof flatMap { _.series } map { _ from (brentM0.start + 1) }
} yield brentM0 ++ brentFwd

fo35FobAraBAvg map { srv.OilEodFc.Fo35FobAraBargesMonthlyAvgUsd.getOrCreate(asof) <-- _ }
iceBrentAvg map { srv.OilEodFc.IceBrentSwapMonthlyAvgUsd.getOrCreate(asof) <-- _ }