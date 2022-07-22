
    val prd = new Service("prd", Some(dsPrd), Some(cdsPrd)) with Containers with Markets with Lim2
    implicit val wb = FO_SUPPORT

    val asof = yesterday
    val supply = Year(2022) to Year(2025)
    def check(series: Series[Day, Double], tag: String) = {
      if (series.start > Day(supply.head)) {
        log warn s"$tag is starting late at ${series.start}"
      }
      if (series.end < Day(supply.last + 1) - 1) {
        log warn s"$tag is terminating early at ${series.end}"
      }
      if (!series.mapTime(_ - series.start).isRegular) {
        log warn s"$tag contains gaps"
      }
      series
    }

    log info "CREATE STATIC TIMESERIES ..."
    val wpVolumes = supply.toSeries(_ => 450000.0)
    val mbmVolumes = supply.toSeries(_ => 70000.0)
    val biopMpp3Volumes = supply.toSeries(_ => 3294.1)
    val biopContractedVolumes = supply.toSeries(_ => 21900.0)
    val heaviesVolumes = supply.toSeries(_ => 26000.0)
    val markup = supply.toSeries(_ => 17.0)
    val correctionFactor = supply.toSeries(_ => 0.6)
    log info "... DONE, VOLUMES DEFINED"

    val efficiency = 0.4

    log info "READING FORWARDCURVES FROM CDS/DS ..."
    val coal = change(check(prd.CoalApi2Fc.Mid(asof).getSeries, "coal")) to Year
    val carbon = change(check(prd.CarbonEuaFc.EcxMid(asof).getSeries, "carbon")) to Year

    val power = change({
      val fc = change(prd.PowerNedFc.Mid(asof).getSeries) to Day
      val fcHist = {
        val Price = Numerical("Average_Price")
        object APXEndexPowerNLDailyAverage extends Symbol[String] {
          lazy val symbol = "APXEndex_PowerNLDailyAverage/ts?&cols=Average_Price&Daily_Results=BASE&timezone=Europe/Amsterdam"
        }
        val range = LimIterator(Day(supply.head) iterator fc.start)
        Price.from(APXEndexPowerNLDailyAverage)(prd, range, Day(_), DateTimeLikeOrdering[Day]) collect {

          case (t, Some(x)) => (t, x)
        }
      }
      check(fcHist ++ fc, "power")
    } ).to(Year)


    val fx = change(check(prd.Reuters1730.EurUsdMid(asof).getSeries,"fx")).to(Year)
    val wp = change({
      val woodPelletsCifAraMidUsd = new prd.BiomassEodFc.ForwardCurveStream[Day, Day]("WOOD_PELLETS_CIF_ARA_MID_USD_%s")
      check(woodPelletsCifAraMidUsd(asof).getSeries, "wood pellets")
    }).to(Year)
    log info "... DONE, GOT FWD CURVES"

    log info "CREATE BIOMASS CURVE FROM INPUTS ..."
    val subsidySum = {
      val subsidy = (-power + 105d).max(0d)
      ((biopMpp3Volumes * 50d).min(biopContractedVolumes * 17.5)  + mbmVolumes * 17.5 + wpVolumes * 17.5) * efficiency * subsidy / 3.6}

    val costSum = {
      val wpEur = wp / fx
      val coalEur = coal / fx
      val wpCost = wpVolumes * wpEur * 17.5 / 17d
      val mbmCost = mbmVolumes * ((carbon + coalEur - 0.02) * 0.5 - 0.3 - 3.01 + markup)
      val biopCost = biopMpp3Volumes * (correctionFactor * 50d / 25.12) * (coalEur + carbon * 2.38)
      val heaviesCost = heaviesVolumes* (coalEur * 38.1 / 25.12)
      wpCost + mbmCost + biopCost + heaviesCost
    }

    val totalVolume = wpVolumes * 17.5 + mbmVolumes * 17.5 + biopMpp3Volumes * 50d + heaviesVolumes * 38.1

    val bioMass = change((costSum - subsidySum) * 17.5 / totalVolume).to(Day)
    log info "... DONE, BIOMASS CREATED"

    log info "STORING RESULT IN CDS/DS ..."
    object BiomassEodFc extends prd.Container("BIOMASS_EOD_FC") with prd.ForwardCurveContainer {

      object BiomassEur extends ForwardCurveStream[Day, Day]("BIOMASS_EUR_%s")
    }

    BiomassEodFc.BiomassEur.getOrCreate(asof) <-- bioMass
    log info "... DONE, BIOMASS CURVE CREATED AND STORED"
