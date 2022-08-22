// TODO: check service used in line 144
// TODO: add this k,v pair to the script config map <- important
// "SKIP_HOLIDAYS"  -> "0",

import eet.io.cds.DataRow
import org.joda.time.{DateTime, DateTimeZone}
import com.eon.servers.common.binarydata.datatables.DataColumn
import scala.util.Try

val prd = new Service("prd", Some(dsPrd), Some(cdsPrd)) with Containers with Markets with Lim2
val uat = new Service("uat", Some(dsUat), Some(cdsUat)) with Containers with Markets with Lim2
implicit val wb = RISK_CALCULATOR

def isHoliday(day: Day): Boolean = day.isWeekend || List(
  Day(2022, 1, 3),
  Day(2022, 4, 14),
  Day(2022, 4, 15),
  Day(2022, 4, 18),
  Day(2022, 5, 2),
  Day(2022, 5, 17),
  Day(2022, 5, 26),
  Day(2022, 6, 2),
  Day(2022, 6, 3),
  Day(2022, 6, 6),
  Day(2022, 8, 29),
  Day(2022, 10, 3),
  Day(2022, 10, 31),
  Day(2022, 12, 26),
  Day(2022, 12, 27),
  Day(2023, 1, 2),
  Day(2023, 4, 6),
  Day(2023, 4, 7),
  Day(2023, 4, 10),
  Day(2023, 5, 1),
  Day(2023, 5, 17),
  Day(2023, 5, 18),
  Day(2023, 5, 29),
  Day(2023, 8, 28),
  Day(2023, 10, 3),
  Day(2023, 12, 25),
  Day(2023, 12, 26)
).contains(day)

def formerWorkingDay(day: Day): Day = {
  var d = day
  while (isHoliday(d)) d -= 1
  d
}

val skip_holidays: Int = V {
  config.getString("SKIP_HOLIDAYS").toInt
} match {
  case Success(value) if 0 <= value && value <= 2 => value
  case _ => log warn s"please set SKIP_HOLIDAYS to correct number of non quoting days"; 0
}
val tradingDate = formerWorkingDay(yesterday)
val tradingDateFormatted = tradingDate.toString("yyyy-MM-dd")
val golden = uat.Masterdataprices

def v[T](ot: Option[T])(error: String): V[Throwable, T] =
  ot.fold(new Throwable(error).failure[T])(_.success[Throwable])

type S = Series[Month, Double]
def vDeltaUpdates(srv: Service, cName: String, bName: String, tradingDate: Day) = {
  val expiryCondition: (ResultSet, Day) => Boolean = (rs: ResultSet, tradingDate: Day) => {
    val expiry = Day(rs.getTimestamp("EXPIRY_DATE"))
    val power_yearly = """(fd_fo_power_\w+_\w+_y.*)""".r
    bName match {
      case power_yearly(_) => {
        val cond1 = (tradingDate <= expiry)
        val cond2 = (expiry.month == 12)
        cond1 && cond2
      }
      case _ => tradingDate <= expiry
    }
  }

  for {
    ds <- v {
      srv.ods
    }(s"DataServices ${srv.name} unavailable.")
    resultSet <- V {
      ds.executeQuery(s"return cds.$cName.${bName}_$tradingDate;")
    }
  } yield {
    scala.collection.immutable.SortedMap.empty[Double, S] ++
      Iterator.continually(resultSet)
        .takeWhile(_.next())
        .collect {
          case rs if expiryCondition(rs, tradingDate) =>
            val delta = rs.getDouble("DELTA")
            // change to desired granularity later, now Month forall
            val t = Month(rs.getTimestamp("START_DATE"))
            val x = rs.getDouble("VALUE")
            delta -> (t, x)
        }.toIterable
        .groupBy { case (delta, _) => delta }
        .mapValues { groupedValues =>
          //Series(tupleValues.map{case(_,(month,vola)) => (month,vola)})  // m00 .. m19 .. m39
          Series(groupedValues.map(_._2))
        }
  }
}

// take existing Table, add new row from prices series
// target: masterdataprices.ng_fo_ng_the_m_dc01 .. dc99
def vUpdate[T](bucketName: String, masterdataprices: golden.Entity, goldenSource: DataTable, prices: S, market: MarketLike): V[Throwable, DataTable] = V {
  def logRow(row: DataRow): Unit = {
    val rowValues = for (i <- 1 to goldenSource.getColumnCount) yield {
      val dataColumn = goldenSource.getColumn(i)
      dataColumn.getColumnName -> row.getValue(dataColumn)
    }
    val rowValuesPatched = rowValues match {
      case xs if xs.size > 12 => xs.patch(6, Seq(("... ", "... ")), xs.size - 11)
      case xs => xs
    }
    log info rowValuesPatched.map(_._1).mkString(", ")
    log info rowValuesPatched.map(_._2).mkString(", ")
  }

  val cbName = masterdataprices.containerBucket.fold("")(t => t._1 + "." + t._2)
  val nCols = goldenSource.getColumnCount - 1
  val lastDay: Day = {
    val lastRow: DataRow = goldenSource.getRow(goldenSource.getRowCount - 1)
    val firstColumn: DataColumn = goldenSource.getColumn(1)
    Day(new DateTime(lastRow.getValue(firstColumn)))
  }
  val lastDayFormatted = lastDay.toString("yyyy-MM-dd")
  val updateCondition: Boolean = {
    lastDay.isBefore(tradingDate) &&
      (lastDay.until(tradingDate).filterNot(_.isWeekend).size - 1 == skip_holidays) // skipping non-quoting days
  }
  if (updateCondition) {
    val row = goldenSource.newRow()
    //val tz: DateTimeZone = TimeZone.forID(market.timeZoneName)
    val tz: DateTimeZone = TimeZone.forID("Europe/Berlin")
    row.setValue(1, tradingDate.dateTime.withZone(tz)) //tradingDate.toString("yyyy-MM-dd")

    // Done: handle gaps in prices update
    val gran = """(^.*_([mqy])$)""".r
    val prices_T = bucketName match {
      case gran(_, "m") => {
        val t0 = prices.head._1
        (t0 until (t0 + nCols.months)).toSeries(t => prices.getOrElse(t, -1))
      }
      case gran(_, "q") => {
        val prices_Q = prices.mapTime(Quarter(_))
        val t0 = prices_Q.head._1
        (t0 until (t0 + nCols.quarters)).toSeries(t => prices.getOrElse(t, -1))
      }
      case gran(_, "y") => {
        val prices_Y = prices.mapTime(Year(_))
        val t0 = prices_Y.head._1
        (t0 until (t0 + nCols.years)).toSeries(t => prices.getOrElse(t, -1))
      }
      case str =>
        throw new IllegalArgumentException(s"granularity not supported: $str")
    }
    prices_T.values.zipWithIndex foreach { case (price, column) => row.setValue(2 + column, price) }

    goldenSource.addRow(row)
    log info s"${cbName.toLowerCase}   (APPENDED)"
    logRow(row)
  } else {
    log info s"${cbName.toLowerCase}   (SKIPPED)   last entry was: $lastDayFormatted."
    //logRow(goldenSource.getRow(goldenSource.getRowCount - 1))
  }
  goldenSource
}

List(
  // HINT: market used to format TimeZone for eod date in target dataTable (first column),
  // hard coded to use Germany ("Europe/Berline") forall, see line 144
  // Carbon, coal, gas
  ("volatility_eod", "fd_fo_carbon_eua_y", "fd_fo_carbon_eua_y_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_carbon_uka_y", "fd_fo_carbon_uka_y_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_coal_api2_m", "fd_fo_coal_api2_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_coal_api2_q", "fd_fo_coal_api2_q_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_coal_api2_y", "fd_fo_coal_api2_y_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_ng_jkm_m", "ng_fo_jkm_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_ng_nbp_m", "ng_fo_nbp_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_ng_the_m", "ng_fo_the_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_ng_ttf_m", "ng_fo_ttf_m_dc", Markets.GERMANY),
  // Power
  ("volatility_eod", "fd_fo_power_deu_base_m", "fd_fo_power_deu_base_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_deu_base_q", "fd_fo_power_deu_base_q_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_deu_base_y", "fd_fo_power_deu_base_y_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_esp_base_m", "fd_fo_power_esp_base_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_esp_base_q", "fd_fo_power_esp_base_q_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_esp_base_y", "fd_fo_power_esp_base_y_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_fra_base_m", "fd_fo_power_fra_base_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_fra_base_q", "fd_fo_power_fra_base_q_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_fra_base_y", "fd_fo_power_fra_base_y_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_ita_base_m", "fd_fo_power_ita_base_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_ita_base_q", "fd_fo_power_ita_base_q_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_ita_base_y", "fd_fo_power_ita_base_y_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_ned_base_m", "fd_fo_power_ned_base_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_ned_base_q", "fd_fo_power_ned_base_q_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_ned_base_y", "fd_fo_power_ned_base_y_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_sys_base_m", "fd_fo_power_sys_base_m_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_sys_base_q", "fd_fo_power_sys_base_q_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_sys_base_y", "fd_fo_power_sys_base_y_dc", Markets.GERMANY),
  ("volatility_eod", "fd_fo_power_uk_base_m", "fd_fo_power_uk_base_m_dc", Markets.GERMANY),

) foreach { case (containerName, bucketName, goldenSourceName, market) =>

  if (skip_holidays == 0) {
    log info s"------------------------------------------------------------------"
    log info s"updating $goldenSourceName @ $tradingDateFormatted for market ${market.name}"
  } else {
    log warn s"------------------------------------------------------------------"
    log warn s"updating $goldenSourceName @ $tradingDateFormatted for market ${market.name}, skipping $skip_holidays holidays."
  }

  // no logging here
  val vAllUpdates = vDeltaUpdates(prd, containerName, bucketName, tradingDate)

  // some logging here in vUpdate
  val vResult = for {
    deltaUpdates <- vAllUpdates //vDeltaUpdates(prd, containerName, bucketName, tradingDate)
  } yield {
    for {
      (delta, update) <- deltaUpdates
      targetBucketName = f"$goldenSourceName${(delta * 100).toInt}%02d"
      masterdataprices = golden.getOrCreate(targetBucketName)
    } yield {
      for {
        targetDataTable <- v {
          masterdataprices.dataTable
        }("")
        updatedDataTable <- vUpdate(bucketName, masterdataprices, targetDataTable, update, market)
        result <- V(uat.ocds.get.writeDataTable("masterdataprices", targetBucketName, updatedDataTable)) //bucket write table
      } yield {
        result
      }
    }
  }

  vResult match {

    case Success(_) =>
      log info s"updated $goldenSourceName @ $tradingDateFormatted"

    case Failure(throwable: Throwable) =>
      log error s"failed to update $goldenSourceName due to:"
      log error s"  (1) ${throwable.getMessage}"
      Try(log error s"  (2) ${throwable.getCause.getMessage}")
  }
  vResult
}