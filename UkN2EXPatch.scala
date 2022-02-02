//more information in http://sm03698.dom1.e-ssi.net/svn/MP/trunk/MPM/Projects/Gaps/

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat

val asof = today

val tzUk = DateTimeZone.forID("Europe/London")
val tzCET = DateTimeZone.forID("Europe/Berlin")

implicit def limIter = LimIterator(Hour(asof + 2) iterator Hour(asof - 3))
val omit = Interpolate.omit[Hour, Double] _

def ukStringToDateTime(dt: String)(implicit patterns: Seq[String] = eet.core.time.pattern): DateTime = {
  patterns.headOption match {
    case None =>
      val msg = "%s does not match one of the provided date formats" format dt
      log.error(msg)
      sys.error(msg)
    case Some(pattern) =>
      try DateTimeFormat.forPattern(pattern).withZone(tzUk).parseDateTime(dt) catch {
        case _: Throwable => ukStringToDateTime(dt)(patterns.tail)
      }
  }
}

def tryOption[T](t: => T): Option[T] = try Some(t) catch {

  case t: Throwable =>
    log error t.getMessage
    None
}

case class CExclTimezoneOverride(override val name: String) extends Column[DateTime](name) { self =>
  implicit val stringToDateTime = ukStringToDateTime _

  val cdateTime = CDateTime(name)
  type J = cdateTime.J

  def cp(newName: String = name) = self.copy(newName)
  val clazz = cdateTime.clazz
  val fromCds = (rs: ResultSet) => rs.getObject(name).toString: DateTime
  val toCds = cdateTime.toCds
  def unapply(s: String) = cdateTime.unapply(s)
}

trait Lim2Override { self: eet.io.cds.Lim2 with eet.io.cds.CanExecuteQuery with Logging =>
  //import eet.io.cds._
  /**
    * @TODO doesn't handle rollover dates nor daylight saving
    */
  override def apply[G <: DateTimeLike[G], A[_]](
                                                  iter: Iterator[G],
                                                  symbol: String,
                                                  fields: A[String])(
                                                  eval: A[Double] => Double)(
                                                  implicit i: Flat[A], c: DateTime => G, o: Ordering[G]): Series[G, Option[Double]] = iter.toList match {
    case Nil =>
      Series.empty[G, Option[Double]]
    case dates =>
      Series(dates map { _ -> None }) ++
        {
          getRecords(symbol, i.decode(fields): _*)(dates.min)(dates.max) map dateAndValue(eval) match {
            case Some(s) => s.mapTime(dates.min.companion(_)).filterTime(dates.contains(_)).mapValues(Some(_))
            case _ => Nil
          }
        }
  }

  private def columns(rs: ResultSet) = {
    val meta = rs.getMetaData
    val name: Int => String = meta.getColumnName
    List.range(2, meta.getColumnCount + 1) map name map { CString(_) }
  }

  private def dateAndValue[A[_]](eval: A[Double] => Double)(implicit i: Flat[A]): ResultSet => Series[DateTime, Double] =
    rs => Series(
      for {
        row <- (new Iterator[ResultSet] { def hasNext = rs.next; def next = rs }).toIterable
        date <- tryOption { CExclTimezoneOverride("Date").fromCds(row) }
        figures <- tryOption { columns(row) map { _.fromCds(row).toDouble } }
        value = eval(i.encode(figures))
      } yield date -> value
    )
}

implicit val svIn = new Service("script", Some(ds), Some(cds)) with Containers with Markets with Lim2 with Lim2Override
val svOut = svIn
implicit val wb = FO_SUPPORT

// missing symbold
object N2exUkDaAuction extends eet.io.Symbol[String] { lazy val symbol = "N2EX.UK.DA.AUCTION" }

// missing fields
val Close = Numerical("Close")
val CloseDST = Numerical("CloseDST")

//which DA prices, DA for UK in local time, i.e. UK time zone
//first relevant price is asof 23:00
//last relevant price is asof = 1 day 22:00
//todo check long and short day

val start = Hour(asof + 1)
//val end = Hour(asof + 2) - 1
//val end = Hour(asof + 4) - 1
val end = Hour(asof + 2) - 1

val ukLongHourFst = Hour(Year(asof.year, tzUk).longDay) //1 + 01:00
val ukLongHourSnd = Hour(Year(asof.year, tzUk).longDay) + 1.hours //1 + 00:00, utc

val closeUkZone = omit(Close from N2exUkDaAuction).mapTime(h => Hour(h.dateTime.withZoneRetainFields(tzUk)))
val closeDstUkZone = omit(CloseDST from N2exUkDaAuction ).mapTime(h => Hour(h.dateTime.withZoneRetainFields(tzUk)))

val close = closeUkZone.mapTime(h => if(h == ukLongHourFst) ukLongHourSnd else h)
val all = (close ++ closeDstUkZone).mapTime(h => Hour(h.dateTime.withZone(tzCET)))
val lim = all filterKeys { timestamp => timestamp >= start && timestamp <= end }

val argusMid = new svIn.PowerUkFc.ForwardCurveStream[Day, HalfHour]("MID_%s").get(asof) flatMap { _.series }

val patch = for {
  mid <- argusMid
  //expectedSize = (HalfHour(asof + 1) until HalfHour(asof + 2)).size
  //expectedSize = (HalfHour(asof + 1) until HalfHour(asof + 4)).size
  expectedSize = (HalfHour(asof + 1) until HalfHour(asof + 2)).size
  dayAhead = change(Series(lim)) to HalfHour
  if(dayAhead.size == expectedSize)
  if(dayAhead.start == mid.start)
} yield {
  mid ++ dayAhead
}

val ukMidPatch = new svOut.PowerUkFc.ForwardCurveStream[Day, HalfHour]("N2EX_MID_%s")

patch map { s =>
  ukMidPatch.getOrCreate(asof).<--(s)
  val t = DataTable.fromListOfString(List(CString("TimeStamp")))(List(now.toString("yyyy-MM-dd HH:mm:ss")::Nil))
  svOut.PowerUkFc.getOrCreate("N2EX_mid_%s_timestamp".format(asof.toString("yyyyMMdd"))).writeToCds(t)
}