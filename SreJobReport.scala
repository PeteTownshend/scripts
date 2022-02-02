val srv = new Service("script2", Some(ds), Some(cds)) with Containers with Markets with Lim2

val asof = yesterday

implicit val wb = Workbench("FC_write")

//get all execution id(s) for OstMarketDataModel jobs
val executionIdsQuery = """
return sre_scheduler.sre_schedules alias schedules
join sre_scheduler.vw_latest_execution_status alias stats
select (
schedules.schedule_name, 
stats.model_execution_id, 
schedules.model_id, 
schedules.is_enabled, 
schedules.is_adhoc, 
stats.status_id, 
stats.model_execution_id)
where (
schedules.model_id = 1081
and
schedules.id = stats.schedule_id);"""

log info srv.execute(executionIdsQuery)

val executionIds = srv.execute(executionIdsQuery) map { rs =>

	val iterator = 	new Iterator[ResultSet] { def hasNext = rs.next; def next = rs } toIterable

	iterator map {item => item.getString(1) -> item.getLong(2)} toList
}

//get recent executions for a given execution id
val executionStatsQuery: Long => String = id => s"""
return sre.model_execution alias execution
select(
execution.execution_start_date_utc,
execution.execution_end_date_utc,
execution.model_response_code,
execution.model_response_message
)
where(
execution.model_execution_id = $id
and
execution.execution_start_date_utc >= date.Parse('${asof.toString("yyyy-MM-dd")} 11:00:00.000+02:00')
);"""

val executionStats = executionIds.get flatMap { case (name, id) => 

	srv.execute(executionStatsQuery(id)) flatMap { rs => 

		val iterator = 	new Iterator[ResultSet] { def hasNext = rs.next; def next = rs } toIterable

		val list = iterator map {item => (item.getObject(1), item.getObject(2), item.getDouble(3), item.getString(4))} toList

		list match {
			case (start, end, response, message) :: Nil => 
				Some(id -> s"$name,$start,$end,$response,$message")
			case _ => 
				None
		}
	}	
}

//get logs that yield write statements
val logQuery: Long => String = id => s"return sre.MODEL_LOG_DATA SELECT (MESSAGE) WHERE (MODEL_EXECUTION_ID = $id);"

def getWriteLogs(id: Long) = srv.execute(logQuery(id)) map { rs => 

	val iterator = 	new Iterator[ResultSet] { def hasNext = rs.next; def next = rs } toIterable

	iterator.map(_.getString(1)).toList.filter(log => log.startsWith("Writing CDS") || log.startsWith("Writing Endur file"))
	
}	

val path = "//u-dom1.u-ssi.net/DFSRoot34000/SPECIAL/Forward Curves/Options/CDSVols/PRD/stats/"
val writeLogsf: ((Long,String)) => String =  item => s"${item._1},${item._2}"

write(path,s"executionStats_${asof.toString("yyyy-MM-dd")}.csv") map { _(executionStats.iterator, writeLogsf)(
	Some("id,schedule_name,execution_start_date,execution_end_date,response_code,response_message")) }

//to create
//id_1, Write_1
//id_1, Write_2
//id_2, ...
val writeLogs = (executionStats :\  List.empty[(Long,String)])((execution, list) => 

	getWriteLogs(execution._1) match {
		case Some(logs) => logs.sorted.map(log => execution._1 -> log) ++ list
		case None => list
	}
)

write(path,s"writeLogs_${asof.toString("yyyy-MM-dd")}.csv") map { _(writeLogs.iterator, writeLogsf)(None) }