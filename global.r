library(RMySQL)
library(DBI)
library(plotly)
library(ggplot2)
library(shiny)
library(data.table)
library(plyr)
library(dplyr)
#------------------CONNECTION SETUP---------
conn <- dbConnect(RMySQL::MySQL(),
                  dbname="service_reminder",
                  user="root",
                  password="dt78#9Cv",
                  host= '142.93.216.190',
                  port=3306)
#----------------create data frame-----------------
inte_hist =data.table(dbGetQuery(conn,"SELECT * FROM integration_history WHERE DAYOFYEAR(DATE(startDate)) > (DAYOFYEAR(SYSDATE()) - 7)"))
event =data.table(dbGetQuery(conn,"SELECT * FROM event"))
sch_count = data.table(dbGetQuery(conn , "SELECT type,DATE(startDate) StartDate, count(*) count from integration_history
                                    WHERE  DAYOFYEAR(DATE(startDate)) > (DAYOFYEAR(SYSDATE()) - 7)
                                    GROUP BY DATE(startDate), type"))
api_count = data.table(dbGetQuery(conn,"SELECT apiName,DATE(startDate) StartDate, count(*) count from `event`
GROUP BY DATE(startDate), apiName"))
#-----------time diffrence between two dates ---------------
inte_hist[, "Time_taken" := difftime((inte_hist$endDate),(inte_hist$startDate))/60]
#---------------seprate date and time-------------
inte_hist[,Start_Date:=as.IDate(substr(startDate,1,10))]
inte_hist[,Start_Time:=as.ITime(substr(startDate,12,19))]
inte_hist[,End_Date:=as.IDate(substr(endDate,1,10))]
inte_hist[,End_Time:=as.ITime(substr(endDate,12,19))]

jobcard <- inte_hist[type=='Jobcard', .(counts,Start_Date,Start_Time,End_Date,End_Time,Time_taken,type)]
retail <- inte_hist[type=='Retail', .(counts,Start_Date,Start_Time,End_Date,End_Time,Time_taken,type)]
updatecalls <- inte_hist[type == "UpdateCalls", .(counts,Start_Date,Start_Time,End_Date,End_Time,Time_taken,type)]
booking <- inte_hist[type == "Booking", .(counts,Start_Date,Start_Time,End_Date,End_Time,Time_taken,type)]
#-------------groupby----------------------
inte_hist[, inte_hist, by= c('error')]
#---------------- count--------------------
jobcard_count <- sch_count[type == "Jobcard"]
retail_count <- sch_count[type == "Retail"]
updatecalls_count <- sch_count[type == "UpdateCalls"]
booking_count <- sch_count[type == "Booking"]
#--------------------success and failure------------------
status <- inte_hist %>%
  group_by(type) %>%
  count(error)
setDT(status)
status_suc <- status[status$error == "success",]
status_fail <- status[status$error == "fail",]
sta_err <- status_fail[status_suc, on = "type"]
error_status <- sta_err[, .(type,Fail_count= n, succ_count = i.n)]
error_status <- error_status[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

#-----------API time diffrence between two dates ---------------
event[, "Time_takenAPI" := difftime((event$endDate),(event$startDate))/60]

event[,Start_Date:=as.IDate(substr(startDate,1,10))]
event[,Start_Time:=as.ITime(substr(startDate,12,19))]
event[,End_Date:=as.IDate(substr(endDate,1,10))]
event[,End_Time:=as.ITime(substr(endDate,12,19))]
login <- event[apiName=='/login', .(Start_Date,End_Date,Time_takenAPI,apiName)]
logout <- event[apiName=='/logout', .(Start_Date,End_Date,Time_takenAPI,apiName)]
login_count <- api_count[apiName == "/login"]
logout_count <- api_count[apiName == "/logout"]
#--------------------success and failure------------------
statusAPI <- event %>%
  group_by(apiName) %>%
  count(status)
setDT(statusAPI)
status_sucAPI <- statusAPI[statusAPI$status == "Success",]
status_failAPI <- statusAPI[statusAPI$status == "Fail",]
sta_errAPI <- status_failAPI[status_sucAPI, on = "apiName"]
error_statusAPI <- sta_errAPI[, .(apiName,Fail_count= n, succ_count = i.n)]
error_statusAPI <- error_statusAPI[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]



#------------close connection-------------
dbDisconnect(conn)
#---------schduler query-----------------
query1= "SELECT counts, type, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime,DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM integration_history WHERE DAYOFYEAR(DATE(startDate)) > (DAYOFYEAR(SYSDATE()) - 7) AND type = 'Jobcard' "

query2 = "SELECT counts, type, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime, DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM integration_history WHERE DAYOFYEAR(DATE(startDate)) > (DAYOFYEAR(SYSDATE()) - 7) AND type = 'Retail'"

query3 = "SELECT counts, type, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime, DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM integration_history WHERE DAYOFYEAR(DATE(startDate)) > (DAYOFYEAR(SYSDATE()) - 7) AND type = 'UpdateCalls'"

query4 = "SELECT counts, type, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime, DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM integration_history WHERE DAYOFYEAR(DATE(startDate)) > (DAYOFYEAR(SYSDATE()) - 7) AND type = 'Booking'"

query5 = "SELECT t1.type, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_success,0)
as error_count_success FROM
((SELECT count(*)as error_count_fail,type, case when error = 'success' then 'success'
            else 'Fail' end as error_status ,DAYOFYEAR(startDate) as day_no
FROM integration_history
WHERE  error != UPPER('success') AND DAYOFYEAR(startDate) > (DAYOFYEAR(SYSDATE()) - 7)
GROUP BY type,error_status) as t1
LEFT OUTER JOIN
(SELECT count(*)as error_count_success,type, case when error = 'success' then 'success'
            else 'Fail' end as error_status,DAYOFYEAR(startDate) as day_no
FROM integration_history
WHERE error = UPPER('success')  AND DAYOFYEAR(startDate)  > (DAYOFYEAR(SYSDATE()) - 7)
GROUP BY type,error_status) as t2
ON t1.type = t2.type)

UNION

SELECT t2.type, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_success,0)
as error_count_success FROM
((SELECT count(*)as error_count_fail,type, case when error = 'success' then 'success'
            else 'Fail' end as error_status ,DAYOFYEAR(startDate) as day_no
FROM integration_history
WHERE  error != UPPER('success')   AND DAYOFYEAR(startDate)  > (DAYOFYEAR(SYSDATE()) - 7)
GROUP BY type,error_status) as t1
#RIGHT OUTER JOIN
RIGHT OUTER JOIN
(SELECT count(*)as error_count_success,type, case when error = 'success' then 'success'
            else 'Fail' end as error_status,DAYOFYEAR(startDate) as day_no
FROM integration_history
WHERE  error = UPPER('success')  AND DAYOFYEAR(startDate) > (DAYOFYEAR(SYSDATE()) - 7)
GROUP BY type,error_status) as t2
ON t1.type = t2.type)"

query6 = "SELECT count(*) as run_count, type FROM integration_history WHERE DAYOFYEAR(startDate) > (DAYOFYEAR(SYSDATE()) - 7) 
GROUP BY type"

query7 = "SELECT type,DATE(startDate) AS Date,TIME(startDate) as startTime,ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) 
AS time_taken FROM `integration_history`where DATE(startDate) = DATE(SYSDATE()) AND type IN ('Jobcard')"

query8 = "SELECT type,DATE(startDate) AS Date,TIME(startDate) as startTime,ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) 
AS time_taken FROM `integration_history`where DATE(startDate) = DATE(SYSDATE()) AND type IN ('Retail')"

query9 = "SELECT type,DATE(startDate) AS Date,TIME(startDate) as startTime,ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) 
AS time_taken FROM `integration_history`where DATE(startDate) = DATE(SYSDATE()) AND type IN ('updateCalls')"

query10 = "SELECT type,DATE(startDate) AS Date,TIME(startDate) as startTime,ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) 
AS time_taken FROM `integration_history`where DATE(startDate) = DATE(SYSDATE()) AND type IN ('Booking')"

query11 = "SELECT t1.type, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_Success,0)
as error_count_Success FROM
((SELECT count(*)as error_count_fail,type, case when error = 'Success' then 'Success'
            else 'Fail' end as error_status
FROM `integration_history`
WHERE error != UPPER('success')   AND DATE(startDate) = DATE(SYSDATE())
GROUP BY type,error_status) as t1
#RIGHT OUTER JOIN
LEFT OUTER JOIN
(SELECT count(*)as error_count_Success,type, case when error = 'Success' then 'Success'
            else 'Fail' end as error_status
FROM `integration_history`
WHERE error = UPPER('success')  AND DATE(startDate) = DATE(SYSDATE())
GROUP BY type,error_status) as t2
ON t1.type = t2.type)
UNION
SELECT t2.type, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_Success,0)
as error_count_Success FROM
((SELECT count(*)as error_count_fail,type, case when error = 'Success' then 'Success'
            else 'Fail' end as error_status 
FROM integration_history
WHERE error != UPPER('success')    AND DATE(startDate) = DATE(SYSDATE())
GROUP BY type,error_status) as t1
#RIGHT OUTER JOIN
RIGHT OUTER JOIN
(SELECT count(*)as error_count_Success,type, case when error = 'Success' then 'Success'
            else 'Fail' end as error_status
FROM integration_history
WHERE error = UPPER('success')  AND DATE(startDate) = DATE(SYSDATE())
GROUP BY type,error_status) as t2
ON t1.type = t2.type)"

query12 = "SELECT count(*) as run_count, type FROM integration_history WHERE DATE(startDate) = DATE(SYSDATE()) GROUP BY type"

query13 = "SELECT type,DATE(startDate) Date,error, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken
FROM integration_history
WHERE DATE(startDate) = DATE(SYSDATE())  AND type = 'UpdateCalls'  
AND ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1)  NOT BETWEEN 1 and 4  OR error != UPPER('success') and type = 'UpdateCalls' AND DATE(startDate) = DATE(SYSDATE())
UNION
SELECT type,DATE(startDate) Date,error, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken
FROM integration_history
WHERE DATE(startDate) = DATE(SYSDATE())  AND type = 'Retail'  
AND ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1)  NOT BETWEEN 1 and 3  OR error != UPPER('success') and type = 'Retail' AND DATE(startDate) = DATE(SYSDATE())
UNION
SELECT type,DATE(startDate) Date,error, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken
FROM integration_history
WHERE DATE(startDate) = DATE(SYSDATE())  AND type = 'Booking'  
AND ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1)  NOT BETWEEN 1 and 4  OR error != UPPER('success')  and type = 'Booking' AND DATE(startDate) = DATE(SYSDATE())
UNION
SELECT type,DATE(startDate) Date,error, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken
FROM integration_history
WHERE DATE(startDate) = DATE(SYSDATE())  AND type = 'Jobcard'  
AND ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1)  NOT BETWEEN 3 and 6  OR error != UPPER('success')  and type = 'Jobcard' AND DATE(startDate) = DATE(SYSDATE())
"

query14 = "SELECT count(*) as error_count from
(SELECT type,DATE(startDate) Date,error, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken
FROM integration_history
WHERE DATE(startDate) = DATE(SYSDATE())  AND type = 'UpdateCalls'  
AND ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1)  NOT BETWEEN 1 and 4  OR error != UPPER('success') and type = 'UpdateCalls' AND DATE(startDate) = DATE(SYSDATE())
UNION
SELECT type,DATE(startDate) Date,error, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken
FROM integration_history
WHERE DATE(startDate) = DATE(SYSDATE())  AND type = 'Retail'  
AND ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1)  NOT BETWEEN 1 and 3  OR error != UPPER('success') and type = 'Retail' AND DATE(startDate) = DATE(SYSDATE())
UNION
SELECT type,DATE(startDate) Date,error, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken
FROM integration_history
WHERE DATE(startDate) = DATE(SYSDATE())  AND type = 'Booking'  
AND ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1)  NOT BETWEEN 1 and 4  OR error != UPPER('success') and type = 'Booking' AND DATE(startDate) = DATE(SYSDATE())
UNION
SELECT type,DATE(startDate) Date,error, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken
FROM integration_history
WHERE DATE(startDate) = DATE(SYSDATE())  AND type = 'Jobcard'  
AND ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1)  NOT BETWEEN 3 and 6  OR error != UPPER('success') and type = 'Jobcard' AND DATE(startDate) = DATE(SYSDATE())
) as t1"


#----------query for get the currnet date fetch--------------
query25 <- "SELECT MAX(DATE(startDate)) as Date from integration_history" 
query26 = "SELECT MAX(DATE(startDate)) as Date from event"

#-------------API query-----------------

query15 = "SELECT apiName, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime, DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM `event` WHERE DAYOFYEAR(DATE(startDate)) > (DAYOFYEAR(SYSDATE()) - 7) AND apiName = '/login'"

query16 = "SELECT apiName, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime, DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM `event` WHERE DAYOFYEAR(DATE(startDate)) > (DAYOFYEAR(SYSDATE()) - 7) AND apiName = '/logOUT'"

query17 = "SELECT t1.apiName, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_Success,0)
as error_count_Success FROM
((SELECT count(*)as error_count_fail,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status ,DAYOFYEAR(startDate) as day_no
FROM `event`
WHERE `status` !=  UPPER('Success')  AND DAYOFYEAR(startDate) > (DAYOFYEAR(SYSDATE()) - 7)
GROUP BY apiName,error_status) as t1
#RIGHT OUTER JOIN
LEFT OUTER JOIN
(SELECT count(*)as error_count_Success,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status,DAYOFYEAR(startDate) as day_no
FROM `event`
WHERE `status` =  UPPER('Success') AND DAYOFYEAR(startDate) > (DAYOFYEAR(SYSDATE()) - 7)
GROUP BY apiName,error_status) as t2
ON t1.apiName = t2.apiName)
UNION
SELECT t2.apiName, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_Success,0)
as error_count_Success FROM
((SELECT count(*)as error_count_fail,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status ,DAYOFYEAR(startDate) as day_no
FROM event
WHERE `status` !=  UPPER('Success') AND DAYOFYEAR(startDate) > (DAYOFYEAR(SYSDATE()) - 7)
GROUP BY apiName,error_status) as t1
#RIGHT OUTER JOIN
RIGHT OUTER JOIN
(SELECT count(*)as error_count_Success,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status
FROM event
WHERE `status` =  UPPER('Success') AND DAYOFYEAR(startDate) > (DAYOFYEAR(SYSDATE()) - 7)
GROUP BY apiName,error_status) as t2
ON t1.apiName = t2.apiName)"

query18 = "SELECT count(*) as run_count, apiName
FROM `event`
WHERE DAYOFYEAR(startDate) > (DAYOFYEAR(SYSDATE()) - 7)
GROUP BY apiName
"

query19 = "SELECT apiName, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate FROM `event` where  DATE(startDate) = DATE(SYSDATE()) AND apiName = ('/login')"

query20 = "SELECT apiName, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate FROM `event` where  DATE(startDate) = DATE(SYSDATE()) AND apiName = ('/logout')"

query21 = "SELECT t1.apiName, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_Success,0)
as error_count_Success FROM
((SELECT count(*)as error_count_fail,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status ,DAYOFYEAR(startDate) as day_no
FROM `event`
WHERE `status` !=  ('Success')  AND DATE(startDate) = DATE(SYSDATE())
GROUP BY apiName,error_status) as t1
#RIGHT OUTER JOIN
LEFT OUTER JOIN
(SELECT count(*)as error_count_Success,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status,DAYOFYEAR(startDate) as day_no
FROM `event`
WHERE `status` =  ('Success') AND DATE(startDate) = DATE(SYSDATE())
GROUP BY apiName,error_status) as t2
ON t1.apiName = t2.apiName)
UNION
SELECT t2.apiName, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_Success,0)
as error_count_Success FROM
((SELECT count(*)as error_count_fail,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status ,DAYOFYEAR(startDate) as day_no
FROM event
WHERE `status` !=  ('Success') AND DATE(startDate) = DATE(SYSDATE())
GROUP BY apiName,error_status) as t1
#RIGHT OUTER JOIN
RIGHT OUTER JOIN
(SELECT count(*)as error_count_Success,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status
FROM event
WHERE `status` =  ('Success') AND DATE(startDate) = DATE(SYSDATE())
GROUP BY apiName,error_status) as t2
ON t1.apiName = t2.apiName)"

query22 = "SELECT count(*) as run_count, apiName FROM `event` WHERE DATE(startDate) = DATE(SYSDATE()) GROUP BY apiName"

query23 = "SELECT apiName,DATE(startDate) date,  `status`	from `event`
WHERE DATE(startDate) = DATE(SYSDATE()) AND `status` != UPPER('Success')"

query24 = "SELECT count(*)  as error_count	from `event`
WHERE DATE(startDate) = DATE(SYSDATE()) AND `status` != UPPER('Success')"

#------------------CONNECTION SETUP---------
getdata <- function(query) {
  conn <- dbConnect(RMySQL::MySQL(),
                    dbname="service_reminder",
                    user="root",
                    password="dt78#9Cv",
                    host= '142.93.216.190',
                    POrt=3306)
  
  #----------------create data frame-----------------
  sch1 <- data.frame(dbGetQuery(conn,query))
  on.exit(dbDisconnect(conn))
  return (sch1)
}
#-----------------query for daterange data of schdule---------------
query27 = paste("SELECT counts, type, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime,DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM integration_history WHERE DATE(startDate) BETWEEN 'std' AND 'etd' AND type = 'Jobcard' ")

query28 = paste("SELECT counts, type, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime,DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM integration_history WHERE DATE(startDate) BETWEEN 'std' AND 'etd' AND type = 'UpdateCalls' ")

query29 = paste("SELECT counts, type, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime,DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM integration_history WHERE DATE(startDate) BETWEEN 'std' AND 'etd' AND type = 'Retail' ")

query30 = paste("SELECT counts, type, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime,DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM integration_history WHERE DATE(startDate) BETWEEN 'std' AND 'etd' AND type = 'Booking' ")

query31 = paste("SELECT t1.type, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_success,0)
as error_count_success FROM
((SELECT count(*)as error_count_fail,type, case when error = 'success' then 'success'
            else 'Fail' end as error_status ,DATE(startDate) as date
FROM integration_history
WHERE  error != UPPER('success') AND DATE(startDate) BETWEEN 'std' AND 'etd'
GROUP BY type,error_status) as t1
LEFT OUTER JOIN
(SELECT count(*)as error_count_success,type, case when error = 'success' then 'success'
            else 'Fail' end as error_status,DATE(startDate) as date
FROM integration_history
WHERE error = UPPER('success')  AND DATE(startDate)  BETWEEN 'std' AND 'etd'
GROUP BY type,error_status) as t2
ON t1.type = t2.type)

UNION

SELECT t2.type, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_success,0)
as error_count_success FROM
((SELECT count(*)as error_count_fail,type, case when error = 'success' then 'success'
            else 'Fail' end as error_status ,DATE(startDate) as date
FROM integration_history
WHERE  error != UPPER('success')   AND DATE(startDate)  BETWEEN 'std' AND 'etd'
GROUP BY type,error_status) as t1
#RIGHT OUTER JOIN
RIGHT OUTER JOIN
(SELECT count(*)as error_count_success,type, case when error = 'success' then 'success'
            else 'Fail' end as error_status,DATE(startDate) as date
FROM integration_history
WHERE  error = UPPER('success')  AND DATE(startDate) BETWEEN 'std' AND 'etd'
GROUP BY type,error_status) as t2
ON t1.type = t2.type)")

query32 = paste("SELECT count(*) as run_count, type FROM integration_history WHERE DATE(startDate) BETWEEN 'std' AND 'etd' 
GROUP BY type")

#---------------query for daterange data of API event---------------
query33 = "SELECT apiName, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime, DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM `event` WHERE DATE(startDate) BETWEEN 'std' AND 'etd' AND apiName = '/login'"

query34 = "SELECT apiName, ROUND((TIME_TO_SEC(TIMEDIFF(endDate, startDate)) / 60), 1) AS time_taken,
DATE(startDate) AS StartDate, TIME(startDate)AS StartTime, DATE(endDate) AS EndDate , TIME(endDate)AS EndTime
FROM `event` WHERE DATE(startDate) BETWEEN 'std' AND 'etd' AND apiName = '/logOUT'"

query35 = "SELECT t1.apiName, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_Success,0)
as error_count_Success FROM
((SELECT count(*)as error_count_fail,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status ,DATE(startDate) as date
FROM `event`
WHERE `status` !=  ('Success') AND DATE(startDate)  BETWEEN 'std' AND 'etd'
GROUP BY apiName,error_status) as t1
#RIGHT OUTER JOIN
LEFT OUTER JOIN
(SELECT count(*)as error_count_Success,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status,DATE(startDate) as date
FROM `event`
WHERE `status` =  ('Success') AND DATE(startDate)  BETWEEN 'std' AND 'etd'
GROUP BY apiName,error_status) as t2
ON t1.apiName = t2.apiName)
UNION
SELECT t2.apiName, COALESCE(t1.error_count_fail,0) as error_count_fail ,COALESCE(t2.error_count_Success,0)
as error_count_Success FROM
((SELECT count(*)as error_count_fail,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status ,DATE(startDate) as date
FROM event
WHERE `status` !=  ('Success') AND DATE(startDate)  BETWEEN 'std' AND 'etd'
GROUP BY apiName,error_status) as t1
#RIGHT OUTER JOIN
RIGHT OUTER JOIN
(SELECT count(*)as error_count_Success,apiName, case when status = 'Success' then 'Success'
            else 'Fail' end as error_status,DATE(startDate) as date
FROM event
WHERE `status` =  ('Success') AND DATE(startDate)  BETWEEN 'std' AND 'etd'
GROUP BY apiName,error_status) as t2
ON t1.apiName = t2.apiName)"

query36 = "SELECT count(*) as run_count, apiName FROM `event` WHERE DATE(startDate)  BETWEEN 'std' AND 'etd' GROUP BY apiName"

#--------------datarnge data function--------------------

getdaterangeData <- function(query,sd, ed)
{
  conn <- dbConnect(RMySQL::MySQL(),
                    dbname="service_reminder",
                    user="root",
                    password="dt78#9Cv",
                    host= '142.93.216.190',
                    POrt=3306)
  stdd = print(sd)
  etdd = print(ed)
  query = gsub("std",stdd,query)
  query = gsub("etd",etdd,query)
  a <- data.frame(dbGetQuery(conn, paste0(query)))
  
  dbDisconnect(conn)
  return(a)
}

#-------------------create dataframe of date range result function of schduler----------

daterange_Jobcard <- reactive({getdaterangeData(query27,input$dateRange[1],input$dateRange[2])})
daterange_UpdateCalls <- reactive({getdaterangeData(query28,input$dateRange[1],input$dateRange[2])})
daterange_Retail <- reactive({getdaterangeData(query29,input$dateRange[1],input$dateRange[2])})
daterange_Booking <- reactive({getdaterangeData(query30,input$dateRange[1],input$dateRange[2])})
success_Failure_schduler_daterange <- reactive({getdaterangeData(query31,input$dateRange[1],input$dateRange[2])})
run_count_schduler_daterange <- reactive({getdaterangeData(query32,input$dateRange[1],input$dateRange[2])})
#---------------create data of API on datarange-------------
daterange_login <- reactive({getdaterangeData(query33,input$dateRangeA[1],input$dateRangeA[2])})
daterange_logout <- reactive({getdaterangeData(query34,input$dateRangeA[1],input$dateRangeA[2])})
daterange_error_status_API <- reactive({getdaterangeData(query35,input$dateRangeA[1],input$dateRangeA[2])})
daterange_run_count_API <- reactive({getdaterangeData(query36,input$dateRangeA[1],input$dateRangeA[2])})

#--------------schduler data frames of last 7 days--------------------
Time_taken_jobacard = getdata(query1)
Time_taken_Retail=getdata(query2)
Time_taken_UpdateCalls=getdata(query3)
Time_taken_Booking=getdata(query4)
success_Failure_schduler=getdata(query5)
no_of_schduler_called=getdata(query6)
#----------schduer data frames on current date-----------
Time_taken_jobacard_cd=getdata(query7)
Time_taken_Retail_cd=getdata(query8)
Time_taken_UpdateCalls_cd=getdata(query9)
Time_taken_Booking_cd=getdata(query10)
success_Failure_schduler_cd=getdata(query11)
no_of_schduler_called_cd=getdata(query12)
#--------failure error occured on current date-----------
Failure_schduler=getdata(query13)
count_Failure_schduler=getdata(query14)

#API data frames fo last 7 days
Time_taken_login=getdata(query15)
Time_taken_logout=getdata(query16)
success_Failure_API=getdata(query17)
no_of_API_called=getdata(query18)
#API data frames on current date
Time_taken_login_cd=getdata(query19)
Time_taken_logout_cd=getdata(query20)
success_Failure_API_cd=getdata(query21)
no_of_API_called_cd=getdata(query22)

#failure error occured on current date
Failure_API=getdata(query23)
count_Failure_API=getdata(query24)


total_failure_count <- data.frame(sum(rbind(count_Failure_schduler$error_count,count_Failure_API$error_count)))
setnames(total_failure_count,"sum.rbind.count_Failure_schduler.error_count..count_Failure_API.error_count..", "count")