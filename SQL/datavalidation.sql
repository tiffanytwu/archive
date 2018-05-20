—- Data Validation Frameworks

set hive.execution.engine=tez;
use db;

-- CHECKING DATES
select A.date1, A.start, B.Date2 
from (select to_Date(concat(substring(filedate,1,4),"-", substring(filedate,5,2),"-", substring(filedate,7,8))) AS date1,
	           date_add(to_Date(concat(substring(filedate,1,4),"-", substring(filedate,5,2),"-", substring(filedate,7,8))),1) AS start 
	  from table 
	  GROUP BY filedate) AS A
LEFT JOIN (select to_Date(concat(substring(filedate,1,4),"-", substring(filedate,5,2),"-", substring(filedate,7,8))) AS Date2 
	         from table 
	         GROUP BY filedate) AS B
ON A.start = B.Date2
WHERE B.Date2 is null;

—- Counts by week adjusting for start of week
select x.weekno, 
       count(distinct(y.user_id)) as uv, 
       count(distinct(concat(y.user_id, y.visit_num))) as visits 
from (select *, to_Date(concat(substring(filedate,1,4),"-", substring(filedate,5,2),"-", substring(filedate,7,8))) as date2
      from table) as y 
left join (select date_sub(to_Date(concat(substring(filedate,1,4),"-", substring(filedate,5,2),"-", substring(filedate,7,8))),1) as date1, 
                  weekofyear(to_Date(concat(substring(filedate,1,4),"-", substring(filedate,5,2),"-", substring(filedate,7,8)))) as weekno 
                  from table
                  where filedate > 20160103 
                  group by filedate) as x 
on y.date2 = x.date1
where y.filedate between 20160103 and 20161127
group by x.weekno;
