use qqbase

select msg_body, session_id
from msgs
where msg_body like '%idu%' or
	msg_body like '%×´Ì¬»ú%' or
	msg_body like '%·´»ã±àÆ÷%'
order by session_id
go

select msg_body, session_id
from msgs as M1
where M1.session_id in
	(select session_id
	 from msgs as M2
	 where M2.msg_body like '%idu%' or
		M2.msg_body like '%×´Ì¬»ú%' or
		M2.msg_body like '%·´»ã±àÆ÷%')
order by session_id, offset asc
go
