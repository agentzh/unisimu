use qqbase
select user_id from users where user_name='ÖÙÎ°Ïé' or user_name='ÕÂÒà´º'
go

select count(*) as Total
from msgs
where msg_from=279005114 and msg_to=348000440
go

select count(*) Total2
from msgs, users as U1, users as U2
where msg_from=U1.user_id and msg_to=U2.user_id and
      U1.user_name='ÕÂÒà´º' and U2.user_name='ÖÙÎ°Ïé'
go

select count(*) as Total3
from msgs
where msg_from in
	(select user_id
	 from users
	 where user_name='ÕÂÒà´º')
      and msg_to in
	(select user_id
	 from users
	 where user_name='ÖÙÎ°Ïé')
