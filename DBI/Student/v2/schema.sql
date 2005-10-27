drop table SC;

drop table Student;

drop table Course;

create table Student
    (Sno   integer primary key,
     Sname char(8) not null,
     Ssex  char(2),
     Sage  integer,
     Sdept char(30));

create table Course
    (Cno integer primary key,
     Cname char(30) not null,
     Cpno integer,
     Ccredit integer);

create table SC
    (Sno   integer,
     Cno   integer,
     Grade integer,
     primary key (Sno, Cno),
     foreign key (Sno) references Student,
     foreign key (Cno) references Course);
