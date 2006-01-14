drop table SPJ

drop table S

drop table P

drop table J

create table S
    (SNO varchar(8) primary key,
     SNAME varchar(8),
     STATUS integer,
     CITY varchar(6))

create table P
    (PNO varchar(8) primary key,
     PNAME varchar(8),
     COLOR varchar(4),
     WEIGHT integer)

create table J
    (JNO varchar(8) primary key,
     JNAME varchar(8),
     CITY varchar(8))

create table SPJ
    (SNO varchar(8),
     PNO varchar(8),
     JNO varchar(8),
     QTY integer,
     primary key (SNO, PNO, JNO),
     foreign key (SNO) references S (SNO),
     foreign key (PNO) references P (PNO),
     foreign key (JNO) references J (JNO))
