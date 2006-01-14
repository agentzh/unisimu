use strict;
use warnings;
use SPJ;
use List::MoreUtils qw(uniq any all);

my (@projects, @parts, @vendors);

@vendors = SPJ::Vendor->retrieve_all;
for my $vendor (@vendors) {
    print $vendor, ": ", $vendor->sname, "\n";
    my @parts = uniq $vendor->parts;
    for my $part (@parts) {
        print "    ", $part, ": ", $part->pname, "\n";
    }
}

print <<'.';

检索上海产的零件的工程名称 

SQL

    select distinct JNAME
    from S, SPJ, J
    where S.SNO = SPJ.SNO and SPJ.JNO = J.JNO
        and S.CITY = '上海'

    ------
    JNAME 
    ------
    三建  
    一汽  
    造船厂
    ------
.

{
my @vendors = SPJ::Vendor->search(city => '上海');
for my $vendor (@vendors) {
    push @projects, $vendor->projects;
}
output(map { $_->jname } @projects);
}

print <<'.';

检索供应工程 J1 零件 P1 的供应商号 SNO 

SQL

    select distinct SNO
    from SPJ
    where JNO = 'J1' and PNO = 'P1'

    ---
    SNO
    ---
    S1 
    S3 
    ---
.

{
my @rels = SPJ::Supply->search(jno => 'J1', pno => 'P1');
output(map { $_->sno } @rels);
}

print <<'.';
检索供应工程 J1 零件为红色的供应商号 SNO； 

SQL

    select distinct SNO
    from SPJ,P
    where SPJ.PNO = P.PNO and JNO = 'J1' and
        COLOR = '红'

    ---
    SNO
    ---
    S1 
    S3 
    ---
.

{
my @rels = SPJ::Supply->search(jno => 'J1');
my @vendors;
for my $rel (@rels) {
    if ($rel->pno->color eq '红') {
        push @vendors, $rel->sno;
    }
}
output(@vendors);
}

print <<'.';

检索没有使用天津生产的红色零件的工程号 JNO； 

SQL

    select JNO
    from J
    where JNO not in
        (select JNO
         from S,SPJ,P
         where S.SNO = SPJ.SNO and SPJ.PNO = P.PNO and
               S.CITY = '天津' and P.COLOR = '红')

    ---
    JNO
    ---
    J2 
    J5 
    J6 
    J7 
    ---
.

{
my @rels = SPJ::Supply->retrieve_all;
my @false_projs;
for my $rel (@rels) {
    if ($rel->sno->city eq '天津' and $rel->pno->color eq '红') {
        push @false_projs, $rel->jno;
    }
}
@projects = 
    grep {
        my $e = $_;
        all { $e ne $_} @false_projs 
    } SPJ::Project->retrieve_all;
output(@projects);
}

print <<'.';

检索至少用了供应商 S1 所供应的全部零件的工程号 JNO； 
设我们要寻找的工程为 x, 并且令

SQL

    select JNO
    from J
    where not exists
        (select PNO
         from SPJ
         where SNO = 'S1' and
            PNO not in
                (select PNO
                 from SPJ
                 where SNO = 'S1' and JNO = J.JNO))

    ---
    JNO
    ---
    ---
.

{
my @s1_parts = SPJ::Vendor->retrieve('S1')->parts;
my @projs = grep {
    my $proj = $_;
    all {
        SPJ::Supply->search(jno => $proj, pno => $_, sno => 'S1');
    } @s1_parts;
} SPJ::Project->retrieve_all;
output(@projs);
}

print <<'.';

检索购买了零件 P1 的工程项目号 JNO 及数量 QTY，并要求对查询的结果按数量 QTY 降序排列。 

SQL

    select JNO, sum(QTY) as Quantity
    from SPJ
    where PNO = 'P1'
    group by JNO
    order by Quantity desc

    ------------
    JNO Quantity
    ------------
    J4  700     
    J1  400     
    J3  100     
    ------------
.

{
use YAML qw(Dump);
my %res;
map {
    #$res{$_->jno} ||= $_->qty;
    $res{$_->jno}  += $_->qty;
} SPJ::Supply->search(pno => 'P1');
my @res;
while (my ($key, $value) = each %res) {
    push @res, [$key, $value];
}
@res = reverse sort { $a->[1] <=> $b->[1] } @res;
print "\n **** ", Dump(\@res), " ****\n\n";
}

sub output {
    print "\n **** ", join(' ', uniq @_), " ****\n\n";
}
