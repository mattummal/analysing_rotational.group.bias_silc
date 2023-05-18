proc sql;
create table analyse as select *
from SILC_20.BASE_OK_20 B
left join SILC_20.RFILE_CROS R on (B.RB030 = R.RB030)
left join SILC_20.DFILE_CROS D on (B.DB030 = D.DB030)
left join SILC_20.PFILE P on (B.RB030 = P.PB030)
left join SILC_20.HFILE H on (B.DB030 =  H.HB030);
quit;

%let path_user = \\sng3users\users_r\&sysuserid. ;

proc export data=analyse dbms=csv outfile="&path_user/merged2020.csv";

proc sort data=analyse;
BY DB075;
run;

proc means data=analyse;
var min60 smd LWI_BD_new EQ_INC20 WORK_INT AROPE_new MD5HH3 sev_msd  ;
weight rb050;
by DB075;
run;



