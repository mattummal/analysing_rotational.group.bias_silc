proc sql;
create table analyse22 as select *
from SILC_22.BASE_OK_22 B
left join SILC_22.RFILE_CROS R on (B.RB030 = R.RB030)
left join SILC_22.DFILE_CROS D on (B.DB030 = D.DB030)
left join SILC_22.PFILE P on (B.RB030 = P.PB030)
left join SILC_22.HFILE H on (B.DB030 =  H.HB030);
quit;

%let path_user = \\sng3users\users_r\&sysuserid. ;

proc export data=analyse22 dbms=csv outfile="&path_user/merged2022.csv";

proc sort data=analyse22;
BY DB075;
run;

proc means data=analyse22;
var min60 smd LWI_BD_new EQ_INC20 WORK_INT AROPE_new MD5HH3 sev_msd  ;
weight rb050;

run;