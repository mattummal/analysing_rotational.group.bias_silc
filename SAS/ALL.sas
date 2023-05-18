proc sql;
create table si19 as select *
from SILC_19.BASE_OK_19 B
left join SILC_19.RFILE_CROS R on (B.RB030 = R.RB030)
left join SILC_19.DFILE_CROS D on (B.DB030 = D.DB030)
left join SILC_19.PFILE P on (B.RB030 = P.PB030)
left join SILC_19.HFILE H on (B.DB030 =  H.HB030);
quit;


proc sql;
create table si20 as select *
from SILC_20.BASE_OK_20 B
left join SILC_20.RFILE_CROS R on (B.RB030 = R.RB030)
left join SILC_20.DFILE_CROS D on (B.DB030 = D.DB030)
left join SILC_20.PFILE P on (B.RB030 = P.PB030)
left join SILC_20.HFILE H on (B.DB030 =  H.HB030);
quit;


proc sql;
create table si21 as select *
from SILC_21.BASE_OK_21 B
left join SILC_21.RFILE_CROS R on (B.RB030 = R.RB030)
left join SILC_21.DFILE_CROS D on (B.DB030 = D.DB030)
left join SILC_21.PFILE P on (B.RB030 = P.PB030)
left join SILC_21.HFILE H on (B.DB030 =  H.HB030);
quit;


proc sql;
create table si22 as select *
from SILC_22.BASE_OK_22 B
left join SILC_22.RFILE_CROS R on (B.RB030 = R.RB030)
left join SILC_22.DFILE_CROS D on (B.DB030 = D.DB030)
left join SILC_22.PFILE P on (B.RB030 = P.PB030)
left join SILC_22.HFILE H on (B.DB030 =  H.HB030);
quit;

data SILC;
set si19 si20 si21 si22;
run;


%let path_user = \\sng3users\users_r\&sysuserid. ;
proc export data=SILC dbms=csv outfile="&path_user/silc19-22.csv";

ods tagsets.simplelatex file="&path_user/all_out.tex" stylesheet="&path_user/sas.sty"(url="sas");

proc tabulate
data=SILC;
format HT HT. RB090 Sex. DB040 $region. AGE cat_age.
        ACTSTA ACTSTA. TENSTA TENSTA.;
var MIN60 LWI_BD_new EQ_INC20 AROPE_new SMD MD5HH3 Sev_MSD ;
class AGE ACTSTA TENSTA RB090 HT DB040 DB075 RB010 / missing;
table ALL AGE ACTSTA TENSTA RB090 HT DB040="Region" DB075,
    RB010*N (MIN60 LWI_BD_new AROPE_new SMD Sev_MSD MD5HH3)*RB010*mean*f=PERCENT8.2 

     EQ_INC20*RB010*mean*f=8.2
/printmiss;
weight RB050;
run;




ods tagsets.latex close;
ods tagsets.colorlatex close;
ods tagsets.tablesonlylatex close;
ods tagsets.simplelatex close;




PROC FORMAT;


value ht (notsorted multilabel)
/*1-8 ='Total no dependent children'
1-4 = 'One person household, total'*/
5 ='5: One person household, total'
6 ='6: 2 adults (no children), both <65 years'
7 ='7: 2 adults (no children), at least one 65+ years'
8 ='8: Other households no dependent children'
/*9-13 = 'Total dependent children' */
9 ='9: Single parent household, one or more dependent children'
10= '10: 2 adults, one dependent child'
11 ='11: 2 adults, two dependent children'
12 ='12: 2 adults, three + dependent children'
13 ='13: Other households with dependent children'
16 = '16: Other'
1 ='1: One person household,  male under 65 years'
2 ='2: One person household, male 65 years and over'
3 ='3: One person household, female under 65 years'
4 ='4: One person household, female 65 years and over';



value ht (notsorted multilabel)
5 ='One person'
6 ='2 adults < 65'
7 ='2 adults 1 > 65'
8 ='Other no children'
9 ='Single parent'
10= '2 adults, 1 child'
11 ='2 adults, 2 children'
12 ='2 adults, 3+ children'
13 ='Other with children'
16 = 'Other'
1 ='1: One person household,  male under 65 years'
2 ='2: One person household, male 65 years and over'
3 ='3: One person household, female under 65 years'
4 ='4: One person household, female 65 years and over';
run;



PROC FORMAT;
VALUE CAT_AGE
0-17='0-17'
18-24='18-24'
25-49='25-49'
50-64='50-64'
65-150='65+';



VALUE SEX
1='Men'
2='Woman'
;



value $region
"BE10"='Brussels'
"BE21"='Flanders'
"BE22"='Flanders'
"BE23"='Flanders'
"BE24"='Flanders'
"BE25"='Flanders'
"BE26"='Flanders'
"BE31"='Wallonia'
"BE32"='Wallonia'
"BE33"='Wallonia'
"BE34"='Wallonia'
"BE35"='Wallonia'
"BE36"='Wallonia';



value actsta (notsorted multilabel)
1-1.9 = 'At Work'
1.1 = '- Self-employed'
1.2 = '- Employee'
2 = 'Unemployed'
3 = 'Retired'
4-4.9 = 'Other inactive'
4.1 = '- Disable/sick'
4.2 = '- Other inactive'
other = "Unconcerned";



value TENSTA
1 = "Owners"
2 = "Renters";

run;



proc fcmp outlib=work.func.sample;
  function xround1000(value);
    return( round(value, 1000) );
  endsub;
run;



*
* make function available
*;
options cmplib=work.func;



proc format;
  value xround1000x
    low - high = [xround1000()]
  ;
run;