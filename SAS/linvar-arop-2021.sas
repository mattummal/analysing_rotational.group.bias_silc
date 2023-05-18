proc sql;
create table analyse21 as select *
from SILC_21.BASE_OK_21 B
left join SILC_21.RFILE_CROS R on (B.RB030 = R.RB030)
left join SILC_21.DFILE_CROS D on (B.DB030 = D.DB030)
left join SILC_21.PFILE P on (B.RB030 = P.PB030)
left join SILC_21.HFILE H on (B.DB030 =  H.HB030);
quit;



* -----------------------------------------------------
MACRO 2 : LINEARIZATION OF THE AT-RISK-OF-POVERTY
RATE %MACRO LIN_ARPR (DATA = , LIBRARY = , INCOME = , WEIGHT = , ORDER = 50 , PERCENT = 60);
----------------------------------------------------- ;

* -------------------------------------------------------------------
> DATA: SAS dataset
> LIBRARY: SAS library containing the dataset
> INCOME: income variable
> WEIGHT: weighting variable
> ORDER: Order of the income quantile (by default, 50%)
> PERCENT: Percentage of the income quantile (by default, 60%)
--------------------------------------------------------------------- ;
* === I. CALCULATION OF THE AT-RISK-OF-POVERTY THRESHOLD === ;

data t;
set analyse21;
run;

%let data = analyse21;
%let income = EQ_INC20;
%let weight = RB050;
%let percent = 60;
%let order = 50;


proc univariate data=t noprint;
var &income;
weight &weight;
output out=_out_ pctlpts=&order pctlpre=quant pctlname=ile;
run;


data _out_;
set _out_;
threshold = (&percent/100)*quantile; /* At-risk-of-poverty threshold */
run;



data _null_;
set _out_;
call symput('quant_val',quantile);

/* Storage of the median income into the macro-variable &quant_val */

call symput('thres_val',threshold);

/* Storage of the poverty threshold into the macro-variable &thres_val */

run;


* === II. CALCULATION OF THE AT-RISK-OF-POVERTY RATE === ;
data t;
set t;
if &income<=&thres_val then poor=1; else poor=0;
run;

proc means data=t noprint;
var poor;
weight &weight;
output out=_out_ mean(poor) = poor; /* At-risk-of-poverty rate */
run;

data _null_;
set _out_;
call symput('rate_val',poor);
/* Storage of the at-risk-of-poverty rate into the macro-variable &rate_val */
run;



* === III. LINEARIZATION OF THE AT-RISK-OF-POVERTY RATE === ;
proc iml;
edit work.t;
param={&income &weight};
read all var param into mat;
inc=mat[,1];
wght=mat[,2];


/* Population size */
N=sum(wght);


/* Bandwith parameter - h=S/Nˆ(1/5) */
h=sqrt((sum(wght#inc#inc)-sum(wght#inc)*sum(wght#inc)/sum(wght))
/sum(wght))/exp(0.2*log(sum(wght)));

/* Estimate of F’(quantile) */
u1=(&quant_val-inc)/h;
vect_f1=exp(-(u1##2)/2)/sqrt(2*3.1415926536);
f_quant1=(vect_f1`*wght)/(N*h);


/* Estimate of F’(beta*quantile) */
u2=(&thres_val-inc)/h;
vect_f2=exp(-(u2##2)/2)/sqrt(2*3.1415926536);
f_quant2=(vect_f2`*wght)/(N*h);


/* Linearization of the at-risk-of-poverty threshold */
lin_thres=-(&percent/100)#(1/N)#((inc<=&quant_val)-&order/100)/f_quant1;


* ========== LINEARIZED VARIABLE OF THE AT-RISK-OF-POVERTY RATE ============== ;
lin=100*((1/N)#((inc<=&thres_val)-&rate_val)+f_quant2*lin_thres);
create lin_var from lin[colname={linvar}];
append from lin;
quit;

data &data;
merge &data lin_var;
run;

proc print data=&data;
var linvar; 
run;


%let path_user = \\sng3users\users_r\&sysuserid. ;

proc export data=&data dbms=csv outfile="&path_user/linvar_merged2021.csv";



