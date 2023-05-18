proc sql;
create table analyse21 as select *
from SILC_21.BASE_OK_21 B
left join SILC_21.RFILE_CROS R on (B.RB030 = R.RB030)
left join SILC_21.DFILE_CROS D on (B.DB030 = D.DB030)
left join SILC_21.PFILE P on (B.RB030 = P.PB030)
left join SILC_21.HFILE H on (B.DB030 =  H.HB030);
quit;



* ----------------------------------------------------
MACRO 2 : LINEARIZATION OF THE INCOME QUINTILE
SHARE RATIO S80/S20
----------------------------------------------------- ;

* ------------------------------------------------------------
> DATA: SAS dataset
> LIBRARY: SAS library containing the dataset
> INCOME: income variable
> WEIGHT: weighting variable
> ALPHA: Order of the income quantile (by default, 20%)
----------------------------------------------------------- ;

%let income=EQ_INC20;
%let weight=RB050;
%let data=analyse20;
%let alpha=20;
%let alpha2=%sysevalf(100-&alpha);

data s80;
set &data;
run;



proc univariate data=s80 noprint;
var &income;
weight &weight;
output out=_out_ pctlpts=&alpha &alpha2 pctlpre=quant pctlname=i1 i2;
run;


data _null_;
set _out_;
call symput ('quant_inf',quanti1); /* Bottom income quantile */
call symput ('quant_sup',quanti2); /* Top income quantile */
run;


data s80;
set s80;
indinf = &income * (&income <= &quant_inf);
indsup = &income * (&income > &quant_sup);
run;


proc means data=s80 noprint;
var indinf indsup;
weight &weight;
output out=_out_ sum(indinf)=den /* Total income for the bottom quintile */
sum(indsup)=sup;/* Total income for the top quintile */
run;


data _null_;
set _out_;
call symput('num_val',num);
call symput('den_val',den);
run;


proc iml;
edit work.s80;
param={&income &weight};
read all var param into mat;
inc=mat[,1];
wght=mat[,2];
v=wght#inc;


/* Population size */
N=sum(wght);

/* Bandwith parameter - h=S/Nˆ(1/5) */
h=sqrt((sum(wght#inc#inc)-
sum(wght#inc)*sum(wght#inc)/sum(wght))/sum(wght))/exp(0.2*log(sum(wght)));
/*===== 1. Linearization of the bottom quantile =====*/
u1=(&quant_inf-inc)/h;
vect_f1=exp(-(u1##2)/2)/sqrt(2*3.1415926536);
f_quant1=(vect_f1`*wght)/(N*h);
lin_inf=-(1/N)#((inc<=&quant_inf)-&alpha/100)/f_quant1;


/*===== 2. Linearization of the top quantile =====*/
u2=(&quant_sup-inc)/h;
vect_f2=exp(-(u2##2)/2)/sqrt(2*3.1415926536);
f_quant2=(vect_f2`*wght)/(N*h);
lin_sup=-(1/N)#((inc<=&quant_sup)-&alpha2/100)/f_quant2;


/*===== 3. Linearization of the total income for the top quintile =====*/
u3=(&quant_sup-inc)/h;
vect_f3=exp(-(u3##2)/2)/sqrt(2*3.1415926536);
f_quant3=(vect_f3`*v)/h;
lin_num=inc-inc#(inc<=&quant_sup)-f_quant3#lin_sup;


/*===== 4. Linearization of the total income for the bottom quintile =====*/
u4=(&quant_inf-inc)/h;
vect_f4=exp(-(u4##2)/2)/sqrt(2*3.1415926536);
f_quant4=(vect_f4`*v)/h;
lin_den=inc#(inc<=&quant_inf)+f_quant4#lin_inf;


/*======== LINEARIZED VARIABLE OF THE IQ SHARE RATIO ========*/

lin=((&den_val)#lin_num-(&num_val)#lin_den)/(&den_val*&den_val);
create lin_var from lin[colname={linvar}];
append from lin;
quit;


data &data;
merge &data lin_var;
run;





