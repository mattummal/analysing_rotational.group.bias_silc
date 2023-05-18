proc sql;
create table analyse20 as select *
from SILC_20.BASE_OK_20 B
left join SILC_20.RFILE_CROS R on (B.RB030 = R.RB030)
left join SILC_20.DFILE_CROS D on (B.DB030 = D.DB030)
left join SILC_20.PFILE P on (B.RB030 = P.PB030)
left join SILC_20.HFILE H on (B.DB030 =  H.HB030);
quit;


* ----------------------------------------------------
MACRO : LINEARIZATION OF THE GINI COEFFICIENT
------------------------------------------------------;

* ---------------------------------------------------
> DATA: SAS dataset
> LIBRARY: SAS library containing the dataset
> INCOME: income variable
> WEIGHT: weighting variable
----------------------------------------------------- ;

%let income=EQ_INC20;
%let weight=RB050;
%let data=analyse20;

proc sort data=&data;
by &income;
run;

proc iml;
edit &data;
param={&income &weight};
read all var param into mat;
taille=nrow(mat); /* Sample size */
N=mat[+,2]; /* Population size */
T=sum(mat[,1]#mat[,2]); /* Total income */
r=j(taille,1,1);
r[1,1]=mat[1,2];
do i=2 to taille;
r[i,1]=r[i-1,1]+mat[i,2]; /* r[i,1] is the cumulative weight of the person i */
end;
Num=sum((2*r[,1]-1)#(mat[,1]#mat[,2]));
Den=N*T;


/*** Gini coefficient ***/

Gini=Num/Den-1;
F=j(taille,1,1);
F[1,1]=mat[1,2]/N;
do i=2 to taille;
F[i,1]=F[i-1,1]+mat[i,2]/N; /* Cumulative income distribution function */
end;
G=j(taille,1,1);
G[1,1]=mat[1,1]*mat[1,2];
do i=2 to taille;
G[i,1]=G[i-1,1]+mat[i,1]*mat[i,2]; /* Weighted partial sum */
end;


/*========== LINEARIZED VARIABLE OF THE GINI COEFFICIENT ==========*/

lin=100*(2*(T-G[,1]+mat[,1]#mat[,2]+N*(mat[,1]#F[,1]))-mat[,1]-(Gini+1)*(T+N*mat[,1]))/(N*T);
create lin_var from lin[colname={linvar}];
append from lin;
quit;


data &data;
merge &data lin_var;
run;


%let path_user = \\sng3users\users_r\&sysuserid. ;

proc export data=&data dbms=csv outfile="&path_user/linvar_gini_20.csv";
