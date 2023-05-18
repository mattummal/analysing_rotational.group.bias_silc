%Macro S80S20 (yy, regio, dsin = 1 , where = 1 );

%if &dsin. = 1 %then %let dsin_loc = silc_&yy..base_ok_&yy.;
%else %let dsin_loc = &dsin.; 

%if &regio = 1:3 %then %do; %let _RN = _BE; %let region = BELGIUM; %end;
%if &regio = 1 %then %do; %let _RN = _BR; %let region = BRUSSEL; %end;
%if &regio = 2 %then %do; %let _RN = _VL; %let region = FLEMISH REGION; %end;
%if &regio = 3 %then %do; %let _RN = _WA; %let region = WALLOON REGION; %end;

PROC UNIVARIATE DATA = &dsin_loc. NOPRINT; /* berekening medianen en Quintielen - */
	var EQ_INC20;
	weight rb050;
	OUTPUT OUT =_Q pctlpts=20 40 60 80 pctlpre= EQ20_P_;
	where regio in (&regio.) and &where.;
	RUN;

data _idb (keep = rb050 regio EQ_INC20 QITILE);
set &dsin_loc. ;
where regio in (&regio.) and &where. ;

if _n_=1 then set _Q;

iF (EQ_INC20 < EQ20_P_20) then QITILE = 1;
IF (EQ_INC20 >= EQ20_P_20 and EQ_INC20 < EQ20_P_40) then QITILE = 2;
IF (EQ_INC20 >= EQ20_P_40 and EQ_INC20 < EQ20_P_60) then QITILE = 3;
IF (EQ_INC20 >= EQ20_P_60 and EQ_INC20 < EQ20_P_80) then QITILE = 4;
IF (EQ_INC20 >= EQ20_P_80) then QITILE = 5;

run;

PROC SQL noprint;

	select count(rb030) into : nb
	from &dsin_loc. where regio in (&regio.) and &where.;

	Create table work._tmp0 as
	select distinct sum(RB050) as totwgh
	from  work._idb;

	Create table work._tmp1 as
	select distinct sum(RB050 * EQ_INC20) as s20
	from  work._idb
	where QITILE=1;

	Create table work._tmp2 as
	select distinct sum(RB050 * EQ_INC20) as s80
	from  work._idb
	where QITILE=5;

	create table s80_s20_&yy.&_rn. as
	SELECT "S80/S20 20&yy. &region. where &where." as indic, 
	&nb as N,
	(S80 / S20) as valeur
	FROM work._tmp0, work._tmp1, work._tmp2;

QUIT;

proc datasets library = work nolist;
delete _:;
quit;
run;

%mend S80S20;

%s80s20(22, 1:3);

%s80s20(21, 1:3);

%s80s20(20, 1:3);

%s80s20(19, 1:3);

%s80s20(18, 1:3);

%s80s20(17, 1:3);

%s80s20(16, 1:3);
