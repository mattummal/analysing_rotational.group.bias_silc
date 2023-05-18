%macro gini (yy, regio);

proc sql;
create table base_ok_&yy as select
    B.*,
    D.DB075
from SILC_&yy..BASE_OK_&yy B
left join SILC_&yy..DFILE_CROS D on (B.DB030 = D.DB030);
quit;



%if &regio = 1:3 %then %do; %let _RN_ = _BE; %let region_ = BELGIUM; %end;
%if &regio = 1 %then %do; %let _RN_ = _BR; %let region_ = BRUSSEL; %end;
%if &regio = 2 %then %do; %let _RN_ = _VL; %let region_ = FLEMISH region_; %end;
%if &regio = 3 %then %do; %let _RN_ = _WA; %let region_ = WALLOON region_; %end;

	** Gini;	
	PROC SORT data=base_ok_&yy. out = _idb(keep = rb050 eq_inc20 regio);
	by eq_inc20;
	where regio in (&regio.) and DB075 = 5;
	RUN;

	DATA _gini;
	set _idb end=last;
	retain swt swtvar swt2var swtvarcw ss 0;
	ss + 1;
	swt + RB050;
	swtvar + RB050 * eq_inc20;
	swt2var + RB050 * RB050 * eq_inc20;
	swtvarcw + swt * RB050 * eq_inc20;
	if last then do;
		valeur  = 100* (( 2 * swtvarcw - swt2var ) / ( swt * swtvar ) - 1);
		output;
	end;
	RUN;
	
	proc sql noprint;
	select count(rb030) into : nb
	from base_ok_&yy. where regio in (&regio.) and DB075 = 5;
	quit;

	DATA gini_&yy.&_RN_.;
	set _gini;
	indic = "Gini 20&yy. &region_. and DB075 = 5";
	N = &nb.;
	keep indic valeur N;
	run;
		
proc datasets library = work nolist;
delete _:;
quit;
run;

%mend gini;


%let yy = 19;

%gini(&yy, 1:3);




