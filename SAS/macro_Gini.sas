%macro gini (yy, regio);

%if &regio = 1:3 %then %do; %let _RN_ = _BE; %let region_ = BELGIUM; %end;
%if &regio = 1 %then %do; %let _RN_ = _BR; %let region_ = BRUSSEL; %end;
%if &regio = 2 %then %do; %let _RN_ = _VL; %let region_ = FLEMISH region_; %end;
%if &regio = 3 %then %do; %let _RN_ = _WA; %let region_ = WALLOON region_; %end;

	** Gini;	
	PROC SORT data=silc_&yy..base_ok_&yy. out = _idb (keep = rb050 eq_inc20 regio);
	by eq_inc20;
	where regio in (&regio.);
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
	from silc_&yy..base_ok_&yy. where regio in (&regio.);
	quit;

	DATA gini_&yy.&_RN_.;
	set _gini;
	indic = "Gini 20&yy. &region_.";
	N = &nb.;
	keep indic valeur N;
	run;
		
proc datasets library = work nolist;
delete _:;
quit;
run;

%mend gini;

%gini (22, 1:3);
%gini (21, 1:3);
%gini (20, 1:3);
%gini (19, 1:3);
%gini (18, 1:3);
%gini (17, 1:3);
%gini (16, 1:3);




