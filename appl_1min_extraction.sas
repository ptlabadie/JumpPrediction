 *MACRO: TAKES OBSERVATIONS FOR BEGINNING OF EACH MINUTE;

%let wrds=wrds.wharton.upenn.edu 4016;
options comamid=TCP remote=WRDS;
signon username=_prompt_;

libname mylib 'C:\Users\Patrick\Documents\Current Semester\SAS';

rsubmit;

/*  Set libraries
	nbbo - National Best Bid/Offer
	cq - consolidated quotes
	ct - consolidated trades	*/
    libname nbbo '/wrds/nyse/sasdata/taqms/nbbo';
    libname cq '/wrds/nyse/sasdata/taqms/cq';
    libname ct '/wrds/nyse/sasdata/taqms/ct';

%macro appl_taq_1min_extraction(start,end);

/* Define Macro variables */
   %let start=%sysfunc(inputn(&start,anydtdte9.));
   %let end=%sysfunc(inputn(&end,anydtdte9.));
   %let dif=%sysfunc(intck(day,&start,&end));

/* Loops through each day from beginning to end */
	%do i=0 %to &dif;
	%let taqday=%sysfunc(intnx(day,&start,&i,b),yymmddn8.);

/* Checks if library exists (skips weekends/holidays)*/
%if %sysfunc(exist(nbbo.nbbom_&taqday)) %then
	%do;

/* Retrieve NBBO data */
    data DailyNBBO&taqday;

        /* Enter NBBO file names in YYYYMMDD format for the dates you want */
        set nbbo.nbbom_&taqday;

		/* Enter company tickers you want */
        where sym_root in ('AAPL')

		/* This selects common stocks only */
        and sym_suffix = '' 
		/* This selects every tick between trading hours*/
		and (("9:30:00.000000000"t) <= time_m <= ("16:01:00.000000000"t));
        format time_m part_time trf_time TIME20.9;
		keep DATE TIME_M SYM_ROOT BID BIDSIZ ASK ASKSIZ;
    run;
		

/* Round time to nearest minute */
		data nbbo&taqday;
			set DailyNBBO&taqday;
			timeround = intnx( 'minute', time_m, 0, 'b');
			format timeround Time8.;
		run;

/* Keep only first observation for each minute*/
		proc summary data=nbbo&taqday
			nway;
		class timeround;
		id DATE TIME_M SYM_ROOT BID BIDSIZ ASK ASKSIZ TIMEROUND;
		output out=appl&taqday
			(drop=_type_);
		run;

/* This adds current taq data to master dataset */
proc datasets nolist;
	append base=taq_data data=appl&taqday;
	run;

%end;
%end;
	
/* Download file and export*/
*proc download data=taq_data out=mylib.taq_data;
*run;
   %mend appl_taq_1min_extraction;

/* Execute Macro fn(start_date, end_date) */
%appl_taq_1min_extraction(20190102, 20190205)
endrsubmit;
