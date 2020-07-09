%let wrds=wrds.wharton.upenn.edu 4016;
options comamid=TCP remote=WRDS;
signon username=_prompt_;

libname mylib 'C:\Users\Patrick\Documents\Current Semester\SAS';

rsubmit;

*****NOTE: Creates duplicates if ran twice. In current form, it drops duplicates of daily files 
*before appending them to master file. Either delete the entire working library(commands at end)
*or append unedited libraries then delete duplicates
*or delete duplicates before and after ;



/*  Set libraries
	nbbo - National Best Bid/Offer
	cq - consolidated quotes
	ct - consolidated trades	*/
    libname nbbo '/wrds/nyse/sasdata/taqms/nbbo';
    libname cq '/wrds/nyse/sasdata/taqms/cq';
    libname ct '/wrds/nyse/sasdata/taqms/ct';

%macro taq_1min_extraction(start,end);

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
        where sym_root in ('FB','AAPL','GOOG','MSFT')

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
		class timeround DATE SYM_ROOT;
		id DATE TIME_M SYM_ROOT BID BIDSIZ ASK ASKSIZ TIMEROUND;
		output out=nbbo_taq&taqday
			(drop=_type_);
		run;

/* This adds current taq data to master dataset */
proc datasets nolist;
	append base=taq_data data=nbbo_taq&taqday;
	run;

%end;
%end;
	/* Keep only first observation for each minute*/
		proc summary data=taq_data
			nway;
		class timeround DATE SYM_ROOT;
		id DATE TIME_M SYM_ROOT BID BIDSIZ ASK ASKSIZ TIMEROUND;
		output out=taq_data_final
			(drop=_type_);
		run;
/* Download file and export*/
*proc download data=nbbo_taq&taqday out=mylib.taq_fb;
*run;
   %mend taq_1min_extraction;

   /* Execute Macro fn(start_date, end_date) */
%taq_1min_extraction(20190102, 20190205)
/* Download file and export*/
proc sort data=taq_data_final;
	by SYM_ROOT DATE timeround;
	run;

proc download data=taq_data_final out=mylib.taq_data;
run;
endrsubmit;

*Delete everything;
/*%let wrds=wrds.wharton.upenn.edu 4016;
options comamid=TCP remote=WRDS;
signon username=_prompt_;

libname mylib 'C:\Users\Patrick\Documents\Current Semester\SAS';

rsubmit;

	libname nbbo '/wrds/nyse/sasdata/taqms/nbbo';
    libname cq '/wrds/nyse/sasdata/taqms/cq';
    libname ct '/wrds/nyse/sasdata/taqms/ct';

proc datasets library=work kill nolist;
quit;
	endrsubmit;

	*/
