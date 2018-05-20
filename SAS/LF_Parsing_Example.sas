/*SAS CODE where you want to make changes*/
/*user input */
%let code_name = code_with_old_varnames.sas;
%put &code_name;

/*Input the lookup table for standardized names*/
/*user input */
%let lookup_table = Lookup_dt;
%put &lookup_table;

/*Input the path where sas code is stored that needs to be changed with standardized names*/
/*user input */
%let code_path = /sasdata/files/analytics/analytics_msi/functional/data/DF/... ;
%put &code_path;

/*Input the name of the new sas code*/
/*user input */
%let new_code = Check_parse_usa_result.sas;
%put &new_code;


/*--------------------------------------------------------------------------*/



proc import file="&code_path./&code_name."

    dbms=csv
    out=work.sas_code
    Replace;
	GETNAMES=NO;
    delimiter='?';
	guessingrows=2000;
run;


PROC SQL NOPRINT; 
SELECT COUNT(*) INTO : cnt_rows FROM work.sas_code;
QUIT; 

%PUT &cnt_rows.; 

data Lookup (keep= MOD_VAR STD_VAR);
set &lookup_table.;
MOD_VAR = UPCASE(MOD_VAR);
STD_VAR = UPCASE(STD_VAR);
run;

PROC SQL NOPRINT; 
SELECT COUNT(*) INTO : cnt FROM work.Lookup;
QUIT;
%PUT &cnt.; 

PROC SQL NOPRINT; 
SELECT MOD_VAR,STD_VAR INTO :old , :std  FROM Lookup; 
QUIT;


%macro new;

%do i=1 %to &cnt.;

data Lookup2;
set Lookup ;
if _n_= &i.;
run;

proc sql;
select STD_VAR,MOD_VAR into :std,:old from Lookup2
quit;

data sas_code;
set sas_code;
VAR1=tranwrd(VAR1,"&old.","&std.");
run;
%end;
%mend;
%new; 

data _null_;
file "&code_path./&new_code.";
set work.sas_code;
put var1;
run;


