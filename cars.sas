FILENAME REFFILE '/home/yaoy890/carmpgdata_2.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=CARS;
	GETNAMES=YES;
RUN;

proc means data=CARS min q1 mean median q3 max;
var MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
run;

title "EDA for Non-imputed Data";
proc corr data=CARS;
var MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE ; 
run;

proc sgscatter data=CARS;
matrix MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE / diagonal=(histogram normal);
run;
title "Scatterplot";

proc mi data=CARS nimpute=1;
var MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
run;
 
title "EDA on Listwise Complete Data";
proc corr data=CARS nomiss;
var MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
run;
 
title "Multiple Imputation (MI) Data";
proc mi data=CARS nimpute=5 out=out seed=1;
var MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
run;

title "MI out dataset";
proc print data=out;
run;
 
title "Linear Regression on Multiple Imputation (MI) Data";
proc reg data=out outest=outreg covout;
model MPG = CYLINDERS SIZE HP WEIGHT ENG_TYPE;
by _Imputation_;
run;
 
title "Regression Output Data";
proc print data=outreg;
run;
 
title "Multiple Imputation (MI) Results Analysis";
proc mianalyze data=outreg;
modeleffects CYLINDERS SIZE HP WEIGHT ENG_TYPE Intercept;
run;
 
title "Predicting MPG on Non Imputed Data - Listwise Deletion";
proc reg data=CARS;
model mpg = CYLINDERS SIZE HP WEIGHT ENG_TYPE / CLB;
run;