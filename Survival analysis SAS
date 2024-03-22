*PROJECT; 
data nba; set work.import; *create dataset nba that is identical to work.import;
run;
*total 4550 observations;

*delete missing values; 
data nba; 
	set nba; 
	if position = '' then delete;
	if year_end = '' then delete; 
	if year_start = '' then delete; 
run; 
*total observations 4549; 

/*
proc freq data = nba; 
	tables position; 
run; 
*/

*create career length variable called time, create position variables, and create retired variable; 
data nba;
	set nba;  
	time = year_end + 1 - year_start; 
	sqrttime = sqrt(time);
	cat_3 = position;
	if position = "F-C" then cat_3 = "F"; 
	if position = "C-F" then cat_3 = "C";
	if position = "G-F" then cat_3 = "G";
	if position = "F-G" then cat_3 = "F";
	cat_5 = position; 
	if position = "F-G" then cat_5 = "G-F";
	if position = "C-F" then cat_5 = "F-C";
	cat_combo = 1; 
	if position = "G" then cat_combo = 0; 
	if position = "F" then cat_combo = 0; 
	if position = "C" then cat_combo = 0; 
	retired = 1;
	if year_end = 2018 then retired = 0; 
run; 

*dummy variables for cat_3
reference group "G" = 0, 0; 
data nba;
	set nba; 
	cat_3_1 = 0; 
	if cat_3 = "F" then cat_3_1 = 1; *1,0;
	cat_3_2 = 0; 
	if cat_3 = "C" then cat_3_2 = 1; *0,1;
run; 

*dummy variable coding for cat_5
reference group "G" = 0, 0, 0, 0; 
data nba;
	set nba; 
	cat_5_1 = 0; 
	if cat_5 = "F" then cat_5_1 = 1; *(1,0,0,0);
	cat_5_2 = 0; 
	if cat_5 = "C" then cat_5_2 = 1; *(0,1,0,0);
	cat_5_3 = 0; 
	if cat_5 = "G-F" then cat_5_3 = 1; *(0,0,1,0);
	cat_5_4 = 0; 
	if cat_5 = "F-C" then cat_5_4 = 1; *(0,0,0,1);
run; 


*height is funny listed as ft-in change to inches;
data nba; 
	set nba; 
	num_height = scan(height,1,"-")*12 + scan(height,2,"-")*1;
	sqrt_height = sqrt(num_height); 
run; 

data nba;
	set nba;
	only_date = datepart(birth_date);
	format only_date date9.; 
	birth_year = year(only_date);
	age_start = year_start - birth_year;
	age_end = year_end - birth_year;
run; 

/* 
proc print data = nba; 
run; 
*/

*subset of nba data set only observations starting in 1950; 
data nba_50;
	set nba; 
	where year_start between 1950 and 2018;
run; 

data nba_50; 
	set nba_50; 
	id = _N_; 
run; 
 
*observations 4254;

data nba_50;
	set nba_50;  
	*if year_start between 1950 and 1959 then decade = 50; 
	if (1950 <= year_start <= 1959) then decade = "1950's";
	if (1960 <= year_start <= 1969) then decade = "1960's"; 
	if (1970 <= year_start <= 1979) then decade = "1970's"; 
	if (1980 <= year_start <= 1989) then decade = "1980's"; 
	if (1990 <= year_start <= 1999) then decade = "1990's"; 
	if (2000 <= year_start <= 2009) then decade = "2000's"; 
	if (2010 <= year_start <= 2019) then decade = "2010's"; 
	count = ; 
run; 

proc freq data = nba_50; 
	tables decade*position ;
	output = out1; 
run; 

*would like to graph that above...; 
/*proc sgplot data = nba_50; 
	series x = year_start y= count / group = position ; 
run; 
*/ 

proc sgplot data=nba_50;
   histogram num_height;
   density num_height / type=normal;
run;
proc sgplot data=nba_50;
   histogram sqrt_height;
   density sqrt_height / type=normal;
run;

*other descriptive statistics;
proc sgplot data=nba_50 noautolegend;
   title "Career Length";
   histogram time;
   xaxis label = "Years"; yaxis label = "Percent";
run;

proc freq data = nba_50; 
	tables position; 
run; 

*count and percent of data set by position;
*overall descriptive statistics;
proc means data = nba_50;
	var age_start;
	var age_end;
	var num_height; 
	var weight; 
run; 
*stratified by all 7 listed positions;
proc means data = nba_50; 
	class position; 
	var num_height; 
	var age_start;
	var weight;
	var age_end; 
run; 

proc means data = nba_50; 
	class cat_3; 
	var num_height; 
	var age_start;
	var weight;
	var age_end; 
run; 

proc means data = nba_50; 
	class cat_5; 
	var num_height; 
	var age_start;
	var weight;
	var age_end; 
run; 

proc means data = nba_50; 
	class cat_combo; 
	var num_height; 
	var age_start;
	var weight;
	var age_end; 
run; 

/*
data nba_50; 
	set nba_50; 
	time2 = time; 
	if year_start = 2018 then time2 = ''; 
run; 

proc means data = nba_50; 
	class position; 
	var time; *not dropping censored obs; 
run; 

proc means data = nba_50; 
	class position; 
	var time2; *dropped censoring obs;
run; 
*/

/****************************   
*****************************
******survival analysis ***** 
*****************************/ 
ods graphics on;
proc lifetest data=nba_50 plots=(survival (CL  TEST)); *test means log-rank test; 
   time time*retired (0); *0 means censored observation, 1 is event;  
run;

ods graphics on;
proc lifetest data=nba_50 plots=(survival (CL  TEST)); *test means log-rank test; 
   time time*retired (0); *0 means censored observation, 1 is event; 
   strata position / order=internal; /*The order = internal option in the strata statement enables one
                                    to order the strata by their internal values*/ 
run;

ods graphics on;
proc lifetest data=nba_50 plots=(survival (CL  TEST)) outsurv= KMcat_5; *test means log-rank test; 
   time time*retired (0); *0 means censored observation, 1 is event; 
   strata cat_5 / order=internal; /*The order = internal option in the strata statement enables one
                                    to order the strata by their internal values*/ 
run;

/*was going to try.... haha nope; 
title "5 Position Kaplan-Meier Survival Curves"; *aneuploidy DNA, group = 2 had diploid;
proc sgplot data=KMcat_5;
*styleattrs datacolors=(black red);
*band x=time upper=SDF_UCL lower=SDF_LCL /group=File_origin  transparency=.2;
step x=time y=SURVIVAL/LINEATTRS=(color = green PATTERN=shortdash) legendlabel="Linear Survival";
step x=time y=SDF_UCL / LINEATTRS=(color = red PATTERN=DASH);
step x=time y=SDF_LCL / LINEATTRS=(color = red PATTERN=DASH); 
where time lt 24;
run;
*/ 

ods graphics off ; 

ods graphics on;
proc lifetest data=nba_50 plots=(survival (CL  TEST)); *test means log-rank test; 
   time time*retired (0); *0 means censored observation, 1 is event; 
   strata cat_combo / order=internal; /*The order = internal option in the strata statement enables one
                                    to order the strata by their internal values*/ 
run;

ods graphics on;
proc lifetest data=nba_50 plots=(survival (CL  TEST)); *test means log-rank test; 
   time time*retired (0); *0 means censored observation, 1 is event; 
   strata cat_3 / order=internal; /*The order = internal option in the strata statement enables one
                                    to order the strata by their internal values*/ 
run;
ods graphics off ; 



/***********************************
now we can adjust for covariates using cox-proportional hazard model
cox-proporitonal hazard
***********************************/

*model selection; 
*I may have some collinearity violations between num_height and cat_5
need to create dummy variable coding for cat_5... ;
*collinearity between height and weight r = .82178;
proc corr data=nba_50;
var cat_5_1 cat_5_2 cat_5_3 cat_5_4 time sqrttime age_start year_start num_height weight;
title 'Examination of Correlation Matrix';
run;
proc corr data=nba_50;
var cat_3_1 cat_3_2 time sqrttime age_start year_start num_height weight;
title 'Examination of Correlation Matrix';
run;
proc corr data=nba_50;
var cat_combo time sqrttime age_start year_start num_height weight;
title 'Examination of Correlation Matrix';
run;

proc phreg data=nba_50;
   *class cat_5_1 (ref=0) cat_5_2 (ref=0) cat_5_3 (ref=0) cat_5_4 (ref=0);
   model time*retired (0) = cat_5_1 cat_5_2 cat_5_3 cat_5_4 age_start year_start num_height
                         / include=4 selection=stepwise slentry=0.15
                           slstay=0.1 details;
                           
run;

proc phreg data=nba_50;
   class cat_5 (ref="C");
   model time*retired (0) = cat_5 age_start year_start num_height
                         / include=1 selection=stepwise slentry=0.15
                           slstay=0.1 details;
                           
run; 
*same as if using the dummy variables all variable remain in the model;

ods graphics on; 
ods output ParameterEstimates = overall; 
proc phreg data = nba_50 plots(overlay)=survival; 
	class cat_5 (ref="C"); 
		model time*retired (0)= cat_5 year_start age_start num_height /rl; 
		baseline covariates = nba_50 out = overall survival= _all_/diradj group = cat_5;
run; 
ods graphics off; 

ods graphics on; 
ods output ParameterEstimates = overall; 
proc phreg data = nba_50 plots(overlay)=survival; 
	class cat_5 (ref="C"); 
		model time*retired (0)= cat_5 year_start age_start num_height/rl; 
		baseline out = overall survival= _all_/diradj group = cat_5;
	assess ph/resample =20;
	output out=outp 
	survival = survival xbeta = xb 
	DFBETA=dfbetcat_5  dfbetyear_start dfbetage_start dfbetnum_height
	logsurv=snell resmart = mart resdev = dev; *this is saving the output includes survival, cox-snell and martingale residuals;
run; 
ods graphics off; 

*manipulate cox-snell to get then plot it; 
/* this is for cat_5 variable */
data outp; 
set outp; 
genres=-snell;
run;

proc lifetest data=outp outsurv=survres;
time genres*year_start(0);
run;

data survres;
set survres;
lls=-log(-log(survival));
loggenr=-log(genres);
run;

proc sgplot data=survres;
reg y=lls x=loggenr / lineattrs= (color = black) markerattrs= (color = black); 
title height=18pt "5 Position"; 
xaxis label = "Log(Cox-Snell Residual)" valueattrs=(size=15pt) labelattrs=(size=15pt); yaxis label = "Log Negative Log Survival" valueattrs=(size=15pt) labelattrs=(size=15pt); 
run;*this plot should go in the paper;

/* old
proc sgplot data=survres;
reg y=lls x=loggenr / lineattrs= (color = black) markerattrs= (color = black);
title "Cox-Snell Residual Plot"; 
title2 "5 position Cox-Proportional Hazard";
xaxis label = "Log(Cox-Snell Residual)" ; yaxis label = "Log Negative Log Survival"; 
run; *this plot should go in the paper; */ 

*martingale residuals need to do for every x variable;
*we want it to be linear, but we might need to transform it;
proc sgplot data =outp; 
loess y = mart x = xb; /* predicted values */ 
run; 
proc sgplot data = outp;
loess y=mart x=cat_5; 
run;  
proc sgplot data = outp; 
loess y=mart x=year_start;
run;

*deviance residuals; 
proc sgplot data=outp;
scatter y=dev x=id;
 refline -2 2 /axis=y;
run;

proc sgplot data=outp;
histogram dev;
run;

/**************************DFBETA residuals (score residuals)8****************/
/**************************Testing for influencial outliers*******************/
/************************* influence cut off = 2/sqrt(n)**********************/ 
/****************************2/sqrt(4254) = .0306641727***********************/
*dfbetcat_5  dfbetyear_start dfbetage_start dfbetnum_height;

proc phreg data = nba_50 noprint; 
	class cat_5 (ref="C"); 
		model time*retired (0)= cat_5 year_start age_start num_height; 
	output out=outp 
	DFBETA=dfbetcat_5 dfbetcat_51 dfbetcat_52 dfbetcat_53 dfbetyear_start dfbetage_start dfbetnum_height;
run; 
 
proc sgplot data = outp; 
	scatter y=dfbetcat_5 x=cat_5;
	refline -0.030664 0.030664 / axis = y; 
run; 

proc sgplot data = outp; 
	scatter y=dfbetcat_51 x=cat_5;
	refline -0.030664 0.030664 / axis = y; 
run; 

proc sgplot data = outp; 
	scatter y=dfbetcat_52 x=cat_5;
	refline -0.030664 0.030664 / axis = y; 
run; 

proc sgplot data = outp; 
	scatter y=dfbetcat_53 x=cat_5;
	refline -0.030664 0.030664 / axis = y; 
run; 

proc sgplot data = outp; 
	scatter y=dfbetyear_start x=year_start;
	refline -0.030664 0.030664 / axis = y; 
run; 

proc sgplot data = outp; 
	scatter y=dfbetage_start x = age_start; 
	refline -0.030664 0.030664 / axis = y; 
run; 

proc sgplot data = outp; 
	scatter y=dfbetnum_height x = num_height; 
	refline -0.030664 0.030664 / axis = y; 
run; 

proc univariate data = outp; 
var dfbetcat_5; 
id id; 
run; 
proc univariate data=outp;
var dfbetyear_start;
id id;
run;
proc univariate data=outp;
var dfbetage_start;
id id;
run;
proc univariate data=outp;
var dfbetnum_height;
id id;
run;

/******check to see if proportional hazard assumption is valid for every x variable******/ 
*standard score or weighted schoenfeld residual;
proc phreg data = nba_50 noprint; 
	class cat_5 (ref="C"); 
		model time*retired (0)= cat_5 year_start age_start num_height; 
	output out=outp 
	WTRESSCH =resschcat_5 resschcat_51 resschcat_52 resschcat_53 resschyear_start resschage_start resschnum_height  ;
run; 

proc sgplot data=outp;
scatter y=resschcat_5 x=time;
run;

proc sgplot data=outp;
scatter y=resschcat_51 x=time;
run;

proc sgplot data=outp;
scatter y=resschcat_52 x=time;
run;

proc sgplot data=outp;
scatter y=resschcat_53 x=time;
run;
   
proc sgplot data=outp;
scatter y=resschyear_start x=time;
run;
   
proc sgplot data=outp;
scatter y=resschage_start x=time;
run;
   
proc sgplot data=outp;
scatter y=resschnum_height x=time;
run;



/***************************** 
cat_3 model
*do i need to put the groups as an indicator variable? NO*
******************************/
proc phreg data=nba_50;
   class cat_3 (ref="C");
   model time*retired (0) = cat_3 age_start year_start num_height
                         / include=1 selection=stepwise slentry=0.15
                           slstay=0.1 details;
                           
run;
*I have some collinearity violations between num_height and weight;
*all variables entered the model; 

ods graphics on; 
ods output ParameterEstimates = overall; 
proc phreg data = nba_50 plots(overlay)=survival; 
	class cat_3 (ref="C"); 
		model time*retired (0)= cat_3 year_start age_start num_height /rl; 
		baseline covariates = nba_50 out = overall survival= _all_/diradj group = cat_3;
run; 
ods graphics off; 


ods graphics on; 
ods output ParameterEstimates = overall; 
proc phreg data = nba_50 plots(overlay)=survival; 
	class cat_3 (ref="C"); 
		model time*retired (0)= cat_3 year_start age_start num_height /rl; 
		baseline out = overall survival= _all_/diradj group = cat_3;
	assess ph/resample =20;
	output out=outp2 
	survival = survival xbeta = xb 
	DFBETA=dfbetcat_3  dfbetyear_start dfbetaage_start dfbetanum_height
	WTRESSCH =resschcat_3 resschcat_31  resschyear_start resschage_start resschnum_height  
	logsurv=snell resmart = mart resdev = dev; *this is saving the output includes survival, cox-snell and martingale residuals;
run; 
ods graphics off; 

*need to cox-snell (see homework 7);
*manipulate cox-snell to get then plot it; 

/* this is for cat_3 variable */
data outp2; 
set outp2; 
genres=-snell;
run;
proc lifetest data=outp2 outsurv=survres;
time genres*year_start(0);
run;
data survres;
set survres;
lls=-log(-log(survival));
loggenr=-log(genres);
run;

/* old graph
proc sgplot data=survres;
reg y=lls x=loggenr / lineattrs= (color = black) markerattrs= (color = black);
title "Cox-Snell Residual Plot"; 
title2 "3 Position Cox-Proportional Hazard";
xaxis label = "Log(Cox-Snell Residual)"; yaxis label = "Log Negative Log Survival";
run; */ 

proc sgplot data=survres;
reg y=lls x=loggenr / lineattrs= (color = black) markerattrs= (color = black); 
title height=18pt "3 Position"; 
xaxis label = "Log(Cox-Snell Residual)" valueattrs=(size=15pt) labelattrs=(size=15pt); yaxis label = "Log Negative Log Survival" valueattrs=(size=15pt) labelattrs=(size=15pt); 
run;*this plot should go in the paper;

*martingale residuals need to do for every x variable;
*we want it to be linear, but we might need to transform it;
proc sgplot data =outp2; 
loess y = mart x = xb; /* predicted values */ 
run; 
proc sgplot data = outp2;
loess y=mart x=position; 
run;  
proc sgplot data = outp2; 
loess y=mart x=year_start;
run;

*deviance residuals; 
proc sgplot data=outp2;
scatter y=dev x=id;
 refline -2 2 /axis=y;
run;

proc sgplot data=outp2;
histogram dev;
run;

/**************************DFBETA residuals (score residuals)8****************/
/**************************Testing for influencial outliers*******************
******** influence cut off = 2/sqrt(n)*******2/sqrt(4254) = .0306641727*******/
proc phreg data = nba_50 noprint; 
	class cat_3 (ref="C"); 
		model time*retired (0)= cat_3 year_start age_start num_height ; 
	output out=outp2 
	DFBETA=dfbetcat_3F dfbetcat_3G dfbetyear_start dfbetage_start dfbetnum_height ;
run; 

*forward; 
proc sgplot data = outp2; 
	scatter y=dfbetcat_3F x=cat_3;
	refline -0.030664 0.030664 / axis = y; 
run; 

proc univariate data = outp2; 
var dfbetcat_3F; 
id id; 
run; 

proc sgplot data = outp2; 
	refline 0 / axis = y; 
	scatter y= dfbetcat_3F x = id; 
run; 

*guard;
proc sgplot data = outp2; 
	scatter y=dfbetcat_3G x=cat_3;
	refline -0.030664 0.030664 / axis = y; 
run;

proc univariate data = outp2; 
var dfbetcat_3G; 
id id; 
run; 

proc sgplot data = outp2; 
	refline 0 / axis = y; 
	scatter y= dfbetcat_3G x = id; 
run; 

*year start;
proc sgplot data = outp2; 
	scatter y=dfbetyear_start x=year_start;
	refline -0.030664 0.030664 / axis = y; 
run; 

proc univariate data=outp2;
var dfbetyear_start;
id id;
run;

proc sgplot data = outp2; 
	refline 0 / axis = y; 
	scatter y= dfbetyear_start x = id; 
run; 
*age start; 
proc sgplot data = outp2; 
	scatter y=dfbetage_start x = age_start; 
	refline -0.030664 0.030664 / axis = y; 
run; 

proc univariate data=outp2;
var dfbetage_start;
id id;
run;

proc sgplot data = outp2; 
	refline 0 / axis = y; 
	scatter y= dfbetage_start x = id; 
run;

*height ; 
proc sgplot data = outp2; 
	scatter y=dfbetnum_height x = num_height; 
	refline -0.030664 0.030664 / axis = y; 
run; 

proc univariate data=outp2;
var dfbetnum_height;
id id;
run;

proc sgplot data = outp2; 
	refline 0 / axis = y; 
	scatter y= dfbetnum_height x = id; 
run;

/******check to see if proportional hazard assumption is valid for every x variable******/ 
*weighted schoenfeld residual;

proc phreg data = nba_50 noprint; 
	class cat_3 (ref="C"); 
		model time*retired (0)= cat_3 year_start age_start num_height ; 
	output out=outp2 
	WTRESSCH =resschcat_3 resschcat_31  resschyear_start resschage_start resschnum_height  ;
run; 

proc sgplot data=outp2;
scatter y=resschcat_3 x=time;
run;

proc sgplot data=outp2;
scatter y=resschcat_31 x=time;
run;
   
proc sgplot data=outp2;
scatter y=resschyear_start x=time;
run;
   
proc sgplot data=outp2;
scatter y=resschage_start x=time;
run;
   
proc sgplot data=outp2;
scatter y=resschnum_height x=time;
run;
   





/*
*
*
this section below is only using G, F, C and excluding the combo players 
*
*
*/ 
*subset data set;  
data nba_50_3;
	set nba_50; 
	where position = "G" or position = "F" or position = "C" ;
run; 
*n = 3196;

ods graphics on; 
ods output ParameterEstimates = overall; 
proc phreg data = nba_50_3 plots(overlay)=survival; 
	class position (ref="C"); 
		model time*retired (0)= position year_start age_start num_height /rl; 
		baseline covariates = nba_50_3 out = overall survival= _all_/diradj group = position;
run; 
ods graphics off; 


******************************
just to verify that it was similar
Forwards had a higher hazard compared to Centers [1.037, 1.367]
Guards non-sig .960 [.773, 1.192]
age and year were sign 
not height though
******************************; 










/***************************** 
cat_combo model
******************************/
*model selection; 
proc phreg data=nba_50;
   class cat_combo (ref="0");
   model time*retired (0) = cat_combo age_start year_start num_height 
                         / include=1 selection=stepwise slentry=0.15
                           slstay=0.1 details;
                           
run;
*num_height is not significant (did also try weight in model and wasn't significant);
*model contains year_start and age_start; 

ods graphics on; 
ods output ParameterEstimates = overall; 
proc phreg data = nba_50 plots(overlay)=survival; 
	class cat_combo ; 
		model time*retired (0)= cat_combo year_start age_start /rl; 
		baseline covariates = nba_50 out = overall survival= _all_/diradj group = cat_combo;
run; 
ods graphics off; 


ods graphics on; 
ods output ParameterEstimates = overall; 
proc phreg data = nba_50 plots(overlay)=survival; 
	class cat_combo (ref="0"); 
		model time*retired (0)= cat_combo year_start age_start /rl; 
			baseline out = overall2 survival= _all_/diradj group = cat_combo;
		assess ph/resample = 20;
		output out=outp3 
	survival = survival xbeta = xb 
	DFBETA=dfbetposition dfbetyear_start 
	WTRESSCH =resschcat_combo  resschyear_start resschage_start resschnum_height  
	logsurv=snell resmart = mart resdev = dev; *this is saving the output includes survival, cox-snell and martingale residuals;
run; 
ods graphics off;


*need to cox-snell (see homework 7);
*manipulate cox-snell to get then plot it; 

/* this is for versatility */
data outp3; 
set outp3; 
genres=-snell;
run;
proc lifetest data=outp3 outsurv=survres;
time genres*year_start(0);
run;
data survres;
set survres;
lls=-log(-log(survival));
loggenr=-log(genres);
run;

proc sgplot data=survres;
reg y=lls x=loggenr / lineattrs= (color = black) markerattrs= (color = black); 
title height=18pt "Combination Player"; 
xaxis label = "Log(Cox-Snell Residual)" valueattrs=(size=15pt) labelattrs=(size=15pt); yaxis label = "Log Negative Log Survival" valueattrs=(size=15pt) labelattrs=(size=15pt); 
run; *this plot should go in the paper;

*martingale residuals need to do for every x variable;
*we want it to be linear, but we might need to transform it;
proc sgplot data =outp3; 
loess y = mart x = xb; /* predicted values */ 
run; 
proc sgplot data = outp3;
loess y=mart x=position; 
run;  
proc sgplot data = outp3; 
loess y=mart x=year_start;
run;

*deviance residuals; 
proc sgplot data=outp3;
scatter y=dev x=id;
 refline -2 2 /axis=y;
run;

proc sgplot data=outp3;
histogram dev;
run;

/**************************Testing for influencial outliers*******************/
/************************* influence cut off = 2/sqrt(n)**********************/ 
/****************************2/sqrt(4254) = .0306641727***********************/
*dfbetcat_combo  dfbetyear_start dfbetage_start dfbetnum_height;
proc phreg data = nba_50 noprint; 
	class cat_combo (ref="0"); 
		model time*retired (0)= cat_combo year_start age_start /rl; 
		output out=outp3 
	DFBETA=dfbetposition dfbetyear_start dfbetage_start ;
run; 


proc sgplot data = outp3; 
	scatter y=dfbetposition x=cat_combo;
	refline -0.030664 0.030664 / axis = y; 
run; 

proc sgplot data = outp3; 
	scatter y=dfbetyear_start x=year_start;
	refline -0.030664 0.030664 / axis = y; 
run; 

proc sgplot data = outp3; 
	scatter y=dfbetage_start x = age_start; 
	refline -0.030664 0.030664 / axis = y; 
run; 

proc univariate data = outp3; 
var dfbetcat_combo; 
id id; 
run; 

proc univariate data=outp3;
var dfbetyear_start;
id id;
run;

proc univariate data=outp3;
var dfbetage_start;
id id;
run;


/******check to see if proportional hazard assumption is valid for every x variable******/ 
*standard score or weighted schoenfeld residual;
proc phreg data = nba_50 plots(overlay)=survival; 
	class cat_combo (ref="0"); 
		model time*retired (0)= cat_combo year_start age_start ; 
		output out=outp3 
	WTRESSCH =resschcat_combo  resschyear_start resschage_start;   
run; 


proc sgplot data=outp3;
scatter y=resschcat_combo x=time;
run;
   
proc sgplot data=outp3;
scatter y=resschyear_start x=time;
run;
   
proc sgplot data=outp3;
scatter y=resschage_start x=time;
run;
 




*proportional hazard violated;
*accelerated survival time;
data nba_50; 
	set nba_50; 
	log_t = log(time)+1; 
run;

proc univariate data = nba_50;
	var log_t; 
	histogram / exponential; 
run; 

proc univariate data = nba_50; 
	var time; 
	histogram / lognormal; 
	run; 

proc univariate data = nba_50; 
	var time; 
	histogram / weibull; 
	run; 
	
proc univariate data = nba_50; 
	var time; 
	histogram / gamma; 
run; 

proc lifereg data=nba_50 order = freq; *can't change the reference group...;
   class  cat_combo ; *reference group = 1;
      model  time*retired(0)=cat_combo year_start age_start /dist=gamma;
	    output out=temp p=perc quantiles=0.1 0.5 0.9 cdf=f xbeta=xb STD=se; 
 PROBPLOT;
 run;

/*
proc lifereg data=nba_50 order = freq; *order by center...;
   class  cat_5 ;
      model  time*retired(0)=cat_5 year_start age_start num_height /dist=gamma;
	    output out=temp p=perc quantiles=0.1 0.5 0.9 cdf=f xbeta=xb STD=se; 
 PROBPLOT;
run;
*/

/* lowest AIC for cat_5 model gamma 10408.84 second lowest lognormal 10410.65 third llogistic 10691.15*/
/* don't need because proportional hazard isn't violated lowest AIC for cat_3 model gamma 10734.26 second lowest lognormal 10769.47 */ 
/* lowest AIC for cat_combo model gamma 10437.90 second lowest lognormal 10438.78 */ 
/* residual analysis: Cox-Snell*/
data res;
 set temp;
 e=-log(1-f); 
run;
proc lifetest data=res plots=(ls) notable graphics outsurv=surv_gamma;
 time e*retired(0);
 symbol1 v=none;
run;

data surv_gamma;
  set surv_gamma;
  ls = -log(survival);
run;
goptions reset=all;
axis1 order=(0 to 7 by 1) minor=none label=(h=2.5 'Gamma AFT Model Cumulative Hazard') value = (height = 2.5);
axis2 order=(0 to 9 by 1) minor=none label=(h=2.5 a=90 'Kaplan-Meier Cumulative Hazard') value = (height = 2.5);
title h=3 'Combination Player'; 
symbol1 i=l1p  c= black v=dot h=1;
symbol2 i = join c = red l = 4 h=1;
proc gplot data=surv_gamma;
  plot (ls e)*e / overlay haxis=axis1 vaxis= axis2;
run;
quit;



*5 position;
proc lifereg data=nba_50 order = freq; *can't change the reference group...;
   class  cat_5 ; *reference group =C;
      model  time*retired(0)=cat_5 year_start age_start /dist=gamma;
	    output out=temp p=perc quantiles=0.1 0.5 0.9 cdf=f xbeta=xb STD=se; 
 PROBPLOT;
 run;
data res;
 set temp;
 e=-log(1-f); 
run;
proc lifetest data=res plots=(ls) notable graphics outsurv=surv_gamma;
 time e*retired(0);
 symbol1 v=none;
run;

data surv_gamma;
  set surv_gamma;
  ls = -log(survival);
run;
goptions reset=all;
*title = ("Combination Player"); 
axis1 order=(0 to 7 by 1) minor=none label=(h=2.5 'Gamma AFT Model Cumulative Hazard') value = (height = 2.5);
axis2 order=(0 to 9 by 1) minor=none label=(h=2.5 a=90 'Kaplan-Meier Cumulative Hazard') value = (height = 2.5);
title h=3 '5 Position'; 
symbol1 i=l1p  c= black v=dot h=1;
symbol2 i = join c = red l = 4 h=1;
proc gplot data=surv_gamma;
  plot (ls e)*e / overlay haxis=axis1 vaxis= axis2;
run;
quit;

