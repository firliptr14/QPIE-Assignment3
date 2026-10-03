/*****************************************/
/*    		  Assignment 3     			 */
/*    by Firli Wulansari Wahyuputri		 */
/* 			   u8323089		 			 */
/*****************************************/

clear		all
set			more off
cd "/Users/Firli/Downloads/IDEC8026"
use "raw_data.dta"

/******Question 1******************/
rename v1 AGE
rename v2 AGEQ
rename v4 EDUC
rename v5 ENOCENT
rename v6 ESOCENT
rename v9 LWKLYWGE
rename v10 MARRIED
rename v11 MIDATL
rename v12 MT
rename v13 NEWENG
rename v16 CENSUS
rename v18 QOB
rename v19 RACE
rename v20 SMSA
rename v21 SOATL
rename v24 WNOCENT
rename v25 WSOCENT
rename v27 YOB


********** YOB dummies **********

replace YOB = YOB - 1900 if YOB >= 1900

foreach i of numlist 0/9 {
    gen YR`i' = 0
    replace YR`i' = 1 if YOB == 20+`i' | YOB == 30+`i' | YOB == 40+`i'
}


********** QOB dummies **********

foreach i of numlist 1/4 {
    gen QTR`i' = 0
    replace QTR`i' = 1 if QOB == `i'
}

////////////////////////////////////////////////////////
********** Select Particular Men Born **********

gen COHORT = 2029

replace COHORT = 3039 if YOB <= 39 & YOB >= 30
replace COHORT = 4049 if YOB <= 49 & YOB >= 40

replace AGEQ = AGEQ - 1900 if CENSUS == 80

gen AGEQSQ = AGEQ * AGEQ


********** Keep 1930-1939 cohort **********

keep if COHORT > 3000 & COHORT < 3040


********** Start Regression **********

eststo clear


* Model 1: OLS
reg LWKLYWGE EDUC YR0-YR8
eststo model1


* Model 2: TSLS using only quarter-of-birth dummies
ivregress 2sls LWKLYWGE YR0-YR8 ///
    (EDUC = QTR1 QTR2 QTR3)

eststo model2
estat firststage


* Model 3: OLS + age controls
reg LWKLYWGE EDUC YR0-YR8 AGEQ AGEQSQ
eststo model3


* Model 4: TSLS + age controls
ivregress 2sls LWKLYWGE YR0-YR8 AGEQ AGEQSQ ///
    (EDUC = QTR1 QTR2 QTR3)

eststo model4
estat firststage


* Model 5: OLS + demographic/geographic controls
reg LWKLYWGE EDUC ///
    RACE MARRIED SMSA ///
    NEWENG MIDATL ENOCENT WNOCENT ///
    SOATL ESOCENT WSOCENT MT ///
    YR0-YR8

eststo model5


* Model 6: TSLS + demographic/geographic controls
ivregress 2sls LWKLYWGE ///
    YR0-YR8 ///
    RACE MARRIED SMSA ///
    NEWENG MIDATL ENOCENT WNOCENT ///
    SOATL ESOCENT WSOCENT MT ///
    (EDUC = QTR1 QTR2 QTR3)

eststo model6
estat firststage


* Model 7: OLS + all controls
reg LWKLYWGE EDUC ///
    RACE MARRIED SMSA ///
    NEWENG MIDATL ENOCENT WNOCENT ///
    SOATL ESOCENT WSOCENT MT ///
    YR0-YR8 AGEQ AGEQSQ

eststo model7


* Model 8: TSLS + all controls
ivregress 2sls LWKLYWGE ///
    YR0-YR8 ///
    RACE MARRIED SMSA ///
    NEWENG MIDATL ENOCENT WNOCENT ///
    SOATL ESOCENT WSOCENT MT ///
    AGEQ AGEQSQ ///
    (EDUC = QTR1 QTR2 QTR3)

eststo model8
estat firststage


********** Table Decoration **********

label variable EDUC     "Years of education"
label variable RACE     "Race (1 = black)"
label variable SMSA     "SMSA (1 = center city)"
label variable MARRIED  "Married (1 = married)"
label variable AGEQ     "Age"
label variable AGEQSQ   "Age-squared"


********** Export Table **********

esttab using ///
"table5_nointeraction.tex", ///
replace ///
se ///
varwidth(25) ///
label ///
keep(EDUC RACE SMSA MARRIED AGEQ AGEQSQ) ///
order(EDUC RACE SMSA MARRIED AGEQ AGEQSQ) ///
title("TABLE V: Quarter-of-Birth Instruments Without Birth-Year Interactions") ///
nonumbers ///
mtitles("(1) OLS" "(2) TSLS" "(3) OLS" "(4) TSLS" ///
        "(5) OLS" "(6) TSLS" "(7) OLS" "(8) TSLS")

		
/***********************************/
/******Question 2******************/		
/***********************************/

**************************************************
********** Standardize Year of Birth *************
**************************************************

replace YOB = YOB - 1900 if YOB >= 1900


**************************************************
********** Create Q1 Instrument ******************
**************************************************

gen Q1 = 0
replace Q1 = 1 if QOB == 1

label variable Q1 "Born in first quarter"


**************************************************
********** Prepare Age Variables *****************
**************************************************

replace AGEQ = AGEQ - 1900 if CENSUS == 80

gen AGEQSQ = AGEQ * AGEQ


**************************************************
********** Clear Stored Estimates ****************
**************************************************

eststo clear


**************************************************
********** Birth Year 1920 ***********************
**************************************************

preserve

keep if YOB == 20

* Mean log weekly wage and education by Q1 status
mean LWKLYWGE EDUC, over(Q1)

* OLS
reg LWKLYWGE EDUC
eststo OLS1920

* IV / Wald estimate
ivregress 2sls LWKLYWGE (EDUC = Q1)
eststo IV1920

* First-stage results
estat firststage

restore


**************************************************
********** Birth Year 1930 ***********************
**************************************************

preserve

keep if YOB == 30

* Mean log weekly wage and education by Q1 status
mean LWKLYWGE EDUC, over(Q1)

* OLS
reg LWKLYWGE EDUC
eststo OLS1930

* IV / Wald estimate
ivregress 2sls LWKLYWGE (EDUC = Q1)
eststo IV1930

* First-stage results
estat firststage

restore


**************************************************
********** Birth Year 1940 ***********************
**************************************************

preserve

keep if YOB == 40

* Mean log weekly wage and education by Q1 status
mean LWKLYWGE EDUC, over(Q1)

* OLS
reg LWKLYWGE EDUC
eststo OLS1940

* IV / Wald estimate
ivregress 2sls LWKLYWGE (EDUC = Q1)
eststo IV1940

* First-stage results
estat firststage

restore


**************************************************
********** Variable Labels ***********************
**************************************************

label variable EDUC "Years of education"


**************************************************
********** Export OLS and IV Table ***************
**************************************************

esttab OLS1920 IV1920 ///
       OLS1930 IV1930 ///
       OLS1940 IV1940 ///
using "question2.tex", ///
replace ///
se ///
label ///
keep(EDUC) ///
title("Year-Specific OLS and IV Estimates") ///
nonumbers ///
mtitles("1920 OLS" "1920 IV" ///
        "1930 OLS" "1930 IV" ///
        "1940 OLS" "1940 IV")


**************************************************
********** Optional: Display in Stata ************
**************************************************

esttab OLS1920 IV1920 ///
       OLS1930 IV1930 ///
       OLS1940 IV1940, ///
se ///
label ///
keep(EDUC) ///
mtitles("1920 OLS" "1920 IV" ///
        "1930 OLS" "1930 IV" ///
        "1940 OLS" "1940 IV")
log close
