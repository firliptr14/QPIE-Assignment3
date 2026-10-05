/*****************************************/
/*    		  Assignment 3     			 */
/*          by Rafi Farhanto		     */
/* 			   u8323089		 			 */
/*****************************************/

clear		all
set			more off
cd "G:\My Drive\File Laptop\Postgraduate\Study\Semester 1\3. Quantitative Policy Impact Evaluation\Assignment 3"
use "raw_data.dta"

/***********************************/
/****** Question 1 *****************/
/***********************************/

********** Rename variables **********

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


********** Standardize YOB **********

replace YOB = YOB - 1900 if YOB >= 1900


********** YOB dummies **********

foreach i of numlist 0/9 {
    gen YR`i' = 0
    replace YR`i' = 1 if YOB == 20+`i' | ///
                         YOB == 30+`i' | ///
                         YOB == 40+`i'
}


********** QOB dummies **********

foreach i of numlist 1/4 {
    gen QTR`i' = 0
    replace QTR`i' = 1 if QOB == `i'
}


********** Create birth-year cohorts **********

gen COHORT = 2029

replace COHORT = 3039 if YOB >= 30 & YOB <= 39
replace COHORT = 4049 if YOB >= 40 & YOB <= 49


********** Prepare age variables **********

replace AGEQ = AGEQ - 1900 if CENSUS == 80

gen AGEQSQ = AGEQ * AGEQ


/********************************************************/
/* IMPORTANT: preserve full sample before restricting    */
/* to 1930-1939 for Question 1                          */
/********************************************************/

preserve


********** Keep 1930-1939 cohort **********

keep if COHORT > 3000 & COHORT < 3040


********** Start Regression **********/

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


********** Export Question 1 Table **********

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


/********************************************************/
/* Question 2                 */
/********************************************************/

restore


/***********************************/
/****** Question 2 *****************/
/***********************************/

********** Create Q1 instrument **********/

gen Q1 = 0
replace Q1 = 1 if QOB == 1

label variable Q1 "Born in first quarter"


********** Clear stored estimates **********/

eststo clear


/************************************************/
/********** Birth Year 1920 *********************/
/************************************************/

preserve

keep if YOB == 20

* Check sample size
count

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


/************************************************/
/********** Birth Year 1930 *********************/
/************************************************/

preserve

keep if YOB == 30

* Check sample size
count

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


/************************************************/
/********** Birth Year 1940 *********************/
/************************************************/

preserve

keep if YOB == 40

* Check sample size
count

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


********** Variable Labels **********/

label variable EDUC "Years of education"


********** Export Question 2 Table **********/

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


********** Display Question 2 Table in Stata **********/

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
