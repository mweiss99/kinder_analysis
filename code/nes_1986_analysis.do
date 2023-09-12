/* This file:
- Imports 1986 ANES data
- Defines variables and constructs indices 
- Performs regression analyses
- Generates descriptive statistics/frequencies

Last updated September 8, 2023 by Mark Weiss */


/* I: Import data, operationalize variables 
------------------------------------------------------------- */ 
cd "X~X"

use nes1986, clear

* identify form A and form B respondents
rename V860046 _form

local forms a b 
foreach form of local forms {
	g _form_`form' = . 
}

replace _form_a = 1 if _form == 1
replace _form_b = 1 if _form == 2

* only keep necessary variables
keep _form_a _form_b V860102 V860489 V860059 V860066 V860100 V860130 V860149 V860755 V860756 V860757 V860754 V860761 V860599 V860490 V860343 V860344 V860345 V860346 V860347 V860348 V860102 V860301 V860485 

* create variables for demographic information
rename V860755 sex
rename V860756 race
rename V860757 hispanic 
rename V860599 education
rename V860754 rent_own 

g white = . // white means non-hispanic whites
replace white = 0 if inrange(race, 1, 4) & race != 99 
replace white = 1 if race == 1 & hispanic == 5 & race != 99 

g black = .
replace black = 1 if race == 2
replace black = 0 if race == 1 | (race > 2 & race != 99)

g nhs = .
replace nhs = 1 if education < 12 
replace nhs = 0 if education >= 12 & education != 99 

g hs = . 
replace hs = 1 if education == 12
replace hs = 0 if education < 12 | (education > 12 & education != 99) 

g somecol = . 
replace somecol = 1 if inrange(education, 13, 15)
replace somecol = 0 if education < 12 | (education > 15 & education != 99)  

g college = .
replace college = 1 if education == 16 
replace college = 0 if education < 16 | (education > 16 & education != 99) 

g own = . 
replace own = 1 if rent_own == 1
replace own = 0 if inrange(rent_own, 2, 7)

g female = .
replace female = 1 if sex == 2 
replace female = 0 if sex == 1

* rename variables related to political knowledge
forval y = 3/8 { // replace each relevent variable name with its label
	foreach v of var V86034`y' {
		local x : var label `v'
		local x = strtoname("`x'")
		rename `v' `x' 
	}
}

rename DOES_R_KNW_CAP_WEINBERGR DOES_R_KNOW_CAP_WEINBERGR
local names GEORGE_BUSH CAP_WEINBERGR WM_REHNQUIST PAUL_VOLKER ROBERT_DOLE TIP_O_NEILL

* sum correct responses to form knowledge of politics index
foreach name of local names {
	g ident_`name' = . 
	replace ident_`name' = 1 if DOES_R_KNOW_`name' == 1 
	replace ident_`name' = 0 if inlist(DOES_R_KNOW_`name', 5, 8)
}

egen knowledge = rowmean(ident_GEORGE_BUSH ident_CAP_WEINBERGR ident_WM_REHNQUIST ident_PAUL_VOLKER ident_ROBERT_DOLE ident_TIP_O_NEILL)

* taste for politics variable
rename V860102 interest_1
rename V860059 interest_2

recode interest_1 (1 = 0.5) (2 = 0.334) (4 = 0.167) (5 = 0) (8 = .) (9 = .), g(interest_a)
recode interest_2 (1 = 0.5) (3 = 0.334) (5 = 0.167) (9 = .), g(interest_b)

egen taste = rowtotal(interest_a interest_b)

* variable for economic sanctions opinions
rename V860490 support_sanctions
rename V860489 know_africa 

g know_sanct = .
replace know_sanct = 1 if inlist(support_sanctions, 1, 5)
replace know_sanct = 0 if inlist(support_sanctions, 0, 8)

g sanctions = . // takes a position on sanctions 
replace sanctions = 1 if support_sanctions == 1
replace sanctions = 0 if inlist(support_sanctions, 5, 8) 

/* II: Regression Analyses
------------------------------------------------------------- */ 
local controls hs college somecol nhs knowledge female 
local surveys _form_a _form_b
local populations white black 



