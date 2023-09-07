/* I: Import data, operationalize variables 
------------------------------------------------------------- */ 
cd "X~X"

use nes1986, clear

* only keep necessary variables
keep V860059 V860066 V860100 V860130 V860149 V860755 V860756 V860757 V860754 V860761 V860599 V860490 V860343 V860344 V860345 V860346 V860347 V860348 V860102 V860301 V860485 


* identify form A and form B respondents
rename V860046 _form

local forms a b 
foreach form of local forms {
	g _form_`form' = . 
}

replace _form_a = 1 if _form == 1
replace _form_b = 1 if _form == 2

* create variables for race, ethnicity, sex, education
rename V860755 sex
rename V860756 race
rename V860757 hispanic 
rename V860599 education

g white = . // white means non-hispanic whites
replace white = 0 if inrange(race, 1, 4) & race != 99 
replace white = 1 if race == 1 & hispanic == 5 & race != 99 

g black = .
replace black = 1 if race == 2

g nhs = 0 
replace nhs = 1 if education < 12 

* rename variables related to knowledge
forval y = 3/8 { // this loop replaces each relevent variable name with its label
	foreach v of var V86034`y' {
		local x : var label `v'
		local x = strtoname("`x'")
		rename `v' `x' 
	}
}



