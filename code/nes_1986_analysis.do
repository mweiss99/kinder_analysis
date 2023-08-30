cd "X~X"

use nes1986, clear

* replace variable names with their label 
foreach v of var * {
	local x : var label `v'
	local x = strtoname("`x'")
	rename `v' `x'
} 
