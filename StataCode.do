/********************
PUBLIC ECONOMICS
Stata Assignment
*********************

authors: Clemens Gerland, Gregor Lauth, Daniel Javes, Tommaso Musetti 
*********************/

*** Program: Public_GP_code
*** first created: 05/12/2024
*** last updated:  05/12/2024


/*----------------------
Initial script configuration
-----------------------*/

	// Please replace your path in the command below
	cd "/home/clemens/Documents/Clem/Pub Econ Task"
	local direc : pwd
	
	// Importing the necessary dataset
	use municipalities5k_2000_2015.dta, clear
	
	// Installing the necessary packages
	ssc install rdrobust
	ssc install rddensity
	net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace
	ssc install ftools
	ssc install estout
	ssc install outreg2
/*----------------------
Exercise 1 
-----------------------*/

	// Describing the data
	des
	sort female
	by female: tabstat lptot_total lp_e_tax_fee lp_e_other, stats(mean, sd, n, min, max)
	
	// Generating the output table 
	dtable lptot_total lpcor_total lpcap_total lp_e_tax_fee lp_e_other, by(female, nototals tests) export(table1.tex, replace) sample(, statistics(freq) place(seplabels)) sformat("(N=%s)" frequency) note(Total sample: N = 5,526)  column(by(hide))


/*----------------------
Exercise 2
-----------------------*/

	
	// Regressing log per capita total expenditures on the female dummy
	eststo reg1: reg lptot_total female 

	// Creating a global for controls
	global g_controls =  "sh_empl sh_hschool hs_mayor sh_young_old logpop i.year share_fcons"
 
	// Running the same regression controlling for covariates
	eststo reg2: reg lptot_total female $g_controls, vce(hc2)
	* Tom: I have a doubt for this: shouldn't the variable year be included as i.year (like a categorical variable)? For now I changed it to i.year, let me know if you agree. Otherwise we are taking years as actual numbers. Also, can we include year in a fixed effect model?

	// Running the same regression with municipality FE
	eststo reg3:reghdfe lptot_total female $g_controls, absorb(n_istat) vce(cluster n_istat)
	
	// Using xtreg and xtreg
	xtset n_istat year
	xtreg lptot_total female $g_controls, fe

 
	*Store all in table
	esttab reg1 reg2 reg3 using "RegExercise2.tex", replace
	
/*----------------------
Exercise 3
-----------------------*/
	// Providing evidence that the design is sharp RDD
	rdplot female mv, graph_options(xtitle("Margin of female victory") ytitle("Female mayor") title("Regression discontinuity plot"))
	* The treatment status (having a female mayor) appears to be deterministically defined by the running variable (female margin of victory). Hence, the design appears to be sharp. 


/*----------------------
Exercise 4
-----------------------*/
	// RD plots for all covariates
	global covariates = "lp_e_tax_fee lp_e_other share_fcons logpop sh_hschool sh_illiteracy sh_empl sh_young_old hs_mayor north south right left litor population pop_dens"

	local rd_graphs ""
	foreach var of global covariates {
		// Rdrobust to estimate coefficient and confidence intervals for baseline covariates
		rdrobust `var' mv
		local coef = e(tau_cl)
		local ci_l = e(ci_l_cl)
		local ci_r = e(ci_r_cl)
		
		local varlabel: variable label `var' // Get the variable label
		local plot_title "Coef = `:display %9.3f `coef'', CI = [`:display %9.3f `ci_l'', `:display %9.3f `ci_r'']"

		// Rdplot to check discontinuities in baseline covariates 
		rdplot `var' mv, graph_options(ytitle("`varlabel'") xtitle("Margin of female victory") title("`plot_title'") legend(off)) name(g_`var', replace)
		graph save g_`var', replace
		local rd_graphs "`rd_graphs' g_`var'.gph"
	}

	graph combine `rd_graphs', altshrink
	graph export DiscontinuitiesBaselineCovariates.pdf, replace
	
	// Erase the generated individual RD plots
	foreach var of global covariates {
		erase "g_`var'.gph"
	}
	
	// Table for RD Coefficients
	ds lp_e_tax_fee lp_e_other share_fcons logpop sh_hschool sh_illiteracy sh_empl sh_young_old hs_mayor north south right left litor population pop_dens

	matrix define R=J(16,6,.)
	matrix rownames R = lp_e_tax_fee lp_e_other share_fcons logpop sh_hschool sh_illiteracy sh_empl sh_young_old hs_mayor north south right left litor population pop_dens
	matrix colnames R = CIp1_L convp1RD CIp1_R CIp2_L convRDp2 CIp2_R
	matrix list R

	*Loop through each covariate and run RDD, use p(1) and p(2) -> Use robust CI
	global g_r = 1 
	foreach var of global covariates {
    rdrobust `var' mv, p(1) q(2) c(0)
    matrix R[$g_r ,1] = e(ci_l_cl)
	matrix R[$g_r ,2] = e(tau_cl)
	matrix R[$g_r ,3] = e(ci_r_cl)
	global g_r = $g_r +1
	}
	global g_r = 1 
	foreach var of global covariates {	
    rdrobust `var' mv, p(2) q(3) c(0)
    matrix R[$g_r ,4] = e(ci_l_cl)
	matrix R[$g_r ,5] = e(tau_cl)
	matrix R[$g_r ,6] = e(ci_r_cl)
	global g_r = $g_r +1
	}
	matrix list R	


/*----------------------
Exercise 5
-----------------------*/
	// Outlining why it is important to verify the continuity of the running variable around the threshold

	// Generating a graph of the density of the running variable 
	rddensity mv, plot  graph_opt(title(Density Discontinuity Test for Voting Margin) legend(off) name(density_plot, replace))

	 graph export "", replace
	 
	// Checking for statistically significant discontinuities at the cutoff in the density of the running variable
	// Manipulation test of running variable using default triangular kernel
	rddensity mv, all
	// Using other kernel types
	rddensity mv, all kernel(uniform)
	rddensity mv, all kernel(epanechnikov) 
	* All three methods give the same result.

	
/*----------------------
Exercise 6
-----------------------*/

*Parametric local linear regression with optimal bandwidth (write down the estimated equation)
// Creating a global for controls
global g_controls =  "sh_empl sh_hschool hs_mayor sh_young_old logpop share_fcons"
 
rdrobust lptot_total mv, p(1) c(0) covs(${g_controls})
outreg2 using rdregressions.tex, replace ctitle(Optimal Bandwidth)

local bwl = e(h_l)
local bwr = e(h_r) 
rdplot lptot_total mv if (mv<=`bwr' & mv >= -`bwl'), c(0) p(1) kernel(tri) nbins(20 20) graph_options(xtitle("Margin of female victory") ytitle(Total expenditures (log, per capita)) title(Parametric Regression (Optimal Bandwidth)))
graph save ParametricRegressionOptimalBandwidth, replace

*Parametric regression on the full sample with 2nd order polynomial fit on both sides of the threshold (write down the estimated equation)

rdrobust lptot_total mv, p(2) c(0) covs(${g_controls})
outreg2 using rdregressions.tex, append ctitle(2nd order polynomial)

rdplot lptot_total mv, c(0) p(2) kernel(tri) nbins(20 20) graph_options(xtitle("Margin of female victory") ytitle(Total expenditures (log, per capita)) title(Parametric Regression (2nd-order Polynomial fit)))
graph save ParametricRegressionSecondPolynomial, replace


*Non-parametric local linear regression with optimal bandwidth. 

rdrobust lptot_total mv, p(1) c(0) covs(${g_controls})
outreg2 using rdregressions.tex, append ctitle(Non-parametric)

local bwl = e(h_l)
local bwr = e(h_r) 
rdplot lptot_total mv if (mv<=`bwr' & mv >= -`bwl'), c(0) kernel(tri) nbins(20 20) graph_options(xtitle("Margin of female victory") ytitle(Total expenditures (log, per capita)) title(Non-Parametric Regression (Optimal Bandwidth)))
graph save NonParametricRegressionOptimalBandwidth, replace


*Extra: Here also with p(2)
rdrobust lptot_total mv, p(2) c(0) covs(${g_controls})
local bwl = e(h_l)
local bwr = e(h_r) 
rdplot lptot_total mv if (mv<=`bwr' & mv >= -`bwl'), c(0) p(2) kernel(tri) nbins(20 20) 

// Combining the three plots
graph combine ParametricRegressionOptimalBandwidth.gph ParametricRegressionSecondPolynomial.gph NonParametricRegressionOptimalBandwidth.gph, altshrink

graph export ParametricRegressions.pdf, replace

// Erase the generated individual RD plots
erase "ParametricRegressionOptimalBandwidth.gph" 
erase "ParametricRegressionSecondPolynomial.gph"
erase "NonParametricRegressionOptimalBandwidth.gph"




/*----------------------
Exercise 7
-----------------------*/

*Sensitivity to bandwidth
gen FemWon = female*mv
matrix coef=J(30,4,.)
mat colnames coef=bw b ul ll
mat rownames coef= 2% 3% 4% 5% 6% 7% 8% 9% 10% 11% 12% 13% 14% 15% 16% 17% 18% 19% 20% 21% 22% 23% 24% 25% 26% 27% 28% 29% 30%
local i=1
*Estimate regressions for different bandwidths (between 2% and 30%)
forvalues h=0.02(0.01)0.31 {
	reg lptot_total female mv FemWon $g_controls if abs(mv)<`h', cluster(n_istat) robust
	mat coef[`i',1]=`h'
	mat coef[`i',2]=_b[female]
	mat coef[`i',3]=_b[female]+1.96*_se[female]
	mat coef[`i',4]=_b[female]-1.96*_se[female]
	local i=`i'+1
}
**Is there only a significant effect for bw 3%?

**Number of obs per h
forvalues h = 0.02(0.01)0.3 {
    count if abs(mv) < `h'
    di "Bandwidth `h': `r(N)' observations"
}
**Table
matrix list coef


* Create new Table
file open myfile using "Exercise7.tex", write replace

*Write LaTeX table header
file write myfile "{\begin{tabular}{cccc}" _newline
file write myfile "\hline" _newline
file write myfile "Bandwith & Coefficient & Upper Bound & Lower Bound \\" _newline
file write myfile "\hline" _newline

* Loop through the matrix and write it to the .tex file
forval i = 1/30 {  
    local row ""
    forval j = 1/4 {  
        local value = coef[`i', `j']
	
	if `j'==4 {
		local row "`row' `value'"

	}
	else {
		local row "`row' `value' &"
	}
    }
    file write myfile "`row' \\" _newline
}

* Close the LaTeX table and file
file write myfile "\hline" _newline
file write myfile "\end{tabular}" _newline
file close myfile




/*----------------------
Exercise 8
-----------------------*/

	// Generating quartiles of the share_fcons
	xtile share_fcons_quartile=share_fcons,n(4)
	
	// Continuous appraoch
	reg lptot_total mv c.share_fcons##i.female $g_controls, robust
	outreg2 using heterogenouseffects.tex, replace ctitle(Continuous approach)
	
	// Discrete approach
	reg lptot_total mv i.share_fcons_quartile##i.female $g_controls, robust
	outreg2 using heterogenouseffects.tex, append ctitle(Discrete approach)

