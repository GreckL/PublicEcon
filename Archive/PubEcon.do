*****Public Economics

/*
 Rules Reply to each question providing the output of your analyses, together with a brief comment. Please provide also the do file used to produce your results. Send your answers and code in a single pdf file to salvatore.lattanzio@unibocconi.it by midnight of 17 December 2024. The assignment is worth 3 points. Data Thedatacalledmunicipalities5k 2000 2015.dta is a panel dataset where units of observations are Italian municipalities over time (years). Only municipalities with more than 5,000 residents are included. You can download the dataset from Blackboard.
 */
*Use this data to study the relationship between mayor's gender and municipalities'balance sheets.

clear all
use "municipalities5k_2000_2015.dta", clear


*Getting packages:
/*
ssc install ftools
ssc install rdrobust
ssc install rddensity
*/

***Q1 Provide descriptive statistics (means, standard deviations, observations) for log per capita expenditures and revenues. Do you observe differences between municipalities run by male mayors and by female mayors? Explain why such differences cannot be interpreted as causal. [0.10 points]
des
sort female
by female: tabstat lptot_total lp_e_tax_fee lp_e_other, stats(mean, sd, n)

*Answer: We observe some differences as expected but we cannot interpret this as causal as there exist many confounders. That means covariates could drive or hide outcomes of a naive test. One example would be share of employed within a city. High employment could be related to the willigness to test a women as a major while also being positivly correlated with revenues / overall health of public finance. 


***Q2 Run an OLS regression of log per capita total expenditures on the female dummy. Then, estimate a regression controlling for covariates (choose which ones to include and justify why). Finally, estimate a regression with municipality fixed effects (keep controlling for the relevant time-varying covariates). Under what conditions can each of these estimates be interpreted as causal? [0.35 points]
 
reg lptot_total female // OLS regression

global g_controls =  "sh_empl sh_hschool hs_mayor sh_young_old logpop year share_fcons"
 
reg lptot_total female $g_controls, vce(hc2) // OLS regrssion controlling for selected covariates


*Used covariates:
*1. sh_emply: As said in a)
*2. sh_hschool: Education drives election outcomes and potentially state of public finance e.g. by not providing enough checks on political leaders
*3. hs_mayor: Women might be differently educated, education drives political decisions, we want to isolate the effect of gender
*4. sh_young_old: Dependent groups increase expenditure, might be correlated to voting for women
*5. logpop: Bigger cities might have a more heterogeneous political scene and thus are more likely to vote for female mayors, while at the same time clearly influencing expenditure
*6. year: We might expect an increase in expenditure over time, while also growing more liberal towards female representation
*7. share_fcons: Females at any level might have the effect and if councillors are already female we expect a more liberal city, so we have to control for this as we are only want the effect of female mayor

*Not used: 
*1. sh_illiteracy as correlated with sh_hschool with -0.7375
*2. population: Perfect collinearity with logpop
*3. lp_e_tax_fee, lp_e_other
*4.

reghdfe lptot_total female $g_controls, absorb(n_istat) vce(cluster n_istat)

* reghdfe more efficient than xtreg with greater number of observations. 
/*xtset n_istat
xtreg lptot_total female $g_controls, fe*/


**The first one cannot really be considered causal, it is to simplistic and simply returns the correlation between the two variables. The second one can still face a number of issues even if selection bias is controlled for: There still might exist omitted variables (i.e. mayors innate political ability or motivation) or reverse causality, which the fixed effects tries to remedy. 
*!!!!Can improve this answer from the slides!!!!
 
 
***Q3 What is (are) the identifying assumption(s) for the female margin of victory to be used as a running variable in a regression discontinuity design? Provide evidence that the appropriate design is sharp RDD in this case. [0.10 points]
 
 *1. No manipulation around cutoff at the Cutoff (No change in density (bunching or similar))
 *2. Running variable affects treatment (0.5 vote share does lead to female mayor)
 *3. Balanced covariates -> No other important variable is significantly altered while not able to be controlled for

**Graph check for sharp RDD
rdplot female mv 
*Seems good, treatment is sharp


***Q4 Check discontinuities in baseline covariates at the threshold of 0 for the female margin of victory. Are there any significant discontinuities? (Hint: estimate RDDs for each covariate to evaluate the statistical significance of the discontinuity). [0.20 points]

** RD plots for all covariates
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
	rdplot `var' mv, graph_options(ytitle("`varlabel'") title("`plot_title'") legend(off)) name(g_`var', replace) hide
    graph save g_`var', replace
    local rd_graphs "`rd_graphs' g_`var'.gph"
}

graph combine `rd_graphs', altshrink


graph combine `rd_graphs', col(3) iscale(0.5) ///
    ysize(6) xsize(8) ///
    title("Discontinuities in baseline covariates") ///
    saving(BaselineCovariatesDiscontinuityCheck, replace)


----

ds lp_e_tax_fee lp_e_other share_fcons logpop sh_hschool sh_illiteracy sh_empl sh_young_old hs_mayor north south right left litor population pop_dens
global g_covariates `r(varlist)'

matrix define R=J(16,6,.)
matrix rownames R = lp_e_tax_fee lp_e_other share_fcons logpop sh_hschool sh_illiteracy sh_empl sh_young_old hs_mayor north south right left litor population pop_dens
matrix colnames R = CIp1_L convp1RD CIp1_R CIp2_L convRDp2 CIp2_R
matrix list R


*Loop through each covariate and run RDD, use p(1) and p(2) -> Use robust CI
global g_r = 1 
foreach var of global g_covariates {
    rdrobust `var' mv, p(1) q(2) c(0)
    matrix R[$g_r ,1] = e(ci_l_cl)
	matrix R[$g_r ,2] = e(tau_cl)
	matrix R[$g_r ,3] = e(ci_r_cl)
	global g_r = $g_r +1
}
global g_r = 1 
foreach var of global g_covariates {	
    rdrobust `var' mv, p(2) q(3) c(0)
    matrix R[$g_r ,4] = e(ci_l_cl)
	matrix R[$g_r ,5] = e(tau_cl)
	matrix R[$g_r ,6] = e(ci_r_cl)
	global g_r = $g_r +1
}
matrix list R	
***This is obviously wrong

**Somehow, most of those seem to be showing manipulation



***Q5 Outline why it is important to verify the continuity of the running variable around the threshold, and explain what it means in this specific context. Show a graph with the density of the running variable. Is there any statistically significant discontinuity? [0.25 points]

*The continuity of the running variable is a core assumption. Else, we would have some evidence for manipulation around the cutoff. The sorting around the cutoff would then not be random. The population left and right of the cutoff could not be compared as there might be some hidden, but seemingly significant, difference between them. 

*Graph
rddensity mv, plot

**Manipulation test of running variable using default triangular kernel
rddensity mv, all
*Other kernels same result
rddensity mv, all kernel(uniform)
rddensity mv, all kernel(epanechnikov) 

**Under Conventional test statistic (not valid when using MSE-optimal bandwidth choice) we reject the null of no manipulation -> !!!! Is this valid here?
**We are fine until the window length / 2 of 0.01 or 0.013



***Q6 Show the RD plots and estimates of the effect of female mayors on log per capita total expenditures using the following approaches:

*Parametric local linear regression with optimal bandwidth (write down the estimated equation)

rdrobust lptot_total mv, p(1) c(0)
local bwl = e(h_l)
local bwr = e(h_r) 
rdplot lptot_total mv if (mv<=`bwr' & mv >= -`bwl'), c(0) p(1) kernel(tri) nbins(20 20) 


*Parametric regression on the full sample with 2nd order polynomial fit on both sides of the threshold (write down the estimated equation)




*Non-parametric local linear regression with optimal bandwidth. 

rdrobust lptot_total mv, p(1) c(0)
local bwl = e(h_l)
local bwr = e(h_r) 
rdplot lptot_total mv if (mv<=`bwr' & mv >= -`bwl'), c(0) p(1) kernel(tri) nbins(20 20) 


*Extra: Here also with p(2)
rdrobust lptot_total mv, p(2) c(0)
local bwl = e(h_l)
local bwr = e(h_r) 
rdplot lptot_total mv if (mv<=`bwr' & mv >= -`bwl'), c(0) p(2) kernel(tri) nbins(20 20) 


*What do you conclude? (Hint: use the features of the rdrobust command). [0.75 points]






***Q7 Show how the estimates vary at different bandwidths (e.g., from 2 to 30%). Report a graph or a table with each estimate together with 95% confidence intervals. [0.50 points]

*Sensitivity to bandwidth
gen FemWon = female*mv
matrix coef=J(30,4,.)
mat colnames coef=bw b ul ll
mat rownames coef= 1% 2% 3% 4% 5% 6% 7% 8% 9% 10% 11% 12% 13% 14% 15% 16% 17% 18% 19% 20% 21% 22% 23% 24% 25% 26% 27% 28% 29% 30%
local i=1
*Estimate regressions for different bandwidths (between 2% and 30%)
forvalues h=0.01(0.01)0.31 {
	reg lptot_total female mv FemWon if abs(mv)<`h', cluster(n_istat)
	mat coef[`i',1]=`h'
	mat coef[`i',2]=_b[female]
	mat coef[`i',3]=_b[female]+1.96*_se[female]
	mat coef[`i',4]=_b[female]-1.96*_se[female]
	local i=`i'+1
}
**Is there only a significant effect for bw 3%?

**Number of obs per h
forvalues h = 0.01(0.01)0.3 {
    count if abs(mv) < `h'
    di "Bandwidth `h': `r(N)' observations"
}
**Table
matrix list coef




***Q8 Gagliarducci and Paserman (2012) suggest that female mayors' policy-making is influenced by the share of women in the municipal council. Report heterogeneous effects using the variable share_fcons as a continuous variable and as a discrete variable (Hint: generate quartiles of the share and interact the female dummy with them as factor variables; see command xtile). [0.75 points]








