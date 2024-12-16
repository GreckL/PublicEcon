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
	cd "C:\Users\tomma\OneDrive\Documents\Graduate 2.1\Public Economics\Group Project\Stata Task"
	local direc : pwd
	
	// Importing the necessary dataset
	use municipalities5k_2000_2015.dta, clear
	
	// Installing the necessary packages
	ssc install ftools
	ssc install rdrobust
	ssc install rddensity
/*----------------------
Exercise 1 
-----------------------*/

	// Describing the data
	des
	sort female
	by female: tabstat lptot_total lp_e_tax_fee lp_e_other, stats(mean, sd, n, min, max)
	
	// Generating the output table 
	dtable lptot_total lpcor_total lpcap_total lp_e_tax_fee lp_e_other, by(female, nototals tests) export(table1.tex, replace) sample(, statistics(freq) place(seplabels)) sformat("(N=%s)" frequency) note(Total sample: N = 5,526)  column(by(hide))

	// Commenting about causality of differences
	* We observe some differences as expected but we cannot interpret this as causal as there exist many confounders. That is, covariates could drive outcomes of a naive test. One example would be the share of employed within a city. High employment could be related to the willigness to test a women as a major while also being positivly correlated with revenues / overall health of public finance. 

/*----------------------
Exercise 2
-----------------------*/
	// Regressing log per capita total expenditures on the female dummy
	reg lptot_total female 

	// Creating a global for controls
	global g_controls =  "sh_empl sh_hschool hs_mayor sh_young_old logpop i.year share_fcons"
 
	// Running the same regression controlling for covariates
	reg lptot_total female $g_controls, vce(hc2)
	* Tom: I have a doubt for this: shouldn't the variable year be included as i.year (like a categorical variable)? For now I changed it to i.year, let me know if you agree. Otherwise we are taking years as actual numbers. Also, can we include year in a fixed effect model?

	// Explaining the choice of control covariates
	*1. sh_emply: see point 1;
	*2. sh_hschool: Education drives election outcomes and potentially state of public finance e.g. by not providing enough checks on political leaders
	*3. hs_mayor: Women might be differently educated, education drives political decisions, we want to isolate the effect of gender
	*4. sh_young_old: Dependent groups increase expenditure, might be correlated to voting for women
	*5. logpop: Bigger cities might have a more heterogeneous political scene and thus are more likely to vote for female mayors, while at the same time clearly influencing expenditure.
	*6. year: We might expect an increase in expenditure over time, while also growing more liberal towards female representation.
	*7. share_fcons: Females at any level might have the effect and if councillors are already female we expect a more liberal city, so we have to control for this as we are only want the effect of female mayor.

	// Running the same regression with municipality FE
	reghdfe lptot_total female $g_controls, absorb(n_istat) vce(cluster n_istat)
	
	// Using xtreg and xtreg
	xtset n_istat year
	xtreg lptot_total female $g_controls, fe
	**xtreg: should we use xtreg?
	* Tom: we get the same result with xtreg and reghdfe

	// Under which conditions can these each of these estimates be interpreted as causal: 
	* Regression 1: the gender of the mayor must be uncorrelated with any factor that might also influence total expenditures per capita. Due to reasons highlighted in Exercise 1, this is likely not the case. 
	* Regression 2: the included covariates are the only factors that influence gender of the mayor and total expenditures per capita. In case there were other omitted covariates that have an effect on both, the estimate could not be considered as causal. 
	* Regression 3: in this case the remaining unobserved variables not included in the model correlated with both expenditures and probability of having a female mayor should be constant over time so that they can be captured by the municipality fixed effects. 
 
 
/*----------------------
Exercise 3
-----------------------*/
	// Discussing identifying assumptions
	* 1. Continuity of potential outcomes: in absence of the treatment, there would be no jump in potential outcomes at the cutoff. In this specific instance, this would entail that other than the difference in a female candidate winning elections, there is no difference in covariates at the threshold for municipalities just above and below the threshold, meaning that the two groups are comparable. 
	*2. No manipulation around cutoff: there is no evidence of bunching on either side of the cutoff, that is there is no jump in density of municipalities with a margin just above 0 or just below 0. 
	*3. Running variable affects treatment: a female victory margin greater than 0 entails the election of a female mayor (as in this case we have a sharp RDD);

	// Providing evidence that the design is sharp RDD
	rdplot female mv, graph_options(xtitle("Margin of female victory") ytitle("Female mayor") title("Regression discontinuity plot"))
	* The treatment status (having a female mayor) appears to be deterministically defined by the running variable (female margin of victory). Hence, the design appears to be sharp. 


/*----------------------
Exercise 4
-----------------------*/


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

	**Somehow, most of those seem to be showing manipulation (they are significant)



/*----------------------
Exercise 5
-----------------------*/
	// Outlining why it is important to verify the continuity of the running variable around the threshold
	*The continuity of the running variable is a core assumption. Else, we would have some evidence for manipulation around the cutoff. The sorting of observations around the cutoff would not be random. The population at the two sides of the cutoff could not be compared as there might be some hidden difference between them that entail one of the two groups opting to sort itself on either side of the cutoff 0 of margin of female victory.  

	// Generating a graph of the density of the running variable 
	rddensity mv, plot

	// Checking for statistically significant discontinuities at the cutoff in the density of the running variable
	// Manipulation test of running variable using default triangular kernel
	rddensity mv, all
	// Using other kernel types
	rddensity mv, all kernel(uniform)
	rddensity mv, all kernel(epanechnikov) 
	* All three methods give the same result.
	
	// Commenting on the presence of statistically significant discontinuity
	* Employing robust methods and considering the obtained p-values higher than 0.05 utilizing all 3 types of kernels, there is not statistically significant discontinuity. The answer, however, would be different when employing conventional methods. 
	* Tom: do you guys agree with this comment?

/*----------------------
Exercise 6
-----------------------*/

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





/*----------------------
Exercise 7
-----------------------*/

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




/*----------------------
Exercise 8
-----------------------*/

	// Generating quartiles of the share_fcons
	xtile share_fcons_quartile=share_fcons,n(4)
	* Generates a numerical variable 
	
	// Estimating the regression with continuous version of share_fcons
	rdrobust lptot_total mv share_fcons*female, p(2) c(0)
	
	* Tom: I think what he wants us to do is to put i.share_fcons_quartile#i.female in the RDD regression. Maybe something like this?: 
	rdrobust lptot_total mv i.share_fcons_quartile#i.female, p(2) c(0)
	



	
