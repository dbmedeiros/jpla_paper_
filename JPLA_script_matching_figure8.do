* --------------------------------------------------------------
*	
*	Paper title: "	Partisan and Nonpartisan Models of Constitution-Making: 
*					A Comparative Case Study of Brazil and Chile	"
*
*	Published in Journal of Politics in Latin America (online first, 2025)
* 
*	Description: 	Script for the Figure 8: "Effect of the Non-Partisanship Treatment in Chile's Constituent Convention (Average Treatment Effect on the Treated)."
*
*						We applied Nearest Neighbor Matching (NNM) using Stata's teffects command and Kernel Matching (KM) via the kmatch package (Jann, 2017).
*
*						The script also includes diagnostics showing standardized differences between original and matched data using kmatch commands.
*
*						Our Kernel Matching estimator also selected bandwidths via cross-validation (CV). Following Galdo et al. (2007), we implemented weighted CV to give more influence to untreated observations located near the treated units. Weighting the CV criterion based on covariates (CV on X) did not improve precision or alter the results. We therefore report estimates based on the weighted CV only applied to the outcome-regression cross-validation criterion, i.e., the CV with respect to Y. 
*
* --------------------------------------------------------------

*******************
*
* 	Setting graph scheme and describing variables
*
*******************

* Using the cleaned and organized dataset in .dta format
use "C:\[...]\1_wnominate_matching.dta"

* Setting graph scheme to grey tones (sj)
set scheme sj
grstyle init
grstyle color background white 
grstyle numstyle legend_cols 4
grstyle set legend, nobox

*** Describing indep. var. (IV)
///// IV Age (continuous): aka edad
sum edad
///// IV Sex (dummy, 1 = Female): aka sexo
tab sexo
///// IV Coalitions (categorical, being 0 to 6 pacts, being the baseline "Chile Vamos" Coalition = 0): aka pactos
tab pactos
///// IV Coalition constituent change (dummy, 1 = yes changed coalition at some point): aka cambio_pacto
tab cambio_pacto
///// Etnicity (dummy, 1 = Chilean natives): aka pueblo_originario
tab pueblo_originario
///// District magnitude (continuous, being the value of constituent spots at CC election and CC assembly seats): aka magdistrito  
sum magdistrito

*** Describing dep. var. (DV)
sum wnom

*** Describing Control (C) and Treatment (T) Groups
* Nonpartisan Independents (T, being 0), Partisan Independents (C1, being 1), and Party-affiliated (C2, being 2)
tab inp_ip_mp
 
* 1st Matching Exercise: Partisan Independents (C1) vs Nonpartisan Independents (T)
tab ip_vs_ind

* 1st Matching Exercise: Party-affiliated (C2) vs Nonpartisan Independents (T)
tab party_vs_ind

*************************************************
*
*********		MATCHING EXERCISES		*********	
*
*************************************************

//// //// //// //// //// //// //// //// //// //// //// ////  ////  //// 
////
////		Partisan Independents (C) vs Nonpartisan Independents (T)
////
////		Nearest Neighbor Matching (NNM)	
////
//// //// //// //// //// //// //// //// //// //// //// ////  ////  //// 


*******		Nearest Neighbor Matching (NNM) for Partisan Independents (C) vs Nonpartisan Independents (T)	*******

///// One Neighbor: NNM=1, ATT (Average Treatment Effect on the Treated = atet)
teffects nnmatch	(wnom	edad 	i.sexo		i.pactos	i.cambio_pacto	i.pueblo_originario		magdistrito) ///
																											(ip_vs_ind),	atet nn(1)
	estimates store ip_vs_ind_nn1

/////  Diagnostics std. difference between matched and original: NNM=1, ATT
kmatch md 	ip_vs_ind 					edad 	i.sexo	 i.pactos	 i.cambio_pacto	 i.pueblo_originario  	magdistrito 	(wnom),	att nn(1)
							kmatch csum
							mat M = r(M)
							coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
		title("NNM=1", 			size(medium)) byopts(xrescale) legend(off)  ///
		name(diff1a, replace)

///// Three Neighbors: NNM=3, ATT
teffects nnmatch	(wnom	edad	i.sexo	i.pactos	i.cambio_pact	 i.pueblo_originario	magdistrito) ///
																													(ip_vs_ind),	atet nn(3)
	estimates store 			ip_vs_ind_nn3

/////  Diagnostics std. difference between matched and original: NNM=3, ATT
kmatch md 	ip_vs_ind 					edad 	i.sexo	 i.pactos 	i.cambio_pacto	 i.pueblo_originario 	 magdistrito (wnom), 			att nn(3)
							kmatch csum
							mat M = r(M)
							coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
		title("NNM=3", 			size(medium)) byopts(xrescale) legend(off)  ///
		name(diff1b, replace)

///// Five Neighbors: NNM=5, ATT
teffects nnmatch	 	(wnom		edad 	i.sexo 		i.pactos 	i.cambio_pacto 		i.pueblo_originario 		magdistrito) ///
																													(ip_vs_ind), 	atet nn(5)
	estimates store 			ip_vs_ind_nn5

/////  Diagnostics std. difference between matched and original: NNM=5, ATT
kmatch md 	ip_vs_ind 					edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario  	magdistrito (wnom), 				att nn(5)
							kmatch csum
							mat M = r(M)
							coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
		title("NNM=5", 			size(medium)) byopts(xrescale) legend(off)  ///
		name(diff1c, replace)
							
* Merging diagnostics NNM summary graphs (with 1, 3 and 5 neighbors)
graph 	combine diff1a diff1b diff1c, 		xcommon cols(3) name(diff1, replace)  ///
		title("Diff. between original and matched (ATT): Partisan-Ind. vs Nonpartisan Independents", size(medsmall))
			graph export diff1.png, replace

			
//// //// //// //// //// //// //// //// //// //// //// ////  ////  //// 
////
////		Partisan Independents (C) vs Nonpartisan Independents (T)
////
////		Kernel Matching (KM): all bootstraped
////
//// //// //// //// //// //// //// //// //// //// //// ////  ////  //// 


*******		Kernel Matching (KM) for Partisan-Indep. (C) vs Nonpartisan Independents (T): KM Pair-Matching (PM)		*******
kmatch md 			ip_vs_ind 									edad	 i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario 	magdistrito ///
								(wnom), att bwidth(pm) vce(boot)
estimates store 	ip_vs_ind_pm

/////  Diagnostics std. difference between matched and original: KM, ATT: PM
					kmatch csum
							mat M = r(M)
								coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
		title("KM: Pair-Matching", 		size(small)) byopts(xrescale) legend(off)  ///
	name(diff1d, replace)
	
///// Kernel Bandwitch Selection (bwidth) for cross validation (cv) with respect to the means of X: bwidth(cv), ATT
kmatch md 			ip_vs_ind 										edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario 	magdistrito ///
									(wnom), att bwidth(cv) vce(boot)
estimates store 	ip_vs_ind_cv_x

/////  Diagnostics std. difference between matched and original: KM, ATT: CV  w/ respect to X
				kmatch csum
							mat M = r(M)
									coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
	title("KM: CV  w/ respect to X", 		size(small)) byopts(xrescale) legend(off)  ///
	name(diff1e, replace)

///// 	Kernel Bandwitch Selection (bwidth) for cross validation (cv) with respect to the means of Y, weighted: bwidth(cv wnom, weighted), ATT
kmatch md 			ip_vs_ind 														edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario  	magdistrito ///
									(wnom), att bwidth(cv wnom, weighted) vce(boot)
estimates store 	ip_vs_ind_cv_y

/////  Diagnostics std. difference between matched and original: ATT, KM: CV  w/ respect to Y (weighted)
							kmatch csum
										mat M = r(M)
											coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
	title("KM: CV w/ respect to Y (weighted)", 	size(small)) byopts(xrescale) legend(off)  ///
	name(diff1f, replace)
	
* Merging diagnostics KM summary graphs (with PM, CV w/ respect to X, and CV w/ respect to Y weighted)
graph combine 	diff1d 	diff1e 	diff1f, 	xcommon cols(3) name(diff1_1, replace) ///
											title("Diff. original and matched (ATT): Partisan Ind. vs Nonpartisan Independents", size(medium))
		graph export diff1_1.png, replace


* Coefplot graph of NNM and KM regression outcomes of Partisan-Indep. (C) vs Nonpartisan Independents (T)
coefplot 		ip_vs_ind_nn1 	ip_vs_ind_nn3 	ip_vs_ind_nn5  /// 
				ip_vs_ind_pm 	ip_vs_ind_cv_x 	ip_vs_ind_cv_y, ///
							xline(0, lcolor(black) lpattern(solid)) ///
plotlabels ///
	(	"NNM=1"  ///
		"NNM=3"  ///
		"NNM=5"  ///
			"KM: Pair-Matching" ///
			"KM: CV  w/ respect to X" ///
			"KM: CV w/ respect to Y (weighted)" ) ///
		 ysc(off) legend(off) ///
								title("Partisan-Indep. (C) vs Nonpartisan Indep. (T)", size(med)) name(ip_ind_outs, replace)
	graph export ip_ind_outs.png, replace


//// //// //// //// //// //// //// //// //// //// //// //// 
////
////		Partisans (C) vs Partyless Independents (T)
////
////		Nearest Neighbor Matching (NNM)	
////
//// //// //// //// //// //// //// //// //// //// //// //// 

*******		Nearest Neighbor Matching (NNM) for Party-affiliated one, aka "party" (C) and Nonpartisan Independents, aka "ind" (T): 
******* 	NNM: party_vs_ind

///// One Neighbor: NNM=1, ATT (Average Treatment Effect on the Treated = atet)
teffects nnmatch	 	(wnom									edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario 	magdistrito) ///
								(party_vs_ind),		atet nn(1)
	
	estimates store party_vs_ind_nn1


/////  Diagnostics std. difference between matched and original: NNM=1, ATT
kmatch md 		party_vs_ind 						edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario  	magdistrito ///
								(wnom),	att nn(1)
													kmatch csum
													mat M = r(M)
													coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
		title("NNM=1", 			size(medium)) byopts(xrescale) legend(off)  ///
		name(diff2a, replace)

///// Three Neighbors: NNM=3, ATT
teffects nnmatch	 	(wnom									edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario 	magdistrito) ///
									(party_vs_ind), atet nn(3)
	estimates store party_vs_ind_nn3

/////  Diagnostics std. difference between matched and original: ATT, NNM=3
kmatch md 		party_vs_ind 						edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario  magdistrito /// 
								(wnom),	att nn(3)
													kmatch csum
													mat M = r(M)
													coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
		title("NNM=3", 			size(medium)) byopts(xrescale) legend(off)  ///
		name(diff2b, replace)

///// Five Neighbors: NNM=5, ATT
teffects nnmatch	 	(wnom								edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario 	magdistrito) ///
								(party_vs_ind),	atet nn(5)
	estimates store 	party_vs_ind_nn5

/////  Diagnostics std. difference between matched and original: ATT, NNM=5
kmatch md 			party_vs_ind 						edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario  	magdistrito ///
									(wnom), att nn(5)
														kmatch csum
														mat M = r(M)
														coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
	title("NNM=5", 			size(medium)) byopts(xrescale) legend(off)  ///
	name(diff2c, replace)
							
* Merging diagnostics NNM summary graphs (with 1, 3 and 5 neighbors)
graph combine 	diff2a 	diff2b 	diff2c, 	xcommon cols(3) name(diff1, replace)  ///
											title("Diff. original and matched (ATT): Party-affiliated vs Nonpartisan Independents", size(medium))
		graph export diff2.png, replace

* Robusteness checks with Kernel Matching (KM) for Partisan-Independents (C) and Nonpartisan Independents (T): KM of ip_vs_ind
///// Pair-Matching (PM)
kmatch md 			party_vs_ind 										edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario  	magdistrito  ///
									(wnom), att bwidth(pm) vce(boot)
estimates store 	party_vs_ind_pm

/////  Diagnostics std. difference between matched and original: ATT, KM: PM
							kmatch csum
										mat M = r(M)
										coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
	title("KM: Pair-Matching", 			size(small)) byopts(xrescale) legend(off)  ///
	name(diff2d, replace)
	
///// Kernel Bandwitch Selection (bwidth) for cross validation (cv) with respect to the means of X: ATT
kmatch md 				party_vs_ind 										edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario  	magdistrito ///
										(wnom), att bwidth(cv) vce(boot)
estimates store 		party_vs_ind_cv_x

/////  Diagnostics std. difference between matched and original: ATT, KM: CV  w/ respect to X
							kmatch csum
										mat M = r(M)
										coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
	title("KM: CV  w/ respect to X", 	size(small)) byopts(xrescale) legend(off)  ///
	name(diff2e, replace)

///// Kernel Bandwitch Selection (bwidth) for cross validation (cv) with respect to the means of Y, weighted (Galdo et. al. 2008): ATT
kmatch md 				party_vs_ind 													edad 	i.sexo 	i.pactos 	i.cambio_pacto 	i.pueblo_originario 	magdistrito ///
										(wnom), att bwidth(cv wnom, weighted) vce(boot)
estimates store 		party_vs_ind_cv_y

/////  Diagnostics std. difference between matched and original: ATT, KM: CV  w/ respect to Y (weighted)
							kmatch csum
												mat M = r(M)
												coefplot matrix(M[,4]), noci nolabels xline(0, lcolor(gs8) lpattern(solid)) ///
	title("KM: CV w/ respect to Y (weighted)", 	size(small)) byopts(xrescale) legend(off)  ///
	name(diff2f, replace)
	
* Merging diagnostics KM summary graphs (with PM, CV w/ respect to X, and CV w/ respect to Y weighted)
graph combine 		diff2d 	diff2e 	diff2f, 	xcommon cols(3) name(diff2_2, replace) ///
												title("Diff. original and matched (ATT): Party-affiliated vs Nonpartisan Independents", size(medium))
graph export diff2_2.png, replace


* Coefplot graph of NNM and KM regression outcomes of Party-affiliated (C) vs Nonpartisan Independents (T)
coefplot 			party_vs_ind_nn1 	party_vs_ind_nn3 	party_vs_ind_nn5  /// 
					party_vs_ind_pm 	party_vs_ind_cv_x 	party_vs_ind_cv_y, ///
																					xline(0, lcolor(black) lpattern(solid))  ///
	plotlabels ///
		(	"NNM=1"  ///
			"NNM=3"  ///
			"NNM=5"  ///
				"KM: Pair-Matching" ///
				"KM: CV  w/ respect to X" ///
				"KM: CV w/ respect to Y (weighted)" ) ///
															ysc(off) legend(cols(2) pos(6) colfirst colgap(*3) size(small)) ///
															title("Party-affiliated (C) vs Nonpartisan Indep. (T)", size(med)) name(party_ind_outs, replace)
graph export party_ind_outs.png, replace

* Final graph merging coefficients of controls and treatment groups (KM and NNM matchings): Figure 8
grc1leg 		ip_ind_outs 		party_ind_outs, 		xcommon cols(2) legendfrom(party_ind_outs) name(nnm_km, replace)
															graph export nnm_km.png, 	replace

*******************
*
* 	Script end
*
*******************



