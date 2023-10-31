****Priject: Social engagement
****Author:  Siyun Peng
****Date:    2023/10/10
****Version: 17
****Purpose: Data Analysis

cd "C:\Users\peng_admin\Dropbox\peng\Academia\Work with Brea\SNAD\SNAD data\Peng\Social Engagement"
use "C:\Users\peng_admin\OneDrive - Indiana University\SNAD SHARED FOLDER\PUBLIC\SNAD Data\Cleaned data\SNAD-SNACK Merged Data\Short_merged_04-26-2023.dta",clear
keep if wave==1

***************************************************************
**#1 Data clean
***************************************************************


fre contacts_phone
clonevar phone=contacts_phone 
replace phone="105" if contacts_phone=="a little over 100"
replace phone="" if contacts_phone=="no cell phone and no contacts on home phone"
replace phone="" if contacts_phone=="unsure/did not have phone"
replace phone="" if contacts_phone=="unsure"
replace phone="250" if contacts_phone=="200 plus"
replace phone="2000" if contacts_phone=="2,000"
replace phone="1252" if contacts_phone=="1,252"
replace phone="" if contacts_phone=="unsure due to not having smartphone "
replace phone="18" if contacts_phone=="15-20"
destring phone, replace
lab var phone "Number of contacts in phone"
fre phone
fre contacts_phone if missing(phone)

gen phone_log=log(phone) //log transformation to correct left skewed distribution
lab var phone_log "Number of contacts in phone (logged)"

/*
clonevar phonetrunc=phone
recode phonetrunc (500/max=500)
lab var phonetrunc "Number of contacts in phone, truncated to correct skew"
*/

*hhinc
gen hhinc=annual_household
recode hhinc (1=5)(2=12.5)(3=20)(4=30)(5=42.5)(6=62.5)(7=87.5)(8=125)(9=.)
lab var hhinc "Household income in thousands of dollars"

*Health
gen badvision=vision
gen badhear=hearing
recode badvision badhear (2=0)

*Life events
fre life1 life2 life3 life4 life5 life6
clonevar life1r=life1
clonevar life2r=life2
clonevar life3r=life3
clonevar life4r=life4
recode life1r-life4r (2=0)
egen socloss=rowtotal(life1r-life4r)
lab var socloss "Number of social losses in past 12 months"

gen anyloss=socloss
recode anyloss (1/max=1)
fre anyloss
lab var anyloss "Experienced social loss in past 12 months"

*QOL
fre family marriage friends 

*Lifestyle cognitive engagement
fre cardgame_f cardgame_d museum_f museum_d famfriends_f famfriends_d visitors_f visitors_d
recode cardgame_f museum_f famfriends_f visitors_f (0=.) (2=4)(3=36)(4=156)(5=300)(6=0)
egen lcesocial=rowtotal(cardgame_f museum_f famfriends_f visitors_f),mi
lab var lcesocial "Social components of lifestyle cognitive engagement (days per year)"

*Social support
fre family_help1 help_support1 friends_help1 go_wrong1 problems_family1 joys_sorrows1 make_decisions1
egen famsup=rowmean(family_help1 help_support1 problems_family1 make_decisions1)
egen frndsup=rowmean(friends_help1 go_wrong1 joys_sorrows1)
lab var famsup "Family social support scale"
lab var frndsup "Friends social support scale"

*Social media
gen twitter=social_media1___1
gen insta=social_media1___2
gen pinterest=social_media1___3
gen facebook=social_media1___4
gen linkedin=social_media1___5
gen snap=social_media1___6
gen whatsapp=social_media1___7
gen reddit=social_media1___8
gen skype=social_media1___10



*recode sm_twitter1 sm_instagram1 sm_pinterest1 sm_facebook1 sm_linkedin1 sm_snapchat1 sm_whatsapp1 sm_reddit1 sm_skype1 (1=90)(2=30)(3=12)(4=1)

*replace twitter=twitter*sm_twitter1 if twitter==1
*replace pinterest=pinterest*sm_pinterest1 if pinterest==1
*replace insta=insta*sm_instagram1 if insta==1
*replace linkedin=linkedin*sm_linkedin1 if linkedin==1
*replace snap=snap*sm_snapchat1 if snap==1
*replace whatsapp=whatsapp*sm_whatsapp1 if whatsapp==1
*replace reddit=reddit*sm_reddit1 if reddit==1
*replace facebook=facebook*sm_facebook1 if facebook==1

egen socmedia=rowtotal(twitter pinterest insta linkedin snap whatsapp reddit facebook),mi

*lab var socmedia "Social media activity - number of times per month"
lab var socmedia "Number of social media sites used"

*Religious participation
fre religious_attendance1
gen church=religious_attendance1
recode church (1=52)(2=18)(3=4)(4=2)(5=0)
lab var church "Religious attendance - number of times per year"

gen freqchurch=church
recode freqchurch (16/max=1)(min/6=0)
lab var freqchurch "Attends church several times a month or more"

*Housing
clonevar grphous=group_housing1
fre living_number

gen livalone=living_number
recode livalone (1/max=1)
lab var livalone "Currently lives alone"


*Social integration

bysort time: fre integration1 integration2 integration3 belong_community1 close_community1 comfort_community1
gen int1=integration1
replace int1=belong_community1 if integration1==.
gen int2=integration2
replace int2=close_community1 if integration2==.
gen int3=integration3
replace int3=comfort_community1 if integration3==.
recode int2 int3 (1=7)(2=6)(3=5)(4=4)(5=3)(6=2)(7=1)
egen integration=rowmean(int1 int2 int3)
lab var integration "Community integration scale"


*Volunteering
fre volunteer-volunteer_change3

fre volunteer volunteer_help

recode volunteer_help (4=.) 

gen volorg=0
replace volorg=1 if volunteer==1 
replace volorg=. if volunteer==.
lab var volorg "Any volunteering in past year"

gen volany=0
replace volany=1 if volunteer==1 | volunteer_help==1
replace volany=. if volunteer==. & volunteer_help==.
lab var volany "Any volunteering in past year"

gen volreg=0
replace volreg=1 if volunteer_often<3
replace volreg=. if volany==.
lab var volreg "Volunteers once or twice a month or more"

gen volpeople=volreg
replace volpeople=0 if volunteer_company<3
lab var volpeople "Volunteers regularly with other people"

*Employment
fre employment
gen working=employment
recode working (1/2=1)(3/9=0)
lab var working "Currently working full or part-time"

*Social participation
egen socpartic=rowtotal(church volorg working),mi
lab var socpartic "Social participation (church, volunteering, work)"


*Caregiving
fre assisting_adult
clonevar caregiver=assisting_adult

*Marriage
fre married

*Kids
recode kids (1/max=1),gen(parenthood)

*Neighborhood segregation
gen whiteneigh=neighbor_white_red
recode whiteneigh (6=1)(1/5=0)
lab var whiteneigh "Lived in majority white neighborhood most of life"

gen segregation=whiteneigh
recode segregation (1=0)(0=1) if white==0
lab var segregation "Lives in neighborhood majority own race"

*lonely
drop lonely
egen lonely=rowmean(lack_companionship1 left_out1 feel_isolated1)


*Controls
replace white=0 if race_string=="Asian" 
replace white=0 if race_string=="Black" 
replace white=0 if race_string=="Mixed" 
replace white=0 if race_string=="Other"
replace white=1 if race_string=="White" 

*Diagnosis
gen dx=diagnosis_iadc
replace dx=diagnosis_moca if diagnosis_iadc==.
lab var dx "Diagnosis (consensus if available, MOCA derived if missing)"







***************************************************************
**#2 Data analysis
***************************************************************

*Controls
sum age edu white female 

*Cognition
foreach x of varlist moca_raw epmem execfxn kids phone_log lcesocial socmedia church integration lonely {
	egen `x'_std =std(`x') 
}
sum moca_raw_std epmem_std execfxn_std 
label var moca_raw_std "Global cognition"
label var epmem_std "Episodic memory"
label var execfxn_std "Executive function"

*Social roles
sum married kids_std working caregiver veteran 
label var married "Married"
label var kids_std "# kids"
label var working "Employed"
label var caregiver "Caregiver"
label var veteran "Veteran"

*Social engagement
sum phone_log_std lcesocial_std socmedia_std church_std volany
label var phone_log_std "# phone contacts"
label var lcesocial_std "Lifestyle engagements"
label var socmedia_std "# social media presence"
label var church_std "Church frequency"
label var volany "Volunteering"

*community and housing
sum livalone integration_std lonely
label var livalone "living alone"
label var integration_std "Community integration"
label var lonely_std "Loneliness"

*What determines bridging?
asdoc pwcorr bridging age edu white female moca_raw_std epmem_std execfxn_std married kids_std working caregiver veteran phone_log_std lcesocial_std socmedia_std church_std volany livalone integration_std, sig save(Correlation.doc) replace

*regression
eststo clear
foreach x of varlist moca_raw_std epmem_std execfxn_std married kids_std working caregiver veteran phone_log_std lcesocial_std socmedia_std church_std volany livalone integration_std {
	eststo `x': reg bridging age female white i.edu source_study `x',vce(robust) 
}
esttab * using "reg.csv", replace b(%5.3f) se(%5.3f) nogap compress nonum 

*coefplot
eststo clear
foreach x of varlist age edu white female moca_raw_std epmem_std execfxn_std married kids_std working caregiver veteran phone_log_std lcesocial_std socmedia_std church_std volany livalone integration_std lonely_std {
	reg bridging source_study `x',vce(robust)
	est store `x'_base
}


foreach x of varlist moca_raw_std epmem_std execfxn_std married kids_std working caregiver veteran phone_log_std lcesocial_std socmedia_std church_std volany livalone integration_std lonely_std {
	reg bridging source_study age female white i.edu `x',vce(robust) 
	est store `x'
}
coefplot (moca_raw_std_base, pstyle(p1) label(Baseline)) (moca_raw_std, pstyle(p2) label(Full model) offset(0.15)) (epmem_std_base, pstyle(p1)) (epmem_std, pstyle(p2) offset(0.15)) (execfxn_std_base, pstyle(p1)) (execfxn_std, pstyle(p2) offset(0.15)) (married_base, pstyle(p1)) (married, pstyle(p2) offset(0.15)) (kids_std_base, pstyle(p1)) (kids_std, pstyle(p2) offset(0.15)) (working_base, pstyle(p1)) (working, pstyle(p2) offset(0.15)) (caregiver_base, pstyle(p1)) (caregiver, pstyle(p2) offset(0.15)) (veteran_base, pstyle(p1)) (veteran, pstyle(p2) offset(0.15)) (phone_log_std_base, pstyle(p1)) (phone_log_std, pstyle(p2) offset(0.15)) (lcesocial_std_base, pstyle(p1)) (lcesocial_std, pstyle(p2) offset(0.15)) (socmedia_std_base, pstyle(p1)) (socmedia_std, pstyle(p2) offset(0.15)) (church_std_base, pstyle(p1)) (church_std, pstyle(p2) offset(0.15)) (volany_base, pstyle(p1)) (volany, pstyle(p2) offset(0.15)) (livalone_base, pstyle(p1)) (livalone, pstyle(p2) offset(0.15)) (integration_std_base, pstyle(p1)) (integration_std, pstyle(p2) offset(0.15)) (lonely_std_base, pstyle(p1)) (lonely_std, pstyle(p2) offset(0.15)), yline(0) ytitle("Bridging social capital") drop(_cons source_study age female white *.edu) legend(position(3) ring(1) size(med)) xlabel(,labsize(med) angle(90)) vertical groups(moca_raw_std epmem_std execfxn_std = "{bf:Cognitive function}" married kids_std working caregiver veteran = "{bf:Social roles}" phone_log_std lcesocial_std socmedia_std church_std volany = "{bf:Social engagements}" livalone integration_std lonely_std = "{bf:Housing/community}") p3(nokey) p4(nokey) p4(nokey) p5(nokey) p6(nokey) p7(nokey) p8(nokey) p9(nokey) p10(nokey) p11(nokey) p12(nokey) p13(nokey) p14(nokey) p15(nokey) p16(nokey) p17(nokey) p18(nokey) p19(nokey) p20(nokey) p21(nokey) p22(nokey) p23(nokey) p24(nokey) p25(nokey) p26(nokey) p27(nokey) p28(nokey) p29(nokey) p30(nokey) p31(nokey) p32(nokey)

graph export "coefplot.tif", width(1386) height(924) replace

*by gender
eststo clear
foreach x of varlist moca_raw_std epmem_std execfxn_std married kids_std working caregiver veteran phone_log_std lcesocial_std socmedia_std church_std volany livalone integration_std lonely_std {
	reg bridging source_study age female white i.edu `x' if female==0,vce(robust) 
	est store `x'
	reg bridging source_study age female white i.edu `x' if female==1,vce(robust) 
	est store `x'_base
}
coefplot (moca_raw_std_base, pstyle(p1) label(Women)) (moca_raw_std, pstyle(p2) label(Men) offset(0.15)) (epmem_std_base, pstyle(p1)) (epmem_std, pstyle(p2) offset(0.15)) (execfxn_std_base, pstyle(p1)) (execfxn_std, pstyle(p2) offset(0.15)) (married_base, pstyle(p1)) (married, pstyle(p2) offset(0.15)) (kids_std_base, pstyle(p1)) (kids_std, pstyle(p2) offset(0.15)) (working_base, pstyle(p1)) (working, pstyle(p2) offset(0.15)) (caregiver_base, pstyle(p1)) (caregiver, pstyle(p2) offset(0.15)) (veteran_base, pstyle(p1)) (veteran, pstyle(p2) offset(0.15)) (phone_log_std_base, pstyle(p1)) (phone_log_std, pstyle(p2) offset(0.15)) (lcesocial_std_base, pstyle(p1)) (lcesocial_std, pstyle(p2) offset(0.15)) (socmedia_std_base, pstyle(p1)) (socmedia_std, pstyle(p2) offset(0.15)) (church_std_base, pstyle(p1)) (church_std, pstyle(p2) offset(0.15)) (volany_base, pstyle(p1)) (volany, pstyle(p2) offset(0.15)) (livalone_base, pstyle(p1)) (livalone, pstyle(p2) offset(0.15)) (integration_std_base, pstyle(p1)) (integration_std, pstyle(p2) offset(0.15)) (lonely_std_base, pstyle(p1)) (lonely_std, pstyle(p2) offset(0.15)), yline(0) ytitle("Bridging social capital") drop(_cons source_study age female white *.edu) legend(position(3) ring(1) size(med)) xlabel(,labsize(med) angle(90)) vertical groups(moca_raw_std epmem_std execfxn_std = "{bf:Cognitive function}" married kids_std working caregiver veteran = "{bf:Social roles}" phone_log_std lcesocial_std socmedia_std church_std volany = "{bf:Social engagements}" livalone integration_std lonely_std = "{bf:Housing/community}") p3(nokey) p4(nokey) p4(nokey) p5(nokey) p6(nokey) p7(nokey) p8(nokey) p9(nokey) p10(nokey) p11(nokey) p12(nokey) p13(nokey) p14(nokey) p15(nokey) p16(nokey) p17(nokey) p18(nokey) p19(nokey) p20(nokey) p21(nokey) p22(nokey) p23(nokey) p24(nokey) p25(nokey) p26(nokey) p27(nokey) p28(nokey) p29(nokey) p30(nokey) p31(nokey) p32(nokey)
graph export "coefplot_gender.tif", width(1386) height(924) replace

/*individual figure
coefplot (moca_raw_std_base, pstyle(p1) label(Women)) (moca_raw_std, pstyle(p2) label(Men) offset(0.15)) (epmem_std_base, pstyle(p1)) (epmem_std, pstyle(p2) offset(0.15)) (execfxn_std_base, pstyle(p1)) (execfxn_std, pstyle(p2) offset(0.15)), yline(0) ytitle("Bridging social capital") drop(_cons source_study age female white *.edu) legend(position(3) ring(1) size(med)) xlabel(,labsize(med) angle(90)) vertical groups(moca_raw_std epmem_std execfxn_std = "{bf:Cognitive function}" married kids_std working caregiver veteran = "{bf:Social roles}" phone_log_std lcesocial_std socmedia_std church_std volany = "{bf:Social engagements}" livalone integration_std lonely_std = "{bf:Housing/community}") p3(nokey) p4(nokey) p4(nokey) p5(nokey) p6(nokey) p7(nokey) p8(nokey) p9(nokey) p10(nokey) p11(nokey) p12(nokey) p13(nokey) p14(nokey) p15(nokey) p16(nokey) p17(nokey) p18(nokey) p19(nokey) p20(nokey) ylabel(-1 (.5) 0.5)

graph export "cog_gender.tif", width(1386) height(924) replace

coefplot (married_base, pstyle(p1) label(Women)) (married, pstyle(p2) label(Men) offset(0.15)) (kids_std_base, pstyle(p1)) (kids_std, pstyle(p2) offset(0.15)) (working_base, pstyle(p1)) (working, pstyle(p2) offset(0.15)) (caregiver_base, pstyle(p1)) (caregiver, pstyle(p2) offset(0.15)) (veteran_base, pstyle(p1)) (veteran, pstyle(p2) offset(0.15)), yline(0) ytitle("Bridging social capital") drop(_cons source_study age female white *.edu) legend(position(3) ring(1) size(med)) xlabel(,labsize(med) angle(90)) vertical groups(moca_raw_std epmem_std execfxn_std = "{bf:Cognitive function}" married kids_std working caregiver veteran = "{bf:Social roles}" phone_log_std lcesocial_std socmedia_std church_std volany = "{bf:Social engagements}" livalone integration_std lonely_std = "{bf:Housing/community}") p3(nokey) p4(nokey) p4(nokey) p5(nokey) p6(nokey) p7(nokey) p8(nokey) p9(nokey) p10(nokey) p11(nokey) p12(nokey) p13(nokey) p14(nokey) p15(nokey) p16(nokey) p17(nokey) p18(nokey) p19(nokey) p20(nokey) ylabel(-1 (.5) 0.5)

graph export "roles_gender.tif", width(1386) height(924) replace


coefplot (livalone_base, pstyle(p1) label(Women)) (livalone, pstyle(p2) label(Men) offset(0.15)) (integration_std_base, pstyle(p1)) (integration_std, pstyle(p2) offset(0.15)) (lonely_std_base, pstyle(p1)) (lonely_std, pstyle(p2) offset(0.15)), yline(0) ytitle("Bridging social capital") drop(_cons source_study age female white *.edu) legend(position(3) ring(1) size(med)) xlabel(,labsize(med) angle(90)) vertical groups(moca_raw_std epmem_std execfxn_std = "{bf:Cognitive function}" married kids_std working caregiver veteran = "{bf:Social roles}" phone_log_std lcesocial_std socmedia_std church_std volany = "{bf:Social engagements}" livalone integration_std lonely_std = "{bf:Housing/community}") p3(nokey) p4(nokey) p4(nokey) p5(nokey) p6(nokey) p7(nokey) p8(nokey) p9(nokey) p10(nokey) p11(nokey) p12(nokey) p13(nokey) p14(nokey) p15(nokey) p16(nokey) p17(nokey) p18(nokey) p19(nokey) p20(nokey) ylabel(-1 (.5) 0.5)
graph export "housing_gender.tif", width(1386) height(924) replace


coefplot (phone_log_std_base, pstyle(p1) label(Women)) (phone_log_std, pstyle(p2) label(Men) offset(0.15)) (lcesocial_std_base, pstyle(p1)) (lcesocial_std, pstyle(p2) offset(0.15)) (socmedia_std_base, pstyle(p1)) (socmedia_std, pstyle(p2) offset(0.15)) (church_std_base, pstyle(p1)) (church_std, pstyle(p2) offset(0.15)) (volany_base, pstyle(p1)) (volany, pstyle(p2) offset(0.15)) , yline(0) ytitle("Bridging social capital") drop(_cons source_study age female white *.edu) legend(position(3) ring(1) size(med)) xlabel(,labsize(med) angle(90)) vertical groups(moca_raw_std epmem_std execfxn_std = "{bf:Cognitive function}" married kids_std working caregiver veteran = "{bf:Social roles}" phone_log_std lcesocial_std socmedia_std church_std volany = "{bf:Social engagements}" livalone integration_std lonely_std = "{bf:Housing/community}") p3(nokey) p4(nokey) p4(nokey) p5(nokey) p6(nokey) p7(nokey) p8(nokey) p9(nokey) p10(nokey) p11(nokey) p12(nokey) p13(nokey) p14(nokey) p15(nokey) p16(nokey) p17(nokey) p18(nokey) p19(nokey) p20(nokey) ylabel(-1 (.5) 0.5)

graph export "engagements_gender.tif", width(1386) height(924) replace