/*
Project Name: Coasean Paradox
Project Author: Jorge D. Ramos-Mercado
Code Objetive: This code processes the empirical findings in the paper named Coasean Paradox
Latest date of code work: 2026-February-2
*/ 

*Application 1: Litigation 
cd "/Users/jorgedavidramosmercado/Dropbox/"
set seed 999
log using coase_logs, replace 

 u FJC.dta, clear
 g a=1
bys dnum fyear:  egen tcaseload=sum(a)
drop a 
 keep if (juris==4 | juris==5)
 merge m:1 dnum using  timing_CJRA 
drop _merge 
 
 g transferred=transdat<.
 
 ta circuit if arbit=="M"
ta circuit if arbit!="M" & arbit!="-8" 
ta circuit 

g disposition=1 if disp==0 | disp==1  | disp==10 | disp==11
replace disposition=2 if disposition==. & (disp==2 | disp==3 | disp==14)
replace disposition=disp-1 if disp>3 & disp<7 
replace disposition=6 if disp==7 | disp==8 | disp==9 | disp==20
replace disposition=7 if disp==15
replace disposition=8 if disp==19  
replace disposition=9 if disposition==. & disp==12 

replace disposition=10 if disposition==. & disp==13

la de resolution 1 "Tranferred" 2 "Involuntary Dismissal" 3 "Motions" 4 "Consent" 5 "Pretrial hearing" 6 "Trial" 7 "Arbitrator Award" 8 "Appeal" 9 "Other" 10 "Settled"
la val disposition resolution 
g progression= 1 if procprog<5
 replace progression=2 if procprog==5
  replace progression=3 if procprog!=2 & procprog<10 & procprog>5
  replace progression=4 if procprog==10
    replace progression=3 if progression==.

  la de progress 1 "No hearing" 2 "Pre-trial hearing" 3 "Trial"
  la val progression progression
  la var progression "How far did case go?"
bys arbitration: ta nos
bys arbitration: ta dnum
bys arbitration: ta dnum if circuit==3
bys arbitration: ta dnum if circuit==2

 ta nos if  dnum==13
 ta nos if  dnum==8
 ta nos 
  
  gen outcome= 1 if (noj==1 | noj==2) & judgment==1 
   replace outcome= 2 if noj==0 & judgment==1 
  replace outcome= 3 if (noj==1 | noj==2) & judgment==2 
   replace outcome= 4 if noj==0 & judgment==2
   replace outcome=5 if outcome==. & disposition!=2
la de oits 1 "Monetary award for Plaintiff" 2 "Non-monetary award for Plaintiff" 3 "Monetary award for Defendant" 4 "Non-monetary award for Defendant" 5 "Other" 
la val outcome oits
 bys arbitration: ta progression
bys arbitration: ta disposition
bys arbitration: ta disposition if fyear>1989 & fyear<1997


bys arbitration: ta outcome


g wait= max(0,365*(year(pretrial)-year(filedate))+12*(month(pretrial)-month(filedate))+(day(pretrial)-day(filedate)))
g post_pre_dur= 365*(year(termdate)-year(pretrial))+12*(month(termdate)-month(pretrial))+(day(termdate)-day(pretrial)) if wait>0
g alpha=min(2200,max(1,365*(year(termdate)-year(filedate))+12*(month(termdate)-month(filedate))+(day(termdate)-day(filedate))))
 la var alpha "Civil Case Duration (in days)"
la var post_pre_dur "Time between Pre-trial Date and Settlement"


g _post=min(36, (post_pre_dur/365.25)*12) if post_pre_dur<.

la var _post "Number of months since Pre-tral Hearing when resolution arrives"
tw (hist _post if arbitration==0 & fyear>=1990 & fyear<=1996 & _post<36 , col(red%75)  w(1) percent) (hist _post if arbitration==2  & fyear>=1990 & fyear<=1996  & _post<36 , w(1) col(blue%75)  percent) , leg(off) caption("Mandatory arbitration (blue, unmediated cases (red) for year 1990-1996")

 su  ld if  arbitration==2 & fyear>1992 & tribegan==. & disposition>3 & _post<.
 su  ld if  arbitration==0 & fyear>1992 & tribegan==. & disposition>3 & _post<.
 su  ld if  arbitration==2 & fyear<=1992 & tribegan==. & disposition>3 & _post<.
 su  ld if  arbitration==0 & fyear<=1992 & tribegan==. & disposition>3 & _post<.
 
 su  ld if  arbitration==2 & fyear>1992 & tribegan==. & disposition>3 
 su  ld if  arbitration==0 & fyear>1992 & tribegan==. & disposition>3  
 su  ld if  arbitration==2 & fyear<=1992 & tribegan==. & disposition>3  
 su  ld if  arbitration==0 & fyear<=1992 & tribegan==. & disposition>3  
 
 su  ld if  arbitration==2 & fyear>1992 & tribegan==. &  _post<.
 su  ld if  arbitration==0 & fyear>1992 & tribegan==. &  _post<.
 su  ld if  arbitration==2 & fyear<=1992 & tribegan==. & _post<.
 su  ld if  arbitration==0 & fyear<=1992 & tribegan==. & _post<.
 
 su  ld if  arbitration==2 & fyear>1992 & tribegan==.  
 su  ld if  arbitration==0 & fyear>1992 & tribegan==.  
 su  ld if  arbitration==2 & fyear<=1992 & tribegan==.  
 su  ld if  arbitration==0 & fyear<=1992 & tribegan==.  
 
 
g when=1 if _post==.
replace when=2 if _post<=2 & _post>=0
replace when=3 if when==.
replace when=. if disposition<3 | (disposition==9 & _post<.) | (_post==. & disposition==10)

reg ld i.arbitration if arbitration!=1 & when<.

reg duration i.arbitration if arbitration!=1 & when<.

tw (hist when if arbitration==0 & fyear>=1990 & fyear<=1996  , col(midblue)   percent) (hist when if arbitration==2  & fyear>=1990 & fyear<=1996  ,  col(red)  percent) , leg(off) caption("Mandatory arbitration (blue), unmediated cases (red). Period: 1990-1996")



disp "No ADR"
ta when if arbitration==0 & fyear>=1990 & fyear<=1996
disp "Mandatory ADR"
ta when if arbitration==2 & fyear>=1990 & fyear<=1996

*tw (hist _post if arbitration==0 & fyear>=1990 & fyear<=1996 & _post<36 & disposition<6 , col(red%75)  w(1) percent) (hist _post if arbitration==2  & fyear>=1990 & fyear<=1996  & _post<36 &  disposition<6 , w(1) col(blue%75)  percent) , leg(off) caption("Mandatory arbitration (blue), unmediated cases (red) for year 1990-1996")

bys arbitration: ta progression if fyear>=1990 & fyear<2001 & disposition!=2

g _duration=ceil(duration/31)
replace _duration=60 if _duration>60 & _duration<.


*This is where I generate the instrument...


 g cats=1 if demanded==0
 replace cats=2 if demanded<9999 & cats==. 
 replace cats=3 if demanded==9999 & cats==.
 
gen tot= 1

bys dnum fyear cats: egen tots=total(tot)
drop tot 

g forced=arbitration==2 

bys dnum fyear cats: egen force=total(forced)

drop forced 

g amicable=arbitration==1

bys dnum fyear cats: egen amigo=total(amicable)

drop amicable 

replace progression=999 if progression==.
*Here's the difference in differences.

g pilot=(district=="70" | district=="72" | district=="71" | district=="05" | district=="11" | district=="3C" | district=="52" | district=="66" | district=="79" | district=="42")
g arb2=arbitration==2 
gen early = (  district == "7-"  |   district == "60"  |   district == "76"  |    district == "54"  |    district == "55"  |   district == "56"  |   district == "83"  |   district == "01"  |   district == "46"  |   district == "77"  |   district == "07"  |   district == "12"  |   district == "47"  |  district == "40"  | district == "22"  |  district == "91"  |   district == "24"  |    district == "25"  |   district == "58"  |   district == "89" ) 
gen post_pilot=fyear>1991
gen post_early=fyear>1992

gen treated_pilot = pilot *post_pilot
gen treated_early = early * post_early

*ta _duration if arbitration==0 & fyear>=1990 & fyear<1997 & disposition>4
*ta _duration if arbitration==2 & fyear>=1990 & fyear<1997 & disposition>4


g near_pre= post_pre_dur<14 if post_pre_dur<.
g near_pre_2=post_pre_dur<32 if post_pre_dur<.
g near_pre_3=post_pre_dur<46 if post_pre_dur<.
g near_pre_4=post_pre_dur<22 if post_pre_dur<.

gen tq = yq(start_year,start_q)

g cuarto=1 if fmonth<4
replace cuarto=2 if fmonth<7 & cuarto==.
replace cuarto=3 if fmonth<10 & cuarto==.
replace cuarto=4 if cuarto==.

gen date=yq(fyear,cuarto)
g caseload=1
g case_pretrial=near_pre_2<.
g dismissed=disposition==2
g trial=disposition==6 | disposition==8
g ldpre=ld if case_pretrial==1
g ldpre3=ld if case_pretrial==1 & disposition==5
g ldpre4=ld if case_pretrial==1 & disposition>5

g ldpre2=ld if case_pretrial==1
g durationpre=duration if case_pretrial==1

g ldtrial=ld if disposition>5
g ldearlyl=ld if case_pretrial==0

*Estimating the cost of ADR per litigant
*In 2023, the average cost per hour for US Federal Civil litigation is $257 USD
* Our estimate is that 20% of the duration is attributed to ADR 
*The average number of hours in prep per day is roughly 2 and it goes up to 6-9 while in mediation
*I will assume 2 hours getting billed per day. 

*g waste=duration/5*2*110 if year

*Data comes from USAO-DC Laffey matrices
g nom_hour_cost=156*4+1500 if fyear==1990
replace nom_hour_cost=163*4+1500 if fyear==1991
replace nom_hour_cost=169*4+1500 if fyear==1992
replace nom_hour_cost=173*4+1500 if fyear==1993
replace nom_hour_cost=178*4+1500 if fyear==1994
replace nom_hour_cost=184*4+1500 if fyear==1995
replace nom_hour_cost=186*4+1500 if fyear==1996
replace nom_hour_cost=191*4+1500 if fyear==1997
replace nom_hour_cost=193*4+1500 if fyear==1998
replace nom_hour_cost=198*4+1500 if fyear==1999

replace nom_hour_cost=203*4+2000 if fyear==2000
replace nom_hour_cost=209*4+2000 if fyear==2001
replace nom_hour_cost=215*4+2000 if fyear==2002
replace nom_hour_cost=221*4+2000 if fyear==2003
replace nom_hour_cost=228*4+2000 if fyear==2004
replace nom_hour_cost=238*4+2000 if fyear==2005
replace nom_hour_cost=249*4+2000 if fyear==2006
replace nom_hour_cost=259*4+2000 if fyear==2007
replace nom_hour_cost=273*4+2000 if fyear==2008
replace nom_hour_cost=273*4+2000 if fyear==2009


replace nom_hour_cost=260*4+2000 if nom_hour_cost==. & fyear<=2020
replace nom_hour_cost=380*4+2000 if nom_hour_cost==. 

g deflator=2.30056 if fyear<=1990
replace deflator=2.23397 if fyear==1991
replace deflator=2.16961 if fyear==1992
replace deflator=2.11029 if fyear==1993
replace deflator=2.05686 if fyear==1994
replace deflator=2.00608 if fyear==1995
replace deflator=1.94051 if fyear==1996
replace deflator=1.90813 if fyear==1997
replace deflator=1.87795 if fyear==1998
replace deflator=1.82900 if fyear==1999
replace deflator=1.76824 if fyear==2000
replace deflator=1.74033 if fyear==2001
replace deflator=1.69821 if fyear==2002
replace deflator=1.66434 if fyear==2003
replace deflator=1.61051 if fyear==2004
replace deflator=1.55848 if fyear==2005
replace deflator=1.52011 if fyear==2007
replace deflator=1.46012 if fyear==2008
replace deflator=1.46044 if fyear==2009
replace deflator=1.42047 if fyear==2010
replace deflator=1.40034 if fyear==2011
replace deflator=1.35873 if fyear==2012
replace deflator=1.33524 if fyear==2013
replace deflator=1.31534 if fyear==2014
replace deflator=1.30680 if fyear==2015
replace deflator=1.29851 if fyear==2016
replace deflator=1.27242 if fyear==2017
replace deflator=1.24588 if fyear==2018
replace deflator=1.22142 if fyear==2019
replace deflator=1.19373 if fyear==2020
replace deflator=1.17818 if fyear==2021
replace deflator=1.09946 if fyear==2022
replace deflator=1.03322 if fyear==2023
replace deflator=1  if fyear==2024
replace deflator=0.97208 if fyear==2025


g real_hour_cost=nom_hour_cost*deflator 
g cost=real_hour_cost*duration*(5/7)
g lcostpre=ln(1+cost) if case_pretrial==1
g lcost=ln(1+cost) 

g arbitrated=arbitration>0
g farbitrated=arbitration==1
g arbpre=0
replace arbpre=1 if arbitrated==1 & duration<180
g farbpre=0
replace farbpre=1 if farbitrated==1 & duration<180

g cohort=pilot==1
replace cohort=2 if early==1
 ta _duration if fyear>=1990 & disposition>4 & fyear<1994 & cohort==0
  ta _duration if fyear>=1993 & disposition>4 & fyear<1997 & cohort==0
 ta _duration if fyear>=1990 & disposition>4 & post_pilot==0 & cohort==1
  ta _duration if disposition>4 & post_pilot==1 & cohort==1 & fyear<1997
  
   ta _duration if fyear>=1990 & disposition>4 & post_early==0 & cohort==2
  ta _duration if fyear<1997 & disposition>4 & post_early==1 & cohort==2
  
  g trialb=tribegan<. if near_pre<.
  g ld190=ldpre if nos==190
  g near190=near_pre_2 if nos==190
  g before=post_pre_dur<0 | near_pre==.
  g later=before==0 & near_pre_2==0
  
  g lwait=ln(1+max(0,pretrial-date))
  bys fyear dnum: egen ppp=kurt(post_pre_dur)
  
  
  g nos190=nos==190
  
  g o1=outcome==1 | outcome==2
  g o2=outcome==3 | outcome==4
  
  bys arbitration: ta disposition if fyear>1989 & fyear<1997 & near_pre_2<.
  
  g progressed=1 if disposition<5 
  replace progressed=1 if  (disposition==10 & near_pre_2==.) & progressed==.

  replace progressed=2 if (disposition==5 | near_pre_2==1) & progressed==.
    replace progressed=4 if progressed==. & (disposition==6 | disposition==7 | disposition==8)
     replace progressed=3 if  progressed==. & disposition==10 
	 
	 
       replace progressed=0 if progressed==.

  
disp "Non-ADR"
 ta progressed if fyear>1989 & fyear<1997 & arbitration==0 & progressed>0
 
 disp "Mandatory ADR"

   ta progressed  if fyear>1989 & fyear<1997 & arbitration==2   & progressed>0
	
	g settled = disposition==10
	g set2=settled if near_pre<.
	
	drop nomoney 
	g nomoney=amtrec==0 if amtrec>=0 & amtrec<. 
g wait_trial=tribegan-pretrial if tribegan<. & pretrial<.

preserve 
 
 
 collapse ld duration durationpre cost near_pre near_pre_* trialb before lwait later lcostpre  lcost ldpre ldpre3 ldpre4 ldearly case_pretrial tq cohort dismissed trial fyear ldtrial ppp nos190 o1 o2 settled set2 transferred nomoney wait_trial  year (sum) *caseload (p50) ldpre2 , by(dnum date)
sa advance_did, replace 
restore 

 u  advance_did,clear 
 drop if fyear<1990 | fyear>2006

   
  
    csdid ldpre,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	    estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
   
   
   
   
     *Settlement increase fails...
    csdid settled,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	    estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
   
       csdid set2,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	    estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
   
   
       csdid ldpre2,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	    estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
   
         csdid durationpre,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	    estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
   
       csdid ldearlyl,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	    estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
   
       csdid ldtrial,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	       estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
   
  csdid near_pre_2,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
      estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
     csdid near_pre_3,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
      estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
        csdid near_pre_4,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
      estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
           csdid ldpre3,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
      estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
   
su near_pre_2 if date< tq
su near_pre_2 if date<= 1991
su near_pre_3 if date<= 1991
su ldpre ldpre3 ldpre4 if date< tq


g pre_near=near_pre_2*case_pretrial+ (1-case_pretrial)
   su pre_near if date< tq

     csdid ld,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
    estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
     csdid near_pre_2,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
    estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
   
      csdid trial,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
    estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
   
         csdid ld190,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
    estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
   
  csdid near190,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
    estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
       csdid before,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	    estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
 
       csdid later,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	    estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
   
     csdid near_pre_2,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
    estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   

        csdid caseload ,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
    estat event
  estat event, window(-6 6)
   csdid_plot, style(rarea)
   
   
 csdid case_pretrial ,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event
 estat event, window(-6 6)
 csdid_plot, style(rarea) 
 
   
    csdid lwait ,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event
 estat event, window(-6 6)
 csdid_plot, style(rarea) 
 
 
 g pp=caseload/tcaseload
 
csdid pp,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event
 estat event, window(-6 6)
 csdid_plot, style(rarea) 
   
   
   csdid nos190,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event
 estat event, window(-6 6)
 csdid_plot, style(rarea) 
 
 g o=o1/(o1+o2)
 
 
    csdid o,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event
 estat event, window(-6 6)
 csdid_plot, style(rarea) 
 
     csdid o1,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event
 estat event, window(-6 6)
 csdid_plot, style(rarea) 
 
     csdid o2,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event
 estat event, window(-6 6)
 csdid_plot, style(rarea) 
 
 g o3=1-(o1+o2)
      csdid o3,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event
 estat event, window(-6 6)
 csdid_plot, style(rarea) 
 
 *Final Robustness
 
  replace transferred=ln(1+transferred*caseload)

     csdid transferred,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
	    estat event
 estat event, window(-6 6)
   csdid_plot, style(rarea)
 
 
 
 replace nomoney=ln(1+nomoney*caseload)
 
      csdid nomoney,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event, window(-6 6)
   csdid_plot, style(rarea)
   

   
   g lcase=ln(1+caseload)
   csdid lcase,  ivar(dnum) time(date) gvar(tq) method(dripw) vce(cluster dnum)
 estat event, window(-6 6)
   csdid_plot, style(rarea)


*Application 2: Strikes



u strikes_ag, clear 
 
 g unending=edate==. 
 
replace edate=2025+5/12 if edate==.
replace duration=edate -sdate if duration==. 
replace ld =ln(duration) if ld==.


   
   g missingman=num_workers==.
   replace lman=0 if  missingman==1 & lman==.
  
  g mind=industry==.
  replace ind=0 if ind==.
  
  g mst=st==. 
  replace st=0 if st==.
qui: reg ld i.ind i.st  unending mind mst government i.syear i.UNION
  
  predict p_ld 
  
  g eld=ld-p_ld
  replace idleshare=0 if noidle==0 & idleshare==. 
  

  
  egen p25m=pctile(num_workers), p(25)
  egen p50m=pctile(num_workers), p(50)
  egen p75m=pctile(num_workers), p(75)

  gen qm=1 if num_workers<p25m 
  replace qm=2 if qm==. & num_workers<p50m 
    replace qm=3 if qm==. & num_workers<p75m 
	  replace qm=4 if qm==.  
	 
  

  
   egen p25i=pctile( idleduration), p(25)
  egen p50i=pctile( idleduration), p(50)
  egen p75i=pctile( idleduration), p(75)

  gen qi=1 if  idleduration<p25i 
replace qi=2 if qi==. &  idleduration<p50i 
    replace qi=3 if qi==. &  idleduration<p75i 
	  replace qi=4 if qi==.  
	  
	  bys qm: su duration if duration<400
	  bys qi: su duration if duration<400

	  reg eld i.qm i.qi, nocons   

	  *Instrument
	  



gen picketing=0

replace picketing=syear-1939 if states=="WI" | states=="MN" | states=="MI"
replace picketing=syear-1941 if states=="TX"
replace picketing=syear-1941 if states=="LA" | states=="VA" | states=="CO" 
replace picketing=syear-1947 if states=="AL" | states=="AK" | states=="MS" | states=="DE" | states=="FL" |states=="GA" | states=="KS" | states=="NE"  | states=="UT" | states=="SD"


egen stnum=group(states)
gen lidle=lti-lman
 
g protect=0 
replace protect=max(0, syear-1975) if states=="CA" 
replace protect=max(0, syear-1981) if states=="WA" 
replace protect=max(0, syear-1983) if states=="MA" 
replace protect=max(0, syear-1991) if states=="CO" 
replace protect=max(0, syear-1994) if states=="NJ" 

 
g dpicketing=picketing>0
g I= dpicketing*idleshare

g _duration= duration 

replace _duration=115 if _duration>115
replace _duration=_duration/7
replace _duration=floor(_duration)
bys dpicketing: ta _duration


*********************************************
*********************************************
*********************************************
*Motivation is here...
*********************************************
*********************************************
*********************************************

qui: reg ld idleshare dpicketing I 
predict P1_ld
 reg P1_ld dpicketing 
qui: reg ld idleshare dpicketing I  lti syear i.ind i.st government i.UNION

foreach x in lti syear ind st government UNION {
	g _`x'=`x'
	egen a=mean(`x')
	replace `x'=a 
	drop a
}
predict P2_ld
 reg P2_ld dpicketing 
 foreach x in lti syear ind st government UNION {
replace `x'=_`x'
drop _`x'
 }
	
	
*	replace `x'=ln(1+`x')
*}
g LT=ln(1+total_num_days_idle)

* Generate region variable
gen region = .

* Northeast (9 states)
replace region = 1 if inlist(statefip, 9, 23, 25, 33, 44, 50)     // New England
replace region = 1 if inlist(statefip, 34, 36, 42)                 // Mid-Atlantic

* Midwest (12 states)
replace region = 2 if inlist(statefip, 17, 18, 26, 39, 55)        // East North Central
replace region = 2 if inlist(statefip, 19, 20, 27, 29, 31, 38, 46) // West North Central

* South (16 states + DC)
replace region = 3 if inlist(statefip, 10, 11, 12, 13, 24, 37, 45, 51, 54) // South Atlantic
replace region = 3 if inlist(statefip, 1, 21, 28, 47)              // East South Central
replace region = 3 if inlist(statefip, 5, 22, 40, 48)              // West South Central

* West (13 states)
replace region = 4 if inlist(statefip, 4, 8, 16, 30, 32, 35, 49, 56) // Mountain
replace region = 4 if inlist(statefip, 2, 6, 15, 41, 53)           // Pacific
replace region=5 if region==.
* Label
label define region_lbl 1 "Northeast" 2 "Midwest" 3 "South" 4 "West" 5 "Multi-state"
label values region region_lbl
	 

factor   protect picketing, factors(2) mineigen(1) pcf
predict common2
su common2 
replace common2=ln(common2-r(min)+1)


factor   st industry , factors(2) mineigen(1) pcf
predict context

  replace union_rate=union_rate/100

  

   ivreg2 ld  (lidle= common2 ) context  union_rate lman, first
 weakiv
 
su ld
 g zd=(ld-r(mean))/r(sd)
 
  su lidle
 g zlid=(lidle-r(mean))/r(sd)
 su union_rate
 g zu=(union_rate-r(mean))/r(sd)
 su num_workers 
 g zman=(num_workers-r(mean))/r(sd)
 su context 
 g zcon=(context-r(mean))/r(sd)
 su common2 
 g zcom=(common2-r(mean))/r(sd)
    ivreg2 ld  (lidle= common2 ) context union_rate  lman, first

 
 g sday=day(sdate)
 g eday=day(edate)

  g reporting=0
  replace reporting=1 if government==0 & ((smonth==6 & sday>15) | (smonth==7 & sday<15))  & (emonth>6 | (emonth==6 & eday>15))
  
  replace reporting=1 if government==0 & ((smonth==9 & sday>15) | (smonth==10 & sday<15))  & (emonth>9 | (emonth==9 & eday>15))
 
  replace reporting=1 if government==0 & ((smonth==12 & sday>15)| (smonth<=2))  & (emonth>12 )
  
  
  su common2 
  
  g factor=(common2-r(mean))/r(sd)
  
  stset duration, failure(unending==0)
  
  su context 
  stcox  i.reporting##c.factor  c.(sdate  context  idleshare  union_rate lman) i.government, vce(r)




log close 
