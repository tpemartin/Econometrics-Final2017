/* purpose: estimate probility of having Internet access using CEX */
/*          and compute imputed propensity scores */
/* date : June 16, 2004 */

options ls=80 compress=yes formdlim='=';


filename all 'cex_basefile97_02.csv';

data all;
    infile all dlm = ',' lrecl = 1000;
    input newid intno year firmth nint cdall
        age white black male hsgrad lesscol colgrad
        fam_size nchle11 nch1217 persot64 hw single
        hwyng hwchbs hwchis hwchas spchbs spchis hwold retired
        headwrk spouwrk incweek1 inc_hrs1 incweek2 inc_hrs2 fincreal
        owner renter ne mw west urban msa ps4mil ps1mil ps330k ps125k
        tv comp sound vcr vehq coldormi occucod1 weight;
    int = 0; if nint = 0 then int = 1;
run;


/* occupation dummies */
data all;
    set all;
    manager=occucod1=1;
    teacher=occucod1=2;
    prof=occucod1=3;
    admin=occucod1=4;
    tech=occucod1=7;
    sales=0; if occucod1 in (5,6) then sales=1;
    service=0; if occucod1 in (8,9,10) then service = 1;
    laborer=0; if occucod1 in (11,12,13) then laborer = 1;
    craft=0; if occucod1 in (14,15) then craft = 1;
    farm=0; if occucod1 in (16,17) then farm = 1;
    armedf=0; if occucod1 = 18 then armedf = 1;
run;

/* appliance ownership */
data all;
    set all;
    computer = 0; if comp > 0 then computer = 1;
    soundcp = 0; if sound > 0 then soundcp = 1;
    vcrp = 0; if vcr > 0 then vcrp = 1;
    age2 = age*age;
    finc2 = fincreal*fincreal;
run;

/******************/
/* select samples */
/******************/
data all;
    set all;
    age1534 = 0; 
    /* hwchis: hw w/ child in school (kids age b/w 6 and 17) */
    if hwchis = 0 and spchis = 0 and age < 35 then age1534 = 1;

    age3549 = 0; 
    if hwchis = 0 and spchis = 0 and age >= 35 and age < 50 then age3549 = 1;

    age50 = 0;
    if hwchis = 0 and spchis = 0 and age >= 50 then age50 = 1;

    hwsk617 = 0;
    if hwchis = 1 or spchis = 1 then hwsk617 = 1;
run;


data all;
    set all;

    napster = 0;
    if year > 1999 then napster = 1;
    if year = 1999 and firmth >= 6 then napster = 1;

    cdwt = cdall*weight;
    i = 1;
run;

proc means data = all noprint;
    var cdwt weight i;
    output out = temp sum = cdwt wgt n;
run;

data temp;
    set temp;
    mcd = cdwt/wgt;
run;

proc print data = temp;
    var mcd wgt n;
run;

filename out 'basefile.csv';

data out;
    set all;
    file out dlm = ',' lrecl = 1000;
    put napster int nint weight cdall 
        nint age age2 white black male hsgrad lesscol colgrad
        fincreal finc2 tv computer soundcp vcrp vehq 
        manager teacher prof admin tech sales service
        fam_size nchle11 nch1217 persot64 hw single
        hwyng hwchbs hwchis hwchas spchbs spchis retired
        headwrk spouwrk incweek1 inc_hrs1 incweek2 inc_hrs2
        owner renter ne mw west urban msa ps4mil ps1mil ps330k ps125k;
run;

proc sort data = all;
     by int napster;
run;

proc means data = all;
     var cdall i;
     by int napster;
run;

/***********/
/* probits */
/***********/
data bf;
    set all;
    if napster = 0;    
run;
data af;
    set all;
    if napster = 1;
run;

/***** Pb *****/
proc probit data= bf outest = bfest1;
    class nint;
    INTERNET: model nint = age age2 white black male hsgrad lesscol colgrad
        fincreal finc2 tv computer soundcp vcrp vehq 
        manager teacher prof admin tech sales service
        fam_size nchle11 nch1217 persot64 hw single
        hwyng hwchbs hwchas spchbs retired
        headwrk spouwrk incweek1 inc_hrs1 incweek2 inc_hrs2
        owner renter ne mw west urban msa ps4mil ps1mil ps330k ps125k;
run;

data bfest1;
    set bfest1;
    v1 = Intercept; v2 = age; v3 = age2; v4 = white; v5 = black;
    v6 = male; v7 = hsgrad; v8 = lesscol; v9 = colgrad; v10 = fincreal;
    v11 = finc2; v12 = tv; v13 = computer; v14 = soundcp; v15 = vcrp;
    v16 = vehq; v17 = manager; v18 = teacher; v19 = prof; v20 = admin;
    v21 = tech; v22 = sales; v23 = service; v24 = fam_size; v25 = nchle11;
    v26 = nch1217; v27 = persot64; v28 = hw; v29 = single; v30 = hwyng;
    v31 = hwchbs; v33 = hwchas; v34 = spchbs;
    v37 = retired; v38 = headwrk; v39 = spouwrk; v40 = incweek1;
    v41 = inc_hrs1;  v42 = incweek2; v43 = inc_hrs2; v44 = owner; v45 = renter;
    v46 = ne; v47 = mw; v48 = west; v49 = urban; v50 = msa;
    v51 = ps4mil; v52 = ps1mil; v53 = ps330k; v54 = ps125k; i = 1;

    drop Intercept age age2 white black male hsgrad lesscol colgrad
        fincreal finc2 tv computer soundcp vcrp vehq 
        manager teacher prof admin tech sales service
        fam_size nchle11 nch1217 persot64 hw single
        hwyng hwchbs hwchas spchbs retired
        headwrk spouwrk incweek1 inc_hrs1 incweek2 inc_hrs2
        owner renter ne mw west urban msa ps4mil ps1mil ps330k ps125k;
run;
proc sort;
    by i;
run;

data all;
    set all;
    i = 1;
run;
proc sort;
    by i;
run;

data all;
    merge all bfest1;
    by i;
run;

data all;
    set all;
    psb = probnorm(v1 + age*v2 + age2*v3 + white*v4 + black*v5 + male*v6
        + hsgrad*v7 + lesscol*v8 + colgrad*v9 + fincreal*v10 + finc2*v11
        + tv*v12 + computer*v13 + soundcp*v14 + vcrp*v15 + vehq*v16 + manager*v17
        + teacher*v18 + prof*v19 + admin*v20 + tech*v21 + sales*v22
        + service*v23 + fam_size*v24  + nchle11*v25 + nch1217*v26 
        + persot64*v27 + hw*v28 + single*v29 + hwyng*v30 + hwchbs*v31 
        + hwchas*v33 + spchbs*v34 
        + retired*v37 + headwrk*v38 + spouwrk*v39 + incweek1*v40 
        + inc_hrs1*v41 + incweek2*v42 + inc_hrs2*v43 + owner*v44 
        + renter*v45 + ne*v46 + mw*v47 + west*v48 + urban*v49 + msa*v50 
        + ps4mil*v51 + ps1mil*v52 + ps330k*v53 + ps125k*v54);
    drop v1-v31 v33-v34 v37-v54;
run;

/***** Pa *****/
proc probit data= af outest = afest1;
    class nint;
    INTERNET: model nint = age age2 white black male hsgrad lesscol colgrad
        fincreal finc2 tv computer soundcp vcrp vehq 
        manager teacher prof admin tech sales service
        fam_size nchle11 nch1217 persot64 hw single
        hwyng hwchbs hwchas spchbs retired
        headwrk spouwrk incweek1 inc_hrs1 incweek2 inc_hrs2
        owner renter ne mw west urban msa ps4mil ps1mil ps330k ps125k;
run;

data afest1;
    set afest1;
    v1 = Intercept; v2 = age; v3 = age2; v4 = white; v5 = black;
    v6 = male; v7 = hsgrad; v8 = lesscol; v9 = colgrad; v10 = fincreal;
    v11 = finc2; v12 = tv; v13 = computer; v14 = soundcp; v15 = vcrp;
    v16 = vehq; v17 = manager; v18 = teacher; v19 = prof; v20 = admin;
    v21 = tech; v22 = sales; v23 = service; v24 = fam_size; v25 = nchle11;
    v26 = nch1217; v27 = persot64; v28 = hw; v29 = single; v30 = hwyng;
    v31 = hwchbs; v33 = hwchas; v34 = spchbs;
    v37 = retired; v38 = headwrk; v39 = spouwrk; v40 = incweek1;
    v41 = inc_hrs1;  v42 = incweek2; v43 = inc_hrs2; v44 = owner; v45 = renter;
    v46 = ne; v47 = mw; v48 = west; v49 = urban; v50 = msa;
    v51 = ps4mil; v52 = ps1mil; v53 = ps330k; v54 = ps125k; i = 1;

    drop Intercept age age2 white black male hsgrad lesscol colgrad
        fincreal finc2 tv computer soundcp vcrp vehq 
        manager teacher prof admin tech sales service
        fam_size nchle11 nch1217 persot64 hw single
        hwyng hwchbs hwchas spchbs retired
        headwrk spouwrk incweek1 inc_hrs1 incweek2 inc_hrs2
        owner renter ne mw west urban msa ps4mil ps1mil ps330k ps125k;
run;
proc sort;
    by i;
run;

data all;
    set all;
    i = 1;
run;
proc sort;
    by i;
run;

data all;
    merge all afest1;
    by i;
run;

data all;
    set all;
    psa = probnorm(v1 + age*v2 + age2*v3 + white*v4 + black*v5 + male*v6
        + hsgrad*v7 + lesscol*v8 + colgrad*v9 + fincreal*v10 + finc2*v11
        + tv*v12 + computer*v13 + soundcp*v14 + vcrp*v15 + vehq*v16 + manager*v17
        + teacher*v18 + prof*v19 + admin*v20 + tech*v21 + sales*v22
        + service*v23 + fam_size*v24  + nchle11*v25 + nch1217*v26 
        + persot64*v27 + hw*v28 + single*v29 + hwyng*v30 + hwchbs*v31 
        + hwchas*v33 + spchbs*v34 
        + retired*v37 + headwrk*v38 + spouwrk*v39 + incweek1*v40 
        + inc_hrs1*v41 + incweek2*v42 + inc_hrs2*v43 + owner*v44 
        + renter*v45 + ne*v46 + mw*v47 + west*v48 + urban*v49 + msa*v50 
        + ps4mil*v51 + ps1mil*v52 + ps330k*v53 + ps125k*v54);
    drop v1-v31 v33-v34 v37-v54;
run;



/***** Pall *****/
/*
proc probit data= all outest = bfafest1;
    class nint;
    INTERNET: model nint = age age2 white black male hsgrad lesscol colgrad
        fincreal finc2 tv computer soundcp vcrp vehq 
        manager teacher prof admin tech sales service
        fam_size nchle11 nch1217 persot64 hw single
        hwyng hwchbs hwchas spchbs retired
        headwrk spouwrk incweek1 inc_hrs1 incweek2 inc_hrs2
        owner renter ne mw west urban msa ps4mil ps1mil ps330k ps125k;
run;

data bfafest1;
    set bfafest1;
    v1 = Intercept; v2 = age; v3 = age2; v4 = white; v5 = black;
    v6 = male; v7 = hsgrad; v8 = lesscol; v9 = colgrad; v10 = fincreal;
    v11 = finc2; v12 = tv; v13 = computer; v14 = soundcp; v15 = vcrp;
    v16 = vehq; v17 = manager; v18 = teacher; v19 = prof; v20 = admin;
    v21 = tech; v22 = sales; v23 = service; v24 = fam_size; v25 = nchle11;
    v26 = nch1217; v27 = persot64; v28 = hw; v29 = single; v30 = hwyng;
    v31 = hwchbs; v33 = hwchas; v34 = spchbs;
    v37 = retired; v38 = headwrk; v39 = spouwrk; v40 = incweek1;
    v41 = inc_hrs1;  v42 = incweek2; v43 = inc_hrs2; v44 = owner; v45 = renter;
    v46 = ne; v47 = mw; v48 = west; v49 = urban; v50 = msa;
    v51 = ps4mil; v52 = ps1mil; v53 = ps330k; v54 = ps125k; i = 1;

    drop Intercept age age2 white black male hsgrad lesscol colgrad
        fincreal finc2 tv computer soundcp vcrp vehq 
        manager teacher prof admin tech sales service
        fam_size nchle11 nch1217 persot64 hw single
        hwyng hwchbs hwchas spchbs retired
        headwrk spouwrk incweek1 inc_hrs1 incweek2 inc_hrs2
        owner renter ne mw west urban msa ps4mil ps1mil ps330k ps125k;
run;
proc sort;
    by i;
run;

data all;
    set all;
    i = 1;
run;
proc sort;
    by i;
run;

data all;
    merge all bfafest1;
    by i;
run;

data all;
    set all;
    psall = probnorm(v1 + age*v2 + age2*v3 + white*v4 + black*v5 + male*v6
        + hsgrad*v7 + lesscol*v8 + colgrad*v9 + fincreal*v10 + finc2*v11
        + tv*v12 + computer*v13 + soundcp*v14 + vcrp*v15 + vehq*v16 + manager*v17
        + teacher*v18 + prof*v19 + admin*v20 + tech*v21 + sales*v22
        + service*v23 + fam_size*v24  + nchle11*v25 + nch1217*v26 
        + persot64*v27 + hw*v28 + single*v29 + hwyng*v30 + hwchbs*v31 
        + hwchas*v33 + spchbs*v34 
        + retired*v37 + headwrk*v38 + spouwrk*v39 + incweek1*v40 
        + inc_hrs1*v41 + incweek2*v42 + inc_hrs2*v43 + owner*v44 
        + renter*v45 + ne*v46 + mw*v47 + west*v48 + urban*v49 + msa*v50 
        + ps4mil*v51 + ps1mil*v52 + ps330k*v53 + ps125k*v54);
    drop v1-v31 v33-v34 v37-v54;
run;
*/



/**********/
data traf;
    set all;
    if int = 1;
    if napster = 1;
run;
data trbf;
    set all;
    if int = 1;
    if napster = 0;
run;

data ctaf;
    set all;
    if int = 0;
    if napster = 1;
run;

data ctbf;
    set all;
    if int = 0;
    if napster = 0;
run;




filename traf 'treatment_after.dat';
filename trbf 'treatment_before.dat';
filename ctaf 'control_after.dat';
filename ctbf 'control_before.dat';


data traf;
    set traf;
    file traf;
    put weight psb psa cdall newid intno;
run;

data trbf;
    set trbf;
    file trbf;
    put weight psb psa cdall;
run;

data ctaf;
    set ctaf;
    file ctaf;
    put weight psb psa cdall;
run;

data ctbf;
    set ctbf;
    file ctbf;
    put weight psb psa cdall;
run;



