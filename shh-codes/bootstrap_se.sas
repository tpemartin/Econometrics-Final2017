filename all 'bs_sample.dat';

data all;
     infile all;
     input ddm cdtraf cdtrbf cdctaf cdctbf ddm2;
run;


proc means data = all;
     var ddm cdtraf cdtrbf cdctaf cdctbf ddm2;
run;
