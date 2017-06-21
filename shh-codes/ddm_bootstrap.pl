#!/usr/bin/perl15 -w# perl code that manages the bootstrap processes.  # last updated: 09/09/08# need to create empty "bs_sample.dat" file before running.# setting up simulation counters$simct = 1;$simtot = 50;# create a blank output file


# estimation for original sample
system "sas propensity_score.sas";

system "f90 llm.f90 -o llm.out";

system "./llm.out";
# running the main LOOPwhile ($simct < $simtot+1) {    print "Running Simulation Number $simct out of $simtot \n";    # resampling for i-th bootstrap    system "sas bootstrap_ddm.sas";

    # compile fortran code for ddm
    system "f90 bootstrap_llm.f90 -o bootstrap_llm.out";

    # run fortran code
    system "./bootstrap_llm.out";

    # combine new bootstrap results with the previous ones
    system "cat bs_sample.dat llm_bootstrap.dat --> combined.dat";

    system "mv combined.dat bs_sample.dat";
    $simct++;}


# compute s.e.
system "sas bootstrap_se.sas";

# End of main LOOP