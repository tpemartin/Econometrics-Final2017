#!/usr/bin/perl15 -w


# estimation for original sample
system "sas propensity_score.sas";

system "f90 llm.f90 -o llm.out";

system "./llm.out";


    # compile fortran code for ddm
    system "f90 bootstrap_llm.f90 -o bootstrap_llm.out";

    # run fortran code
    system "./bootstrap_llm.out";

    # combine new bootstrap results with the previous ones
    system "cat bs_sample.dat llm_bootstrap.dat --> combined.dat";

    system "mv combined.dat bs_sample.dat";



# compute s.e.
system "sas bootstrap_se.sas";

