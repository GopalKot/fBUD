# fBUD
Repo for "Uncertainty Directed Factorial Clinical Trials"

There are three R files:
- fBUD_runscript.R: Code to run the fBUD trial design for a single simulated trial.  For education/illustration and familiarization with the design
- fBUD_runscript_multiple.R: Code to run multiple fBUD trials, parallelized on a cluster, and produce plots shown in the paper.  The three sections simulate the trial designs, generate the plots from the real data example and then conduct the asymptotic analysis.
- fBUD_functions: Most of the source code.  The main sections are "functions for trial simulation", "functions for analysis after the trial", "functions for generating plots" and "functions for asymptotic analysis".  The functions that implement the key algorithms from the paper are "importance_sample", the information metric functions "info_[tracevar/ent/var/teff]", "calculateinformation" and "expectedinformation".  
