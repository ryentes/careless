# (future) careless 1.2.2
* Changed column names in output of psychsyn_critval from ("Var1", "Var2", 
"Freq") to ("var1", "var2", "cor") for accuracy and consistency with this and
other packages. 

# careless 1.2.1
* even-odd consistency score: updated interpretation of even-odd to maintain consistency with other indices. It is now coded such that higher scores indicate higher levels of careless responding. Before, higher scores indicated lower levels of careless responding. This makes the even-odd index consistent with the other indices.
* even-odd consistency score : expanded error and warning messages to provide more information to the user.
* Intra-individual response variability (IRV): now handles NA values.
* psychometric synonym/antonym score: now handles NA values better, and is faster due to implementation of vectorization. The function now uses resampling to address issues arising from NA values.