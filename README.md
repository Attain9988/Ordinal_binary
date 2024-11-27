# Ordinal_binary is a repository containing all of the code for simulating the behavior of ordinal predictors with binary responses under certain conditions. 
Initial parameters needs to be run first, and it has the info on which distributions we are testing. Changing degree of multinomials or labels will require editing this. 
The multinomials need to be run before risk. graphs require multinomials and risks, as does risks and probs, then run simulations, then expand simulations, then model failure and coverage

Script
distribution_graphs.R Graphs distributions for presentations/ explanations
initial_parameters.R Sets requisite parameters for runs
multinomial_gen.R generates the multinomials
risk_gen.R generates the risk of success per level distributions
risk_and_probs_gen.R generates the combinations of risks and multinomials, the 
    "distributions"

playing_around_with_structure.R inital combo file of everything. Basis for the tests 
  and all future generation
test_run.R first 50 to be run on the cluster
test_run2.R next 50 to be run on cluster
test_run_laptop 50 to test on laptop
test_run_single testing a single distribution


Functions
multinomial_distrib_fcts.R gives functions needed to make the multinomials
risk_distrib_fcts.R gives functions for risk generation
score_interval_hand my score interval function
score_interval_prop_test.R score interval from the prop.test function
finding_fcts functions that may be useful when assembling data to graph

Need to remove categoric and figure out why weird in expand.