# Ordinal_binary is a repository containing all of the code for simulating the behavior of ordinal predictors with binary responses under certain conditions. 
Initial parameters needs to be run first, and it has the info on which distributions we are testing. Changing degree of multinomials or labels will require editing this. 
The risks and probs needs to be run first, then run run_*, then expand 

Script
distribution_graphs.R Graphs distributions for presentations/ explanations
initial_parameters.R Sets requisite parameters for runs
risks_and_probs_gen.R generates the distributional information upon which the simulation relies
run_*.R contains appropriate code for the listed platform
model_building.R evaluates the fit of various models/ residuals to see if models are appropriate

