# The Rational Agent Benchmark for Data Visualization

This repository contains supplemental materials for the manuscript, The Rational Agent Benchmark for Data Visualization by Yifan Wu, Ziyang Guo, Michalis Mamakos, Jason Hartline, and Jessica Hullman.

## Contents

Contents include data generating processing, analysis script, and the code used to generate the figures and statistics presented in the paper for one toy example and two demonstrations with two visualization experiments. Some of the generated files such as intermediate data sets and model fit objects are intentionally left out of the repo due to file size (model fit objects can be accessed on [Google drive](https://drive.google.com/file/d/1a6LIKQurkeWNNhH7UjuJkQ6n66lbKghw/view?usp=sharing)). 

forecast_example/ - *Materials for the weather forecast example*
 - example.Rmd: Code containing data-generating model, stimuli generation, and pre-experiment analysis for the hypothetical weather forecast experiment.

demonstrations/ - *Materials for the demonstrations of the framework on Kale et al. (2020) and Fernandes et al. (2018)*
 - effect_size/ - *Materials for Kale et al. (2020) demonstration*
   - analysis.Rmd: Code used to analyze Kale et al. result, including three parts...
     - Code adapted from repository for Kale et al. (2020), used to generate behavioral agent responses (model predictions)
     - Code used to simulate the rational framework and calculate the payoffs for each agent.
     - Code used to generate visualizations related to Kale et al. results used in the paper.
   - data/
     - experimental data from Kale et al. paper.
 - transit_decisions - *Materials for Fernandes et al. (2018) demonstration*
   - analysis.Rmd: Code used to analyze Fernandes et al. result, including three parts...
     - Code used to generate behavioral agent responses (model predcitions).
     - Code used to simulate the rational framework and calculate the payoffs for each agent.
     - Code used to generate visualizations related to Fernandes et al. used in the paper.
   - model_check.R: Custom functions for plotting model predictions against observed data, used to check the models we fit to Fernandes et al. data.
   - data/
     - anonymized experimental data from Fernandes et al. paper.
