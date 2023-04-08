# The Rational Agent Benchmark for Data Visualization

This repository contains supplemental materials for the IEEE VIS 2023 paper, *The Rational Agent Benchmark for Data Visualization*.

## Contents

Contents include data generating processing, analysis script, and the code used to generate the figures and statistics presented in the paper for one toy example and two demonstrations with two visualization experiments. Some of the generated files such as intermediate data sets and model fit objects are intentionally left out of the repo due to file size. 

forecast_example/ - *codes for stimuli and data generating and analysis in weather forecast example*
 - example.Rmd: Code used to generate data and visualization stimuli and analyze payoff function in our weather forecast example

demonstrations/ - *files pertaining to behavioral decision and belief generation, rational framework codebase, and presentation of findings from the main experiment*
 - effect_size/
   - data_generation.Rmd: code used to generate behavioral agent's belief and decision for rational framework.
   - rational_framework.py: code used to simulate the rational framework and calculate the payoffs for each agent.
   - visualization.R: code used to generate visualizations used in our paper.
   - data/
     - experimental data from Kale et al. paper and some intermediate data files.
 - transit_decisions
   - data_generation.Rmd: code used to generate behavioral agent's decision for rational framework.
   - rational_framework.py: code used to simulate the rational framework and calculate the payoffs for each agent.
   - visualization.R: code used to generate visualizations used in our paper.
   - data/
     - experimental data from Fernandes et al. paper and some intermediate data files.
