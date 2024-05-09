# README

This repository contains the code associated with the article:

<i> Improving Equivalent Scores for Clinical Neuropsychology: A new method for regression model selection </i>

by Giorgio Arcara (2024)

The main code included in this repository is contained in the `R_functions` and `R_scripts` folder.  

**I recommend to download the whole code folder as it is, and launch the code contained in the `R_scripts` folder, starting from STEP1**. See also the **Instructions** paragraph below.

Please note that to be sure that the code works all necessary packages should be installed (you may look the `required_packages.R` script to check them). All simulations reported in the manuscript were run using R version 4.3.1 (2023-06-16). Platform: aarch64-apple-darwin20 (64-bit) Running under: macOS Ventura 13.0.


<br>

The folder is structured as follow:

* `Figures`: the folder already that contains the figures that results as output of launching the code of the simulation. (in `R_scripts`).
* `Original_Data`: is a folder that contains the data used for some applications of the code (in particular for the R_scripts/Analysis3)
* `R_functions`: contains the custom functions used to simulate the data.
* `R_scripts`: contains the scripts used for the analyses of the article. This folder contains several subfolders:
  * `Analysis1` contains the main analysis comparing Capitani and Laiacona methods with Arcara2024 method (see Manuscript and Supplementeary Materials for details).
  * `Analysis2`: contains analysis with simulated data using empirical probabilities of combinationof age, education, and sex values as generated from three real datasets.
  * `Analysis3`: checks the performance of the method using a different ratio of participants with impaired performance (simulating the condition of a clinical setting in which a higher proportion of participants with low performance is expected)
  * `Analysis4`: checks the performance of the methods including also a null model with only the Intercept included.
  * `Analysis5`: compare the performance of different versions of the two methods (see Supplementary Materials for details available at https://osf.io/yma69/).
  * `DEMO`: contains some demo of the comparisons between method and of the simulaton of normative data.
  * `OTHERS`: contains other miscellaneous scripts.
* `Results`: the folder is empty, but it will contains the `.RData` files containing the results of the simulations. The results will have a fairy large size (about 30 Gb), so take this into account before launching the code of the stimulation.

## INSTRUCTIONS

To replicate the analysis of the article follow these steps.

1. Download the whole folder containing the code.
2. check the `required_packages.R` folder and if you miss some packages launch the corresponding code.
3. Open R and set the correct path (either using the R project link or setting the working directory <i>inside</i> the whole code folder)
4. go to the `R_scripts/Analysis1`folder and launch `STEP1_loop.R`. This will launch, over a parameter space defined in the script, several time the `STEP2_simulation.R` (you may change the parameters in `STEP1_loop.R` to explore a different parameter space)
5. After the simulation is over (it may take few hours). Launch the remaining scripts (from `STEP3_..` to `STEP9..`, for further plots and summary results)


For any problem or question please don't hesitate to contact me at: <a> giorgio.arcara@gmail.com </a>


