# The Persephone Model
An agent-based model for exploring hidden variables in bioarchaeology

*This code accompanies the paper 'Known unknowns and the osteological paradox: Why bioarchaeology needs agent-based models', by Amy Anderson and Sharon DeWitte* 


This paper (and repository) introduces the **Persephone model** for Bioarchaeology. 

Named for the character in Greek mythology who spends half of each year in the land of the living with her mother, the goddess of fertility, and the other half of her year in the land of the dead as the queen of the underworld, the Persephone model allows the user to:  

  * Specify the size of a living cohort of individuals who are born when the simulation starts.
  * Specify their age-dependent risks of encountering an experience that causes a skeletal lesion.
  * Set the magnitude and age-dependent nature of differential mortality risk associated with having a skeletal lesion.
  * Determine the group-level, age-dependent mortality regime for the cohort (a named Siler function)
  * Examine the resulting age distribution of skeletal lesions in both the surviving members of the cohort in each year of the simulation, and in the deceased members of the cohort (all of them, by the time the model completes its run).
  * Repeat this process with a new cohort.

The agent-based model (ABM) in the `Model_Core_Simulate_Cemetery.R` file generates a cohort of individuals (user specifies cohort size) at age 0 who are exposed to an annual probability of developing a skeletal lesion and an annual probability of dying. The ages at which a skeletal lesion might develop are specified by the user, as is the annual hazard of dying (specified as a named Siler function). 
The ABM produces two data sets: one that includes the number of survivors and the percent of survivors with a skeletal lesion in each year of the model run, and one that includes the age at death and lesion status of every individual in the cohort. This second data set is effectively a paleopathological data set from a simulated cemetery. 





This model is designed for several applications: 
1. To sharpen intuitions about the relationship between starting assumptions about a living population and the observable age distributions of visible stress/disease indicators in the cemetery in which that population is buried.
2. To explore the ways in which age-dependent hidden variables affect observable bioarchaeological data.
3. To simulate bioarchaeological data under known conditions in order to test the accuracy of statistical tests under these data-generating conditions.


The model parameters can be varied, and the model is easily adjusted to incorporate other variables, but in the current experiment the ages at which a new skeletal lesion might form (the lesion's developmental window) are ages 0-9; once an individual agent turns 10, they are no longer at risk of forming a new skeletal lesion. 

Statistical analyses on the simulated cemetery data are currently limited to Kaplan Meier survival analysis (individuals with lesions vs. without lesions).


1. To **load the model and run the analysis in the main text of the Known Unknowns paper**, run the file `Known Unknowns/Main_Analysis.Rmd` (or alternatively download and view the Main_Analysis.html file). 
   This produces simulated cohort and corresponding cemetery data for a 1,000-person cohort in which:
   - Individuals experience a 5% annual probability of developing a skeletal lesion between the ages of 0 and 9.
   - Cohort mortality follows the mortality regime of the Coale and Demeny West model life table for females, Level 5.
   - Skeletal lesions either:
     A. Have no effect on mortality risk, or
     B. Double an individual's mortality risk. 

   It also returns the results of a Kaplan-Meier survival analysis applied to each of these two scenarios of lesion-associated mortality risk.
   *Spoiler Alert*: Under these assumptions, survival analysis produces incorrect inferences about the relationship between skeletal lesions and mortality risk, unless individuals younger than 10 years are omitted from the analysis.


   
2. To **run the sensitivity analysis** of the model (sweeping across other mortality regimes from the Coale and Demeny West female series of model life tables, and annual lesion formation probabilities between 1% and 10%) and generate plots and tables in the supplementary tables, run the file `Known Unknowns/Sensitivity_Analysis.R`. 
   
   This script will automatically load:
   * The Model (`Model_Core_Simulate_Cemetery.R`)
   * Functions for Running and systematically saving the results of the parameter sweep in the sensitivity analysis (`Sweep_Utility_Functions.R`)
   * Functions for Plotting the results of the parameter sweep (`Sweep_Plotting_Functions.R`)
     
You should not need to open and run these three files directly in order to replicate the results of the paper.  




The code of the model can easily be extended and amended to add or adjust variables such as:  
- cohort sex ratio
- sex-specific mortality risks
- sex-specific exposure to lesion-causing risks
- mortality-associated stress that does not produce skeletal lesions
- age-specific probabilities of lesion formation
- age-dependent differences in lesion-related mortality risk
- cause-specific mortality risk and cause-specific lesion-related mortality risk
- subgroup identities with different risks of stress exposure and mortality risk
- lesion healing rate (right-censored data)
- and much, much more.






