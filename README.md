# Preprocessing

For preprocessing the data go to /src/ and run the preprocessing.R script.
In the script there are 3 functions for different preprocessing steps. 

preprocess_set1() -> just cleaning data, no removal

preprocess_set2()  -> removes plots with only grassland and crop types without any value (no info, mixed crops etc)

preprocess_set3()  -> removes plot with only grassland and crop types without any value and adds price information

# Model building

Model building is done in the model.R file.

Specify data set in line 12.

Split data, run model and save rdata of model.

# Model comparison

In the compare_models.R script, all 3 models are built and saved as .rdata

# Exploration

Some data exploration was done in exploration.R.

# Sampling

sampling is done in /src/sampling

In sample_data.R you can specify a sample range in line 11. The data cleaning process for set 1 (without removing any crop types) will be done for the specified sample sizes and saved as .rdata.
In model_samples.R the saved samples will be compared based on accuracy.

# Simulation

Climate simulation will be done in /src/simulation/ 

Run main.R for simulation, preprocessing of simulated data and then update the data.

In preprocessing_simultion.R you can specify for how many years the simulation will be going.
