# Preprocessing

For preprocessing the data go to /src/ and run the preprocessing.R script.
In the script there are 4 functions for different preprocessing steps. 

preprocess_vanilla() -> just cleaning data, no removal

preprocess_low_grass() -> removes plots with only grassland, because it holds less relevant information

preprocess_no_lowvalue_crops() -> removes plots with only grassland and crop types without any value (no info, mixed crops etc)

preprocess_no_lowvalue_crops_with_price() -> removes plot with only grassland and crop types without any value and adds price information

# Model building

Model building is done in the model.R file.

Specify the cleaned data in line 13.

Split data, run model and save rdata of model.

# Model comparison

In the compare_models.R script, all 4 models are built and saved as .rdata

# Exploration

Some data exploration was done in exploration.R.

# Sampling

sampling is done in /src/sampling

In sample_data.R you can specify a sample range in line 13. The data cleaning process for vanilla data (without removing any crop types) will be done for the specified sample sizes and saved as .rdata.
In model_samples.R the saved samples will be compared based on accuracy.

# Simulation

Climate simulation will be done in /src/simulation/ 

Run main.R for simulation, preprocessing of simulated data and then update the data.

In preprocessing_simultion.R you can specify for how many years the simulation will be going.
