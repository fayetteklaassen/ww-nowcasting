# Read me

This is a repository containing the code, and, upon publication, data to reproduce the analyses for the manuscript:

Klaassen, F, Holm, RH, Smith, T, Cohen, T, Bhatnagar, A, Menzies, NA. (2023). Predictive power of wastewater for nowcasting infectious disease transmission: a retrospective case study of five sewershed areas in Louisville, Kentucky. *Environmental Research, 240* (2024) 117395.

(link)[https://www.sciencedirect.com/science/article/pii/S0013935123021990]

The files and folders in this repository can be used in the following steps:

## 1. Data (hidden)

- /data-sources, contains the raw data files
- A make file that uses the following scripts to render cleaned and filtered data
- /R/clean-<targets>.R, scripts to clean each of the data files
- /R/gather-target.R, script to gather all the data files in a single data frame
- /R/filter-target.R, script to preselect the relevant variables and dates from the data frame, to be analyzed

This step results in a dataframe with the following variables
- shedID  (character, ID for the sewershed)
- date  (Date)
- cases   (integer, reported total cases)
- deaths  (integer, reported total deaths, using probabilistic imputation by populationsize)
- deaths2 (integer, reported total deaths, using probabilistic imputation by reported COVID-19 risk)
- N1      (numeric, average SARS-CoV-2 (N1) (copies/ml))
- ratio   (numeric, average SARS-CoV-2 (N1) (copies/ml) divided by average PMMoV (copies/ml))
- N1flow  (numeric, average SARS-CoV-2 (N1) (copies/ml) divided by average flow (MGD))
- pop     (numeric, population size)

An additional two data frames contain the by sewershed demographic information (population size, area, sewershed name); and the serosurvey data, coded by aggregated sewershed and date.

All cleaned and analyzed data are stored (hidden) in `/data-products`
The data that can be shared will be made public after publication. Death data are not publicly available but may be requested from Louisville Metro Department of Public Health and Wellness (LMPHW). Publicly available data (see above, excluding the deaths data) are stored in `/data-products/data-public.csv`. 

## 2. Preparing data for covidestim
We used the `covidestim` package, which has been adapted for the current model in the `wastewater` branch. This can be installed using the following commands in the terminal:
`git clone `http://github.com/covidestim/covidestim`
`git checkout wastewater`

The script `R/generate-configs-main-analyses.R` and `R/generate-configs-deaths-imputation.R` can be used to generate the covidestim configuration objects that are used for the final analyses. Main analyses contains the configurations for both the N1 and N1 with outlier adjustment analyses; the deaths imputation file is used for the additional deaths imputation method.

## 3. Running the covidestim model
The script `R/run-covidestim.R` and `R/run-covidestim-deaths.R` can be used to run the covidestim models. For the manuscript, we produced these results on a computing cluster. The script accesses a folder `configs` where the configuration objects are saved, and a folder `stan` where the stan scripts are saved. 
The folder `/stan` contains four scripts for the four models in the manuscript.
Running this script will output the results in a folder `results/` and `results-deaths/`.

## 4. Extracting and analyzing the results
To create simplified result data frames, run the script `R/extract-results.R`.
To recreate the figures and analyses for the manuscript, run the script `R/manuscript-results.R`
The supplementary file `R/wis.R` is called within this script, with functions to calculate the predictive assessment quantities.