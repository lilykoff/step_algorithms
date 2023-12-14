## Step one: download data and process into unified format 

-   `step_oneA_download_process_clemson_data.R`

    -   Run to download and process Clemson ped eval data from [here](https://cecas.clemson.edu/~ahoover/pedometer/)

    -   output: `data/processed/clemson_ped.csv.gz` and `raw/clemson` with original files

-   `step_oneB_download_process_oxwalk_data.R`

    -   Run to download and process Oxwalk data from [here](https://ora.ox.ac.uk/objects/uuid:19d3cb34-e2b3-4177-91b6-1bad0e0163e7)

    -   output: `data/procesed/ox_data.csv.gz` and `raw/oxwalk` folder with original files

## Step two: process data to be able to use stepcount algorithm

-   `step_two_process_data_for_stepcount.R`

    -   Run to create separate csvs for each ID and study setting with columns `time`, `x`, `y`, `z`, store in `data/processed/stepcount_data`

## Step three: run all step counting algorithms 

-   `step_threeA_fit_algos.R`

    -   Runs Oak, ADEPT, Verisense and SDT on the data, saves in `results/steps_clemson.csv` and `results/steps_ox.csv`

-   `step_threeB_fit_stepcount.sh`

    -   Runs `command.sh` to run stepcount algorithm on all data and outputs results to `results/stepcount`

-   `step_threeC_process_stepcount_results.R`

    -   Reads in files in `results/stepcount` and gets them in unified format (like the other algorithms), output is `results/stepcount_all_clemson.csv` and `results/stepcount_all_ox.csv`

## Step four: combine all step counting algorithms

Since `stepcount` gives results in 10 second epochs, summarize other algorithms at 10 second level and join

-   `step_four_combine_results.R` combines at 10 second level, output is `results/clemson_joined_steps.csv` and `results/ox_joined_steps.csv`
