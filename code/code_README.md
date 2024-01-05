## Step one: download data and process into unified format

-   `step_one_download_process_clemson_data.R`

    -   Run to download and process Clemson ped eval data from [here](https://cecas.clemson.edu/~ahoover/pedometer/)

    -   output: `data/reorganized/clemson` folder with subfolders for each individual and activity with naming convention `clemson-<subjectID>-<activity>-raw15Hz.csv.gz`

-   `step_one_download_process_oxwalk_data.R`

    -   Run to download and process Oxwalk data from [here](https://ora.ox.ac.uk/objects/uuid:19d3cb34-e2b3-4177-91b6-1bad0e0163e7)

    -   output: `data/reorganized/oxwalk` folder with subfolders for each individual and sample rate (25 or 100Hz) with naming convention `oxwalk-<subjectID>-raw<sample_rate>Hz.csv.gz`

-   `step_one_download_process_marea_data.R`

    -   Run to download and process MAREA data from [here](https://wiki.hh.se/caisr/index.php/Gait_database) (after signing release agreement and obtaining data)

    -   output: `data/reorganized/marea` folder with subfolders for each individual and activity with naming convention `marea-<subjectID>-<activity>-raw128Hz.csv.gz`

## Step two: resample all data to 30 Hz

-   `step_two_resample_data.R`

    -   Run to resample all of the data to 30 Hz

    -   Dependencies: `resample_utils.R`

    -   outputs:

        -   Additional files in each of the `data/reorganized` subfolders
        -   Naming convention `<study>-<subjectID>-<activity>-resampled<original_sample_rate>to30Hz.csv.gz`

## Step three: special data processing for stepcount and actilife algorithms

-   `step_three_process_data_for_stepcount.R`

    -   For stepcount algorithm, data need to be `csv` or `csv.gz` with columns `time` (character format) and `x`, `y`, `z`
    -   Outputs:
        -   `csv.gz` files in `data/stepcount` from both raw and resampled data, separate subfolders for each study

-   `step_three_process_data_for_actilife.R`

    -   For actilife algorithm, data need to be in `csv` or `csv.gz` with specific header

    -   Outputs:

        -   `csv.gz` files in `data/actilife` from raw and resampled data with subfolders for each study

## Step four: run all step counting algorithms

-   `step_four_fit_adept_oak_sdt_vs.R`

    -   Runs Oak, ADEPT, Verisense and SDT on all of the raw and resampled data
    -   All of these algorithms can be implemented in R, so they are done at the same time
    -   Outputs:
        -   In each `data/reorganized` study and invididual subfolder, there is an additional subfolder `step_estimates` with step estimates by epoch
        -   Naming convention \`<study>-<subjectID>-<activity>-<raw/resampled><sample_rate>-steps\_<algorithm>.csv

-   `bash/step_four_fit_stepcount.sh`

    -   Shell script to run stepcount in conda environment
    -   Outputs:
        -   `results/stepcount` with study and individual subfolders

-   Actilife needs to be run through their proprietary software

    -   Actilife results in `results/actilife`

## Step five: process step counts from stepcount, ActiLife to make in unified format

-   `step_five_process_stepcount_results.R`

    -   Reads in files in `results/stepcount` and gets them in unified format (like the other algorithms), outputs are individual files in `data/reorganized/study/subject/step_estimates`

-   `step_five_process_actilife_results.R`

-   Reads in files in `results/stepcount` and gets them in unified format (like the other algorithms), outputs are individual files in `data/reorganized/study/subject/step_estimates`

## Step six: join all step estimates by second and nest acceleration data in second-level data frames

-   `step_six_join_and_nest.R`

    -   Reads in files in `data/reorganized` and joins step estimates with raw acceleration data 
    -   Output in `data/processed` subfolders
    -   Second and 10-second level step summaries in `results/all_algorithms`

