## Step one: download data and process into unified format

-   `step_one_download_process_clemson_data.R`

    -   Run to download and process Clemson ped eval data from [here](https://cecas.clemson.edu/~ahoover/pedometer/)

    -   output: `data/processed/clemson_ped.csv.gz` and `raw/clemson` with original files

-   `step_one_download_process_oxwalk_data.R`

    -   Run to download and process Oxwalk data from [here](https://ora.ox.ac.uk/objects/uuid:19d3cb34-e2b3-4177-91b6-1bad0e0163e7)

    -   output: `data/procesed/ox_data.csv.gz` and `raw/oxwalk` folder with original files

## Step two: resample all data to 30 Hz

-   `step_two_resample_data.R`

    -   Run to resample all of the data to 30 Hz

    -   Dependencies: `resample_utils.R`

    -   outputs:

        -   Individual files for each subject and study to `data/raw_resampled` with naming convention `acc_P<subject_ID><original_sample_rate>to<new_sample_rate>.csv.gz` for Oxwalk, `acc_<subject_ID>_<walking_type>_<original_sample rate>to<new_sample_rate>Hz.csv.gz` for Clemson

        -   `data/processed/ox_data_resampled.csv.gz` and `data/processed/clemson_ped_resampled.csv.gz` - same as output files from step one, but resampled to 30Hz

## Step three: special data processing for stepcount algorithm

-   `step_three_process_data_for_stepcount.R`

    -   For stepcount algorithm, data need to be `csv` or `csv.gz` with columns `time` (character format) and `x`, `y`, `z`
    -   Outputs:
        -   `csvs` in `data/stepcount/original` and `data/stepcount/resampled`
        -   `command_table.csv, command_table_resampled.csv` with commands to make bash files to run stepcount

## Step three: special data processing for actilife algorithm

-   `step_four_process_data_for_actilife.R`

    -   For actilife algorithm, data need to be in `csv` or `csv.gz` with specific header

    -   Outputs:

        -   `csvs` in `data/actilife/original` and `data/actilife/resampled`

## Step four: run all step counting algorithms

-   `step_four_fit_adept_oak_sdt_vs.R`

    -   Runs Oak, ADEPT, Verisense and SDT on the original and resampled data, saves in `results/adept_oak_vs_sdt/steps_aovs_clemson.csv` and `results/steps_aovs_ox.csv` (original) and `results/adept_oak_vs_sdt/steps_aovs_clemson_resampled.csv` and `results/steps_aovs_ox_resampled.csv`

-   `step_four_fit_stepcount.sh`

    -   First make two shell scripts: `bash/command.sh` and `bash/command_resampled.sh` by taking `data/stepcount/command_table.csv` and `data/stepcount/command_table_resampled.csv` and pasting into `.sh` files

    -   Then run `bash/command.sh` and `bash/command_resampled.sh`

    -   Outputs: folders in `results/stepcount/original` and `results/stepcount/resampled`

-   Actilife needs to be run through their proprietary software

    -   Actilife results in `results/actilife/original` and `results/actilife/resampled`

## Step five: combine and standardize step counts 

-   `step_five_process_stepcount_results.R`

    -   Reads in files in `results/stepcount` and gets them in unified format (like the other algorithms), output is `results/stepcount_all_clemson.csv` and `results/stepcount_all_ox.csv`
    -   Output: `results/stepcount/processed_steps_sc_clemson.csv, results/stepcount/processed_steps_sc_clemson_resampled.csv` and `results/stepcount/processed_steps_sc_ox.csv, results/stepcount/processed_steps_sc_clemson_ox.csv`

-   `step_five_process_actilife_results.R`

    -   Reads in individual 1 second files from actilife

    -   Output: `results/actilife/processed/steps_acti_clemson.csv,``results/actilife/processed/steps_acti_clemson_resampled.csv` and `results/actilife/processed/steps_acti_ox.csv,``results/actilife/processed/steps_acti_ox_resampled.csv`

## Step six: combine all step counting algorithms

Since `stepcount` gives results in 10 second epochs, we have two different summarized data sets

-   `results/ox_steps_1sec.csv` and `results/clemson_steps_1sec.csv` have summaries of all algorithms except stepcount at 1 second level
-   `results/ox_steps_10sec.csv` and `results/clemson_steps_10sec.csv` have summaries of all algorithms at 10 second level
-   Column names are `steps_<algorithm>`, `steps_<algorithm>_30` for resampled data

## Step seven: compute summaries 
