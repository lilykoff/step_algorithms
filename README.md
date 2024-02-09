# step_algorithms

## Description 
Comparison of step-counting algorithms on three datasets:

+ [Clemson Ped-Eval](https://cecas.clemson.edu/~ahoover/pedometer/)
+ [Ox Walk](https://ora.ox.ac.uk/objects/uuid:19d3cb34-e2b3-4177-91b6-1bad0e0163e7)
+ [MAREA](https://wiki.hh.se/caisr/index.php/Gait_database)

## Algorithms 

+ ADEPT: https://doi.org/10.1093/biostatistics/kxz033, https://cran.r-project.org/package=adept, https://github.com/martakarass/adept
+ Oak: https://doi.org/10.1038/s41746-022-00745-z, https://github.com/onnela-lab/forest
+ Verisense: https://github.com/ShimmerEngineering/Verisense-Toolbox/tree/master/Verisense_step_algorithm adapted from https://ieeexplore.ieee.org/abstract/document/7885036
+ Step detection threshold (SDT): https://doi.org/10.1123%2Fjmpb.2021-0011
+ Stepcount: https://www.medrxiv.org/content/10.1101/2023.02.20.23285750v1, https://github.com/OxWearables/stepcount
+ ActiLife: https://actilife.theactigraph.com/support/software/actilife/


## R Packages Used

* `walking`: https://github.com/muschellij2/walking
* `adept`: https://github.com/martakarass/adept
* `adeptdata`: https://github.com/martakarass/adeptdata
* `stepcount`: https://github.com/jhuwit/stepcount


## Code 

+ `code/R/get_stepcounts` downloads data and fits algorithms 
+ `code/R/analyze_stepconts` analyzes results with respect to accuracy of walking recongition and step counting
+ More details in [`code/code_README.md`](https://github.com/lilykoff/step_algorithms/blob/main/code/code_README.md)

## Results
### actilife
Results from Actilife included for Clemson and OxWalk datasets 
### all_algorithms 
+ `accuracy_stats_bysubject.rds` : f1 score, precision, recall, accuracy for each subject, dataset, algorithm 
+ `step_stats_bysubject.rds` : bias and APE for each subject, dataset, algorithm
+ `total_steps_bysubject.rds` : total steps (truth) and total steps estimated by each algorithm for subject, dataset 
+ `<dataset>_step_estimates_1sec.csv.gz` : second-level step estimates for subject, dataset, and algorithm

## Manuscript

### Figures
All figures used in manuscript 


## Workflow 

To reproduce the results of our analysis completely, follow the steps below. 


### Requirements 
+ Python 3 (Python 3.6 or higher)
+ snakemake 
+ R (version 4.0.x)
+ Stepcount

To install Python follow instructions [here](https://docs.anaconda.com/free/anaconda/install/index.html)

To install snakemake: 

```
pip3 install -r requirements.txt
```
or 
```
pip install -r requirements.txt
```

To install R follow instructions [here](https://rstudio-education.github.io/hopr/starting.html)

To install stepcount follow instructions [here](https://github.com/OxWearables/stepcount/tree/main), but to make results compatible with ours use git version below: 
```
conda create -n stepcount python=3.9 openjdk pip
```

```
conda activate stepcount
```

```
pip install git+https://github.com/OxWearables/stepcount.git1afed4edaeed1d4b3483c60c0b3d8595198b863b
```

To control the packages required and version of packages used, we use `renv`. See the `renv.lock` for packages and package versions required. 

To perform the analysis, run: 
```
snakemake --cores 1 all
```

#### Notes

If no folder named `MAREA_dataset` is present in `data/raw/`, the analysis will run without including MAREA. If you have obtained access to the MAREA data, include the raw data as a folder named `MAREA_dataset` in `data/raw/` before running snakemake. 

The second-level results from actilife are included in the repository for Clemson and OxWalk data since these results cannot be obtained from an open source method. 

Workflow modeled from: 
[Deer, Lachlan and Julian Langer and Ulrich Bergmann. 2021. A Reproducible Workflow for Economics Research Using Snakemake and R](https://github.com/lachlandeer/snakemake-econ-r)


