# step_algorithms

## Description 
Comparison of step-counting algorithms on three datasets:

+ [Clemson ped eval](https://cecas.clemson.edu/~ahoover/pedometer/)
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
