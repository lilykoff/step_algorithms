# step_algorithms

## Description 
Comparison of step-counting algorithms on three datasets:

+ [Clemson ped eval](https://cecas.clemson.edu/~ahoover/pedometer/)
+ [Ox Walk](https://ora.ox.ac.uk/objects/uuid:19d3cb34-e2b3-4177-91b6-1bad0e0163e7)
+ [MAREA](https://wiki.hh.se/caisr/index.php/Gait_database)

## Algorithms 

+ ADEPT
+ Oak
+ Verisense
+ Step detection threshold (SDT)
+ Stepcount 
+ ActiLife


## Code 

+ `code/R/get_stepcounts` downloads data and fits algorithms 
+ `code/R/analyze_stepconts` analyzes results with respect to accuracy of walking recongition and step counting
+ More details in [`code/code_README.md`](https://github.com/lilykoff/step_algorithms/blob/main/code/code_README.md)
