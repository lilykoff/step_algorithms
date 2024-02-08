configfile: "workflow/paths.yaml"
import os 
if os.path.exists("data/raw/MAREA_dataset/"):
   ruleorder: fit_stepcount > fit_stepcount_nom
   ruleorder: fit_algorithms > fit_algorithms_nom
else: 
   ruleorder: fit_stepcount_nom > fit_stepcount 
   ruleorder: fit_algorithms_nom > fit_algorithms

# https://carpentries-incubator.github.io/workflows-snakemake/09-cluster/index.html#:~:text=To%20run%20Snakemake%20on%20a,single%20rule%20and%20then%20exits.
runR = "Rscript --no-save --no-restore --verbose"
# can use "script: " instead? 
datanames = ['clemson', 'marea', 'oxwalk']
rawnames = ['clemson', 'MAREA_dataset', 'oxwalk']
methods = ["oak", "vs", "sdt", "truth", "adept"]
methods2 = ["oak", "vs", "sdt", "adept"]

methodssc = ["stepcountrf", "stepcountssl"]
methods_ea = ["oak", "vs", "sdt", "truth", "adept", "stepcountrf", "stepcountssl"]
methods_all = ["oak", "vs", "sdt", "truth", "adept", "stepcountrf", "stepcountssl", "actilife"]
methods_et = ["oak", "vs", "sdt",  "adept", "stepcountrf", "stepcountssl", "actilife"]

rule fit_stepcount: 
   input:
        script = config["code_stepcounts"] + "step_04_fit_stepcount.R",
        data = (expand(config["out_sc"] + "{dataname}/{dataname}-{id}-walk_{type}-{sample_details}.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"]), 
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-{type}-{sample_details}.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], 
        sample_details=["raw128Hz", "resampled128to30Hz"]),
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-{type}-{sample_details}.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], sample_rate=128, type = ["outdoor_walkrun"], sample_details=["raw128Hz", "resampled128to30Hz"]),
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-{sample_details}.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"]))
   output: 
        (expand(config["out_reorganized"] +   "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"], method = methodssc),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], 
        sample_details=["raw128Hz", "resampled128to30Hz"], method = methodssc),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)],type = ["outdoor_walkrun"], sample_details=["raw128Hz", "resampled128to30Hz"], method = methodssc),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"], method = methodssc))
   shell: 
        "Rscript {input.script} {input.data} {output}" 

rule fit_algorithms: 
    input: 
        script = config["code_stepcounts"] + "step_04_fit_algorithms.R",
        data = (expand(config["out_reorganized"] +    "{dataname}/{id}/{dataname}-{id}-walk_{type}-{sample_details}.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"]),   
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{type}-{sample_details}.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], 
        sample_details=["raw128Hz", "resampled128to30Hz"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{type}-{sample_details}.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], type = ["outdoor_walkrun"], sample_details=["raw128Hz", "resampled128to30Hz"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{sample_details}.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"]))
    output: 
        (expand(config["out_reorganized"] +    "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"], method = methods2),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], 
        sample_details=["raw128Hz", "resampled128to30Hz"], method = methods2),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], type = ["outdoor_walkrun"], sample_details=["raw128Hz", "resampled128to30Hz"], method = methods2),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"], method = methods2),
        expand(config["out_reorganized"] +    "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz"], type = ["regular", "irregular", "semiregular"], method = "truth"),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], 
        sample_details=["raw128Hz"], method = "truth"),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], type = ["outdoor_walkrun"], sample_details=["raw128Hz"], method = "truth"),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz"], method = "truth"))
    shell: 
        "Rscript {input.script} {input.data} {output}" 

rule fit_stepcount_nom: 
   input:
        script = config["code_stepcounts"] + "step_04_fit_stepcount.R",
        data = (expand(config["out_sc"] + "{dataname}/{dataname}-{id}-walk_{type}-{sample_details}.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"]), 
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-{sample_details}.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"]))
   output: 
        (expand(config["out_reorganized"] +   "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"], method = methodssc),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"], method = methodssc))
   shell: 
        "Rscript {input.script} {input.data} {output}" 

rule fit_algorithms_nom: 
    input: 
        script = config["code_stepcounts"] + "step_04_fit_algorithms.R",
        data = (expand(config["out_reorganized"] +    "{dataname}/{id}/{dataname}-{id}-walk_{type}-{sample_details}.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"]),   
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{sample_details}.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"]))
    output: 
        (expand(config["out_reorganized"] +    "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"], method = methods2),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"], method = methods2),
        expand(config["out_reorganized"] +    "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz"], type = ["regular", "irregular", "semiregular"], method = "truth"),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz"], method = "truth"))
    shell: 
        "Rscript {input.script} {input.data} {output}" 