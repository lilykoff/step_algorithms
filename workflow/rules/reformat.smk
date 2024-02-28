configfile: "workflow/paths.yaml"
import os
if os.path.exists("data/raw/MAREA_dataset/"):
    ruleorder: reformat > reformat_nom
else: 
    ruleorder: reformat_nom > reformat 


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

rule reformat: 
    input: 
        script = config["code_stepcounts"] + "step_03_reformat_data_for_actilife_stepcount.R",
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
        expand(config["out_acti"] + "{dataname}/{dataname}-{id}-walk_{type}-{sample_details}.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"]), 
        expand(config["out_acti"] + "{dataname}/{dataname}-{id}-{type}-{sample_details}.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], 
        sample_details=["raw128Hz", "resampled128to30Hz"]),
        expand(config["out_acti"] + "{dataname}/{dataname}-{id}-{type}-{sample_details}.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)],type = ["outdoor_walkrun"], sample_details=["raw128Hz", "resampled128to30Hz"]),
        expand(config["out_acti"] + "{dataname}/{dataname}-{id}-{sample_details}.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"]),
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-walk_{type}-{sample_details}.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"]), 
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-{type}-{sample_details}.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], 
        sample_details=["raw128Hz", "resampled128to30Hz"]),
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-{type}-{sample_details}.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], type = ["outdoor_walkrun"], sample_details=["raw128Hz", "resampled128to30Hz"]),
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-{sample_details}.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"])
    shell:
        "Rscript {input.script} {input.data} {output}" 

rule reformat_nom: 
    input: 
        script = config["code_stepcounts"] + "step_03_reformat_data_for_actilife_stepcount.R",
        data = (expand(config["out_reorganized"] +    "{dataname}/{id}/{dataname}-{id}-walk_{type}-{sample_details}.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"]),   
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{sample_details}.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"]))
    output: 
        expand(config["out_acti"] + "{dataname}/{dataname}-{id}-walk_{type}-{sample_details}.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"]), 
        expand(config["out_acti"] + "{dataname}/{dataname}-{id}-{sample_details}.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"]),
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-walk_{type}-{sample_details}.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz", "resampled15to30Hz"], type = ["regular", "irregular", "semiregular"]), 
        expand(config["out_sc"] + "{dataname}/{dataname}-{id}-{sample_details}.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz", "resampled25to30Hz", "resampled100to30Hz"])
    shell:
        "Rscript {input.script} {input.data} {output}" 
