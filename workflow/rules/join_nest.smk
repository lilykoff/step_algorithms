configfile: "workflow/paths.yaml"
import os
if os.path.exists("data/raw/MAREA_dataset/"):
    ruleorder: combine_results > combine_results_nom
else: 
    ruleorder: combine_results_nom > combine_results


# https://carpentries-incubator.github.io/workflows-snakemake/09-cluster/index.html#:~:text=To%20run%20Snakemake%20on%20a,single%20rule%20and%20then%20exits.
runR = "Rscript --no-save --no-restore --verbose"
# can use "script: " instead? 
datanames = ['clemson', 'marea', 'oxwalk']
datanames2 = ['clemson', 'oxwalk']
rawnames = ['clemson', 'MAREA_dataset', 'oxwalk']
methods = ["oak", "vs", "sdt", "truth", "adept"]
methods2 = ["oak", "vs", "sdt", "adept"]

methodssc = ["stepcountrf", "stepcountssl"]
methods_ea = ["oak", "vs", "sdt", "truth", "adept", "stepcountrf", "stepcountssl"]
methods_all = ["oak", "vs", "sdt", "truth", "adept", "stepcountrf", "stepcountssl", "actilife"]
methods_et = ["oak", "vs", "sdt",  "adept", "stepcountrf", "stepcountssl", "actilife"]




rule combine_results: 
    input: 
        script = config["code_stepcounts"] + "step_06_join_and_nest.R",
        data = (expand(config["out_reorganized"] +   "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz"], type = ["regular", "irregular", "semiregular"], method = methods_ea),
        expand(config["out_reorganized"] +   "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["resampled15to30Hz"], type = ["regular", "irregular", "semiregular"], method = methods_et),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], 
        sample_details=["resampled128to30Hz"], method = methods_et),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], 
        sample_details=["raw128Hz"], method = methods_all),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], type = ["outdoor_walkrun"], sample_details=["resampled128to30Hz"], method = methods_et),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{type}-{sample_details}-steps_{method}.csv", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], type = ["outdoor_walkrun"], sample_details=["raw128Hz"], method = methods_all),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz"], method = methods_ea),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw100Hz", "resampled25to30Hz", "resampled100to30Hz"], method = "actilife"))
    output: 
        (expand(config["out_proc"] +  "{dataname}/{id}/{dataname}-{id}-walk_{type}-nested.rds",  dataname = "clemson",
        id = ["P{:02d}".format(i) for i in range(1, 31)], type = ["regular", "irregular", "semiregular"]),
        expand(config["out_proc"] + "{dataname}/{id}/{dataname}-{id}-{type}-nested,rds", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"]),
        expand(config["out_proc"] + "{dataname}/{id}{dataname}-{id}-{type}-nested,rds", dataname = "marea", 
        id = ["P{:02d}".format(i) for i in range(12, 21)], type = ["outdoor_walkrun"]),
        expand(config["out_proc"] + "{dataname}/{id}/{dataname}-{id}-{sample_details}-nested.rds", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["25Hz", "100Hz"]),
        expand(config["out_proc"] + "{dataname}_nested_all.rds", dataname = datanames),
        expand("results/all_algorithms/{dataname}_step_estimates_1sec.csv.gz", dataname = datanames))
    script: 
        "Rscript {input.script} {input.data} {output}"

rule combine_results_nom: 
    input: 
        script = config["code_stepcounts"] + "step_06_join_and_nest.R",
        data = (expand(config["out_reorganized"] +   "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["raw15Hz"], type = ["regular", "irregular", "semiregular"], method = methods_ea),
        expand(config["out_reorganized"] +   "{dataname}/{id}/step_estimates/{dataname}-{id}-walk_{type}-{sample_details}-steps_{method}.csv", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_details=["resampled15to30Hz"], type = ["regular", "irregular", "semiregular"], method = methods_et),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw25Hz", "raw100Hz"], method = methods_ea),
        expand(config["out_reorganized"] + "{dataname}/{id}/step_estimates/{dataname}-{id}-{sample_details}-steps_{method}.csv", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["raw100Hz", "resampled25to30Hz", "resampled100to30Hz"], method = "actilife"))
    output: 
        (expand(config["out_proc"] +  "{dataname}/{id}/{dataname}-{id}-walk_{type}-nested.rds",  dataname = "clemson",
        id = ["P{:02d}".format(i) for i in range(1, 31)], type = ["regular", "irregular", "semiregular"]),
        expand(config["out_proc"] + "{dataname}/{id}/{dataname}-{id}-{sample_details}-nested.rds", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_details = ["25Hz", "100Hz"]),
        expand(config["out_proc"] + "{dataname}_nested_all.rds", dataname = datanames2),
        expand("results/all_algorithms/{dataname}_step_estimates_1sec.csv.gz", dataname = datanames2))
    script: 
        "Rscript {input.script} {input.data} {output}"