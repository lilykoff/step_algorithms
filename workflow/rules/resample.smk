configfile: "workflow/paths.yaml"
import os 
if os.path.exists("data/raw/MAREA_dataset/"):
    ruleorder: resample > resample_nom
else: 
    ruleorder: resample_nom > resample
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

rule resample: 
    input: 
        script = config["code_stepcounts"] + "step_02_resample_data.R",
        data = (expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-walk_{type}-raw{sample_rate}Hz.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_rate=15, type = ["regular", "irregular", "semiregular"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{type}-raw{sample_rate}Hz.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], sample_rate=128, type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"]),
         expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{type}-raw{sample_rate}Hz.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], sample_rate=128, type = ["outdoor_walkrun"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-raw{sample_rate}Hz.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_rate= [100,25]))
    output:
        (expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-walk_{type}-resampled{sample_rate}to{new}Hz.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_rate=15, type = ["regular", "irregular", "semiregular"], new=30),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{type}-resampled{sample_rate}to{new}Hz.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], sample_rate=128, type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"], new=30),
         expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{type}-resampled{sample_rate}to{new}Hz.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], sample_rate=128, type = ["outdoor_walkrun"], new=30),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-resampled{sample_rate}to{new}Hz.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_rate= [100,25], new=30))
    shell: 
        "Rscript {input.script} {input.data} {output}" 

rule resample_nom: 
    input: 
        script = config["code_stepcounts"] + "step_02_resample_data.R",
        data = (expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-walk_{type}-raw{sample_rate}Hz.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_rate=15, type = ["regular", "irregular", "semiregular"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-raw{sample_rate}Hz.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_rate= [100,25]))
    output:
        (expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-walk_{type}-resampled{sample_rate}to{new}Hz.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_rate=15, type = ["regular", "irregular", "semiregular"], new=30),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-resampled{sample_rate}to{new}Hz.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_rate= [100,25], new=30))
    shell: 
        "Rscript {input.script} {input.data} {output}" 