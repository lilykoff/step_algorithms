configfile: "workflow/paths.yaml"
import os
if os.path.exists("data/raw/MAREA_dataset/"):
   ruleorder: download_data > download_data_nom
else: 
   ruleorder: download_data_nom > download_data

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

rule download_data: 
    input: 
        script = config["code_stepcounts"] + "step_01_download_process_data.R"
    output:
        directory(expand(config["out_raw"] + "{rawname}/", rawname = ["clemson", "oxwalk"])),
        expand(config["out_proc"] + "{name}.csv.gz", name = ["clemson", "marea", "ox_data"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-walk_{type}-raw{sample_rate}Hz.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_rate=15, type = ["regular", "irregular", "semiregular"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{type}-raw{sample_rate}Hz.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(1, 12) if i != 4], sample_rate=128, type = ["indoor_walkrun", "treadmill_slopewalk", "treadmill_walkrun"]),
         expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-{type}-raw{sample_rate}Hz.csv.gz", 
        dataname = "marea", id = ["P{:02d}".format(i) for i in range(12, 21)], sample_rate=128, type = ["outdoor_walkrun"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-raw{sample_rate}Hz.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_rate= [100,25])
    shell: 
        "Rscript {input.script} {output}" 

rule download_data_nom: 
    input: 
        script = config["code_stepcounts"] + "step_01_download_process_data.R"
    output:
        directory(expand(config["out_raw"] + "{rawname}/", rawname = ["clemson", "oxwalk"])),
        expand(config["out_proc"] + "{name}.csv.gz", name = ["clemson", "ox_data"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-walk_{type}-raw{sample_rate}Hz.csv.gz", 
        dataname = "clemson", id = ["P{:02d}".format(i) for i in range(1, 31)], sample_rate=15, type = ["regular", "irregular", "semiregular"]),
        expand(config["out_reorganized"] + "{dataname}/{id}/{dataname}-{id}-raw{sample_rate}Hz.csv.gz", 
        dataname = "oxwalk", id = ["P{:02d}".format(i) for i in range(1, 40)], sample_rate= [100,25])
    shell: 
        "Rscript {input.script} {output}" 

