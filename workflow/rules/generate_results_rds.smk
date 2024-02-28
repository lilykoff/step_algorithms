configfile: "workflow/paths.yaml"
import os
if os.path.exists("data/raw/MAREA_dataset/"):
    ruleorder: generate_results_dfs > generate_results_dfs_nom
else: 
    ruleorder: generate_results_dfs_nom > generate_results_dfs


# https://carpentries-incubator.github.io/workflows-snakemake/09-cluster/index.html#:~:text=To%20run%20Snakemake%20on%20a,single%20rule%20and%20then%20exits.
runR = "Rscript --no-save --no-restore --verbose"
# can use "script: " instead? 
datanames = ['clemson', 'marea', 'oxwalk']
datanames2 = ['clemson', 'oxwalk']
rawnames = ['clemson', 'MAREA_dataset', 'oxwalk']
methods = ["oak", "vs", "sdt", "truth", "adept"]
methods2 = ["oak", "vs", "sdt", "adept"]


rule generate_results_dfs: 
    input: 
        script = config["code_analysis"] + "generate_results_dfs.R",
        data = expand("results/all_algorithms/{dataname}_step_estimates_1sec.csv.gz", dataname = datanames)
    output:
        ("results/all_algorithms/accuracy_stats_bysubject.rds",
        "results/all_algorithms/step_stats_bysubject.rds",
        "results/all_algorithms/total_steps_bysubject.rds")
    shell: 
        "Rscript {input.script} {output}" 

rule generate_results_dfs_nom: 
    input: 
        script = config["code_analysis"] + "generate_results_dfs.R",
        data = expand("results/all_algorithms/{dataname}_step_estimates_1sec.csv.gz", dataname = datanames2)
    output:
        ("results/all_algorithms/accuracy_stats_bysubject.rds",
        "results/all_algorithms/step_stats_bysubject.rds",
        "results/all_algorithms/total_steps_bysubject.rds")
    shell: 
        "Rscript {input.script} {output}" 