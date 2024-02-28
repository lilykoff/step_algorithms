configfile: "workflow/paths.yaml"

import os
if os.path.exists("data/raw/MAREA_dataset/"):
    ruleorder: generate_step_figures > generate_step_figures_nom
    ruleorder: generate_resamp_figures > generate_resamp_figures_nom
else: 
    ruleorder: generate_step_figures_nom > generate_step_figures
    ruleorder: generate_resamp_figures_nom > generate_resamp_figures


datanames = ['clemson', 'marea', 'oxwalk']

# deal with MAREA here 
rule generate_acc_figures: 
    input: 
        script = config["code_analysis"] + "evaluate_classification.R",
        data = expand(config["results"] + "accuracy_stats_bysubject.rds")
    output:
       expand(config["figs"] + "{plotname}.svg", 
       plotname = ["boxplot_all", "boxplot_overall"])
    shell: 
        "Rscript {input.script} {output}" 


rule generate_step_figures: 
    input: 
        script = config["code_analysis"] + "evaluate_steps.R",
        data = (expand(config["results"] + "{dataname}_step_estimates_1sec.csv.gz", dataname = datanames),
        expand(config["results"] + "{rds}.rds", rds = ["step_stats_bysubject", "total_steps_bysubject"]))
    output:
       expand(config["figs"] + "{plotname}.svg", 
       plotname = "truth_v_predicted")
    shell: 
        "Rscript {input.script} {output}" 

rule generate_step_figures_nom: 
    input: 
        script = config["code_analysis"] + "evaluate_steps.R",
        data = (expand(config["results"] + "{dataname}_step_estimates_1sec.csv.gz", dataname = ["clemson", "oxwalk"]),
        expand(config["results"] + "{rds}.rds", rds = ["step_stats_bysubject", "total_steps_bysubject"]))
    output:
       expand(config["figs"] + "{plotname}.svg", plotname = "truth_v_predicted")
    shell: 
        "Rscript {input.script} {output}" 

rule generate_resamp_figures: 
    input: 
        script = config["code_analysis"] + "evaluate_resampling_sensitivity.R",
        data  = expand(config["results"] + "{dataname}_step_estimates_1sec.csv.gz", dataname = datanames)
    output:
       expand(config["figs"] + "bland_altman{plotname}.svg", plotname = ["sc", "vs"])
    shell: 
        "Rscript {input.script} {output}" 

rule generate_resamp_figures_nom: 
    input: 
        script = config["code_analysis"] + "evaluate_resampling_sensitivity.R",
        data  = expand(config["results"] + "{dataname}_step_estimates_1sec.csv.gz", dataname = ["clemson", "oxwalk"])
    output:
       expand(config["figs"] + "bland_altman{plotname}.svg", 
       plotname = ["sc", "vs"])
    shell: 
        "Rscript {input.script} {output}" 

rule generate_speed_figures:
    input: 
        script = config["code_analysis"] + "evaluate_speed_sensitivity.R",
        data  = expand(config["results"] + "{dataname}_step_estimates_1sec.csv.gz", dataname = "marea")
    output:
       expand(config["figs"] + "speed.svg")
    shell: 
        "Rscript {input.script} {output}" 
