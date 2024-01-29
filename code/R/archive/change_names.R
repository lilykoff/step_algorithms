files = list.files(here::here("results/actilife/original"), full.names=TRUE)
ox_files = files[grep("wrist", files)]

for(i in 1:length(ox_files)){
  x = ox_files[i]
  id = sub(".*acc_(.+)\\_wrist.*", "\\1", x)
  sr = sub(".*wrist(.+)1sec.*", "\\1", x)
  fpath_start = sub("original/.*", "", ox_files[i])
  newname = paste0(fpath_start, "oxwalk/oxwalk-", id, "-raw", sr, "Hz1sec.csv.gz")
  file.rename(ox_files[i], newname)
}

files = list.files(here::here("results/actilife/original"), full.names=TRUE)

for(i in 1:length(files)){
  x = files[i]
  id = sub(".*P(.+)\\_.*", "\\1", x)
  id = sprintf("P%02.f", as.numeric(id))
  sr = 15
  act = sub(".*\\_(.+)1sec.*", "\\1", x)
  act = paste0("walk_", tolower(act))
  fpath_start = sub("original/.*", "", files[i])
  newname = paste0(fpath_start, "clemson/clemson-", id, "-",
                   act, "-raw", sr, "Hz1sec.csv.gz")
  file.rename(files[i], newname)
}

files = list.files(here::here("results/actilife/resampled"), full.names=TRUE)
ox_files = files[-grep("15to", files)]

for(i in 1:length(ox_files)){
  x = ox_files[i]
  id = sub(".*acc_(.+)\\_.*", "\\1", x)
  sr = sub(".*\\_(.+)to.*", "\\1", x)
  fpath_start = sub("resampled/.*", "", ox_files[i])
  newname = paste0(fpath_start, "oxwalk/oxwalk-", id, "-resampled", sr, "to30Hz1sec.csv.gz")
  file.rename(ox_files[i], newname)
}

files = list.files(here::here("results/actilife/resampled"), full.names=TRUE)

for(i in 1:length(files)){
  x = files[i]
  id = str_split(sub(".*acc\\_(.+)\\_.*", "\\1", x), "_")[[1]][1]
  id = sprintf("P%02.f", as.numeric(id))
  sr = 15
  act = sub(".*\\_(.+)\\_15.*", "\\1", x)
  act = paste0("walk_", tolower(act))
  fpath_start = sub("resampled/.*", "", files[i])
  newname = paste0(fpath_start, "clemson/clemson-", id, "-",
                   act, "-resampled", sr, "to30Hz1sec.csv.gz")
  file.rename(files[i], newname)
}

files= list.files(here::here("results/actilife/csv"),
                  full.names = TRUE)

for(i in 1:length(files)){
  x = files[i]
  id = str_split(sub(".*acc\\_(.+)\\_.*", "\\1", x), "_")[[1]][1]
  id = sprintf("P%02.f", as.numeric(id))
  sr = 128
  act = str_split(sub(".*acc\\_(.+)\\_128.*", "\\1", x), "_")[[1]]
  act_final = paste0(act[2], "_", act[3])
  fpath_start = sub("csv\\/.*", "", files[i])
  newname = paste0(fpath_start, "marea/marea-", id, "-",
                   act_final, "-resampled", sr, "to30Hz1sec.csv.gz")
  file.rename(files[i], newname)
}

