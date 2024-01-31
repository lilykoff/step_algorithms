library(tidyverse)
# remotes::install_github("muschellij2/write.gt3x")
library(write.gt3x)
options(digits.secs = 3)

# for actilife, data need to be in specific format, so we create specific actilife-compatible csvs
# for stepcount, we also need csvs where column names are time, x, y, z; time in character format

clemson_files =
  grep(
    list.files(
      here::here("data", "reorganized", "clemson"),
      full.names = TRUE,
      recursive = TRUE
    ),
    pattern = "steps",
    invert = TRUE,
    value = TRUE
  )

marea_files =
  grep(
    list.files(
      here::here("data", "reorganized", "marea"),
      full.names = TRUE,
      recursive = TRUE
    ),
    pattern = "steps",
    invert = TRUE,
    value = TRUE
  )

ox_files =
  grep(
    list.files(
      here::here("data", "reorganized", "oxwalk"),
      full.names = TRUE,
      recursive = TRUE
    ),
    pattern = "steps",
    invert = TRUE,
    value = TRUE
  )

if(!file.exists(here::here("data", "actilife", "clemson"))){
  dir.create(here::here("data", "actilife", "clemson"))
}

if(!file.exists(here::here("data", "stepcount", "clemson"))){
  dir.create(here::here("data", "stepcount", "clemson"))
}
map(
  .x = clemson_files,
  .f = function(filename) {
    x = readr::read_csv(filename)
    samp_rate = x$sample_rate[1]
    fname = sub(".*\\/", "", filename)
    temp_acti = x %>%
      select(time = tm_dttm, X, Y, Z)
    write.gt3x::write_actigraph_csv(
      df = temp_acti,
      file = here::here("data", "actilife", "clemson", fname),
      sample_rate = samp_rate,
      max_g = "8",
      progress = FALSE
    )
    temp_sc = x %>%
      mutate(time = as.character(tm_dttm)) %>%
      select(time, x = X, y = Y, z = Z)
    write_csv(temp_sc, here::here("data", "stepcount", "clemson", fname))
  }
)

if(!file.exists(here::here("data", "actilife", "marea"))){
  dir.create(here::here("data", "actilife", "marea"))
}

if(!file.exists(here::here("data", "stepcount", "marea"))){
  dir.create(here::here("data", "stepcount", "marea"))
}

map(
  .x = marea_files,
  .f = function(filename) {
    x = readr::read_csv(filename)
    samp_rate = x$sample_rate[1]
    fname = sub(".*\\/", "", filename)
    temp_acti = x %>%
      select(time = tm_dttm, X, Y, Z)
    write.gt3x::write_actigraph_csv(
      df = temp_acti,
      file = here::here("data", "actilife", "marea", fname),
      sample_rate = samp_rate,
      max_g = "8",
      progress = FALSE
    )
    temp_sc = x %>%
      mutate(time = as.character(tm_dttm)) %>%
      select(time, x = X, y = Y, z = Z)
    write_csv(temp_sc, here::here("data", "stepcount", "marea", fname))
  }
)

if(!file.exists(here::here("data", "actilife", "oxwalk"))){
  dir.create(here::here("data", "actilife", "oxwalk"))
}

if(!file.exists(here::here("data", "stepcount", "oxwalk"))){
  dir.create(here::here("data", "stepcount", "oxwalk"))
}
map(
  .x = ox_files,
  .f = function(filename) {
    x = readr::read_csv(filename)
    samp_rate = x$sample_rate[1]
    fname = sub(".*\\/", "", filename)
    temp_acti = x %>%
      select(time = tm_dttm, X, Y, Z)
    write.gt3x::write_actigraph_csv(
      df = temp_acti,
      file = here::here("data", "actilife", "oxwalk", fname),
      sample_rate = samp_rate,
      max_g = "8",
      progress = FALSE
    )
    temp_sc = x %>%
      mutate(time = as.character(tm_dttm)) %>%
      select(time, x = X, y = Y, z = Z)
    write_csv(temp_sc, here::here("data", "stepcount", "oxwalk", fname))
  }
)


