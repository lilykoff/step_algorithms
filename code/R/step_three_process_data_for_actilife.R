# this part was run on computing cluster
library(tidyverse)
# remotes::install_github("muschellij2/write.gt3x")
library(write.gt3x)
options(digits.secs = 3)
## convert for actilife - clemson
clemson_files = list.files(here::here("data", "reorganized", "clemson"), full.names = TRUE,
                           recursive = TRUE)

ox_files = list.files(here::here("data", "reorganized", "oxwalk"), full.names = TRUE,
                      recursive = TRUE)
marea_files =  list.files(here::here("data", "reorganized", "marea"), full.names = TRUE,
                          recursive = TRUE)

map(.x = clemson_files,
    .f = function(filename){
      x = readr::read_csv(filename)
      samp_rate = x$sample_rate[1]
      fname = sub(".*\\/","",filename)
      temp = x %>%
        select(time = tm_dttm, X, Y, Z)
      write.gt3x::write_actigraph_csv(
        df = temp,
        file = here::here("data", "actilife", "clemson", fname),
        sample_rate = samp_rate,
        max_g = "8",
        progress = FALSE
      )
})

map(.x = marea_files,
    .f = function(filename){
      x = readr::read_csv(filename)
      samp_rate = x$sample_rate[1]
      fname = sub(".*\\/","",filename)
      temp = x %>%
        select(time = tm_dttm, X, Y, Z)
      write.gt3x::write_actigraph_csv(
        df = temp,
        file = here::here("data", "actilife", "marea", fname),
        sample_rate = samp_rate,
        max_g = "8",
        progress = FALSE
      )
    })

map(.x = ox_files,
    .f = function(filename){
      x = readr::read_csv(filename)
      samp_rate = x$sample_rate[1]
      fname = sub(".*\\/","",filename)
      temp = x %>%
        select(time = tm_dttm, X, Y, Z)
      write.gt3x::write_actigraph_csv(
        df = temp,
        file = here::here("data", "actilife", "oxwalk", fname),
        sample_rate = samp_rate,
        max_g = "8",
        progress = FALSE
      )
    })




