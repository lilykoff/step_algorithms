library(tidyverse)
options(digits.secs = 3)

# for stepcount, data need to be invery specific format
# cols time, x, y, z
clemson_files = list.files(here::here("data", "reorganized", "clemson"), full.names = TRUE,
                                      recursive = TRUE)

ox_files = list.files(here::here("data", "reorganized", "oxwalk"), full.names = TRUE,
                             recursive = TRUE)
marea_files =  list.files(here::here("data", "reorganized", "marea"), full.names = TRUE,
                                    recursive = TRUE)


map(.x = clemson_files,
    .f = function(filename){
      x = readr::read_csv(filename)
      fname = sub(".*\\/","",filename)
      temp = x %>%
        mutate(time = as.character(tm_dttm)) %>%
        select(time, x = X, y = Y, z = Z)
      write_csv(temp, here::here("data", "stepcount", "clemson", fname))
})


map(.x = ox_files,
    .f = function(filename){
      x = readr::read_csv(filename)
      fname = sub(".*\\/","",filename)
      temp = x %>%
        mutate(time = as.character(tm_dttm)) %>%
        select(time, x = X, y = Y, z = Z)
      write_csv(temp, here::here("data", "stepcount", "oxwalk", fname))
    })


map(.x = marea_files,
    .f = function(filename){
      x = readr::read_csv(filename)
      fname = sub(".*\\/","",filename)
      temp = x %>%
        mutate(time = as.character(tm_dttm)) %>%
        select(time, x = X, y = Y, z = Z)
      write_csv(temp, here::here("data", "stepcount", "marea", fname))
    })


