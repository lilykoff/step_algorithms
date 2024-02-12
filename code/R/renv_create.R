# options(renv.config.dependencies.limit = 10000)
# # install.packages("renv")
lockfile = renv::lockfile_create()

renv::lockfile_write(lockfile, file = here::here("renv.lock"))
# renv::init()
