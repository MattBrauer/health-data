library(tidyverse)
library(DBI)
library(RSQLite)

workout_data_dir <- "data"
workout_data_file <- fs::dir_ls(workout_data_dir, glob = "*.wcs")

con <- dbConnect(SQLite(), workout_data_file)


workouts <- as_tibble(dbGetQuery(con,
                                 "select w.*, datetime(w.ZDATE,'unixepoch','31 years') as nzdate, \
                                 n.ZNAME as wname from ZTWORKOUT w \
                                 join ZTWORKOUTNAME n on n.Z_PK=w.ZNAME \
                                 where w.ZDONE=1 order by zdate;")) %>%
  mutate(nzdate = as.Date(nzdate) + 1) %>%
  rename(date = nzdate, name = wname) %>%
  select(date, name)         

dbDisconnect(con)
