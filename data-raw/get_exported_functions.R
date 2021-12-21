library(pkgapi)
library(tidyverse)
pacman::p_load_current_gh("r-lib/pkgapi")
pkg <- pkgapi::map_package()

exported <- pkg$defs %>% filter(exported == TRUE)

write.csv(exported,"data-raw/baseballr_exported_functions.csv",row.names=F)