library(pkgapi)
library(tidyverse)
pacman::p_load_current_gh("r-lib/pkgapi")
pkg <- pkgapi::map_package()
function_calls <- pkg$calls
exported <- pkg$defs %>% filter(exported == TRUE)

## --- Exported functions ---
exported %>% 
  knitr::kable()
## --- baseballr calls ---
self_calls <- function_calls %>% 
  dplyr::filter(stringr::str_detect(.data$to,"baseballr::")) 
self_calls %>% 
  knitr::kable()

pkg_usage_summary <- function_calls %>% 
  dplyr::mutate(
    package_called = stringr::str_extract(string = .data$to, ".+(?=::)")) %>% 
  dplyr::group_by(.data$package_called) %>% 
  dplyr::summarize(n = dplyr::n()) %>% 
  dplyr::arrange(desc(.data$n))

write.csv(exported,"data-raw/baseballr_exported_functions.csv",row.names=F)