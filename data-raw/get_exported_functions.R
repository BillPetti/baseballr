library(pkgapi)
library(tidyverse)
pacman::p_load_current_gh("r-lib/pkgapi")
pkg <- pkgapi::map_package()
function_calls <- pkg$calls
exported <- pkg$defs %>% filter(exported == TRUE) %>% 
  dplyr::mutate(new_name = "") %>% 
  dplyr::select(all_of(c("name","new_name", "file", "exported"))) 

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
  dplyr::arrange(dplyr::desc(.data$n))

write.csv(exported,"data-raw/baseballr_exported_functions.csv",row.names=F)

read.csv("data-raw/baseballr_exported_functions_wip.csv") %>% 
  dplyr::filter(.data$new_name !="") %>% 
  dplyr::select(-.data$exported) %>% 
  knitr::kable()