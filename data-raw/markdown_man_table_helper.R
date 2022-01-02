
df <-data.frame(col_name = colnames(x), types = sapply(x, class))
df %>% dplyr::select(col_name, types) %>% knitr::kable(row.names = FALSE)

x %>% knitr::kable()