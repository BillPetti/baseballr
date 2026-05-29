cat(colnames(x), sep = '",\n"')
df <- data.frame(col_name = colnames(x), types = sapply(x, class))
df %>% dplyr::select(col_name, types) %>% knitr::kable(row.names = FALSE)

x %>% knitr::kable()

testy <- function(func) {
  # Extract the parameters of the given function
  params <- formals(func)

  # Loop through each parameter and assign it to the global environment
  for (param_name in names(params)) {
    # Use 'assign' to put each parameter in the global environment
    assign(param_name, eval(params[[param_name]]), envir = .GlobalEnv)
  }

  cat("Parameters have been assigned to the global environment.\n")
}