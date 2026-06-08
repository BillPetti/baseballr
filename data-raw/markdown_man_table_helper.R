cat(colnames(x), sep = '",\n"')
df <- data.frame(col_name = colnames(x), types = sapply(x, class))
df %>% dplyr::select(col_name, types) %>% knitr::kable(row.names = FALSE)

x %>% knitr::kable()

testy <- function(func, envir = parent.frame()) {
  # Extract the parameters of the given function
  params <- formals(func)

  # Assign each parameter's evaluated default into the caller's environment.
  # Defaults to parent.frame() (the global environment when called at the top
  # level interactively) rather than hard-coding .GlobalEnv, so callers can
  # target a scratch environment and avoid clobbering global objects.
  for (param_name in names(params)) {
    assign(param_name, eval(params[[param_name]]), envir = envir)
  }

  cat("Parameters have been assigned to the calling environment.\n")
}