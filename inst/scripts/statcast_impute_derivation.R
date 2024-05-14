library(baseballr)
library(dplyr)

obtain_imputation_training_data = function(start_date = "2017-03-29",
                                           end_date = "2017-10-03",
                                           type = "scrape",
                                           infile = NULL) {
  if (type == "postgres") {
    # postgres db connection here,
    # e.g.
    # library(RPostgres)
    # library(DBI)
    # conn <- dbConnect(RPostgres::Postgres(),
    #  password=SOMEPASS, user=SOME_USER, port=SOME_PORT_PROBABLY_5432, dbname=SOME_NAME)
    # df1 <- dbGetQuery(conn, "select * from SOME_TABLE_NAME where game_year=2017")
  } else if (type == "rds") {
    readRDS(infile)
  } else if (type == "csv") {
    read.csv(infile, stringsAsFactors = FALSE)
  } else if (type == "scrape") {
    date_seq = seq(as.Date(start_date), as.Date(end_date), by = 1)
    dplyr::bind_rows(lapply(date_seq, function(d) {
      scraped_data = baseballr::scrape_statcast_savant(d, d) %>% 
        select("launch_angle", "launch_speed", "bb_type", "events")
      if (nrow(scraped_data) > 0) {
        scraped_data
      } else {
        NULL
      }
    })) %>%   
      filter(!is.na(launch_speed)) %>%
      filter(!is.na(launch_angle))
      
  }
}

statcast_impute_derive = function(imputation_training_data, inverse_precision = 10000) {
  # imputation_training_data must be a statcast data frame, 
  # having columns launch_angle, launch_speed, bb_type, events
  
  la_ls_count = imputation_training_data %>%
    filter(!is.na(launch_speed)) %>%
    filter(!is.na(launch_angle)) %>%
    mutate(
      ila = round(launch_angle * inverse_precision),
      ils = round(launch_speed * inverse_precision)
    ) %>%
    group_by(ila, ils, bb_type, events) %>%
    summarise(n = n())
  
  # use 5 here? some other number? 99.X percentile? this is why I referred to 
  # it as a heuristic in the `label_statcast_imputed_data` documentation
  la_ls_filtered = la_ls_count %>% filter(n >= 5)
  
  la_ls_filtered %>%
    write.csv("CSV_FILE_TO_LOAD_LATER.csv", row.names = FALSE)
  
  la_ls_filtered
}