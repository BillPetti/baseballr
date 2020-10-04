


library(baseballr)
library(dplyr)

obtain_data = function(start_date = "2017-03-29",
                       end_date = "2017-10-03",
                       type = "scrape",
                       infile = NULL) {
  if (type == "postgres") {
    # postgres db connection here
    # e.g.
    # library(RPostgres)
    # library(DBI)
    # conn <- dbConnect(RPostgres::Postgres(),
    #  password=SOMEPASS, user=SOME_USER, port=SOME_PORT_PROBABLY_5432, dbname=SOME_NAME)
    # df1 dbGetQuery(conn, "select * from SOME_TABLE_NAME where game_year=2017")
    # df1 %>% filter()
  } else if (type == "rds") {
    readRDS(infile)
  } else if (type == "csv") {
    read.csv(infile, stringsAsFactors = FALSE)
  } else if (type == "scrape") {
    date_seq = seq(as.Date(start_date), as.Date(end_date), by = 1)
    
    dplyr::bind_rows(lapply(date_seq, function(d) {
      scraped_data = baseballr::scrape_statcast_savant(d, d)
      if (nrow(scraped_data) > 0) {
        scraped_data
      } else {
        NULL
      }
    }))
    
    
  }
  
}

statcast_impute_derive = function(statcast_df, inverse_precision = 10000) {
  # statcast_df must have columns launch_angle, launch_speed, bb_type, events
  
  aa = statcast_df %>%
    filter(!is.na(launch_speed)) %>%
    mutate(
      ila = round(launch_angle * inverse_precision),
      ils = round(launch_speed * inverse_precision)
    )
  la_ls_count = aa %>%
    group_by(ila, ils, bb_type, events) %>%
    summarise(n = n())
  
  # use 5 here? some other number? 99.X percentile? this is why it's a heuristic
  la_ls_filtered = la_ls_count %>% filter(n >= 5)
  
  la_ls_filtered %>%
    write.csv("CSV_FILE_TO_LOAD_LATER.csv", row.names = FALSE)
  
  la_ls_filtered
}