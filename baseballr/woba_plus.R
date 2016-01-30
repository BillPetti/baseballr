# calculate wOBA, wRC, and wRC+, all with one function call

# wOBA = (wBB×uBB + wHBP×HBP + w1B*x1B + w2B×x2B + w3B×x3B + wHR×HR) / PA
# wRC = (((wOBA-League wOBA)/wOBA Scale)+(League R/PA))*PA
# wRC+ = (((wRAA/PA + League R/PA) + (League R/PA – Park Factor* League R/PA))/ (AL or NL wRC/PA excluding pitchers))*100

woba_plus <- function(df) {
  df$season <- as.character(df$season)
  guts_table <- read_html("http://www.fangraphs.com/guts.aspx?type=cn")
  guts_table <- guts_table %>% html_nodes(xpath = '//*[@id="content"]/table') %>% html_table(fill = TRUE)
  guts_table<- as.data.frame(guts_table) %>% .[-(1:2), (1:14)]
  names(guts_table) <- c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP")
  for(i in c(2:ncol(guts_table))) {
    guts_table[,i] <- as.numeric(as.character(guts_table[,i]))
  }
  df_join <- left_join(df, guts_table, by = "season")
  df_join$wOBA <- round((((df_join$wBB * df_join$uBB) + (df_join$wHBP * df_join$HBP) + (df_join$w1B * df_join$x1B) + (df_join$w2B * df_join$x2B) + (df_join$w3B * df_join$x3B) + (df_join$wHR * df_join$HR))/df_join$PA),3)
  df_join
}
