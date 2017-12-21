#' Scrape Batter Leaderboards from FanGraphs
#'
#' This function allows you to scrape all leaderboard statistics (basic and advanced) from FanGraphs.com.
#' @param x First season for which you want data.
#' @param y Last season for which you want data. If multiple years selected, data returned will be aggregate data for the date range. If y = x, function will return single-season data.
#' @param league Option for limiting results to different leagues or overall results. Options are "al", "nl", or "all".
#' @param qual Whether you want only batters that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimumm number of plate appearaces, use the number desired. Defaults to "y".
#' @param ind Whether or not to break the seasons out individual, or roll them up together. 1 = split seasons, 0 = aggregate seasons.
#' @keywords MLB, sabermetrics
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @export
#' @examples
#' \dontrun{fg_bat_leaders(2015, 2015, 400)}

fg_bat_leaders <- function(x, y, league = "all", qual = "y", ind = 0) {

  if (ind == 0) {
    leaders <- read_html(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=", league, "&qual=", qual,
                                "&type=c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211&season=", y, "&month=0&season1=", x, "&ind=", ind, "&team=&rost=&age=&filter=&players=&page=1_100000")) %>% html_nodes("table") %>% `[[`(12) %>% html_table(fill=TRUE)
    leaders <- leaders[-c(1,3),]
    names(leaders) <- leaders[1,]
    leaders <- leaders[-1,]
    c <- as.matrix(names(leaders))
    c <- gsub("%", "_pct", c, fixed = TRUE)
    c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
    c <- gsub("/", "_", c, fixed = TRUE)
    c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
    r <- c("wRC_plus", "WPA_minus", "WPA_plus", "FBall_pct", "AgeRng")
    c[c(62,64,65,73,202),] <- r
    Seasons <- ifelse(x==y, paste0(x), paste0(x, "-", y))
    names(leaders) <- c
    leaders <- cbind(Seasons, leaders)
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("\\ %", "", x))), stringsAsFactors=F)
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("$", "", x, fixed = TRUE))), stringsAsFactors=F)
    leaders$Dol <- gsub("\\(", "-", leaders$Dol)
    leaders$Dol <- gsub("\\)", "", leaders$Dol)
    # Replace any empty cells with NA to avoid a warning message.
    is.na(leaders) <- leaders==""
    # Convert columns 5 to 213 to numeric, exept column 204 "Age Rng"
    for(i in c(5:203, 205:ncol(leaders))) {
        leaders[,i] <- as.numeric(leaders[,i])
    }
    return(leaders)
  }

  else {
    leaders <- read_html(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=", league, "&qual=", qual, "&type=c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211&season=", y, "&month=0&season1=", x, "&ind=", ind, "&team=&rost=&age=&filter=&players=&page=1_100000")) %>% html_nodes("table") %>% `[[`(12) %>% html_table(fill=TRUE)
    leaders <- leaders[-c(1,3),]
    names(leaders) <- leaders[1,]
    leaders <- leaders[-1,]
    c <- as.matrix(names(leaders))
    c <- gsub("%", "_pct", c, fixed = TRUE)
    c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
    c <- gsub("/", "_", c, fixed = TRUE)
    c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
    r <- c("wRC_plus", "WPA_minus", "WPA_plus", "FBall_pct", "AgeRng")
    c[c(63, 65, 66, 74, 204),] <- r
    names(leaders) <- c
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("\\ %", "", x))))
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("$", "", x, fixed = TRUE))))
    leaders$Dol <- gsub("\\(", "-", leaders$Dol)
    leaders$Dol <- gsub("\\)", "", leaders$Dol)
    for(i in c(5:ncol(leaders))) {
      leaders[,i] <- as.numeric(as.character(leaders[,i]))
    }
    return(leaders)
  }
}
