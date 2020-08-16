#' Scrape Batter Leaderboards from FanGraphs
#'
#' @description This function allows you to scrape all leaderboard statistics (basic and advanced) from FanGraphs.com.
#' @param x First season for which you want data.
#' @param y Last season for which you want data. If multiple years selected, data returned will be aggregate data for the date range. If y = x, function will return single-season data.
#' @param league Option for limiting results to different leagues or overall results. Options are "al", "nl", or "all".
#' @param qual Whether you want only batters/pitchers that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimumm number of plate appearaces/innings pitched, use the number desired. Defaults to "y".
#' @param ind Whether or not to break the seasons out individual, or roll them up together. 1 = split seasons, 0 = aggregate seasons.
#' @keywords MLB, sabermetrics
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table html_attr
#' @export
#' @examples
#' \dontrun{fg_bat_leaders(x = 2015, y = 2015, qual = 400)}

fg_bat_leaders <- function(x, y, league = "all", qual = "y", ind = 0) {

  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  if (ind == 0) {
    payload <- read_html(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=", league, "&qual=", qual,
                                "&type=c,-1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286&season=", y, "&month=0&season1=", x, "&ind=", ind, "&team=&rost=&age=&filter=&players=&page=1_100000"))

    leaders <- payload %>%
      html_nodes("table") %>%
      `[[`(length(.)-1) %>% html_table(fill=TRUE)

    leaders <- leaders[-c(1,3),]
    names(leaders) <- leaders[1,]
    leaders <- leaders[-1,]
    c <- as.matrix(names(leaders))
    c <- gsub("%", "_pct", c, fixed = TRUE)
    c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
    c <- gsub(" (pi)", "_pi", c, fixed = TRUE)
    c <- gsub("/", "_", c, fixed = TRUE)
    c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
    r <- c("wRC_plus", "WPA_minus", "WPA_plus", "FBall_pct", "AgeRng")
    c[c(62,64,65,73,202),] <- r
    Seasons <- ifelse(x==y, paste0(x), paste0(x, "-", y))
    names(leaders) <- c
    leaders <- cbind(Seasons, leaders)
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("%", "", x))), stringsAsFactors=F)
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("$", "", x, fixed = TRUE))), stringsAsFactors=F)
    leaders$Dol <- gsub("\\(", "-", leaders$Dol)
    leaders$Dol <- gsub("\\)", "", leaders$Dol)
    # Replace any empty cells with NA to avoid a warning message.
    is.na(leaders) <- leaders==""
    # Convert columns 5 to 213 to numeric, exept column 204 "Age Rng"
    for(i in c(5:203, 205:ncol(leaders))) {
      leaders[,i] <- as.numeric(leaders[,i])
    }

    playerids <- payload %>%
      html_nodes("table") %>%
      .[[length(.)-1]] %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      as.data.frame() %>%
      rename(slug = '.') %>%
      filter(grepl("playerid", slug)) %>%
      mutate(playerid = sub(".*[=] *(.*?) *[&].*", "\\1", slug))

    leaders <- leaders %>%
      mutate(playerid = playerids$playerid) %>%
      select(playerid, everything())

    return(leaders)
  }

  else {
    payload <- read_html(paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=", league, "&qual=", qual, "&type=c,-1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286&season=", y, "&month=0&season1=", x, "&ind=", ind, "&team=&rost=&age=&filter=&players=&page=1_100000"))

    leaders <- payload %>%
      html_nodes("table") %>%
      `[[`(length(.)-1) %>% html_table(fill=TRUE)

    leaders <- leaders[-c(1,3),]
    names(leaders) <- leaders[1,]
    leaders <- leaders[-1,]
    c <- as.matrix(names(leaders))
    c <- gsub("%", "_pct", c, fixed = TRUE)
    c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
    c <- gsub(" (pi)", "_pi", c, fixed = TRUE)
    c <- gsub("/", "_", c, fixed = TRUE)
    c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
    r <- c("wRC_plus", "WPA_minus", "WPA_plus", "FBall_pct", "AgeRng")
    c[c(63, 65, 66, 74, 204),] <- r
    names(leaders) <- c
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("%", "", x))))
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("$", "", x, fixed = TRUE))))
    leaders$Dol <- gsub("\\(", "-", leaders$Dol)
    leaders$Dol <- gsub("\\)", "", leaders$Dol)
    for(i in c(5:ncol(leaders))) {
      leaders[,i] <- as.numeric(as.character(leaders[,i]))
    }

    playerids <- payload %>%
      html_nodes("table") %>%
      .[[length(.)-1]] %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      as.data.frame() %>%
      rename(slug = '.') %>%
      filter(grepl("playerid", slug)) %>%
      mutate(playerid = sub(".*[=] *(.*?) *[&].*", "\\1", slug))

    leaders <- leaders %>%
      mutate(playerid = playerids$playerid) %>%
      select(playerid, everything())

    return(leaders)
  }
}

#' Scrape Pitcher Leaderboards from FanGraphs
#'
#' @rdname fg_pitch_leaders
#' @param pitcher_type Whether you want only starting pitchers, relievers, or all pitchers that meet the criteria specified in the qual argument. Options include "pit", "sta", "rel".
#' @param x First season for which you want data.
#' @param y Last season for which you want data. If multiple years selected, data returned will be aggregate data for the date range. If y = x, function will return single-season data.
#' @param league Option for limiting results to different leagues or overall results. Options are "al", "nl", or "all".
#' @param qual Whether you want only batters/pitchers that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimumm number of plate appearaces/innings pitched, use the number desired. Defaults to "y".
#' @param ind Whether or not to break the seasons out individual, or roll them up together. 1 = split seasons, 0 = aggregate seasons.
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table html_attr
#' @export
#' @examples
#' \dontrun{fg_pitch_leaders(x = 2015, y = 2015, qual = 150)}

fg_pitch_leaders <- function(x, y, league = "all", qual = "y",
                             pitcher_type = "pit", ind = 0) {

  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  url <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=", pitcher_type, "&lg=", league, "&qual=", qual, "&type=c,-1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299&season=", y, "&month=0&season1=", x, "&ind=", ind,"&team=&rost=&age=&filter=&players=&page=1_100000")

  if (ind == 0) {
    payload <- read_html(url)

    leaders <- payload %>%
      html_nodes("table") %>%
      `[[`(length(.)-1) %>% html_table(fill=TRUE)

    leaders <- leaders[-c(1,3),]
    names(leaders) <- leaders[1,]
    leaders <- leaders[-1,]
    c <- as.matrix(names(leaders))
    c <- gsub("%", "_pct", c, fixed = TRUE)
    c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
    c <- gsub(" (pi)", "_pi", c, fixed = TRUE)
    c <- gsub("/", "_", c, fixed = TRUE)
    c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
    r <- c("Start_IP", "Relief_IP", "WPA_minus",
           "WPA_plus", "FBall_pct", "AgeRng")
    c[c(56,58,65,66,76,217),] <- r
    Seasons <- ifelse(x==y, paste0(x), paste0(x, "-", y))
    names(leaders) <- c
    leaders <- cbind(Seasons, leaders)
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("%", "", x))), stringsAsFactors=F)
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("$", "", x, fixed = TRUE))), stringsAsFactors=F)
    leaders$Dol <- gsub("\\(", "-", leaders$Dol)
    leaders$Dol <- gsub("\\)", "", leaders$Dol)
    # Replace any empty cells with NA to avoid a warning message.
    is.na(leaders) <- leaders==""
    # Convert columns 5 to 213 to numeric, exept column 204 "Age Rng"
    for(i in c(5:203, 205:ncol(leaders))) {
      leaders[,i] <- as.numeric(leaders[,i])
    }

    playerids <- payload %>%
      html_nodes("table") %>%
      .[[length(.)-1]] %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      as.data.frame() %>%
      rename(slug = '.') %>%
      filter(grepl("playerid", slug)) %>%
      mutate(playerid = sub(".*[=] *(.*?) *[&].*", "\\1", slug))

    leaders <- leaders %>%
      mutate(playerid = playerids$playerid) %>%
      select(playerid, everything())

    return(leaders)
  }

  else {
    payload <- read_html(url)

    leaders <- payload %>%
      html_nodes("table") %>%
      `[[`(length(.)-1) %>% html_table(fill=TRUE)

    leaders <- leaders[-c(1,3),]
    names(leaders) <- leaders[1,]
    leaders <- leaders[-1,]
    c <- as.matrix(names(leaders))
    c <- gsub("%", "_pct", c, fixed = TRUE)
    c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
    c <- gsub(" (pi)", "_pi", c, fixed = TRUE)
    c <- gsub("/", "_", c, fixed = TRUE)
    c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
    r <- c("Start_IP", "Relief_IP", "WPA_minus",
           "WPA_plus", "FBall_pct", "AgeRng")
    c[c(57,59,66,67,77,218),] <- r
    names(leaders) <- c
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("%", "", x))))
    leaders <- as.data.frame(sapply(leaders, function(x) (gsub("$", "", x, fixed = TRUE))))
    leaders$Dol <- gsub("\\(", "-", leaders$Dol)
    leaders$Dol <- gsub("\\)", "", leaders$Dol)
    for(i in c(5:ncol(leaders))) {
      leaders[,i] <- as.numeric(as.character(leaders[,i]))
    }

    playerids <- payload %>%
      html_nodes("table") %>%
      .[[length(.)-1]] %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      as.data.frame() %>%
      rename(slug = '.') %>%
      filter(grepl("playerid", slug)) %>%
      mutate(playerid = sub(".*[=] *(.*?) *[&].*", "\\1", slug))

    leaders <- leaders %>%
      mutate(playerid = playerids$playerid) %>%
      select(playerid, everything())

    return(leaders)
  }
}

#' Scrape FanGraphs.com Guts!
#'
#' @rdname fg_guts
#' @description This function allows you to scrape the historical, season-by-season wOBA and FIP constants and coefficients at FanGraphs.com.
#' @keywords MLB, sabermetrics
#' @importFrom rvest html_node
#' @export
#' @examples
#' fg_guts()

# scrape historical FanGraphs Guts! table
# wOBA and FIP coefficients and constants

fg_guts <- function() {

  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>%
    html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
    html_table %>%
    setNames(c("season", "lg_woba", "woba_scale", "wBB", "wHBP", "w1B", "w2B",
               "w3B", "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
}

#' Scrape Park Factors from FanGraphs.com
#'
#' @rdname fg_park
#' @description This function allows you to scrape park factors for a given season from FanGraphs.com.
#' @param yr Season for which you want to scrape the park factors.
#' @keywords MLB, sabermetrics
#' @export
#' @examples
#' fg_park(2013)

fg_park <- function(yr) {

  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  read_html(paste0("http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=", yr)) %>%
    html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
    html_table %>%
    setNames(c("season", "home_team", "basic_5yr", "3yr", "1yr", "single", "double", "triple", "hr",
               "so", "UIBB", "GB", "FB", "LD", "IFFB", "FIP"))

}

#' Scrape Park Factors from FanGraphs.com
#'
#' @rdname fg_park
#' @description This function allows you to scrape park factors by handedness from FanGraphs.com for a given single year.
#' @param yr Season for which you want to scrape the park factors.
#' @importFrom stats setNames
#' @export
#' @examples
#' fg_park_hand(2013)

fg_park_hand <- function(yr) {

  message('Data courtey of FanGraphs.com. Please consider supporting FanGraphs by purchasing a membership: https://plus.fangraphs.com/product/fangraphs-membership/?switch-subscription=254671&item=85029&_wcsnonce=3e893e9b53&auto-switch=true')

  read_html(paste0("http://www.fangraphs.com/guts.aspx?type=pfh&teamid=0&season=", yr)) %>%
    html_node(xpath = '//*[(@id = "GutsBoard1_dg1_ctl00")]') %>%
    html_table %>%
    stats::setNames(c("season", "home_team", "single_as_LHH", "single_as_RHH",
                      "double_as_LHH", "double_as_RHH", "triple_as_LHH", "triple_as_RHH",
                      "hr_as_LHH", "hr_as_RHH"))
}
