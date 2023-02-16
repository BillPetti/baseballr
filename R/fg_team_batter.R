#' @rdname fg_team_batter
#' @title **Scrape Team Batter Leaderboards from FanGraphs**
#' @description This function allows you to scrape all leaderboard statistics (basic and advanced) from FanGraphs.com.
#' @param x First season for which you want data.
#' @param y Last season for which you want data. If multiple years selected, data returned will be aggregate data for the date range. If y = x, function will return single-season data.
#' @param league Option for limiting results to different leagues or overall results. Options are "al", "nl", or "all".
#' @param qual Whether you want only batters/pitchers that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimum number of plate appearaces/innings pitched, use the number desired. Defaults to "y".
#' @param ind Whether or not to break the seasons out individual, or roll them up together. 1 = split seasons, 0 = aggregate seasons.
#' @param exc_p (logical) Whether or not to exclude pitchers from the batter leaderboards. TRUE = exclude pitchers, FALSE = retain pitchers.
#' @return A data frame of batter data.
#'  |col_name           |types     |
#'  |:------------------|:---------|
#'  |Season             |character |
#'  |#                  |character |
#'  |Team               |character |
#'  |G                  |numeric   |
#'  |AB                 |numeric   |
#'  |PA                 |numeric   |
#'  |H                  |numeric   |
#'  |1B                 |numeric   |
#'  |2B                 |numeric   |
#'  |3B                 |numeric   |
#'  |HR                 |numeric   |
#'  |R                  |numeric   |
#'  |RBI                |numeric   |
#'  |BB                 |numeric   |
#'  |IBB                |numeric   |
#'  |SO                 |numeric   |
#'  |HBP                |numeric   |
#'  |SF                 |numeric   |
#'  |SH                 |numeric   |
#'  |GDP                |numeric   |
#'  |SB                 |numeric   |
#'  |CS                 |numeric   |
#'  |AVG                |numeric   |
#'  |GB                 |numeric   |
#'  |FB                 |numeric   |
#'  |LD                 |numeric   |
#'  |IFFB               |numeric   |
#'  |Pitches            |numeric   |
#'  |Balls              |numeric   |
#'  |Strikes            |numeric   |
#'  |IFH                |numeric   |
#'  |BU                 |numeric   |
#'  |BUH                |numeric   |
#'  |BB_pct             |numeric   |
#'  |K_pct              |numeric   |
#'  |BB_K               |numeric   |
#'  |OBP                |numeric   |
#'  |SLG                |numeric   |
#'  |OPS                |numeric   |
#'  |ISO                |numeric   |
#'  |BABIP              |numeric   |
#'  |GB_FB              |numeric   |
#'  |LD_pct             |numeric   |
#'  |GB_pct             |numeric   |
#'  |FB_pct             |numeric   |
#'  |IFFB_pct           |numeric   |
#'  |HR_FB              |numeric   |
#'  |IFH_pct            |numeric   |
#'  |BUH_pct            |numeric   |
#'  |wOBA               |numeric   |
#'  |wRAA               |numeric   |
#'  |wRC                |numeric   |
#'  |Bat                |numeric   |
#'  |Fld                |numeric   |
#'  |Rep                |numeric   |
#'  |Pos                |numeric   |
#'  |RAR                |numeric   |
#'  |WAR                |numeric   |
#'  |Dol                |numeric   |
#'  |Spd                |numeric   |
#'  |wRC_plus           |numeric   |
#'  |WPA                |numeric   |
#'  |WPA_minus          |numeric   |
#'  |WPA_plus           |numeric   |
#'  |RE24               |numeric   |
#'  |REW                |numeric   |
#'  |pLI                |numeric   |
#'  |phLI               |numeric   |
#'  |PH                 |numeric   |
#'  |WPA_LI             |numeric   |
#'  |Clutch             |numeric   |
#'  |FBall_pct          |numeric   |
#'  |FBv                |numeric   |
#'  |SL_pct             |numeric   |
#'  |SLv                |numeric   |
#'  |CT_pct             |numeric   |
#'  |CTv                |numeric   |
#'  |CB_pct             |numeric   |
#'  |CBv                |numeric   |
#'  |CH_pct             |numeric   |
#'  |CHv                |numeric   |
#'  |SF_pct             |numeric   |
#'  |SFv                |numeric   |
#'  |KN_pct             |numeric   |
#'  |KNv                |numeric   |
#'  |XX_pct             |numeric   |
#'  |PO_pct             |numeric   |
#'  |wFB                |numeric   |
#'  |wSL                |numeric   |
#'  |wCT                |numeric   |
#'  |wCB                |numeric   |
#'  |wCH                |numeric   |
#'  |wSF                |numeric   |
#'  |wKN                |numeric   |
#'  |wFB_C              |numeric   |
#'  |wSL_C              |numeric   |
#'  |wCT_C              |numeric   |
#'  |wCB_C              |numeric   |
#'  |wCH_C              |numeric   |
#'  |wSF_C              |numeric   |
#'  |wKN_C              |numeric   |
#'  |O-Swing_pct        |numeric   |
#'  |Z-Swing_pct        |numeric   |
#'  |Swing_pct          |numeric   |
#'  |O-Contact_pct      |numeric   |
#'  |Z-Contact_pct      |numeric   |
#'  |Contact_pct        |numeric   |
#'  |Zone_pct           |numeric   |
#'  |F-Strike_pct       |numeric   |
#'  |SwStr_pct          |numeric   |
#'  |BsR                |numeric   |
#'  |FA_pct (sc)        |numeric   |
#'  |FT_pct (sc)        |numeric   |
#'  |FC_pct (sc)        |numeric   |
#'  |FS_pct (sc)        |numeric   |
#'  |FO_pct (sc)        |numeric   |
#'  |SI_pct (sc)        |numeric   |
#'  |SL_pct (sc)        |numeric   |
#'  |CU_pct (sc)        |numeric   |
#'  |KC_pct (sc)        |numeric   |
#'  |EP_pct (sc)        |numeric   |
#'  |CH_pct (sc)        |numeric   |
#'  |SC_pct (sc)        |numeric   |
#'  |KN_pct (sc)        |numeric   |
#'  |UN_pct (sc)        |numeric   |
#'  |vFA (sc)           |numeric   |
#'  |vFT (sc)           |numeric   |
#'  |vFC (sc)           |numeric   |
#'  |vFS (sc)           |numeric   |
#'  |vFO (sc)           |numeric   |
#'  |vSI (sc)           |numeric   |
#'  |vSL (sc)           |numeric   |
#'  |vCU (sc)           |numeric   |
#'  |vKC (sc)           |numeric   |
#'  |vEP (sc)           |numeric   |
#'  |vCH (sc)           |numeric   |
#'  |vSC (sc)           |numeric   |
#'  |vKN (sc)           |numeric   |
#'  |FA-X (sc)          |numeric   |
#'  |FT-X (sc)          |numeric   |
#'  |FC-X (sc)          |numeric   |
#'  |FS-X (sc)          |numeric   |
#'  |FO-X (sc)          |numeric   |
#'  |SI-X (sc)          |numeric   |
#'  |SL-X (sc)          |numeric   |
#'  |CU-X (sc)          |numeric   |
#'  |KC-X (sc)          |numeric   |
#'  |EP-X (sc)          |numeric   |
#'  |CH-X (sc)          |numeric   |
#'  |SC-X (sc)          |numeric   |
#'  |KN-X (sc)          |numeric   |
#'  |FA-Z (sc)          |numeric   |
#'  |FT-Z (sc)          |numeric   |
#'  |FC-Z (sc)          |numeric   |
#'  |FS-Z (sc)          |numeric   |
#'  |FO-Z (sc)          |numeric   |
#'  |SI-Z (sc)          |numeric   |
#'  |SL-Z (sc)          |numeric   |
#'  |CU-Z (sc)          |numeric   |
#'  |KC-Z (sc)          |numeric   |
#'  |EP-Z (sc)          |numeric   |
#'  |CH-Z (sc)          |numeric   |
#'  |SC-Z (sc)          |numeric   |
#'  |KN-Z (sc)          |numeric   |
#'  |wFA (sc)           |numeric   |
#'  |wFT (sc)           |numeric   |
#'  |wFC (sc)           |numeric   |
#'  |wFS (sc)           |numeric   |
#'  |wFO (sc)           |numeric   |
#'  |wSI (sc)           |numeric   |
#'  |wSL (sc)           |numeric   |
#'  |wCU (sc)           |numeric   |
#'  |wKC (sc)           |numeric   |
#'  |wEP (sc)           |numeric   |
#'  |wCH (sc)           |numeric   |
#'  |wSC (sc)           |numeric   |
#'  |wKN (sc)           |numeric   |
#'  |wFA_C (sc)         |numeric   |
#'  |wFT_C (sc)         |numeric   |
#'  |wFC_C (sc)         |numeric   |
#'  |wFS_C (sc)         |numeric   |
#'  |wFO_C (sc)         |numeric   |
#'  |wSI_C (sc)         |numeric   |
#'  |wSL_C (sc)         |numeric   |
#'  |wCU_C (sc)         |numeric   |
#'  |wKC_C (sc)         |numeric   |
#'  |wEP_C (sc)         |numeric   |
#'  |wCH_C (sc)         |numeric   |
#'  |wSC_C (sc)         |numeric   |
#'  |wKN_C (sc)         |numeric   |
#'  |O-Swing_pct (sc)   |numeric   |
#'  |Z-Swing_pct (sc)   |numeric   |
#'  |Swing_pct (sc)     |numeric   |
#'  |O-Contact_pct (sc) |numeric   |
#'  |Z-Contact_pct (sc) |numeric   |
#'  |Contact_pct (sc)   |numeric   |
#'  |Zone_pct (sc)      |numeric   |
#'  |Pace               |numeric   |
#'  |Def                |numeric   |
#'  |wSB                |numeric   |
#'  |UBR                |numeric   |
#'  |AgeRng             |numeric   |
#'  |Off                |numeric   |
#'  |Lg                 |numeric   |
#'  |wGDP               |numeric   |
#'  |Pull_pct           |numeric   |
#'  |Cent_pct           |numeric   |
#'  |Oppo_pct           |numeric   |
#'  |Soft_pct           |numeric   |
#'  |Med_pct            |numeric   |
#'  |Hard_pct           |numeric   |
#'  |TTO_pct            |numeric   |
#'  |CH_pct_pi          |numeric   |
#'  |CS_pct_pi          |numeric   |
#'  |CU_pct_pi          |numeric   |
#'  |FA_pct_pi          |numeric   |
#'  |FC_pct_pi          |numeric   |
#'  |FS_pct_pi          |numeric   |
#'  |KN_pct_pi          |numeric   |
#'  |SB_pct_pi          |numeric   |
#'  |SI_pct_pi          |numeric   |
#'  |SL_pct_pi          |numeric   |
#'  |XX_pct_pi          |numeric   |
#'  |vCH_pi             |numeric   |
#'  |vCS_pi             |numeric   |
#'  |vCU_pi             |numeric   |
#'  |vFA_pi             |numeric   |
#'  |vFC_pi             |numeric   |
#'  |vFS_pi             |numeric   |
#'  |vKN_pi             |numeric   |
#'  |vSB_pi             |numeric   |
#'  |vSI_pi             |numeric   |
#'  |vSL_pi             |numeric   |
#'  |vXX_pi             |numeric   |
#'  |CH-X_pi            |numeric   |
#'  |CS-X_pi            |numeric   |
#'  |CU-X_pi            |numeric   |
#'  |FA-X_pi            |numeric   |
#'  |FC-X_pi            |numeric   |
#'  |FS-X_pi            |numeric   |
#'  |KN-X_pi            |numeric   |
#'  |SB-X_pi            |numeric   |
#'  |SI-X_pi            |numeric   |
#'  |SL-X_pi            |numeric   |
#'  |XX-X_pi            |numeric   |
#'  |CH-Z_pi            |numeric   |
#'  |CS-Z_pi            |numeric   |
#'  |CU-Z_pi            |numeric   |
#'  |FA-Z_pi            |numeric   |
#'  |FC-Z_pi            |numeric   |
#'  |FS-Z_pi            |numeric   |
#'  |KN-Z_pi            |numeric   |
#'  |SB-Z_pi            |numeric   |
#'  |SI-Z_pi            |numeric   |
#'  |SL-Z_pi            |numeric   |
#'  |XX-Z_pi            |numeric   |
#'  |wCH_pi             |numeric   |
#'  |wCS_pi             |numeric   |
#'  |wCU_pi             |numeric   |
#'  |wFA_pi             |numeric   |
#'  |wFC_pi             |numeric   |
#'  |wFS_pi             |numeric   |
#'  |wKN_pi             |numeric   |
#'  |wSB_pi             |numeric   |
#'  |wSI_pi             |numeric   |
#'  |wSL_pi             |numeric   |
#'  |wXX_pi             |numeric   |
#'  |wCH_C_pi           |numeric   |
#'  |wCS_C_pi           |numeric   |
#'  |wCU_C_pi           |numeric   |
#'  |wFA_C_pi           |numeric   |
#'  |wFC_C_pi           |numeric   |
#'  |wFS_C_pi           |numeric   |
#'  |wKN_C_pi           |numeric   |
#'  |wSB_C_pi           |numeric   |
#'  |wSI_C_pi           |numeric   |
#'  |wSL_C_pi           |numeric   |
#'  |wXX_C_pi           |numeric   |
#'  |O-Swing_pct_pi     |numeric   |
#'  |Z-Swing_pct_pi     |numeric   |
#'  |Swing_pct_pi       |numeric   |
#'  |O-Contact_pct_pi   |numeric   |
#'  |Z-Contact_pct_pi   |numeric   |
#'  |Contact_pct_pi     |numeric   |
#'  |Zone_pct_pi        |numeric   |
#'  |Pace_pi            |numeric   |
#' @import rvest 
#' @export
#' @examples \donttest{
#'   try(fg_team_batter(x = 2015, y = 2015, qual = 400))
#' }
fg_team_batter <- function(x, y, league = "all", qual = "y", ind = 1, exc_p = TRUE) {
  
  if (ind == 0) {
    tryCatch(
      expr = {
        if (exc_p){
          payload <- paste0("http://www.fangraphs.com/leaders.aspx?pos=np&stats=bat&lg=", league, "&qual=", qual,
                            "&type=c,-1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286&season=", y, "&month=0&season1=", x, "&ind=", ind, "&team=0,ts&rost=&age=&filter=&players=&page=1_100000") %>% 
            xml2::read_html()
        } else {
          payload <- paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=", league, "&qual=", qual,
                            "&type=c,-1,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286&season=", y, "&month=0&season1=", x, "&ind=", ind, "&team=0,ts&rost=&age=&filter=&players=&page=1_100000") %>% 
            xml2::read_html()
        }
        
        
        leaders <- (payload %>%
                      rvest::html_elements("table"))[[17]] %>%
          rvest::html_table()
        
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
        c[c(60,62,63,71,201),] <- r
        Seasons <- ifelse(x==y, paste0(x), paste0(x, "-", y))
        names(leaders) <- c
        leaders <- leaders %>%
          dplyr::mutate(Season = Seasons) %>%
          dplyr::select("Season", tidyr::everything())
        leaders <- as.data.frame(sapply(leaders, function(x) (gsub("%", "", x))), stringsAsFactors=F)
        leaders <- as.data.frame(sapply(leaders, function(x) (gsub("$", "", x, fixed = TRUE))), stringsAsFactors=F)
        leaders$Dol <- gsub("\\(", "-", leaders$Dol)
        leaders$Dol <- gsub("\\)", "", leaders$Dol)
        # Replace any empty cells with NA to avoid a warning message.
        is.na(leaders) <- leaders==""
        # Convert columns 5 to 287 to numeric, except column 201 "Age Rng"
        for(i in c(4:201, 203:ncol(leaders))) {
          suppressWarnings(
            leaders[,i] <- as.numeric(as.character(leaders[,i]))
          )
        }
        
        
        
        leaders <- leaders %>%
          make_baseballr_data("MLB Team Batting data from FanGraphs.com",Sys.time())
      },
      error = function(e) {
        message(glue::glue("{Sys.time()}: Invalid arguments or no batter leaders data available!"))
      },
      finally = {
      }
    )
    return(leaders)
  }
  
  else {
    tryCatch(
      expr = {
        if (exc_p){
          payload <- paste0("http://www.fangraphs.com/leaders.aspx?pos=np&stats=bat&lg=", league, "&qual=", qual, 
                            "&type=c,-1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286&season=", y, "&month=0&season1=", x, "&ind=", ind, "&team=0,ts&rost=&age=&filter=&players=&page=1_100000") %>% 
            xml2::read_html()
        } else {
          payload <- paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=", league, "&qual=", qual, 
                            "&type=c,-1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286&season=", y, "&month=0&season1=", x, "&ind=", ind, "&team=0,ts&rost=&age=&filter=&players=&page=1_100000") %>% 
            xml2::read_html()
        }
        
        leaders <- (payload %>%
                      rvest::html_elements("table"))[[17]] %>% 
          rvest::html_table()
        
        leaders <- leaders[-c(1,3),]
        names(leaders) <- leaders[1,]
        leaders <- leaders[-1,]
        leaders <- leaders[,-c(4)] # Remove age
        c <- as.matrix(names(leaders))
        c <- gsub("%", "_pct", c, fixed = TRUE)
        c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
        c <- gsub(" (pi)", "_pi", c, fixed = TRUE)
        c <- gsub("/", "_", c, fixed = TRUE)
        c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
        r <- c("wRC_plus", "WPA_minus", "WPA_plus", "FBall_pct", "AgeRng")
        c[c(61,63,64,72,202),] <- r
        names(leaders) <- c
        leaders <- leaders %>%
          dplyr::select("Season", everything())
        leaders <- as.data.frame(sapply(leaders, function(x) (gsub("%", "", x))))
        leaders <- as.data.frame(sapply(leaders, function(x) (gsub("$", "", x, fixed = TRUE))))
        leaders$Dol <- gsub("\\(", "-", leaders$Dol)
        leaders$Dol <- gsub("\\)", "", leaders$Dol)
        for(i in c(4:201, 203:ncol(leaders))) {
          suppressWarnings(
            leaders[,i] <- as.numeric(as.character(leaders[,i]))
          )
        }
        
        
        
        
        leaders <- leaders %>%
          make_baseballr_data("MLB Team Batting data from FanGraphs.com",Sys.time())
      },
      error = function(e) {
        message(glue::glue("{Sys.time()}: Invalid arguments or no batter leaders data available!"))
      },
      finally = {
      }
    )
    return(leaders)
  }
}





