#' @rdname fg_pitcher_leaders
#' @title **Scrape Pitcher Leaderboards from FanGraphs**
#' @param age (integer) Age of players
#' @param pos (character) Position of players, defaults to "all". To exclude pitchers, use "np".
#' @param stats (character) Statistic to return. Defaults to "bat".
#' @param lg (character) League to return. Defaults to "all". Options are "al", "nl", or "all".
#' @param qual (character) Whether you want only batters/pitchers that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimum number of plate appearaces/innings pitched, use the number desired. Defaults to "y".
#' @param startseason (character) Season for which you want to scrape the data.
#' @param endseason (character) Last season for which you want data.
#' @param startdate (character) Start date for which you want data.
#' @param enddate (character) End date for which you want data.
#' @param month (character) Month for which you want data.
#' @param hand (character) Handedness of batter. Options are "L", "R", or "B". Empty string returns all.
#' @param team (character) Teams for which you want data, comma separated.
#' @param pageitems (character) Number of items per page.
#' @param pagenum (character) Page number.
#' @param ind (character) Whether or not to break the seasons out individual, or roll them up together. 1 = split seasons, 0 = aggregate seasons.
#' @param rost (character) Whether or not to include players on the roster. 1 = include, 0 = exclude.
#' @param players (character) Whether or not to include players on the roster. 1 = include only active roster players, 0 = exclude.
#' @param type (character) Defaults to 8, which is the standard leaderboard. The values for the leaderboards appear to go to from type = 0 to 48+, which correspond to links on the leaderboard page.
#' @param postseason (logical) Whether or not to include postseason data. TRUE = include postseason, FALSE = exclude postseason.
#' @param sortdir (character) Sort direction. Options are "asc" or "desc" or "default".
#' @param sortstat (character) Sort by stat. Default is "WAR".
#' @return A data frame of pitcher data.
#' 
#'    |col_name          |types     |
#'    |:-----------------|:---------|
#'    |Season            |integer   |
#'    |team_name         |character |
#'    |Throws            |character |
#'    |xMLBAMID          |integer   |
#'    |PlayerNameRoute   |character |
#'    |PlayerName        |character |
#'    |playerid          |integer   |
#'    |Age               |integer   |
#'    |AgeRng            |character |
#'    |SeasonMin         |integer   |
#'    |SeasonMax         |integer   |
#'    |W                 |integer   |
#'    |L                 |integer   |
#'    |ERA               |numeric   |
#'    |G                 |integer   |
#'    |GS                |integer   |
#'    |CG                |integer   |
#'    |ShO               |integer   |
#'    |SV                |integer   |
#'    |BS                |integer   |
#'    |IP                |numeric   |
#'    |TBF               |integer   |
#'    |H                 |integer   |
#'    |R                 |integer   |
#'    |ER                |integer   |
#'    |HR                |integer   |
#'    |BB                |integer   |
#'    |IBB               |integer   |
#'    |HBP               |integer   |
#'    |WP                |integer   |
#'    |BK                |integer   |
#'    |SO                |integer   |
#'    |GB                |integer   |
#'    |FB                |integer   |
#'    |LD                |integer   |
#'    |IFFB              |integer   |
#'    |Pitches           |integer   |
#'    |Balls             |integer   |
#'    |Strikes           |integer   |
#'    |RS                |integer   |
#'    |IFH               |integer   |
#'    |BU                |integer   |
#'    |BUH               |integer   |
#'    |K_9               |numeric   |
#'    |BB_9              |numeric   |
#'    |K_BB              |numeric   |
#'    |H_9               |numeric   |
#'    |HR_9              |numeric   |
#'    |AVG               |numeric   |
#'    |WHIP              |numeric   |
#'    |BABIP             |numeric   |
#'    |LOB_pct           |numeric   |
#'    |FIP               |numeric   |
#'    |GB_FB             |numeric   |
#'    |LD_pct            |numeric   |
#'    |GB_pct            |numeric   |
#'    |FB_pct            |numeric   |
#'    |IFFB_pct          |numeric   |
#'    |HR_FB             |numeric   |
#'    |IFH_pct           |numeric   |
#'    |BUH_pct           |numeric   |
#'    |TTO_pct           |numeric   |
#'    |CFraming          |numeric   |
#'    |Starting          |numeric   |
#'    |Start_IP          |numeric   |
#'    |RAR               |numeric   |
#'    |WAR               |numeric   |
#'    |Dollars           |numeric   |
#'    |RA9-Wins          |numeric   |
#'    |LOB-Wins          |numeric   |
#'    |BIP-Wins          |numeric   |
#'    |BS-Wins           |numeric   |
#'    |tERA              |numeric   |
#'    |xFIP              |numeric   |
#'    |WPA               |numeric   |
#'    |WPA_minus         |numeric   |
#'    |WPA_plus          |numeric   |
#'    |RE24              |numeric   |
#'    |REW               |numeric   |
#'    |pLI               |numeric   |
#'    |inLI              |numeric   |
#'    |gmLI              |numeric   |
#'    |exLI              |numeric   |
#'    |Pulls             |integer   |
#'    |Games             |integer   |
#'    |WPA_LI            |numeric   |
#'    |Clutch            |numeric   |
#'    |FBall_pct         |numeric   |
#'    |FBv               |numeric   |
#'    |SL_pct            |numeric   |
#'    |SLv               |numeric   |
#'    |CT_pct            |numeric   |
#'    |CTv               |numeric   |
#'    |CB_pct            |numeric   |
#'    |CBv               |numeric   |
#'    |SF_pct            |numeric   |
#'    |SFv               |numeric   |
#'    |XX_pct            |numeric   |
#'    |wFB               |numeric   |
#'    |wSL               |numeric   |
#'    |wCT               |numeric   |
#'    |wCB               |numeric   |
#'    |wSF               |numeric   |
#'    |wFB_C             |numeric   |
#'    |wSL_C             |numeric   |
#'    |wCT_C             |numeric   |
#'    |wCB_C             |numeric   |
#'    |wSF_C             |numeric   |
#'    |O-Swing_pct       |numeric   |
#'    |Z-Swing_pct       |numeric   |
#'    |Swing_pct         |numeric   |
#'    |O-Contact_pct     |numeric   |
#'    |Z-Contact_pct     |numeric   |
#'    |Contact_pct       |numeric   |
#'    |Zone_pct          |numeric   |
#'    |F-Strike_pct      |numeric   |
#'    |SwStr_pct         |numeric   |
#'    |CStr_pct          |numeric   |
#'    |C+SwStr_pct       |numeric   |
#'    |HLD               |integer   |
#'    |SD                |integer   |
#'    |MD                |integer   |
#'    |ERA-              |numeric   |
#'    |FIP-              |numeric   |
#'    |xFIP-             |numeric   |
#'    |K_pct             |numeric   |
#'    |BB_pct            |numeric   |
#'    |K-BB_pct          |numeric   |
#'    |SIERA             |numeric   |
#'    |kwERA             |numeric   |
#'    |RS_9              |numeric   |
#'    |E-F               |numeric   |
#'    |Pull              |integer   |
#'    |Cent              |integer   |
#'    |Oppo              |integer   |
#'    |Soft              |integer   |
#'    |Med               |integer   |
#'    |Hard              |integer   |
#'    |bipCount          |integer   |
#'    |Pull_pct          |numeric   |
#'    |Cent_pct          |numeric   |
#'    |Oppo_pct          |numeric   |
#'    |Soft_pct          |numeric   |
#'    |Med_pct           |numeric   |
#'    |Hard_pct          |numeric   |
#'    |K_9+              |numeric   |
#'    |BB_9+             |numeric   |
#'    |K_BB+             |numeric   |
#'    |H_9+              |numeric   |
#'    |HR_9+             |numeric   |
#'    |AVG+              |numeric   |
#'    |WHIP+             |numeric   |
#'    |BABIP+            |numeric   |
#'    |LOB_pct+          |numeric   |
#'    |K_pct+            |numeric   |
#'    |BB_pct+           |numeric   |
#'    |LD_pct+           |numeric   |
#'    |GB_pct+           |numeric   |
#'    |FB_pct+           |numeric   |
#'    |HRFB_pct+         |numeric   |
#'    |Pull_pct+         |numeric   |
#'    |Cent_pct+         |numeric   |
#'    |Oppo_pct+         |numeric   |
#'    |Soft_pct+         |numeric   |
#'    |Med_pct+          |numeric   |
#'    |Hard_pct+         |numeric   |
#'    |xERA              |numeric   |
#'    |pb_o_CH           |numeric   |
#'    |pb_s_CH           |numeric   |
#'    |pb_c_CH           |numeric   |
#'    |pb_o_CU           |numeric   |
#'    |pb_s_CU           |numeric   |
#'    |pb_c_CU           |numeric   |
#'    |pb_o_FF           |numeric   |
#'    |pb_s_FF           |numeric   |
#'    |pb_c_FF           |numeric   |
#'    |pb_o_SI           |numeric   |
#'    |pb_s_SI           |numeric   |
#'    |pb_c_SI           |numeric   |
#'    |pb_o_SL           |numeric   |
#'    |pb_s_SL           |numeric   |
#'    |pb_c_SL           |numeric   |
#'    |pb_overall        |numeric   |
#'    |pb_stuff          |numeric   |
#'    |pb_command        |numeric   |
#'    |pb_xRV100         |numeric   |
#'    |pb_ERA            |numeric   |
#'    |sp_s_CH           |numeric   |
#'    |sp_l_CH           |numeric   |
#'    |sp_p_CH           |numeric   |
#'    |sp_s_CU           |numeric   |
#'    |sp_l_CU           |numeric   |
#'    |sp_p_CU           |numeric   |
#'    |sp_s_FF           |numeric   |
#'    |sp_l_FF           |numeric   |
#'    |sp_p_FF           |numeric   |
#'    |sp_s_SI           |numeric   |
#'    |sp_l_SI           |numeric   |
#'    |sp_p_SI           |numeric   |
#'    |sp_s_SL           |numeric   |
#'    |sp_l_SL           |numeric   |
#'    |sp_p_SL           |numeric   |
#'    |sp_stuff          |numeric   |
#'    |sp_location       |numeric   |
#'    |sp_pitching       |numeric   |
#'    |PPTV              |integer   |
#'    |CPTV              |integer   |
#'    |BPTV              |integer   |
#'    |DSV               |integer   |
#'    |DGV               |integer   |
#'    |BTV               |integer   |
#'    |rPPTV             |numeric   |
#'    |rBPTV             |numeric   |
#'    |EBV               |integer   |
#'    |ESV               |integer   |
#'    |rFTeamV           |numeric   |
#'    |rBTeamV           |numeric   |
#'    |rTV               |numeric   |
#'    |pfx_FA_pct        |numeric   |
#'    |pfx_SI_pct        |numeric   |
#'    |pfx_SL_pct        |numeric   |
#'    |pfx_CU_pct        |numeric   |
#'    |pfx_CH_pct        |numeric   |
#'    |pfx_vFA           |numeric   |
#'    |pfx_vSI           |numeric   |
#'    |pfx_vSL           |numeric   |
#'    |pfx_vCU           |numeric   |
#'    |pfx_vCH           |numeric   |
#'    |pfx_FA-X          |numeric   |
#'    |pfx_SI-X          |numeric   |
#'    |pfx_SL-X          |numeric   |
#'    |pfx_CU-X          |numeric   |
#'    |pfx_CH-X          |numeric   |
#'    |pfx_FA-Z          |numeric   |
#'    |pfx_SI-Z          |numeric   |
#'    |pfx_SL-Z          |numeric   |
#'    |pfx_CU-Z          |numeric   |
#'    |pfx_CH-Z          |numeric   |
#'    |pfx_wFA           |numeric   |
#'    |pfx_wSI           |numeric   |
#'    |pfx_wSL           |numeric   |
#'    |pfx_wCU           |numeric   |
#'    |pfx_wCH           |numeric   |
#'    |pfx_wFA_C         |numeric   |
#'    |pfx_wSI_C         |numeric   |
#'    |pfx_wSL_C         |numeric   |
#'    |pfx_wCU_C         |numeric   |
#'    |pfx_wCH_C         |numeric   |
#'    |pfx_O-Swing_pct   |numeric   |
#'    |pfx_Z-Swing_pct   |numeric   |
#'    |pfx_Swing_pct     |numeric   |
#'    |pfx_O-Contact_pct |numeric   |
#'    |pfx_Z-Contact_pct |numeric   |
#'    |pfx_Contact_pct   |numeric   |
#'    |pfx_Zone_pct      |numeric   |
#'    |pfx_Pace          |numeric   |
#'    |pi_CH_pct         |numeric   |
#'    |pi_CU_pct         |numeric   |
#'    |pi_FA_pct         |numeric   |
#'    |pi_SI_pct         |numeric   |
#'    |pi_SL_pct         |numeric   |
#'    |pi_vCH            |numeric   |
#'    |pi_vCU            |numeric   |
#'    |pi_vFA            |numeric   |
#'    |pi_vSI            |numeric   |
#'    |pi_vSL            |numeric   |
#'    |pi_CH-X           |numeric   |
#'    |pi_CU-X           |numeric   |
#'    |pi_FA-X           |numeric   |
#'    |pi_SI-X           |numeric   |
#'    |pi_SL-X           |numeric   |
#'    |pi_CH-Z           |numeric   |
#'    |pi_CU-Z           |numeric   |
#'    |pi_FA-Z           |numeric   |
#'    |pi_SI-Z           |numeric   |
#'    |pi_SL-Z           |numeric   |
#'    |pi_wCH            |numeric   |
#'    |pi_wCU            |numeric   |
#'    |pi_wFA            |numeric   |
#'    |pi_wSI            |numeric   |
#'    |pi_wSL            |numeric   |
#'    |pi_wCH_C          |numeric   |
#'    |pi_wCU_C          |numeric   |
#'    |pi_wFA_C          |numeric   |
#'    |pi_wSI_C          |numeric   |
#'    |pi_wSL_C          |numeric   |
#'    |pi_O-Swing_pct    |numeric   |
#'    |pi_Z-Swing_pct    |numeric   |
#'    |pi_Swing_pct      |numeric   |
#'    |pi_O-Contact_pct  |numeric   |
#'    |pi_Z-Contact_pct  |numeric   |
#'    |pi_Contact_pct    |numeric   |
#'    |pi_Zone_pct       |numeric   |
#'    |pi_Pace           |numeric   |
#'    |Events            |integer   |
#'    |EV                |numeric   |
#'    |LA                |numeric   |
#'    |Barrels           |integer   |
#'    |Barrel_pct        |numeric   |
#'    |maxEV             |numeric   |
#'    |HardHit           |integer   |
#'    |HardHit_pct       |numeric   |
#'    |Q                 |numeric   |
#'    |TG                |integer   |
#'    |TIP               |numeric   |
#'    |team_name_abb     |character |
#'    |teamid            |integer   |
#'    |CH_pct            |numeric   |
#'    |CHv               |numeric   |
#'    |wCH               |numeric   |
#'    |wCH_C             |numeric   |
#'    |pb_o_FS           |numeric   |
#'    |pb_s_FS           |numeric   |
#'    |pb_c_FS           |numeric   |
#'    |sp_s_FS           |numeric   |
#'    |sp_l_FS           |numeric   |
#'    |sp_p_FS           |numeric   |
#'    |pfx_FS_pct        |numeric   |
#'    |pfx_vFS           |numeric   |
#'    |pfx_FS-X          |numeric   |
#'    |pfx_FS-Z          |numeric   |
#'    |pfx_wFS           |numeric   |
#'    |pfx_wFS_C         |numeric   |
#'    |pi_FS_pct         |numeric   |
#'    |pi_vFS            |numeric   |
#'    |pi_FS-X           |numeric   |
#'    |pi_FS-Z           |numeric   |
#'    |pi_wFS            |numeric   |
#'    |pi_wFS_C          |numeric   |
#'    |pb_o_FC           |numeric   |
#'    |pb_s_FC           |numeric   |
#'    |pb_c_FC           |numeric   |
#'    |sp_s_FC           |numeric   |
#'    |sp_l_FC           |numeric   |
#'    |sp_p_FC           |numeric   |
#'    |pfx_FC_pct        |numeric   |
#'    |pfx_vFC           |numeric   |
#'    |pfx_FC-X          |numeric   |
#'    |pfx_FC-Z          |numeric   |
#'    |pfx_wFC           |numeric   |
#'    |pfx_wFC_C         |numeric   |
#'    |pi_FC_pct         |numeric   |
#'    |pi_vFC            |numeric   |
#'    |pi_FC-X           |numeric   |
#'    |pi_FC-Z           |numeric   |
#'    |pi_wFC            |numeric   |
#'    |pi_wFC_C          |numeric   |
#'    |pb_o_KC           |numeric   |
#'    |pb_s_KC           |numeric   |
#'    |pb_c_KC           |numeric   |
#'    |sp_s_KC           |numeric   |
#'    |sp_l_KC           |numeric   |
#'    |sp_p_KC           |numeric   |
#'    |rBTV              |numeric   |
#'    |pfx_KC_pct        |numeric   |
#'    |pfx_vKC           |numeric   |
#'    |pfx_KC-X          |numeric   |
#'    |pfx_KC-Z          |numeric   |
#'    |pfx_wKC           |numeric   |
#'    |pfx_wKC_C         |numeric   |
#'    |rCPTV             |numeric   |
#'    |KN_pct            |numeric   |
#'    |KNv               |numeric   |
#'    |wKN               |numeric   |
#'    |wKN_C             |numeric   |
#'    |pfx_KN_pct        |numeric   |
#'    |pfx_vKN           |numeric   |
#'    |pfx_KN-X          |numeric   |
#'    |pfx_KN-Z          |numeric   |
#'    |pfx_wKN           |numeric   |
#'    |pfx_wKN_C         |numeric   |
#'    |pi_KN_pct         |numeric   |
#'    |pi_XX_pct         |numeric   |
#'    |pi_vKN            |numeric   |
#'    |pi_vXX            |numeric   |
#'    |pi_KN-X           |numeric   |
#'    |pi_XX-X           |numeric   |
#'    |pi_KN-Z           |numeric   |
#'    |pi_XX-Z           |numeric   |
#'    |pi_wKN            |numeric   |
#'    |pi_wXX            |numeric   |
#'    |pi_wKN_C          |numeric   |
#'    |pi_wXX_C          |numeric   |
#'    |sp_s_FO           |numeric   |
#'    |sp_l_FO           |numeric   |
#'    |sp_p_FO           |numeric   |
#'    |pfx_FO_pct        |numeric   |
#'    |pfx_vFO           |numeric   |
#'    |pfx_FO-X          |numeric   |
#'    |pfx_FO-Z          |numeric   |
#'    |pfx_wFO           |numeric   |
#'    |pfx_wFO_C         |numeric   |
#'    |rDGV              |numeric   |
#'    |pi_CS_pct         |numeric   |
#'    |pi_vCS            |numeric   |
#'    |pi_CS-X           |numeric   |
#'    |pi_CS-Z           |numeric   |
#'    |pi_wCS            |numeric   |
#'    |pi_wCS_C          |numeric   |
#'    |Relieving         |numeric   |
#'    |Relief_IP         |numeric   |
#'    |rDSV              |numeric   |
#'    |pfx_EP_pct        |numeric   |
#'    |pfx_vEP           |numeric   |
#'    |pfx_EP-X          |numeric   |
#'    |pfx_EP-Z          |numeric   |
#'    |pfx_wEP           |numeric   |
#'    |pfx_wEP_C         |numeric   |
#'    |pfx_SC_pct        |numeric   |
#'    |pfx_vSC           |numeric   |
#'    |pfx_SC-X          |numeric   |
#'    |pfx_SC-Z          |numeric   |
#'    |pfx_wSC           |numeric   |
#'    |pfx_wSC_C         |numeric   |
#'    |pi_SB_pct         |numeric   |
#'    |pi_vSB            |numeric   |
#'    |pi_SB-X           |numeric   |
#'    |pi_SB-Z           |numeric   |
#'    |pi_wSB            |numeric   |
#'    |pi_wSB_C          |numeric   |
#'  
#' @import rvest 
#' @export
#' @examples \donttest{
#'   fg_pitcher_leaders(startseason = 2023, endseason = 2023)
#' }
fg_pitcher_leaders <- function(
    age = "",
    pos = "all",
    stats = "pit",
    lg = "all",
    qual = "0",
    startseason = "2023",
    endseason = "2023",
    startdate = "",
    enddate = "",
    month = "0",
    hand = "",
    team = "0",
    pageitems = "10000",
    pagenum = "1",
    ind = "0",
    rost = "0",
    players = "",
    type = "8",
    postseason = "",
    sortdir = "default",
    sortstat = "WAR") {
  
  params <- list(
    age = age,
    pos = pos,
    stats = stats,
    lg = lg,
    qual = qual,
    season = startseason,
    season1 = endseason,
    startdate = startdate,
    enddate = enddate,
    month = month,
    hand = hand,
    team = team,
    pageitems = pageitems,
    pagenum = pagenum,
    ind = ind,
    rost = rost,
    players = players,
    type = type,
    postseason = postseason,
    sortdir = sortdir,
    sortstat = sortstat
  )
  
  url <- "https://www.fangraphs.com/api/leaders/major-league/data"
  
  fg_endpoint <- httr::modify_url(url, query = params)
  
  tryCatch(
    expr = {
      
      resp <- fg_endpoint %>% 
        mlb_api_call()
      
      fg_df <- resp$data %>% 
        jsonlite::toJSON() %>%
        jsonlite::fromJSON(flatten=TRUE)
      
      c <- colnames(fg_df)
      c <- gsub("%", "_pct", c, fixed = TRUE)
      c <- gsub("/", "_", c, fixed = TRUE)
      c <- ifelse(substr(c, nchar(c) - 1 + 1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
      c <- gsub(" ", "_", c, fixed = TRUE)
      colnames(fg_df) <- c
      leaders <- fg_df %>% 
        dplyr::rename_with(~ gsub("pi", "pi_", .x), starts_with("pi")) %>% 
        dplyr::rename_with(~ gsub("pfx", "pfx_", .x), starts_with("pfx")) %>%
        dplyr::rename(
          "Start_IP" = "Start-IP",
          "Relief_IP" = "Relief-IP",
          "WPA_minus" = "-WPA",
          "WPA_plus" = "+WPA", 
          "AgeRng" = "AgeR",
          "team_name" = "TeamName",
          "team_name_abb" = "TeamNameAbb") %>%
        dplyr::select(-dplyr::any_of(c(
          "Name", 
          "Team"
        ))) %>%
        dplyr::select(
          "Season",
          "team_name",
          "Throws", 
          "xMLBAMID", 
          "PlayerNameRoute",
          "PlayerName",
          "playerid",
          "Age",
          "AgeRng",
          tidyr::everything()) %>% 
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

#' @rdname fg_pitch_leaders
#' @title **(legacy) Scrape Pitcher Leaderboards from FanGraphs**
#' @inheritParams fg_pitcher_leaders
#' @return A data frame of pitcher data.
#' @keywords legacy
#' @export
fg_pitch_leaders <- fg_pitcher_leaders
