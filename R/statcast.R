#' @name statcast
#' @title
#' **Statcast Functions Overview**
#' @description
#' \describe{
#'   \item{```statcast_search()```:}{ Query Statcast by Date Range and Players.}
#'   \item{```statcast_search_batters()```:}{ Query Statcast Batters by Date Range and Player.}
#'   \item{```statcast_search_pitchers()```:}{ Query Statcast Pitchers by Date Range and Player.}
#'   \item{```statcast_leaderboards()```:}{ Query Baseball Savant Leaderboards.}
#' }
#' @details
#' 
#' ### **Query Statcast Batters by Date Range**
#' ```r
#' statcast_search(start_date = "2016-04-06", 
#'                 end_date = "2016-04-15", 
#'                 player_type = 'batter')
#'                             
#'  ## The above is equivalent to:
#' statcast_search_batters(start_date = "2016-04-06", 
#'                         end_date = "2016-04-15", 
#'                         batterid = NULL)
#' ```
#' 
#' ### **Query Statcast Pitchers by Date Range**
#' ```r
#' statcast_search(start_date = "2016-04-06", 
#'                 end_date = "2016-04-15", 
#'                 player_type = 'pitcher')
#'                             
#'  ## The above is equivalent to:
#' statcast_search_pitchers(start_date = "2016-04-06", 
#'                          end_date = "2016-04-15", 
#'                          pitcherid = NULL)
#' ```
#' 
#' ### **Query Statcast Batters by Date Range and Player ID**
#' ```r
#'   correa <- statcast_search(start_date = "2016-04-06", 
#'                             end_date = "2016-04-15", 
#'                             playerid = 621043, 
#'                             player_type = 'batter')
#'                             
#'  ## The above is equivalent to:
#'   correa <- statcast_search_batters(start_date = "2016-04-06", 
#'                                     end_date = "2016-04-15", 
#'                                     batterid = 621043)
#' ```
#' 
#' ### **Query Statcast Pitchers by Date Range and Player ID**
#' ```r
#'   noah <- statcast_search(start_date = "2016-04-06",
#'                           end_date = "2016-04-15", 
#'                           playerid = 592789, 
#'                           player_type = 'pitcher')
#'                           
#'  ## The above is equivalent to:
#'   noah <- statcast_search_pitchers(start_date = "2016-04-06", 
#'                                   end_date = "2016-04-15", 
#'                                   pitcherid = 592789)
#' ```
#' 
#' ### **Query Baseball Savant Leaderboards**
#' ```r
#'   statcast_leaderboards(leaderboard = "exit_velocity_barrels", year = 2021)
#' ```
NULL
