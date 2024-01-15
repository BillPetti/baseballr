#' @name mlb_award
#' @title
#' **MLB All-Star, Awards, Home Run Derby Functions**
#' @description
#' \describe{
#'   \item{```mlb_all_star_ballots()```:}{ Find MLB All-Star Ballots.}
#'   \item{```mlb_all_star_final_vote()```:}{ Find MLB All-Star Final Vote.}
#'   \item{```mlb_all_star_write_ins()```:}{ Find MLB All-Star Write-ins.}
#'   \item{```mlb_awards()```:}{ Find MLB Awards.}
#'   \item{```mlb_awards_recipient()```:}{ Find MLB Award Recipients.}
#'   \item{```mlb_homerun_derby()```:}{ Retrieve MLB Home Run Derby Data.}
#'   \item{```mlb_homerun_derby_bracket()```:}{ Retrieve MLB Home Run Derby Bracket.}
#'   \item{```mlb_homerun_derby_players()```:}{ Retrieve MLB Home Run Derby Players.}
#' }
#' @details
#' ### **Find MLB All-Star Ballots**
#' ```r
#'   try(mlb_all_star_ballots(league_id = 103, season = 2021))
#' ```
#' ### **Find MLB All-Star Final Vote**
#' ```r
#'   try(mlb_all_star_final_vote(league_id = 103, season = 2021))
#' ```
#' ### **Find MLB All-Star Write-ins**
#' ```r
#'   try(mlb_all_star_write_ins(league_id = 103, season = 2021))
#' ```
#' ### **Find MLB Awards** 
#' ```r
#'   try(mlb_awards())
#' ```
#' ### **Find MLB Award Recipients** 
#' ```r
#'   try(mlb_awards_recipient(award_id = 'MLBHOF', season = 2020))
#' ```
#' ### **Retrieve MLB Home Run Derby Data** 
#' ```r
#'   try(mlb_homerun_derby(game_pk = 511101))
#' ```
#' ### **Retrieve MLB Home Run Derby Bracket** 
#' ```r
#'    try(mlb_homerun_derby_bracket(game_pk = 511101))
#' ```
#' ### **Retrieve MLB Home Run Derby Players**
#' ```r
#'    try(mlb_homerun_derby_players(game_pk = 511101))
#' ```
#' 
#' 
NULL
