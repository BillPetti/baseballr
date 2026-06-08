#' @name chadwick
#' @title
#' **Chadwick Bureau Register Player Lookup**
#' @description
#'
#' * `chadwick_player_lu()`: Directly download the Chadwick Bureau's public register of baseball players and the various IDs associated with them in different systems of record.
#' * `playername_lookup()`: Look up Baseball Player Name.
#' * `playerid_lookup()`: Look up Baseball Player IDs.
#'
#' @details
#' ### **Directly download the Chadwick Bureau's public register of baseball players.**
#' ```r
#'   chadwick_player_lu()
#' ```
#' ### **Look up baseball player name by ID**
#' ```r
#'   playername_lookup(4885)
#'   playername_lookup("kaaihki01")
#' ```
#' ### **Look up baseball player IDs by player name**
#' ```r
#'   playerid_lookup("Garcia", "Karim")
#' ```
NULL
