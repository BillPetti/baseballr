mlb_api_call <- function(url){
  # Identify with a plain package User-Agent and back off on transient failures.
  # IMPORTANT: do NOT send a browser User-Agent here. The FanGraphs leaders
  # endpoints (fg_*_leaders(), fg_team_*()) route through this helper and sit
  # behind Cloudflare, which serves a "Just a moment..." JS challenge (HTTP 403)
  # to requests that *claim to be a browser* but cannot solve the challenge. A
  # plain library/package User-Agent is not challenged and returns the data
  # (#385, #397, #369, #343). The MLB Stats API ignores the User-Agent.
  res <- httr::RETRY(
    "GET", url,
    httr::user_agent("baseballr (https://github.com/BillPetti/baseballr)"),
    times = 3,
    pause_base = 1,
    quiet = TRUE
  )

  # If FanGraphs ever tightens Cloudflare to challenge all clients, surface the
  # 403 with a clear message instead of a cryptic downstream parse error.
  if (httr::status_code(res) == 403) {
    cli::cli_alert_danger("Request returned HTTP 403 (Forbidden) for {.url {url}}.")
    cli::cli_alert_info(paste0(
      "FanGraphs is serving a Cloudflare anti-bot challenge; wait and retry ",
      "later, or route requests through a proxy."))
  }

  json <- res$content |>
    rawToChar() |>
    jsonlite::fromJSON(simplifyVector = TRUE)

  return(json)
}

mlb_stats_endpoint <- function(endpoint){
  all_endpoints = c(
    "v1/attendance",#
    "v1/conferences",#
    "v1/conferences/{conferenceId}",#
    "v1/awards/{awardId}/recipients",#
    "v1/awards",#
    "v1/baseballStats",#
    "v1/eventTypes",#
    "v1/fielderDetailTypes",#
    "v1/gameStatus",#
    "v1/gameTypes",#
    "v1/highLow/types",#
    "v1/hitTrajectories",#
    "v1/jobTypes",#
    "v1/languages",
    "v1/leagueLeaderTypes",#
    "v1/logicalEvents",#
    "v1/metrics",#
    "v1/pitchCodes",#
    "v1/pitchTypes",#
    "v1/playerStatusCodes",#
    "v1/positions",#
    "v1/reviewReasons",#
    "v1/rosterTypes",#
    "v1/runnerDetailTypes",#
    "v1/scheduleEventTypes",#
    "v1/situationCodes",#
    "v1/sky",#
    "v1/standingsTypes",#
    "v1/statGroups",#
    "v1/statTypes",#
    "v1/windDirection",#
    "v1/divisions",#
    "v1/draft/{year}",#
    "v1/draft/prospects/{year}",#
    "v1/draft/{year}/latest",#
    "v1.1/game/{gamePk}/feed/live",
    "v1.1/game/{gamePk}/feed/live/diffPatch",#
    "v1.1/game/{gamePk}/feed/live/timestamps",#
    "v1/game/changes",##x
    "v1/game/analytics/game",##x
    "v1/game/analytics/guids",##x
    "v1/game/{gamePk}/guids",##x
    "v1/game/{gamePk}/{GUID}/analytics",##x
    "v1/game/{gamePk}/{GUID}/contextMetricsAverages",##x
    "v1/game/{gamePk}/contextMetrics",#
    "v1/game/{gamePk}/winProbability",#
    "v1/game/{gamePk}/boxscore",#
    "v1/game/{gamePk}/content",#
    "v1/game/{gamePk}/feed/color",##x
    "v1/game/{gamePk}/feed/color/diffPatch",##x
    "v1/game/{gamePk}/feed/color/timestamps",##x
    "v1/game/{gamePk}/linescore",#
    "v1/game/{gamePk}/playByPlay",#
    "v1/gamePace",#
    "v1/highLow/{orgType}",#
    "v1/homeRunDerby/{gamePk}",#
    "v1/homeRunDerby/{gamePk}/bracket",#
    "v1/homeRunDerby/{gamePk}/pool",#
    "v1/league",#
    "v1/league/{leagueId}/allStarBallot",#
    "v1/league/{leagueId}/allStarWriteIns",#
    "v1/league/{leagueId}/allStarFinalVote",#
    "v1/people",#
    "v1/people/freeAgents",#
    "v1/people/{personId}",##U
    "v1/people/{personId}/stats/game/{gamePk}",#
    "v1/people/{personId}/stats/game/current",#
    "v1/jobs",#
    "v1/jobs/umpires",#
    "v1/jobs/datacasters",#
    "v1/jobs/officialScorers",#
    "v1/jobs/umpires/games/{umpireId}",##x
    "v1/schedule/",#
    "v1/schedule/games/tied",#
    "v1/schedule/postseason",#
    "v1/schedule/postseason/series",#
    "v1/schedule/postseason/tuneIn",##x
    "v1/seasons",#
    "v1/seasons/all",#
    "v1/seasons/{seasonId}",#
    "v1/sports",#
    "v1/sports/{sportId}",#
    "v1/sports/{sportId}/players",#
    "v1/standings",#
    "v1/stats",#
    "v1/stats/metrics",##x
    "v1/stats/leaders",#
    "v1/stats/streaks",##404
    "v1/teams",#
    "v1/teams/history",#
    "v1/teams/stats",#
    "v1/teams/stats/leaders",#
    "v1/teams/affiliates",#
    "v1/teams/{teamId}",#
    "v1/teams/{teamId}/stats",#
    "v1/teams/{teamId}/affiliates",#
    "v1/teams/{teamId}/alumni",#
    "v1/teams/{teamId}/coaches",#
    "v1/teams/{teamId}/personnel",#
    "v1/teams/{teamId}/leaders",#
    "v1/teams/{teamId}/roster",##x
    "v1/teams/{teamId}/roster/{rosterType}",#
    "v1/venues"#
  )
  base_url = glue::glue('http://statsapi.mlb.com/api/{endpoint}')
  return(base_url)
}

.ncaa_headers <- function(url){
  headers <- c(
    `Host` = 'stats.ncaa.org',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
    `Accept` = 'application/json, text/html, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
  )
  return(headers)
}


