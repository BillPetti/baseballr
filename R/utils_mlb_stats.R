mlb_api_call <- function(url){
  # Plain package User-Agent + transient-failure backoff. The MLB Stats API
  # ignores the User-Agent. (FanGraphs requests do NOT use this helper -- they
  # go through fg_api_call(), which sends the Cloudflare-exempt okhttp UA.)
  resp <- httr2::request(url) |>
    httr2::req_user_agent("baseballr (https://github.com/BillPetti/baseballr)") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  json <- resp |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(simplifyVector = TRUE)

  return(json)
}

fg_api_call <- function(url){
  # FanGraphs sits behind Cloudflare. As of 2026-06-03 it serves a JS challenge
  # (HTTP 403, `cf-mitigated: challenge`) to *every* unrecognized client --
  # including plain/library User-Agents; no header/TLS tweak passes it (it needs
  # a JS runtime). The challenge exempts the okhttp client the FanGraphs mobile
  # app uses, so sending `okhttp/4.12.0` returns 200 with normal JSON. Fixes the
  # FanGraphs 403 cluster (#404/#402/#397/#389/#385/#373/#369/#358/#353/#343);
  # okhttp-UA approach from PR #405.
  resp <- httr2::request(url) |>
    httr2::req_user_agent("okhttp/4.12.0") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) == 403) {
    cli::cli_alert_danger("FanGraphs returned HTTP 403 (Cloudflare challenge) for {.url {url}}.")
    cli::cli_alert_info(paste0(
      "The okhttp User-Agent exemption may have changed; see issue #404 / PR #405."))
  }

  resp |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(simplifyVector = TRUE)
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

.ncaa_headers <- function(url = NULL){
  # stats.ncaa.org sits behind Akamai, which returns a 403 "Access Denied" to
  # requests that omit modern browser client-hint / fetch-metadata headers.
  # The header set below mimics a real Chrome navigation and is accepted; the
  # `Host` and `Accept-Encoding` headers are intentionally NOT set here so the
  # HTTP client manages them (an explicit `Host` breaks on redirects, and a
  # hard-coded `br` encoding can yield an undecompressed body).
  headers <- c(
    `User-Agent` = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                          "AppleWebKit/537.36 (KHTML, like Gecko) ",
                          "Chrome/124.0.0.0 Safari/537.36"),
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9",
    `sec-ch-ua` = '"Chromium";v="124", "Google Chrome";v="124", "Not-A.Brand";v="99"',
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = '"Windows"',
    `Sec-Fetch-Dest` = "document",
    `Sec-Fetch-Mode` = "navigate",
    `Sec-Fetch-Site` = "none",
    `Sec-Fetch-User` = "?1",
    `Upgrade-Insecure-Requests` = "1"
  )
  return(headers)
}


