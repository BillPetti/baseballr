#' Internal helper -- ESPN baseball league news
#'
#' Fetches the top-level news feed for an ESPN baseball league and returns a
#' tidy tibble of articles. This is the single implementation shared by
#' `espn_mbb_news()` (league = "mlb") and
#' `espn_mlb_news()` (league = "mlb").
#'
#' @param league character. One of `"mlb"`.
#' @param limit integer. Maximum number of articles to return (default `50`).
#' @param ... Currently unused; reserved for future pass-through arguments.
#' @return A `baseballr_data` tibble of articles, or `NULL` on error.
#' @noRd
.espn_baseball_news <- function(league, limit = 50, ...) {
  .espn_baseball_validate_league(league)

  .args <- mget(setdiff(names(formals()), "..."))

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/baseball/",
    league,
    "/news?limit=",
    as.integer(limit)
  )

  news <- .empty_baseballr_data(paste0("ESPN ", toupper(league), " News from ESPN.com"))

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)

      resp <- res %>%
        .resp_text()

      raw <- jsonlite::fromJSON(resp, flatten = TRUE)

      articles <- raw[["articles"]]

      if (is.null(articles) || !is.data.frame(articles) || nrow(articles) == 0) {
        news <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0("ESPN ", toupper(league), " News from ESPN.com"),
            Sys.time()
          )
        return(news)
      }

      # Keep a curated set of top-level columns; use any_of so schema drift is
      # absorbed silently.
      keep_cols <- c(
        "id", "type", "headline", "description", "published",
        "premium", "byline"
      )

      # Pull web link if present
      if ("links.web.href" %in% colnames(articles)) {
        articles[["link_web"]] <- articles[["links.web.href"]]
      } else if (is.data.frame(articles[["links"]]) &&
                 "web.href" %in% colnames(articles[["links"]])) {
        articles[["link_web"]] <- articles[["links"]][["web.href"]]
      } else {
        articles[["link_web"]] <- NA_character_
      }

      # Pull league id from categories if available
      articles[["league_id"]] <- NA_character_
      if ("categories" %in% colnames(articles)) {
        cats <- articles[["categories"]]
        if (is.list(cats)) {
          articles[["league_id"]] <- vapply(cats, function(cat_df) {
            if (!is.data.frame(cat_df) || nrow(cat_df) == 0) return(NA_character_)
            league_rows <- cat_df[!is.na(cat_df[["type"]]) &
                                    cat_df[["type"]] == "league", , drop = FALSE]
            if (nrow(league_rows) == 0) return(NA_character_)
            as.character(league_rows[["leagueId"]][[1]] %||% NA_character_)
          }, character(1))
        }
      }

      keep_cols_ext <- c(keep_cols, "link_web", "league_id")

      news <- articles %>%
        dplyr::select(dplyr::any_of(keep_cols_ext)) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0("ESPN ", toupper(league), " News from ESPN.com"),
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0("Failed to retrieve ESPN ", league, " news"),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0("Warning retrieving ESPN ", league, " news"),
      args = .args
    ),
    finally = {}
  )
  return(news)
}


#' Internal helper -- ESPN baseball team news
#'
#' Fetches the news feed for a specific team in an ESPN baseball league and
#' returns a tidy tibble of articles. This is the single implementation shared
#' by `espn_mbb_team_news()` and `espn_mlb_team_news()`.
#'
#' @param league character. One of `"mlb"`.
#' @param team_id character or integer. ESPN team ID.
#' @param limit integer. Maximum number of articles to return (default `25`).
#' @param ... Currently unused; reserved for future pass-through arguments.
#' @return A `baseballr_data` tibble of articles, or `NULL` on error.
#' @noRd
.espn_baseball_team_news <- function(league, team_id, limit = 25, ...) {
  .espn_baseball_validate_league(league)

  .args <- mget(setdiff(names(formals()), "..."))

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/baseball/",
    league,
    "/news?team=",
    team_id,
    "&limit=",
    as.integer(limit)
  )

  news <- .empty_baseballr_data(paste0( "ESPN ", toupper(league), " Team News (team_id=", team_id, ") from ESPN.com" ))

  tryCatch(
    expr = {
      res <- .retry_request(url)
      check_status(res)

      resp <- res %>%
        .resp_text()

      raw <- jsonlite::fromJSON(resp, flatten = TRUE)

      articles <- raw[["articles"]]

      if (is.null(articles) || !is.data.frame(articles) || nrow(articles) == 0) {
        news <- data.frame(stringsAsFactors = FALSE) %>%
          dplyr::as_tibble() %>%
          make_baseballr_data(
            paste0(
              "ESPN ", toupper(league), " Team News (team_id=", team_id,
              ") from ESPN.com"
            ),
            Sys.time()
          )
        return(news)
      }

      keep_cols <- c(
        "id", "type", "headline", "description", "published",
        "premium", "byline"
      )

      if ("links.web.href" %in% colnames(articles)) {
        articles[["link_web"]] <- articles[["links.web.href"]]
      } else if (is.data.frame(articles[["links"]]) &&
                 "web.href" %in% colnames(articles[["links"]])) {
        articles[["link_web"]] <- articles[["links"]][["web.href"]]
      } else {
        articles[["link_web"]] <- NA_character_
      }

      articles[["league_id"]] <- NA_character_
      if ("categories" %in% colnames(articles)) {
        cats <- articles[["categories"]]
        if (is.list(cats)) {
          articles[["league_id"]] <- vapply(cats, function(cat_df) {
            if (!is.data.frame(cat_df) || nrow(cat_df) == 0) return(NA_character_)
            league_rows <- cat_df[!is.na(cat_df[["type"]]) &
                                    cat_df[["type"]] == "league", , drop = FALSE]
            if (nrow(league_rows) == 0) return(NA_character_)
            as.character(league_rows[["leagueId"]][[1]] %||% NA_character_)
          }, character(1))
        }
      }

      keep_cols_ext <- c(keep_cols, "link_web", "league_id")

      news <- articles %>%
        dplyr::select(dplyr::any_of(keep_cols_ext)) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        dplyr::as_tibble() %>%
        janitor::clean_names() %>%
        make_baseballr_data(
          paste0(
            "ESPN ", toupper(league), " Team News (team_id=", team_id,
            ") from ESPN.com"
          ),
          Sys.time()
        )
    },
    error = function(e) .report_api_error(
      e,
      hint = paste0(
        "Failed to retrieve ESPN ", league, " team news for team_id=", team_id
      ),
      args = .args
    ),
    warning = function(w) .report_api_warning(
      w,
      hint = paste0(
        "Warning retrieving ESPN ", league, " team news for team_id=", team_id
      ),
      args = .args
    ),
    finally = {}
  )
  return(news)
}
