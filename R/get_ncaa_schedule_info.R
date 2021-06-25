#' Get Schedule and Results for NCAA Baseball Teams
#'
#' @param teamid The team's unique NCAA id.
#' @param year The season (i.e. use 2016 for the 2015-2016 season,
#' etc.)
#'
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom tibble tibble rownames_to_column
#' @importFrom tidyr separate
#' @importFrom stringr str_trim str_extract
#' @return A dataframe with the following fields: date, opponent,
#' result, score, innings (if more than regulation), and the url
#' for the game itself.
#' @export
#'
#' @examples \dontrun{get_ncaa_schedule_info(teamid =736, year = 2017)}

get_ncaa_schedule_info <- function(teamid = NULL,
                                   year = NULL) {

  id <- subset(baseballr::ncaa_season_id_lu, baseballr::ncaa_season_id_lu$season == year, select = id)

  school_info <- baseballr::master_ncaa_team_lu %>% 
    dplyr::filter(.data$school_id == teamid, .data$year == year) %>%
    dplyr::select(-.data$year) %>%
    dplyr::distinct()

  url <- paste0("https://stats.ncaa.org/team/", teamid, "/", id)

  payload <- xml2::read_html(url)

  if (year %in% c(2019,2020,2021)) {

    dates <- payload %>%
      rvest::html_nodes("fieldset td:nth-child(1)") %>%
      rvest::html_text() %>%
      as.data.frame() %>%
      dplyr::rename(date = .data$`.`) %>%
      dplyr::filter(!.data$date == "") %>%
      dplyr::mutate(date = gsub("\\s*\\([^\\)]+\\)", "", .data$date)) %>%
      tibble::rownames_to_column('row')

    game_opponents <- payload %>%
      rvest::html_nodes("fieldset td:nth-child(2)") %>%
      rvest::html_text() %>%
      as.data.frame() %>%
      dplyr::rename(opponent = .data$`.`) %>%
      dplyr::filter(!.data$opponent == "") %>%
      dplyr::mutate(opponent = str_trim(.data$opponent)) %>%
      tibble::rownames_to_column('row')

    game_info_url <- payload %>%
      rvest::html_nodes("fieldset td .skipMask") %>%
      rvest::html_attr("href") %>%
      as.data.frame() %>%
      dplyr::rename(slug = .data$`.`) %>%
      dplyr::mutate(game_info_url = paste0("https://stats.ncaa.org", .data$slug)) %>%
      tibble::rownames_to_column('row')

    game_result <- payload %>%
      rvest::html_nodes("fieldset td:nth-child(3)") %>%
      rvest::html_text() %>%
      as.data.frame() %>%
      dplyr::rename(result = .data$`.`) %>%
      dplyr::mutate(result = str_trim(.data$result)) 
    suppressWarnings(
    game_result <- game_result %>%
      tidyr::separate(.data$result, into = c("result", "score", "innings"),
                      sep = " ") %>% 
      dplyr::filter(.data$result != "RPI")
    )
    game_result <- game_result %>%
      dplyr::mutate(innings = str_extract(.data$innings, "[[0-9]]+"))  %>%
      tibble::rownames_to_column('row')

    postponed_rows <- game_result %>%
      dplyr::filter(.data$result == 'Ppd') %>%
      dplyr::pull(.data$row)

    if(length(postponed_rows) > 0) {

      game_info_url <- game_result %>%
        dplyr::filter(.data$result != 'Ppd') %>%
        dplyr::bind_cols(game_info_url[,c(2,3)]) %>%
        dplyr::select(.data$row, .data$slug, .data$game_info_url)
    }

    game_info <- dplyr::full_join(dates, game_opponents, by = 'row')

    game_info <- dplyr::full_join(game_info, game_result, by = 'row')

    game_info <- dplyr::full_join(game_info, game_info_url, by = 'row')

    game_info <- game_info %>%
      dplyr::select(-.data$row)

  } else {

    dates <- payload %>%
      rvest::html_nodes(".smtext:nth-child(1)") %>%
      rvest::html_text() %>%
      as.data.frame() %>%
      dplyr::rename(date = .data$`.`) %>%
      dplyr::filter(!.data$date == "") %>%
      dplyr::mutate(date = gsub("\\s*\\([^\\)]+\\)", "", .data$date)) %>%
      tibble::rownames_to_column('row')

    game_opponents <- payload %>%
      rvest::html_nodes(".smtext:nth-child(2)") %>%
      rvest::html_text() %>%
      as.data.frame() %>%
      dplyr::rename(opponent = .data$`.`) %>%
      dplyr::filter(!.data$opponent == "") %>%
      dplyr::mutate(opponent = str_trim(.data$opponent)) %>%
      tibble::rownames_to_column('row')

    game_info_url <- payload %>%
      rvest::html_nodes(".smtext .skipMask") %>%
      rvest::html_attr("href") %>%
      as.data.frame() %>%
      dplyr::rename(slug = .data$`.`) %>%
      dplyr::mutate(game_info_url = paste0("https://stats.ncaa.org", .data$slug)) %>%
      tibble::rownames_to_column('row')

    game_result <- payload %>%
      rvest::html_nodes(".smtext .skipMask") %>%
      rvest::html_text() %>%
      as.data.frame() %>%
      dplyr::rename(result = .data$`.`) %>%
      dplyr::mutate(result = str_trim(.data$result)) %>%
      dplyr::mutate(result = str_trim(.data$result)) 
    suppressWarnings(
    game_result <- game_result %>%
      tidyr::separate(.data$result, into = c("result", "score", "div",
                                       "opp_score", "innings"), sep = " ") 
    )
    game_result <- game_result %>%
      dplyr::mutate(score = paste0(.data$score, .data$div, .data$opp_score)) %>%
      dplyr::select(-.data$div, -.data$opp_score) %>%
      dplyr::mutate(innings = str_extract(.data$innings, "[[0-9]]+"))  %>%
      tibble::rownames_to_column('row')

    postponed_rows <- game_result %>%
      filter(.data$result == 'Ppd') %>%
      pull(.data$row)

    if(length(postponed_rows) > 0) {

      game_info_url <- game_result %>%
        dplyr::filter(.data$result != 'Ppd') %>%
        dplyr::bind_cols(game_info_url[,c(2,3)]) %>%
        dplyr::select(.data$row, .data$slug, .data$game_info_url)
    }

    game_info <- dplyr::full_join(dates, game_opponents, by = 'row')

    game_info <- dplyr::full_join(game_info, game_result, by = 'row')

    game_info <- dplyr::full_join(game_info, game_info_url, by = 'row')

    game_info <- game_info %>%
      dplyr::select(-.data$row)
  }

  return(game_info)
}
