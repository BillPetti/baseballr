#' @rdname ncaa_baseball_pbp
#' @title **Get Play-By-Play Data for NCAA Baseball Games**
#' @param game_info_url The url for the game's play-by-play data. This can be 
#'  found using the get_ncaa_schedule_info function.
#' @return A data frame with play-by-play data for an individual game.
#'  |col_name       |types     |
#'  |:--------------|:---------|
#'  |date           |character |
#'  |location       |character |
#'  |attendance     |logical   |
#'  |inning         |character |
#'  |inning_top_bot |character |
#'  |score          |character |
#'  |batting        |character |
#'  |fielding       |character |
#'  |description    |character |
#' @importFrom tibble tibble
#' @importFrom tidyr gather spread
#' @importFrom purrr map
#' @importFrom janitor make_clean_names
#' @import rvest 
#' @export
#' @examples \donttest{
#'   x <- ncaa_schedule_info(736, 2021)$game_info_url[2]
#'   ncaa_baseball_pbp(game_info_url = x)
#' }

ncaa_baseball_pbp <- function(game_info_url) {

  payload <- game_info_url %>% 
    xml2::read_html() %>% 
    rvest::html_elements("#root li:nth-child(3) a") %>%
    rvest::html_attr("href") %>%
    as.data.frame() %>%
    dplyr::rename(pbp_url_slug = .data$`.`) %>%
    dplyr::mutate(pbp_url = paste0("https://stats.ncaa.org", .data$pbp_url_slug)) %>%
    dplyr::pull(.data$pbp_url)

  pbp_payload <- payload %>% 
    xml2::read_html()

  game_info <- pbp_payload %>%
    rvest::html_elements("table:nth-child(7)") %>%
    rvest::html_table() %>%
    as.data.frame() %>%
    tidyr::spread(.data$X1, .data$X2) 
  
  game_info <- dplyr::rename_with(game_info,~gsub(":", "", .x)) %>%
    janitor::clean_names() %>%
    dplyr::mutate(game_date = substr(.data$game_date, 1, 10))
  att <- any(!grepl("attendance", colnames(game_info)))
  if (att) {

    game_info$attendance <- NA
  } else {

    game_info <- game_info %>%
      dplyr::mutate(attendance = as.numeric(gsub(",", "", .data$attendance)))
  }

  table_list <- pbp_payload %>%
    rvest::html_elements("[class='mytable']")

  condition <- table_list %>%
    lapply(function(x) nrow(as.data.frame(x %>%
                                            rvest::html_table())) > 3)

  table_list_innings <- table_list[which(unlist(condition))]

  table_list_innings <- table_list_innings %>%
    setNames(seq(1,length(table_list_innings)))

  teams <- tibble::tibble(away = (table_list_innings[[1]] %>%
                            rvest::html_table() %>%
                            as.data.frame())[1,1],
                          home = (table_list_innings[[1]] %>%
                            rvest::html_table() %>%
                            as.data.frame())[1,3])


  
  mapped_table <- purrr::map(.x = table_list_innings,
                             ~format_baseball_pbp_tables(.x, teams = teams)) %>%
    dplyr::bind_rows(.id = "inning")

  mapped_table[1,2] <- ifelse(mapped_table[1,2] == "",
                              "0-0", mapped_table[1,2])

  mapped_table <- mapped_table %>%
    dplyr::mutate(score = ifelse(.data$score == "", NA, .data$score)) %>%
    tidyr::fill(.data$score, .direction = "down")

  mapped_table <- mapped_table %>%
    dplyr::mutate(
      inning_top_bot = ifelse(teams$away == .data$batting, "top", "bot"),
      attendance = game_info$attendance,
      date = game_info$game_date,
      location = game_info$location) %>%
    dplyr::select(
      .data$date, .data$location, .data$attendance, 
      .data$inning, .data$inning_top_bot, dplyr::everything())

  return(mapped_table)
}

#' @rdname get_ncaa_baseball_pbp
#' @title **(legacy) Get Play-By-Play Data for NCAA Baseball Games**
#' @inheritParams ncaa_baseball_pbp
#' @return A data frame with play-by-play data for an individual game.
#' @keywords legacy
#' @export
get_ncaa_baseball_pbp <- ncaa_baseball_pbp


format_baseball_pbp_tables <- function(table_node, teams) {
  table <- (table_node %>% 
              rvest::html_table() %>%
              as.data.frame() %>%
              dplyr::filter(!grepl(pattern = "R:", x = .data$X1)) %>%
              dplyr::mutate(batting = ifelse(.data$X1 != "", teams$away, teams$home)) %>%
              dplyr::mutate(fielding = ifelse(.data$X1 != "", teams$home, teams$away)))[-1,] %>%
    tidyr::gather(key = "X1", value = "value", -c("batting", "fielding", "X2")) %>%
    dplyr::rename(score = .data$X2) %>%
    dplyr::filter(.data$value != "")
  
  table <- table %>%
    dplyr::rename(description = .data$value) %>%
    dplyr::select(-.data$X1)
  
  return(table)
}