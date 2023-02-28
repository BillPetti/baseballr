#' @rdname ncaa_baseball_pbp
#' @title **Get Play-By-Play Data for NCAA Baseball Games**
#' @param game_info_url The url for the game's boxscore data. This can be 
#'  found using the ncaa_schedule_info function.
#' @param game_pbp_url The url for the game's play-by-play data. This can be 
#'  found using the ncaa_schedule_info function.
#' @param raw_html_to_disk Write raw html to disk
#' @param raw_html_path Directory path to write raw html
#' @param read_from_file Read from raw html on disk
#' @param file File with full path to read raw html
#' @param ... Additional arguments passed to an underlying function like httr.
#' @return A data frame with play-by-play data for an individual game.
#' 
#'  |col_name       |types     |
#'  |:--------------|:---------|
#'  |game_date      |character |
#'  |location       |character |
#'  |attendance     |logical   |
#'  |inning         |character |
#'  |inning_top_bot |character |
#'  |score          |character |
#'  |batting        |character |
#'  |fielding       |character |
#'  |description    |character |
#'  |game_pbp_url   |character |
#'  |game_pbp_id    |integer   |
#'  
#' @importFrom tibble tibble
#' @importFrom tidyr gather spread
#' @importFrom purrr map
#' @importFrom janitor make_clean_names
#' @import rvest 
#' @export
#' @examples \donttest{
#'   x <- ncaa_schedule_info(736, 2021)$game_info_url[2]
#'   try(ncaa_baseball_pbp(game_info_url = "https://stats.ncaa.org/contests/2167178/box_score"))
#' }

ncaa_baseball_pbp <- function(game_info_url = NA_character_, 
                              game_pbp_url = NA_character_,
                              raw_html_to_disk = FALSE, 
                              raw_html_path = "/",
                              read_from_file = FALSE,
                              file = NA_character_,
                              ...) {
  
  dots <- rlang::dots_list(..., .named = TRUE)
  proxy <- dots$.proxy
  
  tryCatch(
    expr = {
      if (is.na(game_info_url) && is.na(game_pbp_url) && is.na(file)){
        message(glue::glue("{Sys.time()}: No game_info_url, game_pbp_url, or file provided"))
        return(NULL)
      }
      if (read_from_file == FALSE && !is.na(game_info_url)) {
        contest_id <- as.integer(stringr::str_extract(game_info_url, "\\d+"))
        
        
        game_info_resp <- httr::RETRY("GET", url = game_info_url, proxy, httr::add_headers(.headers = .ncaa_headers()))
        
        check_status(game_info_resp)
        
        init_payload <- game_info_resp %>% 
          httr::content(as = "text", encoding = "UTF-8") %>% 
          xml2::read_html() 
        
        game_pbp_url <- init_payload %>% 
          rvest::html_elements("#root li:nth-child(3) a") %>%
          rvest::html_attr("href") %>%
          as.data.frame() %>%
          dplyr::rename(pbp_url_slug = ".") %>%
          dplyr::mutate(game_pbp_url = paste0("https://stats.ncaa.org", .data$pbp_url_slug)) %>%
          dplyr::pull(.data$game_pbp_url)
        
        pbp_payload_resp <- httr::RETRY("GET", url = game_pbp_url, proxy, httr::add_headers(.headers = .ncaa_headers()))
        
        check_status(pbp_payload_resp)
        
        pbp_payload <- pbp_payload_resp %>% 
          httr::content(as = "text", encoding = "UTF-8") %>% 
          xml2::read_html()
        
        if (raw_html_to_disk == TRUE) {
          pbp_id <- as.integer(stringr::str_extract(payload,"\\d+"))
          xml2::write_xml(pbp_payload, file = glue::glue("{raw_html_path}{pbp_id}.html"))
        }
      }
      if (read_from_file == FALSE && !is.na(game_pbp_url)) {
        payload <- game_pbp_url
        pbp_payload_resp <- httr::RETRY("GET", url = game_pbp_url, proxy, httr::add_headers(.headers = .ncaa_headers()))
        
        check_status(pbp_payload_resp)
        
        pbp_payload <- pbp_payload_resp %>% 
          httr::content(as = "text", encoding = "UTF-8") %>% 
          xml2::read_html()
        
        if (raw_html_to_disk == TRUE) {
          pbp_id <- as.integer(stringr::str_extract(game_pbp_url,"\\d+"))
          xml2::write_xml(pbp_payload, file = glue::glue("{raw_html_path}{pbp_id}.html"))
        }
      }
      if ((read_from_file == TRUE) && (!is.na(file))) {
        pbp_payload <- file %>% 
          xml2::read_html()
        payload <- stringr::str_extract(file, "\\d+")
      }
      
      game_info <- pbp_payload %>%
        rvest::html_elements("table:nth-child(7)") %>%
        rvest::html_table() %>%
        as.data.frame() %>%
        tidyr::spread("X1", "X2") 
      
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
        tidyr::fill("score", .direction = "down")
      
      mapped_table <- mapped_table %>%
        dplyr::mutate(
          inning_top_bot = ifelse(teams$away == .data$batting, "top", "bot"),
          attendance = game_info$attendance,
          game_date = game_info$game_date,
          location = game_info$location,
          year = as.integer(stringr::str_extract(.data$game_date, "\\d{4}")),
          game_pbp_url = payload,
          game_pbp_id = as.integer(stringr::str_extract(payload, "\\d+"))) %>%
        dplyr::select(
          "game_date", "location", "attendance", 
          "inning", "inning_top_bot", tidyr::everything()) %>%
        make_baseballr_data("NCAA Baseball Play-by-Play data from stats.ncaa.org",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided for game_info_url, {game_info_url}"))
    },
    finally = {
    }
  )
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
    dplyr::rename("score" = "X2") %>%
    dplyr::filter(.data$value != "")
  
  table <- table %>%
    dplyr::rename("description" = "value") %>%
    dplyr::select(-"X1")
  
  return(table)
}