#' @title **MLB Sport IDs**
#' @param sport_id The sport_id to return information for.
#' @return Returns a tibble with the following columns
#'  |col_name           |types     |
#'  |:------------------|:---------|
#'  |sport_id           |integer   |
#'  |sport_code         |character |
#'  |sport_link         |character |
#'  |sport_name         |character |
#'  |sport_abbreviation |character |
#'  |sort_order         |integer   |
#'  |active_status      |logical   |
#'  
#'  and the following values: 
#'  
#'  | sport_id|sport_code |sport_link         |sport_name                            |sport_abbreviation | sort_order|active_status |
#'  |--------:|:----------|:------------------|:-------------------------------------|:------------------|----------:|:-------------|
#'  |        1|mlb        |/api/v1/sports/1   |Major League Baseball                 |MLB                |         11|TRUE          |
#'  |       11|aaa        |/api/v1/sports/11  |Triple-A                              |AAA                |        101|TRUE          |
#'  |       12|aax        |/api/v1/sports/12  |Double-A                              |AA                 |        201|TRUE          |
#'  |       13|afa        |/api/v1/sports/13  |High-A                                |A+                 |        301|TRUE          |
#'  |       14|afx        |/api/v1/sports/14  |Low-A                                 |A                  |        401|TRUE          |
#'  |       16|rok        |/api/v1/sports/16  |Rookie                                |ROK                |        701|TRUE          |
#'  |       17|win        |/api/v1/sports/17  |Winter Leagues                        |WIN                |       1301|TRUE          |
#'  |        8|bbl        |/api/v1/sports/8   |Organized Baseball                    |Pros               |       1401|TRUE          |
#'  |       21|min        |/api/v1/sports/21  |Minor League Baseball                 |Minors             |       1402|TRUE          |
#'  |       23|ind        |/api/v1/sports/23  |Independent Leagues                   |IND                |       2101|TRUE          |
#'  |       51|int        |/api/v1/sports/51  |International Baseball                |INT                |       3501|TRUE          |
#'  |      508|nat        |/api/v1/sports/508 |International Baseball (Collegiate)   |INTC               |       3502|TRUE          |
#'  |      509|nae        |/api/v1/sports/509 |International Baseball (18 and under) |18U                |       3503|TRUE          |
#'  |      510|nas        |/api/v1/sports/510 |International Baseball (16 and under) |16U                |       3505|TRUE          |
#'  |       22|bbc        |/api/v1/sports/22  |College Baseball                      |College            |       5101|TRUE          |
#'  |      586|hsb        |/api/v1/sports/586 |High School Baseball                  |H.S.               |       6201|TRUE      
#' @export
#' @examples \donttest{
#'   try(mlb_sports())
#' }
mlb_sports <- function(sport_id = NULL){
  
  mlb_endpoint <- mlb_stats_endpoint("v1/sports")
  query_params <- list(
    sportId = sport_id
  )
  
  mlb_endpoint <- httr::modify_url(mlb_endpoint, query = query_params)
  
  tryCatch(
    expr={
      resp <- mlb_endpoint %>% 
        mlb_api_call()
      
      sports <- resp$sports %>% 
        jsonlite::toJSON() %>% 
        jsonlite::fromJSON(flatten = TRUE) %>% 
        as.data.frame() %>%  
        janitor::clean_names() %>% 
        dplyr::rename(
          sport_id = .data$id,
          sport_code = .data$code,
          sport_link = .data$link,
          sport_name = .data$name,
          sport_abbreviation = .data$abbreviation) %>%
        make_baseballr_data("MLB Sports data from MLB.com",Sys.time())
      
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments provided"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )
  
  return(sports)
}

