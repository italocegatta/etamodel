library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)


eta_days <- function() {
  xml2::read_html("http://ftp.cptec.inpe.br/modelos/io/tempo/regional/Eta15km/embrapa/") %>%
    rvest::html_table() %>%
    '[['(1) %>%
    '['(-1, 2) %>%
    '['(!stringr::str_detect(., "12/$")) %>%
    stringr::str_sub(1, 8)
}

eta_get_data <- function(x) {
  readr::read_csv2(
    x,
    col_names = c("lon", "lat", "p"),
    col_types = readr::cols(
      lon = readr::col_number(),
      lat = readr::col_number(),
      p = readr::col_number()
    )
  )
}

eta_down <- function(x) {

  # tem formato de data?
  # pode ser convertido?
  d <- lubridate::ymd(x)

  url_root <- "http://ftp.cptec.inpe.br/modelos/io/tempo/regional/Eta15km/embrapa/"
  url_day <- paste0(url_root, stringr::str_replace_all(d, "-", ""), "00/")

  s <- xml2::read_html(url_day)

  url_files <- s %>%
    rvest::html_table() %>%
    '[['(1) %>%
    '['(-1, "Name") %>%
    paste0(url_day, .)

  z <- s %>%
    rvest::html_table() %>%
    '[['(1) %>%
    '['(-1, ) %>%
    dplyr::select(Name) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      base_date = d,
      forecast_date = lubridate::ymd(stringr::str_sub(Name, 4, 13), locale = "English_United States.1252"),
      data = purrr::map(Name, ~eta_get_data(paste0(url_day, .x)))
    ) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate_at(dplyr::vars(lon, lat, p), function(x) x / 100)

  return(z)
}


#' @export
eta_download <- function(path, base_date = NULL) {

  if(is.null(base_date)) {
    files_avaliable <- paste0(eta_days(),".txt")
    # d <- today() # futuro
    # file_name <- str_replace_all(d, "-", "")
  } else {
    files_avaliable <- paste0(base_date,".txt")
  }


  files_downloaded <- list.files(path = path, pattern = ".txt")

  # Precisa de alternativa quando não tem nenhum arquivo baixado e o match é 0
  files_to_download <- files_avaliable[ !files_avaliable %in% files_downloaded]

  stringr::str_replace_all(files_to_download, ".txt", "") %>%
    purrr::map(eta_down) %>%
    purrr::walk2(
      paste(path, files_to_download, sep = "/"),
      ~write.table(.x, .y,
                   na = "", sep = ";", row.names = FALSE, dec = ","
      )
    )
}
