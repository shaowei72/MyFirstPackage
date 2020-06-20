#' Read data from file into a tibble
#'
#' Function that reads in data from a csv file and returns a tibble
#'
#' @param filename Name of the csv file
#'
#' @return This function returns a tibble.
#'
#' @details Function aborts and prints error message if the file filename cannot be found
#'
#' @examples
#' fars_read("input_file.csv")
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generate string with defined format for denoting a filename
#'
#' Function that creates a string accident_XXXX.csv.bz2 where XXXX represents the year
#'
#' @param year Year to tag the filename with
#'
#' @return This function returns a string
#'
#' @examples
#' make_filename("2020")
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read yearly FARS data from list of files into a tibble list
#'
#' Function that reads in data from a csv file and returns a tibble
#'
#' @param years Vector denoting the years for which the yearly FARS data is required
#'
#' @return This function returns a list of tibble, with each element containing the yearly FARS data
#'
#' @details Function aborts and prints error message if there is no FARS data for a particular year
#'
#' @examples
#' fars_read_years(c("2020", "2019", "2018"))
#'
#' @importFrom dplyr  mutate select
#' @importFrom magrittr %>%
#'
#' @note Makes use of make_filename() and fars_read() functions
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Presents number of fatal injuries from FARS by month for  multiple years
#'
#' Function that reads in yearly FARS data, and tabulates the number of fatal injuries by month, for the years of interest
#'
#' @param years Vector denoting the years of interest, i.e.,when the yearly FARS data is required
#'
#' @return This function returns a table showing the number of fatal injuries by month, for the years of interest
#'
#' @note Makes use of fars_read_years() functions
#'
#' @importFrom magrittr  %>%
#'
#' @importFrom dplyr bind_rows group_by summarize
#'
#' @importFrom tidyr spread
#'
#' @examples
#' fars_summarize_years(c("2020", "2019", "2015"))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list)
  %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots fatal accidents in a State for a particular year
#'
#' Function that renders on a map the location of fatal accidents that occurred in a specific State in a particular year
#'
#' @param year Integer representing the year of interest
#' @param state.num Integer denoting the state
#'
#' @return This function renders and returns a map object
#'
#' @details Function aborts if the state number state.num is invalid
#'
#' @note Makes use of fars_read() and make_filename() functions
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr  filter
#'
#' @examples
#' fars_map_state(20, 2020)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
