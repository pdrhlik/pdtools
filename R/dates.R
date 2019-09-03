#' Add an hourly variable to group by.
#'
#' @param df Data frame.
#' @param col Column name of Date or POSIXct type.
#'
#' @return Data frame with a new column \code{da}.
#' @export
hourly <- function(df, col) {
  return(add_date_col(df, rlang::enquo(col), "hours"))
}

#' Add a daily variable to group by.
#'
#' @param df Data frame.
#' @param col Column name of Date or POSIXct type.
#'
#' @return Data frame with a new column \code{da}.
#' @export
daily <- function(df, col) {
  return(add_date_col(df, rlang::enquo(col), "days"))
}

#' Add a weekly variable to group by.
#'
#' @param df Data frame.
#' @param col Column name of Date or POSIXct type.
#'
#' @return Data frame with a new column \code{we}.
#' @export
weekly <- function(df, col) {
  return(add_date_col(df, rlang::enquo(col), "weeks"))
}

#' Add a monthly variable to group by.
#'
#' @param df Data frame.
#' @param col Column name of Date or POSIXct type.
#'
#' @return Data frame with a new column \code{mo}.
#' @export
monthly <- function(df, col) {
  return(add_date_col(df, rlang::enquo(col), "months"))
}

#' Add a yearly variable to group by.
#'
#' @param df Data frame.
#' @param col Column name of Date or POSIXct type.
#'
#' @return Data frame with a new column \code{ye}.
#' @export
yearly <- function(df, col) {
  return(add_date_col(df, rlang::enquo(col), "years"))
}

#' Floor date by days, weeks, months, etc.
#'
#' @param d Date or POSIXct vector
#' @param unit Units as in \code{lubridate::floor_date()}.
#'
#' @return Date vector floored by a specified unit.
floor_by <- function(d, unit, week_start = 1) {
  ret <- lubridate::floor_date(d, unit, week_start = week_start)
  if (unit != "hours") {
    ret <- as.Date(ret)
  }
  return(ret)
}

#' Add a new date column that is floored by
#' a unit. The new date col's name will be
#' the first two letters of the unit.
#'
#' @param df Data frame.
#' @param col Column name of Date or POSIXct type.
#' @param unit Units as in \code{lubridate::floor_date()}.
#'
#' @return Data frame with a new floored date column.
add_date_col <- function(df, col, unit) {
  col <- rlang::expr_text(rlang::get_expr(col))
  col_name <- substr(unit, 1, 2)
  ret <- dplyr::mutate(df, !!col_name := floor_by(df[[col]], unit))
  return(ret)
}
