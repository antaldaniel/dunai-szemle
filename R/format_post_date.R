#' Normalise review and publication dates
#'
#' Converts heterogeneous date representations used in editorial
#' intake forms into normalised ISO-8601 publication dates.
#'
#' @details
#' The function standardises common date representations used in
#' curator and editorial workflows.
#'
#' Supported input formats include:
#'
#' - ISO dates (`YYYY-MM-DD`);
#' - Hungarian-style dates (`DD.MM.YYYY`);
#' - `Date` objects;
#' - `POSIXct` timestamps.
#'
#' The function returns dates formatted as:
#'
#' `YYYY-MM-DD`
#'
#' which is compatible with:
#'
#' - Hugo front matter;
#' - RSS feeds;
#' - metadata exports;
#' - provenance-aware publication workflows.
#'
#' Invalid or missing dates return `NA_character_`.
#'
#' @param x A date-like object or character scalar.
#'
#' @return
#' A character scalar containing a normalised ISO-8601 date
#' (`YYYY-MM-DD`) or `NA_character_`.
#'
#' @examples
#' format_post_date("17.04.2026")
#'
#' format_post_date("2026-05-17")
#'
#' format_post_date(
#'   as.POSIXct("2026-05-17 14:32:00")
#' )
#'
#' @export

format_post_date <- function(x) {

  # ------------------------------------------------------------
  # Missing values
  # ------------------------------------------------------------

  if (
    length(x) == 0 ||
    is.na(x) ||
    x == ""
  ) {

    return(NA_character_)

  }

  # ------------------------------------------------------------
  # POSIXct / Date
  # ------------------------------------------------------------

  if (
    inherits(x, "POSIXct") ||
    inherits(x, "Date")
  ) {

    return(
      format(as.Date(x), "%Y-%m-%d")
    )

  }

  x <- as.character(x)

  # ------------------------------------------------------------
  # ISO format already present
  # ------------------------------------------------------------

  if (
    grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  ) {

    return(x)

  }

  # ------------------------------------------------------------
  # Hungarian date format
  # ------------------------------------------------------------

  if (
    grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", x)
  ) {

    d <- as.Date(
      x,
      format = "%d.%m.%Y"
    )

    return(
      format(d, "%Y-%m-%d")
    )

  }

  # ------------------------------------------------------------
  # Unsupported format
  # ------------------------------------------------------------

  NA_character_
}
