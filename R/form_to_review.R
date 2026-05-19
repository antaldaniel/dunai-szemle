#' Convert a curated form response into a review post
#'
#' Creates a Hugo-compatible Markdown review post from a structured
#' curation form response.
#'
#' @details
#' The function converts one row of a curator submission table into a
#' Markdown document with YAML front matter aligned to the
#' *Dunai szemle* review workflow.
#'
#' The generated post preserves:
#'
#' - original publication provenance;
#' - curator attribution;
#' - editorial summaries and commentary;
#' - categorical and tag-based classification.
#'
#' The generated YAML front matter includes:
#'
#' - publication date of the review;
#' - original publication date;
#' - curator identity;
#' - source publication metadata;
#' - editorial classification fields.
#'
#' The function intentionally separates:
#'
#' - the reviewed original publication;
#' - the editorial review publication;
#' - curator attribution;
#' - optional editorial intervention.
#'
#' This supports lightweight provenance-aware editorial workflows
#' suitable for static publishing systems such as Hugo.
#'
#' @param responses A `data.frame` containing structured review
#'   submissions imported from a form workflow.
#'
#' @param row Integer. Row number of the submission to convert.
#'
#' @return
#' A character scalar containing a complete Markdown review post
#' with YAML front matter.
#'
#' @examples
#' responses <- data.frame(
#'   original_title = "Example article",
#'   original_author = "Joe Doe",
#'   use_same_title = "Igen",
#'   original_url = "https://example.org/article",
#'   original_date = "2026-05-17",
#'   original_source = "Example News",
#'   curator_name = "Jane Doe",
#'   content_type = "Dunai szemle: hosszabb cikk",
#'   short_summary = "Short summary.",
#'   lead_summary = "Lead summary.",
#'   editor_summary = NA,
#'   editor_recommendation = NA,
#'   editor_critique = NA,
#'   category = "Kultúra",
#'   tag_1 = "film",
#'   tag_2 = NA,
#'   tag_3 = NA,
#'   stringsAsFactors = FALSE
#' )
#'
#' cat(form_to_review(responses, 1))
#'
#' @seealso [format_post_date()], [na_empty()]
#'
#' @importFrom dplyr slice
#' @importFrom glue glue
#'
#' @export

form_to_review <- function(responses, row) {

  x <- responses %>%
    slice(row)

  original_date <- format_post_date(x$original_date)

  category_block <- if (
    !is.na(x$category) &&
    x$category != ""
  ) {

    paste0("  - ", x$category)

  } else {

    "  - TODO"

  }

  tag_cols <- intersect(
    c("tag_1", "tag_2", "tag_3"),
    names(x)
  )

  tags <- unlist(
    x[tag_cols],
    use.names = FALSE
  )

  tag_block <- if (length(tags) > 0) {

    paste0(
      "  - ",
      tags,
      collapse = "\n"
    )

  } else {

    "  - TODO"

  }

  subtitle_field <- if (
    !is.na(x$use_same_title) &&
    x$use_same_title == "Igen"
  ) {

    glue::glue(
      'subtitle: "Eredeti forrás: {x$original_source}"'
    )

  } else {

    glue::glue(
      'subtitle: "{x$original_source}: {x$original_title}"'
    )

  }

  original_link <- glue::glue(
    'Az eredeti szöveg itt olvasható: {x$original_source} — [{x$original_title}]({x$original_url}).'
  )

  original_author_text <- if (
    !is.null(x$original_author) &&
    !is.na(x$original_author)
  ) {
    glue::glue(
      'original_author: "{x$original_author}"  '
    )
  } else {
    "original_author:  "
  }

  body_sections <- c(
    na_empty(x$lead_summary),

    na_empty(x$editor_summary),

    na_empty(x$editor_recommendation),

    na_empty(x$editor_critique),

    original_link

  )

  body_sections <- body_sections[
    body_sections != ""
  ]

  body_text <- paste(
    body_sections,
    collapse = "\n\n"
  )

  md_text <- glue::glue(
    '---
title: "{x$original_title}"

{subtitle_field}

description: >
  {na_empty(x$short_summary)}

date: {format(Sys.Date(), "%Y-%m-%d")}

original_date: {original_date}

{original_author_text}

curator: "{x$curator_name}"

content_type: "review"

editorial_scope: "{x$content_type}"

original_source: "{x$original_source}"

original_url: "{x$original_url}"

categories:
{category_block}

tags:
{tag_block}
---

{body_text}
'
  )

  md_text
}
