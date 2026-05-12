install.packages("googlesheets4")


library(googlesheets4)
library(dplyr)

sheet_url <- "https://docs.google.com/spreadsheets/d/1G_qp0tsCqG4ACJyTgsud9J9YnzXbCiToAfmcdNFAvF8/edit?gid=1765804905#gid=1765804905"

# first time only: authenticate
gs4_auth()

# read the form results

responses <- read_sheet(sheet_url)

glimpse(responses)
responses <- read_sheet(
  sheet_url,
  sheet = "Form Responses 1"
)

responses %>%
  mutate_all(as.character) %>%
  tidyr::pivot_longer( cols = everything())

responses <- responses %>%
  rename(
    timestamp              = `Timestamp`,
    original_title         = `Az ajánlott mű (cikk, jelentés, javaslat, podcast epizód) eredeti cime`,
    use_same_title         = `Azonos címen vegyük fel`,
    original_url          =  `Eredeti megjelenés URL`,
    original_date          = `Eredeti megjelenés ideje`,
    original_source        = `Eredeti megjelenés helye`,
    curator_name           = `Ajánló kurátor neve`,
    content_type           = `Tartalom jellege`,
    short_summary          = `Rövid összefoglaló (max 200 karakter)`,
    lead_summary           = `Rövid összefoglaló a cikk elejére (max 400 karakter)`,
    editor_summary         = `Opcionális: szerkesztői összefoglaló (max 600 karakter)`,
    editor_recommendation  = `Opcionális: szerkesztői ajánlás (max 600 karakter)`,
    editor_critique        = `Opcionális: szerkesztői kritika (max 600 karakter)`,
    category               = `Rovat`,
    tag_1                  = `hashtag1`,
    tag_2                  = `hashtag1`,
    tag_3                  = `hashtag1`,
  )

responses %>%
  mutate_all(as.character) %>%
  tidyr::pivot_longer( cols = everything())

responses$original_date

library(dplyr)
library(glue)
library(readr)
library(stringr)
library(purrr)

na_empty <- function(x) {
  ifelse(is.na(x) | x == "", "", x)
}

make_slug <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:]]+", "-") %>%
    str_replace_all("(^-|-$)", "")
}

row = 1

library(dplyr)
library(glue)
library(stringr)

na_empty <- function(x) {
  ifelse(is.na(x) | x == "", "", x)
}

format_post_date <- function(x) {

  if(inherits(x, "Date")) return(x)

  if (is.na(x) || x == "") return(NA_character_)

  x <- as.character(x)

  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    return(gsub("-", ".", x))
  }

  if (grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", x)) {
    d <- as.Date(x, format = "%d.%m.%Y")
    return(format(d, "%Y.%m.%d"))
  }

  NA_character_
}



create_post_form <- function(responses, row) {

  x <- responses %>% slice(row)

  short_summary <-

  original_date <- format_post_date(x$original_date)

  editorial_contribution <-
    !is.na(x$editor_summary) ||
    !is.na(x$editor_recommendation) ||
    !is.na(x$editor_critique)

  if (editorial_contribution) {
    author_field  <- glue::glue('author: "{x$curator_name}"')
    curator_field <- ""
  } else {
    author_field  <- ""
    curator_field <- glue::glue('curator: "{x$curator_name}"')
  }

  category_block <- if (!is.na(x$category) && x$category != "") {
    paste0("  - ", x$category)
  } else {
    "  - TODO"
  }

  tags <- c(
    x$tag_1,
    x$tag_2,
    x$tag_3
  )

  tags <- tags[!is.na(tags) & tags != ""]

  tag_block <- if (length(tags) > 0) {
    paste0("  - ", unique(tags), collapse = "\n")
  }

  original_link <- glue::glue(
    'Az eredeti szöveg itt olvasható: {x$original_source}, [{x$original_title}]({x$original_url}).'
  )

  subtitle_field <- if (
    !is.na(x$use_same_title) &&
    x$use_same_title == "Igen"
  ) {
    glue::glue('subtitle: "Eredeti forrás: {x$original_source}"')
  } else {
    glue::glue('subtitle: "{x$original_source}: {x$original_title}"')
  }

  md_text <- glue::glue(
    '---
title: "{x$original_title}"

{subtitle_field}

description: >
  {x$short_summary}

date: {original_date}

{author_field}
{curator_field}

original_source: "{x$original_source}"

content_type: "{x$content_type}"

categories:
{category_block}

tags:
{tag_block}
---

{na_empty(x$lead_summary)}

{na_empty(x$editor_summary)}

{na_empty(x$editor_recommendation)}

{na_empty(x$editor_critique)}

{original_link}
'
  )

  md_text
}


cat(create_post_form(responses, 1))

