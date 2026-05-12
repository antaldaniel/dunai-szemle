decode_html <- function(x) {
  vapply(
    x,
    FUN.VALUE = character(1),
    FUN = function(y) {

      if (is.na(y) || y == "") {
        return(NA_character_)
      }

      xml2::xml_text(
        xml2::read_html(
          paste0("<x>", y, "</x>")
        )
      )
    }
  )
}
library(tidyRSS)
library(dplyr)
library(lubridate)
library(stringr)

rss_source <- "https://vidakamilla.substack.com/feed"


import_rss <- function(original_source,
                       rss_source,
                       curator,
                       use_same_title = "Igen",
                       tag_1 = NA, tag_2 = NA, tag_3 = NA,
                       category = NA) {

  rss <- tidyfeed(rss_source)
  curation <- rss %>%
    transmute(
      imported_at = Sys.time(),

      original_source = original_source,

      original_title = decode_html(item_title),

      use_same_title = use_same_title,

      original_url = item_link,

      original_time = parse_date_time(
        item_pub_date,
        orders = c("a, d b Y H:M:S z", "Y-m-d H:M:S")
      ),

      author_name = NA_character_,

      curator_name = curator,

      content_type = "blogpost",

      short_summary = decode_html(item_description),

      lead_summary = NA_character_,

      editor_summary = NA_character_,

      editor_recommendation = NA_character_,

      editor_critique = NA_character_,

      tag_1  = tag_1,

      tag_2  = tag_2,

      tag_3 = tag_3,

      category = category,

      status = "new"
    )

  curation %>%
    mutate (original_date = lubridate::as_date(original_time))
}

vk <- import_rss(original_source="Vida Kamilla (Substack)",
                 use_same_title="igen",
           rss_source="https://vidakamilla.substack.com/feed",
           curator = "Antal Dániel",
           category = "2026",
           tag_1= "identitás")

cat(create_post_form(responses = vk, row = 1))

library(googlesheets4)

gs4_auth()

initial_sheet <- vk %>%
  tibble::rowid_to_column() %>%
  mutate ( published_on = "YYYY.MM.DD" ) %>%
  dplyr::relocate(published_on, .after = rowid)

str(initial_sheet)


sheet <- gs4_create(
  "Dunai Szemle Curation",
  sheets = list(
    submissions = initial_sheet
  )
)


sheet_url <- "https://docs.google.com/spreadsheets/d/15KJZ_UJGhg1IQv59WKaypxUe14yUSBE4cSDiCLvEAC4/"

existing <- read_sheet(
  sheet_url,
  sheet = "submissions"
)

max(existing$rowid)
schn <- import_rss(original_source="Schultz Nóra (Substack)",
                 use_same_title="igen",
                 rss_source="https://schultznora.substack.com/feed",
                 curator = "Antal Dániel",
                 category = "2026",
                 tag_1= "identitás")


new_items <- schn %>%
  tibble::rowid_to_column() %>%
  mutate ( published_on = "YYYY.MM.DD" ) %>%
  mutate ( selected = "nem") %>%
  dplyr::relocate(published_on, .after = rowid) %>%
  dplyr::relocate(selected, .after = published_on) %>%
  mutate(rowid = rowid + max(existing$rowid))


if (nrow(new_items) > 0) {

  sheet_append(
    ss = sheet_url,
    sheet = "submissions",
    data = new_items
  )
}
