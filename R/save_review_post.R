

make_slug <- function(
    title,
    max_words = 8,
    max_chars = 80
) {

  slug <- title %>%

    stringr::str_to_lower() %>%

    stringi::stri_trans_general(
      "Latin-ASCII"
    ) %>%

    stringr::str_replace_all(
      "[^[:alnum:]]+",
      " "
    ) %>%

    stringr::str_squish()

  slug <- strsplit(slug, " ")[[1]]

  slug <- slug[
    seq_len(
      min(length(slug), max_words)
    )
  ]

  slug <- paste(slug, collapse = "-")

  slug <- substr(
    slug,
    1,
    max_chars
  )

  slug <- stringr::str_replace(
    slug,
    "-$",
    ""
  )

  slug
}


save_review_post <- function(
    markdown_text,
    title,
    date = Sys.Date(),
    output_dir = "content/post"
) {

  slug <- make_slug(title, max_words = 6)

  filename <- paste0(
    format(as.Date(date), "%Y-%m-%d"),
    "-",
    slug,
    ".md"
  )

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  output_file <- file.path(
    output_dir,
    filename
  )

  writeLines(
    markdown_text,
    output_file,
    useBytes = TRUE
  )

  message(
    "Saved review post: ",
    output_file
  )

  invisible(output_file)
}
