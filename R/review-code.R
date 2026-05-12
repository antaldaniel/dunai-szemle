
reprextemplates::concat_text_files(source_dir = here::here(
  c("Hugo-Octopress", "layouts")
),
                                   extension = c("css", "html", "md"),
                                   recursive = T,
                                   output_file = "Hugo-Octopress.txt")


