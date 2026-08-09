site_dir <- "docs"
site_url <- "https://phonics.jameshoward.us"

html_files <- list.files(
  site_dir,
  pattern = "[.]html$",
  recursive = TRUE,
  full.names = TRUE
)

for (html_file in html_files) {
  document <- xml2::read_html(html_file)
  head <- xml2::xml_find_first(document, "//head")

  # Keep pkgdown's canonical targets on redirect stubs and do not canonicalize
  # the intentionally non-indexable error page.
  existing <- xml2::xml_find_first(head, "link[@rel='canonical']")
  if (!inherits(existing, "xml_missing") || basename(html_file) == "404.html") {
    next
  }

  relative_path <- substring(
    normalizePath(html_file, winslash = "/", mustWork = TRUE),
    nchar(normalizePath(site_dir, winslash = "/", mustWork = TRUE)) + 2L
  )
  canonical_url <- if (identical(relative_path, "index.html")) {
    paste0(site_url, "/")
  } else {
    paste0(site_url, "/", relative_path)
  }

  xml2::xml_add_child(head, "link", rel = "canonical", href = canonical_url)
  xml2::xml_add_child(head, "meta", property = "og:url", content = canonical_url)
  xml2::write_html(document, html_file)
}
