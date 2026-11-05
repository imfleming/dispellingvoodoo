library(rvest)
library(chromote)
library(stringr)
library(purrr)
library(httr2)
library(tibble)

pages <- tribble(
  ~slug,                      ~qmd_file,                      ~title,
  "",                         "index.qmd",                    "Dispelling Voodoo",
  "save-v3-1417",             "save-v3-1417.qmd",             "SAVE v3 (2014–2017)",
  "sav3",                     "sav3.qmd",                     "SAV3 Methodology",
  "save-v2-1617",             "save-v2-1617.qmd",             "SAVE v2 (2016–2017)",
  "save-v1-1516",             "save-v1-1516.qmd",             "SAVE v1 (2015–2016)",
  "save-jr",                  "save-jr.qmd",                  "SAVE Jr.",
  "team",                     "team.qmd",                     "TEAM",
  "mate",                     "mate.qmd",                     "MATE",
  "nhl-draft-2019",           "nhl-draft-2019.qmd",           "2019 NHL Draft Predictions",
  "data",                     "data.qmd",                     "NHL Data",
  "juniors-data",             "juniors-data.qmd",             "Juniors Data",
  "goaltender-progressions",  "goaltender-progressions.qmd",  "Goaltender Progressions",
  "articles",                 "articles.qmd",                 "Articles",
  "about",                    "about.qmd",                    "About",
  "contact",                  "contact.qmd",                  "Contact"
)

dir.create("assets/img", recursive = TRUE, showWarnings = FALSE)
dir.create("_raw_html", showWarnings = FALSE)
dir.create("_unpacked_embeds", showWarnings = FALSE)

scrape_to_qmd <- function(slug, qmd_file, title) {
  url <- if (slug == "") "https://www.dispellingvoodoo.com/" else paste0("https://www.dispellingvoodoo.com/", slug)
  file_stub <- if (slug == "") "home" else slug
  message("Scraping: ", url, " -> ", qmd_file)
  
  sess <- read_html_live(url)
  Sys.sleep(5)
  
  # Scroll via sess$session$Runtime to trigger lazy-loaded Wix components
  try(sess$session$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight);"), silent = TRUE)
  Sys.sleep(2)
  
  # Target #PAGES_CONTAINER specifically so inner iframe <main> tags never shadow it
  main_node <- sess |> html_element("#PAGES_CONTAINER")
  if (length(main_node) == 0 || is.na(main_node)) {
    main_node <- sess |> html_element("#SITE_PAGES")
  }
  
  raw_html <- as.character(main_node)
  writeLines(raw_html, file.path("_raw_html", paste0(file_stub, ".html")))
  
  # Extract and download master images from #PAGES_CONTAINER only
  img_urls <- main_node |> 
    html_elements("img") |> 
    html_attr("src") |> 
    na.omit() |> 
    unique()
  
  clean_imgs <- str_extract(img_urls, "https://static\\.wixstatic\\.com/media/[^/\"'?]+") |> 
    na.omit() |> 
    unique()
  
  walk(clean_imgs, function(img_url) {
    fname <- basename(img_url)
    dest <- file.path("assets/img", fname)
    if (!file.exists(dest)) {
      try(req_perform(request(img_url), path = dest), silent = TRUE)
    }
  })
  
  # Extract only content iframes inside #PAGES_CONTAINER (exclude analytics & blank workers)
  iframes <- main_node |> 
    html_elements("iframe") |> 
    html_attr("src") |> 
    na.omit() |> 
    unique()
  
  iframes <- iframes[!str_detect(iframes, "visitor-analytics\\.io|^about:blank$")]
  
  # If an iframe is a Wix .filesusr.com HTML sandbox, fetch and save its inner source
  embed_blocks <- map_chr(iframes, function(src) {
    if (str_detect(src, "filesusr\\.com/html/")) {
      inner_html <- tryCatch(
        req_perform(request(src)) |> resp_body_string(),
        error = function(e) ""
      )
      if (nzchar(inner_html)) {
        writeLines(inner_html, file.path("_unpacked_embeds", paste0(file_stub, "_", basename(src))))
      }
    }
    paste0(
      "```{=html}\n",
      "<div class=\"shiny-embed-container\">\n",
      "  <iframe src=\"", src, "\" allowfullscreen></iframe>\n",
      "</div>\n",
      "```"
    )
  })
  
  # Convert HTML body to Markdown via Pandoc
  tmp_html <- tempfile(fileext = ".html")
  tmp_md   <- tempfile(fileext = ".md")
  writeLines(raw_html, tmp_html)
  
  rmarkdown::pandoc_convert(
    input = tmp_html,
    to = "markdown_strict+pipe_tables",
    output = tmp_md
  )
  
  md_text <- paste(readLines(tmp_md, warn = FALSE), collapse = "\n")
  
  # Replace raw Wix <img ...> blocks with clean centered Quarto Markdown images
  for (img_url in clean_imgs) {
    fname <- basename(img_url)
    md_text <- str_replace_all(
      md_text,
      paste0("<img[^>]*", fixed(fname), "[^>]*>"),
      paste0("\n![](assets/img/", fname, "){.wix-img fig-align=\"center\"}\n")
    )
  }
  
  # Fallback rewrite for any remaining wixstatic URLs
  md_text <- str_replace_all(
    md_text,
    "https://static\\.wixstatic\\.com/media/([^/\"'?)]+)[^)\"']*",
    "assets/img/\\1"
  )
  
  yaml_header <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "---\n\n",
    "::: {.wix-container}\n\n"
  )
  
  iframe_section <- if (length(embed_blocks) > 0) paste0("\n\n", paste(embed_blocks, collapse = "\n\n")) else ""
  
  full_qmd <- paste0(yaml_header, md_text, "\n\n:::", iframe_section, "\n")
  writeLines(full_qmd, qmd_file)
  
  sess$session$close()
}

pwalk(pages, scrape_to_qmd)
message("Scrape and embed unpacking complete.")