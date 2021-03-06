## orderly::orderly_develop_start()
## TODO. Edit this to fetch it directly from gdrive
infile <- "promed-covid-data.xlsx"
sheets <- excel_sheets(infile)
names(sheets) <- to_lower_camel_case(sheets)
promed <- iwalk(
  sheets, function(sheet, name) {
    x <- read_xlsx(infile, sheet = sheet)
    x <- clean_names(x)
    x$date_posted <- as.Date(x$date_posted)
    x$id_number <- str_trim(x$id_number)
    x$id_number_2 <- str_trim(x$id_number)
    x$post_source <- str_trim(x$post_source)
    x$week_posted <- glue("{year(x$date_posted)}-W{week(x$date_posted)}")
    outfile <- glue("promed_{name}.rds")
    saveRDS(x, outfile)
    if (! file.exists("promed_everything.zip")) {
      message("Adding first file ", outfile)
      zip("promed_everything.zip", outfile)
    } else {
      message("Appending ", outfile)
      zip_append("promed_everything.zip", outfile)
    }

  }
)

