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
    saveRDS(x, glue("promed_{name}.rds"))
  }
)
