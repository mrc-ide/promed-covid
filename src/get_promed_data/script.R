## orderly::orderly_develop_start()
infile <- "promed-covid-data.xlsx"
sheets <- excel_sheets(infile)
names(sheets) <- to_lower_camel_case(sheets)
iwalk(
  sheets, function(sheet, name) {
    x <- read_excel(infile, sheet = sheet)
    x <- clean_names(x)
    saveRDS(x, file = glue("promed_{name}.rds"))
  }
)


