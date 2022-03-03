## orderly::orderly_develop_start()
unzip("promed_everything.zip")
infiles <- list.files(path = ".", pattern = "*.rds")
infiles <- infiles[infiles != "promed_importations.rds"]
infiles <- infiles[infiles != "promed_deaths.rds"]
names(infiles) <- gsub(
  x = infiles, pattern = "promed_", replacement = ""
) %>% gsub(pattern = ".rds", replacement = "")

promed <- map(infiles, readRDS)

cols_to_keep <- c(
  "id_number", "date_posted",
  "number_of_new_cases", "cumulative_cases", "number_of_suspected_cases",
  "post_source"
)

## Sometimes column names are different between sheets
promed[["china"]][["number_of_new_cases"]] <-
  promed[["china"]][["new_confirmed_cases"]]

promed[["china"]][["cumulative_cases"]] <-
  promed[["china"]][["cumulative_confirmed_cases"]]

promed[["china"]][["number_of_suspected_cases"]] <-
  promed[["china"]][["new_suspected_cases"]]


## Exclude UAE as the number of new cases is
## noted as Unknown.
promed_cases <- imap_dfr(
  promed, function(x, country) {
    message(country)
    if (country == "uae") {
      return(NULL)
    } else {
      x$id_number <- as.integer(x$id_number)
      x[, cols_to_keep]
    }
  }, .id = "country"
)

## First Imperial report published on 17th Jan
x <- promed_cases[promed_cases$date_posted < as.Date("2020-01-17"), ]
