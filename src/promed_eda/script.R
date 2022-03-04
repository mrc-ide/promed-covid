## orderly::orderly_develop_start()
unzip("promed_everything.zip")
infiles <- list.files(path = ".", pattern = "*.rds")

names(infiles) <- gsub(
  x = infiles, pattern = "promed_", replacement = ""
) %>% gsub(pattern = ".rds", replacement = "")

promed <- map(infiles, readRDS)
promed <- map(promed, function(x) {
    x$id_number <- stringr::str_trim(x$id_number)
    x$id_number_2 <- stringr::str_trim(x$id_number)
    x$post_source <- stringr::str_trim(x$post_source)
    x
})

x <- promed[["deaths"]]
out1 <- x[ ,c("id_number", "date_posted", "country", "post_source")]
out2 <- x[ ,c("id_number_2", "date_posted_2", "country", "post_source")]
out2 <- rename(
  out2, id_number = id_number_2,
  date_posted = date_posted_2
)
## Assume that date will not be NA if id is present
out2 <- out2[!is.na(out2$id_number), ]
out <- rbind(out1, out2)

x <- promed[["importations"]]
out1 <- x[ ,c("id_number", "date_posted", "from_admin_0", "post_source")]
out2 <- x[ ,c("id_number_2", "date_posted_2", "from_admin_0", "post_source")]
out1 <- rename(
  out1, country = from_admin_0
)

out2 <- rename(
  out2, id_number = id_number_2,
  date_posted = date_posted_2,
  country = from_admin_0
)
## Assume that date will not be NA if id is present
out2 <- out2[!is.na(out2$id_number), ]
out <- rbind(out, out1, out2)

promed <- promed[! names(promed) %in% c("deaths", "importations")]

nposts <- imap_dfr(
  promed, function(x, cntry) {
    out1 <- x[ ,c("id_number", "date_posted", "post_source")]
    out2 <- x[ ,c("id_number_2", "date_posted_2", "post_source")]
    out2 <- rename(
      out2, id_number = id_number_2,
      date_posted = date_posted_2
    )
    ## Assume that date will not be NA if id is present
    out2 <- out2[!is.na(out2$id_number), ]
    rbind(out1, out2)
  }, .id = "country")


out <- out[, colnames(nposts)]
nposts <- rbind(nposts, out)
## Fix NA dates

for (row in 1:nrow(nposts)) {
  this_id <- nposts$id_number[row]
  this_date <- nposts$date_posted[row]
  if (is.na(this_date)) {
    ## Then look for another entru with this id_number
    same_id <- nposts[nposts$id_number == this_id, ]
    ## If more than one entry, all non-NA dates should be the same
    dates <- unique(same_id$date_posted[! is.na(same_id$date_posted)])
    nunique <- length(dates)
    ## Flag if this is so.
    if (nunique > 1) {
      message("Multiple dates found for id ", this_id)
      ## For now assign any one to the ones that are missing.
      idx <- which((nposts$id_number == this_id) &
                   is.na(nposts$date_posted))
      nposts$date_posted[idx] <- dates[1]
    } else {
      nposts$date_posted[nposts$id_number == this_id] <- dates
    }
  }
}

nposts$week_posted <- glue("{year(nposts$date_posted)}-W{week(nposts$date_posted)}")
all_weeks <- c("2019-W52", glue("2020-W{1:52}"))
nposts$week_posted <- factor(nposts$week_posted, levels = all_weeks, ordered = TRUE)
nposts$country <- str_trim(nposts$country)
nposts$country <- to_title_case(nposts$country)

x <- count(nposts, country)
x <- arrange(x, n)

x$country <- factor(
  x$country, levels = x$country,
  ordered = TRUE
)

p <- ggplot(x) +
  geom_col(aes(n, country), fill = "black", alpha = 0.7) +
  theme_minimal() +
  xlab("Number of ProMED posts") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.title = element_blank())

ggsave("promed_posts_country.png", p)

date_range <- group_by(nposts, country) %>%
  summarise(min_date = min(date_posted),
            max_date= max(date_posted)) %>%
  ungroup()

date_range <- arrange(date_range, min_date)
date_range$country <- factor(
  date_range$country, levels = date_range$country,
  ordered = TRUE
)


p <- ggplot() +
  geom_segment(
    data = date_range,
    aes(x = min_date, xend = max_date, y = country, yend = country), size = 2
  ) +
  geom_point(
    data = date_range[date_range$min_date == date_range$max_date, ],
    aes(min_date, country), size = 2
  ) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.title = element_blank())


ggsave("promed_posts_country_dates.png", p)


## Recoding sources
news_media <- c(
  "Reuters",
  "South China Morning Post",
  "Herald Sun",
  "Sky News",
  "Finance Sina",
  "Globe and Mail/Reuters",
  "Stat News",
  "China News",
  "Local News",
  "Local News Outlet Photo via Global Times.",
  "US News & World Report, Reuters report",
  "Local News Outlet via National Business Daily",
  "News Kotimaa",
  "Rappler/Agence France-Presse",
  "Reuters",
  "Yahoo Sports",
  "Asia One",
  "New Straits Times, Reuters report",
  "India Today",
  "Outbreak News Today",
  "Daily Mail",
  "CNBC",
  "ABS CBN news / Kyodo News ",
  "Kyodo News ",
  "JiJi News",
  "Straits Times",
  "Malay Mail",
  "Local News Outlet via Ary News",
  "The Express Tribune",
  "Bloomberg",
  "New Strait Times",
  "Strait Times",
  "Local News Outlet via Seoul Broadcasting System.",
  "Channale News Asia",
  "Yahoo News / Reuters",
  "Chiang Mai City Life",
  "The Peninsula",
  "The Guardian",
  "The Washington Post",
  "Local News Outlet via Star Tribune",
  "The New York Times, Reuters Report",
  "Viet Nam News",
  "China.org",
  "Prince George Citizen",
  "The Straits Times",
  "ABS CBN news / Kyodo News",
  "Kyodo News",
  "DevDiscourse, Reuters report",
  "The Hill",
  ## Source is news media even if dissemniated via Twitter
  "Local News Outlet via Public Message Board.",
  "Global Times, News Outlet Photo via Twitter.",
  "Global Times, News Outlet via Twitter.",
  "El Sol de Mexico, News Outlet via Twitter.",
  "MBC News at 5, News Outlet via Twitter",
  "Local News outlet via Sina Weibo",
  "Local News Outlet via Sina Weibo.",
  "News outlet photo via Sina Weibo")

social_media <- c("Twitter", "Local News Outlet via Twitter.", "Twitter Feed",
                  "Local Source Photo via Twitter.",
                  "Reporter via Twitter.", "David Ingles, Market Commentary Via Twitter",
                  "Local Source via Twitter",
                  "Reporter photo via Twitter",
                  "Local Source via twitter",
                  "Local Source via Reddit",
                  "Government photo via Twitter",
                  "Sino Weibo",
                  "Sino Weibei",
                  "Local source via Sina Weibo",
                  "Government via Sina Weibo",
                  "govern via Sina Weibo.",
                  "Local Source via Sina Weibo.",
                  "Local Source via Sina Weibo",
                  " Local Source via Sina Weibo.")


moh <- c(
  "Ontario MOH",
  "Wuhan Public Health Commission Press Release",
  "Wuhan Public Health Committee",
  "Wuhan Public Health Commission",
  "Wuhan Municipal Health Commission",
  "Wuhan Public Health Commission, National Health Committee",
  "Government of China",
  "National Health Commission, Health Emergency Office - China",
  "National Health Commission",
  "National Health Committee",
  "Government via China State Council.",
  "Chinese National Health Committee",
  "Office of Health Emergency",
  "Center for Health Protection, Hong Kong",
  "Hong Kong Centre for Health Protection",
  "China National Health Commission",
  "Ministry of Health, Singapore",
  "CDC Taiwan Press Release",
  "Thai Ministry of Public Health",
  "Wisconsin Department of Health Services",
  "Chinese National Health Commission"
)

promed <- c(
  "Reported by ProMED Mod",
  "Reported by ProMED Follower",
  "Submitted by: Ryan McGinnis",
  "Reported by ProMED Rapporteur",
  "ProMED, Reported by ProMED follower",
  "ProMED mail from HealthMap alerts",
  "ProMED",
  "Dr. Sher Bahadur Pun"
)

healthmap <- c(
  "ProMED-mail from HealthMap Alerts",
  "Healthmap"
)

john_hopkins <- c(
  "Johns Hopkins CSSE",
  "John Hopkins CSSE",
  "John's Hopkins CSSE"
)

john_hopkins_media <- c(
  "Johns Hopkins CSSE, The Hill",
  "Johns Hopkins CSSE, KETV news",
  "John's Hopkins CSSE, The Guardian",
  "Johns Hopkins CSSE, Strait Times",
  "Johns Hopkins CSSE, The Straits Times",
  "Johns Hopkins CSSE, Reuters",
  "John's Hopkins CSSE, the Telegraph"
)
who_plus_media <- c(
  "Kyodo News, WHO",
  "News Joins, WHO",
  "WHO, Thailand MoPH, Bangkok Post"
)



nposts$post_source_category <- case_when(
  nposts$post_source %in% news_media ~ "News Media",
  nposts$post_source %in% social_media ~ "Social Media",
  nposts$post_source %in% john_hopkins_media ~ "John Hopkins + News Media",
  nposts$post_source %in% john_hopkins ~ "John Hopkins",
  nposts$post_source %in% promed ~ "ProMED",
  nposts$post_source %in% healthmap ~ "HealthMap",
  nposts$post_source %in% moh ~ "Official/Ministeries of Health",
  TRUE ~ "Other"
)

saveRDS(nposts, "promed_eda.rds")
## Number of posts per week.
x <- count(nposts, week_posted)


p <- ggplot(x, aes(week_posted, n), fill = "black") +
  geom_col(alpha = 0.7) +
  ggtitle("Number of COVID-19 related ProMED Posts",
          "between 31st Dec 2019 and 13 Feb 2020") +
  xlab("Number of ProMED posts") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.position = "top",
        legend.title = element_blank())

ggsave("post_counts_raw.png", p)

x <- count(nposts, week_posted, post_source_category)

p <- ggplot(x, aes(week_posted, n, fill = post_source_category)) +
  geom_col(alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Source of ProMED posts") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.position = "top",
        legend.title = element_blank())

ggsave("post_sources_raw.png", p)

x <- tabyl(nposts, week_posted, post_source_category) %>%
  adorn_percentages()

x <- gather(x, post_source_category, perc, -week_posted)
x <- x[complete.cases(x), ]

p <- ggplot(x, aes(week_posted, perc, fill = post_source_category)) +
  geom_col(alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Breakdown of ProMED post sources per week") +
  ylab("Percentage") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0),
        legend.position = "top",
        legend.title = element_blank())

ggsave("post_sources_perc.png", p)

