## orderly::orderly_develop_start(use_draft = "newer")
## First Imperial report published on 17th Jan
promed_cases <- readRDS("promed_cases.rds")
cases <- promed_cases[promed_cases$date_posted < as.Date("2020-01-17"), ]
## 41 cases in icl report, here we have 42.
unzip("promed_everything.zip")
deaths <- readRDS("promed_deaths.rds")
deaths <- deaths[deaths$date_posted < as.Date("2020-01-17"), ]
importations <- readRDS("promed_importations.rds")
all_weeks <- c("2019-W52", glue("2020-W{1:52}"))
importations$week_posted <- factor(importations$week_posted, levels = all_weeks, ordered = TRUE)
importations$from_admin_0 <- to_title_case(importations$from_admin_0)

x <- count(importations, week_posted, from_admin_0, to_admin_0) %>%
  arrange(week_posted, n)

x$from_admin_0 <- forcats::fct_reorder2(x$from_admin_0, x$week_posted, x$n)
x$to_admin_0 <- forcats::fct_reorder2(x$to_admin_0, x$week_posted, x$n)

p <- ggplot(x) +
  geom_curve(
    aes(x = week_posted, xend = week_posted, y = from_admin_0, yend = to_admin_0, size = n),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = 0.2
  ) +
  ggtitle("Number of reported COVID-19 importations",
          "31st December 2019 to 13th Feb 2020") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.title = element_blank())


importations <- importations[importations$date_posted < as.Date("2020-01-17"), ]
## Report 1 - 2 deaths in Wuhan; ProMED - 2 deaths
## Report 1 - 2 cases in Thailand, 1 case in Japan
## ProMED - 35??
