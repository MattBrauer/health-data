library(tidyverse)

specs <- cols(
  Date = col_date(format = ""),
  miles = col_double(),
  mile_avg = col_character(),
  pulse0 = col_double(),
  pulse1 = col_double(),
  pulse2 = col_double(),
  pulse3 = col_double(),
  comment = col_character()
)

runs <- read_csv("data/runs.csv", col_types=specs) %>%
  separate(mile_avg, into=c("mins", "secs"), sep=":") %>%
  mutate(mile_avg = as.numeric(mins) + as.numeric(secs)/60) %>%
  mutate(delta1 = pulse0 - pulse1,
         delta2 = pulse0 - pulse2,
         delta3 = pulse0 - pulse3
  )

runs %>% ggplot(aes(x=comment, y=mile_avg)) + geom_point()
runs %>% ggplot(aes(x=Date, y=mile_avg, color=comment)) + geom_point()
runs %>% ggplot(aes(x=mile_avg, y=pulse0, color=miles)) + geom_point()
runs %>% ggplot(aes(x=mile_avg, y=delta1, color=miles)) + geom_point()
runs %>% ggplot(aes(x=pulse0, y=delta1, color=miles)) + geom_point()
runs %>% ggplot(aes(x=Date, y=delta1, color=miles)) + geom_point()

