library(tidyverse)

mortality <- read.csv("global_mortality.csv", stringsAsFactors = FALSE)
colnames(mortality) <- trimws(gsub(x = colnames(mortality),
                                   pattern = "(\\.)+",
                                   replacement = " "))

SDI_regions <- c('Low SDI', 'Low-middle SDI', 'Middle SDI', 'High-middle SDI',
                 'High SDI')

causes <- c('Cardiovascular diseases','Cancers', 'Respiratory diseases','Diabetes',
            'HIV AIDS', 'Neonatal deaths', 'Maternal deaths','Suicide')

mortality <- filter(mortality, country %in% SDI_regions) %>%
  select(country, year, one_of(causes)) %>%
  gather(key = 'cause', value = 'rate', 3:10) %>%
  mutate(country = factor(country, levels = SDI_regions, 
                          labels = c('Low','Low-middle','Middle',
                                     'High-middle','High')),
         cause = factor(cause, levels = causes))

mortality %>% ggplot() +
  geom_path(aes(x = year, y = rate, group = country, col = country),
            size = 1.2) +
  facet_wrap(~cause, nrow = 2) +
  scale_y_continuous(expand = c(0.001,0)) +
  scale_x_continuous(expand = c(.04, .04),
                     breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  labs(title = "Share of deaths by eight common causes",
       subtitle = "Comparison by Socio-Demographic Index (SDI) from year 1990 to 2016",
       y = 'Mortality rate (%)',
       x = 'Year',
       caption = 'Source: 1) ourworldindata.org 2) github.com/rfordatascience/tidytuesday',
       col = "Countries by SDI") +
  theme(plot.background = element_rect(fill = 'gray90'),
        legend.background = element_rect(fill = 'gray90'),
        legend.key = element_rect(fill = 'gray90', color = 'gray90'),
        strip.background = element_rect(fill = 'gray90'),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.major.y = element_line(color = 'azure4'),
        panel.grid.major.x = element_line(color = 'azure4'),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(2.3)),
        plot.subtitle = element_text(size = rel(1.8)),
        axis.title = element_text(size = rel(1.6)),
        axis.text = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.9)),
        legend.text = element_text(size = rel(1.5)),
        plot.caption = element_text(size = rel(1.3)),
        strip.text = element_text(size = rel(1.5)),
        legend.position = 'bottom'
        )

