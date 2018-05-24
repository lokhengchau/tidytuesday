library(tidyverse)
library(ggridges)
library(scales)

fb_salary <- read.csv('tidy_tuesday_week2.csv')

fb_salary <- gather(fb_salary, key = 'position', value = 'salary', 2:11) %>%
  filter(year %in% c(2011, 2018))

fb_salary <- fb_salary[complete.cases(fb_salary),]
fb_salary$position <- as.factor(fb_salary$position)
fb_salary$year <- as.factor(fb_salary$year)


fb_salary %>% ggplot(aes(x = salary,
                         y = fct_reorder(position, salary, median, .desc = TRUE),
                         fill = as.factor(year),
                         color = as.factor(year),
                         point_color = as.factor(year))) +
  geom_density_ridges(alpha = .3, scale = 1, rel_min_height = .01, from = 0,
                      jittered_points = TRUE, point_shape = '|',
                      point_size = 3,
                      position = position_points_jitter(height = 0)) +
  scale_x_continuous(expand = c(0, 0), labels = dollar) +
  scale_fill_manual(values = c('tomato2', 'dodgerblue2')) +
  scale_color_manual(values = c('tomato2', 'dodgerblue2'), guide = 'none') +
  scale_discrete_manual('point_color', values = c('tomato2', 'dodgerblue2'),
                        guide = 'none') +
  guides(fill = guide_legend(override.aes = list(
    fill = c('tomato2', 'dodgerblue2'),
    color = NA, point_color = NA))) +
  labs(title = 'Distribution of NFL Player Salaries in 2011 and 2018',
       caption = 'Source: 1) Spotrac.com 2) github.com/rfordatascience/tidytuesday',
       x = 'Salary (USD)',
       y = 'Position',
       fill = 'Year') +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_line(colour = 'grey85'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = rel(1.8)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.2)),
        plot.caption = element_text(size = rel(1.1)))


 