library(tidyverse)
library(geofacet)
library(stringr)
library(scales)

setwd('c:/Users/LokHeng/Desktop/Rstuff/US_avg_tuition/')
df <- read.csv('us_avg_tuition.csv', stringsAsFactors = FALSE)

#Remove 2004-05
df <- select(df, 1, 3:13)

#Clean up column names
colnames(df) <- str_replace(colnames(df), 'X', '')
colnames(df) <- str_replace(colnames(df), '\\.', '-')

#Remove dollar sign and convert strings to numbers
for (i in 2:12){
  df[,i] <- str_replace(df[,i], '\\$', '')
  df[,i] <- str_replace(df[,i], '\\,', '')
  df[,i] <- as.integer(df[,i])
}


#Add national average tuition for each year
df[51,2:12] <- sapply(df[2:12], mean)
df[51,1] <- 'Average'

df <- df %>% gather(key = 'Year', value = 'Tuition', 2:12)

avg <- filter(df, State == 'Average') %>% rename(Average = Tuition) %>%
  select(-State)

df <- left_join(df, avg, by = 'Year') %>% filter(State != 'Average')

###plot with geofacet

ggplot(df) +
  geom_path(aes(x = Year, y = Tuition, group = 1, linetype = 'a'), size = 1.5) +
  geom_path(aes(x = Year, y = Average, group = 1, linetype = 'c'), size = 1.5) +
  geom_ribbon(aes(x = Year, ymin = Average,
                  ymax = ifelse(Tuition >= Average, Tuition, Average),
                  group = 1, fill = 'blue')) + 
  geom_ribbon(aes(x = Year, ymin = Tuition,
                  ymax = ifelse(Average >= Tuition, Average, Tuition),
                  group = 1, fill = 'red')) +
  facet_geo(~State, grid = 'us_state_grid3') +
  scale_x_discrete(breaks = c('2005-06', '2010-11', '2015-16'),
                   labels = c('05-06', '10-11', '15-16')) +
  scale_y_continuous(labels = dollar) +
  scale_linetype_discrete(labels = c('State','National')) +
  scale_fill_hue(labels = c('More expensive',
                            'Less expensive')) +
  labs(fill = 'Comparing to National Average',
       linetype = 'Average Tuition',
       y = 'Average Tuition',
       x = 'Year',
       title = 'Average College Tuition in the States',
       subtitle = 'From Year 2005 to 2016',
       caption = 'Source: 1) onlinembapage.com 2) github.com/rfordatascience/tidytuesday') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.background = element_rect(fill = "snow2"),
        legend.background = element_rect(fill = 'snow2'),
        legend.key = element_rect(fill = 'snow2', color = 'snow2'),
        panel.grid.major = element_line(colour = 'grey80'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "snow2"),
        panel.border = element_blank(),
        plot.title = element_text(size = rel(2)),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.3)))

        