rm(list=ls())

library(coronavirus)
data("coronavirus")
library(tidyverse)
library(ggplot2)
library(gganimate)

cov.cases <- coronavirus %>%
  group_by(continent_name) %>%
  filter(date=="2022-01-01")%>%
  summarise(total.case = sum(cases))

count <- coronavirus %>%
  filter(date=="2022-01-01")%>%
  group_by(country) %>%
  group_by(continent_name)%>%
  count(continent_name)

merge <- merge(x=count,y=cov.cases,all.x=TRUE,by=c("continent_name"))

### Barplot

ggplot(merge, aes()) + 
  geom_rect(aes(n~cov.cases, colour = continent_name, fill = continent_name)) +
  xlab("number of countries by continent") + 
  ylab("number of cases") +
  theme_ipsum() +
  theme(legend.position="none") 


### Animation 

coronavirus%>%
  filter(date>'2022-01-01')

library(ggplot2)
library(gganimate)

plot1 <- ggplot(coronavirus, aes(cases, long, size = population, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  # scale_colour_manual(values=country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent_name) +
  # Here comes the gganimate specific bits
  labs(title = 'date: {frame_time}', x = 'Cases', y = 'Longitude') +
  transition_time(date) +
  ease_aes('linear')

animate(plot1)
anim_save('plot1.gif')


######## group codes

rm(list=ls())

library(coronavirus)
data("coronavirus")
library(tidyverse)
library(ggplot2)

cov.cases <- coronavirus %>%
  group_by(continent_name) %>%
  filter(date=="2022-01-01")%>%
  summarise(total.case = sum(cases))

count <- coronavirus %>%
  filter(date=="2022-01-01")%>%
  group_by(country) %>%
  group_by(continent_name)%>%
  count(continent_name)

merge <- merge(x=count,y=cov.cases,all.x=TRUE,by=c("continent_name"))

# Variable Width Column Chart
df<-data.frame(Continent=merge$continent_name,Total= merge$total.case,width=merge$n)
df$w <- cumsum(df$width) #cumulative sums
df$wm <- df$w - df$width
df$n<- with(df, wm + (w - wm)/2)

vwcc  <- ggplot(df, aes(ymin = 0))+
  geom_rect(aes(xmin = wm, xmax = w, ymax = Total, fill = Continent))+
  geom_text(aes(x = n, y = Total, label = Continent),size=4,angle = 45)+
  labs(title = "COVID 19 Cases by Continent", x = "Number of Countries", y = "Total Number of Cases")

vwcc
