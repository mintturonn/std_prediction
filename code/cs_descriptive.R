
library(here)
library(tidyverse)
library(ggplot2)

read.csv(here("data/cs_national.csv"), skip=6, header = TRUE) %>%
  mutate(Cases = as.numeric(gsub(",", "", Cases))) %>%
  add_row(Year=2020, Cases=2100) %>%
  mutate(denom = Cases / (Rate.per.100000/10^5))-> csn  # 2100 is projected, 2022 is reported to date

# estimate rate for 2020 using 2019 denominator
csn$Rate.per.100000[csn$Year==2020] <- 10^5 *csn$Cases[csn$Year==2020] / csn$denom[csn$Year==2019] 

csn %>%
  arrange(Year) %>%
  mutate(yrch = Rate.per.100000 / lag(Rate.per.100000)) -> csn

## project
incr <-mean(csn$yrch, na.rm = TRUE)
incr2 <- mean(csn$yrch[csn$Year>2012], na.rm = TRUE)

datext <- data.frame(year = 2020:2030, rate_max = 55.7, rate_min = 55.7) 

for (i in 2:11){
  datext$rate_max[i] <- datext$rate_max[i-1]*incr
}

datext %>%
  mutate(
  ymax = pmax(rate_max, rate_min),
  ymin = pmin(rate_max, rate_min),
  fill = rate_max >= rate_min ) -> datext
##


csn %>%
  ggplot() +
  geom_line(aes(x=Year, y=Rate.per.100000)) +
  geom_point(aes(x=Year, y=Rate.per.100000)) +
  geom_line(data=datext, aes(x=year, y=rate_max), color="red") +
  geom_line(data=datext, aes(x=year, y=rate_min), color="red") +
  geom_ribbon(data = datext, aes(x = year, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.4) +
  ylim(c(0, 135)) +
  ggtitle("Reported CS rate per 100,000 births") +
  theme_bw() + theme(legend.position = "none")