library(gtrendsR)
library(xts)
library(tsbox)
library(dygraphs)
#remotes::install_github("trendecon/trendecon")
library(trendecon)
library(dygraphs)
library(dplyr)
library(readr)
library(htmlwidgets)

#Functions to compare years with dygraph
today <- as.character(Sys.Date())

#the axis label is passed as a date, this function outputs only the month of the date
getMonth <- 'function(d){
               var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
               return monthNames[d.getMonth()];
               }'

#the x values are passed as milliseconds, turn them into a date and extract month and day
getMonthDay <- 'function(d) {
                var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
                date = new Date(d);
                return monthNames[date.getMonth()] + " " +date.getDate(); }'

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Creating graph for article 1 ----
# How is the Swiss economy doing today?

blog_ar_1 <- gtrends(keyword = c("WC Papier","Rezession"), 
                        geo = "CH", 
                        time = "2019-05-29 2020-05-29")


# csv to create graph on webpage for article 1 
wc_papier <- blog_ar_1$interest_over_time[blog_ar_1$interest_over_time$keyword=="WC Papier",1:2]

write.csv(wc_papier, 
          file = file.path("data_examples","wc_papier.csv"),
          row.names = FALSE)

rezession <- blog_ar_1$interest_over_time[blog_ar_1$interest_over_time$keyword=="Rezession",1:2]
write.csv(rezession, 
          file = file.path("data_examples","rezession.csv"),
          row.names = FALSE)

# Creating example how it looks the graph
date_ts <- wc_papier$date
row.names(wc_papier) <- wc_papier$date
wc_papier <- wc_papier["hits"]
wc_papier_ts <- xts(wc_papier, 
                    order.by = date_ts)

date_ts <- rezession$date
row.names(rezession) <- rezession$date
rezession <- rezession["hits"]
rezession_ts <- xts(rezession, 
                    order.by = date_ts)

ts_dygraphs(ts_c(
  `WC Papier` = wc_papier_ts,
  `Rezession` = rezession_ts
)) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dySeries("WC Papier", strokePattern = "dashed") %>%
  dyAxis("x", drawGrid = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Creating graph for article 2 ----
#Large events will be allowed soon – but will people show up?

blog_ar_2 <- ts_gtrends(keyword = c("Festival","Tickets kaufen", "Hallenstadion", "Konzert"),
                        geo = "CH", 
                        time = "today 12-m")


# csv to create graph on webpage for article 2 
write.csv(blog_ar_2, 
          file = file.path("data_examples","article_2.csv"),
          row.names = FALSE)

# Creating example how it looks the graph
ts_dygraphs(blog_ar_2) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dySeries("Konzert", strokePattern = "dashed") %>%
  dyAxis("x", drawGrid = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Creating graph for article 3 ----
# Culinary consumption

blog_ar_3 <- ts_gtrends(keyword = c("Restaurant", "Bar", "Rezept", "Sauerteig"),
                             geo = "CH", 
                             time = "today 12-m")

# csv to create graph on webpage for article 3
write.csv(blog_ar_3, 
          file = file.path(".","data_examples","article_3.csv"),
          row.names = FALSE)

# Creating example how it looks the graph
ts_dygraphs(blog_ar_3,) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dySeries("Restaurant", strokePattern = "dashed") %>%
  dySeries("Bar", strokePattern = "dashed") %>%
  dyAxis("x", drawGrid = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Creating graphs for article 3 A ----
# Mobility trends

# comparisson trendecon and kof mobility index
ar_3_mobility_kof <-  fread("https://raw.githubusercontent.com/KOF-ch/economic-monitoring/master/data/ch.kof.mobind.csv")
ar_3_mobility_kof <- ar_3_mobility_kof[variable == "mobil" & trans == "meanvar"]
ar_3_mobility_kof <- ar_3_mobility_kof[, `:=`(variable = NULL , trans = NULL)]
ar_3_mobility_kof <- as.xts(ar_3_mobility_kof)
ar_3_mobility_kof_r <- (ar_3_mobility_kof-min(ar_3_mobility_kof))/(max(ar_3_mobility_kof)-min(ar_3_mobility_kof))
write.zoo(ar_3_mobility_kof_r, 
          file = file.path("data_examples","article_3_a_mobility_kof.csv"),
          index.name = "time", 
          quote = FALSE,
          sep = ",")


mobility_sa <- as.xts(zoo::read.csv.zoo("https://raw.githubusercontent.com/trendecon/data/master/data/ch/mobility_sa.csv",index.column = 1))
mobility_trendecon <- as.xts(mobility_sa["2020-01-07/2020-08-24"])
mobility_trendecon_r <- (mobility_trendecon-min(mobility_trendecon))/(max(mobility_trendecon)-min(mobility_trendecon))

mobility <- cbind(ar_3_mobility_kof_r,mobility_trendecon_r)

ts_dygraphs(mobility) %>%
  dyAxis("x", drawGrid = FALSE)%>%
  dySeries("value", label = "KOF mobility index") %>%
  dySeries("mobility_trendecon_r", label = "trendecon mobility index", strokePattern = "dashed") %>%
  dyEvent("2020-3-16", "Lockdown    ", labelLoc = "top") %>%
  dyEvent("2020-4-27", "Reopening - Phase I    ", labelLoc = "top") %>%
  dyEvent("2020-5-11", "Reopening - Phase II    ", labelLoc = "top") %>%
  dyEvent("2020-6-8", "Reopening - Phase III    ", labelLoc = "top") %>% 
  dyLegend(width = 500) %>% 
  dyHighlight(highlightCircleSize = 3, 
              highlightSeriesBackgroundAlpha = 0.7,
              hideOnMouseOut = TRUE)


# plot for keywords

blog_ar_3_a <- ts_gtrends(keyword = c("Velo kaufen",
                                    "Auto kaufen",
                                    "Bus Ticket",
                                    "Zugticket"),
                        geo = "CH", 
                        time = "2017-01-01 2020-08-26")

write.csv(blog_ar_3_a, 
          file = file.path("data_examples","article_3_a.csv"),
          row.names = FALSE)

# Creating example how it looks the graph

# This plot isn't so clear even with monthly smoothing
ts_dygraphs(blog_ar_3_a) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>%
  dyAxis("x", drawGrid = FALSE)
#even with monthly smoothing doesn't work completely
ts_dygraphs(blog_ar_3_a %>%
              ts_frequency("month")) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>%
  dyAxis("x", drawGrid = FALSE)

#trying grouping words
ts_dygraphs(blog_ar_3_a[blog_ar_3_a$id=="Velo kaufen" | blog_ar_3_a$id=="Auto kaufen",]) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>%
  dyAxis("x", drawGrid = FALSE)
#the plot is clear with smooth
ts_dygraphs(blog_ar_3_a[blog_ar_3_a$id=="Velo kaufen" | blog_ar_3_a$id=="Auto kaufen",] %>% 
              ts_frequency("month")) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dyAxis("x", drawGrid = FALSE)

ts_dygraphs(blog_ar_3_a[blog_ar_3_a$id=="Bus Ticket" | blog_ar_3_a$id=="Zugticket",] %>% 
              ts_frequency("month")) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dyAxis("x", drawGrid = FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Creating graphs for article 4 ----
# shifts in demands

blog_ar_4 <- ts_gtrends(keyword = c("Brot backen",
                                      "Essen bestellen",
                                      "Netflix"),
                          geo = "CH", 
                          time = "2018-01-01 2020-08-24")

write.csv(blog_ar_4, 
          file = file.path("data_examples","article_4.csv"),
          row.names = FALSE)

# Creating example how it looks the graph

ts_dygraphs(blog_ar_4 %>% 
              ts_frequency("month")) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dyAxis("x", drawGrid = FALSE)


#%%% comparisson trendecon indices

food <- read.csv("https://raw.githubusercontent.com/trendecon/data/master/data/ch/fooddelivery_sa.csv") %>%
  select(time,value) %>%
  ts_xts()

food_2018 <- ts_data.frame(food["2018/2018-09-01"])%>% 
  mutate(time = as.Date(gsub("2018","2020",time))) %>% 
  ts_xts()

food_2019 <- ts_data.frame(food["2019/2019-09-01"])%>% 
  mutate(time = as.Date(gsub("2019","2020",time))) %>% 
  ts_xts()

food_2020 <- ts_data.frame(food["2020/2020-09-01"]) %>% 
  ts_xts()

dygraph(ts_c(
  `2018` = food_2018,
  `2019` = food_2019,
  `2020` = food_2020
)) %>%
  dyAxis("x",valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonth)) %>% 
  dySeries("2018", strokePattern = "dotdash") %>%
  dySeries("2019", strokePattern = "dashed") %>%
  dySeries("2020", strokePattern = NULL) %>%
  dyAxis("x", drawGrid = FALSE) %>% 
  dyAxis("y", label = "Food delivery trendecon index")


garden <- read.csv("https://raw.githubusercontent.com/trendecon/data/master/data/ch/garden_sa.csv") %>%
  select(time,value) %>%
  ts_xts()

garden_2018 <- ts_data.frame(garden["2018/2018-09-01"])%>% 
  mutate(time = as.Date(gsub("2018","2020",time))) %>% 
  ts_xts()

garden_2019 <- ts_data.frame(garden["2019/2019-09-01"])%>% 
  mutate(time = as.Date(gsub("2019","2020",time))) %>% 
  ts_xts()

garden_2020 <- ts_data.frame(garden["2020/2020-09-01"]) %>% 
  ts_xts()

dygraph(ts_c(
  `2018` = garden_2018,
  `2019` = garden_2019,
  `2020` = garden_2020
)) %>%
  dyAxis("x",valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonth)) %>% 
  dySeries("2018", strokePattern = "dotdash") %>%
  dySeries("2019", strokePattern = "dashed") %>%
  dySeries("2020", strokePattern = NULL) %>%
  dyAxis("x", drawGrid = FALSE) %>% 
  dyAxis("y", label = "Gardening and home improvement trendecon index")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Creating graphs for article 6 ----
# Football is back

blog_ar_6 <- ts_gtrends(keyword = c("spielplan",
                                    "saisonkarte",
                                    "fallzahlen",
                                    "maskenpflicht"),
                        geo = "CH", 
                        time = "2016-01-01 2020-09-17")

write.csv(blog_ar_6, 
          file = file.path("data_examples","article_6.csv"),
          row.names = FALSE)

# Cultural Event indicator
social_sa <- read.csv("https://raw.githubusercontent.com/trendecon/data/master/data/ch/social_sa.csv") %>%
  select(time,value) %>%
  ts_xts()
dm_social_sa <- social_sa %>%
  ts_frequency("month")
dm_social_sa_2016_2020 <- ts_data.frame(cultural["2016/2020-09-17"]) %>% 
  ts_xts()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Creating graphs for article 7 ----
# How do Swiss consumers cope with masks

ts_dygraphs(ts_c(blog_ar_6,`Cultural Event TrendEcon Index`=dm_social_sa_2016_2020) %>% 
              ts_frequency("month")) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dyAxis("x", drawGrid = FALSE)


blog_ar_7_CH <- ts_gtrends(keyword = c("öffnungszeiten", 
                                       "shopping center",
                                       "maskenpflicht",
                                       "coop at home",
                                       "le shop",
                                       "galaxus"),
                           geo = c("CH"), 
                           time = "2018-10-20 2020-10-21")

blog_ar_7_ZH <- ts_gtrends(keyword = c("öffnungszeiten", 
                                       "shopping center",
                                       "maskenpflicht"),
                           geo = c("CH-ZH"), 
                           time = "2018-10-20 2020-10-21")

# csv to create graph on webpage for article 3
write.csv(blog_ar_7_CH, 
          file = file.path("..","data_examples","article_7_CH.csv"),
          row.names = FALSE)
write.csv(blog_ar_7_ZH, 
          file = file.path("..","data_examples","article_7_ZH.csv"),
          row.names = FALSE)

# Overview Zürich vs CH
ts_dygraphs(ts_c(
  `CH` = blog_ar_7_CH[blog_ar_7_CH$id=="maskenpflicht",],
  `ZH` = blog_ar_7_ZH[blog_ar_7_ZH$id=="maskenpflicht",]
)) %>%
  dyRoller(rollPeriod = 4)%>% 
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dySeries("maskenpflicht", label = "\"maskenpflicht\" searched in CH") %>%
  dySeries("maskenpflicht.1", label= "\"maskenpflicht\" searched in ZH only", strokePattern = "dashed") %>%
  dyEvent("2020-3-16", "Start Lockdown    ", labelLoc = "top") %>%
  dyEvent("2020-4-27", "Reopening - Phase I    ", labelLoc = "top") %>%
  dyEvent("2020-7-6", "Mask Measure CH - Public transport    ", labelLoc = "top") %>% 
  dyEvent("2020-8-27", "Mask Measure ZH - Retail    ", labelLoc = "top") %>% 
  dyEvent("2020-10-19", "Mask Measure CH - Retail    ", labelLoc = "top") %>% 
  dyAxis("x", drawGrid = FALSE)


# Creating example how it looks the graph
ts_dygraphs(ts_c(blog_ar_7_CH[blog_ar_7_CH$id=="öffnungszeiten" | blog_ar_7_CH$id=="shopping center",], 
                 blog_ar_7_ZH[blog_ar_7_ZH$id=="öffnungszeiten" | blog_ar_7_ZH$id=="shopping center",])) %>%
  dyRoller(rollPeriod = 4)%>% 
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dySeries("öffnungszeiten", label = "\"öffnungszeiten\" searched in CH") %>%
  dySeries("öffnungszeiten.1", label = "\"öffnungszeiten\" searched in ZH", strokePattern = "dashed") %>%
  dySeries("shopping center", label = "\"shopping center\" searched in CH") %>%
  dySeries("shopping center.1", label = "\"shopping center\" searched in ZH", strokePattern = "dashed") %>%
  dyEvent("2020-3-16", "Start Lockdown    ", labelLoc = "top") %>%
  dyEvent("2020-4-27", "Reopening - Phase I    ", labelLoc = "top") %>%
  dyEvent("2020-7-6", "Mask Measure CH - Public transport    ", labelLoc = "top") %>% 
  dyEvent("2020-8-27", "Mask Measure ZH - Retail    ", labelLoc = "top") %>%
  dyEvent("2020-10-19", "Mask Measure CH - Retail    ", labelLoc = "top") %>% 
  dyAxis("x", drawGrid = FALSE)

ts_dygraphs(blog_ar_7_CH[blog_ar_7_CH$id=="coop at home" | blog_ar_7_CH$id=="le shop" | blog_ar_7_CH$id=="galaxus",]) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>%
  dyEvent("2020-3-16", "Start Lockdown    ", labelLoc = "top") %>%
  dyEvent("2020-4-27", "Reopening - Phase I    ", labelLoc = "top") %>%
  dyEvent("2020-7-6", "Mask Measure CH - Public transport    ", labelLoc = "top") %>% 
  dyEvent("2020-8-27", "Mask Measure ZH - Retail   ", labelLoc = "top") %>%
  dyEvent("2020-10-19", "Mask Measure CH - Retail    ", labelLoc = "top") %>% 
  dyAxis("x", drawGrid = FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Creating graphs for article 9 ----
# The employee's best companion
blog_ar_9_CH <- ts_gtrends(keyword = c("qualipet", 
                                       "meerschweinchen",
                                       "tierheim",
                                       "katze kaufen",
                                       "hund kaufen"),
                           geo = c("CH"), 
                           time = "2016-01-01 2021-01-07")
write.csv(blog_ar_9_CH, 
          file = file.path("..","data_examples","article_9.csv"),
          row.names = FALSE)

# Chart 1: long-term
ts_dygraphs(blog_ar_9_CH[blog_ar_9_CH$id=="qualipet" | blog_ar_9_CH$id=="tierheim" | blog_ar_9_CH$id=="meerschweinchen",]) %>%
  dyRoller(rollPeriod = 8)%>% 
  dyAxis("y", label = "Hits (index from Google trends)") %>%
  #dyEvent("2020-2-28", "Declaration of 'Special Situation'", labelLoc = "bottom")%>%
  #dyEvent("2020-3-16", "Declaration of 'Extraordinary Situation'", labelLoc = "bottom") %>%
  #dyEvent("2020-4-27", "Shutdown Easing Phase 1", labelLoc = "bottom") %>%
  #dyEvent("2020-5-11", "Shutdown Easing Phase 2", labelLoc = "bottom") %>%
  #dyEvent("2020-6-8", "Shutdown Easing Phase 3", labelLoc = "bottom") %>%
  #dyEvent("2020-6-22", "End of 'Extraordinary Situation'", labelLoc = "bottom") %>%
  #dyEvent("2020-10-19", "Mask wearing obligation extended", labelLoc = "bottom") %>%
  #dyEvent("2020-10-28", "Reinforce containment measures", labelLoc = "bottom") %>%
  dyAxis("x", drawGrid = FALSE)

# Chart 2: pandemic
blog_ar_9_CH_cut <- ts_data.frame(blog_ar_9_CH["2019/2019-09-01"])%>% 
  mutate(time = as.Date(gsub("2020","2021",time))) %>% 
  ts_xts()

ts_dygraphs(blog_ar_9_CH[blog_ar_9_CH$id=="hund kaufen" | blog_ar_9_CH$id=="katze kaufen" | blog_ar_9_CH$id=="meerschweinchen",]) %>%
  dyRoller(rollPeriod = 4)%>%
  dyRangeSelector(dateWindow = c("2020-01-01", "2021-01-01"))%>%
  dyAxis("y", label = "Hits (index from Google trends)") %>%
  dyEvent("2020-2-28", "Declaration of 'Special Situation'", labelLoc = "bottom")%>%
  dyEvent("2020-3-16", "Declaration of 'Extraordinary Situation'", labelLoc = "bottom") %>%
  dyEvent("2020-4-27", "Shutdown Easing Phase 1", labelLoc = "bottom") %>%
  dyEvent("2020-5-11", "Shutdown Easing Phase 2", labelLoc = "bottom") %>%
  dyEvent("2020-6-8", "Shutdown Easing Phase 3", labelLoc = "bottom") %>%
  dyEvent("2020-6-22", "End of 'Extraordinary Situation'", labelLoc = "bottom") %>%
  dyEvent("2020-10-19", "Mask wearing obligation extended", labelLoc = "bottom") %>%
  dyEvent("2020-10-28", "Reinforce containment measures", labelLoc = "bottom") %>%
  dyAxis("x", drawGrid = FALSE)
