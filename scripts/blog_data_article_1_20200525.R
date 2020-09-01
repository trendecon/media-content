library(gtrendsR)
library(xts)
library(tsbox)
library(dygraphs)
remotes::install_github("trendecon/trendecon")
library(trendecon)

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

