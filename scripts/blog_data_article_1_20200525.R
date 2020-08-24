library(gtrendsR)
library(xts)
library(tsbox)
library(dygraphs)
remotes::install_github("trendecon/trendecon")
library(trendecon)

# Creating graph for article 1 ----

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

# Creating graph for article 2 ----

blog_ar_2 <- ts_gtrends(keyword = c("Festival","Tickets kaufen", "Hallenstadion", "Konzert"),
                        geo = "CH", 
                        time = "today 12-m")


# csv to create graph on webpage for article 1 
write.csv(blog_ar_2, 
          file = file.path("data_examples","article_2.csv"),
          row.names = FALSE)

# Creating example how it looks the graph
ts_dygraphs(blog_ar_2) %>%
  dyAxis("y", label = "Hits (index from Google trends)") %>% 
  dySeries("Konzert", strokePattern = "dashed") %>%
  dyAxis("x", drawGrid = FALSE)

