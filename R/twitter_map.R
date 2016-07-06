library(XLConnect)
library(twitteR)
library(plyr)
library(dplyr)
library(ggplot2)

ws <- readWorksheetFromFile('GIS/Geolocated-twitter.xls',sheet=1,startRow=2)

key <- 'aZolKBHmBUotTNpf5p1gk5SJ3'
secret <- 'S8wSc8KNImg12Ho1ydkzM7HkUX3GRx13JkFipWli4Bvw0mP8ER'
access <- '862564638-2vhtPUVAR5NVHzduKp9396jw1ipToSkT1k7KoFdQ'
access_secret <- '1wH5xEwB6Y7Pr5Ta6GNegFuDU6HOzIq17KMB0q3ib0e9Z'
setup_twitter_oauth(key,secret,access,access_secret)

ids <- ldply(strsplit(ws$URL,'/'), function(x) x[6])

# Check rate limits
g <- getCurRateLimitInfo()
g[g$resource=='/statuses/show/:id',]

# I get 180 calls every 15 minutes; up to 100 per call. 
# see: https://dev.twitter.com/rest/reference/get/statuses/show/%3Aid
# As long as I need less than 18,000 posts, I can do this all in one shot.

ids_grouped <- split(ids$V1, ceiling(seq_along(ids$V1)/100))


# This next step takes a while
latlon <- ldply(ids_grouped,function(g) {
  print(paste(i,'/',length(ids_grouped)))
  lookup_statuses(g) %>%
    ldply(function(x) data.frame(lat=x$latitude,lon=x$longitude))
})

latlon$lat <- latlon$lat %>% as.character() %>% as.numeric()
latlon$lon <- latlon$lon %>% as.character() %>% as.numeric()

ggplot(latlon,aes(lon,lat)) +
  geom_point(color='firebrick4',alpha=0.02) +
  theme_classic()
# Pretty much all in Kigali

write.csv(latlon[,c('lat','lon')],'GIS/Twitter-latlon.csv',row.names=FALSE)
  

