library(leaflet)
library(tmaptools)
library(osmdata)

geocode_for_free <- function(ubiSearch){
  results <- getbb(ubiSearch, format_out = "data.frame")[1,]
  
  boxcoord <- data.frame(
    "sw.lng" = as.numeric(results$boundingbox[[1]][2]),
    "sw.lat" = as.numeric(results$boundingbox[[1]][1]),
    "ne.lng" = as.numeric(results$boundingbox[[1]][4]),
    "ne.lat" = as.numeric(results$boundingbox[[1]][3])
  )
  
  row.names(boxcoord) <- ""
  
  point <- data.frame(
    "lat" = as.numeric(results$lat),
    "lng" = as.numeric(results$lon)
  )
  
  row.names(point) <- ""
  
  salida <- list(place=ubiSearch,box= boxcoord ,point=point)
  
  return(salida)
}

near_geocode <- function(ubiData,mi){
  return(paste0(ubiData$point$lat,",",ubiData$point$lng,",",mi,"mi"))
  }

geocode_for_free("london, uk")%>%near_geocode(100)

input <- "london, uk"

q <- getbb(input, format_out = "data.frame")[1,]

boxcoord <- data.frame(
  "sw.lng" = as.numeric(q$boundingbox[[1]][2]),
  "sw.lat" = as.numeric(q$boundingbox[[1]][1]),
  "ne.lng" = as.numeric(q$boundingbox[[1]][4]),
  "ne.lat" = as.numeric(q$boundingbox[[1]][3])
)

row.names(boxcoord) <- ""

point <- list(lat=as.numeric(q$lat), lng=as.numeric(q$lon))

salida <- list(place=input,box= boxcoord ,point=point)

salida

quakes[1:20,] %>% leaflet() %>% addTiles() %>%addMarkers(~long, ~lat, popup = ~as.character(mag), label =  ~as.character(mag))

st <- search_tweets(
  q = "#covid19",
  n = 100,
  include_rts =FALSE, geocode = geocode_for_free("london, uk")%>%near_geocode(100),
  leng = "en",
  token = token
)

tweet_sentiment <- st %>%
  select(text) %>%
  rowid_to_column() %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))


st %>%
  select(screen_name, text, coords_coords) %>%
  unnest_wider(coords_coords) %>%
  filter(!is.na(...1)) %>%
  set_names(c("screen_name","text","lon","lat")) %>%
  leaflet() %>%
  addTiles() %>% 
  addMarkers(~lon, ~lat, popup = ~as.character(text), label =  ~as.character(screen_name))










req(rv$tweet_sentiment, rv$data)

sentiment_by_row_id_tbl <- tweet_sentiment %>%
  select(-word) %>%
  count(rowid, sentiment) %>%
  pivot_wider(names_from=sentiment, values_from=n, values_fill=list(n=0))%>%
  mutate(sentiment = positive-negative) %>%
  left_join(
    st %>% select(screen_name, text) %>% rowid_to_column()
  )




label_wrap <- label_wrap_gen(width = 60)

data_formatted <- sentiment_by_row_id_tbl %>%
  mutate(text_formatted = str_glue("Row ID: {rowid}
                                       Screen Name: {screen_name}
                                       Text:
                                       {label_warp(text)}"))

g <- data_formatted %>%
  ggplot(aes(rowid, sentment)) +
  geom_line(color="#2c3e50", alpha = 0.5)+
  geom_point(aes(text=text_formatted), color="#2c3e50")+
  geom_smooth(method = "loess", span=0.25, se= FALSE, color= "blue")+
  geom_hline(aes(yintercept = mean(sentiment)), color="blue")+
  geom_hline(aes(yintercept = mean(sentiment) + 1.96*IQR(sentiment)), color="red")+
  geom_hline(aes(yintercept = mean(sentiment) - 1.96*IQR(sentiment)), color="red")+
  themm_tq()+
  labs(x = "Twitter User", y = "Sentiment")

ggplotly(g, tooltip = "text") %>%
  layout(
    xaxis = list(
      rangeslider = list(type = "date")
    )
  )









