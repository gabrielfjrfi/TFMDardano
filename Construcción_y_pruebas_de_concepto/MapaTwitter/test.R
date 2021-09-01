
library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)
library(rtweet)
library(tidytext)
library(ggwordcloud)
library(tidyverse)
library(tidyquant)
library(httpuv)

library(osmdata)
library(tmaptools)

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

# get_timeline("pilarcarracelas",n=50)
#  geocode_for_free("") 

token <- create_token(
  app = "Spark Twitter Streaming Amat",
  consumer_key = "dxlZh3IpB5XOTkt7X8mwssqcp",
  consumer_secret = "r8E2rgQQVkkHR4x4S0UNdgP3Qews4gCddMHCyHyaAqFuyKgdWT",
  access_token="1270835605-E8kj1eyMWUMkoYBHrer1t1hgqaSSI0kNCg0vkLD",
  access_secret="0BVF2UTZXkcoThA4uiFshIKHoQjIuBBQ6ndJlHvP3aBqn"
)

# token %>% writer_rds("../my_twitter_token.rds")
# 
# token <- readRDS("../my_twitter_token.rds")

ui <- navbarPage(
  title = "Shiny Twitter",
  collapsible = TRUE,
  inverse = TRUE,
  theme = shinytheme("paper"),
  
  shiny::tabPanel(
    title = "Hash Tag Tracker",
    sidebarLayout(
      sidebarPanel(
        shiny::textInput(inputId = "query", label = "Topic / Hashtag", value = "#covid19"),
        sliderInput(
          inputId = "n_tweets",
          label = "Number of tweets:",
          min = 1,
          max = 1500,
          value = 500),
        shiny::textInput(inputId = "location", label = "Location", value = "london, uk"),
        sliderInput(
          inputId = "n_miles",
          label = "Twitter Search Radius (miles)",
          min=1,
          max=1500,
          value = 10),
        shiny::actionButton(inputId = "submit", "Submit", class="btn-primary")
        ),
    
    mainPanel(
      plotlyOutput("plotly"),
      leafletOutput("leaflet"),
      plotOutput("wordcloud")
      # div(
      #     class="row",
      #     div(
      #       class= "col-sm-8 panel",
      #       div(class="panel-heading", h5("Sentiment Polarity")),
      #       div(class="panel-body", plotlyOutput(outputId = "plotly", height = "250px"))
      #     ),
      #     div(
      #       class= "col-sm-8 panel",
      #       div(class="panel-heading", h5("Tweet Proximity")),
      #       div(class="panel-body", plotlyOutput(outputId = "leaflet", height = 250))
      #     )
      #   ),
      #   div(
      #     class="row",
      #     div(
      #       class= "col-sm-12 panel",
      #       div(class="panel-heading", h5("Sentiment Word Cloud")),
      #       div(class="panel-body", plotlyOutput(outputId = "wordcloud", height = "400px"))
      #   )
      # )
      )
    )
  )
)
server <- function(session, input, output){
  
  rv <- reactiveValues()
  
  observeEvent(input$submit, {
    
    rv$geocode <- input$location %>% geocode_for_free() %>% near_geocode(input$n_miles)
    
    rv$data <- search_tweets(
      q = input$query,
      n = input$n_tweets,
      include_rts =FALSE,
      geocode = rv$geocode,
      leng = "en",
      token = token
    )
    
    rv$tweet_sentiment <- rv$data %>%
      select(text) %>%
      rowid_to_column() %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("bing"))
    
  }, ignoreNULL = FALSE)
  
  output$plotly <- renderPlotly({
    req(rv$tweet_sentiment, rv$data)
    
    sentiment_by_row_id_tbl <- rv$tweet_sentiment %>%
      select(-word) %>%
      count(rowid, sentiment) %>%
      pivot_wider(names_from=sentiment, values_from=n, values_fill=list(n=0))%>%
      mutate(sentiment = positive-negative) %>%
      left_join(
        rv$data %>% select(screen_name, text) %>% rowid_to_column()
      )
    
    label_wrap <- label_wrap_gen(width = 60)
    
    data_formatted <- sentiment_by_row_id_tbl %>%
      mutate(text_formatted = str_glue("Row ID: {rowid}
                                       Screen Name: {screen_name}
                                       Text:
                                       {label_wrap(text)}"))
    
    g <- data_formatted %>%
      ggplot(aes(rowid, sentiment)) +
      geom_line(color="#2c3e50", alpha = 0.5)+
      geom_point(aes(text=text_formatted), color="#2c3e50")+
      geom_smooth(method = "loess", span=0.25, se= FALSE, color= "blue")+
      geom_hline(aes(yintercept = mean(sentiment)), color="blue")+
      geom_hline(aes(yintercept = mean(sentiment) + 1.96*IQR(sentiment)), color="red")+
      geom_hline(aes(yintercept = mean(sentiment) - 1.96*IQR(sentiment)), color="red")+
      theme_tq()+
      labs(x = "Twitter User", y = "Sentiment")
    
    ggplotly(g, tooltip = "text") %>%
      layout(
        xaxis = list(
          rangeslider = list(type = "date")
        )
      )
  })
  
  output$leaflet <- renderLeaflet({
    
    req(rv$geocode)
    
    data_prepared <- tibble(
      location = rv$geocode
    ) %>%
      separate(location, into= c("lat", "lon", "distance"), sep = ",", remove = FALSE) %>%
      mutate(distance = distance %>% str_remove_all("[^0-9.-]")) %>%
      mutate_at(.vars = vars(-location), as.numeric)
    
    data_prepared %>%
      leaflet() %>%
      setView(data_prepared$lon, data_prepared$lat, zoom=3) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = ~as.character(location), label = ~as.character(location)) %>%
      addCircles(lng = ~lon, lat = ~lat, weight = 1, radius = ~distance/0.000621371)
  })
  
  output$wordcloud <- renderPlot({
    
   req(rv$data)
    
    tweets_tokenized_tbl <- rv$data %>%
      select(text) %>%
      rowid_to_column() %>%
      unnest_tokens(word, text)
    
    sentiment_bing_tbl <- tweets_tokenized_tbl %>%
      inner_join(get_sentiments("bing"))
    
    sentiment_by_word_tbl <-sentiment_bing_tbl %>%
      count(word, sentiment, sort = TRUE)
    
    sentiment_by_word_tbl %>%
      slice(1:100) %>%
      mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
      ggplot(aes(label = word, color = sentiment, size= n)) +
      geom_text_wordcloud_area() + 
      facet_wrap(~sentiment, ncol=2) +
      theme_tq(base_size = 30) +
      scale_color_tq() +
      scale_size_area(max_size=16)
  })
}

shinyApp(ui = ui, server = server)
