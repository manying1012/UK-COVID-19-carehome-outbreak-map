library(shiny) 

ui <- fluidPage(
  titlePanel("Care home infections across time"),
  sliderInput(inputId = "week_slider", "Week:", min(df4$week), max(df4$week), value = 1, step = 1),
  leafletOutput(outputId = "mymap", height = 600)
)

server <- function(input, output) {
  bins <- c(0, 10, 20, 40, 80, 160, Inf)
  
  output$mymap <- renderLeaflet({
    leaflet(lads_eng) %>%
      setView(lng = -2, lat = 52.9, zoom = 5.5)
  })
  
  observeEvent({input$week_slider},{
    covid_week <- input$week_slider
    test_data = df4$total_positive_test[df4$week == covid_week]
    pal <- colorBin("viridis", domain = df4$total_positive_test, bins = bins, na.color = "white")
    leafletProxy("mymap", data = lads_eng) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ pal(test_data),
        weight = .5,
        opacity = 1,
        color = "black",
        dashArray = "",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = paste0(df4$LAD21CD, ": ", df4$total_positive_test[df4$week == covid_week])%>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )) %>% 
      clearControls() %>%
      addLegend(pal = pal, values = ~df4$total_positive_test, opacity = 0.7, title = "Infections",
                position = "bottomright")
  })
}

shinyApp(ui, server)