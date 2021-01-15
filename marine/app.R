
ui <- semanticPage(
        title = "Vessels",
        div(
          flow_layout(
            dropdown_input("ship_type", 
                           choices = choices,
                           value = "Passenger"),
            dropdown_input("select_vessel", 
                           choices = NULL, 
                           value = NULL),
            h2("Marine Travels", 
                         icon("ship"),
                         style = "color:blue; 
                                  font-family: Monospace"),
            column_gap = "8%",
            min_cell_width = "28%",
            max_cell_width = "28%"),
          style="margin:3%"
        ),
      div(class = "ui horizontal divider", "Ship's Path",
                    style="margin:3%"),
      div(
          leafletOutput("map")  %>% 
                    withSpinner(color="#0dc5c1"),
          style = "margin:3%"),
      div(class = "ui horizontal divider", "Stats",
          style = "margin:3%"),
      div(class = "ui raised segment",
        flow_layout(
          div(textOutput("text1")),
          div(textOutput("text2")),
          div(textOutput("text3")),
          div(textOutput("text4")),
          column_gap = "5%",
          min_cell_width = "20%",
          max_cell_width = "20%"),
          style = "font-family:Helvetica, sans-serif;
                   font-size:1.1em;
                   margin:3%;")
)

server <- shinyServer(function(input, output, session) {
  
   
    process_type <- reactive({
      ship_names <- ships %>% filter(ship_type == input$ship_type)
      sort(unique(ship_names$SHIPNAME))
    })
  
    # When the user selects a `ship_type`, update the dropdown with all the
    # `SHIPNAME`s of that type. Defaults to the first alphabetically,
    # just because of the order of the list.
    observeEvent(input$ship_type, {
      choices <- process_type()
      update_dropdown_input(session, "select_vessel", choices = choices)
    }) 
  
    # `geodist_vec` returns distance in meters
    process_ship <- reactive({
      req(input$select_vessel)
      ship <- ships %>% filter(SHIPNAME == input$select_vessel) %>%
                      arrange(DATETIME) %>%
                      mutate(lag_lat = dplyr::lag(LAT), lag_lon = dplyr::lag(LON))
      distance <- geodist_vec(ship$LON, ship$LAT, ship$lag_lon, ship$lag_lat, paired = TRUE)
      cbind(ship, distance)
    })
  

    
    max_index <- reactive({
      # This is a slightly more complicated version of `which.max()`
      # that finds the *last* index instead of the first, because that
      # is the most recent
      max(which(process_ship()$distance == max(process_ship()$distance, na.rm=TRUE)),
          na.rm=TRUE)
    })
  
    beginning_index <- reactive({
      max_index() - 1L # one observation older than the max index
    })
  
    max_dist <- reactive({
      process_ship()[max_index(),]
    })
  
    beginning_dist <- reactive({
      process_ship()[beginning_index(),]
    })
  
    total_dist <- reactive({
      sum(na.omit(process_ship()$distance))
    })
  
  
    average_speed <- reactive({
        mean(process_ship()$SPEED)
    })
    
    most_frequent_port <- reactive({
      DescTools::Mode(process_ship()$PORT)
    })
    
  
    output$text1 <- renderText({
    glue::glue("The maximum distance travelled by ship 
                {input$select_vessel} between two observations
                was {round(max_dist()$distance)} meters.")})
    
   output$text2 <- renderText({
     glue::glue("The total distance travelled by ship 
                {input$select_vessel} over this time period
                was {round(total_dist())} meters.")
   })
   
   output$text3 <- renderText({
     glue::glue("The average speed at which this ship
                travelled was {round(average_speed(), 2)} knots.")
   })
    
   output$text4 <- renderText({
     glue::glue("The most frequent port that this ship was at
                was {most_frequent_port()}.")
   })
   
   
   
    output$map <- renderLeaflet({
      process_ship() %>% 
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addCircleMarkers(popup = glue("Time: {process_ship()$DATETIME}"),
                       radius = 4L, color = "red",
                       labelOptions = labelOptions(textsize = '12px')) %>%
      addMarkers(data = max_dist(), label = "End of longest consecutive travel",
                 labelOptions = labelOptions(direction = "right",
                                             style = list(
                                               "color" = "red",
                                               "font-family" = "serif",
                                               "font-style" = "bold",
                                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)"))) %>%
      addMarkers(data = beginning_dist(), label = "Start of longest consecutive travel",
                 labelOptions = labelOptions(direction = "left",
                                             style = list(
                                               "color" = "blue",
                                               "font-family" = "serif",
                                               "font-style" = "bold",
                                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)")))
    })
    
})

shinyApp(ui = ui, server = server)
