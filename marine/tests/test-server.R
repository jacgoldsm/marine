library(testthat)
library(shiny)

test_that("all outputs are correct for ship KRISTIN", {
  testServer(expr =  {
    session$setInputs(ship_type = "Fishing")
    session$setInputs(select_vessel =  "KRISTIN")
    
    marine <- vroom::vroom("../ships.csv")
    
    fishing <- marine %>% filter(ship_type == "Fishing")
    
    names <- sort(unique(fishing$SHIPNAME))
    
    kristin <- marine %>% 
      filter(SHIPNAME == "KRISTIN") %>%
      arrange(DATETIME) %>%
      mutate(lag_lat = dplyr::lag(LAT), lag_lon = dplyr::lag(LON))
    
    distance <- geodist_vec(kristin$LON, 
                            kristin$LAT, 
                            kristin$lag_lon, 
                            kristin$lag_lat, 
                            paired = TRUE)
    
    kristin <- cbind(kristin, distance)
    max_index <- max(which(kristin$distance == max(kristin$distance, na.rm=TRUE)))
    beginning_index <- max_index - 1L
    
    max_dist <- kristin[max_index,]
    beginning_dist <- kristin[beginning_index,]
    
    
    expect_equal(process_type(), names)
    expect_equal(process_ship(), kristin)
    expect_equal(max_index(), max_index)
    expect_equal(beginning_index(), beginning_index)
    expect_equal(max_dist(), max_dist)
    expect_equal(beginning_dist(), beginning_dist)
    expect_equal(total_dist(), sum(kristin$distance, na.rm=TRUE))
    expect_equal(average_speed(), mean(kristin$SPEED, na.rm=TRUE))
    expect_equal(most_frequent_port(), DescTools::Mode(kristin$PORT, na.rm=TRUE))
  })
})


test_that("all outputs are correct for ship BALTICO", {
  context("app")
  testServer(expr =  {
    session$setInputs(ship_type = "Tanker")
    session$setInputs(select_vessel =  "BALTICO")
    marine <- vroom::vroom("../ships.csv")
    
    tanker <- marine %>% filter(ship_type == "Tanker")
    names <- sort(unique(tanker$SHIPNAME))
    
    baltico <- marine %>% 
      filter(SHIPNAME == "BALTICO") %>%
      arrange(DATETIME) %>%
      mutate(lag_lat = dplyr::lag(LAT), lag_lon = dplyr::lag(LON))
    
    distance <- geodist_vec(baltico$LON, 
                            baltico$LAT, 
                            baltico$lag_lon, 
                            baltico$lag_lat, 
                            paired = TRUE)
    
    baltico <- cbind(baltico, distance)
    
    max_index <- max(which(baltico$distance == max(baltico$distance, na.rm=TRUE)))
    
    beginning_index <- max_index - 1L
    
    max_dist <- baltico[max_index,]
    beginning_dist <- baltico[beginning_index,]
    
    
    expect_equal(process_type(), names)
    expect_equal(process_ship(), baltico)
    expect_equal(max_index(), max_index)
    expect_equal(beginning_index(), beginning_index)
    expect_equal(max_dist(), max_dist)
    expect_equal(beginning_dist(), beginning_dist)
    expect_equal(total_dist(), sum(baltico$distance, na.rm=TRUE))
    expect_equal(average_speed(), mean(baltico$SPEED, na.rm=TRUE))
    expect_equal(most_frequent_port(), DescTools::Mode(baltico$PORT, na.rm=TRUE))
  })
})


test_that("all outputs are correct for ship BALTICO", {
  testServer(expr =  {
    session$setInputs(ship_type = "Tanker")
    session$setInputs(select_vessel =  "BALTICO")
    marine <- vroom::vroom("../ships.csv")
    
    tanker <- marine %>% filter(ship_type == "Tanker")
    names <- sort(unique(tanker$SHIPNAME))
    
    baltico <- marine %>% 
      filter(SHIPNAME == "BALTICO") %>%
      arrange(DATETIME) %>%
      mutate(lag_lat = dplyr::lag(LAT), lag_lon = dplyr::lag(LON))
    
    distance <- geodist_vec(baltico$LON, 
                            baltico$LAT, 
                            baltico$lag_lon, 
                            baltico$lag_lat, 
                            paired = TRUE)
    
    baltico <- cbind(baltico, distance)
    
    max_index <- max(which(baltico$distance == max(baltico$distance, na.rm=TRUE)))
    beginning_index <- max_index - 1L
    
    max_dist <- baltico[max_index,]
    beginning_dist <- baltico[beginning_index,]
    
    
    expect_equal(process_type(), names)
    expect_equal(process_ship(), baltico)
    expect_equal(max_index(), max_index)
    expect_equal(beginning_index(), beginning_index)
    expect_equal(max_dist(), max_dist)
    expect_equal(beginning_dist(), beginning_dist)
    expect_equal(total_dist(), sum(baltico$distance, na.rm=TRUE))
    expect_equal(average_speed(), mean(baltico$SPEED, na.rm=TRUE))
    expect_equal(most_frequent_port(), DescTools::Mode(baltico$PORT, na.rm=TRUE))
  })
})


test_that("all outputs are correct for ship KAROLI", {
  context("app")
  testServer(expr =  {
    session$setInputs(ship_type = "Cargo")
    session$setInputs(select_vessel =  "KAROLI")
    marine <- vroom::vroom("../ships.csv")
    
    cargo <- marine %>% filter(ship_type == "Cargo")
    names <- sort(unique(cargo$SHIPNAME))
    
    karoli <- marine %>% 
      filter(SHIPNAME == "KAROLI") %>%
      arrange(DATETIME) %>%
      mutate(lag_lat = dplyr::lag(LAT), lag_lon = dplyr::lag(LON))
    
    distance <- geodist_vec(karoli$LON, 
                            karoli$LAT, 
                            karoli$lag_lon, 
                            karoli$lag_lat, 
                            paired = TRUE)
    
   karoli <- cbind(karoli, distance)
    
    max_index <- which.max(karoli$distance)
    beginning_index <- max_index - 1L
    
    max_dist <- karoli[max_index,]
    beginning_dist <- karoli[beginning_index,]
    
    
    expect_equal(process_type(), names)
    expect_equal(process_ship(), karoli)
    expect_equal(max_index(), max_index)
    expect_equal(beginning_index(), beginning_index)
    expect_equal(max_dist(), max_dist)
    expect_equal(beginning_dist(), beginning_dist)
    expect_equal(total_dist(), sum(karoli$distance, na.rm=TRUE))
    expect_equal(average_speed(), mean(karoli$SPEED, na.rm=TRUE))
    expect_equal(most_frequent_port(), DescTools::Mode(karoli$PORT, na.rm=TRUE))
  })
})

