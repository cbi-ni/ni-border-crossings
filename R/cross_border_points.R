library(dplyr)
library(ggplot2)
library(ggthemes)
library(jsonlite)
library(magrittr)
library(maptools)



generate_shapefile_map <- function() {
  spdf <- getwd() %>% 
    paste0("/data/map/parliamentaries/NI-parliamentary-boundaries.shp") %>%
    maptools::readShapePoly()
  
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- ggplot2::fortify(spdf, region = "id")
  heatmap.data <- dplyr::inner_join(
    spdf.points, 
    spdf@data, 
    by = "id")
  
  heatmap.data$COUNTYNAME %<>% 
    as.vector()
  
  shapefileMap <- ggplot(
    data = heatmap.data) + 
    geom_polygon(
      colour = "white",
      fill = "grey",
      size = 0.5, 
      aes(
        x = long, 
        y = lat, 
        group = group))
  
  return(shapefileMap)
}


cross_border_points <- function() {
  borderCrossingPoints <- getwd() %>%
    paste0("/data/border-points/borderCrossingPoints2018.geojson") %>%
    jsonlite::fromJSON()
  
  borderData <- cbind(
    borderCrossingPoints$features$properties, 
    borderCrossingPoints$features$geometry)
  
  lat <- long <- mainClass <- c()
  for (i in 1:(borderData %>% nrow())) {
    lat %<>% append(borderData$coordinates[i][[1]][1])
    long %<>% append(borderData$coordinates[i][[1]][2])
    splitRoadClass <- borderData$RoadClass[i] %>% 
      strsplit(split = " - ")
    splitRoadClass <- splitRoadClass[[1]][1]
    splitRoadClass %<>% strsplit(split = "- ")
    mainClass %<>% append(splitRoadClass[[1]][1])
  }
  
  borderData$lat <- lat
  borderData$long <- long
  borderData$mainClass <- mainClass
  
  borderPointsMap <- generate_shapefile_map() +
    geom_point(
      data = borderData,
      aes(
        x = lat, 
        y = long,
        colour = mainClass,
        size = 1),
      alpha = 0.6) + 
    labs(
      color = "Road class") +
    ylab("") + 
    xlab("") +
    coord_cartesian(
      xlim = c(-8.25, -5),
      ylim = c(54, 55.5)) +
    scale_size(guide = 'none') +
    theme_minimal() +
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right") +
    annotate(
      geom = "text", 
      x = -5.3, 
      y = 55.2, 
      label = paste0(max(borderData$Id), " border points"))
  
  getwd() %>%
    paste0("/images/ni-cross-border-map.png") %>%
    ggsave(
      plot = borderPointsMap,
      device = "png")
  
  return(borderPointsMap)
}


cross_border_points()
