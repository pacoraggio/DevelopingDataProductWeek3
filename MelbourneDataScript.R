# Melbourne Data Main Script

## Melbourne Data Main Script

library(plotly)
library(dplyr)
library(ggplot2)

df.mel <- read.csv("./Data/melb_data.csv")

names(df.mel)
dim(df.mel)

df.mel <- df.mel[!(is.na(df.mel$Longtitude)),]
sum(is.na(df.mel$Lattitude))
sum(is.na(df.mel$Price))

windows()
ggplot(df.mel, aes(Price)) +
    geom_histogram(color = "black", fill = "red") +
    geom_vline(xintercept = mean(df.mel$Price)) + 
    geom_vline(xintercept = median(df.mel$Price), color ="red")

df.mel$PriceCategory <-cut(df.mel$Price,
                           breaks = quantile(df.mel$Price),
                           labels = c("low", "medium low", "medium high", "high"))

df.mel$MarkerColor <-cut(df.mel$Price,
                         breaks = quantile(df.mel$Price),
                         labels = c("darkgreen", "green", "red", "darkred"))

head(df.mel$Price)
head(df.mel$PriceCategory)
head(df.mel$MarkerColor)

dim(df.mel)
head(df.mel)
## If I plot 15064 different points it will take ages and the map would result in 
## a complete chaos

n.sample <- 800

df.melsampled <- df.mel[sample(nrow(df.mel), n.sample),]


df.melsampled$HoverText <- with(df, paste(Price, '<br>', 
                                          "Council: ", CouncilArea,
                                          "Region: ", Regionname))


p <- df.melsampled %>%
    plot_ly(
        lat = ~Lattitude,
        lon = ~Longtitude,
        marker = list(color = df.melsampled$MarkerColor),
        type = 'scattermapbox',
        hovertext = df.melsampled[,"Regionname"]) %>%
    layout(
        mapbox = list(
            style = 'open-street-map',
            zoom =9.5,
            center = list(lon = mean(df.melsampled$Longtitude), 
                          lat = mean(df.melsampled$Lattitude))))



plot_ly(df.melsampled, type = "scattermapbox") %>%
    add_trace(lat = filter(df.melsampled, PriceCategory == "low")$Lattitude,
              lon = filter(df.melsampled, PriceCategory == "low")$Longtitude,
              color = "Low",
              marker = list(color = "darkgreen"),
              mode = "markers") %>%
    add_trace(lat = filter(df.melsampled, PriceCategory == "medium low")$Lattitude,
              lon = filter(df.melsampled, PriceCategory == "medium low")$Longtitude,
              color = "Medium Low",
              marker = list(color = "green"),
              mode = "markers") %>%
    layout(
        mapbox = list(
            style = 'open-street-map',
            zoom =9.5,
            center = list(lon = mean(df.melsampled$Longtitude), 
                          lat = mean(df.melsampled$Lattitude)))
    ) 
