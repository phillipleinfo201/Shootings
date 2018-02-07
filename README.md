# Shootings

library(jsonlite)
library(dplyr)
library(httr)
library(plotly)
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoicGxlNDgwNiIsImEiOiJjajlwdXY5emo1cnJjMnFxbTZpd2h6c29mIn0.2vKEN89LyNwT8L7YK0Zr4Q')


shootings <- read.csv("~/Desktop/Info201/a6-mapping-phillipleinfo201/data/shootings-2017.csv")



## Total Amount of Shooting Events
shooting.events <- nrow(shootings)

## Max Death
max.death <- max(shootings$killed)

## Max injured
max.injured <- max(shootings$injured)

# Max Injured + Killed
max.injury.death <- max.injured + max.death

## Total Kiled
total.killed <- summarize(shootings, sum = sum(shootings$killed))

## Total Injured
total.injured <- summarize(shootings, sum = sum(shootings$injured))

## Cities Most Impacted 
city.impacted <- filter(shootings, shootings$killed > 50)
city.impacted2 <- paste(city.impacted$city,",", city.impacted$state)
date.impacted <- select(city.impacted, date)


## The dates are organized from the most recent -> 1st day of the year(2017)
order.shooting <- shootings[order(as.Date(shootings$date, format="%d/%m/%Y")),] %>%
  mutate(total = injured + killed)
no.vegas.shooting <- order.shooting[-c(19), ]

## Little Rock
little.rock <- order.shooting[c(116),]
injured.rock <- little.rock$injured
date.rock <- little.rock$date
name.little.rock <- paste(little.rock$city,",",little.rock$state)

## Cinncinnati Ohio
ohio <- order.shooting[c(217),]
injured.ohio <- ohio$injured
killed.ohio <- ohio$killed
date.ohio <- ohio$date
name.ohio <- paste(ohio$city,",",ohio$state)

## Interactive Map
m <- list(size = 2 * no.vegas.shooting$total)

interactive.map <- no.vegas.shooting %>%
  plot_mapbox( lat = ~lat, lon = ~lng,
               
              type = 'scattermapbox', mode = 'markers', marker = list(
                size= 2 * no.vegas.shooting$total,
                color=no.vegas.shooting$total,
                colors='blue',
                colorbar=list(
                  title='Total Injuries & Deaths'
                ),
                colorscale='Viridis',
                reversescale =T
                
              ), text = ~paste(date, "<br>", 'Location: ', city, state, "<br>", 'Injured: ', injured, "<br>","Killed: ",killed, "<br>", "Total Injured and Killed: ", total)) %>%
  
  layout(title = 'Shootings in the Year of 2017', margin = list(t = 50),
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark', zoom = 2.5, center = list(lat = ~median(order.shooting$lat),
                                                                 lon = ~median(order.shooting$lng))))
  
print(interactive.map)


plot.graph <- plot_ly(data = no.vegas.shooting, height = 800, width = 600, type = "scatter", mode = 'markers',x = ~no.vegas.shooting$total, y = ~no.vegas.shooting$state,
             text = ~paste(date, "<br>", 'Location: ', city, state, "<br>", 'Injured: ', injured, "<br>","Killed: ",killed)) %>%
  layout(
    title = 'Chart of Shootings in 2017', margin = list(l = 150, r = 150, b = 50, t = 100, pad = 4), 
    xaxis = list(
      ##type = 'category',
      title = 'Total Injuries and Death'
    ), 
    yaxis = list(
      title = 'State'
      
    )
  )

print(plot.graph)
