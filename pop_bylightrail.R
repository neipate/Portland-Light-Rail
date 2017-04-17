library('tigris')
library('rgdal')

# load workspace file for block shapefiles e.g. load(file='/Users/neilpatel/Documents/portland_blocks')
# or use blocks command from tigris library


lightrail_stops <- readOGR(dsn='/Users/neilpatel/Downloads/tm_rail_stops/')
lightrail_stops <- subset(lightrail_stops,lightrail_stops$TYPE=="MAX")
lightrail_stops_latlon <- spTransform(lightrail_stops, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))


n_lightrailstops <- dim(lightrail_stops_latlon)[1]


multanomah_blocks <- read.csv('/Users/neilpatel/Downloads/multamonah_or/DEC_10_PL_P2_with_ann.csv', stringsAsFactors = FALSE)
washington_blocks <- read.csv('/Users/neilpatel/Downloads/washington_or/DEC_10_SF1_P1_with_ann.csv', stringsAsFactors = FALSE)
clackmas_blocks  <- read.csv('/Users/neilpatel/Downloads/clackmas_or/DEC_10_SF1_P1_with_ann.csv', stringsAsFactors = FALSE)

multanomah_blocks$population <- as.numeric(multanomah_blocks$D001)
washington_blocks$population <- as.numeric(washington_blocks$D001)
clackmas_blocks$population <- as.numeric(clackmas_blocks$D001)

colnames <- c('GEO.id2','population')

portland_pop_df <- rbind(multanomah_blocks[, colnames], washington_blocks[, colnames], clackmas_blocks[, colnames])

blocklist <- geo_join(portland_blocks, portland_pop_df,"GEOID10","GEO.id2")
blocklist <-blocklist[!is.na(blocklist$STATEFP10),]
blocklist$INTPTLON10 <- as.numeric(blocklist$INTPTLON10)
blocklist$INTPTLAT10 <- as.numeric(blocklist$INTPTLAT10)

n_blocks <- dim(blocklist)[1]

x_dist <- matrix(0,dim(blocklist)[1],dim(lightrail_stops_latlon)[1])
y_dist <- matrix(0,dim(blocklist)[1],dim(lightrail_stops_latlon)[1])
dist   <- y_dist
 
deg2km <- 111.6
walk_dist <- 0.8
lightrail_stops_latlon$walk_pop <- rep(0, n_lightrailstops)

for (i in 1:n_lightrailstops) {

  x_dist[,i] <- (blocklist@data[,'INTPTLON10']-lightrail_stops_latlon@coords[i,1])*deg2km*cos((lightrail_stops_latlon@coords[i,2]/180)*pi)
  y_dist[,i] <- (blocklist@data[,'INTPTLAT10']-lightrail_stops_latlon@coords[i,2])*deg2km
  dist[,i]    <- abs(x_dist[,i]) + abs(y_dist[,i]) 
  close_blocks <- dist[,i] < walk_dist
  lightrail_stops_latlon$walk_pop[i] <-sum(blocklist@data[close_blocks,'population'],na.rm=TRUE)
}
blocklist$dist2rail <- rep(0,n_blocks)
blocklist$dist2rail <- apply(dist,1,min)
sum(blocklist@data[blocklist$dist2rail < walk_dist,'population'],na.rm=TRUE)

lightrail_df <- data.frame(lightrail_stops_latlon)
lightrail_df$stop_color <- rep('red', n_lightrailstops)
lightrail_df$stop_color[lightrail_df$LINE == 'B'] <- 'blue'
lightrail_df$stop_color[lightrail_df$LINE == 'O'] <- 'orange'
lightrail_df$stop_color[lightrail_df$LINE == 'G'] <- 'green'
lightrail_df$stop_color[lightrail_df$LINE == 'Y'] <- "yellow"
lightrail_df$stop_color[lightrail_df$LINE == 'GY'] <- "yellow"
lightrail_df$stop_color[lightrail_df$LINE == 'GOY'] <- "yellow"
lightrail_df$stop_color[lightrail_df$LINE == 'GO'] <- "yellow"
lightrail_df$stop_color[lightrail_df$LINE == 'BR'] <- "blue"
lightrail_df$stop_color[lightrail_df$LINE == 'BGR'] <- "blue"

library('leaflet')

lighrail_map<-leaflet(data=lightrail_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lightrail_df$coords.x1, lightrail_df$coords.x2,
                 fillOpacity = 0.8,
                 radius= lightrail_df$walk_pop/800+0.5,
                 color = lightrail_df$stop_color,
                 popup = paste0("Station Name #: ", lightrail_df$STATION, "<br>", "# of people living in 1/2 mile walk : ", lightrail_df$walk_pop))

lighrail_map      