
#rm(list=ls())

library(data.table)
#library(lubridate)
library(maps)
library(maptools)
library(ggplot2)
library(devtools)
library(mapdata)
library(ggmap)
library(scales)
library(sp)
library(rgdal)
library(broom)
#library(grid) #cannot find package
library(GISTools) 
library(ggsn)
###################################################

dat <- read.csv("FHCGrowthMetaData.csv")
dat <- dat[order(dat$River), ]
# dat[, .N] 
# head(dat)
# summary(dat)


#get points
lat <- as.data.frame(as.numeric(by(dat$Lat, dat$River, mean)))
long<- as.data.frame(as.numeric(by(dat$Long, dat$River, mean)))
names <- as.data.frame(paste(unique(dat$River)))
cords <- cbind(lat, long, names)
names(cords)[names(cords)=="as.numeric(by(dat$Lat, dat$River, mean))"] <- "Lat"
names(cords)[names(cords)=="as.numeric(by(dat$Long, dat$River, mean))"] <- "Long"
names(cords)[names(cords)=="paste(unique(dat$River))"] <- "River"



lake.cords <- subset(cords, River %in% c("Lake Buchanan","Lake Travis",  "Lake Marion","Lake Moultrie", 
                                         "Lake Meredith", "Lake Palestine", "Lake Lyndon B. Johnson"))


river.cords <- subset(cords, River %in% c("Altamaha", "Apalachicola", "Cedar", "Des Moines", "Iowa", "James", 
                                          "JamesTidal", "Kansas", "Llano ", "LittlePeeDee", "Lumber", "Upper Mississippi", 
                                          "Lower Mississippi", "Lumber","NECFR", "Neuse", "North Raccoon",  "Satilla", "Susquehanna"))
river.cords$Lat[10]<- 30.699892 #move Llano cord over alittle so visible
river.cords$Long[10]<- -99.003381


# getting states layer
states <- map_data("state")
lagos_ne <- subset(states, region %in% c("connecticut", "illinois", "indiana","maine","massachusetts","michigan",
                                         "minnesota","missouri","new hampshire", "new jersey","new york","ohio","pennsylvania",
                                         "rhode island","vermont","wisconsin", "iowa", "arkansas", 
                                          "louisiana", "texas", "oklahoma", "kansas", "nebraska", "south dakota", "north dakota", 
                                         "west virginia", "virginia", "north carolina", "south carolina", "georgia", "florida", "alabama",
                                         "mississippi", "tennessee", "kentucky", "maryland"))

states <- subset(lagos_ne, region %in% c( "pennsylvania","kansas", "texas", "iowa" , 
                                          "mississippi", "florida", "south carolina", "north carolina", 
                                          "virginia" , "georgia"))

# #add in great lakes
# world <- map_data("worldHires")
# great_lakes <- subset(world, region %in% c("Great Lakes"))



##########################################RIVER DATA
filename <- "S:/FlatheadOtolithProject/Data/Rprojects/MetaAnalysis/SitesMap/hydrogm020_nt00015.tar.gz"
untar(filename)
usgs <- readOGR("hydrogl020.shp") #has river data
racoon<- subset(usgs, NAME=="North Racoon River")
#problem with des moines river
moines<- subset(usgs, NAME=="Des Moines River")
#code to fill in gaps in des moines river
iawater <- subset(usgs, STATE=="IA")
test <-subset(iawater, NAME=="N/A")
#something in na makes des moines connect
test<- subset(test, FEATURE=="Shoreline")
#connector is something in "shoreline"
# first gap between 38782 - 39630
test<- subset(test, FNODE_>=38782 )
test<- subset(test, FNODE_<=39630 )
test1 <- test[5:6,] 
#second gap between 39663- 39844 
test <-subset(iawater, NAME=="N/A")
test<- subset(test, FEATURE=="Shoreline")
test<- subset(test, FNODE_>=39663 )
test2 <- test[1:3,] 
#second gap between 39663- 39844 
moines <-rbind(moines, test2, test1)
#end code for des moines
apal <- subset(usgs, NAME=="Apalachicola River")
llano <- subset(usgs, NAME=="Llano River")
cedar <- subset(usgs, NAME=="Cedar River")
cedar <- subset(cedar, STATE=="IA") #remove different rivers with same name 

#susquehanna is being difficult
pa<- subset(usgs, STATE=="PA")
main<- subset(usgs, NAME=="Susquehanna River")
west<- subset(usgs, NAME=="West Branch Susquehanna River") #have to separely for NY to plot?
left<- subset(pa, FEATURE=="Left Bank") #part we want is "left bank"
# try to isolate pa to needed lats/longs
na <- subset(pa, NAME=="N/A") #southern main stem not labeled... 
#missing points are in na but contains stuff we dont want as well
na<- subset(na, FNODE_>="43052")
na <- na[8:13,]
na <- subset(na, FNODE_!="43441")
sus <-rbind( main, left, na, west)

#end susquehanna river points

iowa <- subset(usgs, NAME=="Iowa River")
peedee <- subset(usgs, NAME=="Little Pee Dee River")
alt <- subset(usgs, NAME=="Altamaha River")
kan <- subset(usgs, NAME=="Kansas River")
cape <- subset(usgs, NAME=="Northeast Cape Fear River") #does not include rest of cape fear river
james <- subset(usgs, NAME=="James River")
james <- subset(james, STATE=="VA") #remove different rivers with same name 
lum <-  subset(usgs, NAME=="Lumber River")
sat <- subset(usgs, NAME=="Satilla River")
ne <- subset(usgs, NAME=="Neuse River")
ne <- subset(ne, FNODE_!="51726")
#combine all rivers into one dataframe
rivers <- rbind(sus, moines, racoon, apal, llano, cedar,  iowa, peedee, alt, kan, cape, james, lum, sat, ne, west)
rivers <- fortify(rivers, region="NAME") #creates dataframe



#still need Mississippi River
#lines.rivers file HAS Mississippi data but only for MN!?
fileName <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_rivers_lake_centerlines.zip"
temp <- tempfile()
download.file(fileName, temp, mode="wb")
unzip(temp)
shapeData <- readOGR("ne_50m_rivers_lake_centerlines.shp")
unlink(c(temp, "ne_*"))
#convert to dataframe
shapeData@data$id <- rownames(shapeData@data)
watershedPoints <- fortify(shapeData, region = "id")
watershedDF <- merge(watershedPoints, shapeData@data, by = "id")
# Now just subset the data to include the rivers that I want to plot:
miss <- subset(watershedDF, name =="Mississippi")





########################################## LAKE DATA
usgs2 <- readOGR("hydrogp020.shp") #has lake data! YAY!
travis<- subset(usgs2, NAME=="Lake Travis")
buch <-  subset(usgs2, NAME=="Lake Buchanan")
mar <-  subset(usgs2, NAME=="Lake Marion")
mar<-  subset(mar, STATE=="SC") #remove diff lakes with same name
moul<-  subset(usgs2, NAME=="Lake Moultrie")
pal <-  subset(usgs2, NAME=="Lake Palestine")
john <-  subset(usgs2, NAME=="Lake Lyndon B. Johnson")
mer <- subset(usgs2, NAME=="Lake Meredith")
lakes <- rbind ( travis, buch, mar, moul, pal, john, mer)
lakes <- fortify(lakes, region="NAME") #will not fortify, is this nec tho?


# Add transparency to points
source('AddTransFunction.R') #add addtrans (color name, # from 1 to 255 for amount)
# number determines how translucent

######################################################## CREATE SCALE BAR & North arrow

create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}


create_orientation_arrow <- function(scale_bar, length, distance = 1, dist_units = "km"){
  lon <- scale_bar$rectangle2[1,1]
  lat <- scale_bar$rectangle2[1,2]
  
  # Bottom point of the arrow
  beg_point <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist_units, model = "WGS84")
  lon <- beg_point[1,"long"]
  lat <- beg_point[1,"lat"]
  
  # Let us create the endpoint
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist_units, model = "WGS84")
  
  left_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 225, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  right_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 135, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  res <- rbind(
    cbind(x = lon, y = lat, xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = left_arrow[1,"long"], y = left_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = right_arrow[1,"long"], y = right_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]))
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1,"lat"])/2)
  
  return(list(res = res, coords_n = coords_n))
}

scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}
#############end scale bar

north.arrow = function(x, y, h,lab="North",lab.pos="below") {
  polygon(c(x, x, x + h/2), c(y - (1.5*h), y, y - (1 + sqrt(3)/2) * h), col = "black", border = NA)
  polygon(c(x, x + h/2, x, x - h/2), c(y - (1.5*h), y - (1 + sqrt(3)/2) * h, y, y - (1 + sqrt(3)/2) * h))
  if(lab.pos=="below") text(x, y-(2.5*h), lab, adj = c(0.5, 0), cex = 1)
  else text(x, y+(0.25*h), lab, adj = c(0.5, 0), cex = 1.5)
}


#################
plot(c(146.5,147),c(-19,-19.5), type="n",asp=1)
north.arrow(146.65,-19.2,0.05)
north.arrow(146.85,-19.2,0.05, lab="N", lab.pos="above")
#test
usa_map <- map_data("state")
P <- ggplot() + geom_polygon(data = usa_map, aes(x = long, y = lat, group = group)) + coord_map()
P + scale_bar(lon = -130, lat = 26, distance_lon = 500, distance_lat = 100, distance_legend = 200, dist_unit = "km",
              arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)

  P  + scale_bar(lon = -130, lat = 26, 
                 distance_lon = 100, distance_lat = 20, 
                 distance_legend = 40, dist_unit = "km", 
                 arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)

P + north.arrow(146.85,-19.2,0.05, len=100, lab="N")

P+ scalebar(location="bottomright",y.min=21.3755, y.max=21.3715, 
            x.min=-47.865, x.max=-47.869, dist=.1, dd2km= TRUE, model='WGS84',
            st.dist=.04)


P <- ggplot() + geom_polygon(data = usa_map, aes(x = long, y = lat, group = group)) + coord_map()
north2(P, x = 0.8, y = 0.35, scale = 0.1, symbol = 5)


############################################################
# Start actual mapping
p1<- ggplot(data = lagos_ne) + coord_map() +
  geom_polygon(aes(x = long, y = lat,  group = group), fill = "white", color = "grey10", alpha=1) +
  geom_polygon(data=states, aes(x = long, y = lat, group=group), color="grey10", fill="grey80", size=1) +
  geom_polygon(data=rivers, aes(x = long, y = lat, group=group), color="steelblue2", fill="steelblue2", size=1.5) +
  geom_path(data=miss,  aes(x = long, y = lat, group = group), color = "steelblue2",  size=1.5)+
  #geom_polygon(data=lakes, aes(x = long, y = lat, group=group), color="royalblue3", fill="royalblue3", size=3) +
  geom_point( data=river.cords, aes(x=Long, y =Lat), fill= "steelblue2", color= "black", 
              shape=21, size=4, stroke=1.5) +#adds points for rivers
  geom_point( data=lake.cords, aes(x=Long, y =Lat), fill= "royalblue3", color="black", 
               shape=21, size=4, stroke=1.5) +#adds points for lakes
  labs(x="", y="", title="") + #labels +
theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
      axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
      panel.border = element_blank(),
      legend.text=element_text(size=8),
      legend.title=element_text(size=8),
      panel.background = element_blank(),
      plot.margin=unit(c(-15,-15,-15,-15),"mm"),
      plot.title = element_text(lineheight=.8, face="bold", vjust=0.5, hjust=0.5, size=9)) + # make title bold and add space
  
 scale_bar(lon = -75, lat =30, 
            distance_lon = 300, distance_lat = 100, distance_legend = 150, 
            dist_unit = "km", arrow_length = 200, arrow_distance = 200, arrow_north_size = 6, orientation = FALSE)  #remove orientation if you want arrow included

  
 
    #geom_text(data=lake.cords, aes(Long , Lat , label = River), size=4)
  
  
  #geom_point(aes(x=nhd_long, y=nhd_lat, color=dat$depth), data=dat, alpha=1, size=0.1, color="grey20") + # to get outline
  #geom_point(aes(x=nhd_long, y=nhd_lat, color=dat$depth), data=dat, alpha=0.5, size=0.05) +
  #scale_colour_gradient2("Depth (m)", low="red", mid="blue", high="green",breaks=breaks, labels=labels)
  # scale_colour_gradient2("Depth (m)", space="Lab", breaks=breaks, labels=labels)
  # scale_colour_gradientn("Depth (m)", colours=c( "#0000FF","#FF0000"), breaks=breaks, labels=labels)
  # scale_colour_gradientn("Depth (m)", colours=c( "#f9f3c2","#660000"), breaks=breaks)
  
p1 #plot that code

#add north arrow to already plotted map
north2(p1, x =0.87, y = 0.29, scale = 0.08, symbol = 1) 


#create map with different river colors
p2 <- ggplot(data = lagos_ne) + coord_map() +
  geom_polygon(aes(x = long, y = lat,  group = group), fill = "grey80", color = "grey10", alpha=1) +
  geom_polygon(data=ne, aes(x = long, y = lat, group=group), color="slateblue3", fill="slateblue3") +
  geom_polygon(data=iowa, aes(x = long, y = lat, group=group), color="purple", fill="purple") +
  geom_polygon(data=sus, aes(x = long, y = lat, group=group), color="blue", fill="blue") +
  geom_polygon(data=alt, aes(x = long, y = lat, group=group), color="green", fill="green") +
  geom_polygon(data=sat, aes(x = long, y = lat, group=group), color="tan2", fill="tan2")+
  geom_polygon(data=james, aes(x = long, y = lat, group=group), color="cyan", fill="cyan") +
  geom_polygon(data=llano, aes(x = long, y = lat, group=group), color="mediumblue", fill="mediumblue") +
  geom_polygon(data=cedar, aes(x = long, y = lat, group=group), color="darkorange3", fill="darkorange3") +
  geom_polygon(data=cape, aes(x = long, y = lat, group=group), color="yellow", fill="yellow") +
  geom_polygon(data=peedee, aes(x = long, y = lat, group=group), color="deeppink2", fill="deeppink2") +
  geom_polygon(data=lum, aes(x = long, y = lat, group=group), color="seagreen4", fill="seagreen4") +
  geom_polygon(data=apal, aes(x = long, y = lat, group=group), color="magenta2", fill="magenta2") +
  geom_polygon(data=racoon, aes(x = long, y = lat, group=group), color="royalblue3", fill="royalblue3") +
  geom_polygon(data=moines, aes(x = long, y = lat, group=group), color="gold1", fill="gold1") +
  geom_polygon(data=kan, aes(x = long, y = lat, group=group), color="red", fill="red") +
  geom_path(data=miss,  aes(x = long, y = lat, group = group), color = "green2",size=1)+ 
  #this add MS River #path ~polygon, data only works with path..?
  labs(x="", y="") + 
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        panel.border = element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        panel.background = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm")) 
  

p2


###################
#figure out whats wrong with the susquehanna
ia <- subset(states, region=="iowa") 

iawater <- subset(usgs, STATE=="IA")

racoon<- subset(usgs, NAME=="North Racoon River")
moines <- subset(usgs, NAME=="Des Moines River")
iawater <- subset(usgs, STATE=="IA")
test <-subset(iawater, NAME=="N/A")
#something in na makes des moines connect
test<- subset(test, FEATURE=="Shoreline")
#connector is something in "shoreline"
# first gap between 38782 - 39630
test<- subset(test, FNODE_>=38782 )
test<- subset(test, FNODE_<=39630 )
test1 <- test[5:6,] 
#second gap between 39663- 39844 
test<- subset(test, FNODE_>=39663 )
test2 <- test[1:4,] #second gap between 39663- 39844 
rivers2 <-rbind(racoon, moines, test2, test1)























































p2 <- ggplot(data = ia) + coord_map() +
  geom_polygon(aes(x = long, y = lat,  group = group), fill = "grey80", color = "grey10", alpha=1) +
  geom_polygon(data=rivers1, aes(x = long, y = lat, group=group), color="slateblue3", fill="slateblue3")
p2