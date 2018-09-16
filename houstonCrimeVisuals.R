# Data Science for Social Good - Pinpointing Crime Hotspots in Houston
# Authors:
# Kevin Peng - Department of Bioengineering
# Jack Duryea - Department of Computer Science
# David Gan - Department of Computer Science
# Emre Yurtbay - Department of Statistics - Corresponding Author

----------------------------------------------------------------------------
# crimedata is a CSV file of 2 months of HPD data from 2018
crimedata <- read.csv(file.choose()) 

# Import Statements
# ggplot2 was used for the non-spatial of the visualizations
# ggmap was used for the spatial data visualization
library(ggplot2)
library(ggmap)

# ggmap credit to authors David Kahle and Hadley Wickham. Their ionstructional pdfs were instrumental
# in helping us create the spatial visualizations. For more information, visit:
# https://cran.r-project.org/web/packages/ggmap/ggmap.pdf
#------------------------------------------------------------------------------

# What kinds of crime are occuring in Houston in 2018?
table(crimedata$Offense)
ggplot(crimedata,aes(crimedata$Offense)) + geom_bar(aes(fill = crimedata$Offense)) +
  ggtitle("Count of Crimes by Type in Houston (2018)") + xlab("Type of Offense") +
  ylab("Count of Offense") + scale_fill_brewer(name="Crime Type", palette = "Blues")

# --------------------------------------------------------------------------------------------
# At what kinds of locations are crimes happening in Houston about a decade ago?
crime <- ggmap::crime

# Note : The ggmap crime dataset is a collection of crimes commited in the city of Houston in 2010
# that corresponds with the data set we have for 2018

# data frame for simplicity's sake
premiseTypeData <- as.data.frame(sort(table(crime$location)))

# Subset the data based on the 9 most common crime locations in houstons
premiseTypeData <- subset(crime,location == "grocery / supermarket" |
                            location == "commercial parking lot / garage" |
                            location == "restaurant / cafeteria parking lot" |
                            location == "department / discount store" |
                            location == "driveway" |
                            location == "road / street / sidewalk" |
                            location == "apartment parking lot" |
                            location == "residence / house" |
                            location == "apartment")

# final ggplot
ggplot(premiseTypeData,aes(premiseTypeData$location)) + 
  geom_bar(aes(fill = premiseTypeData$location)) +
  ggtitle("Count of Crimes by Location in Houston (2010)") + 
  xlab("Location") +
  ylab("Count of Location") + 
  scale_fill_brewer(name="Location", palette = "Blues") + 
  scale_x_discrete(labels=c("Apmt","Apmt PL","PL/Garage","Store",
                            "Driveway","Grocery/Supermarket","Residence/House",
                            "Restaurant PL","Street"))


# ---------------------------------------------------------------------------------------------
# What days of the week are these crimes occuring?
ggplot(data = crime,
       mapping = aes(crime$day))+
  geom_bar(aes(fill = crime$day))+
  ggtitle("Count of Crimes by Weekday in Houston (2010)")+
  xlab("Weekday")+
  ylab("Count of Crimes")+
  scale_fill_brewer(name = "Weekday", palette = "Blues")


# --------------------------------------------------------------------------------------------
#2010 Mapping Violent Crimes 

# Our dataset contains theft, burglary, auto-theft, murder, rape, robbery, aggravated assault
# We are most interested in mapping violent crimes, which are robbery, murder, rape, and assault
violent_crimes <- subset(crime,offense != "auto theft" & offense != "theft" & offense != "burglary")
violent_crimes$offense <- factor(violent_crimes$offense,
                                 levels = c("robbery", "aggravated assault", "rape", "murder"))

# We are limiting our scope to downtown Houston for the mapping analysis
violent_crimes <- subset(violent_crimes,
                         -95.39681 <= lon & lon <= -95.34188 & 29.73631 <= lat & lat <= 29.78400)


# Qmap is a function from the ggmap that we are using to build maps
# Mapping utility comes from the Google API
HoustonMap <- qmap("houston", zoom = 14, color = "bw")

# Map each violent crime that occured in downtown Houston in the given timeframe 
# from the dataset
HoustonMap +
  geom_point(aes(x = lon, y = lat, colour = offense, size = offense),
             data = violent_crimes)

# Creates a map similar to the point map above, except it splits the map up into sectors
HoustonMap +
  stat_bin2d(
    aes(x = lon, y = lat, colour = offense, fill = offense),
    size = .5, bins = 30, alpha = 1/2,
    data = violent_crimes
  )

# A density plot of violent crime in downtown Houston 
houston <- get_map("houston", zoom = 14)
HoustonMap +
  stat_density2d(
    aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = violent_crimes,
    geom = "polygon"
  )
overlay <- stat_density2d(
  aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
  bins = 4, geom = "polygon",
  data = violent_crimes
)

# A density map of violent crime in downtown houston
# Each map is broken down by weekday
houston <- get_map(location = "houston", zoom = 14, color = "bw",
                   source = "osm")
HoustonMap <- ggmap(houston, base_layer = ggplot(aes(x = lon, y = lat),
                                                 data = violent_crimes))
HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 bins = 5, geom = "polygon",
                 data = violent_crimes) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ day)

# -------------------------------------------------------------------------------------------
# Density plot of all crimes commiteed
all_crimes <- crime
houston <- get_map("houston", zoom = 13)
HoustonMap <- qmap("houston", zoom = 13, color = "bw")
HoustonMap +
  stat_density2d(
    aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 6, data = all_crimes,
    geom = "polygon"
  )
overlay <- stat_density2d(
  aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
  bins = 6, geom = "polygon",
  data = all_crimes)

# ---------------------------------------------------------------------------------------------
# Denisty plot for nonviolent crime in downtown houston
nv_crimes <- subset(crime,offense != "robbery" & 
                      offense != "aggravated assault" & 
                      offense != "rape" & 
                      offense != "murder")
nv_crimes$offense <- factor(nv_crimes$offense,
                            levels = c("auto theft", "theft", "burglary"))
nv_crimes <- subset(violent_crimes,
                         -95.39681 <= lon & lon <= -95.34188 & 29.73631 <= lat & lat <= 29.78400)


# Denisty plot
HoustonMap <- qmap("houston", zoom = 14, color = "bw")
HoustonMap +
  stat_density2d(
    aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 3, data = violent_crimes,
    geom = "polygon"
  )
overlay <- stat_density2d(
  aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
  bins = 3, geom = "polygon",
  data = violent_crimes)


# ------------------------------------------------------------------------------------------------
# Mapping Violent Crime in 2018
violent_crimes <- subset(crimedata,Offense != "Auto Theft" &
                           Offense != "Theft" & 
                           Offense != "Burglary")

violent_crimes$Offense <- factor(violent_crimes$Offense,
                                 levels = c("Robbery", "Aggravated Assault", "Rape", "Murder"))
violent_crimes <- subset(violent_crimes,
                         -95.39681 <= X & X <= -95.34188 & 29.73631 <= Y & Y <= 29.78400)
HoustonMap <- qmap("houston", zoom = 14, color = "bw")

# Map each violent crime that occured in downtown Houston in the given timeframe 
# from the dataset
HoustonMap +
  geom_point(aes(x = X, y = Y, colour = Offense, size = Offense),
             data = violent_crimes)

# A Sector map for current data
HoustonMap +
  stat_bin2d(
    aes(x = X, y = Y, colour = Offense, fill = Offense),
    size = .5, bins = 30, alpha = 1/2,
    data = violent_crimes
  )

# Violent Crime Density Map
houston <- get_map("houston", zoom = 14)
HoustonMap +
  stat_density2d(
    aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = violent_crimes,
    geom = "polygon"
  )
overlay <- stat_density2d(
  aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
  bins = 4, geom = "polygon",
  data = violent_crimes)

# Density by Day 2018
houston <- get_map(location = "houston", zoom = 14, color = "bw",
                   source = "osm")
HoustonMap <- ggmap(houston, base_layer = ggplot(aes(x = X, y = Y),
                                                 data = violent_crimes))
HoustonMap +
  stat_density2d(aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                 bins = 5, geom = "polygon",
                 data = violent_crimes) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ Weekday_name)

# ------------------------------------------------------------------------------------------------
# ALL CRIMES - 2018
all_crimes <- crimedata
houston <- get_map("houston", zoom = 13)
HoustonMap <- qmap("houston", zoom = 13, color = "bw")
HoustonMap +
  stat_density2d(
    aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
    size = 2, bins = 6, data =all_crimes,
    geom = "polygon"
  )
overlay <- stat_density2d(
  aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
  bins = 6, geom = "polygon",
  data = all_crimes)

# Nonviolent CRIMES
nv_crimes <- subset(crimedata,
                    Offense != "Robbery" & Offense != "Aggravated Assault" & Offense != "Rape" & 
                      Offense != "Murder")
violent_crimes$Offense <- factor(violent_crimes$Offense,
                                 levels = c("Auto Theft", "Theft", "Burglary"))
violent_crimes <- subset(violent_crimes,
                         -95.39681 <= X & X <= -95.34188 & 29.73631 <= Y & Y <= 29.78400)
HoustonMap <- qmap("houston", zoom = 14, color = "bw")

# Density plot
HoustonMap +
  stat_density2d(
    aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
    size = 2, bins = 3, data = violent_crimes,
    geom = "polygon"
  )
overlay <- stat_density2d(
  aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
  bins = 3, geom = "polygon",
  data = violent_crimes)

#Density by Day
houston <- get_map(location = "houston", zoom = 14, color = "bw",
                   source = "osm")
HoustonMap <- ggmap(houston, base_layer = ggplot(aes(x = X, y = Y),
                                                 data = violent_crimes))
HoustonMap +
  stat_density2d(aes(x = X, y = Y, fill = ..level.., alpha = ..level..),
                 bins = 5, geom = "polygon",
                 data = violent_crimes) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ Weekday_name)
