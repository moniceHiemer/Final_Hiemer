#############Repository#################
getwd() 
setwd("/Users/monicahiemer/Desktop/CS710 -Data Viz")

#############Libs#################
library(countrycode)
library(ggplot2)
library(reshape2)
library(rlang)
library(dplyr)
library(colorspace)
library(viridis)
library(maps) 
library(mapproj)
library(tm)
library(maps)
library(tidyverse)
library(plotly)
library(sf)
library(albersusa)
library(cartogram)

#############Reading Data#################
data_2018 <- read.csv("Table_10_Offenses_Known_to_Law_Enforcement_by_State_by_Metropolitan_and_Nonmetropolitan_Counties_2018.csv")
data <- read.csv('Table_10_Offenses_Known_to_Law_Enforcement_by_State_by_Metropolitan_and_Nonmetropolitan_Counties_2019.csv')
us_map <- counties_sf()
county_pop <- read_csv("county_pop.csv")

#############Clean Data#################
#Remove Police Department/County in the names
stopwords <- c(' County Police Department', ' Police Department')
data$County <- removeWords(data$County,stopwords)
data$County <- removeNumbers(data$County)
#Run Twice Because of Difference Spacing
data$County <- removeWords(data$County,stopwords)
data$County <- removeNumbers(data$County)
#new column county and state to be able to match the column in both data sets
us_map$name<-as.character(us_map$name)
us_map[1873, 'name'] <- ''
us_map['name_state'] <- tolower(paste(us_map$name, us_map$state))
data['name_state'] <- tolower(paste(data$County, data$State_Clean))
data_2018['name_state'] <- tolower(paste(data_2018$County, data_2018$state2))
county_pop[1803, 'name_state'] <- ''
county_pop['name_state']<- tolower(county_pop$name_state)
#removing any trailing spaces to so the columns will match
data$name_state <- trimws(data$name_state)
data_2018$name_state <- trimws(data_2018$name_state)
us_map$name_state <- trimws(us_map$name_state)
#merging counties that are doubled
dups <- c()
for (i in 1:length(data$name_state)) {
  j = i+1
  if (data$name_state[i] == data$name_state[j]) {
    data$Violent.crime[i] <- data$Violent.crime[i] + data$Violent.crime[j]
    data$Robbery[i] <- data$Robbery[i] + data$Robbery[j]
    data$Property.crime[i] <- data$Property.crime[i] + data$Property.crime[j]
    data$Burglary[i] <- data$Burglary[i] + data$Burglary[j]
    data$Larceny..theft[i] <- data$Larceny..theft[i] + data$Larceny..theft[j]
    dups <- c(dups, j)
  }
  if (j == length(data$name_state)){
    break
  }
}
#Removing Duplicate Counties
data <-data[-dups,]
#Removing Columns that don't matter
data <- data[-c(1,5,6,8,12,13)]
data_2018 <- data_2018[-c(1,5,6,8,12,13)]
#######Which counties Don't Match#########
data$name_state[!(data$name_state %in% us_map$name_state)]
missing <-unique(us_map$name_state[!(us_map$name_state %in% data$name_state)])
#renameing rows 
us_map[872,'name'] <-"la porte"
us_map[1569,'name'] <-"la salle"
us_map[342,'name'] <-"butte-silver bow"
us_map[2793,'name'] <-"westchester public safety"
us_map[3005,'name'] <-"hartsville/trousdale" 
us_map[635,'name'] <-"salt lake county unified" 
#RERUN columns
us_map['name_state'] <- tolower(paste(us_map$name, us_map$state))
data['name_state'] <- tolower(paste(data$County, data$State_Clean))
data_2018['name_state'] <- tolower(paste(data_2018$County, data_2018$state2))
# RERUN removing any trailing spaces to so the columns will match
data$name_state <- trimws(data$name_state)
data_2018$name_state <- trimws(data_2018$name_state)
us_map$name_state <- trimws(us_map$name_state)
data$name_state[!(data$name_state %in% us_map$name_state)]

#Filter Missing Counties in 2018 data
data_2018<-data_2018[which(data_2018$name_state %in% missing),]
#############Merging Data 2019/2018 (countries are missing)#################
#Merge 2019 and 2018 data
data_2018 <- data_2018 %>% rename('State_Clean' = 'state2')
data <- rbind(data_2018, data)

county_pop<- county_pop%>% rename('POP_2015' = '2015 POPULATION')
#############Clean Again#################
#make columns integers
data$Larceny..theft <- as.integer(data$Larceny..theft)
#remove NA
data<-data %>% drop_na(Larceny..theft)

#############Adding a Quartile for Larceny for top theft counties#################
data['Quartile_Larceny..theft']<- ecdf(data$Larceny..theft)(data$Larceny..theft)

#############Merging Data#################
data_map<-left_join(us_map,data,by.x="name_state",by.y="name_state")
data_map<-arrange(data_map,state,name)
data_map$name_state[!(data_map$name_state %in% county_pop$name_state)]
county_pop[69, "name_state"]<-"aleutians west alaska" 
county_pop[71, "name_state"]<-"bethel alaska"  
county_pop[74, "name_state"]<-"dillingham alaska"
county_pop[77, "name_state"]<-"hoonah-angoon alaska"
county_pop[78, "name_state"]<-"juneau alaska"  
county_pop[87, "name_state"]<-"petersburg alaska" 
county_pop[84, "name_state"]<-"nome alaska" 
county_pop[88, "name_state"]<-"prince of wales-hyder alaska" 
county_pop[89, "name_state"]<-"sitka alaska" 
county_pop[91, "name_state"]<-"southeast fairbanks alaska" 
county_pop[92, "name_state"]<-"valdez-cordova alaska"
county_pop[93, "name_state"]<-"wade hampton alaska"
county_pop[94, "name_state"]<-"wrangell alaska"
county_pop[95, "name_state"]<-"yakutat alaska"
county_pop[96, "name_state"]<-"yukon-koyukuk alaska" 
county_pop[645, "name_state"]<-"la salle illinois" 
county_pop[743, "name_state"]<-"la porte indiana" 
county_pop[1645, "name_state"]<-"butte-silver bow montana" 
county_pop[1764, "name_state"]<-"carson city nevada" 
county_pop[1888, "name_state"]<-"westchester public safety new york" 
county_pop[2513, "name_state"]<-"hartsville/trousdale tennessee" 
county_pop[2795, "name_state"]<-"salt lake county unified utah" 
data_map$name_state[!(data_map$name_state %in% county_pop$name_state)]
data_map1 <- merge(data_map, county_pop, by.x = "name_state", by.y = "name_state")
data_map1["text_hover"]<-paste("State: ",data_map1$state,"\nCounty: ",data_map1$name,"\nLarceny Cases: ", data_map1$Larceny..theft,"\nPopulation 2015: ", data_map1$POP_2015) 
#############MAP#################
us_cont<- st_as_sf(data_map1)
us_cont<-st_transform(us_cont, crs = "ESRI:102003")
us_cont4 <- cartogram_cont(us_cont, "POP_2015", itermax = 4)


us_cont<-st_transform(us_cont4, crs = "ESRI:102003")

p<-us_cont %>% 
  st_simplify(TRUE, dTolerance = 5000) %>% 
  st_cast("MULTIPOLYGON")

#map 1
#plot_ly(p )%>%
#  add_sf(color = ~Quartile_Larceny..theft,
#                          split = ~name,
#                          span = I(1)) %>% 
#  layout(showlegend = FALSE, 
#         title = "\n\nLarceny and Population: USA")%>% 
#  add_sf(data = p, 
#        split = ~state, 
#        span = I(.7),
#        alpha = 0)%>%
#  add_sf(data=p,
#         split=~text_hover,
#         span=I(0), 
#         hoverinfo = "text", 
#         hoveron = "fills",
#         alpha =0)%>%
#  colorbar(title = "Percentile of State<br>Larceny Cases")

#map 2 
plot_ly(p )%>%
  add_sf(color = ~Quartile_Larceny..theft,
         split = ~name,
         span = I(1)) %>% 
  layout(showlegend = FALSE, 
         title = "\n\nLarceny and Population: USA")%>% 
  add_sf(data = p, 
         split = ~state, 
         span = I(.7),
         alpha = 0)%>%
  add_sf(data=p,
         color =~Quartile_Larceny..theft,
         split=~text_hover,
         span=I(0), 
         text = ~text_hover,
         hoverinfo = "text", 
         hoveron = "fills",
         alpha =0)%>%
  colorbar(title = "Percentile of State<br>Larceny Cases")#%>%
  colorbar(which = 2, title = " ")



# refrence graph 
plot_ly(data_map1)%>% add_sf(color = ~Quartile_Larceny..theft,
                             split = ~state,
                             span = I(1))%>%hide_legend()
#######ggplot map####Not interactive 
#ggplot()+geom_sf(data=p, aes(fill = Quartile_Larceny..theft))+
#  theme_void()+scale_fill_viridis(alpha = .8)

 
