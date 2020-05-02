+### ##  Description  ######
# R script for Webscraping and interactive Map ###
# Author : Arpita Dutta
# Date : 01-05-2020


# Acknowledgements :
#   Data pulled from following websites
#        1. https://www.distancelatlong.com/country/india
#        2. Goverment of India -MOH website  -https://mohfw.gov.in/


##This plot is in below link
#  https://arpitadu.github.io/Covid-19-Data-Visualization/.


## Load Libraries ###
library(stringr)
library(jsonlite)
library(rvest)
library(XML)
library(dplyr)
library(leaflet)

############## Web Sracpping ##################
url<-'https://mohfw.gov.in/' # Load the url

page<-read_html(url) #get html of the web page
covid_list <-page %>%
  html_nodes("table") %>% html_table(fill = TRUE) #get html of the table



covid_cases<-as.data.frame(covid_list) #change the table into data frame

head(covid_cases)



url<-'https://www.distancelatlong.com/country/india' # Load the url

page<-read_html(url) #get html of the web page
India_states <-page %>%
  html_nodes("table") %>% html_table(fill = TRUE) #get html of the table

India_states<-as.data.frame(India_states[[3]])


########### Data Cleaning ####################
India_states$States<-str_replace(India_states$States, " \\(.*\\)", "") ## remove the brackets after states name 

covid_cases<-covid_cases[c(1:32),]

covid_cases<-covid_cases %>% 
  rename(
    States= Name.of.State...UT) ##change the state names in lat- long file


##Fix the inconsistancies in the states name in both files 
India_states$States[India_states$States=="Andaman And Nicobar"]<-"Andaman and Nicobar Islands"
India_states$States[India_states$States=="Jammu And Kashmir"]<-"Jammu and Kashmir"
India_states$States[India_states$States=="Orissa"]<-"Odisha"
India_states$States[India_states$States=="Uttaranchal"]<-"Uttarakhand"



################## Merge table#######################

covid_India_stateswise<-merge(covid_cases,India_states, by= "States",all.x=TRUE)

## Manually Enter lattitude and longitude for three States which are missing the web scrapped table
covid_India_stateswise$Latitude[covid_India_stateswise$States=='Telengana'] <-18.1124
covid_India_stateswise$Longitude[covid_India_stateswise$States=='Telengana'] <- 79.0193

covid_India_stateswise$Latitude[covid_India_stateswise$States=='Ladakh'] <-34.209515
covid_India_stateswise$Longitude[covid_India_stateswise$States=='Ladakh'] <- 77.615112

covid_India_stateswise$Latitude[covid_India_stateswise$States=="Gujarat"] <-22.2587
covid_India_stateswise$Longitude[covid_India_stateswise$States=="Gujarat"] <-  71.1924

##preparing the columns for merging
covid_India_stateswise$Total.Confirmed.cases..Including.111.foreign.Nationals. <- paste("Total Confirmed Cases",covid_India_stateswise$Total.Confirmed.cases..Including.111.foreign.Nationals., sep=" - ")
covid_India_stateswise$Death <- paste("Total Death",covid_India_stateswise$Death, sep=" - ")

############## Plot the Interactive Map #########################

leaflet(covid_India_stateswise) %>% addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~paste(covid_India_stateswise$States, covid_India_stateswise$Total.Confirmed.cases..Including.111.foreign.Nationals., covid_India_stateswise$Death, sep="<br>"))
