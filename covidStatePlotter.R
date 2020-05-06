library(ggplot2)  # for creating graphs

#note! you need to specify a directory/folder on your computer where the output files will be stored
#I want my files output to ~/Documents/covid/ but yours can go anywhere
#TODO#2020-05-06 test to ensure directory exists 
setwd("~/Documents/covid/")

#download data from NY Times GitHub repository, https://github.com/nytimes/covid-19-data
#use the direct URL for the us-states.csv file: https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv
covidDataUrl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

#create a temporary directory
td <- tempdir()

#create a temporary placeholder file
tf <- tempfile(tmpdir=td, fileext=".csv")

#download the covid data into the tempfile
download.file(covidDataUrl, tf)

#read the downloaded CSV us-states.csv file
covid.states <- read.csv(tf, stringsAsFactors = FALSE)

#convert the date strings to Date objects
covid.states$date <- as.Date(covid.states$date)

#plot cumulative deaths by state
#p <- ggplot(covid.states, aes(date, deaths))
#allStates <- p + geom_bar(stat="identity") + facet_wrap(~ state, scales="free")

#you can create a list of ALL the states in the data set like this:
#states <- unique(covid.states$state)
#
#or, if you only want to produce plots for select states, you can create a list, like this:
#states <- c("Massachusetts", "Florida", "New York")
#
states <- unique(covid.states$state)

#this is the "dumb" way to do this, instead of the split-apply-combine route;
#but, it works, and that's what counts!
#
#loop through each state and do the necessary...
for(myState in states) {
  
  #calculate data just myState
  state.covid <- filter(covid.states, covid.states$state==myState)
  
  #calculate new deaths per day, as the data source only shows total deaths as of the date
  state.covid$newDeaths <- state.covid$deaths - lag(state.covid$deaths, n=1)
  
  #calculate the 3-day rolling mean of new Deaths, right-aligned
  state.covid$avg3days <- rollmean(state.covid$newDeaths, 3, na.pad=TRUE, align = "right")
  
  #calculate the 7-day rolling mean of new Deaths, right-aligned
  state.covid$avg7days <- rollmean(state.covid$newDeaths, 7, na.pad=TRUE, align = "right")
  
  #create a plot showing deaths per day as a bar, with a black line showing the 3-day rolling mean, and a blue line showing the 7-day rolling mean
  statePlot <- ggplot(state.covid, aes(date, newDeaths)) + 
    scale_x_date(date_breaks="week", date_labels="%b %d") +
    geom_bar(stat="identity", fill="#DDDDDD") + 
    geom_line(data=state.covid, aes(x=date, y=avg3days), color="#9999FF", size=2) + 
    geom_line(data=state.covid, aes(x=date, y=avg7days), color="#000099", size=4) + 
    #Create a label and include the state name and the latest date
    labs(title=paste("COVID-19 Deaths Per Day in ", myState, " as of ", format(max(state.covid$date), format="%B %d, %Y"), sep=""), 
         subtitle="Bars show deaths reported per day\nLight Blue Line shows 3-day rolling average\nDark Blue Line shows 7-day rolling average", 
         x=NULL,
         y="Number of Confirmed COVID-19 Deaths Reported per Day",
         caption="Data from State Departments of Public Health as gathered by the New York Times and downloaded from https://github.com/nytimes/covid-19-data") +
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20),
          plot.subtitle = element_text(lineheight=1, size=18),
          plot.caption = element_text(size=14),
          axis.text = element_text(size = 18))
  
  #save it to a PNG file
  outputFile <- paste(myState,"covid", "data", max(state.covid$date), "png", sep=".")
  png(outputFile, width=1100, height=850)
  #note that you could use the pdf() function instead of png() above; then, the width and height are specified as numbers of inches, e.g., pdf(outputFile, width=10, height=7.5)
  print(statePlot)
  dev.off()
}