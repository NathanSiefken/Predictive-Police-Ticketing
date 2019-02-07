## Data Loading and Setup

#We will utilize and load several packages from CRAN to assist with our analysis. These will be automatically downloaded and installed. 

# This code chunk simply makes sure that all the libraries used here are installed. 
packages <- c("knitr","dplyr",  "tidyr", "caret", "ggplot2", "caret",
"plotly","lubridate","leaflet", "stringr","rpart.plot", "rpart")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
install.packages(missing_pkgs)
}

# loading libraries
library(tidyr) 
library(dplyr)
library(ggplot2)
library(lubridate) # for working with dates
library(plotly) # for interactive plots
library(janitor)
library(leaflet) # Geomapping
library(colorRamps)
library(proj4)
library(validate)
library(stringr)
library(rpart)
library(rpart.plot)
library(caret)
library(knitr)

#Load data
parking <- read.csv("parking-citations.csv", stringsAsFactors = FALSE)

str(parking)

# Naming our data set and only keeping the variables we are going to use
FTP <- parking[-1, -4:-12, -15]
# Now we will limit the data set to the last year
FTP <- FTP %>% filter(Issue.Date >= "2018-12-23")

write.csv(FTP,
          "FTP.csv", na = "", row.names=FALSE)

FTP<- read.csv("FTP.csv", stringsAsFactors = FALSE)


## Cleaning our Data

#Removing excess information time stamp of "T00:00:00"
FTP$Issue.Date <- sub("T.*", "", FTP$Issue.Date)

#We will use some string processing techniques to clean up our Issue.Time column.

#Now to put our time into a format that we can use
FTP$Issue.time<-sub("(\\d*)(\\d{2})", "\\1:\\2", FTP$Issue.time) # put in delimitter
# The single digit strings were missed, so this code will convert them
FTP$Issue.time <- str_replace(FTP$Issue.time, "^([0-9])$",":0\\1")
# Now we will need to pad our times with a 0 before the ":" for all data that was less than 1:00
FTP$Issue.time <- sprintf("%04s", FTP$Issue.time)

#We will remove these cordinates with 99999
FTP<- FTP %>%
  filter(Latitude != 99999) 


#The following code converts the coordinates from US feet to Longitude and Latitude coordinates. Then we remove the coordinate columns that are in US feet.
#Create projection element to convert from US Feet coordinates to normal lat lon
pj <- "+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 no_defs"

#Add converted latitude longitude to FTP data frame
FTP<- cbind(FTP, data.frame(project(data.frame(FTP$Latitude, FTP$Longitude), proj = pj, inverse = TRUE)))
str(FTP)
FTP <- FTP[-9:-10] #This removes the Latitude and Longitude  in Feet from our table
names(FTP)[c(9, 10)] <- c('Longitude', 'Latitude') #Rename column names of converted 


#combined the date and time into POSIXlt (time format)
FTP$Date <- as.POSIXlt(paste(FTP$Issue.Date, FTP$Issue.time), format="%Y-%m-%d %H:%M")
#Seperate the days of the week tickets where given and place it into a column
FTP$Weekdays <- weekdays(FTP$Date)
#Seperate the Hours of the day tickets where given and place it into a column
FTP$Hour <- FTP$Date$hour


# NAs that we need to be eliminated. 
FTP <- na.omit(FTP)# Very few for this many observation (less than 1%)
FTP <- FTP[-11] #We will remove our date column because we are
# not able to preform some calculations when a column is in POSIXlt format.
# We have no further use for it, we shall remove it.


##Exploratory Data Analysis section:
# Calculates Revenue
revenue <- FTP %>% summarize(Revenue = sum(Fine.amount))
revenue %>% knitr::kable()



#Filter top 10 Violations
TopViolations <- FTP %>% 
group_by(Violation.Description) %>% 
tally() %>% 
arrange(-n) %>% 
head(10)

TopViolations %>% knitr::kable()


#Filter the top 10 violations
TopViolationsLastYears <- FTP %>% 
  filter(Violation.Description %in%
           TopViolations$Violation.Description)
# Create graph top 10 Violations for the past month.
Graph <- ggplot(TopViolationsLastYears, aes(Issue.Date)) + 
  geom_bar(aes(fill=Violation.Description), stat='count')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Plot the data
Graph


#TThis is a scatter plot for the number of tickets given each day
DailyParkingViolation <- FTP %>%
  group_by(Issue.Date) %>%
  tally() %>%
  ggplot(aes(x=Issue.Date, y=n)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

DailyParkingViolation #Print 

# Table our weekdays
table(FTP$Weekday) %>% knitr::kable()



#Create data frame for weekday counts
WeekdayCounts = as.data.frame(table(FTP$Weekday))

ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

#abel the x and y axis and put the days of the week in order.

WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE,
                            levels=c("Sunday","Monday", "Tuesday","Wednesday",
                                     "Thursday", "Friday","Saturday")) 
#We can change the Var1 variable to be an ordered factor variable
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) +
  xlab("Day of the Week") + ylab("Total Ticket  Given Out")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# This will create our table
table(FTP$Weekday, FTP$Hour)
#We will save this as a data frame 
DayHourCounts = as.data.frame(table(FTP$Weekday, FTP$Hour))
# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
# Create out plot:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))

# Fix the order of the days and add color
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1,
color=Var1), size=2)

#make a distinction between the weekdays and the weekends.
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") |
                              (DayHourCounts$Var1 == "Saturday"),
                            "Weekend", "Weekday")
# Re-do our graph, this time coloring by Type:

ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + 
  geom_line(aes(group=Var1,color=Type), size=2,alpha=0.5) 


# Fix the order of the days:
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE,
                            levels=c("Monday", "Tuesday", "Wednesday",
                                     "Thursday", "Friday", "Saturday", "Sunday"))
#Create our first heat map and Change the label on the legend, and get rid of the y-label:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(name="Total Tickets Given") + 
  theme(axis.title.y = element_blank())

# Heat map with red and white color code
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(name="Total Tickets", low="white", high="red") +
  theme(axis.title.y = element_blank())

# Machine Learning Methods

#Rename variables for better interpretation 
DayHourCounts <- setNames(DayHourCounts, c("Weekday","Hour","Frequency",
                                           "HourAsNumber", "Weekday/Weekend"))
#Spliting our Data into Train and Test set
set.seed(21)
test_index <- createDataPartition(y = DayHourCounts$Frequency, times = 1,
                                  p = 0.2, list = FALSE)
my_train <- DayHourCounts[-test_index,]
test <- DayHourCounts[test_index,]


#Create function the calculates RMSE
RMSE <- function(true_Frequency, predicted_Frequency){
  sqrt(mean((true_Frequency - predicted_Frequency)^2))
}


#Baseline Model

# Find our MU
mu_hat <- mean(my_train$Frequency)
mu_hat 

#Create our Model
Baseline_rmse <- RMSE(test$Frequency, mu_hat)
Baseline_rmse 

#Testing our Basline model with another number other than the MU
predictions <- rep(1000, nrow(test))
NotAsGood <- RMSE(test$Frequency, predictions)
NotAsGood  

#Add our Basline Model to our Results
rmse_results <- data_frame(method = "Baseline", RMSE = Baseline_rmse)
rmse_results %>% knitr::kable()


#Linear Regression

#Create our Linear Regression Model
LR = lm(Frequency ~ Weekday + Hour, data=my_train) 


# Test our model on our predictions
LR_pred <- predict(LR, newdata=test)

LR_rmse <- RMSE(test$Frequency, LR_pred)
LR_rmse 

# Another way of calculating our RMSE
LR_sse <- sum((LR_pred- test$Frequency)^2)
LR_sse 
RMSE <- sqrt(LR_sse/nrow(test))
RMSE 

# Adds our result to the resluts table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Linear Regression Model",  
                                     RMSE = LR_rmse ))
rmse_results %>% knitr::kable()


##Regression Tree
# Create our Cart Model
Tree = rpart(Frequency~ Weekday + Hour, data=my_train) 
prp(Tree) # Print Tree

Tree_pred = predict(Tree, newdata=test) # Predit Tree
Tree_rmse <- RMSE(test$Frequency, Tree_pred)
Tree_rmse 

#Cross Validation
plotcp(Tree) # We can graph the reduction in error with different cp

#Create a tree with 5 terminal nodes
Tree1 <- rpart(Frequency~ Weekday + Hour, data=my_train, 
control = list(cp = .021))
prp(Tree1) # Print Tree 

Tree_pred1 = predict(Tree1, newdata=test) # Test our Predictions for out 5 leaf tree
Tree_rmse1 <- RMSE(test$Frequency, Tree_pred1) # Create our RMSE
Tree_rmse1 # Print RMSE for leaf tree

# The following code will bind all the results together
rmse_results <- bind_rows(rmse_results,
data_frame(method="Regression Tree Model",  
RMSE = Tree_rmse ))
rmse_results %>% knitr::kable()



