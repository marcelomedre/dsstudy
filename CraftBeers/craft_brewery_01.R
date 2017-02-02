# CraftBeers dataset
# Exploring craft Brewery data
setwd("C:/Users/Marcelo/Desktop/DsStudy/CraftBeers")
library(dplyr)
library(data.table)
library(ggplot2)
library(rmarkdown)

df_beer <- data.table(read.csv("beers.csv", header = TRUE,
                               stringsAsFactors = FALSE))
df_brew <- data.table(read.csv("breweries.csv", header = TRUE,
                               stringsAsFactors = FALSE))
colnames(df_brew) <- c("brewery_id", "name", "city", "state")

str(df_beer)
str(df_brew)
summary(df_beer)
summary(df_brew)

head(df_brew)
head(df_beer)

full_beer_data <- merge(df_beer, df_brew, by = "brewery_id")

head(full_beer_data)
names(full_beer_data)

colnames(full_beer_data) <- c("brewery_id", "X", "abv", "ibu", "id", "beer_name",
                              "style", "ounces", "brewery_name", "city", "state")
# Dropping repeated columns
full_beer_data = full_beer_data[,3:11]

# transforming ABV into %
full_beer_data$abv = full_beer_data$abv*100

head(full_beer_data)

# Number of breweries x State
breweryxstate <- full_beer_data %>%
        group_by(brewery_name)%>%
        group_by(state)%>%
        summarise(Freq = n())

ggplot(data = breweryxstate, aes(x = reorder(state, -Freq), y = Freq)) +
       geom_bar(stat="identity") +
        labs(x = "State", y = "Brewery Freq") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        geom_hline(yintercept=mean(breweryxstate$Freq), col = "red")

# Number of breweries x State
breweryxcity <- full_beer_data %>%
        group_by(brewery_name) %>%
        group_by(city) %>%
        summarise(Freq = n())

# subsetting for cities with more than 10 breweries

breweryxcity = breweryxcity[which(breweryxcity[,2]>10),]

ggplot(data = breweryxcity, aes(x = reorder(city, -Freq), y = Freq)) +
        geom_bar(stat="identity", col = "green") +
        labs(x = "City", y = "Number of Breweries", title = "Cities with Most Breweries") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        geom_hline(yintercept=mean(breweryxcity$Freq), col = "red")        

# Beer with highest average alcohol content

abvxstyle <- full_beer_data %>%
        filter(!is.na(abv)) %>%
        group_by(abv)%>%
        group_by(style) %>%
        summarise(Mean = sum(abv)/n())

ggplot(data = abvxstyle, aes(x = reorder(style, -Mean), y = Mean)) +
        geom_bar(stat="identity", col = "green") +
        labs(x = "Style", y = "average % Alcohol", 
             title = "Beers with highest average alcohol") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        geom_hline(yintercept=mean(abvxstyle$Mean), col = "red")
        
# Beers with more than 6% of alcohol content
abvxstyle = abvxstyle[which(abvxstyle[,2]>6),]

ggplot(data = abvxstyle, aes(x = reorder(style, -Mean), y = Mean)) +
        geom_bar(stat="identity", col = "green") +
        labs(x = "Style", y = "average % Alcohol", 
             title = "Beers with highest average alcohol") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        geom_hline(yintercept=mean(abvxstyle$Mean), col = "red")

# Most brewed Beer Styles
beerxstyle <- full_beer_data %>%
        group_by(beer_name)%>%
        group_by(style) %>%
        summarise(Sum = n())

beerxstyle = beerxstyle[which(beerxstyle[,2]>20),]

ggplot(data = beerxstyle, aes(x = reorder(style, -Sum), y = Sum)) +
        geom_bar(stat="identity", col = "grey") +
        labs(x = "Style", y = "# of Beers", 
             title = "Most brewed Beer Styles") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
# Outcomes
# - Colorado is the state with the highest number of breweries, followed by California and Minesota 
# - Grand Rapids, Portland and Chicago are the cities with the highest beer consume - NICE PLACES TO VISIT
# - English Barleywine and Quadrupel present the highest alcohol content in % volume
# - American IPA is by far the most brewed Beer Style