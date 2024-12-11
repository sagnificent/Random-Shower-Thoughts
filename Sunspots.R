library(dplyr)
install.packages("pacman")
pacman::p_load(pacman, rio)
sunspots_csv <- import("sunspot_data.csv")
head(sunspots_csv)
sunspots_csv$`Number of Sunspots`[sunspots_csv$`Number of Sunspots` == -1] <- 0 #converts all -1 values to 0 for summing
vec <- c()
for (i in 1818:2019){
  vec <- c(vec,sum(sunspots_csv$`Number of Sunspots`[sunspots_csv$Year==i]))
} #sums the no of sunspots in each year
(years <- c(1818:2019))
sunspots_yearwise <- as.data.frame(cbind(years,vec)) #creates a data frame containing year and the respective data
head(sunspots_yearwise)
monthly_totals <- sunspots_csv %>%
  group_by(Year, Month) %>%
  summarise(TotalSunspots = sum(`Number of Sunspots`, na.rm = TRUE))
head(monthly_totals)
plotmonthwise <- function(i){
  monthly_totals_year <- monthly_totals[monthly_totals$Year == i, ]
  plot(
    monthly_totals_year$Month, 
    monthly_totals_year$TotalSunspots,
    type = "b",  # Adds lines between points
    xlab = "Month",
    ylab = "Total Sunspots",
    main = paste("Total Sunspots per Month in", i)
  )
}
plot(sunspots_yearwise,
     pch = 19,         # Solid circle
     cex = 0.8,        # Make 150% size
     col = "#cc0080",  # Red
     main = "No. of sunspots in a year",
     xlab = "Year",
     ylab = "No. of Sunspots in that year") #creates a scatter plot of year and no of sunspots
model <- lm(sunspots_yearwise$vec~sunspots_yearwise$years)
abline(model, col = "blue", lwd = 2) #adds regression line to plot
par(mfrow = c(2, 3))
v <- c(1818,1819,1820,2017,2018,2019)
for (i in v){
  plotmonthwise(i)
}
par(mfrow=c(1,1))
rm(list=ls())
dev.off()
