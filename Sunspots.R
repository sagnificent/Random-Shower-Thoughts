install.packages("pacman")
pacman::p_load(pacman, rio)
sunspots_csv <- import("//Users//sagnikroy//Documents//My Works//R Projects//SunSpots//sunspot_data.csv")
head(sunspots_csv)
vec <- c()
for (i in 1818:2019){
  vec <- c(vec,sum(sunspots_csv$`Number of Sunspots`[sunspots_csv$Year==i]))
}
(years <- c(1818:2019))
sunspots_yearwise <- as.data.frame(cbind(years,vec))
head(sunspots_yearwise)
plot(sunspots_yearwise,
     pch = 19,         # Solid circle
     cex = 0.8,        # Make 150% size
     col = "#cc0080",  # Red
     main = "No. of sunspots in a year",
     xlab = "Year",
     ylab = "No. of Sunspots in that year")
months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
vec2 <- c()
for (i in 1:12){
  vec2 <- c(vec,sum(sunspots_csv$'Number of Sunspots'[sunspots_csv$Month==i]))
}
(sunspots_monthwise <- as.data.frame(cbind(months,vec2)))
# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L
