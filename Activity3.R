# install.packages(c("dplyr", "lubridate", "ggplot2", "gridExtra"))
library(gridExtra)
library(dplyr)
library(lubridate)
library(ggplot2)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
colnames(datCO2)[4] <- "CO2" 
datCO2$Entity <- as.factor(datCO2$Entity) 
name.Ent <- levels(datCO2$Entity) #there are 243 entries

# Plotting with base R
plot(datCO2$Year, datCO2$CO2)
US <- datCO2[datCO2$Entity == "United States",]
ME <- datCO2[datCO2$Entity == "Mexico",]
plot(US$Year, US$CO2, type = "b", pch = 19, 
     ylab = "Annual fossil fuel emissions (tons CO2)", xlab = "Year", yaxt = "n", ylim = c(0,6200000000))
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
points(ME$Year, 
       ME$CO2, 
       type = "b", 
       pch = 19, 
       col= "darkgoldenrod3")
legend("topleft",
       c("United States", "Mexico"),
       col=c("black", "darkgoldenrod3"),
       pch=19, bty= "n")

NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(NorthA, aes(x=Year, y=CO2, color=Entity)) + geom_point()+ geom_line() + theme_classic() +
  scale_color_manual(values = c("#7FB3D555","#34495E55", "#E7B80055"))

# Classwork Prompt 1
climate <- read.csv("/cloud/project/activity03/climate-change.csv")
climate$Day <- ymd(climate$Day)
climate$Year <- year(climate$Day)

# Convert the 'Entity' column to a factor
climate$Entity <- as.factor(climate$Entity)
climate.Ent <- levels(climate$Entity)

# Filter for only the Northern Hemisphere and Southern Hemisphere
Northern <- subset(climate, Entity == "Northern Hemisphere")
Southern <- subset(climate, Entity == "Southern Hemisphere")
plot(Northern$Year, Northern$temperature_anomaly, type = "b", pch = 19, col = "blue", 
     ylab = "Temperature Anomaly", xlab = "Year")
lines(Southern$Year, Southern$temperature_anomaly, type = "b", pch = 19, col = "red")
legend("topleft", legend = c("Northern Hemisphere", "Southern Hemisphere"), col = c("blue", "red"), pch = 19, bty = "n")


ggplot(climate, aes(x = Day, y = temperature_anomaly, color = Entity)) +
  geom_line() +
  labs(title = "Temperature Anomalies over Time by Entity",
       x = "Date", 
       y = "Temperature Anomaly") +
  theme_minimal()

# Classwork Prompt 2
Country_filtered <- datCO2%>%
  filter(Entity %in% c('United States', 'Mexico', 'Canada'))

Country_cumulative <- Country_filtered %>%
  group_by(Entity) %>%  
  arrange(Year) %>%  
  mutate(cumulative_emissions = cumsum(CO2))

ggplot(Country_cumulative, aes(x = Year, y = cumulative_emissions, color = Entity)) +
  geom_line(size = 1) +  
  labs(title = "Cumulative CO2 Emissions Over Time",
       x = "Year",
       y = "Cumulative CO2 Emissions",
       color = "Country") +
  theme_minimal() 

# Homework question 1

ggplot(datCO2[datCO2$Entity %in% c("United Kingdom", "France"), ], aes(x = Year, y = CO2 / 1e9, color = Entity)) +
  geom_line(size = 1) +  
  labs(title = expression("Comparative Analysis of CO"[2]~"Emissions Over Time: UK and France"), 
       x = "Year",
       y = expression(CO[2] ~ " Emissions (Billions of Tonnes)"),
       caption = "Source: Our World in Data") + 
  scale_x_continuous(breaks = seq(min(datCO2$Year), max(datCO2$Year), by = 20)) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1))


# Homework question 2
# Plot World CO2 emissions 
p1 <- ggplot(datCO2[datCO2$Entity == 'World', ], aes(x = Year, y = CO2 / 1e9)) +
  geom_line(color = "#6ac5fe", size = 1) +  
  labs(title = expression(CO[2] ~ " Emissions Over Time"),
       x = "Year",
       y = expression(CO[2] ~ "Emissions (Billions of Tonnes)"),
       caption = "Source: Our World in Data") + 
  scale_x_continuous(breaks = seq(min(datCO2$Year), max(datCO2$Year), by = 20)) +  
  scale_y_continuous(limits = c(0, 40)) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1))

# Plot world air temperature anomalies
World_temp_anomalies <- climate %>%
  filter(Entity %in% "World") %>%
  group_by(Year) %>%
  summarize(anomlies = sum(temperature_anomaly, na.rm = TRUE))

p2 <- ggplot(World_temp_anomalies, aes(x = Year, y = anomlies)) +
  geom_line(color = "#6ac5fe", size = 1) +  
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") + 
  labs(title = "World Air Temperature Anomalies Over Time",
       x = "Year",
       y = "Air temperature anonmalies (Celsius)",
       caption = "Source: Our World in Data") + 
  scale_x_continuous(breaks = seq(min(datCO2$Year), max(datCO2$Year), by = 20)) +  
  scale_y_continuous(limits= c(-10,15)) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(p1, p2, ncol = 2)

# Homework question 3
# Website link of our world in data: https://ourworldindata.org/grapher/global-precipitation-anomaly
# Downloaded data related to global precipitation anomaly
precipitation <- read.csv("/cloud/project/activity03/global-precipitation-anomaly.csv")

ggplot(precipitation, aes(x = Year, y = Global.precipitation.anomaly)) +
  geom_line(color = "#FA5F55", size = 1) +  
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") +  
  labs(title = "Global precipitation anomaly",
       x = "Year",
       y = "Anomaly in inches",   
       caption = "Source: Our World in Data") + 
  scale_x_continuous(limits = c(1901, 2021), breaks = c(1901, 1920, 1940, 1960, 1980, 2000, 2021), expand = c(0, 0)) +  # Adjusted expand argument
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1))

land_use <- read.csv("/cloud/project/activity03/land-use-over-the-long-term.csv")

