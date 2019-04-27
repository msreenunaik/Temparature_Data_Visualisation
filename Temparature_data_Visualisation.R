library(magrittr)
library(ggplot2)
library(dplyr)
#=====================

data = read.csv("temperature.csv")
summary(data)

# There are NA's in some variable, so lets clean the data first.
# for that we use magrittr %>% to easy clean
# from here we also see the NA's in AverageTemperatureFahr and
# AverageTemperatureUncertaintyFahr
# lets clean this.

temperature = data %>%
  filter(!is.na(City)) %>%
  filter(!is.na(Country)) %>%
  filter(!is.na(AverageTemperatureFahr)) %>%
  filter(!is.na(AverageTemperatureUncertaintyFahr))

# data set removed the NA's from City and Country
# see at environment data observations

summary(temperature)

# in the day column, it has no other values besodes 1 so, let's get rid of it.

temperature = select(temperature, -(day))

# to check, use head dunction
# day column deleted from the dataset
head(temperature)

# let's start converting Fahrenheit to Celsius

temperature = temperature %>%
  mutate(AverageTemperatureCelsius = (AverageTemperatureFahr - 32) * (5/9)) %>%
  mutate(AverageTemperatureUncertaintyCelsius = (AverageTemperatureUncertaintyFahr - 32) * (5/9))

# now get rid of Fahrenheit column

temperature$AverageTemperatureFahr = NULL
temperature$AverageTemperatureUncertaintyFahr = NULL

# check weather Fahrenheit columns removed or not

head(temperature)

#====================================================

# let's start the Visualisation Journey with GGplot2


plot(x = temperature$year, y = temperature$AverageTemperatureCelsius)

# plot with ggplot2

ggplot(data = temperature, aes(x = year, y = AverageTemperatureCelsius)) +
  geom_point(alpha = 0.05, col = 'red')

# boxplot

ggplot(data = temperature, aes(x = country_id, y = AverageTemperatureCelsius)) +
  geom_boxplot()

# adding points to boxplot we can actually iinterpret the data for that add another
# plot jitter to boxplot

ggplot(data = temperature, aes(x = country_id, y = AverageTemperatureCelsius)) +
  geom_boxplot(alpha = 0, size = 0.9) +
  geom_jitter(alpha = 0.05, color = "tomato")

# challenge 1

ggplot(data = temperature, aes(x = country_id, y = AverageTemperatureCelsius)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  geom_violin(alpha = 0)

# violin plot used for observe data, is there any distribution in the data

# now ploting boxplot with 95% confidence interval in AverageTemparatureUncertainityCelsius

ggplot(data = temperature, aes(x = country_id, y = AverageTemperatureUncertaintyCelsius)) +
  geom_jitter(alpha = 0.01, col = "tomato") +
  geom_boxplot(alpha = 0) +
  scale_y_log10()

# let's plot the Timeseries data
# for this we create new data having average temperature per year per country.
# for that first group the country and year data and then summarize them by mean from every group

yearly_cuntry_temp = temperature %>%
  group_by(year, Country) %>%
  summarise(country_average_Temperature = mean(AverageTemperatureCelsius))
head(yearly_cuntry_temp)

# timeseries graph visible by line graph consist of year on x axis and temparature on y axis

ggplot(data = yearly_cuntry_temp, aes(x = year, y = country_average_Temperature)) +
  geom_line()

# this graph actually consist of all country data so, lets spllit the data by county and add color
ggplot(data = yearly_cuntry_temp, aes(x = year, y = country_average_Temperature, group = Country, color = Country)) +
  geom_line()

# Faceting: 
# ggplot has a special technique called "faceting" that allows to split one plot into multiple plots
# based on some factor. We will use it to plot one time series for each species separately.

ggplot(data = yearly_cuntry_temp, aes(x = year, y = country_average_Temperature, group = Country, color = Country)) +
  geom_line() +
  facet_wrap(~Country)

# Now we would like to split line in each plot by city of each country.
# To do that we need to make measurements in dataframe grouped by country and city.

yearly_cuntry_city_temp = temperature %>%
  group_by(year, Country, City) %>%
  summarise(City_temperature = mean(AverageTemperatureCelsius))
head(yearly_cuntry_city_temp)

# now let's plot some graphs

ggplot(data = yearly_cuntry_city_temp, aes(x = year, y = City_temperature, group = City, color = City)) +
  geom_line() +
  facet_wrap(~Country) +
  theme_bw(base_size = 16) +
  labs(title = "Average Temperature of Time-Series Graph by Country",
       x = "Year of Observation",
       y = "Average Temperature")





