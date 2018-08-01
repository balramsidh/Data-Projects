####### Introduction to Tidyverse - Datacamp 
## 02/02 - Balram Sidh 
## https://campus.datacamp.com/courses/introduction-to-the-tidyverse/data-wrangling-1?ex=1

packages = c("gapminder","dplyr","ggplot2")

package.check <- lapply(packages, FUN = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})


# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder, aes(x=gdpPercap,y=lifeExp,color=continent,size=pop)) +
  geom_point() +
  scale_x_log10()+
  facet_wrap(~year)

# Summarize to find the median life expectancy
gapminder %>%
  summarize( medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy
gapminder %>%
  filter( year == 1957) %>%
  summarize ( medianLifeExp= median(lifeExp))

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder %>%
  filter( year == 1957) %>%
  summarize ( medianLifeExp= median(lifeExp), maxGdpPercap= max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each year
# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder %>%
  group_by( year) %>%
  summarize ( medianLifeExp= median(lifeExp), maxGdpPercap= max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize ( medianLifeExp= median(lifeExp), maxGdpPercap= max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each year/continent combination
gapminder %>%
  group_by(continent,year) %>%
  summarize ( medianLifeExp= median(lifeExp), maxGdpPercap= max(gdpPercap))

###
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time
ggplot(by_year, aes(x=year,y=medianLifeExp)) +
  geom_point() +
  expand_limits(y=0)

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder %>%
  group_by (continent,year) %>%
  summarize(medianGdpPercap = median(gdpPercap))


# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x=year,y=medianGdpPercap)) +
  geom_line() +
  expand_limits(y=0)

# Plot the change in medianGdpPercap in each continent over time

ggplot(by_year_continent, aes(x=year,y=medianGdpPercap,color=continent))+
  geom_point() +
  expand_limits(y=0)

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>%
  filter(year == 2007) %>%
  group_by (continent) %>%
  summarize(medianLifeExp= median(lifeExp),medianGdpPercap = median(gdpPercap))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x=medianGdpPercap,y=medianLifeExp,color=continent)) +
  geom_point()
       

# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>%
  group_by (continent,year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x=year,y=medianGdpPercap,color=continent)) +
  geom_line() +
  expand_limits(y=0)

# Summarize the median gdpPercap by year and continent in 1952
by_continent <- gapminder %>%
  filter(year==1952) %>%
  group_by (continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
ggplot(by_continent,aes(x=continent,y=medianGdpPercap))+
  geom_col()

# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>%
  filter(year==1952,continent=="Oceania")

# Create a bar plot of gdpPerCap by country
ggplot(oceania_1952,aes(x=country,y=gdpPercap))+
  geom_col()

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop)
ggplot(gapminder_1952, aes(x=pop)) +
  geom_histogram()


# Create a boxplot comparing gdpPercap among continents
ggplot(gapminder_1952, aes(x=continent, y=gdpPercap)) +
  geom_boxplot()+
  scale_y_log10()

# Add a title to this graph: "Comparing GDP per capita across continents"
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Comparing GDP per capita across continents")

iris %>%
  group_by(Species) %>%
  summarise( count = n())


