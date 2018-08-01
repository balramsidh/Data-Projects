# Loading required packages
packages <- c('tidyverse','lubridate','stringr','scales')

package.check <- lapply(packages, FUN = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

# Loading the required files

df_airlines <- read.csv("airlines.csv") 
df_airports <- read.csv("airports.csv")
df_flights <- read.csv("flights.csv")


# Data Manipulation 

df_flights$date <- as.Date(paste(df_flights$YEAR,df_flights$MONTH,df_flights$DAY, sep="-"))
df_flights$DEPARTURE_TIME <-  sub("(\\d{2})$", ":\\1", str_sub(paste('000',df_flights$DEPARTURE_TIME, sep=""), -4,-1))
df_flights$ARRIVAL_TIME <- sub("(\\d{2})$", ":\\1", str_sub(paste('000',df_flights$ARRIVAL_TIME, sep=""), -4,-1))
df_flights$SCHEDULED_ARRIVAL <- sub("(\\d{2})$", ":\\1", str_sub(paste('000',df_flights$SCHEDULED_ARRIVAL, sep=""), -4,-1))
df_flights$SCHEDULED_DEPARTURE <- sub("(\\d{2})$", ":\\1", str_sub(paste('000',df_flights$SCHEDULED_DEPARTURE, sep=""), -4,-1))


# Preliminary data visualization, for a random sample of 100000 rows.
set.seed(123)

df_test <- sample_n(df_flights, 100000)

df_test %>%
  ggplot(aes(x=DISTANCE, y=ARRIVAL_DELAY)) +
  geom_point(aes(col= AIRLINE)) +
  labs(title= "Distance vs Delays", x="Distance in Miles", y="Arrival Delay in mins") +
  theme_bw()

# correlation test
cor.test(df_flights$DISTANCE, df_flights$ARRIVAL_DELAY)

# mean delay times 

df_flights %>%
  select (DEPARTURE_DELAY,ARRIVAL_DELAY, AIRLINE) %>% 
  inner_join(df_airlines, by = c("AIRLINE" = "IATA_CODE")) %>%
  group_by (AIRLINE.y) %>%
  summarise(mean_dept_delay=mean(DEPARTURE_DELAY, na.rm = T), mean_arr_delay=mean(ARRIVAL_DELAY, na.rm=T)) %>%
  gather(key, value, -AIRLINE.y) %>%
  ggplot(aes(x=AIRLINE.y, y=value)) +
  geom_col(aes(fill=key)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Mean delay per flight", x="Airlines", y="Delay time in min") +
  coord_flip()


# delays by airports 

# considering source airports for departure delay 

df_flights[df_flights$DEPARTURE_DELAY >0,] %>%
  select (DEPARTURE_DELAY, ORIGIN_AIRPORT) %>% 
  inner_join(df_airports, by = c("ORIGIN_AIRPORT" = "IATA_CODE")) %>%
  group_by (AIRPORT,CITY) %>%
  summarise(delayed_flights = n()) %>%
  arrange(desc(delayed_flights)) %>%
  head(10) %>%
  ggplot(aes(x=reorder(AIRPORT,delayed_flights), y=delayed_flights)) +
  geom_col(fill='#3CB371') +
  theme_bw()+
  labs(title="Most number of departure delays by origin", x="Origin Airport", y="Number of delayed flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(label =comma ) +
  coord_flip()

# considering destination airports for  arrival delay
df_flights[df_flights$ARRIVAL_DELAY >0,] %>%
  select (ARRIVAL_DELAY, DESTINATION_AIRPORT) %>% 
  inner_join(df_airports, by = c("DESTINATION_AIRPORT" = "IATA_CODE")) %>%
  group_by (AIRPORT,CITY) %>%
  summarise(delayed_flights = n()) %>%
  arrange(desc(delayed_flights)) %>%
  head(10) %>%
  ggplot(aes(x=reorder(AIRPORT,delayed_flights), y=delayed_flights)) +
  geom_col(fill='#3CB371') +
  theme_bw()+
  labs(title="Most number of arrival delays by destination", x="Destinaton Airport", y="Number of delayed flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(label =comma ) +
  coord_flip()


# Arrival delay by airlines 

df_airline_delay <- df_flights %>%
  select (ARRIVAL_DELAY, AIRLINE) %>% 
  inner_join(df_airlines, by = c("AIRLINE" = "IATA_CODE")) %>%
  group_by (AIRLINE.y) %>%
  summarise( mean_arr_delay=mean(ARRIVAL_DELAY, na.rm=T),total_flights=n(), delayed_flights=sum(ARRIVAL_DELAY >0, na.rm = T),  delayed_flights_percent= (sum(ARRIVAL_DELAY >0, na.rm = T)/n()*100))

df_airline_delay %>%
  ggplot(aes(x=reorder(AIRLINE.y, desc(mean_arr_delay)), y=mean_arr_delay)) +
  geom_col(fill='#3CB371') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Mean Arrival delay per airline", x="Airlines", y="Time in min") +
  coord_flip()

df_airline_delay %>%
  ggplot(aes(x=reorder(AIRLINE.y, desc(delayed_flights_percent)), y=delayed_flights_percent)) +
  geom_col(fill='#3CB371') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Proportion of Delayed flight", x="Airlines", y="Percentage of delayed flights") +
  coord_flip()


### additional findings


df_airports_stats <- df_flights %>%
  group_by(ORIGIN_AIRPORT,DESTINATION_AIRPORT) %>%
  summarise (total_flights=n(), total_delayed_flights=sum(ARRIVAL_DELAY>0, na.rm=T)) %>%
  arrange(desc(total_flights)) %>%
  mutate(route= paste(ORIGIN_AIRPORT,DESTINATION_AIRPORT, sep="-"))

# busiest route
df_airports_stats %>%
  head(20) %>%
  ggplot(aes(x=reorder(route,total_flights),y=total_flights)) +
  geom_col(fill='#3CB371') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="20 Busiest Routes", x="Route", y="No of flights") +
  coord_flip()

# most flights

df_airline_delay %>%
  arrange(desc(total_flights)) %>%
  mutate(percentage= total_flights/sum(total_flights)*100) %>%
  ggplot(aes(x=reorder(AIRLINE.y, percentage),y=percentage)) +
  geom_col(fill="#3CB371") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Market Share of Airlines", x="Airlines", y="Percentage of total flights") +
  coord_flip()

# 

df_flights$DAY_OF_WEEK <-weekdays(df_flights$date)


df_flights %>%
  group_by (DAY_OF_WEEK) %>%
  summarise(count= n()) %>%
  ggplot(aes(x=DAY_OF_WEEK,count, y=count, group=1)) +
  geom_line() +
  geom_point()s

  

