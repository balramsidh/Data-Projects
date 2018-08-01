####### Data joining in R - Datacamp 
## 02/17 - Balram Sidh 
## https://campus.datacamp.com/courses/joining-data-in-r-with-dplyr/mutating-joins?ex=10

packages = c("gapminder","dplyr","ggplot2","Lahman","purrr")

package.check <- lapply(packages, FUN = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})



# Finish the code below to recreate bands3 with a right join
bands2 <- left_join(bands, artists, by = c("first", "last"))
bands3 <- right_join(artists,bands,by = c("first", "last"))

# Check that bands3 is equal to bands2
setequal(bands2,bands3)

# Join albums to songs using inner_join()
inner_join(songs,albums,by="album")

# Join bands to artists using full_join()
full_join(artists,bands, by=c("first","last"))

# Find guitarists in bands dataset (don't change)
temp <- left_join(bands, artists, by = c("first", "last"))
temp <- filter(temp, instrument == "Guitar")
select(temp, first, last, band)

# Reproduce code above using pipes
bands %>% 
  left_join(artists,by = c("first", "last"))  %>%
  filter(instrument == "Guitar") %>%
  select(first,last,band)


# Examine the contents of the goal dataset
goal

# Create goal2 using full_join() and inner_join() 
goal2 <- artists %>% 
  full_join(bands,by= c("first","last")) %>%
  inner_join(songs, by= c("first","last"))



# Check that goal and goal2 are the samea
setequal(goal,goal2)

# Create one table that combines all information
artists %>%
  full_join(bands, by=c("first","last")) %>%
  full_join(songs, by=c("first","last")) %>%
  full_join(albums, by= c("album","band"))

# Semi-Joins 
#semi-joins provide a concise way to filter data from the first dataset based on 
#information in a second dataset.
# View the output of semi_join()
artists %>% 
  semi_join(songs, by = c("first", "last"))

# Create the same result
artists %>% 
  right_join(songs, by = c("first", "last")) %>% 
  filter(!is.na(instrument)) %>% 
  select(first, last, instrument)


albums %>% 
  # Collect the albums made by a band
  semi_join(bands, by= "band") %>% 
  # Count the albums made by a band
  nrow()

# Anti-joins 

# Return rows of artists that don't have bands info
artists %>% anti_join(bands, by =c("first","last"))


# Set operations 



# Select the song names from live
live_songs <- live %>% select(song)

# Select the song names from greatest_hits
greatest_songs <- greatest_hits %>% select(song)

# Create the new dataset using a set operation
live_songs %>% 
  setdiff(greatest_songs)


# Select songs from live and greatest_hits
live_songs <- live %>% select(song)
greatest_songs <- greatest_hits %>% select(song)

# Return the songs that only exist in one dataset
union(live_songs,greatest_songs) %>%
  setdiff(intersect(live_songs,greatest_songs))

# Check if same order: definitive and complete
identical(definitive,complete)

# Check if any order: definitive and complete
setequal(definitive,complete)


# Songs in definitive but not complete
setdiff(definitive,complete)


# Songs in complete but not definitive
setdiff(complete,definitive)

#### Binds 

# Examine side_one and side_two
side_one
side_two

# Bind side_one and side_two into a single dataset
side_one %>% 
  bind_rows(side_two)

# Examine discography and jimi
discography
jimi

jimi %>% 
  # Bind jimi into a single data frame
  bind_rows(.id="album") %>% 
  # Make a complete data frame
  left_join(discography, by="album")

# Examine hank_years and hank_charts
hank_years
hank_charts

hank_years %>% 
  # Reorder hank_years alphabetically by song title
  arrange(song) %>% 
  # Select just the year column
  select(year) %>% 
  # Bind the year column
  bind_cols(hank_charts) %>% 
  # Arrange the finished dataset
  arrange(year)

# Data frame 
# Make combined data frame using data_frame()
data_frame(year=hank_year,song=hank_song,peak=hank_peak) %>%
  # Extract songs where peak equals 1
  filter(peak==1)

# Examine the contents of hank
hank

# Convert the hank list into a data frame
as_data_frame(hank) %>% 
  # Extract songs where peak equals 1
  filter(peak==1)


# Examine the contents of michael
michael

bind_rows(michael,.id="album") %>% 
  group_by(album) %>% 
  mutate(rank = min_rank(peak)) %>% 
  filter(rank == 1) %>% 
  select(-rank, -peak)

seventies %>% 
  # Coerce seventies$year into a useful numeric
  mutate(year= as.numeric(as.character(year))) %>% 
  # Bind the updated version of seventies to sixties
  bind_rows(sixties) %>% 
  arrange(year)

####### Advance data joining 
# Load the tibble package
library(tibble)

stage_songs %>% 
  # Add row names as a column named song
  rownames_to_column(var="song") %>% 
  # Left join stage_writers to stage_songs
  left_join(stage_writers, by = "song")

# Examine the result of joining singers to two_songs
two_songs %>% inner_join(singers, by = "movie")

# Remove NA's from key before joining
two_songs %>% 
  filter(!is.na(movie)) %>% 
  inner_join(singers, by = "movie")

movie_years %>% 
  # Left join movie_studios to movie_years
  left_join(movie_studios, by="movie") %>% 
  # Rename the columns: artist and studio
  rename(artist=name.x, studio=name.y)

# Identify the key column
elvis_songs
elvis_movies

elvis_movies %>% 
  # Left join elvis_songs to elvis_movies by this column
  left_join(elvis_songs, by = c("name" = "movie")) %>% 
  # Rename columns
  rename(movie = name, song = name.y)

# Identify the key columns
movie_directors
movie_years

movie_years %>% 
  # Left join movie_directors to movie_years
  left_join(movie_directors, by = c("movie" = "name")) %>% 
  # Arrange the columns using select()
  select(year,movie,artist=name,director,studio)

list(more_artists, more_bands, supergroups) %>% 
  # Return rows of more_artists in all three datasets
  reduce(semi_join)


######### case study ##############
#### lahman packages contains sean lahmans baseball database
### 26 tables 



# Examine lahmanNames

lahmanNames <- readRDS(file = "lahmanNames.rds")

# Find variables in common
lahmanNames %>%
  reduce(intersect)

# check what fields are most common among all tables
lahmanNames %>%  
  # Bind the data frames in lahmanNames
  bind_rows(.id="dataframe") %>%
  # Group the result by var
  group_by(var) %>%
  # Tally the number of appearances
  tally() %>%
  # Filter the data
  filter(n > 1) %>% 
  # Arrange the results
  arrange(desc(n))

# check which all tables use playerid
lahmanNames %>% 
  # Bind the data frames
  bind_rows(.id="dataframe") %>%
  # Filter the results
  filter(var == "playerID") %>% 
  # Extract the dataframe variable
  `$`(dataframe)

#creating a df for all players. distinct as the name suggests remove duplicate rows
players <- Master %>% 
  # Return one row for each distinct player
  distinct(playerID,nameFirst,nameLast)

# find out how many players are not in Salaries DF
players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by = "playerID") %>%
  # Count them
  count()

# check how many of these unpaid players actually paid a game 

players %>% 
  anti_join(Salaries, by = "playerID") %>% 
  # How many unsalaried players appear in Appearances?
  semi_join(Appearances, by = "playerID") %>% 
  count()


# check if these players played enough games ?
players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by ="playerID") %>% 
  # Join them to Appearances
  left_join(Appearances, by="playerID") %>% 
  # Calculate total_games for each player
  group_by(playerID) %>%
  summarise(total_games = sum(G_all,na.rm = T)) %>%
              # Arrange in descending order by total_games
              arrange(desc(total_games))

# check if these players actually batted at a game
players %>%
  # Find unsalaried players
  anti_join(Salaries, by = "playerID") %>% 
  # Join Batting to the unsalaried players
  left_join(Batting, by = "playerID") %>% 
  # Group by player
  group_by(playerID) %>% 
  # Sum at-bats for each player
  summarise(total_at_bat= sum(AB, na.rm = T)) %>% 
  # Arrange in descending order
  arrange(desc(total_at_bat))

as_tibble(HallOfFame)


## Hall of fame 
# Find the distinct players that appear in HallOfFame
nominated <- HallOfFame %>% 
  distinct(playerID)

nominated %>% 
  # Count the number of players in nominated
  count()

nominated_full <- nominated %>% 
  # Join to Master
  left_join(Master, by = "playerID") %>% 
  # Return playerID, nameFirst, nameLast
  select(playerID,nameFirst,nameLast)


# Find distinct players in HallOfFame with inducted == "Y"
inducted <- HallOfFame %>% 
  filter(inducted == "Y") %>% 
  distinct(playerID)

inducted %>% 
  # Count the number of players in inducted
  count()

inducted_full <- inducted %>% 
  # Join to Master
  left_join(Master, by = "playerID") %>% 
  # Return playerID, nameFirst, nameLast
  select(playerID, nameFirst, nameLast)


# to check if players who got inducted have higher awards 

# Tally the number of awards in AwardsPlayers by playerID
nAwards <- AwardsPlayers %>% 
  group_by(playerID) %>%
  tally()

nAwards %>% 
  # Filter to just the players in inducted 
  semi_join(inducted, by = "playerID") %>% 
  # Calculate the mean number of awards per player
  summarise( avg_n = mean(n, na.rm = T))

nAwards %>% 
  # Filter to just the players in nominated 
  semi_join(nominated, by = "playerID") %>% 
  # Filter to players NOT in inducted 
  anti_join(inducted, by = "playerID") %>% 
  # Calculate the mean number of awards per player
  summarise( avg_n = mean(n, na.rm = T))


as_tibble(Salaries)


Salaries %>%
  group_by(playerID) %>%
  summarise(max_salary = max(salary)) %>%
  summarise(avg_salary = mean(max_salary))

# Find the players who are in nominated, but not inducted
notInducted <- nominated %>% 
  setdiff(inducted)

Salaries %>% 
  # Find the players who are in notInducted
  semi_join(notInducted) %>% 
  # Calculate the max salary by player
  group_by(playerID) %>% 
  summarise(max_salary = max(salary)) %>% 
  # Calculate the average of the max salaries
  summarise(avg_salary= mean(max_salary))

# Repeat for players who were inducted
Salaries %>%
  semi_join(inducted) %>%
  group_by(playerID) %>%
  summarise(max_salary = max(salary)) %>%
  summarise(avg_salary = mean(max_salary))


Appearances %>% 
  # Filter Appearances against nominated
  semi_join(nominated, by = "playerID") %>%
  group_by( playerID) %>%
  summarise(last_year = max(yearID)) %>%
  left_join(HallOfFame, by = "playerID") %>%
  filter (yearID <= last_year)
