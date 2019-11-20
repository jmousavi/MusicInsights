## -----------------------------------------------------------------------------
include <- function(library_name){
  if( !(library_name %in% installed.packages()) )
    install.packages(library_name) 
  library(library_name, character.only=TRUE)
}


## ---- message=FALSE, warning=FALSE--------------------------------------------
include("tidyverse")
survey <- read_csv("https://raw.githubusercontent.com/introdsci/MusicSurvey/master/music-survey.csv")
preferences <- read_csv("https://raw.githubusercontent.com/introdsci/MusicSurvey/master/preferences-survey.csv")


## -----------------------------------------------------------------------------
colnames(survey)[colnames(survey)=="Timestamp"] <- "time_submitted"
colnames(survey)[colnames(survey)=="First, we are going to create a pseudonym for you to keep this survey anonymous (more or less). Which pseudonym generator would you prefer?"] <- "pseudonym_generator"
colnames(survey)[colnames(survey)=="What is your pseudonym?"] <- "pseudonym"
colnames(survey)[colnames(survey)=="Sex"] <- "sex"
colnames(survey)[colnames(survey)=="Major"] <- "major"
colnames(survey)[colnames(survey)=="Academic Year"] <- "academic_level"
colnames(survey)[colnames(survey)=="Year you were born (YYYY)"] <- "birth_year"
colnames(survey)[colnames(survey)=="Which musical instruments/talents do you play? (Select all that apply)"] <- "instruments"
colnames(survey)[colnames(survey)=="Artist"] <- "artist" 
colnames(survey)[colnames(survey)=="Song"] <- "song"
colnames(survey)[colnames(survey)=="Link to song (on Youtube or Vimeo)"] <- "link"


## -----------------------------------------------------------------------------
person <- tibble(time_submitted = survey$time_submitted, generator = survey$pseudonym_generator,
                 pseudonym = survey$pseudonym, sex = survey$sex, major = survey$major,
                 academic_level = survey$academic_level, birth_year = survey$birth_year)
favorite_song <- tibble(pseudonym = survey$pseudonym, artist = survey$artist, 
                        title = survey$song, video_link = survey$link)
talents <- tibble(pseudonym = survey$pseudonym, instruments = survey$instruments)


## -----------------------------------------------------------------------------
colnames(preferences)[colnames(preferences)=="Timestamp"] <- "time_rated"
colnames(preferences)[colnames(preferences)=="What was your pseudonym?"] <- "pseudonym"
ratings <- preferences %>% 
            gather(key="artist_song",value="rating","40 crew\tNot Enough":"Wheezer\tBuddy Holly")


## -----------------------------------------------------------------------------
talents <- separate_rows(talents,-pseudonym, sep=",")
#clean responses for consistency:
talents$instruments <- talents$instruments %>% 
                        trimws() %>% 
                        tolower() %>%
                        str_replace_all(pattern=".*piano.*","piano") %>%
                        str_replace_all(pattern=".*ukulele.*","ukulele") %>%
                        as.factor()


## -----------------------------------------------------------------------------
ratings$time_rated <- ratings$time_rated %>% 
                        parse_datetime(format="%m/%d/%y %H:%M") %>% 
                        as.POSIXct()
person$time_submitted <- person$time_submitted %>% 
                          parse_datetime(format="%m/%d/%y %H:%M") %>% 
                          as.POSIXct()


## -----------------------------------------------------------------------------
earliest_time <- min(ratings$time_rated[ratings$pseudonym=="Angel Angel"])
ratings <- ratings %>% filter(!(pseudonym=="Angel Angel" & time_rated!=earliest_time))
earliest_time <- min(ratings$time_rated[ratings$pseudonym=="Mission Theory"])
ratings <- ratings %>% filter(!(pseudonym=="Mission Theory" & time_rated!=earliest_time))


## -----------------------------------------------------------------------------
## Reconcile the difference between single- and double-column format of representing
## a song and its artist
ratings$artist_song <- str_replace_all(ratings$artist_song, 
                                       pattern = "\t", 
                                       replacement = " ")
# favorite_song$artist_song <- paste(favorite_song$artist, " ", favorite_song$title)
favorite_rating <- ratings %>% 
                    left_join(favorite_song, by="pseudonym") %>%
                    filter(artist_song==paste(artist,title)) %>%
                    select(pseudonym,artist_song,rating)
print(favorite_rating)


## -----------------------------------------------------------------------------
summary(favorite_rating$rating)

