


# Load Packages #
{
  
  library(tidyverse)
  library(dplyr)
  library(readxl)
  library(openxlsx)
  library(tidyr)
  library(cfbfastR)
  library(stringr)  
  library(devtools)
  library(weatherData)
  library(rnoaa)
  library(lubridate)
  library(data.table)
  library(lutz)
  library(purrr)
  library(sf)
  library(nflreadr)
  library(nflfastR)
  library(DescTools)
  library(httr)
  library(stringr)
  library(jsonlite)
  library(scales)
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(rsconnect)
  library(reticulate)
  
  
  Sys.setenv(CFBD_API_KEY = "yJvtYxIrByWfpgPo/qasS5h82oeY86W1BJ4Rl1SZu8L0mazSR1gBR76pacK7CTF9")
  
}


# CFB #
{
  
  
  # Get CFB Stadiums & Schedules #
  {
    
    # Load Stadiums
    cfb.stadiums <- cfbfastR::cfbd_venues()
    
    cfb.stadiums <- data.frame(cfb.stadiums)
    
    # Edit Stadiums #
    cfb.stadiums <- cfb.stadiums %>% 
      mutate(
        
        surface = ifelse(grass == 'TRUE', 'Grass', 'Turf'),
        roof = ifelse(dome == 'TRUE', 'Dome', 'Open Air')
        
      )
    
    # Load Schedules #
    cfb.schedule <- cfbfastR::cfbd_game_info(2022)
    
  }
  
  
  # Clean CFB Sets #
  {
    
    # Clean CFB Schedules #
    {
      
      
      cfb.schedule <- cfb.schedule %>% 
        filter(home_division == 'fbs' | home_division == 'fcs' | away_division == 'fbs' | away_division == 'fcs' )
      
      
      cfb.schedule <- cfb.schedule %>% 
        mutate(
          competition = ifelse(home_division == 'fbs' | away_division == 'fbs', 'FBS', 'FCS'),
          competition = ifelse(is.na(competition), 'FCS', competition)
        )
      
      
      cfb.schedule <- cfb.schedule %>% 
        filter(is.na(home_points) & start_date > Sys.Date()) %>% 
        select(c('competition','game_id', 'week', 'season_type', 'start_date', 'venue_id','venue', 'home_team', 'away_team'),) 
      
      cfb.schedule <- data.frame(cfb.schedule)
      
      
      cfb.schedule[c('date', 'time')] <- str_split_fixed(cfb.schedule$start_date, 'T', 2)
      
      
      cfb.schedule[c('start_time', 'ms')] <- str_split_fixed(cfb.schedule$time, '.000', 2)
      
      
      cfb.schedule$game_time <- as.POSIXct(paste(cfb.schedule$date, cfb.schedule$start_time, sep = " "))
      
      
      cfb.schedule$half_time <- (cfb.schedule$game_time + 7200)
      
      
      cfb.schedule <- cfb.schedule %>% 
        select(c('competition','game_id':'away_team', 'date','game_time', 'half_time'),)
      
      
    }
    
    
    # Clean CFB Stadiums #
    {
      
      cfb.stadiums <- cfb.stadiums %>% 
        select(c('venue_id', 'name', 'capacity', 'surface', 'city','state', 'location', 'elevation', 'roof'),)
      
      
      latitude.vector <- pull(cfb.stadiums$location, x)
      longitude.vector <- pull(cfb.stadiums$location, y)
      
      cfb.stadiums$latitude <- latitude.vector
      cfb.stadiums$longitude <- longitude.vector
      
      cfb.stadiums <- data.frame(cfb.stadiums)
      
      colnames(cfb.stadiums)[2] <- ('venue')
      
    }
    
    
  }
  
  
  # Combine CFB Stadiums and Schedules #
  {
    
    cfb.schedule.comb <- left_join(cfb.schedule, cfb.stadiums, by = c('venue_id', 'venue'))
    
    
    # Filter Columns #
    cfb.schedule.comb <- cfb.schedule.comb %>% 
      select(c('competition','game_id':'state', 'elevation':'longitude'))
    
    
  }
  
  
  # Create CFB Time Zone Adjustments #
  {
    
    cfb.schedule.comb <-  cfb.schedule.comb %>% 
      mutate(
        time.zone = tz_lookup_coords(lat = latitude, lon = longitude, method = 'accurate')
      ) 
    
    
    cfb.schedule.comb <- cfb.schedule.comb %>% 
      mutate(
        
        time.adj = ifelse(time.zone == 'America/New_York' | time.zone == 'America/Kentucky/Louisville' |
                            time.zone == 'America/Indiana/Indianapolis' | time.zone == 'America/Detroit', -4,
                          ifelse(time.zone == 'America/Chicago', -5,
                                 ifelse(time.zone == 'America/Denver' | time.zone == 'America/Boise', -6,
                                        ifelse(time.zone == 'America/Los_Angeles' | time.zone == 'America/Phoenix', -7, -10
                                        ))))
      )
    
    
    
    # Adjust Times #
    cfb.schedule.comb <- cfb.schedule.comb %>% 
      mutate(
        adj.game.time = (game_time + (3600 * time.adj)),
        adj.half.time = (half_time + (3600 * time.adj)),
        local.start.time = (game_time + (3600 * -7))
      )
    
    
    # Select Relevant Columns #
    cfb.schedule.comb <- cfb.schedule.comb %>% 
      select(c('competition','game_id':'season_type', 'local.start.time','adj.game.time', 'adj.half.time', 'venue_id':'away_team','capacity':'roof', 'latitude', 'longitude'),)
    
    
    # Select Relevant Columns Again #
    cfb.schedule.comb <- cfb.schedule.comb %>% 
      select(c('competition', 'local.start.time','adj.game.time', 'adj.half.time', 'venue','home_team','away_team','capacity', 'surface', 'city', 'roof', 'latitude', 'longitude'),)
    
    
    # Change Certain Column Names #
    colnames(cfb.schedule.comb) <- c('competition', 'lv.start.time','game.time', 'half.time', 'stadium','home_team','away_team','capacity', 'surface', 'city', 'roof', 'latitude', 'longitude')
    
    
    
  }
  
  
}


# NFL #
{
  
  # Get NFL Stadiums & Schedules #
  {
    
    # Read In NFL Stadium Info #
    nfl.stadiums <- read_excel('~/nfl.stadiums.xlsx') 
    
    
    # Load NFL Schedule #
    nflreadr::.clear_cache()
    
    nfl.schedule <- nflreadr::load_schedules(2022)
    
    
    
    # Change Super Dome Name #
    nfl.schedule <- nfl.schedule %>% 
      mutate(
        stadium = ifelse(stadium == 'Mercedes-Benz Superdome', 'Caesars Superdome', stadium)
      )
    
    
    
    # Filter Needed Columns #
    nfl.schedule <- nfl.schedule %>% 
      select(c('game_id', 'gameday', 'gametime', 'away_team', 'home_team', 'location', 'roof', 'surface', 'stadium_id', 'stadium'),)
    
    
    # Join Two Sets Together #
    nfl.schedule <- left_join(nfl.schedule, nfl.stadiums, by = 'stadium')
    
    
    # Filter Needed Columns #
    nfl.schedule <- nfl.schedule %>% 
      select(c('game_id', 'gameday', 'gametime', 'away_team', 'home_team', 'location', 'latitude','longitude','city','state','capacity','roof.y', 'surface.y', 'stadium_id', 'stadium'),)
    
    # Change Certain Column Names #
    colnames(nfl.schedule) <- c('game_id', 'gameday', 'gametime', 'away_team', 'home_team', 'location', 'latitude','longitude','city','state','capacity','roof', 'surface', 'stadium_id', 'stadium')
    
    
    # Create Game Time and Half Time #
    nfl.schedule <- nfl.schedule %>% 
      mutate(
        game_time = as.POSIXct(paste(gameday, gametime, sep = " ")),
        half_time = game_time + 7200
      )
    
    
    
  }
  
  
  # Create NFL Time Zone Adjustments #
  {
    
    nfl.schedule <-  nfl.schedule %>% 
      mutate(
        time.zone = tz_lookup_coords(lat = latitude, lon = longitude, method = 'accurate')
      ) 
    
    
    # Create Adjustments #
    nfl.schedule <- nfl.schedule %>% 
      mutate(
        
        time.adj = ifelse(time.zone == 'America/New_York' | time.zone == 'America/Detroit' | time.zone == 'America/Indiana/Indianapolis', 0,
                          ifelse(time.zone == 'Europe/Berlin', 6,
                                 ifelse(time.zone == 'Europe/London', 5,
                                        ifelse(time.zone == 'America/Chicago'| time.zone == 'America/Mexico_City', -1,
                                               ifelse(time.zone == 'America/Denver', -2, 
                                                      ifelse(time.zone == 'America/Phoenix', -3, -3)
                                               ))))))
    
    
    # Adjust Times #
    nfl.schedule <- nfl.schedule %>% 
      mutate(
        adj.game.time = (game_time + (3600 * time.adj)),
        adj.half.time = (half_time + (3600 * time.adj)),
        local.start.time = (game_time + (3600 * -3))
      )
    
    
    # Select Relevant Columns #
    nfl.schedule <- nfl.schedule %>% 
      select(c('game_id', 'gametime', 'local.start.time','adj.game.time', 'adj.half.time', 'longitude', 'latitude','stadium_id',
               'stadium', 'home_team', 'away_team', 'capacity','surface', 'city', 'state', 'location', 'roof'),)
    
    
    # Create Competition Column #
    nfl.schedule$competition <- 'NFL'
    
    
    # Select Relevant Columns Again #
    nfl.schedule <- nfl.schedule %>% 
      select(c('competition', 'local.start.time', 'adj.game.time', 'adj.half.time', 'stadium', 'home_team', 'away_team', 'capacity', 'surface', 'city', 'roof', 'longitude', 'latitude',),)
    
    
    # Change Column Names #
    colnames(nfl.schedule) <- c('competition', 'lv.start.time','game.time', 'half.time', 'stadium','home_team','away_team','capacity', 'surface', 'city', 'roof', 'longitude', 'latitude')
    
    
  }
  
  
}


# Join Together To Create Full Football Set #
{
  
  # Join Tables Together #
  football.schedule <- full_join(nfl.schedule, cfb.schedule.comb)
  
  # Create Gamedate Column #
  football.schedule[c('game.date', 'time')] <- str_split_fixed(football.schedule$game.time, " ", 2)
  
  # Create Matchup Column #
  football.schedule <- football.schedule %>% 
    mutate(
      matchup = paste(away_team, home_team, sep = " @ ")
    )
  
  # Select Relevant Columns #
  football.schedule <- football.schedule %>% 
    select(c('competition','game.date', 'lv.start.time':'half.time', 'matchup', 'stadium', 'capacity':'roof', 'latitude', 'longitude'),)
  
  # Remove Duplicate Rows #
  football.schedule <- football.schedule %>% distinct()
  
  
}


# Create Weekly Schedule #
{
  
  football.weekly <- football.schedule %>% 
    filter(lv.start.time < (Sys.Date() + 7) & lv.start.time > Sys.Date())
  
  
  football.weekly <- data.frame(football.weekly)
  
  # Create Reference City #
  football.weekly <- football.weekly %>% 
    mutate(
      
      ref.city = str_replace(city, " ", replacement = "_"),
      ref.city = str_replace(ref.city, " ", replacement = "_"),
      gn = row_number(),
      coordinates = paste(latitude, longitude, sep = ",")
      
    )
  
  
  
}


# Weekly Football Weather #
{
  
  weather_df <- data.frame()
  
  football.weekly$coordinates <- as.character(football.weekly$coordinates)
  
  coordinates <- c(football.weekly$coordinates)
  
  # Create Function #
  {
    
    weather.func <- function(url){
      web_content <- httr::GET(url)
      web_content <- content(web_content,"text")
      json_data <- fromJSON(web_content, flatten = TRUE)
      df <- as.data.frame(json_data)
      return(df)
    }
    
  }
  
  
  # Loop Through Each City #
  for (cord in coordinates){
    
    url<-str_glue("http://api.weatherapi.com/v1/forecast.json?key=8dfa15d8d61848ad862182755223108&q={cord}&days=10&aqi=no&alerts=no")
    weather_df<- bind_rows(weather_df,weather.func(url))
    
  }
  
  
  # Create Row Number #
  weather_df <- weather_df %>% 
    mutate(
      rn = row_number()
    )
  
  
  # Create Groups #
  {
    
    # Find Game Counts #  
    wgs <- (nrow(weather_df) / 10 )
    wgs.list <- c(1:wgs)
    
    # Define Beginning #
    beg.a <- 1
    
    # Create Loop #
    for(group in wgs.list){
      
      end.a <- (group * 10)
      
      weather_df$gn[beg.a:end.a] <- group
      
      beg.a <- (end.a + 1)
      
      
    }
    
    }
  
  
  # Change One Column Name #
  colnames(weather_df)[1] <- 'city'
  
  
  # Combine Weekly Schedule & Weekly Weather #
  comb.football.weekly <- left_join(football.weekly, weather_df, by = 'gn')
  
  
  # Select Relevant Columns #
  comb.football.weekly <- comb.football.weekly %>% 
    select(c('competition':'roof', 'gn','forecast.forecastday.date', 'forecast.forecastday.hour' ),)
  
  
  # Filter Correct Day #
  comb.football.weekly <- comb.football.weekly %>% 
    filter(game.date == forecast.forecastday.date)
  
  
}


# Join Extra Tables Together #
{
  
  weather.table <- data.frame()
  
  weekly.games <- as.numeric(nrow(comb.football.weekly))
  
  game.numbers <- c(1:weekly.games)
  
  
  # Create Hourly Weather Through Loop #
  for(game in game.numbers){
    
    temp.df <- comb.football.weekly$forecast.forecastday.hour[[game]]
    
    temp.df <- data.frame(temp.df)
    
    
    weather.table <- bind_rows(weather.table, temp.df)
    
    
  }
  
  
}


# Multiply Weekly Set By 24 #
{
  
  
  weekly.games.df <- bind_rows(comb.football.weekly, comb.football.weekly, comb.football.weekly, comb.football.weekly,
                               comb.football.weekly, comb.football.weekly, comb.football.weekly, comb.football.weekly,
                               comb.football.weekly, comb.football.weekly, comb.football.weekly, comb.football.weekly,
                               comb.football.weekly, comb.football.weekly, comb.football.weekly, comb.football.weekly,
                               comb.football.weekly, comb.football.weekly, comb.football.weekly, comb.football.weekly,
                               comb.football.weekly, comb.football.weekly, comb.football.weekly, comb.football.weekly
  )
  
  
  
}


# Clean Weekly Games #
{
  
  # Change Order #
  weekly.games.df <- weekly.games.df %>% arrange(gn)
  
  
  # Join Together #
  weekly.games.df <- bind_cols(weekly.games.df, weather.table)
  
  
  # Remove Unnecessary Columns #
  weekly.games.df <- weekly.games.df %>% 
    select(c('competition':'roof', 'gn', 'time', 'temp_f', 'wind_mph', 'precip_in', 'feelslike_f', 'chance_of_rain', 'chance_of_snow', 'vis_miles', 'gust_mph', 'vis_miles','condition.text'))
  
  
  # Add One Second To Start Time and Half Time #
  weekly.games.df <- weekly.games.df %>% 
    mutate(
      
      game.time = game.time + 1,
      half.time = half.time + 1
      
    )
  
  
  
  # Find Difference In Times #
  weekly.games.df <- weekly.games.df %>% 
    mutate(
      
      diff.start.time = as.numeric(abs(difftime(game.time, time, units = 'mins'))),
      diff.half.time = as.numeric(abs(difftime(half.time, time, units = 'mins')))
      
    )
  
  
  
  # Find Minimum Value By Group #
  weekly.games.df <- weekly.games.df %>% 
    group_by(gn) %>% 
    mutate(
      start.min = min(diff.start.time),
      half.min = min(diff.half.time)
    ) %>% ungroup()
  
  
  # Filter Start Times #
  weekly.games.df <- weekly.games.df %>%
    group_by(gn) %>% 
    filter(diff.start.time == start.min | diff.half.time == half.min) %>% 
    ungroup()
  
  
  # Select Relevant Columns #
  weekly.games.df <- weekly.games.df %>% 
    select(c('gn', 'competition', 'lv.start.time', 'matchup', 'stadium', 'capacity','city.x','surface','roof', 'temp_f', 'feelslike_f', 
             'wind_mph', 'gust_mph', 'chance_of_rain', 'chance_of_snow', 'precip_in', 'vis_miles','condition.text'),)
  
  
  # Change Column Names #
  colnames(weekly.games.df) <- c('gn','competition', 'lv.start.time', 'matchup', 'stadium', 'capacity','city','surface','roof', 'temp', 'feels_like', 
                                 'wind_mph', 'gust_mph', 'chance_of_rain', 'chance_of_snow', 'precip_in', 'vis_miles','condition')
  
  
  
  # Make Start Time A Character #
  weekly.games.df$lv.start.time <- as.character(weekly.games.df$lv.start.time)
  
  
  # Assign League Numbers #
  weekly.games.df <- weekly.games.df %>% 
    mutate(
      
      ln = ifelse(competition == 'NFL', 1,
                  ifelse(competition == 'FBS', 2, 3))
      
    )
  
  # Arrange Table #
  weekly.games.df <- weekly.games.df %>% 
    arrange(ln, gn)
  
  
  # Remove Even Row Column Entries #
  for(i in 1:nrow(weekly.games.df)){
    if(i %% 2 == 0){
      
      
      weekly.games.df$competition[i] <- ""
      weekly.games.df$lv.start.time[i] <- ""
      weekly.games.df$matchup[i] <- ""
      weekly.games.df$stadium[i] <- ""
      weekly.games.df$capacity[i] <- ""
      weekly.games.df$surface[i] <- ""
      weekly.games.df$city[i] <- ""
      weekly.games.df$roof[i] <- ""
      
      
    }
    
  }
  
  
  # Make Rain & Snow % #
  weekly.games.df <- weekly.games.df %>% 
    mutate(
      
      chance_of_rain = chance_of_rain / 100 ,
      chance_of_snow = chance_of_snow / 100 ,
      
      chance_of_rain = percent(chance_of_rain, accuracy = 1),
      chance_of_snow = percent(chance_of_snow, accuracy = 1)
      
      
    )
  
  # Select Relevant Columns #
  weekly.games.df <- weekly.games.df %>% 
    select(c('gn','competition', 'lv.start.time', 'matchup', 'stadium', 'city','surface', 'roof', 'temp':'condition'),)
  
  
  # Make Competition Column Name System Time #
  temp.col <- as.character(Sys.time())
  colnames(weekly.games.df)[2] <- temp.col
    
  # Make Data Table #
  weekly.games.df <- data.frame(weekly.games.df)
  
  
}


# Write Excel #
{
  
  write.xlsx(weekly.games.df, 'weeklyweather.xlsx')
  
}


