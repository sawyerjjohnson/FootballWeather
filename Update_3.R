
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
  library(zoo)
  
  
  Sys.setenv(CFBD_API_KEY = "yJvtYxIrByWfpgPo/qasS5h82oeY86W1BJ4Rl1SZu8L0mazSR1gBR76pacK7CTF9")
  
}


# Zip Codes #
{
  
  us.zip.codes <- read_excel('~/us_zipcodes.xlsx')
  
  us.zip.codes <- us.zip.codes %>% 
    select(c('Country':'state_abbr'),)
  
  clean.zip.codes <- us.zip.codes %>% 
    group_by(Country, city, state, state_abbr) %>% 
    summarise(
      
      zip.code = median(zip_code),
      zip.code = round(zip.code, digits = 0)
    )
  
  clean.zip.codes$zip.code <- as.character(clean.zip.codes$zip.code)
  
  
  
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
    
    
    # Loop In Compare Zip Code #
    for(i in 1:nrow(cfb.stadiums)){
      
      city.a <- cfb.stadiums$city[i]
      state.a <- cfb.stadiums$state[i]
      
      
      
      cfb.stadiums$comp.zip[i] <- clean.zip.codes[clean.zip.codes$state_abbr == state.a & clean.zip.codes$city == city.a , 'zip.code']
      
      
      
    }
    
    
    # Real Zip Code #
    cfb.stadiums <- cfb.stadiums %>% 
      mutate(
        
        ref.zip.code = ifelse(is.na(zip), comp.zip, zip),
        ref.zip.code = ifelse(ref.zip.code == "character(0)", '12345', ref.zip.code),
        ref.zip.code = ifelse(name == 'Woodforest Bank Stadium', '77385', ref.zip.code),
        ref.zip.code = ifelse(name == 'Warrior Field at Raabe Stadium', '53226', ref.zip.code),
        ref.zip.code = ifelse(name == 'Reynolds Field', '55113', ref.zip.code),
        ref.zip.code = ifelse(name == 'Redwood Bowl', '95521', ref.zip.code)
        
        
      )
    
    
    # Test #
    test <- cfb.stadiums %>% 
      filter(ref.zip.code == "")
    
    
    
    
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
        select(c('competition','game_id', 'week', 'season_type', 'start_date', 'venue_id','venue', 'home_team', 'away_team', 'home_points'),) 
      
      cfb.schedule <- data.frame(cfb.schedule)
      
      
      cfb.schedule[c('date', 'time')] <- str_split_fixed(cfb.schedule$start_date, 'T', 2)
      
      
      cfb.schedule[c('start_time', 'ms')] <- str_split_fixed(cfb.schedule$time, '.000', 2)
      
      
      cfb.schedule$game_time <- as.POSIXct(paste(cfb.schedule$date, cfb.schedule$start_time, sep = " "))
      
      
      cfb.schedule$half_time <- (cfb.schedule$game_time + 7200)
      
      
      cfb.schedule <- cfb.schedule %>% 
        select(c('competition','game_id':'away_team', 'home_points','date','game_time', 'half_time'),)
      
      
    }
    
    
    # Clean CFB Stadiums #
    {
      
      cfb.stadiums <- cfb.stadiums %>% 
        select(c('venue_id', 'name', 'capacity', 'surface', 'city','state', 'ref.zip.code','location', 'elevation', 'roof'),)
      
      
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
      select(c('competition','game_id':'ref.zip.code', 'elevation':'longitude'))
    
    
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
      select(c('competition','game_id':'season_type', 'local.start.time','adj.game.time', 'adj.half.time', 'venue_id':'away_team','home_points','capacity':'roof', 'latitude', 'longitude'),)
    
    
    # Select Relevant Columns Again #
    cfb.schedule.comb <- cfb.schedule.comb %>% 
      select(c('competition', 'local.start.time','adj.game.time', 'adj.half.time', 'venue','home_team','away_team','home_points','capacity', 'surface', 'city', 'ref.zip.code','roof', 'latitude', 'longitude'),)
    
    
    # Change Certain Column Names #
    colnames(cfb.schedule.comb) <- c('competition', 'lv.start.time','game.time', 'half.time', 'stadium','home_team','away_team','home_points','capacity', 'surface', 'city', 'zip.code','roof', 'latitude', 'longitude')
    
    
    
  }
  
  
  # Change Specific Teams Locations #
  {
    
    cfb.schedule.comb <- cfb.schedule.comb %>% 
      mutate(
        
        city = ifelse(stadium == 'Kessler Stadium', 'Long Beach', city),
        city = ifelse(stadium == 'MetLife Stadium', 'Rutherford', city),
        city = ifelse(stadium == 'Wagner College Stadium', 'Arrochar', city),
        city = ifelse(stadium == 'Rice-Totten Field', 'Indianola', city),
        city = ifelse(stadium == 'Panther Stadium' & city == 'Prairie View', 'Hempstead', city),
        city = ifelse(stadium == 'Alumni Stadium' & city == 'Chestnut Hill', 'Boston', city),
        city = ifelse(stadium == 'Barker-Lane Stadium' & city == 'Buies Creek', 'Lillington', city),
        city = ifelse(stadium == 'Ernest W. Spangler Stadium' & city == 'Boiling Springs', 'Shelby', city),
        city = ifelse(stadium == 'Joe Walton Stadium' & city == 'Moon Township', 'Coraopolis', city),
        city = ifelse(stadium == 'Hard Rock Stadium' & city == 'Miami Gardens', 'Miami', city),
        city = ifelse(stadium == 'W.B. Mason Stadium' & city == 'North Easton', 'Easton', city),
        latitude = ifelse(stadium == 'Toyota Field', 34.68, latitude) ,
        longitude = ifelse(stadium == 'Toyota Field', -86.73, longitude)
        
        
      )
    
    
    
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
      select(c('game_id', 'gameday', 'gametime', 'away_team', 'home_team', 'home_score','location', 'roof', 'surface', 'stadium_id', 'stadium'),)
    
    
    # Join Two Sets Together #
    nfl.schedule <- left_join(nfl.schedule, nfl.stadiums, by = 'stadium')
    
    
    # Filter Needed Columns #
    nfl.schedule <- nfl.schedule %>% 
      select(c('game_id', 'gameday', 'gametime', 'away_team', 'home_team', 'home_score','location', 'latitude','longitude','city','state','zip.code','capacity','roof.y', 'surface.y', 'stadium_id', 'stadium'),)
    
    # Change Certain Column Names #
    colnames(nfl.schedule) <- c('game_id', 'gameday', 'gametime', 'away_team', 'home_team', 'home_score','location', 'latitude','longitude','city','state','zip.code','capacity','roof', 'surface', 'stadium_id', 'stadium')
    
    
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
               'stadium', 'home_team', 'away_team', 'home_score','capacity','surface', 'city', 'state', 'zip.code','location', 'roof'),)
    
    
    # Create Competition Column #
    nfl.schedule$competition <- 'NFL'
    
    
    # Select Relevant Columns Again #
    nfl.schedule <- nfl.schedule %>% 
      select(c('competition', 'local.start.time', 'adj.game.time', 'adj.half.time', 'stadium', 'home_team', 'away_team', 'home_score','capacity', 'surface', 'city', 'zip.code','roof', 'latitude','longitude'),)
    
    
    # Change Column Names #
    colnames(nfl.schedule) <- c('competition', 'lv.start.time','game.time', 'half.time', 'stadium','home_team','away_team', 'home_points','capacity', 'surface', 'city', 'zip.code','roof', 'latitude','longitude')
    
    
  }
  
  
}


# Join Together To Create Full Football Set #
{
  
  # Make Each Characters #
  nfl.schedule$zip.code <- as.character(nfl.schedule$zip.code)
  cfb.schedule.comb$zip.code <- as.character(cfb.schedule.comb$zip.code)
  
  
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
    select(c('competition','game.date', 'lv.start.time':'half.time', 'matchup', 'home_points','stadium', 'capacity':'roof', 'latitude', 'longitude'),)
  
  # Remove Duplicate Rows #
  football.schedule <- football.schedule %>% distinct()
  
  # Write To Excel #
  write.xlsx(football.schedule, 'football.schedule.xlsx')
  
}


# Create Weekly Schedule #
{
  
  # Read Excel #
  football.schedule <- read_excel('football.schedule.xlsx')
  
  
  football.weekly <- football.schedule %>% 
    filter(lv.start.time < (Sys.Date() + 7) & is.na(home_points) & lv.start.time > (Sys.Date() - 2))
  
  
  
  
  football.weekly <- data.frame(football.weekly)
  
  # Create Reference City #
  football.weekly <- football.weekly %>% 
    mutate(
      
      ref.city = str_replace(city, " ", replacement = "_"),
      ref.city = str_replace(ref.city, " ", replacement = "_"),
      gn = row_number(),
      latitude = round(latitude, digits = 2),
      longitude = round(longitude, digits = 2),
      coordinates = paste(latitude, longitude, sep = ",")
      
    )
  
  
  
  # Change Certain Zip Codes #
  football.weekly <- football.weekly %>% 
    mutate(
      
      zip.code = ifelse(city == 'Durham' & stadium == 'Wildcat Stadium', '03824', zip.code) ,
      zip.code = ifelse(city == 'Tampa' & stadium == 'Raymond James Stadium', '33607', zip.code),
      zip.code = ifelse(city == 'East Hartford' & stadium == 'Rentschler Field', '06118', zip.code),
      zip.code = ifelse(city == 'Boston' & stadium == 'Alumni Stadium', '02467', zip.code),
      zip.code = ifelse(city == 'Fargo' & stadium == 'Fargodome', '58102', zip.code),
      zip.code = ifelse(city == 'Long Beach' & stadium == 'Kessler Stadium', '07764', zip.code),
      zip.code = ifelse(city == 'Orono' & stadium == 'Alfond Stadium', '04473', zip.code),
      zip.code = ifelse(city == 'New Britain' & stadium == 'Arute Field', '06053', zip.code),
      zip.code = ifelse(city == 'Boston' & stadium == 'Harvard Stadium', '02163', zip.code),
      zip.code = ifelse(city == 'Princeton' & stadium == 'Princeton University Stadium', '08540', zip.code),
      zip.code = ifelse(city == 'Fairfield' & stadium == 'Campus Field', '06825', zip.code),
      zip.code = ifelse(city == 'Providence' & stadium == 'Brown Stadium', '02906', zip.code),
      zip.code = ifelse(city == 'Kingston' & stadium == 'Meade Stadium', '02881', zip.code),
      zip.code = ifelse(city == 'Hanover' & stadium == 'Memorial Field', '03755', zip.code),
      zip.code = ifelse(city == 'Worcester' & stadium == 'Fitton Field', '01610', zip.code),
      zip.code = ifelse(city == 'Rutherford' & stadium == 'MetLife Stadium', '07073', zip.code),
      zip.code = ifelse(city == 'Hadley' & stadium == 'Warren McGuirk Alumni Stadium', '01003', zip.code),
      zip.code = ifelse(city == 'New Haven' & stadium == 'Yale Bowl', '06515', zip.code),
      
      
      zip.code = ifelse(city == 'Indianapolis' & stadium == 'Butler Bowl', '46208', zip.code),
      zip.code = ifelse(city == 'Mobile' & stadium == 'Hancock Whitney Stadium', '36688', zip.code),
      zip.code = ifelse(city == 'Hempstead' & stadium == 'Panther Stadium', '77446', zip.code),
      zip.code = ifelse(city == 'Smithfield' & stadium == 'Bulldog Stadium', '02917', zip.code),
      zip.code = ifelse(city == 'Baltimore' & stadium == 'Hughes Stadium', '21218', zip.code),
      zip.code = ifelse(city == 'Elon' & stadium == 'Rhodes Stadium', '27244', zip.code),
      zip.code = ifelse(city == 'Kennesaw' & stadium == 'Fifth Third Bank Stadium', '30144', zip.code),
      
    )
  
  
  
}



# Weekly Football Weather (Zip Code) #
{
  
  weather_zip_df <- data.frame()
  
  zip.list <- c(football.weekly$zip.code)
  
  # Create Function #
  {
    
    weather.zip.func <- function(url){
      web_content <- httr::GET(url)
      web_content <- content(web_content,"text")
      json_data <- fromJSON(web_content, flatten = TRUE)
      df <- as.data.frame(json_data)
      return(df)
    }
    
  }
  
  
  # Loop Through Each City #
  for (zip in zip.list){
    
    url<-str_glue("http://api.weatherapi.com/v1/forecast.json?key=8dfa15d8d61848ad862182755223108&q={zip}&days=10&aqi=no&alerts=no")
    weather_zip_df<- bind_rows(weather_zip_df,weather.zip.func(url))
    
  }
  
  
  # Create Row Number #
  weather_zip_df <- weather_zip_df %>% 
    mutate(
      rn = row_number()
    )
  
  
  # Create Groups #
  {
    
    # Find Game Counts #  
    wgs <- (nrow(weather_zip_df) / 10 )
    wgs.list <- c(1:wgs)
    
    # Define Beginning #
    beg.a <- 1
    
    # Create Loop #
    for(group in wgs.list){
      
      end.a <- (group * 10)
      
      weather_zip_df$gn[beg.a:end.a] <- group
      
      beg.a <- (end.a + 1)
      
      
    }
    
    }
  
  
  # Select Relevant Columns #
  weather_zip_df <- weather_zip_df %>% select(c('gn','location.name':'location.lon','forecast.forecastday.date', 'forecast.forecastday.hour'),)
  
  
  # Change Column Names #
  colnames(weather_zip_df) <- c('gn', 'loc.city', 'loc.state', 'loc.country', 'loc.lat', 'loc.lon', 'zip.date', 'zip.hour.forecast')
  
  
  # Combine Weekly Schedule & Weekly Weather #
  comb.weekly.1 <- left_join(football.weekly, weather_zip_df, by = 'gn')
  
  
  
  
  # Find Absolute Difference of Coordinates #
  comb.weekly.1 <- comb.weekly.1 %>% 
    mutate(
      
      zip.abs.diff = (abs(loc.lat - latitude) + abs(loc.lon - longitude))
      
    )
  
  
  # Filter Relevant Columns #
  comb.weekly.1 <- comb.weekly.1 %>% 
    select(c('competition':'gn', 'zip.date':'zip.abs.diff'),)
  
  
  
  
}


# Weekly Football Weather (Cities) #
{
  
  weather_city_df <- data.frame()
  
  city.list <- c(football.weekly$ref.city)
  
  # Create Function #
  {
    
    weather.city.func <- function(url){
      web_content <- httr::GET(url)
      web_content <- content(web_content,"text")
      json_data <- fromJSON(web_content, flatten = TRUE)
      df <- as.data.frame(json_data)
      return(df)
    }
    
  }
  
  
  # Loop Through Each City #
  for (city in city.list){
    
    url<-str_glue("http://api.weatherapi.com/v1/forecast.json?key=8dfa15d8d61848ad862182755223108&q={city}&days=10&aqi=no&alerts=no")
    weather_city_df<- bind_rows(weather_city_df,weather.city.func(url))
    
  }
  
  
  # Create Row Number #
  weather_city_df <- weather_city_df %>% 
    mutate(
      rn = row_number()
    )
  
  
  # Create Groups #
  {
    
    # Find Game Counts #  
    wgs.2 <- (nrow(weather_city_df) / 10 )
    wgs.list.2 <- c(1:wgs.2)
    
    # Define Beginning #
    beg.a <- 1
    
    # Create Loop #
    for(group in wgs.list.2){
      
      end.a <- (group * 10)
      
      weather_city_df$gn[beg.a:end.a] <- group
      
      beg.a <- (end.a + 1)
      
      
    }
    
    }
  
  
  # Select Relevant Columns #
  weather_city_df <- weather_city_df %>% select(c('gn','location.name':'location.lon','forecast.forecastday.date', 'forecast.forecastday.hour'),)
  
  
  # Change Column Names #
  colnames(weather_city_df) <- c('gn', 'loc.city', 'loc.state', 'loc.country', 'loc.lat', 'loc.lon', 'city.date', 'city.hour.forecast')
  
  
  # Order Correctly #
  weather_city_df <- weather_city_df %>% 
    arrange(gn, city.date)
  
  
  # Combine Weekly Schedule & Weekly Weather #
  comb.weekly.2 <- bind_cols(comb.weekly.1, weather_city_df)
  
  
  
  
  # Find Absolute Difference of Coordinates #
  comb.weekly.2 <- comb.weekly.2 %>% 
    mutate(
      
      city.abs.diff = (abs(loc.lat - latitude) + abs(loc.lon - longitude))
      
    )
  
  
  # Select Relevant Columns #
  comb.weekly.2 <- comb.weekly.2 %>% 
    select(c('competition':'zip.abs.diff', 'city.date':'city.abs.diff'),)
  
  
  
}


# Weekly Football Weather (Coordinates) #
{
  
  weather_cord_df <- data.frame()
  
  football.weekly$coordinates <- as.character(football.weekly$coordinates)
  
  coordinates <- c(football.weekly$coordinates)
  
  # Create Function #
  {
    
    weather.cord.func <- function(url){
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
    weather_cord_df<- bind_rows(weather_cord_df,weather.cord.func(url))
    
  }
  
  
  # Create Row Number #
  weather_cord_df <- weather_cord_df %>% 
    mutate(
      rn = row_number()
    )
  
  
  # Create Groups #
  {
    
    # Find Game Counts #  
    wgs.3 <- (nrow(weather_cord_df) / 10 )
    wgs.list.3 <- c(1:wgs.3)
    
    # Define Beginning #
    beg.b <- 1
    
    # Create Loop #
    for(group in wgs.list.3){
      
      end.b <- (group * 10)
      
      weather_cord_df$gn[beg.b:end.b] <- group
      
      beg.b <- (end.b + 1)
      
      
    }
    
    
    }
  
  
  # Select Relevant Columns #
  weather_cord_df <- weather_cord_df %>% select(c('gn','location.name':'location.lon','forecast.forecastday.date', 'forecast.forecastday.hour'),)
  
  
  # Change Column Names #
  colnames(weather_cord_df) <- c('gn', 'cord.city', 'cord.state', 'cord.country', 'cord.lat', 'cord.lon', 'cord.date', 'cord.hour.forecast')
  
  
  # Arrange Data Frame #
  weather_cord_df <- weather_cord_df %>% 
    arrange(gn, cord.date)
  
  
  # Combine Weekly Schedule & Weekly Weather #
  comb.football.weekly <- bind_cols(comb.weekly.2, weather_cord_df)
  
  
  # Find Absolute Difference of Coordinates #
  comb.football.weekly <- comb.football.weekly %>% 
    mutate(
      
      cord.abs.diff = (abs(cord.lat - latitude) + abs(cord.lon - longitude))
      
    )
  
  
  
  # Select Relevant Columns #
  comb.football.weekly <- comb.football.weekly %>% 
    select(c('competition':'city.abs.diff', 'cord.date':'cord.abs.diff'),)
  
  
  # Change Column Name #
  colnames(comb.football.weekly)[17] <- c('gn')
  
 
  
  
}


# Clean Combined Set #
{
  
  comb.football.weekly <- comb.football.weekly %>% 
    mutate(
      
      ref.1 = ifelse(zip.abs.diff < 1, 'zip',
                     ifelse(city.abs.diff < 1, 'city', 'cord')) ,
      
      ref.2 = ifelse(ref.1 == 'city', 'cord',
                     ifelse(ref.1 == 'zip' & city.abs.diff < 1, 'city', 'cord')),
      
      
      ref.date.1 = ifelse(ref.1 == 'zip', zip.date, city.date) ,
      ref.date.2 = ifelse(ref.2 == 'city', city.date, cord.date) ,
      
      hour.forecast.1 = ifelse(ref.1 == 'zip', zip.hour.forecast, city.hour.forecast) ,
      hour.forecast.2 = ifelse(ref.2 == 'city', city.hour.forecast, cord.hour.forecast)
      
      )
  
  
  # Select Relevant Columns #
  comb.football.weekly <- comb.football.weekly %>% 
    select(c('competition':'gn', 'ref.1':'hour.forecast.2'),)
  
  
  # Filter Correct Day #
  comb.football.weekly <- comb.football.weekly %>% 
    filter(game.date == ref.date.1 & game.date == ref.date.2)
  
  
  # Create Game Start Column #
  comb.football.weekly[c('start.date', 'start.time')] <- str_split_fixed(comb.football.weekly$game.time, ' ', 2)
  
  comb.football.weekly[c('start.hour', 'minute', 'second')] <- str_split_fixed(comb.football.weekly$start.time, ':', 3)
  
  
  comb.football.weekly$start.hour <- as.numeric(comb.football.weekly$start.hour)
  comb.football.weekly$minute <- as.numeric(comb.football.weekly$minute)
  comb.football.weekly$second <- as.numeric(comb.football.weekly$second)
  
  
  
  comb.football.weekly <- comb.football.weekly %>% 
    mutate(
      
      half.hour = (start.hour + 2),
      minute = (minute + 1)
    )
  
  
  
  # Select Relevant Columns #
  comb.football.weekly <- comb.football.weekly %>% 
    select(c('gn', 'competition':'ref.city', 'ref.1':'hour.forecast.2', 'start.hour':'half.hour'),)
  
  
  
  
}


# Expanded Hourly DFs #
{

  
# Expand DF Columns (Ref 1) #
{
  
  weather.table.1 <- data.frame()
  
  weekly.games.1 <- as.numeric(nrow(comb.football.weekly))
  
  game.numbers.1 <- c(1:weekly.games.1)
  
  
  # Create Hourly Weather Through Loop #
  for(game in game.numbers.1){
    
    temp.df.1 <- comb.football.weekly$hour.forecast.1[[game]]
    
    temp.df.1 <- data.frame(temp.df.1)
    
    
    weather.table.1 <- bind_rows(weather.table.1, temp.df.1)
    
    
  }
  
  
  
  # Select Relevant Columns #
  weather.table.1 <- weather.table.1 %>% 
    select(c('time', 'temp_f', 'feelslike_f', 'wind_mph', 'gust_mph','precip_in', 'chance_of_rain', 'chance_of_snow', 'vis_miles', 'condition.text'))
  
  
  
  
  # Change Column Names #
  colnames(weather.table.1) <- c('time.1', 'temp_f.1', 'feelslike_f.1', 'wind_mph.1', 'gust_mph.1','precip_in.1', 'chance_of_rain.1', 'chance_of_snow.1', 'vis_miles.1', 'condition.text.1')
  
  
  
}


# Expand DF Columns (Ref 2) #
{
  
  weather.table.2 <- data.frame()
  
  weekly.games.2 <- as.numeric(nrow(comb.football.weekly))
  
  game.numbers.2 <- c(1:weekly.games.2)
  
  
  # Create Hourly Weather Through Loop #
  for(game in game.numbers.2){
    
    temp.df.2 <- comb.football.weekly$hour.forecast.2[[game]]
    
    temp.df.2 <- data.frame(temp.df.2)
    
    
    weather.table.2 <- bind_rows(weather.table.2, temp.df.2)
    
    
  }
  
  
  
  # Select Relevant Columns #
  weather.table.2 <- weather.table.2 %>% 
    select(c('time', 'temp_f', 'feelslike_f', 'wind_mph', 'gust_mph','precip_in', 'chance_of_rain', 'chance_of_snow', 'vis_miles', 'condition.text'))
  
  
  
  
  # Change Column Names #
  colnames(weather.table.2) <- c('time.2', 'temp_f.2', 'feelslike_f.2', 'wind_mph.2', 'gust_mph.2','precip_in.2', 'chance_of_rain.2', 'chance_of_snow.2', 'vis_miles.2', 'condition.text.2')
  
  
  
}
  
  
# Combine Weather Tables #
{
  
  combined.weather.tables <- bind_cols(weather.table.1, weather.table.2)  
   
  
  # Change Based On Precipitation #
  combined.weather.tables <- combined.weather.tables %>% 
    mutate(
      
      new_rain.1 = ifelse(precip_in.1 <= .1, (0 + chance_of_rain.1)/2, chance_of_rain.1),
      new_snow.1 = ifelse(precip_in.1 <= .1, (0 + chance_of_snow.1)/2, chance_of_snow.1),
      new_rain.2 = ifelse(precip_in.2 <= .1, (0 + chance_of_rain.2)/2, chance_of_rain.2),
      new_snow.2 = ifelse(precip_in.2 <= .1, (0 + chance_of_snow.2)/2, chance_of_snow.2)
      
      
    )
  
  # Change Based On Weather Description #
  combined.weather.tables <- combined.weather.tables %>% 
    mutate(
      
      desc.1 = ifelse(condition.text.1 == 'Sunny' |
                        condition.text.1 == 'Clear' |
                        condition.text.1 == 'Partly cloudy', 1, 0) ,
      
      
      desc.2 = ifelse(condition.text.2 == 'Sunny' |
                        condition.text.2 == 'Clear' |
                        condition.text.2 == 'Partly cloudy', 1, 0) ,
      
      
      
      new_rain.1 = ifelse(desc.1 == 1, (0 + new_rain.1)/2, new_rain.1),
      new_snow.1 = ifelse(desc.1 == 1, (0 + new_snow.1)/2, new_snow.1),
      new_rain.2 = ifelse(desc.2 == 1, (0 + new_rain.2)/2, new_rain.2),
      new_snow.2 = ifelse(desc.2 == 1, (0 + new_snow.2)/2, new_snow.2)
      
      
    )
  
  
  
   
  # Create Groups #
  {
    
    # Find Game Counts #  
    wgs.4 <- (nrow(combined.weather.tables) / 24 )
    wgs.list.4 <- c(1:wgs.4)
    
    # Define Beginning #
    beg.c <- 1
    
    # Create Loop #
    for(group in wgs.list.4){
      
      end.c <- (group * 24)
      
      combined.weather.tables$gn[beg.c:end.c] <- group
      
      beg.c <- (end.c + 1)
      
      
    }
    
    
  }
  
  
  # Rearrange Columns #
  combined.weather.tables <- combined.weather.tables %>% 
    select(c('gn', 'time.1':'new_snow.2'),)
  
  
  # Find Rolling Averages Of Weather #
  combined.weather.tables <- combined.weather.tables %>% 
    group_by(gn) %>% 
    mutate(
      
      roll_chance_of_rain.1 = lag(rollapply(new_rain.1, 1, mean)),
      roll_chance_of_snow.1 = lag(rollapply(new_snow.1, 1, mean)),
      roll_chance_of_rain.2 = lag(rollapply(new_rain.2, 1, mean)),
      roll_chance_of_snow.2 = lag(rollapply(new_snow.2, 1, mean)),
      
      
      roll_chance_of_rain.1 = ifelse(is.na(roll_chance_of_rain.1), 0, roll_chance_of_rain.1),
      roll_chance_of_snow.1 = ifelse(is.na(roll_chance_of_snow.1), 0, roll_chance_of_snow.1),
      roll_chance_of_rain.2 = ifelse(is.na(roll_chance_of_rain.2), 0, roll_chance_of_rain.2),
      roll_chance_of_snow.2 = ifelse(is.na(roll_chance_of_snow.2), 0, roll_chance_of_snow.2)
      
      
    ) %>% ungroup()
  
  
  # Create New Weather Predictions #
  combined.weather.tables <- combined.weather.tables %>% 
    mutate(
      
      adj.rain.1 = (roll_chance_of_rain.1 * .33) + (new_rain.1 * .67) ,
      adj.snow.1 = (roll_chance_of_snow.1 * .33) + (new_snow.1 * .67) ,
      
      adj.rain.2 = (roll_chance_of_rain.2 * .33) + (new_rain.2 * .67) ,
      adj.snow.2 = (roll_chance_of_snow.2 * .33) + (new_snow.2 * .67) 
      
    )
  
  
    
  # Find Averages #
  combined.weather.tables <- combined.weather.tables %>% 
    mutate(
      
      time = time.1 ,
      mu.temp_f = (temp_f.1 + temp_f.2) / 2 ,
      mu.feelslike_f = (feelslike_f.1 + feelslike_f.2) / 2 ,
      mu.wind_mph = (wind_mph.1 + wind_mph.2) / 2 ,
      mu.gust_mph = (gust_mph.1 + gust_mph.2) / 2 ,
      mu.precip_in = (precip_in.1 + precip_in.2) / 2 ,
      mu.chance_of_rain = (adj.rain.1 + adj.rain.2) / 2 ,
      mu.chance_of_snow = (adj.snow.1 + adj.snow.2) / 2 ,
      mu.vis_miles = (vis_miles.1 + vis_miles.2) / 2 ,
      condition.text = condition.text.1 
      
      )
    
   
  # Select Relevant Columns #
  combined.weather.tables <- combined.weather.tables %>% 
    select(c('time':'condition.text'),)
  
  
    
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
  
  
  # Remove Unnecessary Columns #
  weekly.games.df <- weekly.games.df %>% 
    select(c('gn','competition':'matchup', 'stadium':'city', 'roof', 'start.hour':'half.hour'),)
  
  
  
  # Join Together #
  weekly.games.df <- bind_cols(weekly.games.df, combined.weather.tables)
  
  
  # Change Column Names #
  colnames(weekly.games.df) <- c('gn', 'competition', 'game.date', 'lv.start.time', 'game.time', 'half.time', 'matchup', 'stadium', 'capacity', 'surface', 'city', 'roof', 'start.hour',
                                 'minute', 'second', 'half.hour', 'time', 'temp_f', 'feelslike_f', 'wind_mph', 'gust_mph', 'precip_in', 'chance_of_rain', 'chance_of_snow', 'vis_miles', 'condition.text')
  
  
  
  # Remove Unnecessary Columns #
  weekly.games.df <- weekly.games.df %>% 
    select(c('gn','competition':'half.hour','time', 'temp_f', 'feelslike_f', 'wind_mph', 'gust_mph',   'chance_of_rain', 'chance_of_snow', 'precip_in', 'vis_miles',  'condition.text'))
  
  
  # Change Time Column Format #
  weekly.games.df$time <- as.POSIXct(weekly.games.df$time)
  
  # Create Time Comparison Columns #
  weekly.games.df[c('ref.date', 'ref.time')] <- str_split_fixed(weekly.games.df$time, ' ', 2)
  weekly.games.df[c('ref.hour', 'ref.minute', 'ref.second')] <- str_split_fixed(weekly.games.df$ref.time, ':', 3)
  
  weekly.games.df$ref.hour <- as.numeric(weekly.games.df$ref.hour)
  weekly.games.df$ref.minute <- as.numeric(weekly.games.df$ref.minute)
  weekly.games.df$ref.second <- as.numeric(weekly.games.df$ref.second)
  
  
  # Find Difference Columns #
  weekly.games.df <- weekly.games.df %>% 
    mutate(
      
      diff.start.time = (abs(start.hour - ref.hour) * 3600) + (abs(minute - ref.minute)* 60) + (abs(second - ref.second)) ,
      diff.half.time = (abs(half.hour - ref.hour) * 3600) + (abs(minute - ref.minute)* 60) + (abs(second - ref.second)) 
      
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
    select(c('gn', 'competition', 'lv.start.time', 'matchup', 'stadium','city', 'surface','roof', 'temp_f', 'feelslike_f', 
             'wind_mph', 'gust_mph', 'chance_of_rain', 'chance_of_snow', 'precip_in', 'vis_miles','condition.text'),)
  
  
  # Change Column Names #
  colnames(weekly.games.df) <- c('gn','competition', 'lv.start.time', 'matchup', 'stadium', 'city','surface','roof', 'temp', 'feels_like', 
                                 'wind_mph', 'gust_mph', 'chance_of_rain', 'chance_of_snow', 'precip_in', 'vis_miles','condition')
  
  
  # Remove Single Entries #
  weekly.games.df$random <- 1
  
  weekly.games.df <- weekly.games.df %>% 
    group_by(gn) %>% 
    mutate(
      
      count = sum(random)
    ) %>% ungroup()
  
  
  weekly.games.df <- weekly.games.df %>% 
    filter(count == 2) %>% 
    select(c('gn':'condition'),)
  
  
  
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
  

  
  
  # Change Matchup Names #
  for(i in 1:nrow(weekly.games.df)){
    if(i %% 2 != 0){
      
      weekly.games.df$matchup[i+1] <- paste( weekly.games.df$matchup[i+1], '(Half)', sep = ' ')
      
      
    }
  }
  
 
  
  # Remove Even Row Column Entries #
  for(i in 1:nrow(weekly.games.df)){
    if(i %% 2 == 0){
      
      
      weekly.games.df$competition[i] <- ""
      weekly.games.df$lv.start.time[i] <- ""
      weekly.games.df$stadium[i] <- ""
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
  
  write.xlsx(weekly.games.df, file = '/home/jupyter/WeeklyWeather/FootballWeather/appContents/weeklyweather.xlsx',
             overwrite = TRUE)
  
}


# Deploy App #
{
  rsconnect::deployApp('/home/jupyter/WeeklyWeather/FootballWeather/appContents', forceUpdate = TRUE)
  
}   



