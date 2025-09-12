# Install the collegebaseball package by Robert Frey (only run once)
devtools::install_github("robert-frey/collegebaseball") 

# Load necessary libraries
library(collegebaseball)  
library(dplyr)             

# Create a data frame of Division 1 teams for the 2025 season
college_baseball <- ncaa_teams(years = 2025, divisions = c(1)) 

# Get positional player stats for Illinois
Illinois <- ncaa_stats(team_id = 596563, year = 2025, type = "batting") 

# Remove pitchers 
Illinois <- Illinois |> 
  filter(Pos != "P") 

# Add Plate Appearances (PA)
Illinois <- Illinois  |> 
  mutate(PA = AB + BB + HBP + SF + SH) 

# Filter for players with more than 50 PA
Illinois <- Illinois |> 
  filter(PA > 50) 

# Add Isolated Power (ISO)
Illinois <- Illinois |> 
  mutate(ISO = SlgPct - BA)

# Add On-Base Plus Slugging (OPS)
Illinois <- Illinois |> 
  mutate(OPS = OBPct + SlgPct)

# Add Strikeout and Walk Rates
Illinois <- Illinois |> 
  mutate(
    Strikeout_rate = paste0(round((K / PA) * 100), "%"),
    Walk_rate = paste0(round((BB / PA) * 100), "%")
  )

# Add Batting Average on Balls in Play (BABIP)
Illinois <- Illinois |> 
  mutate(
    BABIP = (H - HR) / (AB - K - HR + SF),
    BABIP = round(BABIP, 3)
  )

# Move key variables to the front
Illinois <- Illinois |> 
  relocate(player_name, team_name, Yr, year, Pos, conference, division, Jersey,
           GP, GS, PA, Strikeout_rate, Walk_rate, BABIP, ISO, BA, OPS)

# View cleaned and manipulated dataset
print(Illinois)



























