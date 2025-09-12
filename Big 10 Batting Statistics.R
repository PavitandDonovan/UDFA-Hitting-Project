#Load necessary libraries
library(collegebaseball)  
library(dplyr)
library(tibble)
library(purrr)


# A lookup table for Big 10 teams with their corresponding IDs
Big_10_Teams <- tibble::tibble( 
  name = c("Illinois", "Indiana", "Iowa", "Maryland", "Michigan", "Michigan_St",
           "Minnesota", "Nebraska", "Northwestern", "Ohio_St", "Oregon", "Penn_St",
           "Purdue", "Rutgers", "Southern_California", "UCLA", "Washington"),
  id = c(596563, 596566, 596569, 596593, 596602, 596601,
         596604, 596712, 596636, 596639, 596644, 596647,
         596653, 596660, 596681, 596514, 596443)
) 

# Function to pull and clean batting stats for a given team and year
Big10_Stats <- function(team_id, team_name, min_PA = 50) {
  
  # Pull Big 10 Raw Stats
  data <- ncaa_stats(team_id = team_id, year = 2025, type = "batting")

  # Cleaned and calculated stats with additional metrics.
  data_clean <- data %>%
    filter(Pos != "P") %>%
    mutate(
      PA = AB + BB + HBP + SF + SH,
      ISO = SlgPct - BA,
      Strikeout_rate = K / PA,
      Walk_rate = BB / PA,
      BABIP = (H - HR) / (AB - K - HR + SF),
      OPS = OBPct + SlgPct,
      BABIP = round(BABIP, 3)
    ) %>%
    filter(PA > min_PA) %>%
    arrange(Strikeout_rate) %>%
    mutate(
      Strikeout_rate = paste0(round(Strikeout_rate * 100), "%"),
      Walk_rate = paste0(round(Walk_rate * 100), "%")
    ) %>%
    relocate(
      player_name, team_name, Yr, year, Pos, conference, division, Jersey,
      GP, GS, PA, Strikeout_rate, Walk_rate, BABIP, ISO, BA, OPS
    )
  
  return(data_clean)
} 

#Pull stats for Washington. Just need to change team_id and team_name to match and data will get pulled
Washington <- Big10_Stats(team_id = 596443, team_name = "Washington") 

# Pull combined Big 10 hitters stats using pmap_dfr for all teams in the look up table
Big_10_Hitters <- pmap_dfr(
  Big_10_Teams,
  ~ Big10_Stats(team_id = ..2, team_name = ..1)
) 



