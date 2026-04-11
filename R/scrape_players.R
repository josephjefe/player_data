library(dplyr)
library(tidyr)
library(rvest)
library(nflreadr)
library(janitor)
library(stringr)
library(httr)

# dataset -----------------------------------------------------------------

scrape_otc_free_agents <- function(year = nflreadr::get_current_season(roster = TRUE)){
  
  res <- POST(
    "https://overthecap.com/wp-admin/admin-ajax.php",
    body = list(
      action = "get_free_agents",
      season = year
    ),
    encode = "form"
  )
  
  html <- content(res, as = "text")
  
  html_wrapped <- paste0(
    "<table><tbody>",
    html,
    "</tbody></table>"
  )
  
  otc_fa <- read_html(html_wrapped) |>
    html_element("table") |>
    html_table() |> 
    rename(player = X1, position_roster = X2, old_team = X3, new_team = X4) |> 
    mutate(new_team = na_if(trimws(new_team), "")) |> 
    filter(is.na(new_team))
  
  free_agents <- otc_fa |> 
    mutate(player = str_trim(str_replace(player,',',''))) |> 
    mutate(position_roster = str_trim(position_roster)) |> 
    mutate(rank = row_number(), .before = player) |> 
    arrange(rank) |> 
    mutate(player = nflreadr::clean_player_names(player), .after = "player") |> 
    mutate(position_roster = case_when(
      position_roster %in% c("LT", "RT", "T") ~ "OT", 
      position_roster %in% c("LG", "RG", "G") ~ "OG", 
      position_roster %in% c("FB") ~ "RB", 
      position_roster %in% c("DE", "OLB") ~ "EDGE", 
      position_roster %in% c("ILB") ~ "LB", 
      position_roster %in% c("LDT", "RDT", "NT", "DT", "IDL") ~ "DL", 
      position_roster %in% c("FS", "SS") ~ "S", 
      TRUE ~ position_roster
    ), .after = position_roster) |> 
    select(player, position_roster)
  
  return(free_agents)
}

free_agents <- scrape_otc_free_agents()

saveRDS(free_agents, paste0("Data/", "new_players.rds"))

