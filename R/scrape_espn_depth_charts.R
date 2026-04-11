library(dplyr)
library(tidyr)
library(stringr)
library(nflreadr)
library(janitor)
library(rvest)

# Data----

teams_espn <- load_teams(current = TRUE) |> 
  mutate(team_abbr_espn = case_when(
    team_abbr == "LA" ~ "LAR", 
    team_abbr == "WAS" ~ "WSH", 
    TRUE ~ team_abbr))

team_names_espn <- c(teams_espn$team_abbr_espn)

datalist = list()

for(i in 1:32){
  
  #i <- "KC"
  url <- paste0("https://www.espn.com/nfl/team/depth/_/name/", team_names_espn[i])
  # url <- paste0("https://www.espn.com/nfl/team/depth/_/name/", "SF")
  
  # Get data from ESPN
  tables <- read_html(url) |> 
    html_elements(".ResponsiveTable--fixed-left")
  # Extract data from table for offense
  depth_chart_off1 <- tables[[1]] |> 
    html_table()
  names_row_off <- which(depth_chart_off1[1] == "Starter")
  depth_chart_off2 <- depth_chart_off1 |> 
    row_to_names(row_number = names_row_off, remove_rows_above = FALSE)  |> 
    clean_names()
  pos_off <- depth_chart_off2 |> 
    slice_head(n = names_row_off - 1) |>
    select("position" = "starter")
  depth_chart_off3 <- depth_chart_off2 |> 
    slice_tail(n = names_row_off - 1)
  depth_chart_off <- bind_cols(pos_off, depth_chart_off3) |> 
    mutate(rank = row_number(), .by = position) |> 
    mutate(position = case_when(
      rank == 1 & position == "WR" ~ "LWR", 
      rank == 2 & position == "WR" ~ "RWR", 
      rank == 3 & position == "WR" ~ "SWR", 
      position == "FB" ~ "RB", 
      TRUE ~ position
    )) |> 
    select(-rank)
  # Extract data for defense
  depth_chart_def1 <- tables[[2]] |> 
    html_table()
  names_row_def <- which(depth_chart_def1[1] == "Starter")
  depth_chart_def2 <- depth_chart_def1 |> 
    row_to_names(row_number = names_row_def, remove_rows_above = FALSE) |> 
    clean_names()
  pos_def <- depth_chart_def2 |> 
    slice_head(n = names_row_def - 1) |>
    select("position" = "starter")
  depth_chart_def3 <- depth_chart_def2 |> 
    slice_tail(n = names_row_def - 1)
  depth_chart_def <- bind_cols(pos_def, depth_chart_def3)
  # Extract data for special teams
  depth_chart_st1 <- tables[[3]] |> 
    html_table()
  names_row_st <- which(depth_chart_st1[1] == "Starter")
  depth_chart_st2 <- depth_chart_st1 |> 
    row_to_names(row_number = names_row_st, remove_rows_above = FALSE) |> 
    clean_names()
  pos_st <- depth_chart_st2 |> 
    slice_head(n = names_row_st - 1) |>
    select("position" = "starter")
  depth_chart_st3 <- depth_chart_st2 |> 
    slice_tail(n = names_row_st - 1)
  depth_chart_st <- bind_cols(pos_st, depth_chart_st3) |> 
    filter(position %in% c("PK", "P", "LS")) |> 
    mutate(position = if_else(position == "PK", "K", position))
  # Extract table titles to figure out defense style (34 or 43)
  titles_raw <- read_html(url) |> 
    html_elements(".Table__Title")
  titles <- titles_raw[[2]] |> 
    html_text2() 
  # Join everything together
  depth_chart <- bind_rows(depth_chart_off, depth_chart_def, depth_chart_st)
  depth_chart$def_style <- titles
  depth_chart$team_abbr <- team_names_espn[i]
  
  datalist[[i]] <- depth_chart
  
}

df_raw <- dplyr::bind_rows(datalist)
df_raw[df_raw == "-"] <- NA

df_clean <- df_raw |> 
  select(team_abbr, position, starter, second = x2nd, third = x3rd, fourth = x4th, def_style) |> 
  mutate(starter = case_when(
    str_ends(starter, " O") ~ word(starter, 1, -2), 
    str_ends(starter, " Q") ~ word(starter, 1, -2), 
    str_ends(starter, " SUSP") ~ word(starter, 1, -2),
    str_ends(starter, " IR") ~ word(starter, 1, -2), 
    TRUE ~ starter
  )) |> 
  mutate(starter = clean_player_names(starter)) |> 
  mutate(second = case_when(
    str_ends(second, " O") ~ word(second, 1, -2),
    str_ends(second, " Q") ~ word(second, 1, -2),
    str_ends(second, " SUSP") ~ word(second, 1, -2),
    str_ends(second, " IR") ~ word(second, 1, -2), 
    TRUE ~ second
  )) |>
  mutate(second = clean_player_names(second)) |> 
  mutate(third = case_when(
    str_ends(third, " O") ~ word(third, 1, -2),
    str_ends(third, " Q") ~ word(third, 1, -2),
    str_ends(third, " SUSP") ~ word(third, 1, -2),
    str_ends(third, " IR") ~ word(third, 1, -2), 
    TRUE ~ third
  )) |>
  mutate(third = clean_player_names(third)) |>
  mutate(fourth = case_when(
    str_ends(fourth, " O") ~ word(fourth, 1, -2),
    str_ends(fourth, " Q") ~ word(fourth, 1, -2),
    str_ends(fourth, " SUSP") ~ word(fourth, 1, -2),
    str_ends(fourth, " IR") ~ word(fourth, 1, -2), 
    TRUE ~ fourth
  )) |>
  mutate(fourth = clean_player_names(fourth)) |>
  mutate(team_abbr = clean_team_abbrs(team_abbr)) |> 
  pivot_longer(cols = c(starter, second, third, fourth), 
               names_to = "depth", values_to = "name") |> 
  mutate(depth = case_when(
    depth == "starter" ~ 1, 
    depth == "second" ~ 2, 
    depth == "third" ~ 3, 
    depth == "fourth" ~ 4
  )) |> 
  mutate(position_roster = case_when(
    position %in% c("LT", "RT") ~ "OT", 
    position %in% c("LG", "RG") ~ "OG", 
    position %in% c("LWR", "RWR", "SWR") ~ "WR", 
    position %in% c("WLB", "SLB") & def_style == "Base 3-4 D" ~ "EDGE", 
    position %in% c("LILB", "RILB", "MLB", "WLB", "SLB") ~ "LB", 
    position %in% c("LDE", "RDE") & def_style == "Base 3-4 D" ~ "DL", 
    position %in% c("LDE", "RDE") ~ "EDGE", 
    position %in% c("LDT", "RDT", "NT") ~ "DL", 
    position %in% c("FS", "SS") ~ "S", 
    position %in% c("LT", "RT") ~ "OT", 
    position %in% c("LCB", "RCB", "NB") ~ "CB", 
    TRUE ~ position
  ), .after = position) |> 
  mutate(position_depth_chart = case_when(
    position %in% c("WLB") & def_style == "Base 3-4 D" ~ "LOLB", 
    position %in% c("SLB") & def_style == "Base 3-4 D" ~ "ROLB", 
    position %in% c("LDE") & def_style == "Base 3-4 D" ~ "LDT", 
    position %in% c("RDE") & def_style == "Base 3-4 D" ~ "RDT", 
    TRUE ~ position
  ), .after = position) |> 
  filter(!is.na(name)) |> 
  mutate(rank = row_number(), .by = team_abbr) |> 
  arrange(team_abbr, position_roster, depth)

depth_charts <- df_clean |> 
  select(team_abbr, position_roster, position_depth_chart, name)

defense_style <- df_clean |> 
  select(team_abbr, def_style) |> 
  distinct()

saveRDS(depth_charts, paste0("Data/", "espn_depth_charts.rds"))

saveRDS(defense_style, paste0("Data/", "espn_defense_style.rds"))
