library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(janitor)
library(nflreadr)

# Get Team Roster Data
teams_raw <- jsonlite::fromJSON(
  "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams",
  flatten = FALSE
)

sports <- teams_raw$sports

leagues <- sports$leagues[[1]] # first (and only) sport entry

teams <- leagues$teams[[1]]

team_ids <- teams |>
  dplyr::select(team) |>
  tidyr::unnest_wider(team) |>
  dplyr::select(team_abbr = abbreviation, team_id = id)


get_roster <- function(team_abbr, team_id) {
  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams/",
    team_abbr,
    "/roster"
  )

  resp <- request(url) |> req_perform()

  data <- resp_body_string(resp) |> jsonlite::fromJSON(flatten = TRUE)

  players <- data$athletes$items

  roster <- bind_rows(players) |>
    janitor::clean_names() |>
    mutate(team_abbr = team_abbr, team_id = team_id) |>
    select(
      team_abbr,
      team_id,
      espn_id = id,
      espn_alt_id = alternate_ids_sdr,
      first_name,
      last_name,
      display_name,
      short_name,
      position_roster = position_abbreviation,
      experience_years,
      status_name
    )

  roster
}


# Get Depth Chart data
get_depth <- function(
  team_abbr,
  team_id,
  season = get_current_season(roster = TRUE)
) {
  url <- paste0(
    "http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/",
    season,
    "/teams/",
    team_id,
    "/depthcharts?lang=en&region=us"
  )

  resp <- request(url) |> req_perform()

  txt <- resp_body_string(resp)

  depth <- jsonlite::fromJSON(txt, flatten = TRUE)

  items <- depth$items

  ath_cols <- names(items)[stringr::str_detect(
    names(items),
    "^positions\\.[a-z]{1,3}\\.athletes$"
  )]

  purrr::map_dfr(seq_len(nrow(items)), function(i) {
    row <- items[i, ]

    purrr::map_dfr(ath_cols, function(col) {
      tmp <- row[[col]][[1]]

      if (is.null(tmp) || nrow(tmp) == 0) {
        return(NULL)
      }

      tmp |>
        mutate(
          espn_id = stringr::str_extract(
            .data[["athlete.$ref"]],
            "(?<=athletes/)\\d+(?=\\?lang)"
          ),
          team_abbr = team_abbr,
          team_id = team_id,
          scheme_id = row$id[[1]],
          scheme_name = row$name[[1]],
          position_depth_chart = stringr::str_extract(
            col,
            "(?<=positions\\.)[a-z]{1,3}(?=\\.athletes)"
          ),
          # position = jefeR::clean_positions(position_espn, target = "clean"),
          # position_group = jefeR::clean_positions(position, target = "generic")
        )
    })
  })
}

all_rosters <- team_ids |>
  purrr::pmap_dfr(get_roster)

all_depth <- team_ids |>
  purrr::pmap_dfr(get_depth)

depth_charts <- all_depth |>
  mutate(espn_id = as.character(espn_id)) |>
  left_join(
    all_rosters |>
      mutate(espn_id = as.character(espn_id)),
    by = c("team_abbr", "team_id", "espn_id")
  ) |>
  mutate(taem_abbr = nflreadr::clean_team_abbrs(team_abbr)) |>
  mutate(position_depth_chart = toupper(position_depth_chart)) |>
  select(
    espn_id,
    espn_alt_id,
    first_name,
    last_name,
    display_name,
    short_name,
    position_roster,
    position_depth_chart,
    pos_slot = slot,
    pos_rank = rank,
    team_abbr,
    team_id,
    experience_years,
    scheme_id,
    scheme_name
  )

defense_styles <- depth_charts |>
  select(team_abbr, def_style = scheme_name) |>
  filter(str_starts(scheme_name, "Base")) |>
  distinct()


saveRDS(depth_charts, paste0("Data/", "espn_depth_charts.rds"))

saveRDS(defense_styles, paste0("Data/", "espn_defense_style.rds"))
