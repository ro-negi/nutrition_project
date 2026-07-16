install.packages(c(
  "dplyr",
  "readr",
  "readxl",
  "stringr",
  "lubridate",
  "tidyr",
  "purrr",
  "tibble"
))

# ============================================================
# CLEAN FOCAL DATA
# ============================================================

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)
library(tibble)


# ============================================================
# 0. FILE LOCATIONS
# ============================================================

project_root <- "C:/Users/rohit_negi/Desktop/Nutrition_RN"

raw_file <- file.path(
  project_root,
  "data",
  "clean",
  "2024.03.12 - 2026.04.01_focal_data_clean_RN.csv"
)

support_basename <- file.path(
  project_root,
  "data",
  "clean",
  "Subj_ID_Naming_List_2024_11"
)

output_dir <- file.path(
  project_root,
  "analysis"
)

output_file <- file.path(
  output_dir,
  "2025.12.01 - 2026.04.01_focal_data_cleaned_final_RN.csv"
)

# Create the analysis folder if it does not exist.
if (!dir.exists(output_dir)) {
  dir.create(
    output_dir,
    recursive = TRUE
  )
}


# ============================================================
# HELPER FUNCTIONS
# ============================================================

find_supporting_file <- function(base) {
  
  possible_files <- paste0(
    base,
    c(".csv", ".xlsx", ".xls")
  )
  
  existing_files <- possible_files[
    file.exists(possible_files)
  ]
  
  if (length(existing_files) == 0) {
    stop(
      paste0(
        "Supporting file not found. Expected one of: ",
        paste(
          possible_files,
          collapse = ", "
        )
      )
    )
  }
  
  existing_files[1]
}


is_missing_value <- function(x) {
  
  missing_strings <- c(
    "",
    "na",
    "n/a",
    "nan",
    "none",
    "null",
    "."
  )
  
  is.na(x) |
    (
      is.character(x) &
        str_to_lower(str_trim(x)) %in%
        missing_strings
    )
}


standardize_missing_character <- function(x) {
  
  x <- str_trim(x)
  
  missing_strings <- c(
    "",
    "na",
    "n/a",
    "nan",
    "none",
    "null",
    "."
  )
  
  x[
    str_to_lower(x) %in%
      missing_strings
  ] <- NA_character_
  
  x
}


case_insensitive_equal <- function(x, value) {
  
  !is.na(x) &
    str_to_lower(str_trim(x)) ==
    str_to_lower(str_trim(value))
}


parse_capture_date <- function(x) {
  
  suppressWarnings(
    parse_date_time(
      x,
      orders = c(
        "ymd",
        "Ymd",
        "mdy",
        "dmy",
        "Y.m.d",
        "m/d/Y",
        "d/m/Y"
      ),
      tz = "UTC",
      quiet = TRUE
    )
  )
}


parse_capture_datetime <- function(date, time) {
  
  combined <- paste(
    str_trim(as.character(date)),
    str_trim(as.character(time))
  )
  
  suppressWarnings(
    parse_date_time(
      combined,
      orders = c(
        "ymd HMS",
        "ymd HM",
        "Ymd HMS",
        "Ymd HM",
        "mdy HMS",
        "mdy HM",
        "dmy HMS",
        "dmy HM",
        "Y.m.d HMS",
        "Y.m.d HM"
      ),
      tz = "UTC",
      quiet = TRUE
    )
  )
}


parse_event_date <- function(x) {
  
  as.Date(
    x,
    format = "%Y.%m.%d"
  )
}


safe_integer <- function(x) {
  
  numeric_x <- suppressWarnings(
    as.numeric(x)
  )
  
  non_missing <- numeric_x[
    !is.na(numeric_x)
  ]
  
  if (
    length(non_missing) == 0 ||
    all(
      abs(
        non_missing -
        round(non_missing)
      ) < 1e-12
    )
  ) {
    return(as.integer(numeric_x))
  }
  
  numeric_x
}


# ============================================================
# FIND SUPPORTING FILE
# ============================================================

support_file <- find_supporting_file(
  support_basename
)

message(
  "Supporting file found: ",
  support_file
)


# ============================================================
# 1. LOAD RAW DATA
# ============================================================

if (!file.exists(raw_file)) {
  stop(
    paste0(
      "Raw focal-data file not found: ",
      raw_file
    )
  )
}

df <- read_csv(
  raw_file,
  show_col_types = FALSE,
  progress = FALSE,
  na = c(
    "",
    "NA",
    "N/A",
    "NaN",
    "nan",
    "None",
    "NULL",
    "."
  )
)

names(df) <- str_trim(
  names(df)
)

# Keep the original date and time columns as ordinary text.
# The combined parsed timestamp is stored separately in .capture_dt.
df <- df %>%
  mutate(
    `Date (Capture local)` = as.character(
      `Date (Capture local)`
    ),
    
    `Time (Capture local)` = as.character(
      `Time (Capture local)`
    )
  )

required_columns <- c(
  "Focal ID",
  "Date (Capture local)",
  "Time (Capture local)"
)

missing_columns <- setdiff(
  required_columns,
  names(df)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The following required columns are missing: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 2. BUILD UNIFIED TIMESTAMP
# ============================================================

df <- df %>%
  mutate(
    .original_row_number = row_number(),
    
    .capture_dt = parse_capture_datetime(
      `Date (Capture local)`,
      `Time (Capture local)`
    )
  )

bad_datetime_count <- sum(
  is.na(df$.capture_dt)
)

if (bad_datetime_count > 0) {
  warning(
    bad_datetime_count,
    " rows have an invalid date or time and could not be parsed."
  )
}


# ============================================================
# 3. ASSIGN FOCAL ID
# ============================================================

df <- df %>%
  arrange(
    .capture_dt,
    .original_row_number
  )

twenty_five_minutes <- 25 * 60

lip_exception_start <- ymd_hms(
  "2026-01-24 14:09:21",
  tz = "UTC"
)

lip_exception_end <- ymd_hms(
  "2026-01-24 15:12:53",
  tz = "UTC"
)

number_of_rows <- nrow(df)

focal_ids <- integer(
  number_of_rows
)

current_focal_id <- 0L
previous_subject <- NA_character_
previous_datetime <- as.POSIXct(
  NA,
  tz = "UTC"
)

previous_date <- as.Date(NA)

for (i in seq_len(number_of_rows)) {
  
  subject <- str_trim(
    as.character(
      df$`Focal ID`[i]
    )
  )
  
  current_datetime <- df$.capture_dt[i]
  
  current_date <- if (
    is.na(current_datetime)
  ) {
    as.Date(NA)
  } else {
    as.Date(current_datetime)
  }
  
  start_new_focal <- FALSE
  
  if (i == 1) {
    
    start_new_focal <- TRUE
    
  } else if (
    is.na(subject) ||
    is.na(previous_subject) ||
    subject != previous_subject
  ) {
    
    start_new_focal <- TRUE
    
  } else if (
    is.na(current_datetime) ||
    is.na(previous_datetime)
  ) {
    
    start_new_focal <- TRUE
    
  } else if (
    is.na(current_date) ||
    is.na(previous_date) ||
    current_date != previous_date
  ) {
    
    start_new_focal <- TRUE
    
  } else {
    
    gap_seconds <- as.numeric(
      difftime(
        current_datetime,
        previous_datetime,
        units = "secs"
      )
    )
    
    is_lip_exception <- (
      subject == "Lip" &&
        previous_datetime >= lip_exception_start &&
        previous_datetime <= lip_exception_end &&
        current_datetime >= lip_exception_start &&
        current_datetime <= lip_exception_end
    )
    
    if (
      gap_seconds > twenty_five_minutes &&
      !is_lip_exception
    ) {
      start_new_focal <- TRUE
    }
  }
  
  if (start_new_focal) {
    current_focal_id <- current_focal_id + 1L
  }
  
  focal_ids[i] <- current_focal_id
  previous_subject <- subject
  previous_datetime <- current_datetime
  previous_date <- current_date
}

df$focal_id <- focal_ids

# Move focal_id immediately after Focal ID.
df <- df %>%
  relocate(
    focal_id,
    .after = `Focal ID`
  )


# ============================================================
# 4. NORMALIZE WEATHER
# ============================================================

if ("Weather" %in% names(df)) {
  
  df <- df %>%
    mutate(
      Weather = case_when(
        str_detect(
          Weather,
          regex(
            "^\\s*heavy\\s+rain\\s*$",
            ignore_case = TRUE
          )
        ) ~ "Heavy rain",
        
        TRUE ~ str_trim(
          as.character(Weather)
        )
      )
    )
}


# ============================================================
# 5. CALCULATE DOMINANT LANDSCAPE
# ============================================================

landscape_column <- paste(
  "Landscape at the beginning",
  "of the focal"
)

if (landscape_column %in% names(df)) {
  
  df <- df %>%
    mutate(
      .landscape_norm = case_when(
        .data[[landscape_column]] %in% c(
          "Mangrove",
          "Big mangrove",
          "Big mangrove area"
        ) ~ "Mangrove",
        
        .data[[landscape_column]] ==
          "Shore" ~ "Shore",
        
        .data[[landscape_column]] ==
          "Forest" ~ "Forest",
        
        TRUE ~ str_trim(
          as.character(
            .data[[landscape_column]]
          )
        )
      )
    ) %>%
    arrange(
      focal_id,
      .capture_dt,
      .original_row_number
    ) %>%
    group_by(focal_id) %>%
    mutate(
      .next_dt = lead(.capture_dt),
      
      .row_duration_seconds = as.numeric(
        difftime(
          .next_dt,
          .capture_dt,
          units = "secs"
        )
      ),
      
      .row_duration_seconds = case_when(
        is.na(.row_duration_seconds) ~ 0,
        .row_duration_seconds < 0 ~ 0,
        TRUE ~ .row_duration_seconds
      )
    ) %>%
    ungroup()
  
  dominant_landscape_table <- df %>%
    group_by(
      focal_id,
      .landscape_norm
    ) %>%
    summarise(
      total_seconds = sum(
        .row_duration_seconds,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    arrange(
      focal_id,
      desc(total_seconds)
    ) %>%
    group_by(focal_id) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(
      focal_id,
      dominant_landscape =
        .landscape_norm
    )
  
  df <- df %>%
    left_join(
      dominant_landscape_table,
      by = "focal_id"
    ) %>%
    relocate(
      dominant_landscape,
      .after = all_of(
        landscape_column
      )
    ) %>%
    select(
      -any_of(
        c(
          ".landscape_norm",
          ".next_dt",
          ".row_duration_seconds"
        )
      )
    )
}


# ============================================================
# 6. CORRECT SUBJECT NAMES GLOBALLY
# ============================================================

name_map <- c(
  "Lady Boy" = "Lady_boy",
  "Ladyboy" = "Lady_boy",
  "Legolas" = "Lady_boy",
  "Vulva" = "Vula",
  "Jim Lim" = "Jim_lim",
  "Tin tin" = "Tin_tin",
  "Harely" = "Harley"
)

df <- df %>%
  mutate(
    across(
      where(is.character),
      ~ str_trim(.x)
    )
  ) %>%
  mutate(
    across(
      where(is.character),
      ~ recode(
        .x,
        !!!name_map,
        .default = .x
      )
    )
  )


# ============================================================
# 7. ADD SEX
# ============================================================

if (
  str_ends(
    str_to_lower(support_file),
    ".csv"
  )
) {
  
  sex_df <- read_csv(
    support_file,
    show_col_types = FALSE,
    progress = FALSE,
    na = c(
      "",
      "NA",
      "N/A",
      "NaN",
      "nan",
      "None",
      "NULL",
      "."
    )
  )
  
} else {
  
  sex_df <- read_excel(
    support_file
  )
}

names(sex_df) <- str_trim(
  names(sex_df)
)

required_support_columns <- c(
  "Focal ID",
  "sex"
)

missing_support_columns <- setdiff(
  required_support_columns,
  names(sex_df)
)

if (
  length(missing_support_columns) > 0
) {
  stop(
    paste0(
      "The supporting file is missing: ",
      paste(
        missing_support_columns,
        collapse = ", "
      )
    )
  )
}

sex_df <- sex_df %>%
  mutate(
    `Focal ID` = str_trim(
      as.character(`Focal ID`)
    ),
    
    `Focal ID` = recode(
      `Focal ID`,
      !!!name_map,
      .default = `Focal ID`
    ),
    
    .sex_original = str_to_lower(
      str_trim(
        as.character(sex)
      )
    ),
    
    sex = case_when(
      .sex_original %in% c(
        "f",
        "female",
        "♀"
      ) ~ "f",
      
      .sex_original %in% c(
        "m",
        "male",
        "♂"
      ) ~ "m",
      
      str_starts(
        .sex_original,
        "f"
      ) ~ "f",
      
      str_starts(
        .sex_original,
        "m"
      ) ~ "m",
      
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    `Focal ID`,
    sex
  ) %>%
  distinct()

# Check whether any subject has conflicting sex entries.
sex_conflicts <- sex_df %>%
  filter(!is.na(sex)) %>%
  distinct(
    `Focal ID`,
    sex
  ) %>%
  count(
    `Focal ID`,
    name = "number_of_sexes"
  ) %>%
  filter(number_of_sexes > 1)

if (nrow(sex_conflicts) > 0) {
  stop(
    paste0(
      "Conflicting sex entries were found for: ",
      paste(
        sex_conflicts$`Focal ID`,
        collapse = ", "
      )
    )
  )
}

main_names <- unique(
  as.character(
    df$`Focal ID`
  )
)

support_names <- unique(
  as.character(
    sex_df$`Focal ID`
  )
)

matching_names <- intersect(
  main_names,
  support_names
)

message(
  "[sex merge] names in main: ",
  length(main_names),
  " | names in support: ",
  length(support_names),
  " | intersection: ",
  length(matching_names)
)

df <- df %>%
  left_join(
    sex_df,
    by = "Focal ID"
  ) %>%
  mutate(
    sex = case_when(
      str_detect(
        as.character(`Focal ID`),
        fixed("?")
      ) ~ "f",
      
      `Focal ID` %in% c(
        "Lady_boy",
        "Babosa"
      ) ~ "m",
      
      TRUE ~ sex
    )
  ) %>%
  relocate(
    sex,
    .after = focal_id
  )


# ============================================================
# 8. CORRECT REPRODUCTIVE STATES
# ============================================================

if (
  !"Reproductive state" %in%
  names(df)
) {
  df$`Reproductive state` <-
    NA_character_
}

df <- df %>%
  mutate(
    repro_state_corrected =
      as.character(
        `Reproductive state`
      ),
    
    .date_only = as.Date(
      .capture_dt
    )
  )

reproductive_events <- tribble(
  ~female, ~birth, ~stop,
  
  "Aura", "2024.05.17", "2024.07.31",
  "Aura", "2025.04.12", "2026.01.04",
  
  "Cacao", "2025.03.26", NA_character_,
  
  "Dandi", "2025.06.01", NA_character_,
  
  "Flame", "2024.05.11", "2024.06.23",
  "Flame", "2025.05.20", NA_character_,
  
  "Granny", "2024.03.31", "2025.03.31",
  "Granny", "2026.06.10", NA_character_,
  
  "Harley", "2023.05.23", NA_character_,
  "Harley", "2025.04.11", NA_character_,
  
  "Iyla", "2024.05.18", "2024.05.28",
  "Iyla", "2025.04.24", NA_character_,
  
  "Jim_lim", "2024.03.29", "2024.03.30",
  "Jim_lim", "2025.02.28", NA_character_,
  
  "Kaya", "2024.03.29", "2024.06.13",
  "Kaya", "2025.03.09", NA_character_,
  
  "Lip", "2024.04.07", "2025.04.07",
  "Lip", "2026.01.24", NA_character_,
  
  "Manee", "2025.05.28", NA_character_,
  
  "Naina", "2024.11.26", "2025.02.25",
  "Naina", "2026.01.10", "2026.02.19",
  
  "Oreo", "2025.04.14", "2025.08.10",
  "Oreo", "2026.03.23", NA_character_,
  
  "Quindoline", "2024.05.25", "2025.05.25",
  
  "Robin", "2025.04.11", NA_character_,
  
  "Taipan", "2025.04.10", NA_character_,
  
  "Vula", "2024.05.07", "2025.05.07",
  
  "Whitney", "2025.06.25", NA_character_,
  
  "Yaiko", "2025.04.05", NA_character_
) %>%
  mutate(
    birth_date = parse_event_date(
      birth
    ),
    
    stop_date = parse_event_date(
      stop
    ),
    
    pregnancy_start =
      birth_date - 150
  ) %>%
  arrange(
    female,
    birth_date
  ) %>%
  group_by(female) %>%
  mutate(
    next_birth_date = lead(
      birth_date
    ),
    
    next_pregnancy_start =
      next_birth_date - 150
  ) %>%
  ungroup()

for (
  event_number in
  seq_len(
    nrow(reproductive_events)
  )
) {
  
  event <- reproductive_events[
    event_number,
  ]
  
  female_name <- event$female
  birth_date <- event$birth_date
  stop_date <- event$stop_date
  
  pregnancy_start <-
    event$pregnancy_start
  
  next_pregnancy_start <-
    event$next_pregnancy_start
  
  female_mask <- (
    df$`Focal ID` == female_name &
      df$sex == "f"
  )
  
  # Pregnancy:
  # from 150 days before birth until
  # the day before birth.
  pregnancy_mask <- (
    female_mask &
      !is.na(df$.date_only) &
      df$.date_only >= pregnancy_start &
      df$.date_only < birth_date
  )
  
  df$repro_state_corrected[
    pregnancy_mask
  ] <- "Pregnant"
  
  # Lactation.
  if (!is.na(stop_date)) {
    
    lactation_mask <- (
      female_mask &
        !is.na(df$.date_only) &
        df$.date_only >= birth_date &
        df$.date_only < stop_date
    )
    
  } else if (
    !is.na(next_pregnancy_start)
  ) {
    
    lactation_mask <- (
      female_mask &
        !is.na(df$.date_only) &
        df$.date_only >= birth_date &
        df$.date_only <
        next_pregnancy_start
    )
    
  } else {
    
    lactation_mask <- (
      female_mask &
        !is.na(df$.date_only) &
        df$.date_only >= birth_date
    )
  }
  
  df$repro_state_corrected[
    lactation_mask
  ] <- "Lactating"
  
  # Non-lactating and non-pregnant,
  # beginning on the known stop date.
  if (!is.na(stop_date)) {
    
    if (
      !is.na(next_pregnancy_start)
    ) {
      
      non_reproductive_mask <- (
        female_mask &
          !is.na(df$.date_only) &
          df$.date_only >= stop_date &
          df$.date_only <
          next_pregnancy_start
      )
      
    } else {
      
      non_reproductive_mask <- (
        female_mask &
          !is.na(df$.date_only) &
          df$.date_only >= stop_date
      )
    }
    
    df$repro_state_corrected[
      non_reproductive_mask
    ] <- "Non-lactating & non-pregnant"
  }
}

df <- df %>%
  relocate(
    repro_state_corrected,
    .after = `Reproductive state`
  )


# ============================================================
# 9. PAD FOCALS SHORTER THAN 15 MINUTES
# ============================================================

if (
  !"Behavior category" %in%
  names(df)
) {
  df$`Behavior category` <-
    NA_character_
}

df <- df %>%
  arrange(
    focal_id,
    .capture_dt,
    .original_row_number
  )

copy_columns <- c(
  "Date (Capture local)",
  "Weather",
  "Wind",
  "Tide level",
  "Landscape at the beginning of the focal",
  "dominant_landscape",
  "Shore/mangrove available (m)",
  "Group tool use activity (>5)",
  "Focal ID",
  "Oestrus",
  "Reproductive state",
  "Infant",
  "focal_id",
  "sex",
  "General notes",
  "repro_state_corrected",
  "Username",
  "Mean distance from the focal individual",
  "Hand used with tools"
)

copy_columns <- intersect(
  copy_columns,
  names(df)
)

focal_groups <- split(
  df,
  df$focal_id
)

padding_rows <- vector(
  mode = "list",
  length = 0
)

for (
  focal_group in focal_groups
) {
  
  valid_datetimes <- focal_group$.capture_dt[
    !is.na(focal_group$.capture_dt)
  ]
  
  if (
    length(valid_datetimes) == 0
  ) {
    next
  }
  
  start_datetime <- min(
    valid_datetimes
  )
  
  end_datetime <- max(
    valid_datetimes
  )
  
  duration_seconds <- as.numeric(
    difftime(
      end_datetime,
      start_datetime,
      units = "secs"
    )
  )
  
  if (
    is.na(duration_seconds) ||
    duration_seconds >= 15 * 60
  ) {
    next
  }
  
  target_datetime <- (
    start_datetime +
      minutes(15)
  )
  
  last_row <- focal_group %>%
    arrange(.capture_dt) %>%
    slice_tail(n = 1)
  
  new_row <- df[
    rep(NA_integer_, 1),
    ,
    drop = FALSE
  ]
  
  for (column_name in names(new_row)) {
    
    if (
      inherits(
        df[[column_name]],
        "POSIXct"
      )
    ) {
      new_row[[column_name]] <-
        as.POSIXct(
          NA,
          tz = "UTC"
        )
      
    } else if (
      inherits(
        df[[column_name]],
        "Date"
      )
    ) {
      new_row[[column_name]] <-
        as.Date(NA)
      
    } else {
      new_row[[column_name]] <-
        NA
    }
  }
  
  for (
    column_name in copy_columns
  ) {
    new_row[[column_name]] <-
      last_row[[column_name]]
  }
  
  new_row$.capture_dt <-
    target_datetime
  
  new_row$.date_only <-
    as.Date(target_datetime)
  
  new_row$`Date (Capture local)` <-
    format(
      target_datetime,
      "%m/%d/%Y"
    )
  
  new_row$`Time (Capture local)` <-
    format(
      target_datetime,
      "%H:%M:%S"
    )
  
  new_row$`Behavior category` <-
    "NOT VISIBLE"
  
  new_row$.original_row_number <-
    max(
      df$.original_row_number,
      na.rm = TRUE
    ) +
    length(padding_rows) +
    1
  
  padding_rows[[length(padding_rows) + 1]] <- new_row
}

if (length(padding_rows) > 0) {
  
  padding_df <- bind_rows(
    padding_rows
  )
  
  df <- bind_rows(
    df,
    padding_df
  ) %>%
    arrange(
      focal_id,
      .capture_dt,
      .original_row_number
    )
}

message(
  "Added ",
  length(padding_rows),
  " padding rows to focals shorter than 15 minutes."
)


# ============================================================
# 10. FIX STRIKES AND SUCCESS
# ============================================================

strikes_column <- paste(
  "Number of",
  "strikes/poundings"
)

success_column <- paste(
  "Success to open",
  "and feed"
)

if (
  !strikes_column %in%
  names(df)
) {
  df[[strikes_column]] <- NA_integer_
}

if (
  !success_column %in%
  names(df)
) {
  df[[success_column]] <- NA_character_
}

df[[strikes_column]] <- abs(
  suppressWarnings(
    as.numeric(
      df[[strikes_column]]
    )
  )
)

non_integer_strikes <- (
  !is.na(df[[strikes_column]]) &
    abs(
      df[[strikes_column]] -
        round(df[[strikes_column]])
    ) >= 1e-12
)

if (any(non_integer_strikes)) {
  stop(
    "Non-integer values were found in Number of strikes/poundings."
  )
}

df[[strikes_column]] <- as.integer(
  df[[strikes_column]]
)

success_missing <- is_missing_value(
  df[[success_column]]
)

df[[success_column]][
  success_missing &
    !is.na(df[[strikes_column]]) &
    df[[strikes_column]] == 0
] <- "No"

df[[success_column]][
  success_missing &
    !is.na(df[[strikes_column]]) &
    df[[strikes_column]] > 0
] <- "Yes"


# ============================================================
# 11. BEHAVIOUR BACKFILL
# ============================================================

is_not_visible <- function(x) {
  
  !is.na(x) &
    str_to_lower(
      str_trim(
        as.character(x)
      )
    ) == "not visible"
}

df <- df %>%
  arrange(
    focal_id,
    .capture_dt,
    .original_row_number
  )

processed <- 0L
filled <- 0L
no_source <- 0L

focal_id_values <- unique(
  df$focal_id
)

for (
  current_focal_id in focal_id_values
) {
  
  focal_indices <- which(
    df$focal_id ==
      current_focal_id
  )
  
  focal_indices <- focal_indices[
    order(
      df$.capture_dt[
        focal_indices
      ],
      na.last = TRUE
    )
  ]
  
  if (
    length(focal_indices) < 2
  ) {
    next
  }
  
  last_index <- tail(
    focal_indices,
    1
  )
  
  second_last_index <- tail(
    focal_indices,
    2
  )[1]
  
  if (
    !is_not_visible(
      df$`Behavior category`[
        last_index
      ]
    )
  ) {
    next
  }
  
  processed <- processed + 1L
  
  if (
    is_missing_value(
      df$`Behavior category`[
        second_last_index
      ]
    )
  ) {
    
    earlier_indices <- head(
      focal_indices,
      -1
    )
    
    earlier_behaviours <-
      df$`Behavior category`[
        earlier_indices
      ]
    
    valid_behaviours <-
      earlier_behaviours[
        !is_missing_value(
          earlier_behaviours
        )
      ]
    
    if (
      length(valid_behaviours) > 0
    ) {
      
      df$`Behavior category`[
        second_last_index
      ] <- tail(
        valid_behaviours,
        1
      )
      
      filled <- filled + 1L
      
    } else {
      
      no_source <- no_source + 1L
    }
  }
}

message(
  "[Second-last Behavior fix] padded focals: ",
  processed,
  " | filled: ",
  filled,
  " | no-source: ",
  no_source
)

filled_last <- 0L
total_focals <- 0L

for (
  current_focal_id in focal_id_values
) {
  
  total_focals <- total_focals + 1L
  
  focal_indices <- which(
    df$focal_id ==
      current_focal_id
  )
  
  focal_indices <- focal_indices[
    order(
      df$.capture_dt[
        focal_indices
      ],
      na.last = TRUE
    )
  ]
  
  if (
    length(focal_indices) == 0
  ) {
    next
  }
  
  last_index <- tail(
    focal_indices,
    1
  )
  
  if (
    is_missing_value(
      df$`Behavior category`[
        last_index
      ]
    )
  ) {
    
    focal_behaviours <-
      df$`Behavior category`[
        focal_indices
      ]
    
    valid_behaviours <-
      focal_behaviours[
        !is_missing_value(
          focal_behaviours
        )
      ]
    
    if (
      length(valid_behaviours) > 0
    ) {
      
      df$`Behavior category`[
        last_index
      ] <- tail(
        valid_behaviours,
        1
      )
      
      filled_last <-
        filled_last + 1L
    }
  }
}

message(
  "[Last-row Behavior fill] focals processed: ",
  total_focals,
  " | filled: ",
  filled_last
)


# ============================================================
# 12. MEALYBUG CORRECTION
# ============================================================

species_column <- "Insect species"
leaves_licking_column <- "Leaves licking"
number_items_column <- "Number of item eaten"
number_bites_column <- "Number of bites"
number_cheeks_column <- "Number of cheek-pouch full"

required_mealybug_columns <- c(
  species_column,
  leaves_licking_column,
  number_items_column,
  number_bites_column,
  number_cheeks_column
)

for (
  column_name in
  required_mealybug_columns
) {
  
  if (
    !column_name %in%
    names(df)
  ) {
    df[[column_name]] <- NA
  }
}

text_columns <- names(df)[
  vapply(
    df,
    is.character,
    logical(1)
  )
]

mealybug_scan_columns <- setdiff(
  text_columns,
  species_column
)

found_mealybug_elsewhere <- rep(
  FALSE,
  nrow(df)
)

for (
  column_name in
  mealybug_scan_columns
) {
  
  mealybug_mask <- (
    !is.na(df[[column_name]]) &
      str_to_lower(
        str_trim(
          df[[column_name]]
        )
      ) == "mealybug"
  )
  
  if (any(mealybug_mask)) {
    
    df[[column_name]][
      mealybug_mask
    ] <- NA_character_
    
    found_mealybug_elsewhere <- (
      found_mealybug_elsewhere |
        mealybug_mask
    )
  }
}

species_missing_before <- is_missing_value(
  df[[species_column]]
)

mealybug_from_text <- (
  found_mealybug_elsewhere &
    species_missing_before
)

df[[species_column]][
  mealybug_from_text
] <- "Mealybug"

leaves_licking_yes <- (
  !is.na(
    df[[leaves_licking_column]]
  ) &
    str_to_lower(
      str_trim(
        as.character(
          df[[leaves_licking_column]]
        )
      )
    ) == "yes"
)

species_missing_after <- is_missing_value(
  df[[species_column]]
)

mealybug_from_leaves <- (
  leaves_licking_yes &
    species_missing_after
)

df[[species_column]][
  mealybug_from_leaves
] <- "Mealybug"

mealybug_added <- (
  mealybug_from_text |
    mealybug_from_leaves
)

number_items <- suppressWarnings(
  as.numeric(
    df[[number_items_column]]
  )
)

number_bites <- suppressWarnings(
  as.numeric(
    df[[number_bites_column]]
  )
)

number_cheeks <- suppressWarnings(
  as.numeric(
    df[[number_cheeks_column]]
  )
)

all_counts_missing <- (
  is.na(number_items) &
    is.na(number_bites) &
    is.na(number_cheeks)
)

df[[number_bites_column]][
  mealybug_added &
    all_counts_missing
] <- 1

df[[number_bites_column]] <-
  safe_integer(
    df[[number_bites_column]]
  )


# ============================================================
# 13. FOOD AND SPECIES CORRECTIONS
# ============================================================

food_column <- "Food"
food_tools_column <- "Food with tools"

other_food_tools_column <- paste(
  "Other food with tools"
)

fruit_tool_column <- paste(
  "Fruit/nut species with tool"
)

fruit_column <- "Fruit/nut species"

shellfish_tool_column <- paste(
  "Shellfish species with tool"
)

other_shellfish_tool_column <- paste(
  "Other shellfish with tool"
)

foraging_behaviour_column <- paste(
  "Foraging behaviours"
)

food_columns <- c(
  food_column,
  food_tools_column,
  other_food_tools_column,
  fruit_tool_column,
  fruit_column,
  shellfish_tool_column,
  other_shellfish_tool_column,
  foraging_behaviour_column
)

for (
  column_name in food_columns
) {
  
  if (
    !column_name %in%
    names(df)
  ) {
    df[[column_name]] <-
      NA_character_
  }
  
  df[[column_name]] <-
    as.character(
      df[[column_name]]
    )
}


# Sea cucumber:
# Other food with tools -> Food with tools.
sea_cucumber_mask <-
  case_insensitive_equal(
    df[[other_food_tools_column]],
    "Sea cucumber"
  )

df[[food_tools_column]][
  sea_cucumber_mask
] <- "Sea cucumber"

df[[other_food_tools_column]][
  sea_cucumber_mask
] <- NA_character_


# Sea slaters:
# Other food with tools -> Food.
sea_slaters_other_food_mask <-
  case_insensitive_equal(
    df[[other_food_tools_column]],
    "Sea slaters"
  )

df[[food_column]][
  sea_slaters_other_food_mask
] <- "Sea slaters"

df[[other_food_tools_column]][
  sea_slaters_other_food_mask
] <- NA_character_


# Nerita balteata:
# Fruit/nut with tools -> Shellfish with tools.
nerita_fruit_mask <-
  case_insensitive_equal(
    df[[fruit_tool_column]],
    "Nerita balteata"
  )

df[[shellfish_tool_column]][
  nerita_fruit_mask
] <- "Nerita balteata"

df[[fruit_tool_column]][
  nerita_fruit_mask
] <- NA_character_


# Sonneratia alba:
# Tool column -> non-tool column when recorded
# as "without tools".
sonneratia_mask <-
  case_insensitive_equal(
    df[[fruit_tool_column]],
    "Sonneratia alba"
  )

without_tools_mask <-
  case_insensitive_equal(
    df[[foraging_behaviour_column]],
    "without tools"
  )

sonneratia_move_mask <- (
  sonneratia_mask &
    without_tools_mask
)

df[[fruit_column]][
  sonneratia_move_mask
] <- "Sonneratia alba"

df[[fruit_tool_column]][
  sonneratia_move_mask
] <- NA_character_


# Remove numeric-only shellfish entries.
numeric_shellfish_mask <- (
  !is.na(
    df[[shellfish_tool_column]]
  ) &
    str_detect(
      str_trim(
        df[[shellfish_tool_column]]
      ),
      "^\\d+(\\.\\d+)?$"
    )
)

df[[shellfish_tool_column]][
  numeric_shellfish_mask
] <- NA_character_


# Sea slaters:
# Shellfish with tools -> Food.
sea_slaters_shellfish_mask <-
  case_insensitive_equal(
    df[[shellfish_tool_column]],
    "Sea slaters"
  )

df[[food_column]][
  sea_slaters_shellfish_mask
] <- "Sea slaters"

df[[shellfish_tool_column]][
  sea_slaters_shellfish_mask
] <- NA_character_


# Standardize Nerita spellings.
shellfish_typo_map <- c(
  "black" = "Nerita balteata",
  "n. black" = "Nerita balteata",
  "nerita bateata" = "Nerita balteata",
  "nerita balteata" = "Nerita balteata",
  "nerita chameleon" = "Nerita chamaeleon",
  "nerita chamaeleon" = "Nerita chamaeleon",
  "nerita sp." = "Nerita",
  "nerita" = "Nerita"
)

shellfish_lower <- str_to_lower(
  str_trim(
    df[[shellfish_tool_column]]
  )
)

shellfish_matches <- (
  !is.na(shellfish_lower) &
    shellfish_lower %in%
    names(shellfish_typo_map)
)

df[[shellfish_tool_column]][
  shellfish_matches
] <- unname(
  shellfish_typo_map[
    shellfish_lower[
      shellfish_matches
    ]
  ]
)


# Sea slaters:
# Other shellfish with tools -> Food.
sea_slaters_other_shellfish_mask <-
  case_insensitive_equal(
    df[[other_shellfish_tool_column]],
    "Sea slaters"
  )

df[[food_column]][
  sea_slaters_other_shellfish_mask
] <- "Sea slaters"

df[[other_shellfish_tool_column]][
  sea_slaters_other_shellfish_mask
] <- NA_character_


# Standardize Atrina vexillum.
atrina_map <- c(
  "atrina vexillum" = "Atrina vexillum",
  "big oyster thingy" = "Atrina vexillum",
  "prev too" = "Atrina vexillum",
  "big oyster thingy, prev too" = "Atrina vexillum"
)

other_shellfish_original <- str_to_lower(
  str_trim(
    df[[other_shellfish_tool_column]]
  )
)

atrina_matches <- (
  !is.na(other_shellfish_original) &
    other_shellfish_original %in%
    names(atrina_map)
)

df[[other_shellfish_tool_column]][
  atrina_matches
] <- unname(
  atrina_map[
    other_shellfish_original[
      atrina_matches
    ]
  ]
)

combined_atrina_mask <- (
  !is.na(other_shellfish_original) &
    other_shellfish_original ==
    "big oyster thingy, prev too"
)

df[[shellfish_tool_column]][
  combined_atrina_mask
] <- "Atrina vexillum"

df[[other_shellfish_tool_column]][
  combined_atrina_mask
] <- NA_character_

message(
  "[Food/species] Moves and corrections completed."
)


# ============================================================
# 14. OYSTER KEYWORD COUNT CORRECTION
# ============================================================

for (
  column_name in c(
    number_items_column,
    number_bites_column,
    number_cheeks_column
  )
) {
  
  if (
    !column_name %in%
    names(df)
  ) {
    df[[column_name]] <- NA
  }
}

oyster_pattern <- paste0(
  "oyster\\s+sessile\\s*\\(attached\\)",
  "|oyster\\s+sessile",
  "|oyster\\s+non-?sessile"
)

text_columns <- names(df)[
  vapply(
    df,
    is.character,
    logical(1)
  )
]

oyster_mask <- rep(
  FALSE,
  nrow(df)
)

for (
  column_name in text_columns
) {
  
  current_mask <- str_detect(
    str_to_lower(
      coalesce(
        df[[column_name]],
        ""
      )
    ),
    oyster_pattern
  )
  
  oyster_mask <- (
    oyster_mask |
      current_mask
  )
}

number_items <- suppressWarnings(
  as.numeric(
    df[[number_items_column]]
  )
)

number_bites <- suppressWarnings(
  as.numeric(
    df[[number_bites_column]]
  )
)

number_cheeks <- suppressWarnings(
  as.numeric(
    df[[number_cheeks_column]]
  )
)

all_oyster_counts_missing <- (
  is.na(number_items) &
    is.na(number_bites) &
    is.na(number_cheeks)
)

set_item_to_one <- (
  oyster_mask &
    all_oyster_counts_missing
)

df[[number_items_column]][
  set_item_to_one
] <- 1

df[[number_items_column]] <-
  safe_integer(
    df[[number_items_column]]
  )

message(
  "[Oyster count correction] Set '",
  number_items_column,
  "' to 1 on ",
  sum(set_item_to_one),
  " rows."
)


# ============================================================
# 15. FINAL CLEANUP
# ============================================================

df <- df %>%
  mutate(
    across(
      where(is.character),
      standardize_missing_character
    )
  ) %>%
  arrange(
    focal_id,
    .capture_dt,
    .original_row_number
  ) %>%
  select(
    -any_of(
      c(
        ".capture_dt",
        ".date_only",
        ".original_row_number"
      )
    )
  )


# ============================================================
# 16. SAVE CLEANED DATASET
# ============================================================

write_csv(
  df,
  output_file,
  na = ""
)

message(
  "Cleaned dataset saved to: ",
  output_file
)