# GET <- memoise(httr::GET)




# declare variables for R CMD check
utils::globalVariables(c("attendee_count", "internal_count"))


embedded_lists <- function(x){
  el <- x %>% map_lgl(is.list)
  el[el] %>% names()
}

rename_embedded_lists <- function(x){
  el <- embedded_lists(x)
  x[el] <- lapply(seq_along(el), function(i){
    el_i_name <- names(x[el][i])
    this <- x[el][[i]]
    names(this) <- paste(el_i_name, names(this), sep = "_")
    this
  })
  x
}

squash_without_warning <- function(x)suppressWarnings(squash(x))


utils::globalVariables(c(".", "created", "updated", "start_date", "end_date", "end_dateTime", "start_dateTime"))


pull_or_na <- function(df, varname, fn = as_date) {
  if (varname %in% names(df)) df %>% dplyr::pull(!!varname) else NA
}


transform_or_na <- function(df, varname, fn = as_date) {
  if (varname %in% names(df)) df %>% dplyr::pull(!!varname) %>% fn() else fn(NA)
}


# declare variables for R CMD check
utils::globalVariables(c("date", "dateTime", "summary", "description", "attendees"))
utils::globalVariables(c("attendee_count", "internal_count"))

extract_meetings <- function(events){
  events %>%
    map(~squash_if(is.data.frame)) %>%
    bind_cols() %>%
    select(date, dateTime, summary, description, attendees) %>%
    mutate(
      date = if_else(!is.na(date), lubridate::date(date), lubridate::date(dateTime)),
      attendee_count = map_int(events$attendees, ~length(.$email)),
      internal_count = map_int(events$attendees, ~sum(grepl("@rstudio.com", .$email))),
      external = attendee_count > internal_count,
      internal = attendee_count == internal_count
    ) %>%
    select(
      -one_of("dateTime")
    )

}


