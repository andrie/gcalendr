utils::globalVariables(
  c("start", "end", "summary", "location", ".",
    "start_datetime", "start_timezone", "end_datetime", "end_timezone",
    "start_date", "end_date"))

transform_or_na <- function(df, varname, fn = as_date) {
  if (varname %in% names(df)) df %>% dplyr::pull(!!varname) %>% fn() else fn(NA)
}


# - extract_attendees -----------------------------------------------------


extract_attendees <- function(items){
  attendee_tibble <- function(x){
    tibble(
      email           = x[["email"]]                     %||% NA_character_,
      response_status = x[["responseStatus"]]            %||% NA_character_,
      organizer       = x[["organizer"]]                 %||% NA,
      optional        = x[["optional"]]                  %||% NA,
      display_name    = x[["displayName"]]               %||% NA_character_,
      self            = x[["self"]]                      %||% NA
    )
  }

  z <-
    items %>%
    map_dfr(
      ~tibble(
        id = pluck(., "id"),
        attendees = list(
          pluck(., "attendees") %>%
            map_dfr(
              attendee_tibble
            )
        )
      )
    )

  if (nrow(z) != length(items)) warning("Incorrect number of rows in attendees")
  z
}


# - extract_creator -------------------------------------------------------

extract_creator <- function(items){
z <-
  items %>%
  map_dfr(
    ~tibble(
      id = pluck(., "id"),
      creator = list(
        tibble(
          email        = .[["creator"]][["email"]]       %||% NA_character_,
          display_name = .[["creator"]][["displayName"]] %||% NA_character_,
          self         = .[["creator"]][["self"]]        %||% NA
        )
      )
    )
  )
  if (nrow(z) != length(items)) warning("Incorrect number of rows in creator")
  z
}


# extract_organizer -------------------------------------------------------

extract_organizer <- function(items){
  z <-
  items %>%
  map_dfr(
    ~tibble(
      id = pluck(., "id"),
      organizer = list(
        tibble(
          email        = .[["organizer"]][["email"]]       %||% NA_character_,
          display_name = .[["organizer"]][["displayName"]] %||% NA_character_,
          self         = .[["organizer"]][["self"]]        %||% NA
        )
      )
    )
  )
  if (nrow(z) != length(items)) warning("Incorrect number of rows in organizer")
  z

}



# - extract_simple_columns ------------------------------------------------

extract_simple_columns <- function(items){
  items %>%
    map_dfr(~tibble(
      kind                        = .[["kind"]] %||% NA,
      etag                        = .[["etag"]] %||% NA,
      id                          = .[["id"]] %||% NA,
      status                      = .[["status"]] %||% NA,
      html_link                   = .[["htmlLink"]] %||% NA,
      created                     = .[["created"]] %||% NA,
      updated                     = .[["updated"]] %||% NA,
      summary                     = .[["summary"]] %||% NA,
      location                    = .[["location"]] %||% NA,

      start_date                 = .[["start"]][["date"]] %||% NA,
      start_datetime             = .[["start"]][["dateTime"]] %||% NA,
      start_timezone             = .[["start"]][["timeZone"]] %||% NA,

      end_date                   = .[["end"]][["date"]] %||% NA,
      end_datetime               = .[["end"]][["dateTime"]] %||% NA,
      end_timezone               = .[["end"]][["timeZone"]] %||% NA,

      # recurrence                  = .[["recurrence"]] %||% NA,
      ical_uid                    = .[["iCalUID"]] %||% NA,
      sequence                    = .[["sequence"]] %||% NA,
      # extended_properties         = .[["extendedProperties"]] %||% NA,
      # reminders                   = .[["reminders"]] %||% NA,
      description                 = .[["description"]] %||% NA,
      recurring_event_id          = .[["recurringEventId"]] %||% NA,
      original_start_datetime     = .[["originalStartTime"]][["dateTime"]] %||% NA,
      original_start_timezone     = .[["originalStartTime"]][["timeZone"]] %||% NA,
      transparency                = .[["transparency"]] %||% NA,
      guests_can_modify           = .[["guestsCanModify"]] %||% NA,
      visibility                  = .[["visibility"]] %||% NA,
      guests_can_see_other_guests = .[["guestsCanSeeOtherGuests"]] %||% NA,
      guests_can_invite_others    = .[["guestsCanInviteOthers"]] %||% NA,
      private_copy                = .[["privateCopy"]] %||% NA,
      color_id                    = .[["colorId"]] %||% NA,

      organizer_email             = .[["organizer"]][["email"]]       %||% NA_character_,
      organizer_display_name      = .[["organizer"]][["displayName"]] %||% NA_character_,
      organizer_self              = .[["organizer"]][["self"]]        %||% NA,

      creator_email               = .[["creator"]][["email"]]       %||% NA_character_,
      creator_display_name        = .[["creator"]][["displayName"]] %||% NA_character_,
      creator_self                = .[["creator"]][["self"]]        %||% NA

      # source                      = .[["source"]] %||% NA,
      # attachments                 = .[["attachment"]] %||% NA,
    )) %>%
    select(
      summary,
      location,
      start_datetime,
      start_timezone,
      end_datetime,
      end_timezone,
      dplyr::everything()
    ) %>%
    mutate(
      created        = transform_or_na(., "created", as_datetime),
      updated        = transform_or_na(., "updated", as_datetime),
      start_date     = transform_or_na(., "start_date", as.Date),
      start_date     = if_else(is.na(start_date), as.Date(start_datetime), start_date),
      end_date       = transform_or_na(., "end_date", as.Date),
      end_date       = if_else(is.na(end_date), as.Date(end_datetime), end_date),
      start_datetime = transform_or_na(., "start_datetime", as_datetime),
      end_datetime   = transform_or_na(., "end_datetime", as_datetime)
    )

}


# - convert_items_to events -----------------------------------------------

#' @importFrom dplyr inner_join
convert_items_to_events <- function(items){

  events <- extract_simple_columns(items)

  attendees <- extract_attendees(items)
  organizer <- extract_organizer(items)
  creator   <- extract_creator(items)

  if (all(
    all.equal(items %>% map_chr("id"), attendees$id),
    all.equal(items %>% map_chr("id"), organizer$id),
    all.equal(items %>% map_chr("id"), creator$id)
  )) {
    bind_cols(
      events,
      attendees[, "attendees"]
    )
  } else {
    warning ("Incorrect number of rows")
    events %>%
      left_join(attendees, by = "id")
  }


}
