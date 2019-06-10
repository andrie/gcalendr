utils::globalVariables(c("start", "end", "summary", "location"))

transform_or_na <- function(df, varname, fn = as_date) {
  if (varname %in% names(df)) df %>% dplyr::pull(!!varname) %>% fn() else fn(NA)
}


convert_items_to_events <- function(items){

  events <- items %>%
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
      start                       = .[["start"]] %||% NA,
      end                         = .[["end"]] %||% NA,
      recurrence                  = .[["recurrence"]] %||% NA,
      ical_uid                    = .[["iCalUID"]] %||% NA,
      sequence                    = .[["sequence"]] %||% NA,
      extended_properties         = .[["extendedProperties"]] %||% NA,
      reminders                   = .[["reminders"]] %||% NA,
      description                 = .[["description"]] %||% NA,
      recurring_event_id          = .[["recurringEventId"]] %||% NA,
      original_start_time         = .[["originalStartTime"]] %||% NA,
      transparency                = .[["transparency"]] %||% NA,
      guests_can_modify           = .[["guestsCanModify"]] %||% NA,
      visibility                  = .[["visibility"]] %||% NA,
      guests_can_see_other_guests = .[["guestsCanSeeOtherGuests"]] %||% NA,
      guests_can_invite_others    = .[["guestsCanInviteOthers"]] %||% NA,
      private_copy                = .[["privateCopy"]] %||% NA,
      source                      = .[["source"]] %||% NA,
      attachments                 = .[["attachment"]] %||% NA
    )) %>%
    select(
      summary,
      location,
      start,
      end,
      dplyr::everything()
    )


  attendees <-
    items %>%
    map_dfr(
      ~tibble(
        id = pluck(., "id"),
        attendees = list(
          pluck(., "attendees") %>%
            map_dfr(
              ~tibble(
                email           = .[["email"]] %||% NA,
                response_status = .[["responseStatus"]] %||% NA,
                organizer       = .[["organizer"]] %||% NA,
                optional        = .[["optional"]] %||% NA,
                display_name    = .[["displayName"]] %||% NA,
                self            = .[["self"]] %||% NA
              )
            )
        )
      )
    )


  creator <-
    items %>%
    map_dfr(
      ~tibble(
        id = pluck(., "id"),
        creator = list(
          tibble(
            email        = pluck(., "creator", "email") %||% NA,
            display_name = pluck(., "creator", "displayName") %||% NA,
            self         = pluck(., "creator", "self") %||% NA
          )
        )
      )
    )

  organizer <-
    items %>%
    map_dfr(
      ~tibble(
        id = pluck(., "id"),
        organizer = list(
          tibble(
            email        = pluck(., "organizer", "email") %||% NA,
            display_name = pluck(., "organizer", "displayName") %||% NA,
            self         = pluck(., "organizer", "self") %||% NA
          )
        )
      )
    )

  events %>%
    # select(-one_of(c("attendees", "creator", "organizer"))) %>%
    left_join(attendees, by = "id") %>%
    left_join(creator, by = "id") %>%
    left_join(organizer, by = "id") %>%
    mutate(
      created        = transform_or_na(., "created", as_datetime),
      updated        = transform_or_na(., "updated", as_datetime),
      start_date     = transform_or_na(., "start_date", as.Date),
      end_date       = transform_or_na(., "end_date", as.Date),
      start_dateTime = transform_or_na(., "start_dateTime", as_datetime),
      end_dateTime   = transform_or_na(., "end_dateTime", as_datetime)
    )
}
