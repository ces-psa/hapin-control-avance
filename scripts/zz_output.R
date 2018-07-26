#------------------------------------------------------------------------------*
# Funciones para mostrar datos ----
#------------------------------------------------------------------------------*



# Tabla interactiva de crfs llenos
tabla_interactiva <- function(.data, ...){
  # eval unnamed parameters
  dots <- quos(...)
  
  # Keep values as read and drop variables to rows
  mutate_at(
    .data,
    vars(matches("date|m17_ga")),
    funs(as.character)
  ) %>%
    gather(
      variable, value, -id, -redcap_event_name, -fpp, -rowname,
      # ignore extra variables
      !!!dots,
      factor_key = TRUE
    ) %>%
    # Tag each crf and keep order
    mutate(
      crf = gsub("^([^_]+)_.+", "\\1", variable),
      letter = gsub("[a-z]+[0-9]+([a-z]*)", "\\1", crf) %>%
        if_else(. == "", NA_character_, .) %>%
        zoo::na.locf(na.rm = FALSE) %>%
        if_else(is.na(.), "", .),
      crf = paste0(gsub("([a-z]+[0-9]+)[a-z]*", "\\1", crf), letter) %>%
        factor(levels = unique(.))
    ) %>%
    select(-letter) %>%
    # For each participant, event and crf
    group_by(id, redcap_event_name, crf, fpp) %>%
    # Get completion status
    bind_rows(
      summarize(., not_complete =!any(grepl("_complete", variable))) %>%
        filter(not_complete) %>%
        select(-not_complete) %>%
        mutate(
          variable = paste0(crf, "_complete"),
          value = "0"
        )
    ) %>%
    # TODO: status for crfs without {crf}_complete variable
    mutate(
      status = value[grepl("_complete", variable)] %>%
        recode(
          `0` = "incomplete",
          `1` = "unverified",
          `2` = "complete"
        ) %>%
        recode(
          `0` = "incomplete",
          `1` = "unverified",
          `2` = "complete"
        ) %>%
        unique() %>%
        paste(collapse = "")
    ) %>%
    filter(!grepl("_complete", variable)) %>%
    # Label if there is data
    summarize(
      date = first(value[grepl(paste0(first(crf), "_date$"), variable)]),
      data = ifelse(
        any(!is.na(value)),
        first(status),
        "no-data"
      ) %>%
        recode(
          incomplete = "red",
          unverified = "yellow",
          complete = "green",
          "no-data" = "gray",
          .missing = "error"
        ) %>%
        # Include semaphore image
        paste0(
          # Include value for tables
          "<div style=\"display:none;\">",
          date, ";",
          first(status),
          "</div>",
          # Include image for display
          "<img src=\"img/circle_", ., ".png\" width=16 height=16></img>"
        )
    ) %>%
    select(id, crf, data, fpp) %>%
    spread(crf, data) %>%
    select(everything(), -redcap_event_name, redcap_event_name) %>%
    # DT::renderDataTable(escape = FALSE) %>%
    DT::datatable(
      # Keep html in cells
      escape = FALSE,
      # Fill vertically
      height = "100%",
      # declare extensions
      extensions = c(
        # provide downloads
        "Buttons",
        # fix columns with IDs
        "FixedColumns",
        # Fixed header
        # "FixedHeader",
        # Only render visible
        "Scroller"
      ),
      # configure options
      options = list(
        dom = "Bfrtip",
        scrollX = TRUE, scrollY = TRUE,
        fixedColumns = list(leftColumns = 2),
        # fixedHeader = TRUE,
        extend = "collection",
        buttons = c("csv", "excel", "pdf"),
        # Scroller
        deferRender = TRUE,
        scroller = TRUE
      )
      
    )
}
