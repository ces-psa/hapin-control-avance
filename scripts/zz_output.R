#------------------------------------------------------------------------------*
# Funciones para mostrar datos ----
#------------------------------------------------------------------------------*

# Datos para tabla interactiva

datos_tabla <- function(.data, ...){
  # eval unnamed parameters
  dots <- quos(...)
  
  # Keep values as read and drop variables to rows
  mutate_at(
    .data,
    vars(matches("date|m17_ga")),
    funs(as.character)
  ) %>%
    gather(
      variable, value, -id, -visit, -redcap_event_name, -fpp,
      # ignore extra variables
      -one_of(gsub("~", "", as.character(dots))),
      factor_key = TRUE
    ) %>%
    # Separate variable names from dummy columns
    separate(variable, into = c("variable", "dummy_option"), sep = "___") %>%
    # Tag each crf and keep order
    left_join(
      select(gt_emory_dictionary, variable, crf)
    ) %>%
    mutate(
      crf = zoo::na.locf(crf, na.rm = FALSE),
      variable = factor(variable, levels = unique(variable)),
      crf = factor(crf, levels = unique(crf))
    ) %>%
    arrange(id, crf, variable) %>%
    # For each participant, event and crf
    group_by(id, visit, crf, fpp, !!!dots) %>%
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
          date,# ";", first(status),
          "</div>",
          # Include image for display
          "<img src=\"img/circle_", ., ".png\" width=16 height=16></img>"
        )
    ) %>%
    ungroup() %>%
    select(id, visit, crf, data, fpp, !!!dots) %>%
    spread(crf, data) %>%
    select(everything(), -visit, visit)
}

# Tabla interactiva de crfs llenos
tabla_interactiva <- function(.data, ...){
  .data %>%
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
      fillContainer = TRUE,
      # configure options
      options = list(
        dom = "Bfrtip",
        scrollX = TRUE, scrollY = TRUE,
        fixedColumns = list(leftColumns = 2),
        # fixedHeader = TRUE,
        extend = "collection",
        buttons = c("csv", "excel"),
        # Scroller
        deferRender = TRUE,
        scroller = TRUE
      )
      
    )
}
