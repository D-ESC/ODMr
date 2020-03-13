###############################################################################
stat_ui <-
  function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::tableOutput(ns("stats")),
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("densityplot"), height = "250px")),
      shiny::br(),
      shiny::tableOutput(ns("diffstats"))
    )
  }

###############################################################################
Stat_server <-
  function(input,
           output,
           session,
           data,
           selected,
           active) {
    ###########################################################################
    output$stats <- shiny::renderTable({
      shiny::req(data$ODMdata)
      data$ODMdata %>%
        dplyr::rename("QCLevelID" = "QualityControlLevelID") %>%
        dplyr::filter(index %in% selected()) %>%
        dplyr::group_by(SiteID, VariableID, MethodID, QCLevelID, SourceID) %>%
        dplyr::summarise(
          n = length(DataValue),
          mean = mean(DataValue, na.rm = TRUE),
          std.dev. = stats::sd(DataValue, na.rm = TRUE),
          min = min(DataValue, na.rm = TRUE),
          p.05 = stats::quantile(DataValue, 0.05, na.rm = TRUE),
          p.25 = stats::quantile(DataValue, 0.25, na.rm = TRUE),
          p.50 = stats::quantile(DataValue, 0.50, na.rm = TRUE),
          p.75 = stats::quantile(DataValue, 0.75, na.rm = TRUE),
          p.95 = stats::quantile(DataValue, 0.95, na.rm = TRUE),
          max = max(DataValue, na.rm = TRUE)
        )
    }, digits = 3)
    ###########################################################################
    output$densityplot <- shiny::renderPlot({
      shiny::req(data$ODMdata)
      data$ODMdata %>%
        dplyr::filter(index %in% selected()) %>%
        ggplot2::ggplot(ggplot2::aes(DataValue, colour = label)) +
        ggplot2::geom_density() +
        ggplot2::theme_minimal()
    })
    ###########################################################################
    output$diffstats <- shiny::renderTable({
      shiny::req(active())
      shiny::req(nrow(data$meta) > 1)
      Stat <- data$ODMdata %>%
        dplyr::mutate(
          LocalDateTime = lubridate::floor_date(LocalDateTime, "10 minutes")) %>%
        dplyr::filter(index %in% selected()) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(
          data$ODMdata %>%
            dplyr::mutate(
              LocalDateTime = lubridate::floor_date(LocalDateTime, "10 minutes")),
          by = "LocalDateTime") %>%
        dplyr::filter(label.x == data$meta$label[active()],
                      label.y != data$meta$label[active()]) %>%
        dplyr::group_by(label.x, label.y, LocalDateTime) %>%
        dplyr::mutate(Diff = DataValue.x - DataValue.y) %>%
        dplyr::group_by(label.x, label.y) %>%
        dplyr::summarise(
          n = length(Diff),
          mean = mean(Diff, na.rm = TRUE),
          std.dev. = stats::sd(Diff, na.rm = TRUE),
          min = min(Diff, na.rm = TRUE),
          p.05 = stats::quantile(Diff, 0.05, na.rm = TRUE),
          p.25 = stats::quantile(Diff, 0.25, na.rm = TRUE),
          p.50 = stats::quantile(Diff, 0.50, na.rm = TRUE),
          p.75 = stats::quantile(Diff, 0.75, na.rm = TRUE),
          p.95 = stats::quantile(Diff, 0.95, na.rm = TRUE),
          max = max(Diff, na.rm = TRUE)
        )
      shiny::req(nrow(Stat) > 0)
      Stat
    }, digits = 3, caption = "difference (x-y)",
    caption.placement = getOption("xtable.caption.placement", "top"))
  }
