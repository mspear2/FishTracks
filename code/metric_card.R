# ============================================================================
# metric_card.R
# IRBS stat card for bslib Shiny apps.
# Returns an htmltools tag; use directly in UI or inside renderUI().
# Requires the CSS block in irbs_setup.R (via irbs_css) loaded in the UI.
# ============================================================================

#' IRBS metric / KPI card
#'
#' @param label     Short uppercase label (keep to 3–5 words)
#' @param value     Pre-formatted display value (character). Format before
#'                  passing — the function does no rounding or formatting.
#' @param unit      Optional unit string rendered at smaller size (e.g. "fish / hr")
#' @param sub       Optional secondary line below the value
#' @param delta     Optional change value shown with directional arrow (e.g. "0.8")
#' @param delta_dir Arrow direction and semantic colour: "up" (prairie/green),
#'                  "down" (altgeld/red), or "neutral" (storm/grey)
#' @param accent    Top border colour token: "orange", "blue", "prairie", or "harvest"
#'
#' @return An \code{htmltools} tag object (shiny.tag)
#'
#' @examples
#' metric_card(
#'   "Bighead Carp CPUE",
#'   sprintf("%.1f", cpue),
#'   unit = "fish / hr",
#'   delta = sprintf("%.1f", abs(cpue_delta)),
#'   delta_dir = if (cpue_delta < 0) "down" else "up",
#'   sub = "vs. prior year",
#'   accent = "orange"
#' )
metric_card <- function(
  label,
  value,
  unit      = NULL,
  sub       = NULL,
  delta     = NULL,
  delta_dir = c("up", "down", "neutral"),
  accent    = c("orange", "blue", "prairie", "harvest")
) {
  delta_dir <- match.arg(delta_dir)
  accent    <- match.arg(accent)

  border_color <- switch(accent,
    orange  = "#FF5F05",
    blue    = "#13294B",
    prairie = "#006230",
    harvest = "#FCB316"
  )

  arrow <- switch(delta_dir,
    up      = "↑",
    down    = "↓",
    neutral = "→"
  )

  htmltools::tags$div(
    class = "irbs-metric-card",
    style = paste0("border-top: 3px solid ", border_color, ";"),

    htmltools::tags$span(class = "irbs-metric-label", label),

    htmltools::tags$div(
      class = "irbs-metric-value",
      value,
      if (!is.null(unit))
        htmltools::tags$span(class = "irbs-metric-unit", unit)
    ),

    if (!is.null(delta) || !is.null(sub)) {
      htmltools::tags$div(
        class = "irbs-metric-sub",
        if (!is.null(delta))
          htmltools::tags$span(
            class = paste0("irbs-delta-", delta_dir),
            paste(arrow, delta)
          ),
        if (!is.null(delta) && !is.null(sub)) " ",
        sub
      )
    }
  )
}
