# ============================================================================
# ggplot2_theme.R
# IRBS ggplot2 color palette, scales, and two theme variants.
# Requires: ggplot2, showtext (for Montserrat / Source Sans 3 in plots)
# ============================================================================

# ── Color palette ─────────────────────────────────────────────────────────────

irbs_colors <- c(
  "illini-orange" = "#FF5F05",
  "illini-blue"   = "#13294B",
  "storm"         = "#707372",
  "storm-mid"     = "#9C9A9D",
  "storm-light"   = "#C8C6C7",
  "industrial"    = "#1D58A7",
  "patina"        = "#007E8E",
  "arches"        = "#009FD4",
  "harvest"       = "#FCB316",
  "earth"         = "#7D3E13",
  "berry"         = "#5C0E41",
  "prairie"       = "#006230",
  "altgeld"       = "#C84113"
)

# Default series order for multi-series plots (qualitatively distinct)
irbs_series <- c(
  "#FF5F05", "#13294B", "#006230", "#1D58A7",
  "#FCB316", "#007E8E", "#C84113", "#009FD4",
  "#7D3E13", "#5C0E41", "#9C9A9D"
)

#' Pick named IRBS colors by token name
#'
#' @param ... Token names (e.g. "illini-orange", "prairie")
#' @return Named character vector of hex values
irbs_pal <- function(...) {
  nms <- c(...)
  if (length(nms) == 0) return(irbs_colors)
  out <- irbs_colors[nms]
  if (anyNA(out)) warning("Unknown IRBS token(s): ", paste(nms[is.na(out)], collapse = ", "))
  out
}

#' IRBS discrete color scale
scale_color_irbs <- function(...) {
  ggplot2::scale_color_manual(values = irbs_series, ...)
}

#' IRBS discrete fill scale
scale_fill_irbs <- function(...) {
  ggplot2::scale_fill_manual(values = irbs_series, ...)
}

# ── Font loading (call once per session) ──────────────────────────────────────

#' Load IRBS Google Fonts for use in ggplot2 via showtext
#'
#' Call irbs_fonts() once at the top of a script or in global.R.
#' After calling, set showtext::showtext_auto() to activate.
irbs_fonts <- function() {
  if (!requireNamespace("showtext", quietly = TRUE)) {
    message("Install showtext for full IRBS typography in plots: install.packages('showtext')")
    return(invisible(NULL))
  }
  sysfonts::font_add_google("Montserrat",    "Montserrat")
  sysfonts::font_add_google("Source Sans 3", "Source Sans 3")
  sysfonts::font_add_google("Atkinson Hyperlegible", "Atkinson Hyperlegible")
  showtext::showtext_auto()
  invisible(NULL)
}

# ── Theme: standard ───────────────────────────────────────────────────────────

#' Standard IRBS ggplot2 theme
#'
#' Both major (0.4 pt) and minor (0.2 pt) gridlines; axis lines and ticks;
#' Atkinson Hyperlegible axis text; Montserrat bold title.
#'
#' @param base_size   Base font size (default 12)
#' @param base_family Body font family (default "Source Sans 3")
#' @param grid        Which gridlines to show: "both" (default), "x" (vertical
#'                    lines only), "y" (horizontal lines only), or "none"
theme_irbs <- function(base_size = 12, base_family = "Source Sans 3",
                       grid = c("both", "x", "y", "none")) {
  grid <- match.arg(grid)
  half <- base_size / 2

  t <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Canvas
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", color = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA),

      # Grid
      panel.grid.major = ggplot2::element_line(color = "#C8C6C7", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_line(color = "#C8C6C7", linewidth = 0.2),

      # Axes
      axis.line  = ggplot2::element_line(color = "#707372", linewidth = 0.5),
      axis.ticks = ggplot2::element_line(color = "#707372", linewidth = 0.4),
      axis.text  = ggplot2::element_text(
        family = "Atkinson Hyperlegible",
        size   = ggplot2::rel(0.80),
        color  = "#707372"
      ),
      axis.title = ggplot2::element_text(
        size  = ggplot2::rel(0.90),
        color = "#13294B"
      ),

      # Title / subtitle / caption
      plot.title = ggplot2::element_text(
        family = "Montserrat", face = "bold",
        size = ggplot2::rel(1.15), color = "#13294B",
        margin = ggplot2::margin(b = half)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.95), color = "#707372",
        margin = ggplot2::margin(b = half)
      ),
      plot.caption = ggplot2::element_text(
        family = "Atkinson Hyperlegible",
        size = ggplot2::rel(0.75), color = "#707372",   # storm: 4.8:1 (storm-mid was 2.8:1)
        hjust = 1
      ),

      # Legend
      legend.position    = "bottom",
      legend.key         = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(
        family = "Atkinson Hyperlegible", size = ggplot2::rel(0.85)
      ),
      legend.title       = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.85)),

      # Facets
      strip.text = ggplot2::element_text(
        family = "Montserrat", face = "bold",
        size = ggplot2::rel(0.90), color = "#13294B"
      ),
      strip.background = ggplot2::element_rect(fill = "#EEF2F8", color = NA),

      plot.margin = ggplot2::margin(half, half, half, half)
    )

  # Keep only the gridlines requested: "x" = vertical lines at x-axis breaks,
  # "y" = horizontal lines at y-axis breaks (the usual choice for bar charts).
  if (grid == "x") {
    t <- t + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                            panel.grid.minor.y = ggplot2::element_blank())
  } else if (grid == "y") {
    t <- t + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                            panel.grid.minor.x = ggplot2::element_blank())
  } else if (grid == "none") {
    t <- t + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  t
}

# ── Theme: minimal (Mike's preference) ───────────────────────────────────────

#' Minimal IRBS ggplot2 theme
#'
#' Faint major gridlines only; no axis lines or ticks; slightly larger base
#' size for readability. Mike's preferred style for clean data figures.
#'
#' @param base_size   Base font size (default 14 — larger than standard)
#' @param base_family Body font family (default "Source Sans 3")
theme_irbs_minimal <- function(base_size = 14, base_family = "Source Sans 3") {
  half <- base_size / 2

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Canvas
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", color = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA),

      # Grid — major only, very faint
      panel.grid.major = ggplot2::element_line(color = "#C8C6C7", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),

      # No axis lines or ticks
      axis.line  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text  = ggplot2::element_text(
        family = "Atkinson Hyperlegible",
        size   = ggplot2::rel(0.85),   # larger than standard for readability
        color  = "#707372"
      ),
      axis.title = ggplot2::element_text(
        size  = ggplot2::rel(0.90),
        color = "#13294B"
      ),

      # Title / subtitle / caption
      plot.title = ggplot2::element_text(
        family = "Montserrat", face = "bold",
        size = ggplot2::rel(1.10), color = "#13294B",
        margin = ggplot2::margin(b = half)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.95), color = "#707372",
        margin = ggplot2::margin(b = half)
      ),
      plot.caption = ggplot2::element_text(
        family = "Atkinson Hyperlegible",
        size = ggplot2::rel(0.75), color = "#707372",   # storm: 4.8:1 (storm-mid was 2.8:1)
        hjust = 1
      ),

      # Legend
      legend.position    = "bottom",
      legend.key         = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(
        family = "Atkinson Hyperlegible", size = ggplot2::rel(0.85)
      ),
      legend.title       = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.85)),

      # Facets
      strip.text = ggplot2::element_text(
        family = "Montserrat", face = "bold",
        size = ggplot2::rel(0.90), color = "#13294B"
      ),
      strip.background = ggplot2::element_rect(fill = "#F5F5F5", color = NA),

      plot.margin = ggplot2::margin(half, half, half, half)
    )
}
