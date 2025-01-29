#' RGB to Hex Color Converter
#' @param r Red value (0-255)
#' @param g Green value (0-255)
#' @param b Blue value (0-255)
#' @return A hex color string
#' @keywords internal
rgb_to_hex <- function(r, g, b) {
  sprintf("#%02X%02X%02X", r, g, b)
}

#' BRG Theme Color Palettes
#' @export
theme_brg_colors <- list(
  primary = list(
    dark_blue = rgb_to_hex(0, 39, 76),
    bright_blue = rgb_to_hex(0, 189, 242),
    dark_blue_tint = rgb_to_hex(66, 82, 107),
    bright_blue_tint = rgb_to_hex(190, 227, 246)
  ),
  secondary = list(
    yellow = rgb_to_hex(253, 185, 19),
    cool_gray = rgb_to_hex(192, 191, 191),
    yellow_tint = rgb_to_hex(245, 220, 174),
    cool_gray_tint = rgb_to_hex(222, 221, 222)
  ),
  tertiary = list(
    teal = rgb_to_hex(0, 167, 157),
    purple = rgb_to_hex(92, 30, 61),
    orange = rgb_to_hex(245, 127, 50),
    red = rgb_to_hex(190, 30, 45),
    beige = rgb_to_hex(213, 199, 189),
    teal_tint = rgb_to_hex(189, 218, 216),
    purple_tint = rgb_to_hex(123, 92, 108),
    orange_tint = rgb_to_hex(230, 173, 134),
    red_tint = rgb_to_hex(222, 173, 174),
    beige_tint = rgb_to_hex(231, 226, 221)
  )
)

#' BRG Custom Theme for ggplot2
#'
#' A custom ggplot2 theme using the BRG color palette
#'
#' @param base_size Base font size
#' @param base_family Base font family
#' @return A ggplot2 theme object
#' @export
#' @import ggplot2
theme_brg <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = theme_brg_colors$secondary$cool_gray_tint, size = 0.2),
      panel.grid.minor = element_line(color = theme_brg_colors$secondary$cool_gray_tint, size = 0.1),
      text = element_text(color = theme_brg_colors$primary$dark_blue),
      plot.title = element_text(
        color = theme_brg_colors$primary$dark_blue,
        size = rel(1.5),
        face = "bold"
      ),
      plot.subtitle = element_text(
        color = theme_brg_colors$primary$dark_blue_tint,
        size = rel(1.1)
      ),
      axis.text = element_text(color = theme_brg_colors$primary$dark_blue_tint),
      axis.title = element_text(color = theme_brg_colors$primary$dark_blue),
      legend.title = element_text(color = theme_brg_colors$primary$dark_blue),
      legend.text = element_text(color = theme_brg_colors$primary$dark_blue_tint),
      complete = TRUE
    )
}

#' BRG Color Scale
#'
#' Create a color scale using the BRG palette
#'
#' @param palette The palette to use ("primary", "secondary", "tertiary", or "all")
#' @return A ggplot2 color scale
#' @export
scale_color_brg <- function(palette = "primary") {
  colors <- switch(palette,
                   "primary" = unlist(theme_brg_colors$primary),
                   "secondary" = unlist(theme_brg_colors$secondary),
                   "tertiary" = unlist(theme_brg_colors$tertiary),
                   "all" = unlist(theme_brg_colors)
  )
  scale_color_manual(values = colors)
}

#' BRG Fill Scale
#'
#' Create a fill scale using the BRG palette
#'
#' @param palette The palette to use ("primary", "secondary", "tertiary", or "all")
#' @return A ggplot2 fill scale
#' @export
scale_fill_brg <- function(palette = "primary") {
  colors <- switch(palette,
                   "primary" = unlist(theme_brg_colors$primary),
                   "secondary" = unlist(theme_brg_colors$secondary),
                   "tertiary" = unlist(theme_brg_colors$tertiary),
                   "all" = unlist(theme_brg_colors)
  )
  scale_fill_manual(values = colors)
}
