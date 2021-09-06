#' Complete list of palettes
#'
#' @export

dyRup_palettes <- list(
  devoid_of_all_desires = c('#25557B', '#F4CE80', '#C4031D'),
  calm_down_woman = rev(c('#ECE4DC', '#EBD9D0', '#BCDBDB', '#C3AC98', '#B98675', '#637D74', '#607B8E')),
  warm_modern = rev(c('#E5DAC8', '#ABB39E', '#A2B7B5', '#AB6057', '#C09C6A', '#604344', '#464860')),
  brighteyes = rev(c('#E8EAE6', '#B6B0A9', '#414040', '#ADBAE3', '#A54975', '#4F6997', '#0190A8')),
  dark_musk = rev(c('#F2EBE6', '#A0928B', '#935F43', '#3A5F7D', '#3F4C5A', '#464444', '#29545C')),
  braveheart = rev(c('#EAE9E7', '#F5DEAE', '#F0B9A9', '#A49D70', '#C5394B', '#625141', '#3A5F7D')),
  soft_n_hazy = rev(c('#F1EEE4', '#D3B6BA', '#B3C4BA', '#75978F', '#3A5F7D', '#645D5F', '#604046'))
)

#ggplot2::qplot(x = 1:7, y = 1, fill = I(rev(calm_down_woman)), geom = 'col', width = 1) + ggplot2::theme_void()

#' These are a handful of colour palettes created based on paint colours.
#' Devoid of all desires will guaranteed replace your current continuous selection.
#'
#' @param number_of_colours Number of colours desired. All but one palette contain 7 colours.
#'   If omitted, uses all colours.
#' @param palette Name of desired palette. Choices are:
#'   \code{devoid_of_all_desires}, \code{calm_down_woman},  \code{warm_modern},
#'   \code{brighteyes}, \code{dark_musk},  \code{braveheart}, \code{soft_n_hazy}
#' @param type Either 'single' or 'scale'. Use scale if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export

dyRup <- function(palette, number_of_colours, type = c('single', 'scale')) {
  type <- match.arg(type)

  pal <- dyRup_palettes[[palette]]
  if (is.null(pal))
    stop('No such palette with that name. Spellcheck time!')

  if (missing(number_of_colours)) {
    number_of_colours <- length(pal)
  }

  if (type == 'single' && number_of_colours > length(pal)) {
    stop('You are walking on thin ice here. 7 is the limit!')
  }

  out <- switch(type, scale = grDevices::colorRampPalette(pal)(number_of_colours), single = pal[1:number_of_colours]
  )
  structure(out, class = 'palette', palette = palette)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb

print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = '', xaxt = 'n', yaxt = 'n', bty = 'n')

  rect(0, 0.97, n + 1, 1.03, col = rgb(1, 1, 1, 0.4), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, 'palette'), cex = 1)
}
