dyRup_palettes <- list(
  devoid <- c('#25557B', '#F4CE80', '#C4031D'),
  calm_down_woman <- c('#ECE4DC', '#EBD9D0', '#BCDBDB', '#C3AC98', '#B98675', '#637D74', '#607B8E'),
  warm_modern <- c('#E5DAC8', '#ABB39E', '#A2B7B5', '#AB6057', '#C09C6A', '#604344', '#464860'),
  brighteyes <- c('#E8EAE6', '#B6B0A9', '#414040', '#ADBAE3', '#A54975', '#4F6997', '#0190A8'),
  dark_musk <- c('#F2EBE6', '#A0928B', '#935F43', '#3A5F7D', '#3F4C5A', '#464444', '#29545C'),
  braveheart <- c('#EAE9E7', '#F5DEAE', '#F0B9A9', '#A49D70', '#C5394B', '#625141', '#3A5F7D'),
  soft_n_hazy <- c('#F1EEE4', '#D3B6BA', '#B3C4BA', '#75978F', '#3A5F7D', '#645D5F', '#604046')
)

#ggplot2::qplot(x = 1:7, y = 1, fill = I(rev(calm_down_woman)), geom = 'col', width = 1) + ggplot2::theme_void()

dyRup <- function(palette, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  
  pal <- dyRup_palettes[[palette]]
  if (is.null(pal))
    stop("Palette not found.")
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", palette = palette)
}



print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "palette"), cex = 1, family = "serif")
}
