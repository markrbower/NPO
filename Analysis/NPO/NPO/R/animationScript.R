animationScript <- function() {
#  https://gganimate.com/index.html
  library(gganimation)
  library(av)
  library(transformr)

  p <- ggplot(airquality, aes(Day, Temp)) +
    geom_line(size = 2, colour = 'steelblue') +
    transition_states(Month, 4, 1) +
    shadow_mark(size = 1, colour = 'grey')
  animate(p, renderer = av_renderer())
}