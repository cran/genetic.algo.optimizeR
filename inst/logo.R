library(hexSticker)
imgurl <-
  system.file("man/figures/used_for_logo.png", package = "genetic.algo.optimizeR")
sticker(
  imgurl,
  package = "genetic.algo.optimizeR",
  p_size = 12,
  s_x = 1,
  s_y = .8,
  s_width = .6,
  s_height = .8,
  p_color = "black",
  h_fill = "white",
  h_color = "deepskyblue2",
  filename = "man/figures/logo.png"
)
