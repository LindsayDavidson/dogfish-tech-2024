library(jpeg)

model = "hblldog" #hbll and circle and j hook
model = "hbll-n-s" #hbll
model <- "dog-j-circle" # circle and j hook sog surveys

hblldog <- readJPEG(paste0("Figures/sog-model-mesh-", "hblldog", ".jpg"))
hbll <- readJPEG(paste0("Figures/sog-model-mesh-", "hbll-n-s", ".jpg"))
dog <- readJPEG (paste0("Figures/sog-model-mesh-", "dog-j-circle", ".jpg"))


cv <- cowplot::plot_grid(
  hblldog, hbll,
  ncol = 1,
  nrow = 2,
  labels = c("(a)", "(b)"),   # Labels for each plot
  #align = "hv",
  rel_heights = c(1.5,1),
  rel_widths = rep(0.75,1)
)

cv

cv2 <- cowplot::plot_grid(
  cv, dog,
  ncol = 1,
  nrow = 2,
  labels = c("(a)", "(b)"),   # Labels for each plot
  #align = "hv",
  rel_heights = c(1.5,1),
  rel_widths = rep(0.75,1)
)

cv2

ggsave(paste0("figures/length_boxplot.png"), cv2, height = 9, width = 7, dpi = 200)