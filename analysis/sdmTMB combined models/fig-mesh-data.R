library(cowplot)

hblldog <- readRDS(paste0("figures/sog-model-mesh-", "hblldog", ".rds"))
hbll <- readRDS(paste0("figures/sog-model-mesh-", "hbll-n-s", ".rds"))
dog <- readRDS (paste0("figures/sog-model-mesh-", "dog-j-circle", ".rds"))

cv <- cowplot::plot_grid(
  hblldog, hbll,
  ncol = 1,
  nrow = 2,
  labels = c("(a) HBLL & SoG dogfish circle/J-hook", "(b) HBLL"),   # Labels for each plot
  #align = "hv",
  hjust = 0,

  rel_heights = c(1,1),
  rel_widths = rep(1,1)
)
cv

cv2 <- cowplot::plot_grid(
  cv, dog,
  ncol = 1,
  nrow = 2,
  hjust = 0,
  labels = c(" ", "(c) SoG dogfish circle/J-hook"),
  #align = "hv",
  rel_heights = c(1,0.56),
  rel_widths = rep(1,1)
)
cv2

ggsave(paste0("figures/mesh_points.png"), cv2, height = 10, width = 7, dpi = 200)
