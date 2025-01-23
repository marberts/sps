library(ggplot2)
library(hexSticker)

gg <- ggplot(data.frame(x = 1:5, y = 1:5, f = factor(c(1, 1, 1, 2, 2)))) +
  geom_col(aes(x, y, fill = f)) +
  scale_fill_manual(values = c("#5C5858", "#B6B6B4")) +
  guides(fill = "none") +
  theme_void()

sticker(gg,
        package = "sps",
        filename = "man/figures/logo.png",
        s_x = 1,
        s_width = 1,
        p_size = 18,
        p_family = "mono",
        p_color = "#245B37",
        h_fill = "#E2A328",
        h_color = "#245B37")
