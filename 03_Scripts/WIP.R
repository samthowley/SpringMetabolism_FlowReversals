
library(strucchange)


AM <- ER_flagged %>%
  filter(ID == "AM") %>%
  select(ER, depth) %>%
  filter(!is.na(ER), !is.na(depth)) %>%
  arrange(depth)

bp2 <- breakpoints(ER ~ depth, data = AM, breaks = 2)

idx <- bp2$breakpoints
idx <- idx[!is.na(idx)]

x_bp <- AM$depth[idx]

cuts <- c(-Inf, sort(x_bp), Inf)
AM <- AM %>%
  mutate(segment = cut(depth, breaks = cuts, include.lowest = TRUE))

# slopes per segment (safe: skip empty groups)
seg_slopes <- AM %>%
  filter(!is.na(segment)) %>%
  group_by(segment) %>%
  summarise(
    n = n(),
    slope = coef(lm(ER ~ depth, data = cur_data()))[["depth"]],
    .groups = "drop"
  )

x_bp
seg_slopes
