# Explore raw gas-dome K600 vs depth data per site before choosing any
# outlier-trimming methodology. Purely diagnostic -- writes nothing that
# downstream scripts depend on.

library(tidyverse)
library(readxl)

sites <- c("AM", "GB", "ID", "LF")

raw <- map_dfr(sites, function(s) {
  read_excel("04_Outputs/rC_k600.xlsx", sheet = s) %>%
    mutate(ID = s, row = row_number())
}) %>%
  select(ID, row, Date, rep, depth, k600_1.day) %>%
  mutate(Date = suppressWarnings(mdy(Date)))

valid <- raw %>%
  filter(!is.na(depth), !is.na(k600_1.day), depth > 0, k600_1.day > 0)

cat("N raw rows per site:\n"); print(table(raw$ID))
cat("\nN valid (non-NA, positive) rows per site:\n"); print(table(valid$ID))

write_csv(valid, "04_Outputs/Power Function RC/raw_valid_k600.csv")

p <- ggplot(valid, aes(x = depth, y = k600_1.day, label = row)) +
  geom_point(size = 2.2, alpha = 0.85) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  facet_wrap(~ID, scales = "free") +
  labs(title = "Raw gas-dome K600 vs depth (point label = row index in sheet)",
       x = "depth (m)", y = "K600 (1/day)") +
  theme_bw(base_size = 12)

ggsave("04_Outputs/Power Function RC/figures/00_raw_k600_scatter.png", p,
       width = 10, height = 7, dpi = 150)

# also print sorted-by-depth tables per site for a close read
for (s in sites) {
  cat("\n====", s, "====\n")
  valid %>% filter(ID == s) %>% arrange(depth) %>%
    select(row, Date, rep, depth, k600_1.day) %>% print(n = 50)
}
