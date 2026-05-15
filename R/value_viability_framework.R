# ============================================================
# Value-Viability Framework - Reproducible Synthetic Illustration
# Consolidated version with vector outputs (PDF + SVG)
# ============================================================

### Cleaning the memory
rm(list = ls())

### Environment
setwd("e:/datasets/")

### Memory
if (.Platform$OS.type == "windows") {
  try(memory.limit(size = 9999999999999), silent = TRUE)
}

# 1. Load required packages
required_packages <- c("tidyverse", "ggrepel", "gridExtra", "svglite")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}

# 1.1 Helper function to save vector graphics
save_vector_plot <- function(plot_obj, filename_base, width, height, bg = "white") {
  # PDF vetorial
  ggsave(
    filename = paste0(filename_base, ".pdf"),
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = grDevices::pdf,
    bg = bg
  )
  
  # SVG vetorial
  ggsave(
    filename = paste0(filename_base, ".svg"),
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = svglite::svglite,
    bg = bg
  )
}

# 2. BCV parameters
n_panelists <- 15
alpha <- 0.05
p_cut <- 0.25
k_critical <- qbinom(1 - alpha, n_panelists, p_cut) + 1
cat("BCV critical threshold (k_c):", k_critical, "\n")

# 3. Synthetic BCV panelist responses
# 0 = dispensable
# 1 = useful but not essential
# 2 = essential
# 3 = don't know
# These responses are synthetic and included only for didactic illustration and reproducibility.
bcv_raw <- as.data.frame(rbind(
  c("P01", 3, 1, 1, 1, 0, 2, 2, 0, 2, 1, 2, 2, 2, 1, 0, 1, 2, 0, 0, 0),
  c("P02", 0, 2, 2, 0, 0, 1, 1, 2, 2, 2, 1, 1, 2, 3, 1, 0, 0, 1, 0, 2),
  c("P03", 2, 1, 1, 3, 1, 0, 2, 0, 1, 1, 1, 1, 1, 2, 2, 0, 0, 0, 0, 3),
  c("P04", 0, 2, 2, 0, 2, 2, 1, 0, 3, 1, 2, 3, 1, 0, 2, 0, 0, 3, 3, 1),
  c("P05", 1, 2, 2, 0, 1, 1, 2, 0, 1, 2, 1, 2, 3, 1, 0, 1, 2, 1, 3, 1),
  c("P06", 1, 1, 3, 0, 1, 1, 1, 0, 1, 1, 2, 1, 1, 0, 0, 3, 3, 0, 0, 2),
  c("P07", 0, 2, 2, 3, 3, 1, 1, 0, 1, 0, 0, 1, 1, 0, 3, 1, 0, 2, 0, 2),
  c("P08", 0, 0, 2, 0, 2, 2, 0, 0, 2, 2, 1, 2, 2, 3, 1, 2, 3, 0, 0, 1),
  c("P09", 2, 1, 1, 0, 1, 2, 3, 3, 1, 1, 1, 0, 1, 0, 1, 1, 0, 3, 2, 1),
  c("P10", 1, 0, 0, 3, 2, 1, 1, 0, 0, 2, 1, 1, 2, 2, 0, 1, 0, 0, 0, 0),
  c("P11", 0, 1, 1, 2, 1, 0, 1, 2, 2, 2, 0, 1, 1, 0, 0, 2, 0, 0, 2, 1),
  c("P12", 3, 2, 1, 2, 2, 1, 1, 3, 1, 3, 2, 1, 2, 3, 0, 1, 0, 0, 0, 1),
  c("P13", 0, 3, 1, 0, 1, 1, 2, 0, 0, 1, 3, 1, 1, 0, 3, 1, 0, 1, 1, 1),
  c("P14", 0, 1, 2, 1, 1, 3, 1, 0, 1, 1, 1, 1, 2, 0, 0, 1, 3, 2, 3, 1),
  c("P15", 3, 2, 2, 0, 1, 2, 0, 3, 2, 2, 1, 0, 0, 0, 3, 2, 0, 3, 0, 2)
), stringsAsFactors = FALSE)

colnames(bcv_raw) <- c("Panelist", sprintf("C%02d", 1:20))

bcv_raw <- bcv_raw %>%
  mutate(across(-Panelist, as.integer))

# 4. BCV summaries
bcv_long <- bcv_raw %>%
  pivot_longer(-Panelist, names_to = "Criterion", values_to = "Response")

bcv_counts <- bcv_long %>%
  count(Criterion, Response, name = "n") %>%
  complete(Criterion, Response = 0:3, fill = list(n = 0)) %>%
  arrange(Criterion, Response)

bcv_summary <- bcv_long %>%
  mutate(endorsement_component = ifelse(Response == 3, 0, Response)) %>%
  group_by(Criterion) %>%
  summarise(
    n_0 = sum(Response == 0),
    n_1 = sum(Response == 1),
    n_2 = sum(Response == 2),
    n_3 = sum(Response == 3),
    E_j = sum(endorsement_component),
    Valid = ifelse(E_j >= k_critical, "Yes", "No"),
    .groups = "drop"
  ) %>%
  arrange(Criterion)

write.csv(bcv_raw, "bcv_panel_responses_synthetic.csv", row.names = FALSE)
write.csv(bcv_summary, "bcv_summary_synthetic.csv", row.names = FALSE)

# 5. BCV figures
response_labels <- c(
  "0" = "0 = Dispensable",
  "1" = "1 = Useful but not essential",
  "2" = "2 = Essential",
  "3" = "3 = Don't know"
)

response_colors <- c(
  "0 = Dispensable" = "red",
  "1 = Useful but not essential" = "lightgreen",
  "2 = Essential" = "darkgreen",
  "3 = Don't know" = "gray50"
)

plot_counts <- bcv_counts %>%
  mutate(Response = factor(Response, levels = 0:3, labels = response_labels))

p_counts <- ggplot(plot_counts, aes(x = Criterion, y = n, fill = Response)) +
  geom_col(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = response_colors) +
  labs(
    title = "(a) Response composition by criterion",
    x = NULL,
    y = "Number of panelists"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_threshold <- bcv_summary %>%
  mutate(Status = factor(Valid, levels = c("Yes", "No"), labels = c("Validated", "Rejected")))

p_threshold <- ggplot(plot_threshold, aes(x = Criterion, y = E_j, fill = Status)) +
  geom_col(color = "white", linewidth = 0.2) +
  geom_hline(yintercept = k_critical, linetype = "dashed", linewidth = 0.7) +
  geom_text(aes(label = E_j), vjust = -0.35, size = 3.1) +
  annotate(
    "text",
    x = length(unique(plot_threshold$Criterion)) - 0.1,
    y = k_critical + 0.6,
    label = paste0("k[c] == ", k_critical),
    parse = TRUE,
    hjust = 1,
    size = 4
  ) +
  scale_fill_manual(values = c("Validated" = "darkgreen", "Rejected" = "red")) +
  labs(
    title = "(b) Endorsement totals and decision threshold",
    x = "Criterion",
    y = expression("Endorsement total " * E[j])
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Show the graphics
p_counts
p_threshold

# Combine BCV panels into one object
bcv_combined <- gridExtra::arrangeGrob(
  p_counts, p_threshold,
  ncol = 1,
  heights = c(1.05, 0.95)
)

# Save combined BCV figure as vector
save_vector_plot(bcv_combined, "BCV", width = 10, height = 12)

# 6. Synthetic project ratings on the 12 validated criteria
projects <- as.data.frame(rbind(
  c("P01", 4, 4, 5, 5, 4, 4, 5, 5, 4, 4, 4, 4),
  c("P02", 4, 2, 3, 4, 3, 4, 3, 3, 5, 4, 4, 3),
  c("P03", 4, 4, 3, 4, 4, 4, 4, 5, 4, 3, 5, 3),
  c("P04", 4, 2, 3, 4, 5, 4, 4, 4, 3, 3, 4, 5),
  c("P05", 4, 3, 4, 4, 3, 4, 5, 5, 3, 4, 4, 5),
  c("P06", 4, 4, 3, 3, 5, 5, 4, 5, 4, 3, 4, 5),
  c("P07", 4, 5, 2, 5, 4, 4, 4, 2, 4, 4, 5, 4),
  c("P08", 3, 4, 5, 4, 4, 4, 4, 5, 3, 4, 4, 3),
  c("P09", 4, 4, 4, 3, 2, 3, 3, 3, 3, 4, 5, 4),
  c("P10", 4, 3, 2, 3, 4, 5, 3, 4, 3, 2, 5, 4),
  c("P11", 4, 3, 5, 2, 4, 5, 3, 3, 4, 3, 2, 4),
  c("P12", 3, 4, 3, 5, 3, 3, 4, 2, 4, 5, 2, 4),
  c("P13", 4, 4, 2, 2, 4, 4, 4, 4, 3, 4, 4, 3),
  c("P14", 5, 4, 2, 4, 3, 4, 5, 3, 4, 4, 4, 5),
  c("P15", 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 3, 5),
  c("P16", 4, 3, 3, 4, 3, 4, 4, 3, 3, 2, 3, 4),
  c("P17", 3, 2, 3, 3, 2, 3, 3, 2, 3, 4, 4, 4),
  c("P18", 2, 2, 4, 4, 4, 5, 4, 4, 4, 4, 3, 4),
  c("P19", 2, 3, 3, 3, 5, 1, 4, 1, 3, 4, 3, 2),
  c("P20", 2, 4, 2, 3, 3, 2, 5, 4, 1, 3, 2, 4),
  c("P21", 2, 3, 4, 4, 2, 3, 3, 2, 5, 3, 2, 4),
  c("P22", 5, 4, 1, 3, 4, 2, 3, 4, 2, 3, 1, 2),
  c("P23", 3, 2, 5, 2, 3, 3, 4, 2, 4, 3, 2, 3),
  c("P24", 3, 2, 3, 3, 3, 4, 5, 2, 5, 1, 3, 4),
  c("P25", 3, 2, 2, 2, 2, 3, 3, 2, 3, 3, 3, 3),
  c("P26", 2, 2, 3, 3, 2, 3, 4, 2, 3, 2, 2, 4),
  c("P27", 3, 3, 4, 3, 3, 2, 3, 2, 3, 3, 2, 5),
  c("P28", 1, 1, 4, 3, 3, 3, 2, 2, 3, 2, 4, 2),
  c("P29", 2, 2, 3, 2, 2, 3, 3, 2, 2, 3, 1, 1),
  c("P30", 2, 2, 3, 4, 3, 2, 2, 1, 2, 2, 3, 2)
), stringsAsFactors = FALSE)

colnames(projects) <- c(
  "ID", "C02", "C03", "C05", "C06", "C07", "C09",
  "C10", "C11", "C12", "C13", "C16", "C20"
)

projects <- projects %>%
  mutate(across(-ID, as.integer))

value_criteria <- c("C02", "C03", "C05", "C06", "C07", "C09")
viability_criteria <- c("C10", "C11", "C12", "C13", "C16", "C20")

# 7. Rescale and compute composite scores
projects_rescaled <- projects %>%
  mutate(across(-ID, ~ (.x - 1) / 4)) %>%
  mutate(
    Value = rowMeans(across(all_of(value_criteria))),
    Viability = rowMeans(across(all_of(viability_criteria)))
  )

# 8. 2x2 classification
value_median <- median(projects_rescaled$Value)
viability_median <- median(projects_rescaled$Viability)

projects_rescaled <- projects_rescaled %>%
  mutate(
    Cell_2x2 = paste0(
      ifelse(Value >= value_median, "High", "Low"), "-",
      ifelse(Viability >= viability_median, "High", "Low")
    )
  )

# 9. 3x3 classification
value_tertiles <- quantile(projects_rescaled$Value, c(1/3, 2/3))
viability_tertiles <- quantile(projects_rescaled$Viability, c(1/3, 2/3))

tertile_band <- function(x, cuts) {
  ifelse(x >= cuts[2], "High",
         ifelse(x >= cuts[1], "Medium", "Low"))
}

projects_rescaled <- projects_rescaled %>%
  mutate(
    Cell_3x3 = paste0(
      tertile_band(Value, value_tertiles), "-",
      tertile_band(Viability, viability_tertiles)
    ),
    Balance_HM = ifelse(Value + Viability == 0, 0, 2 * Value * Viability / (Value + Viability))
  )

write.csv(projects_rescaled, "project_classification_results.csv", row.names = FALSE)

# 10. Figures: 2x2 and 3x3 maps
# Standardized axis limits for Value-Viability maps
x_axis_limits <- c(0.3, 0.9)
y_axis_limits <- c(0.2, 0.9)
x_axis_breaks <- seq(0.3, 0.9, by = 0.1)
y_axis_breaks <- seq(0.2, 0.9, by = 0.1)

colors_2x2 <- c(
  "High-High" = "darkgreen",
  "High-Low"  = "orange",
  "Low-High"  = "blue",
  "Low-Low"   = "red"
)

p_2x2 <- ggplot(projects_rescaled, aes(x = Value, y = Viability, color = Cell_2x2, label = ID)) +
  geom_hline(yintercept = viability_median, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = value_median, linetype = "dashed", color = "gray50") +
  geom_point(size = 3.8, alpha = 0.95) +
  ggrepel::geom_text_repel(size = 3.2, max.overlaps = Inf) +
  scale_color_manual(values = colors_2x2) +
  scale_x_continuous(breaks = x_axis_breaks) +
  scale_y_continuous(breaks = y_axis_breaks) +
  coord_cartesian(xlim = x_axis_limits, ylim = y_axis_limits) +
  labs(
    title = "2x2 Value-Viability matrix",
    subtitle = "Median-based classification",
    x = "Value (Benefit)",
    y = "Viability (Feasibility)",
    color = "2x2 classification"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

# Show and save 2x2 matrix
p_2x2
save_vector_plot(p_2x2, "2x2_matrix", width = 10, height = 7.5)

colors_3x3 <- c(
  "High-High"     = "darkgreen",
  "High-Medium"   = "green",
  "Medium-High"   = "lightgreen",
  "Medium-Medium" = "gray50",
  "Medium-Low"    = "orange",
  "Low-Medium"    = "blue",
  "Low-Low"       = "red"
)

p_3x3 <- ggplot(projects_rescaled, aes(x = Value, y = Viability, color = Cell_3x3, label = ID)) +
  geom_hline(yintercept = viability_tertiles, linetype = "dotted", color = "gray50") +
  geom_vline(xintercept = value_tertiles, linetype = "dotted", color = "gray50") +
  geom_point(size = 3.8, alpha = 0.95) +
  ggrepel::geom_text_repel(size = 3.2, max.overlaps = Inf) +
  scale_color_manual(values = colors_3x3, drop = FALSE) +
  scale_x_continuous(breaks = x_axis_breaks) +
  scale_y_continuous(breaks = y_axis_breaks) +
  coord_cartesian(xlim = x_axis_limits, ylim = y_axis_limits) +
  labs(
    title = "3x3 Value-Viability matrix with transition zones",
    subtitle = "Tertile-based classification",
    x = "Value (Benefit)",
    y = "Viability (Feasibility)",
    color = "3x3 classification"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

# Show and save 3x3 matrix
p_3x3
save_vector_plot(p_3x3, "3x3_matrix", width = 10, height = 7.5)

# 11. Console summaries
cat("\nValidated criteria:\n")
print(bcv_summary %>% filter(Valid == "Yes") %>% select(Criterion, E_j))

cat("\n2x2 classification summary:\n")
print(table(projects_rescaled$Cell_2x2))

cat("\n3x3 classification summary:\n")
print(table(projects_rescaled$Cell_3x3))

cat("\nArquivos vetoriais gerados com sucesso:\n")
cat("- BCV.pdf\n")
cat("- BCV.svg\n")
cat("- 2x2_matrix.pdf\n")
cat("- 2x2_matrix.svg\n")
cat("- 3x3_matrix.pdf\n")
cat("- 3x3_matrix.svg\n")
