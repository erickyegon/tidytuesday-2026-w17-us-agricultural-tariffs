# ============================================================================
#  US AGRICULTURAL TARIFFS — END-TO-END ANALYSIS
#  Data: TidyTuesday 2026, Week 17 | USITC Tariff Database (HTS Ch. 1–24)
#  Author: Generated with Claude Sonnet 4.6
# ============================================================================
# CONTENTS
#  0.  Setup & package loading
#  1.  Data ingestion & validation
#  2.  Data cleaning & feature engineering
#  3.  Exploratory statistics
#  4.  Figure 1  — MFN rate trajectories by HTS section (small-multiples)
#  5.  Figure 2  — Trade-agreement coverage heatmap (product × agreement)
#  6.  Figure 3  — MFN vs preferential rate divergence (dumbbell chart)
#  7.  Figure 4  — Tariff volatility ranking (lollipop chart)
#  8.  Figure 5  — NAFTA → USMCA transition (ribbon / area chart)
#  9.  Figure 6  — Rate-change waterfall: biggest movers 1997–2024
# 10.  Figure 7  — Agreement-count vs MFN-rate scatter (bubble chart)
# 11.  Figure 8  — Specific vs ad valorem composition (stacked area)
# 12.  Statistical summary tables (gt)
# 13.  Export all outputs
# ============================================================================

# ── 0. Setup ────────────────────────────────────────────────────────────────

# Install missing packages silently
pkgs <- c(
  "tidyverse", "lubridate", "scales", "ggtext", "ggrepel",
  "patchwork", "gt", "gtExtras", "glue", "viridis",
  "ggridges", "gganimate", "transformr", "RColorBrewer",
  "forcats", "stringr", "here", "janitor"
)

installed <- rownames(installed.packages())
to_install <- pkgs[!pkgs %in% installed]
if (length(to_install)) install.packages(to_install, quiet = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(ggtext)
  library(ggrepel)
  library(patchwork)
  library(gt)
  library(gtExtras)
  library(glue)
  library(viridis)
  library(ggridges)
  library(RColorBrewer)
  library(forcats)
  library(stringr)
  library(janitor)
})

# Output directory
dir.create("outputs", showWarnings = FALSE)

# Global theme -----------------------------------------------------------
theme_tariff <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text                = element_text(colour = "#2C2C2A", family = "sans"),
      plot.title          = element_markdown(size = rel(1.4), face = "bold",
                                             margin = margin(b = 6)),
      plot.subtitle       = element_markdown(size = rel(1.0), colour = "#5F5E5A",
                                             lineheight = 1.4, margin = margin(b = 14)),
      plot.caption        = element_markdown(size = rel(0.75), colour = "#888780",
                                             hjust = 0, margin = margin(t = 12)),
      plot.background     = element_rect(fill = "#FAFAF8", colour = NA),
      panel.background    = element_rect(fill = "#FAFAF8", colour = NA),
      panel.grid.major    = element_line(colour = "#D3D1C7", linewidth = 0.35),
      panel.grid.minor    = element_blank(),
      axis.title          = element_text(size = rel(0.85), colour = "#5F5E5A"),
      axis.text           = element_text(size = rel(0.80), colour = "#5F5E5A"),
      axis.ticks          = element_blank(),
      strip.text          = element_markdown(size = rel(0.88), face = "bold",
                                             colour = "#3C3489"),
      strip.background    = element_rect(fill = "#EEEDFE", colour = NA),
      legend.position     = "bottom",
      legend.title        = element_text(size = rel(0.82), colour = "#5F5E5A"),
      legend.text         = element_text(size = rel(0.80)),
      legend.key.size     = unit(0.5, "cm"),
      plot.margin         = margin(16, 16, 12, 16)
    )
}

theme_set(theme_tariff())

# Colour palette (colour-blind safe)
pal_section <- c(
  "Live Animals & Animal Products" = "#D55E00",
  "Vegetable Products"             = "#009E73",
  "Animal/Vegetable Fats & Oils"   = "#CC79A7",
  "Prepared Foodstuffs"            = "#0072B2"
)

pal_agreement <- c(
  "MFN"   = "#534AB7",
  "NAFTA/USMCA" = "#1D9E75",
  "Bilateral"   = "#D85A30",
  "Other GSP"   = "#BA7517"
)

# ── 1. Data Ingestion ────────────────────────────────────────────────────────

message("── Downloading data from GitHub …")

base_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-28/"

safe_read <- function(url) {
  tryCatch(
    readr::read_csv(url, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      message("  ✖  Could not fetch: ", url, "\n  Reason: ", conditionMessage(e))
      NULL
    }
  )
}

agreements         <- safe_read(paste0(base_url, "agreements.csv"))
quantity_codes     <- safe_read(paste0(base_url, "quantity_codes.csv"))
tariff_agricultural <- safe_read(paste0(base_url, "tariff_agricultural.csv"))
tariff_codes       <- safe_read(paste0(base_url, "tariff_codes.csv"))

stopifnot(
  !is.null(agreements),
  !is.null(tariff_agricultural)
)

message("  ✔  Raw rows: ", nrow(tariff_agricultural),
        " | HTS codes: ", n_distinct(tariff_agricultural$hts8))


# ── 2. Cleaning & Feature Engineering ───────────────────────────────────────

message("── Engineering features …")

# 2a. Normalise dates
tariff_clean <- tariff_agricultural |>
  janitor::clean_names() |>
  mutate(
    begin_date   = as.Date(begin_effective_date),
    end_date     = as.Date(end_effective_date),
    begin_year   = year(begin_date),
    end_year     = year(end_date),
    # Convert ad_val_rate stored as decimal (e.g. 0.05 = 5%) to percentage
    ad_val_pct   = case_when(
      ad_val_rate >= 1 ~ ad_val_rate,            # already a percentage
      !is.na(ad_val_rate) ~ ad_val_rate * 100,   # decimal → percent
      TRUE ~ NA_real_
    ),
    # HTS chapter (first 2 digits)
    hts_chapter  = as.integer(str_sub(as.character(hts8), 1, 2)),
    # Section classification
    section = case_when(
      hts_chapter <= 5  ~ "Live Animals & Animal Products",
      hts_chapter <= 14 ~ "Vegetable Products",
      hts_chapter <= 15 ~ "Animal/Vegetable Fats & Oils",
      hts_chapter <= 24 ~ "Prepared Foodstuffs",
      TRUE              ~ "Other"
    ),
    section = factor(section, levels = names(pal_section))
  )

# 2b. Expand to annual panel (every HTS×agreement×year combination)
YEARS <- 2000:2024

annual_panel <- tariff_clean |>
  distinct(hts8, agreement) |>
  crossing(year = YEARS) |>
  left_join(
    tariff_clean |> select(hts8, agreement, begin_year, end_year,
                           ad_val_pct, specific_rate, hts_chapter, section),
    join_by(hts8, agreement,
            year >= begin_year,
            year <= end_year)
  ) |>
  filter(!is.na(ad_val_pct), ad_val_pct >= 0, ad_val_pct < 500) |>
  # Deduplicate (keep first match per hts8×agreement×year)
  distinct(hts8, agreement, year, .keep_all = TRUE)

# 2c. Join agreement labels
annual_panel <- annual_panel |>
  left_join(agreements |> janitor::clean_names(), by = "agreement")

# 2d. Agreement group classification
annual_panel <- annual_panel |>
  mutate(
    agree_group = case_when(
      str_detect(agreement_full, regex("most favored|MFN", ignore_case = TRUE)) ~ "MFN",
      str_detect(agreement_full, regex("NAFTA|USMCA|Canada|Mexico", ignore_case = TRUE)) ~ "NAFTA/USMCA",
      str_detect(agreement_full, regex("bilateral|Australia|Chile|Korea|Colombia|Peru|Singapore", ignore_case = TRUE)) ~ "Bilateral",
      TRUE ~ "Other GSP"
    )
  )

# 2e. Extract MFN rates separately (reference baseline)
mfn_panel <- annual_panel |>
  filter(agree_group == "MFN") |>
  select(hts8, year, section, hts_chapter, mfn_rate = ad_val_pct)

message("  ✔  Annual panel rows: ", nrow(annual_panel),
        " | MFN panel rows: ", nrow(mfn_panel))


# ── 3. Exploratory Statistics ────────────────────────────────────────────────

message("── Computing summary statistics …")

# Section × Year summaries
section_yr <- mfn_panel |>
  group_by(section, year) |>
  summarise(
    mean_rate   = mean(mfn_rate, na.rm = TRUE),
    median_rate = median(mfn_rate, na.rm = TRUE),
    sd_rate     = sd(mfn_rate, na.rm = TRUE),
    n_products  = n_distinct(hts8),
    .groups = "drop"
  ) |>
  filter(n_products >= 5)

# Volatility: SD of MFN rate across all years
volatility_by_product <- mfn_panel |>
  group_by(hts8, section) |>
  filter(n() >= 5) |>
  summarise(
    rate_sd       = sd(mfn_rate, na.rm = TRUE),
    rate_range    = max(mfn_rate, na.rm = TRUE) - min(mfn_rate, na.rm = TRUE),
    mean_mfn      = mean(mfn_rate, na.rm = TRUE),
    first_rate    = first(mfn_rate[year == min(year)]),
    last_rate     = last(mfn_rate[year == max(year)]),
    change        = last_rate - first_rate,
    .groups = "drop"
  ) |>
  arrange(desc(rate_sd))

# Agreement coverage per product
agreement_coverage <- annual_panel |>
  group_by(hts8, section, agree_group) |>
  summarise(years_covered = n_distinct(year), .groups = "drop") |>
  pivot_wider(names_from = agree_group, values_from = years_covered, values_fill = 0) |>
  clean_names()

# MFN vs preferential divergence
mfn_vs_pref <- annual_panel |>
  filter(agree_group %in% c("MFN", "NAFTA/USMCA", "Bilateral")) |>
  select(hts8, year, section, agree_group, ad_val_pct) |>
  pivot_wider(names_from = agree_group, values_from = ad_val_pct,
              values_fn = mean) |>
  clean_names() |>
  filter(!is.na(mfn)) |>
  mutate(
    nafta_saving  = mfn - coalesce(nafta_usmca, mfn),
    bilat_saving  = mfn - coalesce(bilateral,  mfn)
  )

# NAFTA → USMCA trajectory
nafta_usmca_trend <- annual_panel |>
  filter(agree_group == "NAFTA/USMCA") |>
  group_by(year) |>
  summarise(
    mean_rate   = mean(ad_val_pct, na.rm = TRUE),
    median_rate = median(ad_val_pct, na.rm = TRUE),
    n_products  = n_distinct(hts8),
    .groups = "drop"
  )


# ════════════════════════════════════════════════════════════════════════════
# ── 4. FIGURE 1 — MFN rate trajectories (small-multiples) ───────────────────
# ════════════════════════════════════════════════════════════════════════════

message("── Figure 1: MFN trajectories …")

# Ribbon data (mean ± 1 SD)
section_ribbon <- section_yr |>
  mutate(
    lo = pmax(mean_rate - sd_rate, 0),
    hi = mean_rate + sd_rate
  )

# Background (all sections, faded) for small-multiples
bg_data <- section_yr |>
  rename(bg_section = section)

fig1 <- ggplot() +
  # Grey background lines for context
  geom_line(
    data    = bg_data,
    mapping = aes(x = year, y = mean_rate, group = bg_section),
    colour  = "#D3D1C7", linewidth = 0.6, alpha = 0.7
  ) +
  # Ribbon (uncertainty band)
  geom_ribbon(
    data    = section_ribbon,
    mapping = aes(x = year, ymin = lo, ymax = hi, fill = section),
    alpha   = 0.20
  ) +
  # Highlighted section line
  geom_line(
    data    = section_yr,
    mapping = aes(x = year, y = mean_rate, colour = section),
    linewidth = 1.3
  ) +
  geom_point(
    data    = section_yr,
    mapping = aes(x = year, y = mean_rate, colour = section),
    size    = 1.5, shape = 21, fill = "white", stroke = 1.0
  ) +
  scale_colour_manual(values = pal_section, guide = "none") +
  scale_fill_manual(values = pal_section, guide = "none") +
  scale_x_continuous(breaks = seq(2000, 2024, 6)) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    limits = c(0, NA)
  ) +
  facet_wrap(~section, ncol = 2, scales = "free_y") +
  labs(
    title    = "MFN tariff trends by agricultural section (2000–2024)",
    subtitle = "Mean ad valorem rate ± 1 SD (shaded). Grey lines show all four sections for comparison.",
    x        = NULL,
    y        = "Average MFN tariff rate",
    caption  = "Source: USITC Tariff Database · TidyTuesday 2026 Week 17 · Products with ≥ 5 data points."
  )

ggsave("outputs/fig1_mfn_trajectories.png", fig1,
       width = 11, height = 7, dpi = 300, bg = "#FAFAF8")
message("  ✔  Saved fig1_mfn_trajectories.png")


# ════════════════════════════════════════════════════════════════════════════
# ── 5. FIGURE 2 — Agreement coverage heatmap ────────────────────────────────
# ════════════════════════════════════════════════════════════════════════════

message("── Figure 2: Agreement heatmap …")

# Summarise: for each chapter × agreement group → % of years covered
heatmap_data <- annual_panel |>
  mutate(chapter_label = glue("Ch. {str_pad(hts_chapter, 2, 'left', '0')}")) |>
  group_by(section, hts_chapter, chapter_label, agree_group) |>
  summarise(
    n_products_years = n(),
    avg_rate         = mean(ad_val_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(agree_group != "Other GSP") |>
  mutate(
    agree_group = factor(agree_group, levels = c("MFN", "NAFTA/USMCA", "Bilateral")),
    chapter_label = fct_reorder(chapter_label, hts_chapter)
  )

fig2 <- ggplot(heatmap_data,
               aes(x = agree_group, y = fct_rev(chapter_label),
                   fill = avg_rate)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(
    aes(label = ifelse(avg_rate >= 1, sprintf("%.1f%%", avg_rate), "")),
    size = 2.4, colour = "white", fontface = "bold"
  ) +
  scale_fill_gradientn(
    colours  = c("#E6F1FB", "#378ADD", "#185FA5", "#042C53"),
    na.value = "#F1EFE8",
    name     = "Avg. rate",
    labels   = label_number(suffix = "%"),
    guide    = guide_colourbar(
      barwidth  = 10,
      barheight = 0.5,
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  facet_grid(section ~ ., scales = "free_y", space = "free_y") +
  labs(
    title    = "Average tariff rate by HTS chapter and trade agreement",
    subtitle = "Darker cells = higher rates. Blank cells = no data for that chapter × agreement combination.",
    x        = "Trade agreement group",
    y        = "HTS chapter",
    caption  = "Source: USITC Tariff Database · TidyTuesday 2026 Week 17"
  ) +
  theme(
    panel.grid   = element_blank(),
    axis.text.y  = element_text(size = 7),
    strip.text.y = element_text(angle = 0, hjust = 0, size = 7),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(2.5, "cm")
  )

ggsave("outputs/fig2_agreement_heatmap.png", fig2,
       width = 10, height = 13, dpi = 300, bg = "#FAFAF8")
message("  ✔  Saved fig2_agreement_heatmap.png")


# ════════════════════════════════════════════════════════════════════════════
# ── 6. FIGURE 3 — MFN vs preferential divergence (dumbbell) ─────────────────
# ════════════════════════════════════════════════════════════════════════════

message("── Figure 3: Dumbbell chart …")

# Summarise by chapter
dumbbell_data <- annual_panel |>
  filter(agree_group %in% c("MFN", "NAFTA/USMCA")) |>
  group_by(hts_chapter, section, agree_group) |>
  summarise(avg_rate = mean(ad_val_pct, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = agree_group, values_from = avg_rate) |>
  clean_names() |>
  filter(!is.na(mfn), !is.na(nafta_usmca)) |>
  mutate(
    saving        = mfn - nafta_usmca,
    chapter_label = glue("Ch. {str_pad(hts_chapter, 2, 'left', '0')}"),
    chapter_label = fct_reorder(chapter_label, saving)
  ) |>
  filter(saving > 0)

fig3 <- ggplot(dumbbell_data) +
  # Connecting segment
  geom_segment(
    aes(x = nafta_usmca, xend = mfn,
        y = chapter_label, yend = chapter_label,
        colour = section),
    linewidth = 1.6, alpha = 0.45
  ) +
  # NAFTA/USMCA endpoint
  geom_point(
    aes(x = nafta_usmca, y = chapter_label, colour = section),
    size = 3.5, shape = 21, fill = "white", stroke = 1.2
  ) +
  # MFN endpoint
  geom_point(
    aes(x = mfn, y = chapter_label, colour = section),
    size = 3.5
  ) +
  # Saving label
  geom_text(
    aes(x = mfn + 0.3, y = chapter_label,
        label = sprintf("−%.1f pp", saving)),
    hjust = 0, size = 2.8, colour = "#5F5E5A"
  ) +
  scale_colour_manual(values = pal_section, name = "Section") +
  scale_x_continuous(
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0.01, 0.15))
  ) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  labs(
    title    = "NAFTA/USMCA preferential rates vs MFN rates by chapter",
    subtitle = "Open circle = NAFTA/USMCA rate · Filled circle = MFN rate · Label = percentage-point saving",
    x        = "Average ad valorem rate",
    y        = NULL,
    caption  = "Source: USITC Tariff Database · TidyTuesday 2026 Week 17"
  ) +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/fig3_dumbbell_mfn_vs_nafta.png", fig3,
       width = 10, height = 8, dpi = 300, bg = "#FAFAF8")
message("  ✔  Saved fig3_dumbbell_mfn_vs_nafta.png")


# ════════════════════════════════════════════════════════════════════════════
# ── 7. FIGURE 4 — Tariff volatility ranking (lollipop) ──────────────────────
# ════════════════════════════════════════════════════════════════════════════

message("── Figure 4: Volatility lollipop …")

# Top 30 most volatile HTS-8 products
top_volatile <- volatility_by_product |>
  filter(mean_mfn > 0) |>
  slice_max(rate_sd, n = 30) |>
  left_join(
    tariff_codes |> janitor::clean_names() |>
      select(hts8, description = starts_with("brief")) |>
      distinct(hts8, .keep_all = TRUE),
    by = "hts8"
  ) |>
  mutate(
    label = coalesce(str_trunc(description, 45), as.character(hts8)),
    label = fct_reorder(label, rate_sd)
  )

fig4 <- ggplot(top_volatile, aes(x = rate_sd, y = label, colour = section)) +
  geom_segment(
    aes(x = 0, xend = rate_sd, y = label, yend = label),
    linewidth = 0.9, alpha = 0.55
  ) +
  geom_point(size = 3.5) +
  geom_text(
    aes(label = sprintf("%.1f%%", rate_sd)),
    hjust = -0.3, size = 2.6, colour = "#5F5E5A"
  ) +
  scale_colour_manual(values = pal_section, name = "Section") +
  scale_x_continuous(
    labels  = label_number(suffix = "%"),
    expand  = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title    = "Top 30 most volatile agricultural tariff lines (MFN)",
    subtitle = "Ranked by standard deviation of annual MFN ad valorem rate, 2000–2024.",
    x        = "Rate standard deviation (pp)",
    y        = NULL,
    caption  = "Source: USITC Tariff Database · TidyTuesday 2026 Week 17"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 7.5)
  )

ggsave("outputs/fig4_volatility_lollipop.png", fig4,
       width = 12, height = 10, dpi = 300, bg = "#FAFAF8")
message("  ✔  Saved fig4_volatility_lollipop.png")


# ════════════════════════════════════════════════════════════════════════════
# ── 8. FIGURE 5 — NAFTA → USMCA transition (annotated ribbon) ──────────────
# ════════════════════════════════════════════════════════════════════════════

message("── Figure 5: NAFTA → USMCA …")

nafta_section <- annual_panel |>
  filter(agree_group == "NAFTA/USMCA") |>
  group_by(year, section) |>
  summarise(
    mean_rate = mean(ad_val_pct, na.rm = TRUE),
    lo        = quantile(ad_val_pct, 0.25, na.rm = TRUE),
    hi        = quantile(ad_val_pct, 0.75, na.rm = TRUE),
    .groups   = "drop"
  )

fig5 <- ggplot(nafta_section, aes(x = year, colour = section, fill = section)) +
  geom_vline(xintercept = 2020, linetype = "dashed",
             colour = "#534AB7", linewidth = 0.7, alpha = 0.7) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, linewidth = 0) +
  geom_line(aes(y = mean_rate), linewidth = 1.2) +
  geom_point(aes(y = mean_rate), size = 1.6, shape = 21, fill = "white") +
  annotate(
    "richtext",
    x = 2020.3, y = Inf, hjust = 0, vjust = 1.3, size = 3,
    label    = "**USMCA** replaces NAFTA",
    fill     = NA, label.colour = NA, colour = "#534AB7"
  ) +
  scale_colour_manual(values = pal_section, name = NULL) +
  scale_fill_manual(values = pal_section, name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  scale_y_continuous(
    labels  = label_number(suffix = "%"),
    limits  = c(0, NA)
  ) +
  labs(
    title    = "NAFTA → USMCA: preferential tariff rates over time",
    subtitle = "Mean (line) ± IQR (ribbon) of ad valorem rates for NAFTA/USMCA agreement by section.",
    x        = NULL,
    y        = "Average preferential rate",
    caption  = "Source: USITC Tariff Database · TidyTuesday 2026 Week 17"
  ) +
  theme(legend.position = "top")

ggsave("outputs/fig5_nafta_usmca_transition.png", fig5,
       width = 11, height = 6, dpi = 300, bg = "#FAFAF8")
message("  ✔  Saved fig5_nafta_usmca_transition.png")


# ════════════════════════════════════════════════════════════════════════════
# ── 9. FIGURE 6 — Rate-change waterfall (biggest movers 1997–2024) ───────────
# ════════════════════════════════════════════════════════════════════════════

message("── Figure 6: Waterfall of biggest movers …")

# Products with substantial change in MFN from earliest to latest year
big_movers <- mfn_panel |>
  group_by(hts8, section) |>
  filter(n_distinct(year) >= 10) |>
  summarise(
    rate_start = mean(mfn_rate[year == min(year)], na.rm = TRUE),
    rate_end   = mean(mfn_rate[year == max(year)], na.rm = TRUE),
    change     = rate_end - rate_start,
    .groups    = "drop"
  ) |>
  filter(!is.na(change)) |>
  mutate(direction = if_else(change > 0, "Increase", "Decrease")) |>
  group_by(direction) |>
  slice_max(abs(change), n = 15) |>
  ungroup() |>
  left_join(
    tariff_codes |> janitor::clean_names() |>
      distinct(hts8, .keep_all = TRUE) |>
      select(hts8, description = starts_with("brief")),
    by = "hts8"
  ) |>
  mutate(
    label = coalesce(str_trunc(description, 40), as.character(hts8)),
    label = fct_reorder(label, change)
  )

fig6 <- ggplot(big_movers, aes(x = change, y = label, fill = section)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_vline(xintercept = 0, colour = "#2C2C2A", linewidth = 0.6) +
  geom_text(
    aes(label  = sprintf("%+.1f pp", change),
        hjust  = if_else(change > 0, -0.15, 1.15)),
    size   = 2.7, colour = "#2C2C2A"
  ) +
  scale_fill_manual(values = pal_section, name = "Section") +
  scale_x_continuous(
    labels  = label_number(suffix = " pp", style_positive = "plus"),
    expand  = expansion(mult = 0.20)
  ) +
  labs(
    title    = "Biggest MFN tariff rate changes: earliest vs latest observation",
    subtitle = "Top 15 increases and top 15 decreases (percentage-point change in ad valorem rate).",
    x        = "Change in MFN rate (pp)",
    y        = NULL,
    caption  = "Source: USITC Tariff Database · TidyTuesday 2026 Week 17"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 7)
  )

ggsave("outputs/fig6_rate_change_waterfall.png", fig6,
       width = 13, height = 10, dpi = 300, bg = "#FAFAF8")
message("  ✔  Saved fig6_rate_change_waterfall.png")


# ════════════════════════════════════════════════════════════════════════════
# ── 10. FIGURE 7 — Agreement count vs MFN rate scatter (bubble) ──────────────
# ════════════════════════════════════════════════════════════════════════════

message("── Figure 7: Bubble scatter …")

bubble_data <- annual_panel |>
  group_by(hts8, section) |>
  summarise(
    n_agreements = n_distinct(agree_group[agree_group != "MFN"]),
    mean_mfn     = mean(ad_val_pct[agree_group == "MFN"], na.rm = TRUE),
    pref_saving  = mean_mfn - mean(ad_val_pct[agree_group != "MFN"], na.rm = TRUE),
    .groups      = "drop"
  ) |>
  filter(!is.na(mean_mfn), mean_mfn > 0, is.finite(pref_saving)) |>
  mutate(pref_saving = pmax(pref_saving, 0))

# Summarise to chapter level to reduce overplotting
bubble_chapter <- bubble_data |>
  mutate(hts_chapter = as.integer(str_sub(as.character(hts8), 1, 2))) |>
  group_by(hts_chapter, section) |>
  summarise(
    n_agreements = mean(n_agreements, na.rm = TRUE),
    mean_mfn     = mean(mean_mfn, na.rm = TRUE),
    pref_saving  = mean(pref_saving, na.rm = TRUE),
    n_products   = n(),
    .groups      = "drop"
  ) |>
  mutate(chapter_label = glue("Ch.{hts_chapter}"))

fig7 <- ggplot(bubble_chapter,
               aes(x = n_agreements, y = mean_mfn,
                   size = n_products, colour = section)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(
    aes(label = chapter_label),
    size = 2.6, max.overlaps = 20,
    segment.colour = "#B4B2A9", segment.linewidth = 0.4
  ) +
  scale_size_area(max_size = 14, name = "# products") +
  scale_colour_manual(values = pal_section, name = "Section") +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title    = "Do higher-tariff chapters have more preferential agreements?",
    subtitle = "Each bubble = one HTS chapter. Size = number of tariff lines. X-axis = average number\nof distinct preferential agreement groups covering that chapter.",
    x        = "Average number of preferential agreement groups",
    y        = "Mean MFN tariff rate",
    caption  = "Source: USITC Tariff Database · TidyTuesday 2026 Week 17"
  ) +
  guides(
    size   = guide_legend(override.aes = list(colour = "#888780")),
    colour = guide_legend(override.aes = list(size   = 4))
  )

ggsave("outputs/fig7_bubble_agreements_vs_mfn.png", fig7,
       width = 10, height = 7, dpi = 300, bg = "#FAFAF8")
message("  ✔  Saved fig7_bubble_agreements_vs_mfn.png")


# ════════════════════════════════════════════════════════════════════════════
# ── 11. FIGURE 8 — Specific vs ad valorem composition (stacked area) ─────────
# ════════════════════════════════════════════════════════════════════════════

message("── Figure 8: Rate composition stacked area …")

composition_data <- annual_panel |>
  filter(agree_group == "MFN") |>
  mutate(
    has_specific  = !is.na(specific_rate) & specific_rate > 0,
    has_ad_val    = !is.na(ad_val_pct)    & ad_val_pct    > 0,
    rate_type = case_when(
      has_specific & has_ad_val  ~ "Compound (both)",
      has_specific & !has_ad_val ~ "Specific only",
      !has_specific & has_ad_val ~ "Ad valorem only",
      TRUE                       ~ "Zero / unknown"
    )
  ) |>
  count(year, section, rate_type) |>
  group_by(year, section) |>
  mutate(share = n / sum(n)) |>
  ungroup()

type_pal <- c(
  "Ad valorem only"   = "#378ADD",
  "Specific only"     = "#D85A30",
  "Compound (both)"   = "#BA7517",
  "Zero / unknown"    = "#D3D1C7"
)

fig8 <- ggplot(composition_data,
               aes(x = year, y = share, fill = rate_type)) +
  geom_area(alpha = 0.85, position = "stack") +
  scale_fill_manual(values = type_pal, name = "Tariff type") +
  scale_x_continuous(breaks = seq(2000, 2024, 6)) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  facet_wrap(~section, ncol = 2) +
  labs(
    title    = "Composition of MFN tariff types by agricultural section",
    subtitle = "Share of tariff lines with ad valorem, specific, or compound (both) rate structures.",
    x        = NULL,
    y        = "Share of tariff lines",
    caption  = "Source: USITC Tariff Database · TidyTuesday 2026 Week 17"
  ) +
  theme(legend.position = "bottom")

ggsave("outputs/fig8_tariff_type_composition.png", fig8,
       width = 11, height = 7, dpi = 300, bg = "#FAFAF8")
message("  ✔  Saved fig8_tariff_type_composition.png")


# ════════════════════════════════════════════════════════════════════════════
# ── 12. FIGURE 9 — Ridge plot: MFN rate distributions by year (every 4 yrs) ──
# ════════════════════════════════════════════════════════════════════════════

message("── Figure 9: Ridge distributions …")

if (requireNamespace("ggridges", quietly = TRUE)) {
  ridge_data <- mfn_panel |>
    filter(year %in% seq(2000, 2024, 4), mfn_rate > 0, mfn_rate <= 100) |>
    mutate(year_f = factor(year))
  
  fig9 <- ggplot(ridge_data,
                 aes(x = mfn_rate, y = year_f, fill = section)) +
    ggridges::geom_density_ridges(
      aes(fill = section),
      alpha        = 0.65,
      scale        = 1.8,
      rel_min_height = 0.01,
      bandwidth    = 1.5
    ) +
    scale_fill_manual(values = pal_section, name = "Section") +
    scale_x_continuous(
      labels = label_number(suffix = "%"),
      limits = c(0, 60)
    ) +
    facet_wrap(~section, ncol = 2) +
    labs(
      title    = "Distribution of MFN tariff rates over time",
      subtitle = "Density ridgelines every 4 years. Rates truncated at 60% for readability.",
      x        = "MFN ad valorem rate",
      y        = NULL,
      caption  = "Source: USITC Tariff Database · TidyTuesday 2026 Week 17"
    ) +
    theme(legend.position = "none")
  
  ggsave("outputs/fig9_ridge_distributions.png", fig9,
         width = 11, height = 8, dpi = 300, bg = "#FAFAF8")
  message("  ✔  Saved fig9_ridge_distributions.png")
}


# ════════════════════════════════════════════════════════════════════════════
# ── 13. SUMMARY TABLE (gt) ──────────────────────────────────────────────────
# ════════════════════════════════════════════════════════════════════════════

message("── Summary table …")

summary_table <- mfn_panel |>
  group_by(section) |>
  summarise(
    `Tariff lines`    = n_distinct(hts8),
    `Avg MFN rate`    = mean(mfn_rate, na.rm = TRUE),
    `Median MFN rate` = median(mfn_rate, na.rm = TRUE),
    `Max MFN rate`    = max(mfn_rate, na.rm = TRUE),
    `Avg change (pp)` = {
      tmp <- mfn_panel |>
        filter(section == cur_group()$section) |>
        group_by(hts8) |>
        filter(n() >= 2) |>
        summarise(chg = last(mfn_rate) - first(mfn_rate), .groups = "drop")
      mean(tmp$chg, na.rm = TRUE)
    },
    .groups = "drop"
  )

gt_table <- summary_table |>
  gt() |>
  tab_header(
    title    = md("**US Agricultural Tariff Summary: MFN Rates (2000–2024)**"),
    subtitle = md("*Ad valorem rates (%). Source: USITC Tariff Database, TidyTuesday 2026 Week 17*")
  ) |>
  cols_label(section = "HTS Section") |>
  fmt_number(
    columns  = c(`Avg MFN rate`, `Median MFN rate`, `Max MFN rate`, `Avg change (pp)`),
    decimals = 2,
    suffix   = "%"
  ) |>
  fmt_integer(columns = `Tariff lines`) |>
  data_color(
    columns = `Avg MFN rate`,
    palette = c("#E6F1FB", "#185FA5")
  ) |>
  data_color(
    columns  = `Avg change (pp)`,
    palette  = c("#D85A30", "#F1EFE8", "#185FA5"),
    domain   = NULL,
    na_color = "#F1EFE8"
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_options(
    table.font.size           = px(13),
    heading.title.font.size   = px(16),
    heading.subtitle.font.size = px(12),
    table.border.top.style    = "hidden",
    column_labels.border.top.width = px(2),
    column_labels.border.top.color = "#534AB7"
  )

gtsave(gt_table, "outputs/summary_table.html")
message("  ✔  Saved summary_table.html")

# ════════════════════════════════════════════════════════════════════════════
# ── 14. COMPOSITE DASHBOARD (patchwork) ─────────────────────────────────────
# ════════════════════════════════════════════════════════════════════════════

message("── Composite dashboard …")

# Smaller versions for dashboard
fig1b <- fig1 + theme(plot.title = element_text(size = 10),
                      plot.subtitle = element_text(size = 8))
fig3b <- fig3 + theme(plot.title = element_text(size = 10),
                      plot.subtitle = element_text(size = 8))
fig5b <- fig5 + theme(plot.title = element_text(size = 10),
                      plot.subtitle = element_text(size = 8))
fig7b <- fig7 + theme(plot.title = element_text(size = 10),
                      plot.subtitle = element_text(size = 8))

dashboard <- (fig1b | fig5b) / (fig3b | fig7b) +
  plot_annotation(
    title   = "US Agricultural Tariff Analysis Dashboard",
    subtitle = "USITC Tariff Database · HTS Chapters 1–24 · TidyTuesday 2026 Week 17",
    theme   = theme_tariff(base_size = 11) +
      theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10, colour = "#5F5E5A")
      )
  )

ggsave("outputs/dashboard.png", dashboard,
       width = 20, height = 14, dpi = 300, bg = "#FAFAF8")
message("  ✔  Saved dashboard.png")


# ── 15. Session info ─────────────────────────────────────────────────────────

message("\n── All outputs saved to ./outputs/ ──")
message("\n── Session info ──")
sessionInfo()