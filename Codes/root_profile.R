# CRK Root profile 
# Moeka last updated 2025-05-16

library(dplyr)
library(ggplot2)

root_raw <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/14qw9dRYHd8dZjzA8G95VphuTjELP4HycTQqvvmr-zjY/edit?gid=0#gid=0")

# clean up
root <- 
  root_raw %>% 
  mutate(
    Plot = as.character(unlist(Plot)),
    Replicate_ID = as.character(unlist(Replicate_ID)),
    Depth = as.character(unlist(`Depth (cm)`)),
    LF = as.numeric(`live fine (g)`), 
    DF = as.numeric(`dead fine (g)`)
  ) %>% 
  select(Plot, Replicate_ID, Depth, LF, DF, RE_DF, RE_LF) 

# Combine 30–45 cm
combined_30_45 <- root %>%
  filter(Depth %in% c("30-35", "35-40", "40-45")) %>%
  group_by(Plot, Replicate_ID) %>%
  summarise(
    Depth = "30-45",
    LF = sum(LF, na.rm = TRUE),
    DF = sum(DF, na.rm = TRUE),
    RE_DF = NA_real_,  # placeholder
    RE_LF = NA_real_,
    .groups = "drop"
  )

# Combine 75-90 cm
combined_75_90 <- root %>%
  filter(Depth %in% c("75-80", "80-85", "85-90")) %>%
  group_by(Plot, Replicate_ID) %>%
  summarise(
    Depth = "75-90",
    LF = sum(LF, na.rm = TRUE),
    DF = sum(DF, na.rm = TRUE),
    RE_DF = NA_real_,
    RE_LF = NA_real_,
    .groups = "drop"
  )

# Bind both new groups and remove original individual depth slices
root_updated <- 
  root %>%
  filter(!Depth %in% c("30-35", "35-40", "40-45", "75-80", "80-85", "85-90")) %>%
  bind_rows(combined_30_45, combined_75_90) %>%
  arrange(Plot, Replicate_ID, Depth) %>% 
  mutate(
    LF = ifelse(is.na(RE_LF), LF, RE_LF),
    DF = ifelse(is.na(RE_DF), DF, RE_DF),
    FRB = LF + DF
  ) %>% 
  select(-RE_DF, -RE_LF) %>% 
  filter(!Depth %in% c("bottom 5", "middle 5", "top 5"))

# get summary with mean and sd
root_stat <-
  root_updated %>% 
  group_by(Depth) %>% 
  summarise(
    LF_mean = mean(LF),
    LF_sd = sd(LF),
    DF_mean = mean(DF),
    DF_sd = sd(DF),
    FRB_mean = mean(FRB),
    FRB_sd = sd(FRB),
    n = n()
  ) 

# depth order
root_stat$Depth <- 
  factor(root_stat$Depth, levels = rev(c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-45", "45-60", "75-90")))

# line until
line_subset <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30")


# plot altogether
root_long <- 
  root_stat %>%
  select(Depth, LF_mean, LF_sd, DF_mean, DF_sd, FRB_mean, FRB_sd) %>%
  tidyr::pivot_longer(
    cols = -Depth,
    names_to = c("Type", ".value"),
    names_pattern = "(.*)_([a-z]+)"
  ) %>% 
  mutate(Type_full = 
           case_when(
             Type == "LF" ~ "Live",
             Type == "DF" ~ "Dead",
             Type == "FRB" ~ "Total"
             ))

# Reorder factor levels for plotting
root_long$Type_full <- factor(root_long$Type_full, levels = c("Live", "Dead", "Total"))
root_long$Depth <- factor(root_long$Depth, levels = levels(root_stat$Depth))  # keep depth order


root_plt_theme <-
  theme(
    axis.title.x = element_text(size = 25), 
    axis.text.x = element_text(size = 25),
    axis.title.y = element_text(size = 25), 
    axis.text.y = element_text(size = 25),
    legend.title = element_blank(), 
    legend.text = element_text(size = 25),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.1),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )

theme() 

# Set dodge to avoid overlapping
pd <- position_dodge(width = 0.15)

ggplot(root_long, aes(x = Depth, y = mean, color = Type_full, shape = Type_full, linetype = Type_full)) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = pd) +
  geom_line(data = filter(root_long, Depth %in% line_subset),
            aes(group = Type_full), position = pd, linewidth = 1) +
  scale_linetype_manual(values = c("dashed", "dotdash", "solid")) +  # Solid for FRB
  scale_shape_manual(values = c(16, 17, 15)) +  # Different shapes
  scale_color_manual(values = c("#20A39E", "darkorange", "black")) +  # Custom colors
  coord_flip() +
  labs(y = "Root biomass (g)", x = "Soil Depth (cm)", color = "Root Type", shape = "Root Type", linetype = "Root Type") +
  theme_classic() +
  root_plt_theme

