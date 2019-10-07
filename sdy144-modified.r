library(ImmuneSpaceR)
library(data.table)
library(ggplot2)

study <- CreateConnection("SDY144")

flow <- study$getDataset("fcs_analyzed_result")
hai <- study$getDataset("hai")
vn <- study$getDataset("neut_ab_titer")

# data manipulation
pb <- flow[population_name_reported %in% c("BDCA3+ myeloid dendritic cell", "CCR6+ CD8+ T cell")]
pb <- pb[, population_cell_number := as.numeric(population_cell_number)]
pb <- pb[study_time_collected == 28 & study_time_collected_unit == "Days"]
pb <- pb[, list(participant_id, population_cell_number, population_name_reported)]


# hai
hai <- hai[, response:= value_preferred / value_preferred[study_time_collected == 0], 
  by = "virus,cohort,participant_id"][study_time_collected == 30]
hai <- hai[, list(participant_id, virus, response)]
dat_hai <- merge(hai, pb, by = "participant_id", allow.cartesian = TRUE)

# vn
vn <- vn[, response:= value_preferred/value_preferred[study_time_collected == 0], 
  by = "virus,cohort,participant_id"][study_time_collected == 30]
vn <- vn[, list(participant_id, virus, response)]
dat_vn <- merge(vn, pb, by = "participant_id", allow.cartesian = TRUE)


# correlation between myeloid dentric cells and t cells 28 days after vaccination with
# and fold-increase of HI titers from baseline to day 30 after vaccination
ggplot(dat_hai, aes(x = population_cell_number, y = response)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(virus ~ population_name_reported, scale = "free") +
  xlab("Number of cells") +
  ylab("HI fold-increase Day 30 vs. baseline") +
  theme_IS()

# correlation between myeloid dentric cells and t cells 28 days after vaccination with
# and fold-increase of VN titers from baseline to day 30 after vaccination
ggplot(dat_vn, aes(x = population_cell_number, y = response)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(virus ~ population_name_reported, scale = "free") +
  xlab("Number of cells") +
  ylab("VN fold-increase Day 30 vs. baseline") +
  theme_IS()