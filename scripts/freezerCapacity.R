require(readr)
require(DBI)
require(odbc)
require(scales)

# con <- dbConnect(odbc(), "Freezerworks64")
# freezers <- dbReadTable(con, "FreezerPhysical")
# dbDisconnect(con)
# rm(con)

# freezerCapacity <- read_csv("../freezerCapacity.csv")
# containerRatios <- read_csv("../containerRatios.csv")
# basketCounts <- readRDS("data/basketCounts.RDS")

# meanCR <- mean(unique(containerRatios$CT_Ratio))

freezer_counts <- counts %>%
  group_by(FreezerPhysName) %>%
  count(CONTAINER_TYPE) %>%
  left_join(freezers %>% 
              select(FreezerPhysName, Description)) %>%
  left_join(freezerCapacity %>%
              select(-Size),
            by = c("Description" = "DESCRIPTION")) %>%
  left_join(containerRatios) %>%
  mutate(uses_capacity = (n*CT_Ratio/Capacity)) %>%
  select(FreezerPhysName, CONTAINER_TYPE, n, CT_Ratio, Capacity, uses_capacity) %>%
  ungroup()
freezer_counts$FreezerPhysName <- as.factor(freezer_counts$FreezerPhysName)
freezer_counts$CONTAINER_TYPE <- as.factor(freezer_counts$CONTAINER_TYPE)
names(freezer_counts) <- tolower(names(freezer_counts))

freezer_space_used <- freezer_counts %>% 
  group_by(freezerphysname) %>%
  # mutate(uses_capacity = ifelse(is.na(uses_capacity), n*meanCR/capacity, uses_capacity)) %>%
  summarise(used_capacity = sum(uses_capacity, na.rm=T)) %>%
  mutate(inputID = paste0(gsub(' ' ,'', freezerphysname), 'slide')) %>%
  left_join(basketCounts) %>%
  mutate(used_capacity = used_capacity + value/12) %>%
  select(-inputID, -value) %>%
  filter(used_capacity > 0)

# Estimate used space for the entire bank
total_est <- sum(freezer_space_used$used_capacity) / length(unique(freezer_space_used$freezerphysname))
total_est <- round(total_est, 4)*100

# Get plot orders
freezer_plot_order_all <- freezer_space_used %>%
  mutate(freezerphysname = fct_reorder(freezerphysname, used_capacity)) %>%
  pull(freezerphysname) %>%
  levels()
freezer_plot_order_same <- freezer_space_used %>%
  mutate(freezerphysname = fct_reorder(freezerphysname, used_capacity)) %>%
  filter(freezerphysname %in% unique(freezer_space_used$freezerphysname)) %>%
  pull(freezerphysname) %>%
  droplevels() %>%
  levels()

na_containers <- c(NA, 
                   levels(freezer_counts$container_type)[-which(levels(freezer_counts$container_type) %in% containerRatios$CONTAINER_TYPE)])

# Bad container type counts
bad_containers <- freezer_counts %>%
  filter(freezerphysname %in% levels(freezer_space_used$freezerphysname),
         container_type %in% na_containers) %>%
  group_by(freezerphysname, container_type) %>%
  summarise(nn = sum(n)) %>%
  ungroup()
bad_freezer_order <- bad_containers %>%
  group_by(freezerphysname) %>%
  summarise(nnn = sum(nn)) %>%
  arrange(nnn) %>%
  pull(freezerphysname) %>%
  as.character()
bad_containers$freezerphysname <- factor(bad_containers$freezerphysname,
                                         levels = bad_freezer_order)

# Include all freezers
bad_containers_all <- bad_containers %>%
  filter(freezerphysname %in% freezer_plot_order_all)

# Include same freezers as other plots
bad_containers_same <- bad_containers %>%
  filter(freezerphysname %in% freezer_plot_order_same) %>%
  droplevels()

# Faceted pie charts
freezer_space_used %>% 
  ggplot(aes(y = used_capacity*100,
             x = factor(1),
             fill = used_capacity*100)) +
  geom_col(width = 1) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'bottom') +
  labs(title = paste0("Estimated Space Utilization: ", total_est, "%"),
       subtitle = paste("NIST Biorepository, Charleston, SC as of", Sys.Date()),
       caption = paste("Space utilization unable to be estimated for", 
                       # length(unique(bad_containers_all$container_type)),
                       length(unique(bad_containers_same$container_type)),
                       "container types.\nThis includes", 
                       # comma(sum(bad_containers_all$nn)), 
                       comma(sum(bad_containers_same$nn)), 
                       "of",
                       comma(sum(freezer_counts %>% 
                                   ungroup() %>% 
                                   # filter(freezerphysname %in% freezer_plot_order_all) %>% 
                                   filter(freezerphysname %in% freezer_plot_order_same) %>% 
                                   pull(n))),
                       "aliquots."),
       fill = "Used Capacity (%)") +
  facet_wrap(~paste0(freezerphysname, ": ", (100*round(used_capacity, 2)), "%"), 
             ncol = 7) +
  scale_fill_gradientn(colours = c("darkgreen", "blue", "orange", "darkred", "darkred"),
                       values = c(0, 0.5, 0.75, 0.9, 1)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(from = 0, to = 100, by = 10)) +
  coord_polar("y") -> vis_used_facets

# Race track chart
freezer_space_used %>% 
  mutate(freezerphysname = fct_reorder(freezerphysname, used_capacity)) %>%
  ggplot(aes(x = freezerphysname,
             y = used_capacity*100)) +
  geom_col(aes(fill = used_capacity*100)) +
  geom_label(aes(label = paste0(freezerphysname, " (", round(used_capacity*100, 0), "%)")), 
            y = 0,
            size = 3,
            label.size = NA, 
            label.padding = unit(0.1, "lines"),
            color = "black",
            fill = "white",
            alpha = 0.8,
            lwd = NA,
            hjust = 1,
            vjust = 0.3) +
  theme_classic() + 
  theme(axis.line = element_blank(), 
        legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(title = paste0("Estimated Space Utilization: ", total_est, "%"),
       subtitle = paste("NIST Biorepository, Charleston, SC as of", Sys.Date()),
       caption = paste("Mean space ratio of", round(meanCR, 2), "used for", 
                       length(unique(bad_containers$container_type)),
                       "container types.\nThis includes", 
                       comma(sum(bad_containers_same$nn)), 
                       # comma(sum(bad_containers_all$nn)),
                       "of",
                       comma(sum(freezer_counts %>% 
                                   ungroup() %>% 
                                   filter(freezerphysname %in% freezer_plot_order_same) %>% 
                                   # filter(freezerphysname %in% freezer_plot_order_all) %>% 
                                   pull(n))),
                       "aliquots."),
       fill = "Used Capacity (%)") +
  coord_polar("y", start = 0) +
  scale_y_continuous(limits = c(0, 100), labels = c(0, 25, 50, 75, 100)) + 
  scale_fill_gradientn(colours = c("darkgreen", "blue", "orange", "red", "darkred"),
                       values = c(0, 0.5, 0.75, 0.9, 1)) -> vis_used_race_track



# Plot bad containers
# bad_containers_all %>%
#   filter(freezerphysname %in% freezer_plot_order_all) %>%
bad_containers_same %>%
  filter(freezerphysname %in% freezer_plot_order_same) %>%
  ggplot(aes(x = freezerphysname,
             y = nn,
             fill = container_type)) +
  geom_col(position = "stack") +
  theme_classic() +
  labs(title = "Container Types either missing or space utilization unable to be estimated",
       # subtitle = paste("This includes", comma(sum(bad_containers_all$nn)), "aliquots."),
       subtitle = paste("This includes", comma(sum(bad_containers_same$nn)), "aliquots."),
       x = "Freezer",
       y = "Count",
       fill = "Container Type") +
  coord_flip() -> vis_bad_container_freezers

