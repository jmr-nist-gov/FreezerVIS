freezerCapacityFig <- freezerCapacity %>% 
#freezerCapacity %>% 
  inner_join(inventorySpaceCounts) %>% 
  mutate(UTILIZED = round(n/CAPACITY*100, 2), 
         FREEZERPHYSNAME = gsub("Freezer ", "", FREEZERPHYSNAME)) %>% 
  group_by(FREEZERPHYSNAME) %>% 
  summarise("Percent Full" = sum(UTILIZED, na.rm=TRUE)) %>% 
  filter(`Percent Full` > 0) %>% 
  rename("Freezer" = "FREEZERPHYSNAME") %>% 
  ggplot(aes(x=Freezer, 
             y=`Percent Full`, 
             fill=`Percent Full`)) + 
  geom_col() + 
  annotate(geom = "rect", 
           xmin = -1, 
           xmax = 20, 
           ymin = 80, 
           ymax = 90, 
           fill = "yellow", 
           alpha = 0.4) + 
  annotate(geom = "rect", 
           xmin = -1, 
           xmax = 20, 
           ymin = 90, 
           ymax = 100, 
           fill = "orange", 
           alpha = 0.4) + 
  annotate(geom = "rect", 
           xmin = -1, 
           xmax = 20, 
           ymin = 100, 
           ymax = 110, 
           fill = "darkred", 
           alpha = 0.4) + 
  geom_col() + 
  scale_fill_continuous(low = "lightgreen", high = "darkred") + 
  theme_bw() + 
  theme(legend.position = 'none', 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid = element_blank()) + 
  labs(x = "Freezer Name", 
       y = "Utilized (%)", 
       title = "Freezer Utilization in the Marine ESB") +
  scale_y_continuous(breaks = seq(0, 110, 10), expand = c(0, 0)) + 
  coord_cartesian(ylim = c(0, 110), xlim = c(1, 17))


relCap1830s <- freezerCapacity %>% 
  filter(grepl("183", DESCRIPTION)) %>% 
  select(-DESCRIPTION) %>% 
  distinct() %>% 
  mutate(RELCAP = max(CAPACITY)/CAPACITY)

MESB1830seriesFULL <- inventorySpaceCounts %>% 
  filter(grepl("183", DESCRIPTION)) %>% 
  inner_join(relCap1830s) %>% 
  select(-DESCRIPTION,
         -PK_FREEZERPHYSID,
         -PK_FREEZERSECTID) %>%
  mutate(n = as.integer(n),
         CAPFRAC = n/CAPACITY, 
         RELCOUNT = n * RELCAP,
         FREEZERPHYSNAME = gsub("Freezer ", "", FREEZERPHYSNAME)) %>%
  rename("Freezer" = "FREEZERPHYSNAME",
         "Container" = "CONTAINER_TYPE")

MESB1830seriesFULL$Container <- gsub(" jar", "", MESB1830seriesFULL$Container)

MESB1830seriesFULL$Container <- factor(MESB1830seriesFULL$Container,
                                       levels = MESB1830seriesFULL %>% 
                                         group_by(Container) %>% 
                                         summarise(sN = sum(n)) %>% 
                                         arrange(sN) %>% 
                                         pull(Container))


MESB1830seriesFULL %>%
  ggplot(aes(x = Container)) +
  geom_col(aes(y = RELCOUNT),
           fill = 'red') +
  geom_col(aes(y = n),
           fill = 'blue') +
  annotate('text',
           x = 4,
           y = 150000,
           label = '4,558 aliquots in 180 mL Teflon jars\nuse the space equivalent of\n157,077 2 mL or 5 mL cryovials',
           colour = 'red',
           size = 4) +
  annotate('text',
           x = 4,
           y = 130000,
           label = 'Only 16,312 cryvoials are stored\nin these freezers.',
           colour = 'blue',
           size = 4) +
  labs(y = "Count / Relative Count",
       x = element_blank(),
       title = "Relative Freezer Space Consumption by Container Type",
       subtitle = "MVE 1830 Series LN2 Vapor Freezers\nAliquot Count (blue) versus Space Relative Aliquot Count (red)") + 
  theme_classic() +
  coord_cartesian(ylim = c(0, 155000))
