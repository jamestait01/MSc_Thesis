data <- readRDS("/rds/general/user/jt1019/home/Summer_project/baseline_models/model_3a/final_data.rds")

sankey_data <- subset(data,select=c(eid,`Medication combination`,`Last medication combination`))

# Using pivot_longer to reshape the dataframe
#sankey_data <- sankey_data %>%
  #pivot_longer(cols = c("Medication combination", "Last medication combination"),
               #names_to = "Time period",
               #values_to = "Medication combination")


library(ggplot2)
library(ggalluvial)

library(dplyr)

#change "Calcium channel blockers" to CCB otherwise labels don't fit
sankey_data$`Medication combination` <- gsub("Calcium channel blockers", "CCB",
                                             as.character(sankey_data$`Medication combination`))

sankey_data$`Last medication combination` <- gsub("Calcium channel blockers", "CCB",
                                             as.character(sankey_data$`Last medication combination`))

sankey_data <- sankey_data %>%
rename(Baseline = `Medication combination`,
       `End of study` = `Last medication combination`)

counts <- sankey_data %>%
  count(`Baseline`,`End of study`) %>%
  arrange(desc(n))

# Create the Sankey plot:
skeypl2 <- ggplot(data = counts,
                  aes(axis1 = Baseline,   # First variable on the X-axis
                      axis2 = `End of study`, # Second variable on the X-axis
                      y = n)) +
  geom_alluvium(aes(fill = Baseline)) +
  geom_stratum() +
  scale_x_discrete(limits = c("Baseline", "End of study"), expand = c(.05, .05)) + 
  scale_linetype_manual(values = c("blank", "solid")) +
  ggrepel::geom_text_repel(
    aes(label = Baseline),
    stat = "stratum", size = 3, direction = "y", nudge_x = -2
  ) +
  ggrepel::geom_text_repel(
    aes(label = `End of study`),
    stat = "stratum", size = 3, direction = "y", nudge_x = 2,
  ) + scale_fill_viridis_d() +
  theme(panel.background = element_blank(),#remove grid and colour from background
        axis.text.y = element_blank(),#remove y axis tick labels,
        axis.ticks.y =element_blank(),#remove tick from y axis 
        legend.text = element_text(size=5)) 

ggsave("sankey_plot.png", plot = skeypl2, width = 10, height = 6)


plain_sankey <-ggplot(data = counts,
                    aes(axis1 = Baseline,   # First variable on the X-axis
                        axis2 = `End of study`, # Second variable on the X-axis
                        y = n)) +
  geom_alluvium(aes(fill = Baseline)) +
  geom_stratum() +
  scale_x_discrete(limits = c("Baseline", "End of study"), expand = c(.05, .05)) +
  theme(panel.background = element_blank(),#remove grid and colour from background
        axis.text.y = element_blank(),#remove y axis tick labels,
        axis.ticks.y = element_blank())#remove tick from y axis 

plain_sankey
  
ggsave("plain_sankey_plot.png", plot = plain_sankey, width = 10, height = 6)
