# AUTHOR: Jenny Kay
# PURPOSE: Create a tile plot of MC, E2/P4 synthesis, ERagonist, and gentox results 
#           for BC-rel list paper, to visually represent dataspace covered for each chem in these assays 
# STARTED: 2021-05-31
# Last update: 2022-07-26
# written in version: R version 4.1.0 (2021-05-18)

library(tidyverse)
library(cowplot)

workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)



##### Data #####

# Read in BC-relevant list and pull out relevant info
#   chem names, MC, steroid synth summary, ER agonist, and gentox
BCrelList <- read.csv('./outputs/BCRelList.csv') 



#### Organize data for heatmap ####

BCrelList_heatmap <- BCrelList %>% 
  select(preferred_name, MC, HormoneSummary, ERactivity, Genotoxicity) %>% 
  filter(preferred_name!= "Ionizing radiation") %>% 
  mutate(MC = case_when(MC == "MC" ~ "positive",
                        MC == "Bioassay"  ~ "tested.in.bioassay",
                        TRUE ~ "no data")) %>% 
  
  mutate(HormoneSummary = case_when(HormoneSummary == "negative" | HormoneSummary == "NA" ~ "no effect", 
                                    HormoneSummary == "-" ~ "no data", 
                                    TRUE ~ "positive")) %>%
  
  mutate(ERactivity = case_when(ERactivity == "inactive" | str_detect(ERactivity, "antagonist") ~ "no effect", 
                                ERactivity == "-" ~ "no data", 
                                TRUE ~ "positive")) %>%
  
  mutate(Genotoxicity = case_when(Genotoxicity == "-" ~ "no data", 
                                  Genotoxicity == "negative" ~ "no effect", 
                                   TRUE ~ "positive")) %>% 
  # assign values to results from different assays, so that chems w/ more data show up first
  # and MCs are prioritized, followed by steroidogens, then ER agonists
  mutate(MCval = ifelse(MC == "positive", 10, 0)) %>% 
  mutate(steroidval = ifelse(HormoneSummary != "no data", 5, 0)) %>% 
  mutate(ERval = ifelse(ERactivity != "no data", 3, 0)) %>% 
  mutate(gentoxval = ifelse(Genotoxicity != "no data", 1, 0)) %>% 
  mutate(datadepth = MCval+steroidval+ERval+gentoxval) %>% 
  arrange(desc(datadepth)) %>% 
  # Make nice label names
  rename(`E2/P4_synthesis` = HormoneSummary, ER_Agonism = ERactivity, Genotoxic = Genotoxicity) %>% 
  pivot_longer(cols = MC:Genotoxic, names_to = "test", values_to = "Result")

#listlength <- nrow(BCrelList_heatmap)


##### Plot setup #####


# Manually select colors for fill in for no data, no effect, positive, and neg-bioassay results
colors <- c("#d9d9d9", "#2166ac", "#b2182b", "#9ecae1")
myscale <- scale_fill_manual(name = "Result", values = colors)


# Break full BC-relevant list down into 9 groups of 103 chems to plot per row
# Then later stack each row on top of each other into a single plot

p1 <- BCrelList_heatmap[c(1:412),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 10))

p2 <- BCrelList_heatmap[c(413:824),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 10))

p3 <- BCrelList_heatmap[c(825:1236),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 10))

p4 <- BCrelList_heatmap[c(1237:1648),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 10))

p5 <- BCrelList_heatmap[c(1649:2060),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 10))

p6 <- BCrelList_heatmap[c(2061:2472),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 10))

p7 <- BCrelList_heatmap[c(2473:2884),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 10))

p8 <- BCrelList_heatmap[c(2885:3296),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 10))

p9 <- BCrelList_heatmap[c(3297:3688),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 10))

# p10 <- BCrelList_heatmap[c(3313:3600),] %>%
#   mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "E2/P4_synthesis", "MC"))) %>%
#   ggplot(aes(x = reorder(preferred_name, desc(datadepth)), y = test, fill = Result)) +
#   geom_tile() +
#   coord_fixed(ratio = 2) +
#   labs(x = "", y = "") +
#   myscale +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
#         axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
#         text = element_text(size = 6))
# 



### Create a legend to put at the bottom
legend <- get_legend(
  p3 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", text = element_text(size = 10))
)



# Create a svg where I'll save the figure
svg(file = "./outputs/BCRel_dataspace.svg")

# Stack plots and legend
plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, legend, 
          rel_widths = c(10,10,10,10,10,10,10,10,10,.1), ncol = 1)

# export figure to file
dev.off()


