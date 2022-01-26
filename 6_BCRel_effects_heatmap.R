# AUTHOR: Jenny Kay
# PURPOSE: Create a tile plot of MC, E2/P4 synthesis, ERagonist, and gentox results 
#           for BC-rel list paper, to visually represent dataspace covered for each chem in these assays 
# STARTED: 2021-05-31
# written in version: R version 4.1.0 (2021-05-18)

library(tidyverse)
library(cowplot)

workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)



##### Data #####

# Read in BC-relevant list and pull out relevant info
#   chem names, MC, steroid synth summary, ER agonist, and gentox
BCrelList <- read.csv('./outputs/BCRelList.csv') 

BCrelList_heatmap <- BCrelList %>% 
  select(chem_name, MC, HormoneSummary, ERactivity, Genotoxicity) %>% 
  mutate(MC = ifelse(is.na(MC) == TRUE, "no data", "positive")) %>% 
  mutate(HormoneSummary = case_when(HormoneSummary == "negative" ~ "no effect", 
                                    HormoneSummary == "-" ~ "no data", 
                                    TRUE ~ "positive")) %>%
  mutate(ERactivity = case_when(ERactivity == "not_agonist" ~ "no effect", 
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
  #subset(select = -c(MCval:datadepth)) %>% 
  # Make nice label names
  rename(Hormone_Synthesis = HormoneSummary, ER_Agonism = ERactivity, Genotoxic = Genotoxicity) %>% 
  pivot_longer(cols = MC:Genotoxic, names_to = "test", values_to = "Result")




##### Plot setup #####


# Manually select colors for fill in for no data, no effect, and positive results
colors <- c("grey70", "royalblue3", "red3")
myscale <- scale_fill_manual(name = "Result", values = colors)


# Break full BC-relevant list down into groups of 100 chems to plot per row
# Then later we'll stack each row on top of each other into a single plot

p1 <- BCrelList_heatmap[c(1:400),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))

p2 <- BCrelList_heatmap[c(401:800),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))

p3 <- BCrelList_heatmap[c(801:1200),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))

p4 <- BCrelList_heatmap[c(1201:1600),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))

p5 <- BCrelList_heatmap[c(1601:2000),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))

p6 <- BCrelList_heatmap[c(2001:2400),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))

p7 <- BCrelList_heatmap[c(2401:2800),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))

p8 <- BCrelList_heatmap[c(2801:3200),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))

p9 <- BCrelList_heatmap[c(3201:3600),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))

p10 <- BCrelList_heatmap[c(3601:3780),] %>%
  mutate(test = factor(test, levels = c("Genotoxic", "ER_Agonism", "Hormone_Synthesis", "MC"))) %>%
  ggplot(aes(x = reorder(chem_name, desc(datadepth)), y = test, fill = Result)) +
  geom_tile() +
  coord_fixed(ratio = 2) +
  labs(x = "", y = "") +
  myscale +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_line(size = 0.1), 
        axis.ticks.y = element_line(size = 0.2), legend.position = "none", 
        text = element_text(size = 6))



### Create a legend to put at the bottom
legend <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", text = element_text(size = 7))
)



# Create a pdf where I'll save the figure
pdf(file = "./outputs/BCRel_dataspace.pdf")

# Stack plots and legend
plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, legend, 
          rel_widths = c(10,10,10,10,10,10,10,10,10,4.5,.1), ncol = 1)

# export figure to file
dev.off()




