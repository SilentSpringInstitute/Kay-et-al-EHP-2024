# AUTHOR: Jenny Kay
# PURPOSE: Figures and supplemental tables for BCRC paper
# STARTED: 2023-04-06
# last update: 2023-07-24
# written in version: R version 4.2.2 (2022-10-31 ucrt)


library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(openxlsx)


#### write supplemental tables into a single .xlsx file ####
# Output then gets further modified by creating tabs for an overview of the tables,
#    references used in the supplemental tables, data dictionary, 
#    and Table S2 is the overview of MCs with equivocal/dismissed evidence

BCrelList <- read.csv("outputs/BCrelList.csv")
EDC_gentox <- read.csv("outputs/EDC_gentox.csv")
MGdev <- read.csv("outputs/BCrel_MGdev_comparison.csv")
MC_and_Bioassay_effects <- read.csv("./outputs/MC_and_Bioassay_effects.csv")

listoftables <- list("Excel Table S1" = BCrelList, "Excel Table S3" = EDC_gentox, 
                     "Excel Table S4" = MGdev, "Excel Table S5" = MC_and_Bioassay_effects)
write.xlsx(listoftables, file = "outputs/combosupp.xlsx")


#### Figure 2: stacked bar chart of EDC strength for non-MCs and MCs #### 
MCvsNon_EDCmags <- filter(MC_and_Bioassay_effects, topEDCscore != "-") %>% 
  count(MammaryTumorEvidence, topEDCscore) %>% 
  mutate(`Top EDC score` = factor(topEDCscore, levels = c("none", "borderline", "low", "medium", "high"))) %>% 
  mutate(MammaryTumorEvidence = case_when(MammaryTumorEvidence == "Bioassay_noMC" ~ "Non-MCs",
                                          MammaryTumorEvidence == "MC" ~ "MCs",
                                          TRUE ~ MammaryTumorEvidence))

# color scale for edc strength
EDCcolors <- c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d")
EDCscale <- scale_fill_manual(name = "Top EDC score", values = EDCcolors)

# Make chart
fig2 <-
ggplot(MCvsNon_EDCmags, aes(x = reorder(MammaryTumorEvidence, desc(MammaryTumorEvidence)), y = n, 
                        fill = `Top EDC score`)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "", y = "Proportion") +
  EDCscale +
  coord_fixed(ratio = 3) +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 'solid', colour = "black", linewidth = 0.3),
        axis.ticks.y = element_line(linewidth = 0.3),
        axis.title.y = element_text(vjust = 2),
        legend.title = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10))

ggsave(filename = "./outputs/Fig2.tiff", plot = fig2, width = 3, height = 2.5, device = "tiff", dpi = 700)


#### Figure 3: Mosaic plots for EDCstrength + gentox ####

#set up dataframe to have desired output formats
MCvsNon_EDCgentoxmosaic <- filter(MC_and_Bioassay_effects, topEDCscore != "-" & Genotoxicity != "-") %>% 
  select(MammaryTumorEvidence, topEDCscore, Genotoxicity) %>% 
  mutate(MammaryTumorEvidence = case_when(MammaryTumorEvidence == "Bioassay_noMC" ~ "Non-MCs",
                                          MammaryTumorEvidence == "MC" ~ "MCs",TRUE ~ MammaryTumorEvidence)) %>% 
  mutate(`Top EDC score` = factor(topEDCscore, levels = c("none", "borderline", "low", "medium", "high"))) %>% 
  mutate(Genotoxicity = ifelse(Genotoxicity == "positive", "positive", "negative"))

# set same color scale for EDC strength
mosaiccolors <- rev(c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d"))
mosaicscale <- scale_fill_manual(name = "topEDCscore", values = mosaiccolors, guide = guide_legend(reverse = TRUE))

# Make figure
fig3 <-
ggplot(filter(MCvsNon_EDCgentoxmosaic) %>% 
         mutate(MammaryTumorEvidence = factor(MammaryTumorEvidence, levels = c("Non-MCs", "MCs"))) %>% 
         mutate(topEDCscore = factor(topEDCscore, levels = rev(c("none", "borderline", "low", "medium", "high")))) %>% 
         mutate(Genotoxicity = factor(Genotoxicity, levels = c("positive", "negative")))) +
  geom_mosaic(aes(x = product(Genotoxicity, topEDCscore), fill = topEDCscore),
              divider=c("hspine", "vspine"), offset = 0.02) +
  labs(y = "Top EDC Score", x = "Genotoxicity") +
  facet_grid(~MammaryTumorEvidence) +
  mosaicscale +
  coord_fixed(ratio = 1) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        axis.title.x = element_text(vjust = 0),
        axis.title.y = element_text(vjust = 2),
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 9))


ggsave(filename = "./outputs/Fig3.tiff", plot = fig3, width = 5, height = 4, device = "tiff", dpi = 700)


