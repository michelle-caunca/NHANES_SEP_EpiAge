setwd("/Users/bc/Documents/NHANES/caunca/")

# Run script to merge data
source("./code/NHANES-bioage-merge.R")

# load survey package (analyzes complex design surveys)
library(survey)  
options( survey.lonely.psu = "adjust" )

# tbl_regression package for presentable results
library(tidyverse)
library(ggpubr)
library(RColorBrewer)

# calling data
load("./d_ses.Rdata") #n=2532, v=105
view(names(d_ses))
					
############################
############################

# list of outcomes
epigen <- c("HorvathAge", "HannumAge", "SkinBloodAge", "PhenoAge", 
            "GrimAgeMort", "GrimAge2Mort", "HorvathTelo", "YangCell", 
            "ZhangAge", "LinAge", "VidalBraloAge", "WeidnerAge", "TELOMEAN")
# "DunedinPoAm" already represents years/chron age in years, so should not be in age-adjusted models

### Unadjusted ----
## With economic class variable
get_residuals <- function(y){
  
  formula <- as.formula(paste0(y, " ~ RIDAGEYR"))
  residuals <- lm(formula, data=d_ses, na.action=na.exclude)$residuals
  residuals
}

# running models 
all_residuals <- map(epigen, get_residuals)
all_residuals[[14]] <- d_ses$DunedinPoAm
all_residuals <- do.call(cbind, all_residuals) %>% 
  as.data.frame()
epigen_all <- c("HorvathAge", "HannumAge", "SkinBloodAge", 
                "PhenoAge", "GrimAgeMort", "GrimAge2Mort",  
                "HorvathTelo", "VidalBraloAge", "YangCell",  
                "ZhangAge", "WeidnerAge", "LinAge", "TELOMEAN", "DunedinPoAm")
old_names <- names(all_residuals)
all_residuals <- all_residuals %>% 
  rename_at(vars(old_names), ~epigen_all)
all_residuals_ses <- cbind(SEQN = d_ses$SEQN, 
                           SEP = d_ses$ses, 
                           all_residuals) %>% 
  mutate(SEP = factor(SEP,
                      levels = c("Lower",
                                 "Middle",
                                 "Upper")))

# box plots
get_plot <- function(z){
  
  p <- ggviolin(all_residuals_ses, 
                x = "SEP", y = `z`,
                color = "SEP", 
                palette =c("#1B9E77", "#D95F02", "#7570B3"),
                add = "boxplot", 
                fill = "SEP",
                shape = "SEP",
                add.params = list(color = "black",
                                  linetype = 1)) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
    
    stat_compare_means(
      comparisons = list(c("Lower", "Upper"), 
                         c("Lower", "Middle"),
                         c("Middle", "Upper")),
      label = "p.signif") + 
  
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 30, 
                                 vjust = 0.5, 
                                 hjust = 1,
                                 size = 10)
      )
}
plots <- map(epigen_all, get_plot)
plots[[1]]

# making figure 
figure <- ggarrange(plotlist = plots, 
                    ncol = 7, nrow = 2)

ggsave("figure.png",
       plot = figure,
       units = "in",
       width = 12,
       height = 6)
