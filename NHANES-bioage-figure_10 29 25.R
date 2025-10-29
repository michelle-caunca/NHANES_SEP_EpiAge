setwd("/Users/bc/Documents/NHANES/caunca/")

# Run script to merge data
#source("./code/NHANES-bioage-merge.R")

# load survey package (analyzes complex design surveys)
library(survey)  
options( survey.lonely.psu = "adjust" )

# tbl_regression package for presentable results
library(tidyverse)
library(ggpubr)
library(RColorBrewer)

# to read xlsx files
library(openxlsx)

# to make nice figures
library(patchwork)

############################
############################

# FIGURE 1 

# calling data
load("./d_ses.Rdata") #n=2532, v=105
view(names(d_ses))

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

# FIGURE 2
epigen_strat <- c("GrimAgeMort", "GrimAge2Mort","DunedinPoAm")

# get data
ses_men <- read.xlsx("./all_ses.xlsx",
                     sheet = "m2c_men") %>%
  mutate(outcome = factor(outcome, 
                          levels = epigen_strat)) %>%
  mutate(model = "Men")
view(ses_men)

ses_women <- read.xlsx("./all_ses.xlsx",
                     sheet = "m2d_women") %>%
  mutate(outcome = factor(outcome, 
                          levels = epigen_strat)) %>%
  mutate(model = "Women")
view(ses_women)

ses_white <- read.xlsx("./all_ses.xlsx",
                       sheet = "m2e_white") %>%
  mutate(outcome = factor(outcome, 
                          levels = epigen_strat)) %>%
  mutate(model = "White")
view(ses_white)

ses_black <- read.xlsx("./all_ses.xlsx",
                       sheet = "m2f_black") %>%
  mutate(outcome = factor(outcome, 
                          levels = epigen_strat)) %>%
  mutate(model = "Black")
view(ses_black)

ses_hispanic <- read.xlsx("./all_ses.xlsx",
                       sheet = "m2g_hispanic") %>%
  mutate(outcome = factor(outcome, 
                          levels = epigen_strat)) %>%
  mutate(model = "Hispanic")
view(ses_hispanic)

ses_strat <- rbind(ses_men, 
                   ses_women,
                   ses_white,
                   ses_black,
                   ses_hispanic) %>%
  mutate(model = factor(model,
                        levels = c("Men",
                                   "Women",
                                   "White",
                                   "Black",
                                   "Hispanic")))
view(ses_strat)

# forest plot 
make_fp <- function(x) {
  
  ses_strat_fp <- ses_strat %>%
    filter(outcome == `x`) %>% 
    
    # we flip the coordinates later
    ggplot(aes(x = fct_rev(as.factor(Group)), 
               y = Estimate,
               ymin = LCL,
               ymax = UCL,
               fill = fct_rev(as.factor(model)), 
               col = fct_rev(as.factor(model)))) +
    
    # customizing the error bars
    geom_linerange(linewidth = 2,
                   position = position_dodge(width = 0.4),
                   show.legend = TRUE) +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
                       name = "Model",
                       guide = guide_legend(position = "bottom")) + 
    
    # customizing the dots
    geom_point(size = 3, 
               shape = 21, 
               colour = "black", 
               stroke = 0.5,
               position = position_dodge(width = 0.4),
               show.legend = FALSE) +
    scale_fill_manual(values = c("white","white", "white", "white", "white")) + 
    geom_hline(yintercept = 0, linetype = "dashed", col = "black") +
    
    # removing labels for the x and y axes
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "") +
    
    # removing legend for the dots 
    guides(fill = "none") + 
    
    # title
    labs(subtitle = `x`) + 
    
    coord_flip() + # flips coordinates
    #facet_grid(outcome~., scales = "free_y") +
    theme_minimal() +
    theme(text = 
            element_text(size = 15),
          axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=1),
          axis.title.x = element_text(size = 10))
  
  ses_strat_fp
  
}

fp_all <- map(epigen_strat, make_fp)
fp_all[[1]]

# creating accompanying table 
make_tbl <- function(x){
  
  gt_ses_strat <- ses_strat %>%
    filter(outcome == `x`) %>%
    select(-c("outcome", "row.names", "p-value")) %>%
    gt(rowname_col = "model",
       groupname_col = "Group") %>%
    tab_options(column_labels.hidden = FALSE)
  
  gt_ses_strat
}

tbl_all <- map(epigen_strat, make_tbl)
tbl_all[[1]]

fp_tbl <- function(x, y){
  x + wrap_table(y, space = "fixed", panel = "full") 
}

fp_tbl_all <- map2(fp_all, tbl_all, fp_tbl)

# first generation
fp_tbl_grimagemort <- fp_tbl_all[[1]]
ggsave("fp_tbl_grimagemort.jpg",
       plot = fp_tbl_grimagemort,
       dpi = 300)

fp_tbl_grimagemort2 <- fp_tbl_all[[2]]
ggsave("fp_tbl_grimagemort2.jpg",
       plot = fp_tbl_grimagemort2,
       dpi = 300)

fp_tbl_dunedinpoam <- fp_tbl_all[[3]]
ggsave("fp_tbl_dunedinpoam.jpg",
       plot = fp_tbl_dunedinpoam,
       dpi = 300)

