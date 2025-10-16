# B010 analysis th 2024

# 
library( tidyverse )
library( magrittr )
library( ggrain )
library( cowplot )
library( afex )
library( flextable )
library( officer )
library( this.path )
#dataPath <- "/Users/b1082628/Tresorit/projects/B010 tactile S-R/R/R_B010_th/data/"
#projectPath <- "/Users/b1082628/Tresorit/projects/B010 tactile S-R/R/R_B010_th/"

source( "functions_common_b010.R" )
parentPath <- "/Users/jossando/trabajo/E299/05_Code/02_Analysis/R_B010_th"
dataPath <-path.join(parentPath,"data")


exp1_filename <- "B010_exp1_stimFeet_allParticipants.csv"
# exp2_filename_footStim <- "allSubjectExp3LHcross_FOOTSTIM.csv"   
# exp2_filename_handStim <- "allSubjectExp3HLcross_HANDSTIM.csv"   
exp2_filename_footStim <- "allSubjectExp3LHcross_2024-03-22_footStim.csv"
exp2_filename_handStim <- "allSubjectExp3HLcross_2024-03-22_handStim.csv"
exp3_filename_pro <- "B010_exp3_proAnti_PRO_allParticipants.csv"
exp3_filename_anti <- "B010_exp3_proAnti_ANTI_allParticipants.csv"

colorsFullDesign <- c( "#4454C4FF", "#4454C4FF", "#1FC8DEFF", "#1FC8DEFF", "#BE2102FF", "#BE2102FF", "#F1CA3AFF", "#F1CA3AFF" )
colorsInstr <- c( "#4454C4FF","#1FC8DEFF", "#BE2102FF", "#F1CA3AFF" )
colorsInstr <- c( "#f61d09", "#b11515", "#29a7ea", "#3f68b9" )
colorsDiff1 <- c( "#2D708EFF" ) # from viridis/mako( 4 )[ 3:4 ]
colorsDiff2 <- colors <- c( "#A62098FF" )

# DATA, STATISTICS AND FIGURES EXP 1
#source("Exp1_figs_stats_B010.R")

# STATISTICS AND FIGURES EXP 2
#source("Exp2_figs_stats_B010.R")

# STATISTICS AND FIGURES EXP 3
#source("Exp3_figs_stats_B010.R")
