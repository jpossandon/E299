# B010 analysis th 2024
################################################################################
## EXPERIMENT 1 (FOOT STIM)
################################################################################
d1 <- read.csv( path.join( dataPath, exp1_filename ), header = TRUE, sep = ",", dec = "." )
# create a factor that contains which task it was (for consistence across all datasets)
d1$proAnti <- "pro"
d1$proAnti <- as.factor( d1$proAnti )
d1$exp <- "Exp1"
d1$stimuliAt <- "feet"
d1$responsesWith <- "hands"
# rename variables to be consistent with the labs recent variable names and across experiments
d1 <- d1 %>% rename( stimLimbPost = trial_crossed_legs )
d1 <- d1 %>% rename( respLimbPost = trial_crossed_hand ) 
d1 <- rawdata_preprocessing( d1 )
d1 <- d1 %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
# keepList <- c()
# keepList <- append( keepList, c( "1", "2", "3", "4", "5", "6", "7", "8", "9", 
#                           "10", "11", "12", "13", "14", "15", "16", "17", "18", 
#                           "19", "20", "21", "22" ) )
# dC <- keep_participants( dC, keepList )
# participants 9, 10, and 13 have very low accuracy in some conditions
# Participant 12 looks as if s/he did the same in ext/anat, with the two datapoint series
# looking identical. We could try whether this influences the results in any important way,
# but there is no an objective reason to exclude this participant.
removeList <- c()
removeList <- append( removeList, c( "9", "10", "13" ) )
d1 <- remove_participants( d1, removeList )
d <- d1
expAbbrev <- "1footS"

nrow(d)
dC <- clean_data( d, 3, 150, subjNr, instruction, stimLimbPost, respLimbPost )
nrow(dC)

dC.m <- summarize_per_participant( dC, subjNr, instruction, stimLimbPost, respLimbPost )
dC.g <- aggregate_group_raw( dC.m, instruction, stimLimbPost, respLimbPost )

# plot overall results
dC.m <- summarize_per_participant( dC, subjNr, instruction, stimLimbPost, respLimbPost ) # drop SOA for some analyses/figures
( fig.rt.group <-  plot_1Dgrid_group_with_violins( dC.m, stimLimbPost : respLimbPost, rtCorr,      instruction, stimLimbPost: respLimbPost, "stimulated (s) and responding (r) limb posture", "RT (correct trials) in ms", colorsInstr ) )
( fig.pc.group <-  plot_1Dgrid_group_with_violins( dC.m, stimLimbPost : respLimbPost, pc,         instruction, stimLimbPost: respLimbPost, "stimulated (s) and responding (r) limb posture", "accuracy (% correct)",      colorsInstr ) )
( fig.bis.group <- plot_1Dgrid_group_with_violins( dC.m, stimLimbPost : respLimbPost, bisGrouped, instruction, stimLimbPost: respLimbPost, "stimulated (s) and responding (r) limb posture", "BIS (unitless)",            colorsInstr ) )

# plot difference external minus anatomical
d.diff <- aggregate_per_participant_diff( dC.m, instruction, c( "stimLimbPost", "respLimbPost" ) )
yLowerRt <- min( d.diff$rtDiff ) - 50
yUpperRt <- max(d.diff$rtDiff ) + 50
yLowerPc <- min( d.diff$pcDiff ) - 2
yUpperPc <- max(d.diff$pcDiff ) + 2
yLowerBis <- min( d.diff$bisDiff ) - 0.5
yUpperBis <- max(d.diff$bisDiff ) + 0.5
nXaxis <- 4
( fig.rt.diff <-  plot_difference_with_violin( d.diff, stimLimbPost : respLimbPost, rtDiff,  "posture of stimulated and responding limbs", "RT diff. (ms): ext minus anatomical",          colorsDiff1, yLowerRt , yUpperRt,  nXaxis ) )
( fig.pc.diff <-  plot_difference_with_violin( d.diff, stimLimbPost : respLimbPost, pcDiff,  "posture of stimulated and responding limbs", "acc. diff. (% correct): ext minus anatomical", colorsDiff1, yLowerPc, yUpperPc,   nXaxis ) )
( fig.bis.diff <- plot_difference_with_violin( d.diff, stimLimbPost : respLimbPost, bisDiff, "posture of stimulated and responding limbs", "BIS diff. (unitless): ext minus anatomical",   colorsDiff1, yLowerBis, yUpperBis, nXaxis ) )

# plot difference between single-crossed conditions within each instruction (X-II vs. II-X)
cmp_1limb_crossing <- function( d ){
  # subtract two specific conditions, namely those where either hands or feet are crossed,
  # but not both
  d.diff <- d %>% group_by( subjNr ) %>%
    summarize(
      subjNr = first( subjNr ),
      instruction = first( instruction ),
      rtDiff = mean( rtCorr[ stimLimbPost == "sX" ] ) - mean( rtCorr[ stimLimbPost == "sII" ] ),
      pcDiff = mean( pc[ stimLimbPost == "sX" ] ) - mean( pc[ stimLimbPost == "sII" ] ),
      bisDiff = mean( bisGrouped[ stimLimbPost == "sX" ] ) - mean( bisGrouped[ stimLimbPost == "sII" ] ),
      stimLimbPost = first( stimLimbPost ),
      respLimbPost = first( respLimbPost ),
    )
}

d.sel <- dC.m %>% filter( instruction == "ext" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.ext.1 <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.sel <- dC.m %>% filter( instruction == "anat" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.anat.1 <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.contr <- bind_rows( d.diff.ext.1, d.diff.anat.1 )
yLowerRt <- min( d.contr$rtDiff ) - 50
yUpperRt <- max(d.contr$rtDiff ) + 50
yLowerPc <- min( d.contr$pcDiff ) - 2
yUpperPc <- max(d.contr$pcDiff ) + 2
yLowerBis <- min( d.contr$bisDiff ) - 0.5
yUpperBis <- max(d.contr$bisDiff ) + 0.5
nXaxis <- 2
( fig.rt.contr <- plot_difference_with_violin( d.contr, instruction, rtDiff, "instruction", "RT diff. (ms): sX-rII minus sII-rX", colorsDiff1, yLowerRt , yUpperRt, nXaxis ) )
( fig.pc.contr <- plot_difference_with_violin( d.contr, instruction, pcDiff, "instruction", "acc. diff. (% correct): sX-rII minus sII-rX", colorsDiff1, yLowerPc, yUpperPc, nXaxis ) )
( fig.bis.contr <- plot_difference_with_violin( d.contr, instruction, bisDiff, "instruction", "BIS diff. (unitless): sX-rII minus sII-rX", colorsDiff1, yLowerBis, yUpperBis, nXaxis ) )


# assembled figures Exp. 1
# REaction time
labelSize <- 18
rowHeight <- 1.2
lowerWidth <- 1.7
labelsTop <- c('b')
labelsBottom <- c('c', 'd')
figWidth = 20

topRow <- plot_grid( fig.rt.group, 
                     labels = labelsTop, label_size = labelSize, 
                     rel_widths = c( 1 ) )
bottomRow <- plot_grid( fig.rt.diff, fig.rt.contr,
                        labels = labelsBottom, label_size = labelSize,
                        ncol = 2, rel_widths = c( lowerWidth, 1 ) )
( exp1FigRt <- cowplot::plot_grid(topRow, bottomRow, 
                                  rel_heights = c( rowHeight, 1), ncol = 1) )
ggsave( paste0( parentPath,  "/outputNEW/fig_Exp", expAbbrev, "_rt.pdf" ),
        width = figWidth, height = figWidth/1.1, units = "cm" )

# Percentaje correct
topRow <- plot_grid( fig.pc.group,
                     labels = labelsTop, label_size = labelSize,
                     rel_widths = c( 1 ) )
bottomRow <- plot_grid( fig.pc.diff, fig.pc.contr,
                        labels = labelsBottom, label_size = labelSize,
                        ncol = 2, rel_widths = c( lowerWidth, 1 ) )
( exp1FigPc <- cowplot::plot_grid(topRow, bottomRow,
                                  rel_heights = c( rowHeight, 1), ncol = 1) )
ggsave( paste0( parentPath, "/outputNEW/fig_Exp", expAbbrev, "_pc.pdf" ),
        width = figWidth, height = figWidth/1.1, units = "cm" )


topRow <- plot_grid( fig.bis.group,
                     labels = labelsTop, label_size = labelSize,
                     rel_widths = c( 1 ) )
bottomRow <- plot_grid( fig.bis.diff, fig.bis.contr,
                        labels = labelsBottom, label_size = labelSize,
                        ncol = 2, rel_widths = c( lowerWidth, 1 ) )
( exp1FigBis <- cowplot::plot_grid(topRow, bottomRow,
                                   rel_heights = c( rowHeight, 1), ncol = 1) )
ggsave( paste0( parentPath,  "/outputNEW/fig_Exp", expAbbrev, "_bis.pdf" ),
        width = figWidth, height = figWidth/1.1, units = "cm" )


# ##### Exp. 1 STATS
# BIS anova
# we call anova via afex's aov_ez, as it is the simplest way to define it
dC.mi <- summarize_per_participant( dC, subjNr, instruction, stimLimbPost, respLimbPost, intensity )
print( dC.mi %>% group_by( instruction, stimLimbPost, respLimbPost, intensity ) %>%
         summarize(
           rtSd = sd( rtCorr ),
           rt = mean( rtCorr ),
           pcSd = sd( pc ),
           pc = mean( pc ),
           bisSd = sd( bisGrouped ),
           bis = mean( bisGrouped ),
         ), n = Inf )
( nice( exp1int.aov.bis <- aov_ez( id = "subjNr",
                                    dv = "bisGrouped",
                                    data = dC.mi,
                                    within = c( "instruction", "stimLimbPost", "respLimbPost", "intensity" ),
                                    type = 3 ) ) )

print( dC.m %>% group_by( instruction, stimLimbPost, respLimbPost ) %>%
         summarize(
           rtSd = sd( rtCorr ),
           rt = mean( rtCorr ),
           pcSd = sd( pc ),
           pc = mean( pc ),
           bisSd = sd( bisGrouped ),
           bis = mean( bisGrouped ),
         ), n = Inf )
( nice( exp1.aov.bis <- aov_ez( id = "subjNr",
                                    dv = "bisGrouped",
                                    data = dC.m,
                                    within = c( "instruction", "stimLimbPost", "respLimbPost" ),
                                    type = 3 ) ) )

t1 <- dC.m %>% filter( instruction == "ext"  & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t2 <- dC.m %>% filter( instruction == "ext"  & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped )
t3 <- dC.m %>% filter( instruction == "anat" & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t4 <- dC.m %>% filter( instruction == "anat" & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped ) 
tc <- data.frame( v1 = t1$bisGrouped, v2 = t2$bisGrouped, v3 = t3$bisGrouped, v4 = t4$bisGrouped )

# leg crossing ext vs. anat
crossLegs_ext_vs_anat <- t.test( tc$v2, tc$v4, paired = TRUE )

# arm crossing anat vs. ext
crossArms_ext_vs_anat <- t.test( tc$v1, tc$v3, paired = TRUE )

# external, crossed legs vs. crossed arms
ext_crossLegs_vs_crossedArms <- t.test( tc$v2, tc$v1, paired = TRUE )

# anatomical, crossed arms vs. crossed legs
anat_crossedLegs_vs_crossArms <- t.test( tc$v4, tc$v3, paired = TRUE )


# export tables for paper
# anova reported in main paper
t <- format_anova_table( exp1.aov.bis )
tt <- format_ttest_table(crossLegs_ext_vs_anat,crossArms_ext_vs_anat,ext_crossLegs_vs_crossedArms,anat_crossedLegs_vs_crossArms)
read_docx() %>% body_add_flextable( style_table( data.frame( t ) ) ) %>% body_add_par(value = "") %>% body_add_flextable( style_table( data.frame( tt ) ) ) %>%
  print( target = paste0( parentPath,  "/outputNEW/anovatable_Exp1.docx" ))

#anova incl. intensity factor reported in supplementary information
t <- format_anova_table( exp1int.aov.bis )
read_docx() %>% body_add_flextable( style_table( data.frame( t ) ) ) %>%
  print( target = paste0( parentPath,  "/outputNEW/anovatable_Exp1_withIntensity.docx" ))

