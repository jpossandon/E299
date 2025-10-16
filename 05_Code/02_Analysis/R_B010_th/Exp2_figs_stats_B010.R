# B010 analysis th 2024

################################################################################
## EXPERIMENT 2 FOOT STIM
################################################################################
d2.footStim <- read.csv( path.join( dataPath,exp2_filename_footStim ), header = TRUE, sep = ",", dec = "." )
d2.footStim$proAnti <- "pro"
d2.footStim$exp <- "Exp2"
d2.footStim$stimuliAt <- "feet"
d2.footStim$responsesWith <- "hands"
# rename variables to be consistent with the labs recent variable names and across experiments
d2.footStim <- d2.footStim %>% rename( stimLimbPost = trial_crossed_legs )
d2.footStim <- d2.footStim %>% rename( respLimbPost = trial_crossed_hand ) 
d2.footStim <- rawdata_preprocessing( d2.footStim )
d2.footStim <- d2.footStim %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
keepList <- c()
keepList <- append( keepList, c( "1", "45", "71", "72", "73", "74", "75", "78", "79", 
                                 "80", "81", "82", "83", "84", "85", "87", "88", 
                                 "89", "90", "91", "92", "93", "94" ) )
d2.footStim <- keep_participants( d2.footStim, keepList )
removeList <- c()
removeList <- append( removeList, c( "10", "45", "94" ) )
# removeList <- append( removeList, c( "10", "45", "75", "79", "80", "84", "94" ) )
d2.footStim <- remove_participants( d2.footStim, removeList )
d <- d2.footStim
expAbbrev <- "2footS"

################################################################################
## EXPERIMENT 2 HAND STIM
################################################################################
d2.handStim <- read.csv( path.join( dataPath,exp2_filename_handStim ), header = TRUE, sep = ",", dec = "." )
# create a factor that contains which task it was (for consistence across all datasets)
d2.handStim$proAnti <- "pro"
d2.handStim$exp <- "Exp2"
d2.handStim$stimuliAt <- "hands"
d2.handStim$responsesWith <- "feet"
# rename variables to be consistent with the labs recent variable names and across experiments
d2.handStim <- d2.handStim %>% rename( stimLimbPost = trial_crossed_hand )
d2.handStim <- d2.handStim %>% rename( respLimbPost = trial_crossed_legs ) 
d2.handStim <- rawdata_preprocessing( d2.handStim )
d2.handStim <- d2.handStim %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
keepList <- c()
keepList <- append( keepList, c( "1", "45", "71", "72", "73", "74", "75", "78", "79", 
                                 "80", "81", "82", "83", "84", "85", "87", "88", 
                                 "89", "90", "91", "92", "93", "94" ) )
d2.handStim <- keep_participants( d2.handStim, keepList )
removeList <- c()
removeList <- append( removeList, c( "10", "45", "94" ) )
# removeList <- append( removeList, c( "10", "45", "75", "79", "80", "84", "94" ) )
d2.handStim <- remove_participants( d2.handStim, removeList )
d <- d2.handStim
expAbbrev <- "2handS"

dC.2f <- clean_data( d2.footStim, 3, 150, subjNr, instruction, stimLimbPost, respLimbPost )
nrow(dC.2f)
dC.2h <- clean_data( d2.handStim, 3, 150, subjNr, instruction, stimLimbPost, respLimbPost )
nrow(dC.2h)
dC <- bind_rows( dC.2f, dC.2h )
#fct_relevel(dC$proAnti, "pro", "anti")
dC$stimuliAt <- fct_relevel(dC$stimuliAt, "feet", "hands")
dC$stimuliAt <- fct_recode(dC$stimuliAt, "stim: feet" = "feet", "stim: hands" = "hands")

print( paste( "Number of participants included:", length( unique( dC$subjNr ) ) ) )

nrow(dC)
expAbbrev <- "2hf"

# Frage: erst alle BIS und dann teilen, oder BIS geteilt berechnen?
dC.m <- summarize_per_participant( dC, subjNr, stimuliAt, instruction, stimLimbPost, respLimbPost )
dC.g <- aggregate_group_raw( dC.m, stimuliAt, instruction, stimLimbPost, respLimbPost )

# plot overall results
# dC.m.2f <- dC.2f %>% summarize_per_participant( subjNr, stimuliAt, instruction, stimLimbPost, respLimbPost )
# dC.m.2f <- dC.m %>% filter( stimuliAt == "feet" )
# dC.m.2h <- dC.m %>% filter( stimuliAt == "hands" )

( fig.rt.hf <-  plot_2Dgrid_group_with_violins( dC.m, stimLimbPost : respLimbPost, rtCorr,     instruction, stimuliAt, stimLimbPost: respLimbPost, "stimulated (s) and responding (r) limb posture", "RT (correct trials) in ms", colorsInstr ) )
( fig.pc.hf <-  plot_2Dgrid_group_with_violins( dC.m, stimLimbPost : respLimbPost, pc,         instruction, stimuliAt, stimLimbPost: respLimbPost, "stimulated (s) and responding (r) limb posture", "accuracy (% correct)",      colorsInstr ) )
( fig.bis.hf <- plot_2Dgrid_group_with_violins( dC.m, stimLimbPost : respLimbPost, bisGrouped, instruction, stimuliAt, stimLimbPost: respLimbPost, "stimulated (s) and responding (r) limb posture", "BIS (unitless)",            colorsInstr ) )


# plot difference external minus anatomical
d.diff <- aggregate_per_participant_diff( dC.m, instruction, c( "stimuliAt", "stimLimbPost", "respLimbPost" ) )
yLowerRt <- min( d.diff$rtDiff ) - 50
yUpperRt <- max(d.diff$rtDiff ) + 50
yLowerPc <- min( d.diff$pcDiff ) - 2
yUpperPc <- max(d.diff$pcDiff ) + 2
yLowerBis <- min( d.diff$bisDiff ) - 0.5
yUpperBis <- max(d.diff$bisDiff ) + 0.5
( fig.rt.diff <-  plot_1Dgrid_difference_with_violin( d.diff, stimLimbPost : respLimbPost, rtDiff,  stimuliAt,  "posture of stimulated and responding limbs", "RT diff. (ms): ext minus anat",          colorsDiff1, yLowerRt,  yUpperRt  ) )
( fig.pc.diff <-  plot_1Dgrid_difference_with_violin( d.diff, stimLimbPost : respLimbPost, pcDiff,  stimuliAt,  "posture of stimulated and responding limbs", "acc. diff. (% correct): ext minus anat", colorsDiff1, yLowerPc,  yUpperPc  ) )
( fig.bis.diff <- plot_1Dgrid_difference_with_violin( d.diff, stimLimbPost : respLimbPost, bisDiff, stimuliAt, "posture of stimulated and responding limbs", "BIS diff. (unitless): ext minus anat",    colorsDiff1, yLowerBis, yUpperBis ) )

# plot difference between single-crossed conditions within each instruction (X-II vs. II-X)

cmp_1limb_crossing <- function( d ){
  # subtract two specific conditions, namely those where either hands or feet are crossed,
  # but not both
  d.diff <- d %>% group_by( subjNr ) %>%
    summarize(
      subjNr = first( subjNr ),
      stimuliAt = first( stimuliAt ),
      instruction = first( instruction ),
      rtDiff = mean( rtCorr[ stimLimbPost == "sX" ] ) - mean( rtCorr[ stimLimbPost == "sII" ] ),
      pcDiff = mean( pc[ stimLimbPost == "sX" ] ) - mean( pc[ stimLimbPost == "sII" ] ),
      bisDiff = mean( bisGrouped[ stimLimbPost == "sX" ] ) - mean( bisGrouped[ stimLimbPost == "sII" ] ),
      stimLimbPost = first( stimLimbPost ),
      respLimbPost = first( respLimbPost ),
    )
}

d.sel <- dC.m %>% filter( stimuliAt == "stim: feet", instruction == "ext" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.ext.2f <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, stimuliAt, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.sel <- dC.m %>% filter( stimuliAt == "stim: feet", instruction == "anat" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.anat.2f <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, stimuliAt, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.sel <- dC.m %>% filter( stimuliAt == "stim: hands", instruction == "ext" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.ext.2h <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, stimuliAt, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.sel <- dC.m %>% filter( stimuliAt == "stim: hands", instruction == "anat" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.anat.2h <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, stimuliAt, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.contr <- bind_rows( d.diff.ext.2f, d.diff.anat.2f, d.diff.ext.2h, d.diff.anat.2h )
yLowerRt <- min( d.contr$rtDiff ) - 50
yUpperRt <- max(d.contr$rtDiff ) + 50
yLowerPc <- min( d.contr$pcDiff ) - 2
yUpperPc <- max(d.contr$pcDiff ) + 2
yLowerBis <- min( d.contr$bisDiff ) - 0.5
yUpperBis <- max(d.contr$bisDiff ) + 0.5
( fig.rt.contr <-  plot_1Dgrid_difference_with_violin( d.contr, instruction, rtDiff,  stimuliAt, "instruction", "RT diff. (ms): sX-rII minus sII-rX",          colorsDiff1, yLowerRt,  yUpperRt  ) )
( fig.pc.contr <-  plot_1Dgrid_difference_with_violin( d.contr, instruction, pcDiff,  stimuliAt, "instruction", "acc. diff. (% correct): sX-rII minus sII-rX", colorsDiff1, yLowerPc,  yUpperPc  ) )
( fig.bis.contr <- plot_1Dgrid_difference_with_violin( d.contr, instruction, bisDiff, stimuliAt, "instruction", "BIS diff. (unitless): sX-rII minus sII-rX",   colorsDiff1, yLowerBis, yUpperBis ) )


labelSize <- 18
labelsTop <- c('a')
labelsBottom <- c('b', 'c')
rowHeight <- 1.5
lowerWidth <- 1.7
figWidth = 20

topRow <- plot_grid( fig.rt.hf,
                     labels = labelsTop, label_size = labelSize,
                     rel_widths = c( 1 ) )
bottomRow <- plot_grid( fig.rt.diff, fig.rt.contr,
                        labels = labelsBottom, label_size = labelSize,
                        ncol = 2, rel_widths = c( lowerWidth, 1 ) )
( exp1FigRt <- cowplot::plot_grid(topRow, bottomRow,
                                  rel_heights = c( rowHeight, 1), ncol = 1) )
ggsave( paste0( parentPath,  "/outputNEW/fig_Exp", expAbbrev, "_rt.pdf" ),
        width = figWidth, height = figWidth/1.1, units = "cm" )

topRow <- plot_grid( fig.pc.hf,
                     labels = labelsTop, label_size = labelSize,
                     rel_widths = c( 1 ) )
bottomRow <- plot_grid( fig.pc.diff, fig.pc.contr,
                        labels = labelsBottom, label_size = labelSize,
                        ncol = 2, rel_widths = c( lowerWidth, 1 ) )
( exp1FigPc <- cowplot::plot_grid(topRow, bottomRow,
                                  rel_heights = c( rowHeight, 1), ncol = 1) )
ggsave( paste0( parentPath,  "/outputNEW/fig_Exp", expAbbrev, "_pc.pdf" ),
        width = figWidth, height = figWidth/1.1, units = "cm" )

topRow <- plot_grid( fig.bis.hf,
                     labels = labelsTop, label_size = labelSize,
                     rel_widths = c( 1 )  )
bottomRow <- plot_grid( fig.bis.diff, fig.bis.contr,
                        labels = labelsBottom, label_size = labelSize,
                        ncol = 2, rel_widths = c( lowerWidth, 1 ) )
( exp1FigBis <- cowplot::plot_grid(topRow, bottomRow,
                                   rel_heights = c( rowHeight, 1), ncol = 1) )
ggsave( paste0( parentPath,  "/outputNEW/fig_Exp", expAbbrev, "_bis.pdf" ),
        width = figWidth, height = figWidth/1.1, units = "cm" )


# ##### Exp. 2 STATS

# BIS anova both limbs
print( dC.m %>% group_by( stimuliAt, instruction, stimLimbPost, respLimbPost ) %>%
         summarize(
           rtSd = sd( rtCorr ),
           rt = mean( rtCorr ),
           pcSd = sd( pc ),
           pc = mean( pc ),
           bisSd = sd( bisGrouped ),
           bis = mean( bisGrouped ),
         ), n = Inf )
( nice( exp2full.aov.bis <- aov_ez( id = "subjNr",
                                    dv = "bisGrouped",
                                    data = dC.m,
                                    within = c( "stimuliAt", "instruction", "stimLimbPost", "respLimbPost" ),
                                    type = 3 ) ) )

# BIS anova foot stimulation 
dC.m.2f <- dC.m %>% filter( stimuliAt == "stim: feet" )
print( dC.m.2f %>% group_by( instruction, stimLimbPost, respLimbPost ) %>%
         summarize(
           rtSd = sd( rtCorr ),
           rt = mean( rtCorr ),
           pcSd = sd( pc ),
           pc = mean( pc ),
           bisSd = sd( bisGrouped ),
           bis = mean( bisGrouped ),
         ), n = Inf )
( nice( exp2f.aov.bis <- aov_ez( id = "subjNr",
                                 dv = "bisGrouped",
                                 data = dC.m.2f,
                                 within = c( "instruction", "stimLimbPost", "respLimbPost" ),
                                 type = 3 ) ) )

# BIS anova hand stimulation
dC.m.2h <- dC.m %>% filter( stimuliAt == "stim: hands" )
print( dC.m.2h %>% group_by( instruction, stimLimbPost, respLimbPost ) %>%
         summarize(
           rtSd = sd( rtCorr ),
           rt = mean( rtCorr ),
           pcSd = sd( pc ),
           pc = mean( pc ),
           bisSd = sd( bisGrouped ),
           bis = mean( bisGrouped ),
         ), n = Inf )
( nice( exp2h.aov.bis <- aov_ez( id = "subjNr",
                                    dv = "bisGrouped",
                                    data = dC.m.2h,
                                    within = c( "instruction", "stimLimbPost", "respLimbPost" ),
                                    type = 3 ) ) )

t1 <- dC.m.2f %>% filter( instruction == "ext"  & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t2 <- dC.m.2f %>% filter( instruction == "ext"  & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped )
t3 <- dC.m.2f %>% filter( instruction == "anat" & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t4 <- dC.m.2f %>% filter( instruction == "anat" & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped ) 

t5 <- dC.m.2h %>% filter( instruction == "ext"  & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t6 <- dC.m.2h %>% filter( instruction == "ext"  & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped )
t7 <- dC.m.2h %>% filter( instruction == "anat" & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t8 <- dC.m.2h %>% filter( instruction == "anat" & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped ) 

tc <- data.frame( v1 = t1$bisGrouped, v2 = t2$bisGrouped, v3 = t3$bisGrouped, v4 = t4$bisGrouped,
                  v5 = t5$bisGrouped, v6 = t6$bisGrouped, v7 = t7$bisGrouped, v8 = t8$bisGrouped)
tc$v9 = tc$v2- tc$v4 #stimulated feet , ext vs anantomical, 
tc$v10 = tc$v6- tc$v8 #stimulated hand , ext vs anantomical, 
tc$v11 = tc$v1- tc$v3 #response hand ,, ext vs anantomical, 
tc$v12 = tc$v5- tc$v7 #response feet , ext vs anantomical, 
tc$v13 = tc$v2- tc$v1 #stimulated feet , ext instrucntion, crossed feet vs crossed hands
tc$v14 = tc$v6- tc$v5 #stimulated hand , ext instrucntion, crossed hands vs crossed feet
tc$v15 = tc$v4- tc$v3 #stimulated feet , anat instrucntion, crossed feet vs crossed hands
tc$v16 = tc$v8- tc$v7 #stimulated hand , anat instrucntion, crossed hands vs crossed feet


# feet stimulated
# leg crossing ext vs. anat
FcrossLegs_ext_vs_anat <- t.test( tc$v2, tc$v4, paired = TRUE )

# arm crossing anat vs. ext
FcrossArms_ext_vs_anat <- t.test( tc$v1, tc$v3, paired = TRUE )

# external, crossed legs vs. crossed arms
Fext_crossLegs_vs_crossedArms <- t.test( tc$v2, tc$v1, paired = TRUE )

# anatomical, crossed legs vs. crossed arms
Fanat_crossedLegs_vs_crossArms <- t.test( tc$v4, tc$v3, paired = TRUE )

# hand stimulated
# arm crossing ext vs. anat
HcrossArms_ext_vs_anat <- t.test( tc$v6, tc$v8, paired = TRUE )

# leg crossing anat vs. ext
HcrossLegs_ext_vs_anat <- t.test( tc$v5, tc$v7, paired = TRUE )

# external, crossed arms vs. crossed legs
Hext_crossArms_vs_crossedLegs <- t.test( tc$v6, tc$v5, paired = TRUE )

# anatomical, crossed legs vs. crossed arms
Hanat_crossedArms_vs_crossLegs <- t.test( tc$v8, tc$v7, paired = TRUE )

# Differences between subexperiments
# stimulated feet vs stimulated hand: stimulated limb corssing ext vs anat 
crossStim_ext_vs_anat_stimF_vs_stimH <- t.test( tc$v9, tc$v10, paired = TRUE )

# response hand vs response feet: responselimb corssing anat vs external
crossResp_ext_vs_anat_stimF_vs_stimH <- t.test( tc$v11, tc$v12, paired = TRUE )

# external response hand vs response feet: stimulated vs response limb crossing
ext_crossS_vs_crossR_stimF_vs_stimH <- t.test( tc$v13, tc$v14, paired = TRUE )

# external response hand vs response feet: stimulated vs response limb crossing
anat_crossS_vs_crossR_stimF_vs_stimH <- t.test( tc$v15, tc$v16, paired = TRUE )


# export tables for paper
# anova reported in main paper
t <- format_anova_table( exp2f.aov.bis )
tt <- format_ttest_table(FcrossLegs_ext_vs_anat,FcrossArms_ext_vs_anat,Fext_crossLegs_vs_crossedArms,Fanat_crossedLegs_vs_crossArms)
read_docx() %>% body_add_flextable( style_table( data.frame( t ) ) ) %>% body_add_par(value = "") %>% body_add_flextable( style_table( data.frame( tt ) ) ) %>%
  print( target = paste0( parentPath,  "/outputNEW/anovatable_Exp2f.docx" ))

t <- format_anova_table( exp2h.aov.bis )
tt <- format_ttest_table(HcrossArms_ext_vs_anat,HcrossLegs_ext_vs_anat,Hext_crossArms_vs_crossedLegs ,Hanat_crossedArms_vs_crossLegs)
read_docx() %>% body_add_flextable( style_table( data.frame( t ) ) ) %>% body_add_par(value = "") %>% body_add_flextable( style_table( data.frame( tt ) ) ) %>%
  print( target = paste0( parentPath,  "/outputNEW/anovatable_Exp2h.docx" ))


# Anova with both stimulation
 t <- format_anova_table( exp2full.aov.bis )
 tt <- format_ttest_table(crossStim_ext_vs_anat_stimF_vs_stimH,crossResp_ext_vs_anat_stimF_vs_stimH,ext_crossS_vs_crossR_stimF_vs_stimH, anat_crossS_vs_crossR_stimF_vs_stimH)
 
 read_docx() %>% body_add_flextable( style_table( data.frame( t ) ) ) %>% body_add_par(value = "") %>% body_add_flextable( style_table( data.frame( tt ) ) ) %>%
   print( target = paste0( parentPath,  "/outputNEW/anovatable_Exp2_full.docx" ))



