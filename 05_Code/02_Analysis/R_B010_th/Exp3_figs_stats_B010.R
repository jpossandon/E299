# B010 analysis th 2024

################################################################################
## EXPERIMENT 3 PRO (FOOT STIM)
################################################################################
d3.pro <- read.csv( path.join( dataPath, exp3_filename_pro ), header = TRUE, sep = ",", dec = "." )
d3.pro$proAnti <- "pro"
d3.pro$proAnti <- as.factor( d3.pro$proAnti )
d3.pro$exp <- "Exp3"
d3.pro$stimuliAt <- "stim: feet"
d3.pro$responsesWith <- "resp: hands"
d3.pro <- d3.pro %>% rename( stimLimbPost = trial_crossed_legs )
d3.pro <- d3.pro %>% rename( respLimbPost = trial_crossed_hand ) 
d3.pro <- rawdata_preprocessing( d3.pro )
d3.pro <- d3.pro %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
removeList <- c()
removeList <- append( removeList, c( "32", "33", "49", "52" ) )
d3.pro <- remove_participants( d3.pro, removeList )
d <- d3.pro
expAbbrev <- "3pro"
# subjects: 31, 32, 33, 36, 37, 38, 39, 40, 41, 42, 43, 44
# 45, 47, 48, 49, 51, 52, 53, 54, 55, 59, 60, 61, 62, 63

################################################################################
## EXPERIMENT 3 ANTI (FOOT STIM)
################################################################################
d3.anti <- read.csv( path.join( dataPath, exp3_filename_anti ), header = TRUE, sep = ",", dec = "." )
d3.anti$proAnti <- "anti"
d3.anti$proAnti <- as.factor( d3.anti$proAnti )
d3.anti$exp <- "Exp3"
d3.anti$stimuliAt <- "feet"
d3.anti$responsesWith <- "hands"
d3.anti <- d3.anti %>% rename( stimLimbPost = trial_crossed_legs )
d3.anti <- d3.anti %>% rename( respLimbPost = trial_crossed_hand ) 
d3.anti <- rawdata_preprocessing( d3.anti )
d3.anti <- d3.anti %>% select( exp, proAnti, stimuliAt, responsesWith, subjNr, instruction, stimLimbPost, respLimbPost, intensity, rt, respCorr, respButton, stimLimb )
removeList <- c()
removeList <- append( removeList, c( "32", "33", "49", "52" ) )
d3.anti <- remove_participants( d3.anti, removeList )
d <- d3.anti
expAbbrev <- "3anti"

dC.3p <- clean_data( d3.pro, 3, 150, subjNr, instruction, stimLimbPost, respLimbPost )
nrow(dC.3p)
dC.3a <- clean_data( d3.anti, 3, 150, subjNr, instruction, stimLimbPost, respLimbPost )
nrow(dC.3a)
dC <- bind_rows( dC.3p, dC.3a )
dC$proAnti <- fct_relevel(dC$proAnti, "pro", "anti")
dC$proAnti <- fct_recode(dC$proAnti, "task: pro" = "pro", "task: anti" = "anti")

nrow(dC)
expAbbrev <- "3pa"

dC.m <- summarize_per_participant( dC, subjNr, proAnti, instruction, stimLimbPost, respLimbPost )
dC.g <- aggregate_group_raw( dC.m, proAnti, instruction, stimLimbPost, respLimbPost )

# plot overall results
( fig.rt.pa <-  plot_2Dgrid_group_with_violins( dC.m, stimLimbPost : respLimbPost, rtCorr,     instruction, proAnti, stimLimbPost: respLimbPost, "stimulated (s) and responding (r) limb posture", "RT (correct trials) in ms", colorsInstr ) )
( fig.pc.pa <-  plot_2Dgrid_group_with_violins( dC.m, stimLimbPost : respLimbPost, pc,         instruction, proAnti, stimLimbPost: respLimbPost, "stimulated (s) and responding (r) limb posture", "accuracy (% correct)",      colorsInstr ) )
( fig.bis.pa <- plot_2Dgrid_group_with_violins( dC.m, stimLimbPost : respLimbPost, bisGrouped, instruction, proAnti, stimLimbPost: respLimbPost, "stimulated (s) and responding (r) limb posture", "BIS (unitless)",            colorsInstr ) )

# plot difference external minus anatomical
d.diff <- aggregate_per_participant_diff( dC.m, instruction, c( "proAnti", "stimLimbPost", "respLimbPost" ) )
yLowerRt <- min( d.diff$rtDiff ) - 50
yUpperRt <- max(d.diff$rtDiff ) + 50
yLowerPc <- min( d.diff$pcDiff ) - 2
yUpperPc <- max(d.diff$pcDiff ) + 2
yLowerBis <- min( d.diff$bisDiff ) - 0.5
yUpperBis <- max(d.diff$bisDiff ) + 0.5
( fig.rt.diff <-  plot_1Dgrid_difference_with_violin( d.diff, stimLimbPost : respLimbPost, rtDiff,  proAnti,  "posture of stimulated and responding limbs", "RT diff. (ms): ext minus anatomical",         colorsDiff1, yLowerRt,  yUpperRt  ) )
( fig.pc.diff <-  plot_1Dgrid_difference_with_violin( d.diff, stimLimbPost : respLimbPost, pcDiff,  proAnti, "posture of stimulated and responding limbs", "acc. diff. (% correct): ext minus anatomical", colorsDiff1, yLowerPc,  yUpperPc  ) )
( fig.bis.diff <- plot_1Dgrid_difference_with_violin( d.diff, stimLimbPost : respLimbPost, bisDiff, proAnti, "posture of stimulated and responding limbs", "BIS diff. (unitless): ext minus anatomical",   colorsDiff1, yLowerBis, yUpperBis ) )

# plot difference between single-crossed conditions within each instruction (X-II vs. II-X)

cmp_1limb_crossing <- function( d ){
  # subtract two specific conditions, namely those where either hands or feet are crossed,
  # but not both
  d.diff <- d %>% group_by( subjNr ) %>%
    summarize(
      subjNr = first( subjNr ),
      proAnti = first( proAnti ),
      instruction = first( instruction ),
      rtDiff = mean( rtCorr[ stimLimbPost == "sX" ] ) - mean( rtCorr[ stimLimbPost == "sII" ] ),
      pcDiff = mean( pc[ stimLimbPost == "sX" ] ) - mean( pc[ stimLimbPost == "sII" ] ),
      bisDiff = mean( bisGrouped[ stimLimbPost == "sX" ] ) - mean( bisGrouped[ stimLimbPost == "sII" ] ),
      stimLimbPost = first( stimLimbPost ),
      respLimbPost = first( respLimbPost ),
    )
}

d.sel <- dC.m %>% filter( proAnti == "task: pro", instruction == "ext" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.ext.3p <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, proAnti, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.sel <- dC.m %>% filter( proAnti == "task: pro", instruction == "anat" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.anat.3p <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, proAnti, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.sel <- dC.m %>% filter( proAnti == "task: anti", instruction == "ext" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.ext.3a <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, proAnti, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.sel <- dC.m %>% filter( proAnti == "task: anti", instruction == "anat" & substr(as.character(dC.m$respLimbPost), 2, 3 ) != substr(as.character(dC.m$stimLimbPost), 2, 3 ) )
d.diff.anat.3a <- cmp_1limb_crossing( d.sel ) %>%
  select( subjNr, proAnti, instruction, stimLimbPost, respLimbPost, rtDiff, pcDiff, bisDiff )

d.contr <- bind_rows( d.diff.ext.3p, d.diff.anat.3p, d.diff.ext.3a, d.diff.anat.3a )
yLowerRt <- min( d.contr$rtDiff ) - 50
yUpperRt <- max(d.contr$rtDiff ) + 50
yLowerPc <- min( d.contr$pcDiff ) - 2
yUpperPc <- max(d.contr$pcDiff ) + 2
yLowerBis <- min( d.contr$bisDiff ) - 0.5
yUpperBis <- max(d.contr$bisDiff ) + 0.5
( fig.rt.contr <-  plot_1Dgrid_difference_with_violin( d.contr, instruction, rtDiff,  proAnti, "instruction", "RT diff. (ms): sX-rII minus sII-rX",          colorsDiff1, yLowerRt,  yUpperRt  ) )
( fig.pc.contr <-  plot_1Dgrid_difference_with_violin( d.contr, instruction, pcDiff,  proAnti, "instruction", "acc. diff. (% correct): sX-rII minus sII-rX", colorsDiff1, yLowerPc,  yUpperPc  ) )
( fig.bis.contr <- plot_1Dgrid_difference_with_violin( d.contr, instruction, bisDiff, proAnti, "instruction", "BIS diff. (unitless): sX-rII minus sII-rX",   colorsDiff1, yLowerBis, yUpperBis ) )


labelSize <- 18
labelsTop <- c('a')
labelsBottom <- c('b', 'c')
rowHeight <- 1.5
lowerWidth <- 1.7
figWidth = 20

topRow <- plot_grid( fig.rt.pa,
                     labels = labelsTop, label_size = labelSize,
                     rel_widths = c( 1 ) )
bottomRow <- plot_grid( fig.rt.diff, fig.rt.contr,
                        labels = labelsBottom, label_size = labelSize,
                        ncol = 2, rel_widths = c( lowerWidth, 1 ) )
( exp1FigRt <- cowplot::plot_grid(topRow, bottomRow,
                                  rel_heights = c( rowHeight, 1), ncol = 1) )
ggsave( paste0( parentPath, "/outputNEW/fig_Exp", expAbbrev, "_rt.pdf" ),
        width = figWidth, height = figWidth/1.1, units = "cm" )

topRow <- plot_grid( fig.pc.pa,
                     labels = labelsTop, label_size = labelSize,
                     rel_widths = c( 1 ) )
bottomRow <- plot_grid( fig.pc.diff, fig.pc.contr,
                        labels = labelsBottom, label_size = labelSize,
                        ncol = 2, rel_widths = c( lowerWidth, 1 ) )
( exp1FigPc <- cowplot::plot_grid(topRow, bottomRow,
                                  rel_heights = c( rowHeight, 1), ncol = 1) )
ggsave( paste0( parentPath, "/outputNEW/fig_Exp", expAbbrev, "_pc.pdf" ),
        width = figWidth, height = figWidth/1.1, units = "cm" )

topRow <- plot_grid( fig.bis.pa,
                     labels = labelsTop, label_size = labelSize,
                     rel_widths = c( 1 )  )
bottomRow <- plot_grid( fig.bis.diff, fig.bis.contr,
                        labels = labelsBottom, label_size = labelSize,
                        ncol = 2, rel_widths = c( lowerWidth, 1 ) )
( exp1FigBis <- cowplot::plot_grid(topRow, bottomRow,
                                   rel_heights = c( rowHeight, 1), ncol = 1) )
ggsave( paste0( parentPath, "/outputNEW/fig_Exp", expAbbrev, "_bis.pdf" ),
        width = figWidth, height = figWidth/1.1, units = "cm" )

##### Exp. 3 STATS

# BIS anova both PRO and ANTI
print( dC.m %>% group_by( proAnti, instruction, stimLimbPost, respLimbPost ) %>%
         summarize(
           rtSd = sd( rtCorr ),
           rt = mean( rtCorr ),
           pcSd = sd( pc ),
           pc = mean( pc ),
           bisSd = sd( bisGrouped ),
           bis = mean( bisGrouped ),
         ), n = Inf )
( nice( exp3full.aov.bis <- aov_ez( id = "subjNr",
                                    dv = "bisGrouped",
                                    data = dC.m,
                                    within = c( "proAnti", "instruction", "stimLimbPost", "respLimbPost" ),
                                    type = 3 ) ) )

# BIS anova PRO
dC.m.3p <- dC.m %>% filter( proAnti == "task: pro" )
print( dC.m.3p %>% group_by( instruction, stimLimbPost, respLimbPost ) %>%
         summarize(
           rtSd = sd( rtCorr ),
           rt = mean( rtCorr ),
           pcSd = sd( pc ),
           pc = mean( pc ),
           bisSd = sd( bisGrouped ),
           bis = mean( bisGrouped ),
         ), n = Inf )
( nice( exp3pro.aov.bis <- aov_ez( id = "subjNr",
                                 dv = "bisGrouped",
                                 data = dC.m.3p,
                                 within = c( "instruction", "stimLimbPost", "respLimbPost" ),
                                 type = 3 ) ) )

# BIS anova ANTI
dC.m.3a <- dC.m %>% filter( proAnti == "task: anti" )
print( dC.m.3a %>% group_by( instruction, stimLimbPost, respLimbPost ) %>%
         summarize(
           rtSd = sd( rtCorr ),
           rt = mean( rtCorr ),
           pcSd = sd( pc ),
           pc = mean( pc ),
           bisSd = sd( bisGrouped ),
           bis = mean( bisGrouped ),
         ), n = Inf )
( nice( exp3anti.aov.bis <- aov_ez( id = "subjNr",
                                 dv = "bisGrouped",
                                 data = dC.m.3a,
                                 within = c( "instruction", "stimLimbPost", "respLimbPost" ),
                                 type = 3 ) ) )

t1 <- dC.m.3p %>% filter( instruction == "ext"  & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t2 <- dC.m.3p %>% filter( instruction == "ext"  & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped )
t3 <- dC.m.3p %>% filter( instruction == "anat" & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t4 <- dC.m.3p %>% filter( instruction == "anat" & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped ) 

t5 <- dC.m.3a %>% filter( instruction == "ext"  & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t6 <- dC.m.3a %>% filter( instruction == "ext"  & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped )
t7 <- dC.m.3a %>% filter( instruction == "anat" & stimLimbPost == "sII" & respLimbPost == "rX"  ) %>% select( bisGrouped ) 
t8 <- dC.m.3a %>% filter( instruction == "anat" & stimLimbPost == "sX"  & respLimbPost == "rII" ) %>% select( bisGrouped ) 

tc <- data.frame( v1 = t1$bisGrouped, v2 = t2$bisGrouped, v3 = t3$bisGrouped, v4 = t4$bisGrouped,
                  v5 = t5$bisGrouped, v6 = t6$bisGrouped, v7 = t7$bisGrouped, v8 = t8$bisGrouped)
tc$v9 = tc$v2- tc$v4  #pro , ext vs anantomical, 
tc$v10 = tc$v6- tc$v8 #anti , ext vs anantomical, 
tc$v11 = tc$v1- tc$v3 #pro , ext vs anantomical, 
tc$v12 = tc$v5- tc$v7 #anti , ext vs anantomical, 
tc$v13 = tc$v2- tc$v1 #pro , ext instrucntion, crossed feet vs crossed hands
tc$v14 = tc$v6- tc$v5 #anti , ext instrucntion, crossed feet vs crossed hands
tc$v15 = tc$v4- tc$v3 #pro , anat instrucntion, crossed feet vs crossed hands
tc$v16 = tc$v8- tc$v7 #anti , anat instrucntion,rossed feet vs crossed hands


# pro
# leg crossing ext vs. anat
Pro_crossLegs_ext_vs_anat <- t.test( tc$v2, tc$v4, paired = TRUE )
# arm crossing anat vs. ext
Pro_crossArms_ext_vs_anat <- t.test( tc$v1, tc$v3, paired = TRUE )
# external, crossed legs vs. crossed arms
Pro_ext_crossLegs_vs_crossedArms <- t.test( tc$v2, tc$v1, paired = TRUE )
# anatomical, crossed arms vs. crossed legs
Pro_anat_crossedLegs_vs_crossArms <- t.test( tc$v4, tc$v3, paired = TRUE )

# Anti
# leg crossing anat vs. ext
Anti_crossLegs_ext_vs_anat <- t.test( tc$v6, tc$v8, paired = TRUE )
# arm crossing ext vs. anat
Anti_crossArms_ext_vs_anat <- t.test( tc$v5, tc$v7, paired = TRUE )
# external, crossed arms vs. crossed legs
Anti_ext_crossLegs_vs_crossedArms <- t.test( tc$v6, tc$v5, paired = TRUE )
# anatomical, crossed legs vs. crossed arms
Anti_anat_crossedLegs_vs_crossArms <- t.test( tc$v8, tc$v7, paired = TRUE )


# Differences between subexperiments
# Pro vs anti: stimulated limb corssing ext vs anat 
# crossStim_ext_vs_anat_Pro_vs_Anti <- t.test( tc$v9, tc$v10, paired = TRUE )
crossStim_ext_vs_anat_Pro_vs_negAnti <- t.test( tc$v9, -tc$v10, paired = TRUE )
# Pro vs anti: response limb crossing anat vs external
crossResp_ext_vs_anat_Pro_vs_negAnti<- t.test( tc$v11, -tc$v12, paired = TRUE )

# external Pro vs anat anti: stimulated vs response limb crossing
ext_crossS_vs_crossR_Pro_vs_anat_crossS_vs_crossR_Anti <- t.test( tc$v13, tc$v16, paired = TRUE )

# anat Pro vs ext anti: stimulated vs response limb crossing
anat_crossS_vs_crossR_Pro_vs_ext_crossS_vs_crossR_Anti <- t.test( tc$v14, tc$v15, paired = TRUE )


# export tables for paper
# anova reported in main paper
t <- format_anova_table( exp3pro.aov.bis )
tt <- format_ttest_table(Pro_crossLegs_ext_vs_anat,Pro_crossArms_ext_vs_anat,Pro_ext_crossLegs_vs_crossedArms,Pro_anat_crossedLegs_vs_crossArms)
read_docx() %>% body_add_flextable( style_table( data.frame( t ) ) ) %>% body_add_par(value = "") %>% body_add_flextable( style_table( data.frame( tt ) ) ) %>%
  print( target = paste0( parentPath,  "/outputNEW/anovatable_Exp3Pro.docx" ))

t <- format_anova_table( exp3anti.aov.bis )
tt <- format_ttest_table(Anti_crossLegs_ext_vs_anat,Anti_crossArms_ext_vs_anat,Anti_ext_crossLegs_vs_crossedArms,Anti_anat_crossedLegs_vs_crossArms)
read_docx() %>% body_add_flextable( style_table( data.frame( t ) ) ) %>% body_add_par(value = "") %>% body_add_flextable( style_table( data.frame( tt ) ) ) %>%
  print( target = paste0( parentPath,  "/outputNEW/anovatable_Exp3Anti.docx" ))


# Anova with both stimulation
t <- format_anova_table( exp3full.aov.bis )
tt <- format_ttest_table(crossStim_ext_vs_anat_Pro_vs_negAnti,crossResp_ext_vs_anat_Pro_vs_negAnti,ext_crossS_vs_crossR_Pro_vs_anat_crossS_vs_crossR_Anti,anat_crossS_vs_crossR_Pro_vs_ext_crossS_vs_crossR_Anti)

read_docx() %>% body_add_flextable( style_table( data.frame( t ) ) ) %>% body_add_par(value = "") %>% body_add_flextable( style_table( data.frame( tt ) ) ) %>%
  print( target = paste0( parentPath,  "/outputNEW/anovatable_Exp3_full.docx" ))


# # ##### Exp. 3 STATS
# # =UNCLEAR WHAT IS HAPPENING HERE. REDID LIKE OTHER EXPERIMENTS
# 
# # recode factors
# dC.m$sA_sS <- dC.m$stimLimbPost
# dC.m$sA_sS <- fct_recode( dC.m$sA_sS, "-1" = "sII", "1" = "sX" )
# dC.m$rA_rS <- dC.m$respLimbPost
# dC.m$rA_rS <- fct_recode( dC.m$rA_rS, "-1" = "rII", "1" = "rX" )
# dC.m$sA_rA <- "-1"
# dC.m$sA_rA[ dC.m$proAnti == "pro" & dC.m$instruction == "ext" & dC.m$stimLimbPost == "sII" & dC.m$respLimbPost == "rX" ] <- "1"
# dC.m$sA_rA[ dC.m$proAnti == "pro" & dC.m$instruction == "ext" & dC.m$stimLimbPost == "sX" & dC.m$respLimbPost == "rII" ] <- "1"
# dC.m$sA_rA[ dC.m$proAnti == "anti" & dC.m$instruction == "ext" & dC.m$stimLimbPost == "sII" & dC.m$respLimbPost == "rII" ] <- "1"
# dC.m$sA_rA[ dC.m$proAnti == "anti" & dC.m$instruction == "ext" & dC.m$stimLimbPost == "sX" & dC.m$respLimbPost == "rX" ] <- "1"
# dC.m$sA_rA[ dC.m$proAnti == "anti" & dC.m$instruction == "anat" ] <- "1"
# dC.m$sA_rA <- as.factor( dC.m$sA_rA )
# 
# dC.m$sA_rS <- as.factor( as.numeric( as.character( dC.m$sA_rA ) ) * as.numeric( as.character( dC.m$rA_rS ) ) )
# 
# # test whether recoding worked: half of lines must now be incomp
# # this pans out
# nrow(dC.m %>% filter( sA_rA == "1"))
# 
# # BIS anova
# print( dC.g <- dC.m %>% group_by( proAnti, instruction, stimLimbPost, respLimbPost, sA_rA, sA_sS, rA_rS ) %>%
#          summarize(
#            rtSd = sd( rtCorr ),
#            rt = mean( rtCorr ),
#            pcSd = sd( pc ),
#            pc = mean( pc ),
#            bisSd = sd( bisGrouped ),
#            bis = mean( bisGrouped ),
#          ), n = Inf )
# ( nice( exp3.aov.bis.confl <- aov_ez( id = "subjNr", dv = "bisGrouped", data = dC.m, within = c( "sA_rA", "sA_sS", "rA_rS" ), type = 3 ) ) )
# ( nice( exp3.aov.bis.full16 <- aov_ez( id = "subjNr", dv = "bisGrouped", data = dC.m, within = c( "instruction", "sA_rA", "sA_sS", "rA_rS" ), type = 3 ) ) )
# 
# print( dC.g <- dC.m %>% group_by( sA_rA, sA_sS, rA_rS ) %>%
#          summarize(
#            rtSd = sd( rtCorr ),
#            rt = mean( rtCorr ),
#            pcSd = sd( pc ),
#            pc = mean( pc ),
#            bisSd = sd( bisGrouped ),
#            bis = mean( bisGrouped ),
#          ), n = Inf )
# 
# 
# av_eff <- function( d ){
#   dn <- d%>% summarize(
#     bisGrouped = mean( bisGrouped )
#   ) %>%
#   summarize(
#     sd = sd( bisGrouped ),
#     bis = mean( bisGrouped )
#   )
# }
# 
# ( eff <- data.frame( sA_rA = dC.m %>% filter( sA_rA == "-1" ) %>% group_by( subjNr ) %>% av_eff ) )
# 
# ( eff$sA_sS <- dC.m %>% filter( sA_sS == "-1" ) %>% group_by( subjNr ) %>% av_eff )
# 
# ( eff$rA_rS <- dC.m %>% filter( rA_rS == "-1" ) %>% group_by( subjNr )  %>% av_eff )
# 
# ( eff$sA_rS <- dC.m %>% filter( sA_rS == "-1" ) %>% group_by( subjNr )  %>% av_eff )
# 
# t1 <- dC.m %>% filter( instruction == "ext" & stimLimbPost == "sII" & respLimbPost == "rX" ) %>% select( bisGrouped ) 
# t2 <- dC.m %>% filter( instruction == "ext" & stimLimbPost == "sX" & respLimbPost == "rII" ) %>% select( bisGrouped )
# t3 <- dC.m %>% filter( instruction == "anat" & stimLimbPost == "sII" & respLimbPost == "rX" ) %>% select( bisGrouped ) 
# t4 <- dC.m %>% filter( instruction == "anat" & stimLimbPost == "sX" & respLimbPost == "rII" ) %>% select( bisGrouped ) 
# tc <- data.frame( v1 = t1$bisGrouped, v2 = t2$bisGrouped, v3 = t3$bisGrouped, v4 = t4$bisGrouped )
# 
# # external, crossed legs vs. crossed arms
# t.test( tc$v1, tc$v2, paired = TRUE )
# 
# # anatomical, crossed legs vs. crossed arms
# t.test( tc$v3, tc$v4, paired = TRUE )
# 
# # leg crossing ext vs. anat
# t.test( tc$v2, tc$v4, paired = TRUE )
# 
# # arm crossing ext vs. anat
# t.test( tc$v1, tc$v3, paired = TRUE )
# 
