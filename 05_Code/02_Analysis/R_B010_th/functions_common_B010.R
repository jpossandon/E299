################################################################################
## prepare logfile data
################################################################################
# set thresholds for cleaning in switch section at beginning of manuscript
rawdata_preprocessing <- function( d ){
# rename variables to be consistent with the labs recent variable names and across experiments
  d <- d %>% rename( subjNr = subjIndx )
  d <- d %>% rename( rt = trial_RT )
  d <- d %>% rename( respCorr = trial_correct )
  d <- d %>% rename( instruction = trial_blockType )
  d <- d %>% rename( respButton = trial_response )
  d <- d %>% rename( stimLimb = trial_limbside )
  d <- d %>% rename( intensity = trial_int )
  # code rt in ms, not s
  d$rt <- d$rt * 1000
  # convert variables into correct types, mostly factors
  d$instruction <- fct_recode( as.factor( d$instruction ), ext = "1", anat = "2")
  d$stimLimbPost <- fct_recode( as.factor( d$stimLimbPost ), sII = "0", sX = "1")
  d$respLimbPost <- fct_recode( as.factor( d$respLimbPost ), rII = "0", rX = "1")
  d$respButton <- fct_recode( as.factor( d$respButton ), leftButton = "1", rightButton = "2", invalidTrial = "0" )
  d$stimLimb <- fct_recode( as.factor( d$stimLimb ), leftLimb = "1", rightLimb = "2")
  d$subjNr <- as.factor( d$subjNr )
  d$instruction <- as.factor( d$instruction )
  d$respCorr <- as.integer( d$respCorr )
  d$intensity <- as.factor( d$intensity )
  # select relevant columns
  
  # remove all invalid trials from dataset
  # these can be identified by rt = NaN, respButton = invalidTrial
  d <- d %>% filter( !is.na( rt ) )
  # sanity check: we filter by rt; afterwards, there should not be any trials with invalid button press
  d$respButton <- droplevels( d$respButton )
  levels(d$respButton)
  # this pans out ;)
  return ( d )
}


################################################################################
## Clean Data by removing very fast and very slow trials
################################################################################
# set thresholds for cleaning in switch section at beginning of manuscript
clean_data <- function( d, upperCutOffSd, lowerCutOffMs, ... ){
  # pass up to 5 factors according to which the dataset will be grouped, 
  # so that cleaning by some sd value will refer to the respective cells
  # rather than the entire dataset
  print( paste0( "cleaning data: upper: ", upperCutOffSd, " s.d., lower: ", lowerCutOffMs, " ms.") )
  
  clean <- function( x, upper, lower ) {replace( x, x > mean( x, na.rm=T ) + upper * sd( x, na.rm=T ) | x < lower, NA ) }
  
  dClean <- d %>% group_by( ... ) %>%
    mutate(
      rtClean = clean( rt, upperCutOffSd, lowerCutOffMs )
    )
  
  dClean[ !is.na( dClean$rtClean ), "respClean"] <- dClean[ !is.na( dClean$rtClean ), "respCorr" ]
  dClean$rt[ is.na( dClean$respClean ) ] <- NA
  dClean$respCorr[ is.na( dClean$respClean ) ] <- NA
  
  dC <- subset( dClean, !is.na( dClean$respClean ) )
  
  print( paste( "trials total before cleaning:", nrow( d ) ) )
  print( paste( "trials total after cleaning:", nrow( dC ) ) )
  print( paste( "percent eliminated:", round( 100 - nrow( dC ) / nrow( d ) * 100, digits = 2 ) ) )
  
  ## Sanity checks: check whether data is as expected/planned.
  # no NA remaining in responseCorr column
  # number of participants 
  sanityCheck <- dClean %>% group_by( ... )  %>%
    summarize(
      N = sum( is.na( respCorr ) )
    )
  print( paste( "number of NA remaining in response column (if not \"\", something is wrong):", nrow( sanityCheck$N > 0 ) ) )
  print( paste( "number of participants in data frame:", length( levels( dC$subjNr ) ) ) ) # nr of subjects
  
  # group_by adds plucks as new columns, so remove them again
  return ( dC )  
}


###############################################################################
## compute Balanced Integration Score (BIS) using a grouping variable
## that is used for normalization
###############################################################################
bis_grouped <- function( dataFrame, rt, pc, groupingFactor ){
  # computes the BIS by normalizing per unit, rather than by the entire dataset.
  # We use this function to normalize by participant, given that we are interested
  # in within-subject factors.
  # rt, pc = variables to normalize
  # groupingFactor = grouping variable, e.g. participants, experimental groups
  # for equations, see
  # Liesefeld, H. R., & Janczyk, M. (2019). Combining speed and accuracy to control
  # for speed-accuracy trade-offs(?).
  # Behavior Research Methods, 51(1), 40–60.
  # https://doi.org/10.3758/s13428-018-1076-x
  dataFrame <- dataFrame %>% group_by( {{groupingFactor}} ) %>%
    mutate(
      varBisRtGrouped = sqrt( sum ( ( {{rt}} - mean( {{rt}} ) ) ^ 2 ) / n() ),
      varBisPcGrouped = sqrt( sum ( ( {{pc}} - mean( {{pc}} ) ) ^ 2 ) / n() ),
      n = n(),
      zRtGrouped = round( ( {{rt}} - mean( {{rt}} ) ) / varBisRtGrouped, 3 ),
      zPcGrouped = round( ( {{pc}} - mean( {{pc}} ) ) / varBisPcGrouped, 3 ),
      bisGrouped = zPcGrouped - zRtGrouped
    )
  return( dataFrame )
}


###############################################################################
## compute Balanced Integration Score (BIS) across the entire dataset
###############################################################################
bis_samplewise <- function(dataFrame, rt, pc ){
  # computes the BIS by normalizing across the entire dataset.
  # There is a discussion about how to normalize, see
  # Liesefeld, H. R., & Janczyk, M. (2022). Same same but different:
  # Subtle but consequential differences between two measures to linearly integrate
  # speed and accuracy (LISAS vs. BIS).
  # Behavior Research Methods. 
  # https://doi.org/10.3758/s13428-022-01843-2
  rte <- eval( substitute( rt ), dataFrame )
  varBisRtSample <- sqrt( sum ( ( rte - mean( rte ) ) ^ 2 ) / nrow( dataFrame ) )
  dataFrame <- dataFrame %>% ungroup() %>% mutate( 
    zRtSample = ( {{rt}} - mean( {{rt}} ) ) / varBisRtSample
  )
  pce <- eval( substitute( pc ), dataFrame )
  varBisPcSample <- sqrt( sum ( ( pce - mean( pce ) ) ^ 2 ) / nrow( dataFrame ) )
  dataFrame <- dataFrame %>% ungroup() %>% mutate( 
    zPcSample = ( {{pc}} - mean( {{pc}} ) ) / varBisPcSample,
    bisSample = zPcSample - zRtSample
  )
  return( dataFrame )
}


################################################################################
## Remove excluded participants from data.frame
################################################################################
remove_participants <- function( dC, removeList ){
  print( "removing participants..." )
  dC <- dC %>% filter( !(subjNr %in% removeList) )
  # remove factor levels from subjNr factor
  dC$subjNr <- droplevels( dC$subjNr )
  return( dC )
}


################################################################################
## Keep included participants in data.frame
################################################################################
keep_participants <- function( dC, keepList ){
  print( "keeping only some participants..." )
  dC <- dC %>% filter( (subjNr %in% keepList) )
  # remove factor levels from subjNr factor
  dC$subjNr <- droplevels( dC$subjNr )
  print( "keeping: ", unique( dC$subjNr ) )
  return( dC )
}


################################################################################
## 
################################################################################
summarize_per_participant <- function( d, ... ){
  # pass dC = all single trials, cleaned
  # and the list of factors according to which you want to group
  # return dC.m = aggregated at participant level
  dn <- d %>% group_by( ... ) %>% 
    summarize( 
      sdRtCorr = sd( rt[ respCorr == 1 ] ),
      rtCorr = mean( rt[ respCorr == 1 ] ),
      rtAll = mean( rt ),
      corrCount = sum( respCorr == 1 ),
      totalCount = n(),
      pc = corrCount / totalCount *100
    )
  dn <- bis_grouped( dn, rtCorr, pc, subjNr )
  dn <- bis_samplewise( dn, rtCorr, pc )
  # print( dn, n = Inf )
  return( dn ) # used to be dC.m
}


################################################################################
## 
################################################################################
aggregate_group_raw <- function( d, ... ){
  # pass: dC.m = aggregated at participant level
  # return: group-level agregate
  nSubj <- length( unique( levels( d$subjNr ) ) )
  dn <- d %>% group_by( ... ) %>%
    summarize( 
      seRt = sd( rtCorr ) / sqrt( nSubj ),
      rt = mean( rtCorr ),
      sePc = sd( pc ) / sqrt( nSubj ),
      pc = mean( pc ),
      seBis = sd( bisGrouped ) / sqrt( nSubj ),
      bis = mean( bisGrouped )
    )
  print( dn, n = Inf, width = Inf )
  return( dn ) # used to be dC.g
}


###############################################################################
## plot the data of single participants, with x axis showing 3 factors
################################################################################
plot_single_participants_3x <- function( dataFrame, x1, x2, x3, y, yAxisTitle, yMin, yMax ){
  # y is the dependent variable to be plotted
  # yAxisTitle is the text of the y axis
  # yMin, yMax are axis min and max values that can be provided if needed
  # otherwise, min/max values are determined and multiplied by 1.1
  head(dataFrame)
  if (missing( yMin ) ) {
    yMin <- 0
    dataMin <- min( pluck( dataFrame, y ) )
    if ( dataMin < 0 ){ yMin <- dataMin * 1.1 }
  }
  if( missing( yMax ) ) {
    yMax <- max( pluck( dataFrame, y  ) * 1.1 )
  }
  g <- ggplot( data = dataFrame, aes( x = pluck( dataFrame, x1 ) : pluck( dataFrame, x2 ) : pluck( dataFrame, x3 ), color = pluck( dataFrame, x1 ), y = pluck( dataFrame, y ) ) ) +
    geom_point() +
    facet_wrap( ~ subjNr ) +
    scale_y_continuous( limits = c( yMin, yMax ) ) + 
    ylab( yAxisTitle )
  if (yMin < 0 ){
    g <- g + geom_hline( yintercept = 0, color = "blue" )
  }
  return( g )
}


################################################################################
## plot group (plot all conditions in 1 panel)
################################################################################
plot_group_with_violins <- function( d, x, y, colorVar, xlabText, ylabText, colorValues, legendPos, yMin, yMax ){
  fontSize <- 9
  dotSizeMean <- 2
  dotSizeSingle <- 1
  dotSizeSum <- 0.3
  yAsString <- deparse( substitute( y ) )
  if (missing( yMin ) ) {
    yMin <- 0
    dataMin <- min( pluck( d, yAsString ) )
    if ( dataMin < 0 ){ yMin <- dataMin * 1.1 }
  }
  if( missing( yMax ) ) {
    yMax <- max( pluck( d, yAsString  ) * 1.1 )
  }
  g <- ggplot( d, aes( x = {{ x }}, y = {{ y }}, color = {{ colorVar }}, fill = {{ colorVar }} ) )
  g <- g + geom_rain( alpha = 0.2,
                      boxplot.args.pos = list( position = ggpp::position_dodgenudge( x = .15 ), width = 0.1 ),
                      violin.args.pos = list( side = "r", position = position_nudge( x = 0.3 ), linewidth = 0.2 ) )
  g <- g + stat_summary( size = 0.5, geom = "errorbar", width = 0.1 )
  g <- g + stat_summary( size = dotSizeSum )
  g <- g + cowplot::theme_cowplot()
  g <- g + ggplot2::scale_color_manual( values = colorValues )
  g <- g + scale_fill_manual( values = colorValues )
  g <- g + scale_shape_manual( values = c( 16, 4 ) )
  g <- g + xlab( xlabText )
  g <- g + ylab( ylabText )
  g <- g + ylim( yMin, yMax )
  g <- g + theme( axis.title = element_text( size = fontSize ) )
  g <- g + theme( axis.text = element_text( size = fontSize ) )
  g <- g + theme( legend.title = element_text(size = fontSize ) )
  g <- g + theme( legend.text = element_text(size = fontSize ) )
  g <- g + theme( legend.title = element_blank() )
  g <- g + theme( legend.position = "none" )
  return( g )
}


################################################################################
## plot group (plot all conditions in 2 panels)
################################################################################
plot_1Dgrid_group_with_violins <- function( d, x, y, gridVar, colorVar, xlabText, ylabText, colorValues, legendPos, yMin, yMax ){
  fontSize <- 9
  dotSizeMean <- 2
  dotSizeSingle <- 1
  dotSizeSum <- 0.3
  yAsString <- deparse( substitute( y ) )
  if (missing( yMin ) ) {
    yMin <- 0
    dataMin <- min( pluck( d, yAsString ) )
    if ( dataMin < 0 ){ yMin <- dataMin * 1.1 }
  }
  if( missing( yMax ) ) {
    yMax <- max( pluck( d, yAsString  ) * 1.1 )
  }
  g <- ggplot( d, aes( x = {{ x }}, y = {{ y }}, color = {{ colorVar }}, fill = {{ colorVar }} ) )
  g <- g + geom_rain( alpha = 0.2,
                      boxplot.args.pos = list( position = ggpp::position_dodgenudge( x = .15 ), width = 0.1 ),
                      violin.args.pos = list( side = "r", position = position_nudge( x = 0.3 ), linewidth = 0.2 ) )
  g <- g + stat_summary( size = 0.5, geom = "errorbar", width = 0.1 )
  g <- g + stat_summary( size = dotSizeSum )
  g <- g + facet_grid( cols =vars( {{gridVar}} ) )
  g <- g + cowplot::theme_cowplot()
  g <- g + ggplot2::scale_color_manual( values = colorValues )
  g <- g + scale_fill_manual( values = colorValues )
  g <- g + scale_shape_manual( values = c( 16, 4 ) )
  g <- g + xlab( xlabText )
  g <- g + ylab( ylabText )
  g <- g + ylim( yMin, yMax )
  g <- g + theme( axis.title = element_text( size = fontSize ) )
  g <- g + theme( axis.text = element_text( size = fontSize ) )
  g <- g + theme( legend.title = element_text(size = fontSize ) )
  g <- g + theme( legend.text = element_text(size = fontSize ) )
  g <- g + theme( legend.title = element_blank() )
  g <- g + theme( legend.position = "none" )
  return( g )
}


################################################################################
## plot group (plot all conditions in 4 panels)
################################################################################
plot_2Dgrid_group_with_violins <- function( d, x, y, gridVarCol, gridVarRow, colorVar, xlabText, ylabText, colorValues, legendPos, yMin, yMax ){
  fontSize <- 9
  dotSizeMean <- 2
  dotSizeSingle <- 1
  dotSizeSum <- 0.3
  yAsString <- deparse( substitute( y ) )
  if (missing( yMin ) ) {
    yMin <- 0
    dataMin <- min( pluck( d, yAsString ) )
    if ( dataMin < 0 ){ yMin <- dataMin * 1.1 }
  }
  if( missing( yMax ) ) {
    yMax <- max( pluck( d, yAsString  ) * 1.1 )
  }
  g <- ggplot( d, aes( x = {{ x }}, y = {{ y }}, color = {{ colorVar }}, fill = {{ colorVar }} ) )
  g <- g + geom_rain( alpha = 0.2,
                      boxplot.args.pos = list( position = ggpp::position_dodgenudge( x = .15 ), width = 0.1 ),
                      violin.args.pos = list( side = "r", position = position_nudge( x = 0.3 ), linewidth = 0.2 ) )
  g <- g + stat_summary( size = 0.5, geom = "errorbar", width = 0.1 )
  g <- g + stat_summary( size = dotSizeSum )
#  g <- g + facet_grid( cols = vars( proAnti, instruction ) )
  g <- g + facet_grid( cols = vars( {{gridVarCol}} ), rows = vars( {{gridVarRow}} ) )
  g <- g + cowplot::theme_cowplot()
  g <- g + ggplot2::scale_color_manual( values = colorValues )
  g <- g + scale_fill_manual( values = colorValues )
  g <- g + scale_shape_manual( values = c( 16, 4 ) )
  g <- g + xlab( xlabText )
  g <- g + ylab( ylabText )
  g <- g + ylim( yMin, yMax )
  g <- g + theme( axis.title = element_text( size = fontSize ) )
  g <- g + theme( axis.text = element_text( size = fontSize ) )
  g <- g + theme( legend.title = element_text(size = fontSize ) )
  g <- g + theme( legend.text = element_text(size = fontSize ) )
  g <- g + theme( legend.title = element_blank() )
  g <- g + theme( legend.position = "none" )
  return( g )
}


################################################################################
## difference between several conditions
################################################################################
aggregate_per_participant_diff <- function( d, by, factors ){
  # pass: dC.m = aggregated at participant level
  # by = the variable by which the dataset is split and taken the diference of => y-axis
  # factors = the factors that will appear on the e-axis, as a text vector
  # return: participant-level difference Level 1 minus Level 2
  # using "mean" allows averaging over factors that are ignored, such as 
  # soa in Exp. 1. If no factor is ignored, then mean() is identical to first().
  
  # transform the factor according to which the diff is to be computed into factor levels as numbers
  # allowing us to abstract from factor names
  d$diffVar <- as.numeric( select( d, {{ by }} )[[ 1 ]] )
  dn <- d  %>% group_by( subjNr, select( d, all_of( factors ) ) ) %>%
    summarize( 
      rtLevel1 = mean( rtCorr[ diffVar == 1 ] ),
      rtLevel2 = mean( rtCorr[ diffVar == 2 ] ),
      rtDiff = rtLevel1 - rtLevel2,
      pcLevel1 = mean( pc[ diffVar == 1 ] ),
      pcLevel2 = mean( pc[ diffVar == 2 ] ),
      pcDiff = pcLevel1 - pcLevel2,
      bisLevel1 = mean( bisGrouped[ diffVar == 1 ] ),
      bisLevel2 = mean( bisGrouped[ diffVar == 2 ] ),
      bisDiff = bisLevel1 - bisLevel2,
    )
  return( dn ) # used to be dC.m.Diff
}


################################################################################
## difference plots
################################################################################
plot_difference_with_violin <- function( d, x, y, xlabText, ylabText, colorValues, yMin, yMax, nXaxis ){
  # pass: dC.m.(noSoa.)Cr = participant-level differences
  fontSize <- 9
  dotSizeMean <- 2
  dotSizeSingle <- 1
  dotSizeSum <- 0.3
  yAsString <- deparse( substitute( y ) )
  if (missing( yMin ) ) {
    yMin <- 0
    dataMin <- min( pluck( d, yAsString ) )
    if ( dataMin < 0 ){ yMin <- dataMin * 1.1 }
  }
  if( missing( yMax ) ) {
    yMax <- max( pluck( d, yAsString  ) * 1.1 )
  }
  d$fakeVar <- 1
  d$fakeVar <- as.factor( d$fakeVar )

  if (missing( nXaxis ) ) {
    nXaxis <- 4
  }
  
  g <- ggplot( d, aes( x = {{ x }}, y = {{ y }}, color = fakeVar, fill = fakeVar ) )
  g <- g + geom_rain( alpha = 0.2,
                      boxplot.args.pos = list( position = ggpp::position_dodgenudge( x = .15 ), width = 0.1 ),
                      violin.args.pos = list( side = "r", position = position_nudge( x = 0.3 ), linewidth = 0.2 ) )
  g <- g + stat_summary( size = 0.5, geom = "errorbar", width = 0.1, color = rep( colorValues[1], nXaxis ) )
  g <- g + stat_summary( size = dotSizeSum, color = colorValues[1] )
  g <- g + geom_hline( yintercept = 0, linetype = "dashed" )
  g <- g + cowplot::theme_cowplot()
  g <- g + ggplot2::scale_color_manual( values = colorValues )
  g <- g + scale_fill_manual( values = colorValues )
  g <- g + xlab( xlabText )
  g <- g + ylab( ylabText )
  g <- g + ylim( yMin, yMax )
  g <- g + theme( axis.title = element_text(size =fontSize ) )
  g <- g + theme( axis.text = element_text( size = fontSize ) )
  g <- g + theme( legend.position = "none" )
  return( g )
}


################################################################################
## difference plots
################################################################################
plot_1Dgrid_difference_with_violin <- function( d, x, y, gridVar, xlabText, ylabText, colorValues, yMin, yMax ){
  # pass: dC.m.(noSoa.)Cr = participant-level differences
  fontSize <- 9
  dotSizeMean <- 2
  dotSizeSingle <- 1
  dotSizeSum <- 0.3
  yAsString <- deparse( substitute( y ) )
  if (missing( yMin ) ) {
    yMin <- 0
    dataMin <- min( pluck( d, yAsString ) )
    if ( dataMin < 0 ){ yMin <- dataMin * 1.1 }
  }
  if( missing( yMax ) ) {
    yMax <- max( pluck( d, yAsString  ) * 1.1 )
  }
  d$fakeVar <- 1
  d$fakeVar <- as.factor( d$fakeVar )
  
  g <- ggplot( d, aes( x = {{ x }}, y = {{ y }}, color = fakeVar, fill = fakeVar ) )
  g <- g + geom_rain( alpha = 0.2,
                      boxplot.args.pos = list( position = ggpp::position_dodgenudge( x = .15 ), width = 0.1 ),
                      violin.args.pos = list( side = "r", position = position_nudge( x = 0.3 ), linewidth = 0.2 ) )
  g <- g + stat_summary( size = 0.5, geom = "errorbar", width = 0.1, color = colorValues[1] )
  g <- g + stat_summary( size = dotSizeSum, color = colorValues[1] )
  g <- g + geom_hline( yintercept = 0, linetype = "dashed" )
  g <- g + facet_grid( cols =vars( {{gridVar}} ) )
  g <- g + cowplot::theme_cowplot()
  g <- g + ggplot2::scale_color_manual( values = colorValues )
  g <- g + scale_fill_manual( values = colorValues )
  g <- g + xlab( xlabText )
  g <- g + ylab( ylabText )
  g <- g + ylim( yMin, yMax )
  g <- g + theme( axis.title = element_text(size =fontSize ) )
  g <- g + theme( axis.text = element_text( size = fontSize ) )
  g <- g + theme( legend.position = "none" )
  return( g )
}


################################################################################
## prepare anova data for table printing
################################################################################
format_anova_table <- function( a ){
  n <- a$Anova$terms
  n <- n[ 2: length( n ) ]
  t <- a$anova_table
  t %<>% mutate( newDf = as.character( paste0( as.character( round( `num Df`, 2 ) ), ", ",
                                               as.character( round( `den Df`, 2 ) ) ) ) )
  t %<>% select( df = newDf, F, ges, p = `Pr(>F)` )
  t$F <- round( t$F, 2 )
  t[ 3:4 ]<- round( t[ 3:4 ], 4 )
  t$ges <- as.character( t$ges )
  t$p <- as.character( t$p )
  t[ t == "0" ] <- "<0.0001"
  t[ t == "1e-04" ] <- "0.0001"
  t <- bind_cols( n, t )
  names(t)[1] <- "effect"
  return( t )
}

################################################################################
## prepare ttest data for table printing
################################################################################
format_ttest_table <- function(...) {
  # Capture the t-test objects and their names
  tests <- list(...)
  test_names <- as.character(substitute(list(...)))[-1]  # Remove 'list' from names
#  print(test_names)
  
 
  # Initialize result data frame
  results <- data.frame(
    comparison = character(),
    diff = numeric(),
    `95% CI` = character(),
    t = numeric(),
    df = numeric(),
    `p-value` = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each t-test
  for (i in seq_along(tests)) {
    test <- tests[[i]]
    
    # Extract values from t-test object
    diff_val <- test$estimate
    
    
    # Format confidence interval
    ci_lower <- round(test$conf.int[1], 2)
    ci_upper <- round(test$conf.int[2], 2)
    ci_formatted <- paste0("[", ci_lower, ", ", ci_upper, "]")
    
    # Create row for this test
    row <- data.frame(
      comparison = test_names[i],
      diff = round(diff_val, 2),
      `95% CI` = ci_formatted,
      t = round(test$statistic, 2),
      df = test$parameter,
      `p-value` = round(test$p.value, 4),
      stringsAsFactors = FALSE
    )
    row[ row == "0" ] <- "<0.0001"
    row[ row == "1e-04" ] <- "0.0001"
    # Add to results
    results <- rbind(results, row)
  }
  
  # Fix column names (R changes them due to special characters)
  names(results) <- c("contrast", "diff", "95% CI", "t", "df", "p-value")
  
  return(results)
}

################################################################################
## format the to-be-exported anova table
################################################################################
style_table <- function( table ) {
  require( flextable )
  black_border = fp_border( color="black", width = 0.8 )
  flextable( table ) %>% 
    align( align = "left", part="all" ) %>% 
    valign( valign = "center", part="all" ) %>%   
    padding( padding = 0.5, part="all" ) %>% 
    line_spacing( space = 1, part="all" ) %>% 
    fontsize( size = 8, part="all" ) %>% 
    bold(bold = TRUE, part="header" ) %>% 
    border_remove() %>% 
    autofit() %>% 
    #add borders 
    hline_bottom( part="all", border = black_border )
}