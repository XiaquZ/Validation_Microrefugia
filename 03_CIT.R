# ---- 6. CIT CALCULATION  ----

#Calculate CIT per plot sample by taking the weighted mean (abundance as weights) 
#of the respective mean temperature of the species present in that plot
  
#CIT based on the maximum temperature during the growing season  
  CIT_maxTempGS <- vegdata_mean_maxTempGS %>% group_by(sample) %>% summarise(  
    CIT_maxTempGS = weighted.mean(mean_maxTempGS,abundance))
  
#CIT based on the minimum temperature during spring 
  CIT_minTempSpring <- vegdata_mean_minTempSpring %>% group_by(sample) %>% summarise(  
    CIT_minTempSpring = weighted.mean(mean_minTempSpring,abundance))
  
#CIT based on the minimum temperature during winter  
  CIT_minTempWinter <- vegdata_mean_minTempWinter %>% group_by(sample) %>% summarise(  
    CIT_minTempWinter = weighted.mean(mean_minTempWinter,abundance)) 
  
  
#The CIT data will be converted to a wide data format (now in long format) based on the survey type (baseline survey B vs resurvey R1 to R5) 
#in the following steps for the three different temperatures
  
# For the maximum temperature during the growing season
  
#First create a new column with the plot ID (removing the suffix of the baseline survey and resurvey to obtain plot ID)
  CIT_maxTempGS$plotID <- stri_replace_all_regex(CIT_maxTempGS$sample,
                                            pattern=c('_B', '_R1', '_R2','_R3','_R4','_R5'),
                                            replacement=c('','','','','',''),
                                            vectorize=FALSE)

#Extract the survey type (baseline survey or resurvey 1 to 5) of the plot sample with a nested ifelse statement 
#(e.g. if plot sample contains _R1 then give value R1 in new column)
  CIT_maxTempGS$survey_type <- ifelse(grepl("_B",CIT_maxTempGS$sample),'B',
                                ifelse(grepl("_R1",CIT_maxTempGS$sample), 'R1',
                                  ifelse(grepl("_R2",CIT_maxTempGS$sample), 'R2',
                                   ifelse(grepl("_R3",CIT_maxTempGS$sample), 'R3',
                                     ifelse(grepl("_R4",CIT_maxTempGS$sample), 'R4', 'R5' )))))

#Plot sample column not needed anymore (since this is now split between plotID and survey type), so exclude it 
  CIT_maxTempGS <- CIT_maxTempGS[,-1]

#Change the order of the columns to a more logical order
  col_order <- c("plotID","survey_type","CIT_maxTempGS")
  CIT_maxTempGS <- CIT_maxTempGS[,col_order]

#The CIT data is now in long format, so first it needs to be converted to a wide format data frame 
#to separate the baseline surveys and resurveys into separate columns, which makes it easier for later calculations
  CIT_maxTempGS <- spread(CIT_maxTempGS,survey_type,CIT_maxTempGS)

#Order plotIDs numerically (same order as plot data, more logical to view/find data)
  CIT_maxTempGS <- CIT_maxTempGS[str_order(CIT_maxTempGS$plotID, numeric = T),] 
  
#Give columns more clear names  
  colnames(CIT_maxTempGS) <- c("plotID","B_CIT_maxTempGS","R1_CIT_maxTempGS",
                    "R2_CIT_maxTempGS","R3_CIT_maxTempGS","R4_CIT_maxTempGS","R5_CIT_maxTempGS")