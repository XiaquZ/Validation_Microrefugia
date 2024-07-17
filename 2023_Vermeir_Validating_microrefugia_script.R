
  ## Script of thesis : 
  ## Validating microrefugia in European forests 
  ## by Vermeir Zander 

# ---- 1. LOAD PACKAGES ----

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(stringi)

#The easyclimate package is located on a Github repository, extract it from there with the remote package
library(remotes)
remotes::install_github("VeruGHub/easyclimate")
library(easyclimate) #Used to extract the macroclimate data from the downscaled E-OBS database
library(lubridate)
library(raster)
library(nlme)  #Used for the regression (part 9)
library(ape)  #Used to test spatial autocorrelation (part 9)
library(performance) #Used to check for multicollinearity (part 9)
library(lme4) #To test the difference in delta CIT between the processes (part 8)
library(emmeans) #To perform a post hoc Tukey test (part 8)

# ---- 2. LOAD DATA ----

## Set working directory to where data is saved
wd <- setwd("D:/2e Master/Thesis/Analyses/Data")

## ClimPlant (newest update: 24/01/2023)

#Maximum temperature during growing season (April-September) at 1000 random locations for all 1168 species
  maxTempGS <- read.csv("MaxTempGrowingSeason.csv")
#Monthly minimum temperatures during spring (March to May) at 1000 random locations for all 1168 species
  minTempMarch <- read.csv("MinTempMar.csv")
  minTempApril <- read.csv("MinTempApr.csv")
  minTempMay <- read.csv("MinTempMay.csv")
#Monthly minimum temperatures during winter (December to February) at 1000 random locations for all 1168 species
  minTempDecember <- read.csv("MinTempDec.csv")
  minTempJanuary <- read.csv("MinTempJan.csv")
  minTempFebruary <- read.csv("MinTempFeb.csv")

## ForestREplot 
  
#Plot data of all plots in ForestREplot database 
  load("plot_data.RData")
#Vegetation data of all plots
  load("veg_data.RData")
  
## MI data

#MIs based on three climate offsets (mean annual temperature, minimmum temperature during spring and winter and maximum temperature during summer)
#and synthesis MIs (mean of the three MIs)
  load("ClimateOffset_MIs_XiquZ_V3.RData")
  MI_data_Climate_Offset_magnitude <- coordinates_plots
  
# ---- 3. FILTER AND CLEAN DATA ----  

#Select plots and vegetation data from Europe (and excluding that of North America)  
  vegdata_ForestREplot_EU <- veg_data[grep("EU",veg_data$sample),]
  plotdata_ForestREplot_EU <- plot_data[grep("EU",plot_data$plotID),]
  
#Select the herb layer (for the further analysis) and tree layer (for the calculation of forest canopy changes later on) of the plots   
  vegdata_ForestREplot_herblayer <- filter(vegdata_ForestREplot_EU, layer =="H") #herb layer (H)
  vegdata_ForestREplot_treelayer <- filter(vegdata_ForestREplot_EU, layer =="T") #tree layer (T)
  
#Exclude missing values (values of 9999)
  vegdata_ForestREplot_herblayer <- filter(vegdata_ForestREplot_herblayer, abundance != 9999)
  vegdata_ForestREplot_treelayer <- filter(vegdata_ForestREplot_treelayer, abundance != 9999)

#Check for abnormal total cover of herb layer per plot sample by grouping per plot sample and calculating the sum per plot sample
  total_cover_herblayer <- vegdata_ForestREplot_herblayer %>% group_by(sample) %>% summarise(total_abundance=sum(abundance))
  total_cover_treelayer <- vegdata_ForestREplot_treelayer %>% group_by(sample) %>% summarise(total_cover=sum(abundance))
  
#Add the total cover per plot to the vegetation data
#This is only needed for the herb layer, since we in the tree layer we only work with the total cover while in the herb layer we work with individual abundances
  vegdata_ForestREplot_herblayer <- left_join(vegdata_ForestREplot_herblayer,total_cover_herblayer,by="sample")
  
#Plots with a total cover up to 300% were considered realistic, plots higher than that were filtered
  vegdata_ForestREplot_herblayer <- filter(vegdata_ForestREplot_herblayer, total_abundance <= 300)
  total_cover_treelayer <- filter(total_cover_treelayer, total_cover <= 300)
  
  
# ---- 4. EXTRACTING RELEVANT TEMPERATURE INFORMATION PER SPECIES ----
  
#Calculate the mean temperature per species for the maximum temperature during the growing season
  mean_maxTempGS <- data.frame(maxTempGS[,1],rowMeans(maxTempGS[,2:1001]))
  colnames(mean_maxTempGS) <- c("species_name","mean_maxTempGS")
  
#Calculate the minimum temperature during spring (March to May) for the 1000 sampling points 
#by calculating the average minimum temperature for these months
  minTempSpring <- data.frame(minTempMarch[,1],(minTempMarch[,2:1001]+minTempApril[,2:1001]+minTempMay[,2:1001])/3)
  
#Calculate the mean temperature per species for the minimum temperature during spring
  mean_minTempSpring <- data.frame(minTempSpring[,1],rowMeans(minTempSpring[,2:1001]))
  colnames(mean_minTempSpring) <- c("species_name","mean_minTempSpring")
  
#Calculate the minimum temperature during winter (December to February) for the 1000 sampling points 
#by calculating the average minimum temperature for these months
  minTempWinter <- data.frame(minTempDecember[,1],(minTempDecember[,2:1001]+minTempJanuary[,2:1001]+minTempFebruary[,2:1001])/3)
  
#Calculate the mean temperature per species for the minimum temperature during winter
  mean_minTempWinter <- data.frame(minTempWinter[,1],rowMeans(minTempWinter[,2:1001]))
  colnames(mean_minTempWinter) <- c("species_name","mean_minTempWinter")

#Join the respective mean temperature per species with the vegetation data of the species, right join here (join based on ClimPlant) 
#because we are interested in the species present in the ClimPlant database (no temperature data for species only present in forestREplot)
#there are 1168 understory species in ClimPlant and 1410 in forestREplot, so ClimPlant covers 82,8% of forestREplot
  
#For the maximum temperature during growing season  
  vegdata_mean_maxTempGS <- right_join(vegdata_ForestREplot_herblayer,mean_maxTempGS, by = "species_name")
#For the minimum temperature during spring  
  vegdata_mean_minTempSpring <- right_join(vegdata_ForestREplot_herblayer,mean_minTempSpring, by = "species_name")
#For the minimum temperature during winter  
  vegdata_mean_minTempWinter <- right_join(vegdata_ForestREplot_herblayer,mean_minTempWinter, by = "species_name")
  
#Displaying species response curves for illustration (one example here for minimum temperature during spring)
  plot_minTempSpring <- data.frame(t(minTempSpring[,2:1001])) #ggplot takes columns so invert dataframe first

#Example for the species response curve of Abies alba for the minimum temperature during spring
  ggplot(plot_minTempSpring, aes(plot_minTempSpring[,1])) + geom_density(color='black',lwd=1.1, fill='darkgreen', alpha=0.4) + 
    theme_bw() + 
    ylab("Frequency of occurence") + xlab("Minimum temperature during spring (°C)") + ggtitle("Abies alba") +
    theme(plot.title = element_text(hjust=0,size = 20)) + 
    geom_vline(aes(xintercept = mean(plot_minTempSpring[,1])), linetype = "dashed", linewidth = 0.75) #adding mean as vertical dashed line


# ---- 5. EXTRACTING AND PREPARING OTHER RELEVANT VARIABLES ----

#Other variables that are later used in the regression will be extracted and prepared here. This is done before the CIT calculation,
#because these variables can be added to the plot data (data per plot), while the CIT is dependent on which temperature is used,
#meaning that I only need to extract these other variables once instead of per temperature (three times)

## Canopy cover change (%)

#The canopy cover will be determined by the total cover percentage of the tree layer from the ForestREplot database
#It is easier to calculate when the total tree layer cover data is in wide format (now in long format), so this will be done in the following steps first

#Create a new column with the plot ID (removing the suffix of the baseling survey and resurvey to obtain plot ID)
  total_cover_treelayer$plotID <- stri_replace_all_regex(total_cover_treelayer$sample,
                                                       pattern=c('_B', '_R1', '_R2','_R3','_R4','_R5'),
                                                       replacement=c('','','','','',''),
                                                       vectorize=FALSE)

#Extract the survey type (baseline survey or resurvey 1 to 5) of the plot sample with a nested ifelse statement 
#(e.g. if plot sample contains _R1 then give value R1 in new column)
  total_cover_treelayer$survey_type <- ifelse(grepl("_B",total_cover_treelayer$sample),'B',
                                            ifelse(grepl("_R1",total_cover_treelayer$sample), 'R1',
                                                   ifelse(grepl("_R2",total_cover_treelayer$sample), 'R2',
                                                          ifelse(grepl("_R3",total_cover_treelayer$sample), 'R3',
                                                                 ifelse(grepl("_R4",total_cover_treelayer$sample), 'R4', 'R5' )))))

#Plot sample column not needed anymore (since this is now split between plotID and survey type), so exclude it 
  total_cover_treelayer <- total_cover_treelayer[,-1]

#Change the order of the columns to a more logical order
  col_order <- c("plotID","survey_type","total_cover")
  total_cover_treelayer <- total_cover_treelayer[,col_order]

#The total tree layer coverage per plot sample is now in long format, so first it needs to be converted to a wide format data frame 
#to separate the baseline surveys and resurveys into separate columns, which makes it easier for later calculations
  total_cover_treelayer <- spread(total_cover_treelayer,survey_type,total_cover)

#Order plotIDs numerically (more logical to view/find data) 
  total_cover_treelayer <- total_cover_treelayer[str_order(total_cover_treelayer$plotID, numeric = T),] 

#Give columns more clear names  
  colnames(total_cover_treelayer) <- c("plotID","B_cover_treelayer","R1_cover_treelayer",
                                       "R2_cover_treelayer","R3_cover_treelayer","R4_cover_treelayer","R5_cover_treelayer")
  
#Join the tree layer data with the other plot data based on the plotID
#Right join here (join based on other plot data) so that all plots are still present because some 
#plots have no tree layer data available
  plotdata_ForestREplot_EU <- right_join(total_cover_treelayer,plotdata_ForestREplot_EU, by = "plotID")

#Calculate the forest canopy changes by subtracting the tree layer coverage of the latest resurvey 
#with the tree layer coverage of the baseline survey using a nested ifelse statement. 
#Explanation code: Starting from resurvey 5, if there is a cover value for this resurvey then subtract this by cover value from baseline survey,
#if not (else), look if there is a cover value for resurvey 4, if there is then subtract this cover value by cover value from baseline survey,
#if not, move on to resurvey 3 and continue to do so until all plots have a forest canopy change value
#This way, it is guaranteed that the change between latest resurvey and the baseline survey is calculated 
  plotdata_ForestREplot_EU$canopy_cover_changes <- ifelse(is.na(plotdata_ForestREplot_EU$R5_cover_treelayer) == F,
                                                      plotdata_ForestREplot_EU$R5_cover_treelayer-plotdata_ForestREplot_EU$B_cover_treelayer,
                                                      ifelse(is.na(plotdata_ForestREplot_EU$R4_cover_treelayer) == F,
                                                             plotdata_ForestREplot_EU$R4_cover_treelayer-plotdata_ForestREplot_EU$B_cover_treelayer,
                                                             ifelse(is.na(plotdata_ForestREplot_EU$R3_cover_treelayer) == F,
                                                                    plotdata_ForestREplot_EU$R3_cover_treelayer-plotdata_ForestREplot_EU$B_cover_treelayer,
                                                                    ifelse(is.na(plotdata_ForestREplot_EU$R2_cover_treelayer) == F,
                                                                           plotdata_ForestREplot_EU$R2_cover_treelayer-plotdata_ForestREplot_EU$B_cover_treelayer,
                                                                           plotdata_ForestREplot_EU$R1_cover_treelayer-plotdata_ForestREplot_EU$B_cover_treelayer))))



#Visualise the canopy cover changes with a density plot
  ggplot(plotdata_ForestREplot_EU, aes(canopy_cover_changes)) + geom_density(color='black',lwd=1.1, fill='darkgreen', alpha=0.4) + 
    theme_bw() + xlab("canopy cover changes (%)") + theme(plot.title = element_text(hjust=0,size = 20)) +
    geom_vline(aes(xintercept = mean(canopy_cover_changes,na.rm = T)), linetype = "dashed", linewidth = 0.6)


## Macroclimate change (°C/year)

#The baseline survey for 284 plots was performed before 1950, so for these plots the macroclimate data from 1950 was used

#The next few steps take very long to process so these were already done on the remote desktop of the division and saved to a csv file. 
#The resulting data frame with the yearly mean temperature from 1950 to 2020 for all plots can therefore just be loaded. This file can be found on 
#my Github repository (https://github.com/ZanderVermeir/script_thesis/blob/9c2f43075cdf42cdbff10c643862a3a8a924f6b1/yearly_mean_temp_1950_2020.csv) as a csv file with the name yearly_mean_temp_1950_2020
  yearly_mean_temp_1950_2020 <- read.csv("yearly_mean_temp_1950_2020.csv")
  
  
#NOT NECESSARY TO RUN (only needs to be done once, here for illustration how it was done) -> skip to line 247

#Extract the coordinates of the plots and give appropriate column names for easyclimate    
  coordinates_plots <- data.frame(plotdata_ForestREplot_EU$plotID,plotdata_ForestREplot_EU$longitude,plotdata_ForestREplot_EU$latitude)
  colnames(coordinates_plots) <- c("plotID","lon","lat")
  
#Extract the daily minimum and maximum temperature from the coordinates of all the plots for the time period 1950 to 2020
#This was done with the get_daily_climate function from the easyclimate package  
#Time intervals of ten years were used here because larger time intervals tend to give errors
  daily_max_min_temp_1950_1960 <- get_daily_climate(coordinates_plots,period="1950-01-01:1960-12-31",climatic_var = c("Tmax","Tmin"))
  daily_max_min_temp_1961_1970 <- get_daily_climate(coordinates_plots,period="1961-01-01:1970-12-31",climatic_var = c("Tmax","Tmin"))
  daily_max_min_temp_1971_1980 <- get_daily_climate(coordinates_plots,period="1971-01-01:1980-12-31",climatic_var = c("Tmax","Tmin"))
  daily_max_min_temp_1981_1990 <- get_daily_climate(coordinates_plots,period="1981-01-01:1990-12-31",climatic_var = c("Tmax","Tmin"))
  daily_max_min_temp_1991_2000 <- get_daily_climate(coordinates_plots,period="1991-01-01:2000-12-31",climatic_var = c("Tmax","Tmin"))
  daily_max_min_temp_2001_2010 <- get_daily_climate(coordinates_plots,period="2001-01-01:2010-12-31",climatic_var = c("Tmax","Tmin"))
  daily_max_min_temp_2011_2020 <- get_daily_climate(coordinates_plots,period="2011-01-01:2020-12-31",climatic_var = c("Tmax","Tmin"))
  
#Combine all time intervals into a single data frame so that there is one data frame containing daily data from 1950 to 2020
  daily_max_min_temp_1950_2020 <- rbind(daily_max_min_temp_1950_1960,daily_max_min_temp_1961_1970,daily_max_min_temp_1971_1980,
                                        daily_max_min_temp_1981_1990,daily_max_min_temp_1991_2000,daily_max_min_temp_2001_2010,
                                        daily_max_min_temp_2011_2020)
  
#Calculate the daily mean temperature as the average of the daily maximum and minimum temperatures
  daily_max_min_mean_temp_1950_2020 <- mutate(daily_max_min_temp_1950_2020,Tmean=(Tmax+Tmin)/2)
  
#Extract the year from the date for the following step
  daily_max_min_mean_temp_1950_2020 <- mutate(daily_max_min_mean_temp_1950_2020,year=lubridate::year(date)) 
  
#Group the daily data per plot and year to calculate the yearly average for the daily mean temperature
  yearly_mean_temp_1950_2020 <- daily_max_min_mean_temp_1950_2020 %>% group_by(plotID,year) %>% 
    summarise(Tmean_year=mean(Tmean,na.rm=T))
  
#Save this data frame as a csv file, so that previous steps (which take very long to process) only have to be executed once
  write.csv(yearly_mean_temp_1950_2020,"yearly_mean_temp_1950_2020.csv",row.names = F)
  
  
#RUN AGAIN FROM HERE
  
  
#Join the yearly macroclimate data with the plot data based on the plotID to link the year of the macroclimate temperature to the year of the survey
  plotdata_ForestREplot_macrotemp <- left_join(yearly_mean_temp_1950_2020,plotdata_ForestREplot_EU,by="plotID")
  
#Check if the year of the baseline survey corresponds wit the year of the yearly macroclimate data with an ifelse statement
#if this is the case, then return the yearly mean macroclimate temperature for that corresponding year, if not, check if baseline survey year is before 1950
#if this is the case, return the yearly mean macroclimate temperature for 1950, if not, return NA
  plotdata_ForestREplot_macrotemp$mean_macrotemp_B <- ifelse(plotdata_ForestREplot_macrotemp$year==plotdata_ForestREplot_macrotemp$year_baseline_survey,
                                                             plotdata_ForestREplot_macrotemp$Tmean_year,
                                                             ifelse(plotdata_ForestREplot_macrotemp$year_baseline_survey<1950 & 
                                                                      plotdata_ForestREplot_macrotemp$year==1950,
                                                                      plotdata_ForestREplot_macrotemp$Tmean_year,NA))
  
#The same but for the first resurvey to the fifth resurvey. There is no need here to check for plots before 1950, because the earliest resurvey year is 1969
  plotdata_ForestREplot_macrotemp$mean_macrotemp_R1 <- ifelse(plotdata_ForestREplot_macrotemp$year==plotdata_ForestREplot_macrotemp$year_resurvey_R1,
                                                              plotdata_ForestREplot_macrotemp$Tmean_year,NA)
  plotdata_ForestREplot_macrotemp$mean_macrotemp_R2 <- ifelse(plotdata_ForestREplot_macrotemp$year==plotdata_ForestREplot_macrotemp$year_resurvey_R2,
                                                              plotdata_ForestREplot_macrotemp$Tmean_year,NA)
  plotdata_ForestREplot_macrotemp$mean_macrotemp_R3 <- ifelse(plotdata_ForestREplot_macrotemp$year==plotdata_ForestREplot_macrotemp$year_resurvey_R3,
                                                              plotdata_ForestREplot_macrotemp$Tmean_year,NA)
  plotdata_ForestREplot_macrotemp$mean_macrotemp_R4 <- ifelse(plotdata_ForestREplot_macrotemp$year==plotdata_ForestREplot_macrotemp$year_resurvey_R4,
                                                              plotdata_ForestREplot_macrotemp$Tmean_year,NA)
  plotdata_ForestREplot_macrotemp$mean_macrotemp_R5 <- ifelse(plotdata_ForestREplot_macrotemp$year==plotdata_ForestREplot_macrotemp$year_resurvey_R5,
                                                              plotdata_ForestREplot_macrotemp$Tmean_year,NA)
  
#Extract the macroclimate temperature for each survey per plot by taking the average per plot (plotID as grouping factor) and excluding the NAs
#since there is only 1 temperature value for each survey per plot
  plotdata_ForestREplot_macrotemp <- plotdata_ForestREplot_macrotemp %>% group_by(plotID) %>% 
    summarise(mean_macrotemp_B=mean(mean_macrotemp_B,na.rm=T),
              mean_macrotemp_R1=mean(mean_macrotemp_R1,na.rm=T),
              mean_macrotemp_R2=mean(mean_macrotemp_R2,na.rm=T),
              mean_macrotemp_R3=mean(mean_macrotemp_R3,na.rm=T),
              mean_macrotemp_R4=mean(mean_macrotemp_R4,na.rm=T),
              mean_macrotemp_R5=mean(mean_macrotemp_R5,na.rm=T))
  
#Join the plot data with the macroclimate temeprature data (joined based on plot data) to have everything in 1 data frame for easier further calculations
  plotdata_ForestREplot_EU <- left_join(plotdata_ForestREplot_EU,plotdata_ForestREplot_macrotemp,by="plotID")
  
#Subtract the yearly mean macroclimate temperature of the latest resurvey from that of the baseline survey to calculate the macroclimate change
#This is done similarly to the canopy cover changes with an nested ifelse statement (except the NA here are NaN)
  plotdata_ForestREplot_EU$macroclimate_change <- ifelse(is.nan(plotdata_ForestREplot_EU$mean_macrotemp_R5) == F,
                                                      plotdata_ForestREplot_EU$mean_macrotemp_R5-plotdata_ForestREplot_EU$mean_macrotemp_B,
                                                      ifelse(is.nan(plotdata_ForestREplot_EU$mean_macrotemp_R4) == F,
                                                            plotdata_ForestREplot_EU$mean_macrotemp_R4-plotdata_ForestREplot_EU$mean_macrotemp_B,
                                                            ifelse(is.nan(plotdata_ForestREplot_EU$mean_macrotemp_R3) == F,
                                                                   plotdata_ForestREplot_EU$mean_macrotemp_R3-plotdata_ForestREplot_EU$mean_macrotemp_B,
                                                                   ifelse(is.nan(plotdata_ForestREplot_EU$mean_macrotemp_R2) == F,
                                                                          plotdata_ForestREplot_EU$mean_macrotemp_R2-plotdata_ForestREplot_EU$mean_macrotemp_B,
                                                                          plotdata_ForestREplot_EU$mean_macrotemp_R1-plotdata_ForestREplot_EU$mean_macrotemp_B))))
  
#We want to divide this by the time period between the latest resurvey and the baseline survey
#Therefore, we subtract the year of the latest resurvey from the year of the baseline survey
#However, not all survey years are numerical (not all single years) so we cannot use them in calculations (otherwise gives NA when column is converted to numerical)
#Therefore first replace all baseline survey years and resurvey years that are not just one year to a single year 

#Baseline survey
  plotdata_ForestREplot_EU$year_baseline_survey <- stri_replace_all_regex(plotdata_ForestREplot_EU$year_baseline_survey,
                                                                          pattern=c('1976 \\(T, S 1975\\)','1956-57','1955 and 57','\\?','1998/99'),
                                                                          replacement=c('1976','1956','1956','','1998'),
                                                                          vectorize=FALSE)
#Resurvey 1
  plotdata_ForestREplot_EU$year_resurvey_R1 <- sub('2019-2020','2019',plotdata_ForestREplot_EU$year_resurvey_R1)
  
#Resurvey 2
  plotdata_ForestREplot_EU$year_resurvey_R2 <- sub('1976 \\(T, S 1975\\)','1976',plotdata_ForestREplot_EU$year_resurvey_R2)
  
#For resurvey 3 to 5 not needed
  
#Convert the survey years columns to numerical so that they can be used in further calculations
  plotdata_ForestREplot_EU$year_baseline_survey <- as.numeric(plotdata_ForestREplot_EU$year_baseline_survey)
  plotdata_ForestREplot_EU$year_resurvey_R1 <- as.numeric(plotdata_ForestREplot_EU$year_resurvey_R1)
  plotdata_ForestREplot_EU$year_resurvey_R2 <- as.numeric(plotdata_ForestREplot_EU$year_resurvey_R2)

#Calculate the time period between the baseline survey and the latest resurvey with a nested ifelse statement
#Again the same principle is followed as above, only now the year from the latest resurvey is subtracted from the year of the baseline survey  
  plotdata_ForestREplot_EU$time_period <- ifelse(is.na(plotdata_ForestREplot_EU$year_resurvey_R5) == F,
                                              plotdata_ForestREplot_EU$year_resurvey_R5-plotdata_ForestREplot_EU$year_baseline_survey,
                                              ifelse(is.na(plotdata_ForestREplot_EU$year_resurvey_R4) == F,
                                                   plotdata_ForestREplot_EU$year_resurvey_R4-plotdata_ForestREplot_EU$year_baseline_survey,
                                                   ifelse(is.na(plotdata_ForestREplot_EU$year_resurvey_R3) == F,
                                                          plotdata_ForestREplot_EU$year_resurvey_R3-plotdata_ForestREplot_EU$year_baseline_survey,
                                                          ifelse(is.na(plotdata_ForestREplot_EU$year_resurvey_R2) == F,
                                                                 plotdata_ForestREplot_EU$year_resurvey_R2-plotdata_ForestREplot_EU$year_baseline_survey,
                                                                 plotdata_ForestREplot_EU$year_resurvey_R1-plotdata_ForestREplot_EU$year_baseline_survey))))  

#Calculate the macroclimate change per year for each plot to allow for comparison between plots by dividing the macroclimate change by 
#the time period between the latest resurvey and the baseline survey 
  plotdata_ForestREplot_EU$macroclimate_change_per_year <- plotdata_ForestREplot_EU$macroclimate_change/plotdata_ForestREplot_EU$time_period
 
#Visualise the macroclimate change per year with a density plot
  ggplot(plotdata_ForestREplot_EU, aes(macroclimate_change_per_year)) + geom_density(color='black',lwd=1.1, fill='darkgreen', alpha=0.4) + 
    theme_bw() + xlab("macroclimate change (°C/year)") + theme(plot.title = element_text(hjust=0,size = 20)) +
    geom_vline(aes(xintercept = mean(macroclimate_change_per_year,na.rm = T)), linetype = "dashed", linewidth = 0.6)
  
  

## MI data (range between 0 and 1)

#Exclude coordinates from MI data, not needed anymore (already available with rest of plot data)  
MI_data_Climate_Offset_magnitude <- MI_data_Climate_Offset_magnitude[,c(1,4:8)]
    
#Join the MI data with the rest of the plot data
plotdata_ForestREplot_EU <- left_join(plotdata_ForestREplot_EU,MI_data_Climate_Offset_magnitude,by="plotID")



## Atmospheric nitrogen deposition (kg/ha/year)

#We want to obtain the mean nitrogen deposition rate for each plot. This is done in different steps

#First extract the dry and wet deposition data of oxidized and reduced nitrogen from the EMEP database for the year 2000

#Extract the coordinates of the plots as a matrix so they can be used in raster extraction
coordinates_plots_matrix <- as.matrix(plotdata_ForestREplot_EU[,c("longitude","latitude")])

#Dry deposition of oxidized nitrogen
dry_deposition_Nox <- raster("EMEP01_rv4.45_year.2000met_2000emis_rep2022.nc",var="DDEP_OXN_m2Grid")
dry_deposition_Nox <- raster::extract(dry_deposition_Nox,coordinates_plots_matrix,method='simple')
dry_deposition_Nox <- data.frame(cbind(plotdata_ForestREplot_EU[,c("plotID","longitude","latitude")],dry_deposition_Nox))

#Dry deposition of reduced nitrogen
dry_deposition_Nred <- raster("EMEP01_rv4.45_year.2000met_2000emis_rep2022.nc",var="DDEP_RDN_m2Grid")
dry_deposition_Nred <- raster::extract(dry_deposition_Nred,coordinates_plots_matrix,method='simple')
dry_deposition_Nred <- data.frame(cbind(plotdata_ForestREplot_EU[,c("plotID","longitude","latitude")],dry_deposition_Nred))

#Wet deposition of oxidized nitrogen
wet_deposition_Nox <- raster("EMEP01_rv4.45_year.2000met_2000emis_rep2022.nc",var="WDEP_OXN")
wet_deposition_Nox <- raster::extract(wet_deposition_Nox,coordinates_plots_matrix,method='simple')
wet_deposition_Nox <- data.frame(cbind(plotdata_ForestREplot_EU[,c("plotID","longitude","latitude")],wet_deposition_Nox))

#Wet deposition of reduced nitrogen
wet_deposition_Nred <- raster("EMEP01_rv4.45_year.2000met_2000emis_rep2022.nc",var="WDEP_RDN")
wet_deposition_Nred <- raster::extract(wet_deposition_Nred,coordinates_plots_matrix,method='simple')
wet_deposition_Nred <- data.frame(cbind(plotdata_ForestREplot_EU[,c("plotID","longitude","latitude")],wet_deposition_Nred))

#Join these four variables together so it is easier for further calculations
dry_deposition <- full_join(dry_deposition_Nox,dry_deposition_Nred,by=c("plotID","longitude","latitude"))
wet_deposition <- full_join(wet_deposition_Nox,wet_deposition_Nred,by=c("plotID","longitude","latitude"))
deposition_N2000 <- full_join(dry_deposition,wet_deposition,by=c("plotID","longitude","latitude")) 
  
#Sum these four variables to obtain the total nitrogen deposition in the plots for the year 2000 (N2000)
#and divide it by 100 to convert it from mg/m2 to kg/ha (10000/1000000)
deposition_N2000$N2000 <- (deposition_N2000$dry_deposition_Nox+deposition_N2000$dry_deposition_Nred+
                             deposition_N2000$wet_deposition_Nox+deposition_N2000$wet_deposition_Nred)/100
 
 
#Next, calculate the cumulative N deposition by summing the N2000 value per year while multiplying with the according correction factor per decade (Duprè et al., 2010)
#E.g.: baseline survey in 1939 and resurvey in 1973 -> Ncum = N2000*(0.1*11 years) + N2000*(0.5*10 years) + N2000*(0.9*10 years) + N2000*(1.3*4 years)

#Correction factors for the different decades (starting from earliest baseline survey to most recent resurvey)
corr_factors_Ndep_decade <- data.frame(0.1,0.1,0.5,0.9,1.3,1.1,1,1,1)
colnames(corr_factors_Ndep_decade) <- c("1933-1939","1940-1949","1950-1959","1960-1969","1970-1979","1980-1989","1990-1999","2000-2009","2010-2020")

#convert this to correction factors per year for further calculations  
corr_factors_Ndep_year <- data.frame(c(1933:2020),c(rep(0.1,17),rep(0.5,10),rep(0.9,10),rep(1.3,10),rep(1.1,10),rep(1,31)))
colnames(corr_factors_Ndep_year) <- c("year","correction_factor")

#Extract the survey years per plot from the plot data
survey_years <- plotdata_ForestREplot_EU[,c("plotID","year_baseline_survey","year_resurvey_R1","year_resurvey_R2",
                                            "year_resurvey_R3","year_resurvey_R4","year_resurvey_R5")]

#Extract the latest resurvey per plot with a nested ifelse statement (same logic as canopy cover changes, line 175)
survey_years$latest_resurvey <- ifelse(is.na(survey_years$year_resurvey_R5) == F,
                                       survey_years$year_resurvey_R5,
                                       ifelse(is.na(survey_years$year_resurvey_R4) == F,
                                              survey_years$year_resurvey_R4,
                                              ifelse(is.na(survey_years$year_resurvey_R3) == F,
                                                     survey_years$year_resurvey_R3,
                                                     ifelse(is.na(survey_years$year_resurvey_R2) == F,
                                                            survey_years$year_resurvey_R2,
                                                            survey_years$year_resurvey_R1)))) 

#Only keep survey years of baseline survey and latest resurvey 
survey_years <- survey_years[,c("plotID","year_baseline_survey","latest_resurvey")]  

#Now we want all intermediate years between the baseline survey and the resurvey per plot so that the correction factors per year can be linked to the plots
#This is done in different steps

#First join the full range of years of the surveys (1933-2020) to each plotID 

#Extract the plotIDs and repeat each plotID 88 times (number of years)
plotIDs <- plotdata_ForestREplot_EU[,"plotID"]
plotIDs <- plotIDs[rep(seq_len(nrow(plotIDs)), each = 88), ]

#Repeat the range of years 4389 times (number of plots)
range_survey_years <- data.frame(rep(c(1933:2020),4389))
colnames(range_survey_years) <- "year"

#Bind the full range of survey years and plotIDs
range_survey_years <- cbind(plotIDs,range_survey_years)

#Put in the same order as the survey_years data frame
range_survey_years <- range_survey_years[str_order(range_survey_years$plotID, numeric = T),] 

#Then join this full range of survey years with the baseline survey and resurvey years based on plotID
survey_years <- left_join(range_survey_years,survey_years,by="plotID")

#Finally, extract all intermediate years between the baseline survey and resurvey (including the baseline and resurvey year) with an ifelse statement
survey_years$survey_year <- ifelse(survey_years$year >= survey_years$year_baseline_survey & 
                                     survey_years$year <= survey_years$latest_resurvey,survey_years$year, NA)

#Exclude NAs so that only the years between the baseline survey and resurvey remain and exclude the redundant year column
survey_years <- na.omit(survey_years)
survey_years <- survey_years[,-2]

#Join the correction factors to the years between surveys
survey_years_corr <- left_join(survey_years,corr_factors_Ndep_year,by=c("survey_year"="year"))
            
#Join this information with the N2000 values based on the plotID
deposition_N <- left_join(survey_years_corr,deposition_N2000,by="plotID")

#Calculate the cumulative N deposition per plot by summing the multiplication of the N2000 value with the correction factor per plot
cum_deposition_N <- deposition_N %>% group_by(plotID) %>% summarise(N_cum=sum(correction_factor*N2000))

#Join the cumulative N deposition with rest of the plot data
plotdata_ForestREplot_EU <- left_join(plotdata_ForestREplot_EU,cum_deposition_N,by="plotID")

#Finally divide the cumulative N deposition per plot by the time period between the baseline survey and the resurvey of each plot
#to obtain the mean nitrogen deposition rate of each plot
plotdata_ForestREplot_EU$N_mean <- plotdata_ForestREplot_EU$N_cum/plotdata_ForestREplot_EU$time_period

#Visualise the mean N deposition per plot with a density plot
ggplot(plotdata_ForestREplot_EU, aes(N_mean)) + geom_density(color='black',lwd=1.1, fill='darkgreen', alpha=0.4) + 
  theme_bw() + xlab("N deposition") + theme(plot.title = element_text(hjust=0,size = 20)) +
  geom_vline(aes(xintercept = mean(N_mean,na.rm = T)), linetype = "dashed", linewidth = 0.6)



## Plot size

#The plot size will also be included in the regression later so we want to change it to a numerical vector (now it is a character)
#Some values have a extra description so change this to 1 value for the plot size (e.g. EU_009a)
plotdata_ForestREplot_EU$plot_size <- stri_replace_all_regex(plotdata_ForestREplot_EU$plot_size,
                                                             pattern=c('1 \\(16 for T1, T2\\)','7 x 7','20x20m','15x15','10x3','20x20','10x10'),
                                                             replacement=c('1','49','400','225','30','400','100'),
                                                             vectorize=FALSE)

plotdata_ForestREplot_EU$plot_size <- as.numeric(plotdata_ForestREplot_EU$plot_size)

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

# For the minimum temperature during spring  
  
#First create a new column with the plot ID (removing the suffix of the baseline survey and resurvey to obtain plot ID)
  CIT_minTempSpring$plotID <- stri_replace_all_regex(CIT_minTempSpring$sample,
                                                 pattern=c('_B', '_R1', '_R2','_R3','_R4','_R5'),
                                                 replacement=c('','','','','',''),
                                                 vectorize=FALSE)
  
#Extract the survey type (baseline survey or resurvey 1 to 5) of the plot sample with a nested ifelse statement 
#(e.g. if plot sample contains _R1 then give value R1 in new column)
  CIT_minTempSpring$survey_type <- ifelse(grepl("_B",CIT_minTempSpring$sample),'B',
                                      ifelse(grepl("_R1",CIT_minTempSpring$sample), 'R1',
                                             ifelse(grepl("_R2",CIT_minTempSpring$sample), 'R2',
                                                    ifelse(grepl("_R3",CIT_minTempSpring$sample), 'R3',
                                                           ifelse(grepl("_R4",CIT_minTempSpring$sample), 'R4', 'R5' )))))
  
#Plot sample column not needed anymore (since this is now split between plotID and survey type), so exclude it 
  CIT_minTempSpring <- CIT_minTempSpring[,-1]
  
#Change the order of the columns to a more logical order
  col_order <- c("plotID","survey_type","CIT_minTempSpring")
  CIT_minTempSpring <- CIT_minTempSpring[,col_order]
  
#The CIT data is now in long format, so first it needs to be converted to a wide format data frame 
#to separate the baseline surveys and resurveys into separate columns, which makes it easier for later calculations
  CIT_minTempSpring <- spread(CIT_minTempSpring,survey_type,CIT_minTempSpring)
  
#Order plotIDs numerically (same order as plot data, more logical to view/find data)
  CIT_minTempSpring <- CIT_minTempSpring[str_order(CIT_minTempSpring$plotID, numeric = T),] 
  
#Give columns more clear names  
  colnames(CIT_minTempSpring) <- c("plotID","B_CIT_minTempSpring","R1_CIT_minTempSpring",
                               "R2_CIT_minTempSpring","R3_CIT_minTempSpring","R4_CIT_minTempSpring","R5_CIT_minTempSpring")  

# For the minimum temperature during winter  
  
#First create a new column with the plot ID (removing the suffix of the baseline survey and resurvey to obtain plot ID)
  CIT_minTempWinter$plotID <- stri_replace_all_regex(CIT_minTempWinter$sample,
                                                 pattern=c('_B', '_R1', '_R2','_R3','_R4','_R5'),
                                                 replacement=c('','','','','',''),
                                                 vectorize=FALSE)
  
#Extract the survey type (baseline survey or resurvey 1 to 5) of the plot sample with a nested ifelse statement 
#(e.g. if plot sample contains _R1 then give value R1 in new column)
  CIT_minTempWinter$survey_type <- ifelse(grepl("_B",CIT_minTempWinter$sample),'B',
                                      ifelse(grepl("_R1",CIT_minTempWinter$sample), 'R1',
                                             ifelse(grepl("_R2",CIT_minTempWinter$sample), 'R2',
                                                    ifelse(grepl("_R3",CIT_minTempWinter$sample), 'R3',
                                                           ifelse(grepl("_R4",CIT_minTempWinter$sample), 'R4', 'R5' )))))
  
#Plot sample column not needed anymore (since this is now split between plotID and survey type), so exclude it 
  CIT_minTempWinter <- CIT_minTempWinter[,-1]
  
#Change the order of the columns to a more logical order
  col_order <- c("plotID","survey_type","CIT_minTempWinter")
  CIT_minTempWinter <- CIT_minTempWinter[,col_order]
  
#The CIT data is now in long format, so first it needs to be converted to a wide format data frame 
#to separate the baseline surveys and resurveys into separate columns, which makes it easier for later calculations
  CIT_minTempWinter <- spread(CIT_minTempWinter,survey_type,CIT_minTempWinter)
  
#Order plotIDs numerically (same order as plot data, more logical to view/find data)
  CIT_minTempWinter <- CIT_minTempWinter[str_order(CIT_minTempWinter$plotID, numeric = T),] 
  
#Give columns more clear names  
  colnames(CIT_minTempWinter) <- c("plotID","B_CIT_minTempWinter","R1_CIT_minTempWinter",
                               "R2_CIT_minTempWinter","R3_CIT_minTempWinter","R4_CIT_minTempWinter","R5_CIT_minTempWinter")  

  

# ---- 7. DELTA CIT PER YEAR CALCULATION ----

#Join the CIT data per plot with the other plot data based on the plot ID
#Left join here (join based on CIT data), because some plots contain no vegetation at the herb layer for both baseline and resurvey, 
#so there is no CIT data 
  
#Join the maximum temperature during the growing season  
  plotdata_CIT <- left_join(CIT_maxTempGS,plotdata_ForestREplot_EU,by = "plotID")
#Add the minimum temperature during spring
  plotdata_CIT <- left_join(CIT_minTempSpring,plotdata_CIT,by = "plotID")
#Add the minimum temperature during winter  
  plotdata_CIT <- left_join(CIT_minTempWinter,plotdata_CIT,by = "plotID")
  
#Calculate the delta CIT per plot (CIT latest resurvey - CIT baseline survey) with a nested ifelse statement for the three different temperatures
#Explanation code (same principle as earlier): if there is a value for resurvey 5 (is.na()==FALSE), then subtract CIT resurvey 5 from CIT baseline survey.
#If not (is.na()==TRUE), then see if there is a value for resurvey 4.
#If this is the case, then subtract CIT resurvey 4 from CIT baseline survey. If not, see if there is a value for resurvey 3 
#and continue to do this until all plots have a delta CIT based on the difference in CIT between the latest resurvey and the baseline survey
  
#For the maximum temperature during the growing season  
  plotdata_CIT$delta_CIT_maxTempGS <- ifelse(is.na(plotdata_CIT$R5_CIT_maxTempGS) == F,
                                         plotdata_CIT$R5_CIT_maxTempGS-plotdata_CIT$B_CIT_maxTempGS,
                                            ifelse(is.na(plotdata_CIT$R4_CIT_maxTempGS) == F,
                                             plotdata_CIT$R4_CIT_maxTempGS-plotdata_CIT$B_CIT_maxTempGS,
                                                ifelse(is.na(plotdata_CIT$R3_CIT_maxTempGS) == F,
                                                 plotdata_CIT$R3_CIT_maxTempGS-plotdata_CIT$B_CIT_maxTempGS,
                                                    ifelse(is.na(plotdata_CIT$R2_CIT_maxTempGS) == F,
                                                     plotdata_CIT$R2_CIT_maxTempGS-plotdata_CIT$B_CIT_maxTempGS,
                                                     plotdata_CIT$R1_CIT_maxTempGS-plotdata_CIT$B_CIT_maxTempGS))))
#For the minimum temperature during spring
  plotdata_CIT$delta_CIT_minTempSpring <- ifelse(is.na(plotdata_CIT$R5_CIT_minTempSpring) == F,
                                            plotdata_CIT$R5_CIT_minTempSpring-plotdata_CIT$B_CIT_minTempSpring,
                                            ifelse(is.na(plotdata_CIT$R4_CIT_minTempSpring) == F,
                                              plotdata_CIT$R4_CIT_minTempSpring-plotdata_CIT$B_CIT_minTempSpring,
                                              ifelse(is.na(plotdata_CIT$R3_CIT_minTempSpring) == F,
                                                 plotdata_CIT$R3_CIT_minTempSpring-plotdata_CIT$B_CIT_minTempSpring,
                                                 ifelse(is.na(plotdata_CIT$R2_CIT_minTempSpring) == F,
                                                    plotdata_CIT$R2_CIT_minTempSpring-plotdata_CIT$B_CIT_minTempSpring,
                                                    plotdata_CIT$R1_CIT_minTempSpring-plotdata_CIT$B_CIT_minTempSpring))))
#For the minimum temperature during winter
  plotdata_CIT$delta_CIT_minTempWinter <- ifelse(is.na(plotdata_CIT$R5_CIT_minTempWinter) == F,
                                           plotdata_CIT$R5_CIT_minTempWinter-plotdata_CIT$B_CIT_minTempWinter,
                                           ifelse(is.na(plotdata_CIT$R4_CIT_minTempWinter) == F,
                                                plotdata_CIT$R4_CIT_minTempWinter-plotdata_CIT$B_CIT_minTempWinter,
                                                ifelse(is.na(plotdata_CIT$R3_CIT_minTempWinter) == F,
                                                     plotdata_CIT$R3_CIT_minTempWinter-plotdata_CIT$B_CIT_minTempWinter,
                                                     ifelse(is.na(plotdata_CIT$R2_CIT_minTempWinter) == F,
                                                          plotdata_CIT$R2_CIT_minTempWinter-plotdata_CIT$B_CIT_minTempWinter,
                                                          plotdata_CIT$R1_CIT_minTempWinter-plotdata_CIT$B_CIT_minTempWinter))))

#Calculate the delta CIT per year by dividing the calculated delta CIT for each of the three temperatures by 
#the calculated time period between the latest resurvey and the baseline survey
  
#For the maximum temperature during growing season  
  plotdata_CIT$delta_CIT_per_year_maxTempGS <- plotdata_CIT$delta_CIT_maxTempGS/plotdata_CIT$time_period
  
#For the minimum temperature during spring  
  plotdata_CIT$delta_CIT_per_year_minTempSpring <- plotdata_CIT$delta_CIT_minTempSpring/plotdata_CIT$time_period
  
#For the minimum temperature during winter  
  plotdata_CIT$delta_CIT_per_year_minTempWinter <- plotdata_CIT$delta_CIT_minTempWinter/plotdata_CIT$time_period  

#Visualise the delta CIT per year for each of the three temperature with a density plot
  
#For the maximum temperature during growing season    
  ggplot(plotdata_CIT, aes(delta_CIT_per_year_maxTempGS)) + geom_density(color='black',lwd=1.1, fill='darkgreen', alpha=0.4) + 
    theme_bw() + xlab("delta CIT per year based on maximum temperature during the growing season (°C/yr)") + theme(plot.title = element_text(hjust=0,size = 20)) +
    geom_vline(aes(xintercept = mean(delta_CIT_per_year_maxTempGS,na.rm = T)), linetype = "dashed", linewidth = 0.75)
  
#For the minimum temperature during spring    
  ggplot(plotdata_CIT, aes(delta_CIT_per_year_minTempSpring)) + geom_density(color='black',lwd=1.1, fill='darkgreen', alpha=0.4) + 
    theme_bw() + xlab("delta CIT per year based on minimum  temperature during spring (°C/yr)") + theme(plot.title = element_text(hjust=0,size = 20)) +
    geom_vline(aes(xintercept = mean(delta_CIT_per_year_minTempSpring,na.rm = T)), linetype = "dashed", linewidth = 0.75)
  
#For the minimum temperature during winter  
  ggplot(plotdata_CIT, aes(delta_CIT_per_year_minTempWinter)) + geom_density(color='black',lwd=1.1, fill='darkgreen', alpha=0.4) + 
    theme_bw() + xlab("delta CIT per year based on minimum temperature during winter (°C/yr)") + theme(plot.title = element_text(hjust=0,size = 20)) +
    geom_vline(aes(xintercept = mean(delta_CIT_per_year_minTempWinter,na.rm = T)), linetype = "dashed", linewidth = 0.75)

#Visualise the relation between delta CIT per year for each of the three temperatures and canopy cover changes
  
#For the maximum temperature during growing season  
  ggplot(plotdata_CIT,aes(canopy_cover_changes,delta_CIT_per_year_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT per year (°C/yr)") + theme_light()

#For the minimum temperature during spring
  ggplot(plotdata_CIT,aes(canopy_cover_changes,delta_CIT_per_year_minTempSpring)) + geom_point() + 
    geom_smooth(method="glm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT per year (°C/yr)") + theme_bw() 
  
#For the minimum temperature during winter
  ggplot(plotdata_CIT,aes(canopy_cover_changes,delta_CIT_per_year_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT per year (°C/yr)") + theme_bw()
  
#Visualise the relation between delta CIT per year for each of the three temperatures and macroclimate changes per year
  
#For the maximum temperature during growing season  
  ggplot(plotdata_CIT,aes(macroclimate_change_per_year,delta_CIT_per_year_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT per year (°C/yr)")

#For the minimum temperature during spring
  ggplot(plotdata_CIT,aes(macroclimate_change_per_year,delta_CIT_per_year_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT per year (°C/yr)")  
  
#For the minimum temperature during winter
  ggplot(plotdata_CIT,aes(macroclimate_change_per_year,delta_CIT_per_year_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT per year (°C/yr)") 

#Visualize the relationship between the delta CIT per year for the three temperatures and MI based on climate offset (9 combinations)
  
#For the maximum temperature during growing season and offset based on mean annual temperature  
  ggplot(plotdata_CIT,aes(MI_MatT_Offset,delta_CIT_per_year_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)") 

#For the minimum temperature during spring and offset based on mean annual temperature  
  ggplot(plotdata_CIT,aes(MI_MatT_Offset,delta_CIT_per_year_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")     

#For the minimum temperature during winter and offset based on mean annual temperature  
  ggplot(plotdata_CIT,aes(MI_MatT_Offset,delta_CIT_per_year_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")  

#For the maximum temperature during growing season and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_CIT,aes(MI_MinT_Offset,delta_CIT_per_year_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)") 
  
#For the minimum temperature during spring and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_CIT,aes(MI_MinT_Offset,delta_CIT_per_year_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")     
  
#For the minimum temperature during winter and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_CIT,aes(MI_MinT_Offset,delta_CIT_per_year_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")        
  
#For the maximum temperature during growing season and offset based on maximum temperature during summer
  ggplot(plotdata_CIT,aes(MI_MaxT_Offset,delta_CIT_per_year_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)") 
  
#For the minimum temperature during spring and offset based on maximum temperature during summer
  ggplot(plotdata_CIT,aes(MI_MaxT_Offset,delta_CIT_per_year_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")     
  
#For the minimum temperature during winter and offset based on maximum temperature during summer
  ggplot(plotdata_CIT,aes(MI_MaxT_Offset,delta_CIT_per_year_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")  

#Visualize the relationship between the delta CIT per year for the three temperatures and MI based climate change magnitude
  
#For the maximum temperature during growing season 
  ggplot(plotdata_CIT,aes(MI_Warming,delta_CIT_per_year_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)") 
  
#For the minimum temperature during spring
  ggplot(plotdata_CIT,aes(MI_Warming,delta_CIT_per_year_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")     
  
#For the minimum temperature during winter 
  ggplot(plotdata_CIT,aes(MI_Warming,delta_CIT_per_year_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")    
    
#Visualize the relationship between the delta CIT per year for the three temperatures and the synthesis MI
 
#For the maximum temperature during growing season
  ggplot(plotdata_CIT,aes(MIs_synthesis,delta_CIT_per_year_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)") 
  
#For the minimum temperature during spring
  ggplot(plotdata_CIT,aes(MIs_synthesis,delta_CIT_per_year_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")     
  
#For the minimum temperature during winter
  ggplot(plotdata_CIT,aes(MIs_synthesis,delta_CIT_per_year_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT per year (°C/yr)")    

#Visualize the relationship between the delta CIT per year and the mean N deposition for the three temperatures  
    
#For the maximum temperature during growing season
  ggplot(plotdata_CIT,aes(N_mean,delta_CIT_per_year_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT per year (°C/yr)") 
  
#For the minimum temperature during spring
  ggplot(plotdata_CIT,aes(N_mean,delta_CIT_per_year_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT per year (°C/yr)")     
  
#For the minimum temperature during winter
  ggplot(plotdata_CIT,aes(N_mean,delta_CIT_per_year_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT per year (°C/yr)")      

#Visualize the relationship between the delta CIT per year and the plot size for the three temperatures  
  
#For the maximum temperature during growing season
  ggplot(plotdata_CIT,aes(plot_size,delta_CIT_per_year_maxTempGS)) + geom_point(color="grey80") + 
    geom_smooth(method="lm" , color="black", fill="red", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT per year (°C/yr)") + theme_classic() + geom_hline(yintercept = 0,linetype = "dashed") + 
    ggtitle("Maximum temperature during growing season") + theme(axis.title = element_text(size=12),plot.title = element_text(size=18)) + 
    ylim(-0.15,0.2)
  
#For the minimum temperature during spring
  ggplot(plotdata_CIT,aes(plot_size,delta_CIT_per_year_minTempSpring)) + geom_point(color="grey80") + 
    geom_smooth(method="lm" , color="black", fill="red", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT per year (°C/yr)")  + theme_classic() + geom_hline(yintercept = 0,linetype = "dashed")  +
    ggtitle("Minimum temperature during spring") + theme(axis.title = element_text(size=12),plot.title = element_text(size=18))+ 
    ylim(-0.15,0.2)
  
#For the minimum temperature during winter
  ggplot(plotdata_CIT,aes(plot_size,delta_CIT_per_year_minTempWinter)) + geom_point(color="grey80") + 
    geom_smooth(method="lm" , color="black", fill="red", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT per year (°C/yr)")    + theme_classic() + geom_hline(yintercept = 0,linetype = "dashed") + 
    ggtitle("Minimum temperature during winter") + theme(axis.title = element_text(size=12),plot.title = element_text(size=18))+ 
    ylim(-0.15,0.2)
  
  
  
# ---- 8. SUBDIVISION CIT ----

#Extract all plot sample ID's and species names of the herb layer    
  scoring_species_processes <- vegdata_ForestREplot_herblayer[,c("sample","species_name")]

#Create a new column with the plot ID (removing the suffix of the baseline survey and resurvey to obtain plot ID)    
  scoring_species_processes$plotID <- stri_replace_all_regex(scoring_species_processes$sample,
                                                pattern=c('_B', '_R1', '_R2','_R3','_R4','_R5'),
                                                replacement=c('','','','','',''),
                                                vectorize=FALSE)

#Extract the survey type (baseline survey or resurvey 1 to 5) of the plot sample with a nested ifelse statement 
  scoring_species_processes$survey_type <- ifelse(grepl("_B",scoring_species_processes$sample),'B',
                                             ifelse(grepl("_R1",scoring_species_processes$sample), 'R1',
                                                ifelse(grepl("_R2",scoring_species_processes$sample), 'R2',
                                                   ifelse(grepl("_R3",scoring_species_processes$sample), 'R3',
                                                      ifelse(grepl("_R4",scoring_species_processes$sample), 'R4', 'R5' )))))

#Plot sample column not needed anymore (since this is now split between plotID and survey type), so exclude it     
  scoring_species_processes <- scoring_species_processes[,-1]

#Change the order of the columns to a more logical order    
  col_order <- c("plotID","survey_type","species_name")
  scoring_species_processes <- scoring_species_processes[,col_order]

#Every species present in the baseline survey gets a score of 1, and each species in the resurveys a score of 0.5  
  scoring_species_processes$score <- ifelse(scoring_species_processes$survey_type=="B",1,0.5)

#The CIT data is now in long format, so first it needs to be converted to a wide format data frame 
#to separate the baseline surveys and resurveys into separate columns for further calculations  
  scoring_species_processes <- spread(scoring_species_processes,survey_type,score)

#All species that are not present in a certain survey get a score of 0  
  scoring_species_processes["B"][is.na(scoring_species_processes["B"])] <- 0
  scoring_species_processes["R1"][is.na(scoring_species_processes["R1"])] <- 0
  scoring_species_processes["R2"][is.na(scoring_species_processes["R2"])] <- 0
  scoring_species_processes["R3"][is.na(scoring_species_processes["R3"])] <- 0
  scoring_species_processes["R4"][is.na(scoring_species_processes["R4"])] <- 0
  scoring_species_processes["R5"][is.na(scoring_species_processes["R5"])] <- 0

#Join the scoring data frame with the CIT data frame (based on plotID) to calculate the score of the different processes 
#based on the latest resurvey for which there is CIT data 
#It does not matter which CIT we use for this, because we just want to know for which resurvey there is CIT data and this is the same for the three CITs
  scoring_species_processes <- left_join(scoring_species_processes,CIT_maxTempGS,by="plotID")

#Calculate the species score for the different processes per plot by subtracting the score of the baseline survey with the score of the latest resurvey of that plot
#This is again done with a nested ifelse statement
#Explanation code: if there is a CIT value for resurvey 5 (is.na()==FALSE), then subtract score baseline survey from score resurvey 5.
#If not (is.na()==TRUE), then see if there is a value for resurvey 4.
#If this is the case, then subtract score baseline survey from score resurvey 4. If not, see if there is a value for resurvey 3 
#and continue to do this until all species per plot have a score based on the process they went through between the baseline survey and the latest resurvey    
  scoring_species_processes$species_process_score <- ifelse(is.na(scoring_species_processes$R5_CIT_maxTempGS) == F,
                                                       scoring_species_processes$B-scoring_species_processes$R5,
                                                       ifelse(is.na(scoring_species_processes$R4_CIT_maxTempGS) == F,
                                                         scoring_species_processes$B-scoring_species_processes$R4,
                                                         ifelse(is.na(scoring_species_processes$R3_CIT_maxTempGS) == F,
                                                           scoring_species_processes$B-scoring_species_processes$R3,
                                                           ifelse(is.na(scoring_species_processes$R2_CIT_maxTempGS) == F,
                                                              scoring_species_processes$B-scoring_species_processes$R2,
                                                              scoring_species_processes$B-scoring_species_processes$R1))))  

#For each of three processes a CIT will now be calculated.  
#Afterwards the relative contribution of these processes to temporal changes in the CIT will be determined.
#This is done for each of the three temperatures  
  
# ---- 8.1 Based on the maximum temperature during growing season ----
  
#To calculate the CIT of the different processes, we need the abundance of the species in the baseline surveys and resurveys and 
#we need the species temperature data from ClimPlant for all species so this will be prepared and joined to the scores in the following steps
  
#Extract all plot sample ID's and species names of species for which there is abundance and temperature data
  vegdata_mean_maxTempGS_wide <- vegdata_mean_maxTempGS[,c("sample","species_name","abundance","mean_maxTempGS")]

#Create a new column with the plot ID (removing the suffix of the baseline survey and resurvey to obtain plot ID)      
  vegdata_mean_maxTempGS_wide$plotID <- stri_replace_all_regex(vegdata_mean_maxTempGS_wide$sample,
                                                               pattern=c('_B', '_R1', '_R2','_R3','_R4','_R5'),
                                                               replacement=c('','','','','',''),
                                                               vectorize=FALSE)

#Extract the survey type (baseline survey or resurvey 1 to 5) of the plot sample with a nested ifelse statement 
  vegdata_mean_maxTempGS_wide$survey_type <- ifelse(grepl("_B",vegdata_mean_maxTempGS_wide$sample),'B',
                                                    ifelse(grepl("_R1",vegdata_mean_maxTempGS_wide$sample), 'R1',
                                                           ifelse(grepl("_R2",vegdata_mean_maxTempGS_wide$sample), 'R2',
                                                                  ifelse(grepl("_R3",vegdata_mean_maxTempGS_wide$sample), 'R3',
                                                                         ifelse(grepl("_R4",vegdata_mean_maxTempGS_wide$sample), 'R4', 'R5' )))))

#Plot sample column not needed anymore (since this is now split between plotID and survey type), so exclude it   
  vegdata_mean_maxTempGS_wide <- vegdata_mean_maxTempGS_wide[,-1]

#Change the order of the columns to a more logical order   
  col_order <- c("plotID","survey_type","species_name","abundance","mean_maxTempGS") 
  vegdata_mean_maxTempGS_wide <- vegdata_mean_maxTempGS_wide[,col_order]  

#The CIT data is now in long format, so first it needs to be converted to a wide format data frame 
#to separate the baseline surveys and resurveys into separate columns for further calculations  
  vegdata_mean_maxTempGS_wide <- spread(vegdata_mean_maxTempGS_wide,survey_type,abundance)  

#Give the columns more clear names  
  colnames(vegdata_mean_maxTempGS_wide) <- c("plotID","species_name","mean_maxTempGS","B_abundance","R1_abundance","R2_abundance",
                                             "R3_abundance","R4_abundance","R5_abundance")  
  
#Join the species scores with the abundance data and the species temperature data from ClimPlant based on the plotIDs and species names
#so that everything is in one data frame which makes it easier for later calculations  
  scoring_species_processes_maxTempGS <- left_join(scoring_species_processes,vegdata_mean_maxTempGS_wide,by=c("plotID","species_name"))  
    
#Split the species per plot into different data frames based on their score
  scoring_disappear_maxTempGS <- filter(scoring_species_processes_maxTempGS, species_process_score == 1)
  scoring_new_maxTempGS <- filter(scoring_species_processes_maxTempGS, species_process_score == -0.5)
  scoring_remain_maxTempGS <- filter(scoring_species_processes_maxTempGS,species_process_score == 0.5)

#Now the CITs of the three different processes can be calculated  

#For the disappearing species (only present in the baseline survey)
  CIT_disappear_maxTempGS <- scoring_disappear_maxTempGS %>% group_by(plotID) %>% 
    summarise(CIT_disappear = weighted.mean(mean_maxTempGS,B_abundance,na.rm=T))
  
#For the newly occurring species (only present in the resurvey) 
#First extract the abundance of the latest resurvey with a nested ifelse statement (same logic as before)  
  scoring_new_maxTempGS$abundance_new <- ifelse(is.na(scoring_new_maxTempGS$R5_abundance) == F,
                                            scoring_new_maxTempGS$R5_abundance,
                                            ifelse(is.na(scoring_new_maxTempGS$R4_abundance) == F,
                                                  scoring_new_maxTempGS$R4_abundance,
                                                  ifelse(is.na(scoring_new_maxTempGS$R3_abundance) == F,
                                                         scoring_new_maxTempGS$R3_abundance,
                                                         ifelse(is.na(scoring_new_maxTempGS$R2_abundance) == F,
                                                                scoring_new_maxTempGS$R2_abundance,
                                                                scoring_new_maxTempGS$R1_abundance))))

#Then calculate the CIT based on the abundance of the latest resurvey  
  CIT_new_maxTempGS <- scoring_new_maxTempGS %>% group_by(plotID) %>% 
    summarise(CIT_new = weighted.mean(mean_maxTempGS,abundance_new,na.rm=T))
  
#For the remaining species (present in both baseline survey and resurvey)  
#First extract the abundance of the latest resurvey with a nested ifelse statement (same logic as before)  
  scoring_remain_maxTempGS$abundance_remain <- ifelse(is.na(scoring_remain_maxTempGS$R5_abundance) == F,
                                                  scoring_remain_maxTempGS$R5_abundance,
                                                  ifelse(is.na(scoring_remain_maxTempGS$R4_abundance) == F,
                                                         scoring_remain_maxTempGS$R4_abundance,
                                                         ifelse(is.na(scoring_remain_maxTempGS$R3_abundance) == F,
                                                                scoring_remain_maxTempGS$R3_abundance,
                                                                ifelse(is.na(scoring_remain_maxTempGS$R2_abundance) == F,
                                                                      scoring_remain_maxTempGS$R2_abundance,
                                                                      scoring_remain_maxTempGS$R1_abundance))))  

#Then calculate the CIT based on the abundance of the latest resurvey  
  CIT_remain_maxTempGS <- scoring_remain_maxTempGS %>% group_by(plotID) %>% 
    summarise(CIT_remain_R = weighted.mean(mean_maxTempGS,abundance_remain,na.rm=T))  

#Now the CITs per process will be compared  to the general CIT

#By visually comparing the CITs based on the processes (as density plots) and the general CIT for the baseline survey and the resurvey (mean as vertical lines)

#First extract the general CIT of the latest resurvey (now spread over multiple columns, preferably in a single column) with a nested ifelse statement
  CIT_maxTempGS$CIT_latest_resurvey <- ifelse(is.na(CIT_maxTempGS$R5_CIT_maxTempGS) == F,
                                              CIT_maxTempGS$R5_CIT_maxTempGS,
                                              ifelse(is.na(CIT_maxTempGS$R4_CIT_maxTempGS) == F,
                                                    CIT_maxTempGS$R4_CIT_maxTempGS,
                                                    ifelse(is.na(CIT_maxTempGS$R3_CIT_maxTempGS) == F,
                                                           CIT_maxTempGS$R3_CIT_maxTempGS,
                                                           ifelse(is.na(CIT_maxTempGS$R2_CIT_maxTempGS) == F,
                                                                  CIT_maxTempGS$R2_CIT_maxTempGS,
                                                                  CIT_maxTempGS$R1_CIT_maxTempGS))))

#Join the CITs of the three processes to combine them in one figure
CIT_processes_maxTempGS <- full_join(CIT_disappear_maxTempGS,CIT_new_maxTempGS,by="plotID")
CIT_processes_maxTempGS <- full_join(CIT_processes_maxTempGS,CIT_remain_maxTempGS, by="plotID")
colnames(CIT_processes_maxTempGS) <- c("plotID","Species loss","Species gain","Changing abundance","CIT_remain_B")

#Convert to wide data format to plot multiple density plots on top of each other
CIT_processes_maxTempGS <- gather(CIT_processes_maxTempGS,process,CIT,c("Species loss","Species gain","Changing abundance"))
colnames(CIT_processes_maxTempGS) <- c("plotID","CIT_remain_B","Species_process","CIT")    
    
#CIT disappearing species vs general CIT  
  ggplot(CIT_disappear_maxTempGS, aes(CIT_disappear)) + geom_density(fill='darkgreen',lwd=1.1,alpha=0.4)+
    xlim(15,20.5) + theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_maxTempGS$B_CIT_maxTempGS,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    annotate("text",x=17.1,y=0.56,label="mean CIT baseline survey",size=3) +
    geom_vline(aes(xintercept = mean(CIT_maxTempGS$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    annotate("text",x=18.52,y=0.6,label="mean CIT latest resurvey",size=3) +
    xlab("CIT disappearing species (°C)") +ylab("Probability of occurrence") + ggtitle("Disappeared species") + theme_bw()
  
#CIT newly occurring species vs general CIT  
  ggplot(CIT_new_maxTempGS, aes(CIT_new)) + geom_density(fill='darkgreen',lwd=1.1,alpha=0.4)+
    xlim(15,20.5) + theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_maxTempGS$B_CIT_maxTempGS,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    annotate("text",x=17.12,y=0.56,label="mean CIT baseline survey",size=3) +
    geom_vline(aes(xintercept = mean(CIT_maxTempGS$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    annotate("text",x=18.52,y=0.66,label="mean CIT latest resurvey",size=3) +
    xlab("CIT newly occuring species (°C)") +ylab("Probability of occurrence") + ggtitle("Newly occurring species") + theme_bw()

#CIT remaining species vs general CIT
  ggplot(CIT_remain_maxTempGS, aes(CIT_remain_R)) + geom_density(fill='darkgreen',lwd=1.1,alpha=0.4)+
    xlim(15,20.5) + theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_maxTempGS$B_CIT_maxTempGS,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    annotate("text",x=17.12,y=0.56,label="mean CIT baseline survey",size=3) +
    geom_vline(aes(xintercept = mean(CIT_maxTempGS$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    annotate("text",x=18.5,y=0.74,label="mean CIT latest resurvey",size=3) +
    xlab("CIT remaining species (°C)") +ylab("Probability of occurrence") + ggtitle("Remaining species") + theme_bw()

#CIT of all processes vs general CIT
  ggplot(CIT_processes_maxTempGS, aes(CIT,color=Species_process,fill=Species_process,linetype=Species_process)) + geom_density(lwd=1.1,alpha=0.4)+
    xlim(15,20.5) + theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_maxTempGS$B_CIT_maxTempGS,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    geom_vline(aes(xintercept = mean(CIT_maxTempGS$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    xlab("Community-inferred temperature (°C)") + ylab("Probability of occurrence") + theme_bw() + 
    scale_fill_manual(name = "Species process",values = c("blue2","tomato","springgreen4"),breaks=c("Species loss","Species gain","Changing abundance")) + 
    scale_color_manual(name = "Species process",values = c("blue2","tomato4","springgreen4"),breaks=c("Species loss","Species gain","Changing abundance")) + 
    scale_linetype_manual(name = "Species process",values = c("longdash","dotdash","solid"),breaks=c("Species loss","Species gain","Changing abundance")) 
    
#By calculating and comparing the mean CIT per process and the mean general CIT for the baseline survey and resurvey
  mean_CIT_maxTempGS <- data.frame(mean(CIT_maxTempGS$B_CIT_maxTempGS,na.rm=T),mean(CIT_maxTempGS$CIT_latest_resurvey,na.rm=T),
                                   mean(CIT_disappear_maxTempGS$CIT_disappear,na.rm=T),mean(CIT_new_maxTempGS$CIT_new,na.rm=T),
                                   mean(CIT_remain_maxTempGS$CIT_remain_R,na.rm=T))
  colnames(mean_CIT_maxTempGS) <- c("mean_CIT_baseline_survey","mean_CIT_latest_resurvey","mean_CIT_disappear","mean_CIT_new","mean_CIT_remain")

#We also want to test whether the CIT significantly differ between the processes
#This is done with first defining a linear mixed model using the process as a fixed effect and the plot as a random effect  
  lme_maxTempGS <- lmer(CIT~process + (1|plotID),data=CIT_processes_maxTempGS)
  summary(lme_maxTempGS)  

#Then a posthoc Tukey test is performed to see which CITs of the processes significantly differ from each other    
  emmeans(lme_maxTempGS, list(pairwise ~ process), adjust = "tukey")
  
#The CIT of the newly occurring species is significantly higher than that of both the remaining and disappeared species
#The CIT of the remaining and disappeared species do not significantly differ from each other  
  
  
#The relative contribution of these three processes to temporal changes in the CIT will be calculated by first calculating the delta CIT per process
#Then the delta CIT of one process will be divided by the delta CIT of all three processes to obtain the relative contribution of that one process
#Here it is done for the delta CIT based on the maximum temperature during growing season  
  
#The delta CIT for the disappeared species is equal to the CIT of the disappeared species since these species only have abundance values
#for the baseline survey and not for the resurvey (so subtraction by zero for delta CIT)
  
#Likewise, the delta CIT for the newly occurring species is equal to the CIT of the newly occurring species since these species only have abundance values  
#for the resurvey and not for the baseline survey (so subtraction by zero for delta CIT)   
  
#The delta CIT for the remaining species is equal to the CIT of the remaining species based on abundances from the resurvey subtracted by 
#the CIT of the remaining species based on abundances from the baseline survey  
  
#Calculate the CIT of the remaining species based on the abundance of the baseline survey
  CIT_remain_B_maxTempGS <- scoring_remain_maxTempGS %>% group_by(plotID) %>% 
    summarise(CIT_remain_B = weighted.mean(mean_maxTempGS,B_abundance,na.rm=T))  

#Join both CITs of the remaining species so it is easier to calculate the delta CIT
  CIT_remain_maxTempGS <- full_join(CIT_remain_maxTempGS,CIT_remain_B_maxTempGS,by="plotID")
      
#Subtract the CIT of the resurvey from the CIT of the baseline survey to obtain the delta CIT of the remaining species
  delta_CIT_remain_maxTempGS <- CIT_remain_maxTempGS %>% group_by(plotID) %>% 
    summarise(delta_CIT_remain = CIT_remain_R - CIT_remain_B)
  
#Join all three delta CITs into one data frame for easier further calculations and a more clear overview
  delta_CIT_processes_maxTempGS <- full_join(delta_CIT_remain_maxTempGS,CIT_disappear_maxTempGS,by="plotID")
  delta_CIT_processes_maxTempGS <- full_join(delta_CIT_processes_maxTempGS,CIT_new_maxTempGS,by="plotID") 
  colnames(delta_CIT_processes_maxTempGS) <- c("plotID","delta_CIT_remain_maxTempGS","delta_CIT_disappear_maxTempGS","delta_CIT_new_maxTempGS")  
   
#Relative contribution of disappeared species per plot
  rel_contr_processes_maxTempGS <- delta_CIT_processes_maxTempGS
  rel_contr_processes_maxTempGS$rel_contr_disappear <- rel_contr_processes_maxTempGS$delta_CIT_disappear_maxTempGS/
    (abs(rel_contr_processes_maxTempGS$delta_CIT_remain_maxTempGS)+rel_contr_processes_maxTempGS$delta_CIT_disappear_maxTempGS+rel_contr_processes_maxTempGS$delta_CIT_new_maxTempGS)
  
#Relative contribution of newly occurring species per plot
  rel_contr_processes_maxTempGS$rel_contr_new <- rel_contr_processes_maxTempGS$delta_CIT_new_maxTempGS/
    (abs(rel_contr_processes_maxTempGS$delta_CIT_remain_maxTempGS)+rel_contr_processes_maxTempGS$delta_CIT_disappear_maxTempGS+rel_contr_processes_maxTempGS$delta_CIT_new_maxTempGS)
  
#Relative contribution of remaining species per plot
  rel_contr_processes_maxTempGS$rel_contr_remain <- abs(rel_contr_processes_maxTempGS$delta_CIT_remain_maxTempGS)/
    (abs(rel_contr_processes_maxTempGS$delta_CIT_remain_maxTempGS)+rel_contr_processes_maxTempGS$delta_CIT_disappear_maxTempGS+rel_contr_processes_maxTempGS$delta_CIT_new_maxTempGS)
  
#Mean contribution of the three processes for all plots
  mean_rel_contr_processes_maxTempGS <- data.frame(mean(rel_contr_processes_maxTempGS$rel_contr_disappear,na.rm=T),
                                                   mean(rel_contr_processes_maxTempGS$rel_contr_new,na.rm=T),
                                                   mean(rel_contr_processes_maxTempGS$rel_contr_remain,na.rm=T))
  colnames(mean_rel_contr_processes_maxTempGS) <- c("mean contribution disappearing","mean contribution newly occurring","mean contribution remaining")
  
      
# ---- 8.2 Based on the minimum temperature during spring ----
  
#To calculate the CIT of the different processes based on the minimum temperature during spring, 
#we need the species temperature data for the minimum temperature during spring from ClimPlant for all species 
  
#Extract all plot sample ID's and species names of species for which there is temperature data
  vegdata_mean_minTempSpring_wide <- vegdata_mean_minTempSpring[,c("sample","species_name","abundance","mean_minTempSpring")]

#Extract the survey type (baseline survey or resurvey 1 to 5) of the plot sample with a nested ifelse statement 
  vegdata_mean_minTempSpring_wide$survey_type <- ifelse(grepl("_B",vegdata_mean_minTempSpring_wide$sample),'B',
                                                        ifelse(grepl("_R1",vegdata_mean_minTempSpring_wide$sample), 'R1',
                                                               ifelse(grepl("_R2",vegdata_mean_minTempSpring_wide$sample), 'R2',
                                                                      ifelse(grepl("_R3",vegdata_mean_minTempSpring_wide$sample), 'R3',
                                                                             ifelse(grepl("_R4",vegdata_mean_minTempSpring_wide$sample), 'R4', 'R5' )))))  
    
#Create a new column with the plot ID (removing the suffix of the baseline survey and resurvey to obtain plot ID)      
  vegdata_mean_minTempSpring_wide$plotID <- stri_replace_all_regex(vegdata_mean_minTempSpring_wide$sample,
                                                               pattern=c('_B', '_R1', '_R2','_R3','_R4','_R5'),
                                                               replacement=c('','','','','',''),
                                                               vectorize=FALSE)
    
#Plot sample column not needed anymore (since this is now split between plotID and survey type), so exclude it   
  vegdata_mean_minTempSpring_wide <- vegdata_mean_minTempSpring_wide[,-1]
  
#Convert to a wide format data frame so that each species that occurs in a certain plot occurs only once in the data frame for that plot
#(e.g. in long format, the species would occur for both baseline survey as resurvey(s) if it was present in both)
  vegdata_mean_minTempSpring_wide <- spread(vegdata_mean_minTempSpring_wide,survey_type,abundance)  
  
#Exclude redundant columns
  vegdata_mean_minTempSpring_wide <- vegdata_mean_minTempSpring_wide[,1:3]
  
#Change the order of the columns to a more logical order   
  col_order <- c("plotID","species_name","mean_minTempSpring") 
  vegdata_mean_minTempSpring_wide <- vegdata_mean_minTempSpring_wide[,col_order]
   
#Join the species scores with the species temperature data from ClimPlant based on the plotIDs and species names
#so that everything is in one data frame which makes it easier for later calculations  
  scoring_species_processes_minTempSpring <- left_join(scoring_species_processes_maxTempGS,vegdata_mean_minTempSpring_wide,by=c("plotID","species_name"))  

#Split the species per plot into different data frames based on their score  
  scoring_disappear_minTempSpring <- filter(scoring_species_processes_minTempSpring, species_process_score == 1)
  scoring_new_minTempSpring <- filter(scoring_species_processes_minTempSpring, species_process_score == -0.5)
  scoring_remain_minTempSpring <- filter(scoring_species_processes_minTempSpring,species_process_score == 0.5)  
  
  
#Now the CITs of the three different processes can be calculated  
    
#For the disappearing species (only present in the baseline survey)
  CIT_disappear_minTempSpring <- scoring_disappear_minTempSpring %>% group_by(plotID) %>% 
    summarise(CIT_disappear = weighted.mean(mean_minTempSpring,B_abundance,na.rm=T))
  
#For the newly occurring species (only present in the resurvey) 
#First extract the abundance of the latest resurvey with a nested ifelse statement (same logic as before)  
  scoring_new_minTempSpring$abundance_new <- ifelse(is.na(scoring_new_minTempSpring$R5_abundance) == F,
                                                scoring_new_minTempSpring$R5_abundance,
                                                ifelse(is.na(scoring_new_minTempSpring$R4_abundance) == F,
                                                       scoring_new_minTempSpring$R4_abundance,
                                                       ifelse(is.na(scoring_new_minTempSpring$R3_abundance) == F,
                                                              scoring_new_minTempSpring$R3_abundance,
                                                              ifelse(is.na(scoring_new_minTempSpring$R2_abundance) == F,
                                                                     scoring_new_minTempSpring$R2_abundance,
                                                                     scoring_new_minTempSpring$R1_abundance))))
  
#Then calculate the CIT based on the abundance of the latest resurvey  
  CIT_new_minTempSpring <- scoring_new_minTempSpring %>% group_by(plotID) %>% 
    summarise(CIT_new = weighted.mean(mean_minTempSpring,abundance_new,na.rm=T))
  
#For the remaining species (present in both baseline survey and resurvey)  
#First extract the abundance of the latest resurvey with a nested ifelse statement (same logic as before)  
  scoring_remain_minTempSpring$abundance_remain <- ifelse(is.na(scoring_remain_minTempSpring$R5_abundance) == F,
                                                       scoring_remain_minTempSpring$R5_abundance,
                                                       ifelse(is.na(scoring_remain_minTempSpring$R4_abundance) == F,
                                                           scoring_remain_minTempSpring$R4_abundance,
                                                           ifelse(is.na(scoring_remain_minTempSpring$R3_abundance) == F,
                                                                scoring_remain_minTempSpring$R3_abundance,
                                                                ifelse(is.na(scoring_remain_minTempSpring$R2_abundance) == F,
                                                                    scoring_remain_minTempSpring$R2_abundance,
                                                                    scoring_remain_minTempSpring$R1_abundance))))  
      
#Then calculate the CIT based on the abundance of the latest resurvey  
  CIT_remain_minTempSpring <- scoring_remain_minTempSpring %>% group_by(plotID) %>% 
    summarise(CIT_remain_R = weighted.mean(mean_minTempSpring,abundance_remain,na.rm=T))  
  
#Now the CITs per process will be compared  to the general CIT
  
#By visually comparing the CITs based on the processes (as density plots) and the general CIT for the baseline survey and the resurvey (mean as vertical lines)
  
#First extract the general CIT of the latest resurvey (now spread over multiple columns, preferably in a single column) with a nested ifelse statement
  CIT_minTempSpring$CIT_latest_resurvey <- ifelse(is.na(CIT_minTempSpring$R5_CIT_minTempSpring) == F,
                                              CIT_minTempSpring$R5_CIT_minTempSpring,
                                              ifelse(is.na(CIT_minTempSpring$R4_CIT_minTempSpring) == F,
                                                     CIT_minTempSpring$R4_CIT_minTempSpring,
                                                     ifelse(is.na(CIT_minTempSpring$R3_CIT_minTempSpring) == F,
                                                            CIT_minTempSpring$R3_CIT_minTempSpring,
                                                            ifelse(is.na(CIT_minTempSpring$R2_CIT_minTempSpring) == F,
                                                                   CIT_minTempSpring$R2_CIT_minTempSpring,
                                                                   CIT_minTempSpring$R1_CIT_minTempSpring))))

#Join the CITs of the three processes to combine them in one figure
  CIT_processes_minTempSpring <- full_join(CIT_disappear_minTempSpring,CIT_new_minTempSpring,by="plotID")
  CIT_processes_minTempSpring <- full_join(CIT_processes_minTempSpring,CIT_remain_minTempSpring, by="plotID")
  colnames(CIT_processes_minTempSpring) <- c("plotID","Species loss","Species gain","Changing abundance","CIT_remain_B")
  
#Convert to wide data format to plot multiple density plots on top of each other
  CIT_processes_minTempSpring <- gather(CIT_processes_minTempSpring,process,CIT,c("Species loss","Species gain","Changing abundance"))  
  colnames(CIT_processes_minTempSpring) <- c("plotID","CIT_remain_B","Species_process","CIT")  
  
#CIT disappearing species vs general CIT  
  ggplot(CIT_disappear_minTempSpring, aes(CIT_disappear)) + geom_density(fill='darkgreen',lwd=1.1,alpha=0.4)+
    theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_minTempSpring$B_CIT_minTempSpring,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    annotate("text",x=0.7,y=0.45,label="mean CIT baseline survey",size=3) +
    geom_vline(aes(xintercept = mean(CIT_minTempSpring$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    annotate("text",x=2.95,y=0.5,label="mean CIT latest resurvey",size=3) +
    xlab("CIT disappearing species (°C)") +ylab("Probability of occurrence") + ggtitle("Disappeared species") +theme_bw()
  
#CIT newly occurring species vs general CIT  
  ggplot(CIT_new_minTempSpring, aes(CIT_new)) + geom_density(fill='darkgreen',lwd=1.1,alpha=0.4)+
    xlim(-2,5) + theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_minTempSpring$B_CIT_minTempSpring,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    annotate("text",x=0.9,y=0.50,label="mean CIT baseline survey",size=3) +
    geom_vline(aes(xintercept = mean(CIT_minTempSpring$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    annotate("text",x=2.75,y=0.58,label="mean CIT latest resurvey",size=3) +
    xlab("CIT newly occuring species (°C)") +ylab("Probability of occurrence") + ggtitle("Newly occurring species") +theme_bw()
  
#CIT remaining species vs general CIT
  ggplot(CIT_remain_minTempSpring, aes(CIT_remain_R)) + geom_density(fill='darkgreen',lwd=1.1,alpha=0.4)+
    xlim(-1.3,5) + theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_minTempSpring$B_CIT_minTempSpring,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    annotate("text",x=1,y=0.47,label="mean CIT baseline survey",size=3) +
    geom_vline(aes(xintercept = mean(CIT_minTempSpring$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    annotate("text",x=2.7,y=0.55,label="mean CIT latest resurvey",size=3) +
    xlab("CIT remaining species (°C)") +ylab("Probability of occurrence") + ggtitle("Remaining species") +theme_bw()
  
#CIT of all processes vs general CIT
  ggplot(CIT_processes_minTempSpring, aes(CIT,color=Species_process,fill=Species_process,linetype=Species_process)) + geom_density(lwd=1.1,alpha=0.4)+
    xlim(-2,5) + theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_minTempSpring$B_CIT_minTempSpring,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    geom_vline(aes(xintercept = mean(CIT_minTempSpring$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    xlab("Community-inferred temperature (°C)") + ylab("Probability of occurrence") + theme_bw() + 
    scale_fill_manual(name = "Species process",values = c("blue2", "tomato","springgreen4"),breaks=c("Species loss","Species gain","Changing abundance")) + 
    scale_color_manual(name = "Species process",values = c("blue2", "tomato4","springgreen4"),breaks=c("Species loss","Species gain","Changing abundance")) +
    scale_linetype_manual(name = "Species process",values = c("longdash","dotdash","solid"),breaks=c("Species loss","Species gain","Changing abundance"))  
    
#By calculating and comparing the mean CIT per process and the mean general CIT for the baseline survey and resurvey
  mean_CIT_minTempSpring <- data.frame(mean(CIT_minTempSpring$B_CIT_minTempSpring,na.rm=T),mean(CIT_minTempSpring$CIT_latest_resurvey,na.rm=T),
                                   mean(CIT_disappear_minTempSpring$CIT_disappear,na.rm=T),mean(CIT_new_minTempSpring$CIT_new,na.rm=T),
                                   mean(CIT_remain_minTempSpring$CIT_remain_R,na.rm=T))
  colnames(mean_CIT_minTempSpring) <- c("mean_CIT_baseline_survey","mean_CIT_latest_resurvey","mean_CIT_disappear","mean_CIT_new","mean_CIT_remain")
  
#We also want to test whether the CIT significantly differ between the processes
#This is done with first defining a linear mixed model using the process as a fixed effect and the plot as a random effect  
  lme_minTempSpring <- lmer(CIT~process + (1|plotID),data=CIT_processes_minTempSpring)
  summary(lme_minTempSpring)  
  
#Then a posthoc Tukey test is performed to see which CITs of the processes significantly differ from each other    
  emmeans(lme_minTempSpring, list(pairwise ~ process), adjust = "tukey")  

#All CITs differ significantly from each other in this order: CIT newly occurring > CIT remaining > CIT disappeared species 
    
  
#The relative contribution of these three processes to temporal changes in the CIT will now be calculated
  
#The delta CIT for the disappeared species is equal to the CIT of the disappeared species since these species only have abundance values
#for the baseline survey and not for the resurvey (so subtraction by zero for delta CIT)
  
#Likewise, the delta CIT for the newly occurring species is equal to the CIT of the newly occurring species since these species only have abundance values  
#for the resurvey and not for the baseline survey (so subtraction by zero for delta CIT)   
  
#The delta CIT for the remaining species is equal to the CIT of the remaining species based on abundances from the resurvey subtracted by 
#the CIT of the remaining species based on abundances from the baseline survey  
  
#Calculate the CIT of the remaining species based on the abundance of the baseline survey
  CIT_remain_B_minTempSpring <- scoring_remain_minTempSpring %>% group_by(plotID) %>% 
    summarise(CIT_remain_B = weighted.mean(mean_minTempSpring,B_abundance,na.rm=T))  
  
#Join both CITs of the remaining species so it is easier to calculate the delta CIT
  CIT_remain_minTempSpring <- full_join(CIT_remain_minTempSpring,CIT_remain_B_minTempSpring,by="plotID")
  
#Subtract the CIT of the resurvey from the CIT of the baseline survey to obtain the delta CIT of the remaining species
  delta_CIT_remain_minTempSpring <- CIT_remain_minTempSpring %>% group_by(plotID) %>% 
    summarise(delta_CIT_remain = CIT_remain_R - CIT_remain_B)
  
#Join all three delta CITs into one data frame for easier further calculations and a more clear overview
  delta_CIT_processes_minTempSpring <- full_join(delta_CIT_remain_minTempSpring,CIT_disappear_minTempSpring,by="plotID")
  delta_CIT_processes_minTempSpring <- full_join(delta_CIT_processes_minTempSpring,CIT_new_minTempSpring,by="plotID") 
  colnames(delta_CIT_processes_minTempSpring) <- c("plotID","delta_CIT_remain_minTempSpring","delta_CIT_disappear_minTempSpring","delta_CIT_new_minTempSpring")  
  
#Relative contribution of disappeared species per plot
  rel_contr_processes_minTempSpring <- delta_CIT_processes_minTempSpring
  rel_contr_processes_minTempSpring$rel_contr_disappear <- abs(rel_contr_processes_minTempSpring$delta_CIT_disappear_minTempSpring)/
    (abs(rel_contr_processes_minTempSpring$delta_CIT_remain_minTempSpring)+abs(rel_contr_processes_minTempSpring$delta_CIT_disappear_minTempSpring)
     +abs(rel_contr_processes_minTempSpring$delta_CIT_new_minTempSpring))
  
#Relative contribution of newly occurring species per plot
  rel_contr_processes_minTempSpring$rel_contr_new <- abs(rel_contr_processes_minTempSpring$delta_CIT_new_minTempSpring)/
    (abs(rel_contr_processes_minTempSpring$delta_CIT_remain_minTempSpring)+abs(rel_contr_processes_minTempSpring$delta_CIT_disappear_minTempSpring)
     +abs(rel_contr_processes_minTempSpring$delta_CIT_new_minTempSpring))
  
#Relative contribution of remaining species per plot
  rel_contr_processes_minTempSpring$rel_contr_remain <- abs(rel_contr_processes_minTempSpring$delta_CIT_remain_minTempSpring)/
    (abs(rel_contr_processes_minTempSpring$delta_CIT_remain_minTempSpring)+abs(rel_contr_processes_minTempSpring$delta_CIT_disappear_minTempSpring)
     +abs(rel_contr_processes_minTempSpring$delta_CIT_new_minTempSpring))
  
#Mean contribution of the three processes for all plots
  mean_rel_contr_processes_minTempSpring <- data.frame(mean(rel_contr_processes_minTempSpring$rel_contr_disappear,na.rm=T),
                                                   mean(rel_contr_processes_minTempSpring$rel_contr_new,na.rm=T),
                                                   mean(rel_contr_processes_minTempSpring$rel_contr_remain,na.rm=T))
  colnames(mean_rel_contr_processes_minTempSpring) <- c("mean contribution disappearing","mean contribution newly occurring","mean contribution remaining")  
  
  
# ---- 8.3 Based on the minimum temperature during winter ----
  
#To calculate the CIT of the different processes based on the minimum temperature during winter, 
#we need the species temperature data for the minimum temperature during winter from ClimPlant for all species 
  
#Extract all plot sample ID's and species names of species for which there is temperature data
  vegdata_mean_minTempWinter_wide <- vegdata_mean_minTempWinter[,c("sample","species_name","abundance","mean_minTempWinter")]
  
#Extract the survey type (baseline survey or resurvey 1 to 5) of the plot sample with a nested ifelse statement 
  vegdata_mean_minTempWinter_wide$survey_type <- ifelse(grepl("_B",vegdata_mean_minTempWinter_wide$sample),'B',
                                                        ifelse(grepl("_R1",vegdata_mean_minTempWinter_wide$sample), 'R1',
                                                               ifelse(grepl("_R2",vegdata_mean_minTempWinter_wide$sample), 'R2',
                                                                      ifelse(grepl("_R3",vegdata_mean_minTempWinter_wide$sample), 'R3',
                                                                             ifelse(grepl("_R4",vegdata_mean_minTempWinter_wide$sample), 'R4', 'R5' )))))  
  
#Create a new column with the plot ID (removing the suffix of the baseline survey and resurvey to obtain plot ID)      
  vegdata_mean_minTempWinter_wide$plotID <- stri_replace_all_regex(vegdata_mean_minTempWinter_wide$sample,
                                                                   pattern=c('_B', '_R1', '_R2','_R3','_R4','_R5'),
                                                                   replacement=c('','','','','',''),
                                                                   vectorize=FALSE)
  
#Plot sample column not needed anymore (since this is now split between plotID and survey type), so exclude it   
  vegdata_mean_minTempWinter_wide <- vegdata_mean_minTempWinter_wide[,-1]
  
#Convert to a wide format data frame so that each species that occurs in a certain plot occurs only once in the data frame for that plot
#(e.g. in long format, the species would occur for both baseline survey as resurvey(s) if it was present in both)
  vegdata_mean_minTempWinter_wide <- spread(vegdata_mean_minTempWinter_wide,survey_type,abundance)  
  
#Exclude redundant columns
  vegdata_mean_minTempWinter_wide <- vegdata_mean_minTempWinter_wide[,1:3]
  
#Change the order of the columns to a more logical order   
  col_order <- c("plotID","species_name","mean_minTempWinter") 
  vegdata_mean_minTempWinter_wide <- vegdata_mean_minTempWinter_wide[,col_order]
  
#Join the species scores with the species temperature data from ClimPlant based on the plotIDs and species names
#so that everything is in one data frame which makes it easier for later calculations  
  scoring_species_processes_minTempWinter <- left_join(scoring_species_processes_maxTempGS,vegdata_mean_minTempWinter_wide,by=c("plotID","species_name"))  
  
#Split the species per plot into different data frames based on their score  
  scoring_disappear_minTempWinter <- filter(scoring_species_processes_minTempWinter, species_process_score == 1)
  scoring_new_minTempWinter <- filter(scoring_species_processes_minTempWinter, species_process_score == -0.5)
  scoring_remain_minTempWinter <- filter(scoring_species_processes_minTempWinter,species_process_score == 0.5)  
  
  
#Now the CITs of the three different processes can be calculated  
  
#For the disappearing species (only present in the baseline survey)
  CIT_disappear_minTempWinter <- scoring_disappear_minTempWinter %>% group_by(plotID) %>% 
    summarise(CIT_disappear = weighted.mean(mean_minTempWinter,B_abundance,na.rm=T))
  
#For the newly occurring species (only present in the resurvey) 
#First extract the abundance of the latest resurvey with a nested ifelse statement (same logic as before)  
  scoring_new_minTempWinter$abundance_new <- ifelse(is.na(scoring_new_minTempWinter$R5_abundance) == F,
                                                    scoring_new_minTempWinter$R5_abundance,
                                                    ifelse(is.na(scoring_new_minTempWinter$R4_abundance) == F,
                                                           scoring_new_minTempWinter$R4_abundance,
                                                           ifelse(is.na(scoring_new_minTempWinter$R3_abundance) == F,
                                                                  scoring_new_minTempWinter$R3_abundance,
                                                                  ifelse(is.na(scoring_new_minTempWinter$R2_abundance) == F,
                                                                         scoring_new_minTempWinter$R2_abundance,
                                                                         scoring_new_minTempWinter$R1_abundance))))
  
#Then calculate the CIT based on the abundance of the latest resurvey  
  CIT_new_minTempWinter <- scoring_new_minTempWinter %>% group_by(plotID) %>% 
    summarise(CIT_new = weighted.mean(mean_minTempWinter,abundance_new,na.rm=T))
  
#For the remaining species (present in both baseline survey and resurvey)  
#First extract the abundance of the latest resurvey with a nested ifelse statement (same logic as before)  
  scoring_remain_minTempWinter$abundance_remain <- ifelse(is.na(scoring_remain_minTempWinter$R5_abundance) == F,
                                                          scoring_remain_minTempWinter$R5_abundance,
                                                          ifelse(is.na(scoring_remain_minTempWinter$R4_abundance) == F,
                                                                 scoring_remain_minTempWinter$R4_abundance,
                                                                 ifelse(is.na(scoring_remain_minTempWinter$R3_abundance) == F,
                                                                        scoring_remain_minTempWinter$R3_abundance,
                                                                        ifelse(is.na(scoring_remain_minTempWinter$R2_abundance) == F,
                                                                               scoring_remain_minTempWinter$R2_abundance,
                                                                               scoring_remain_minTempWinter$R1_abundance))))  
  
#Then calculate the CIT based on the abundance of the latest resurvey  
  CIT_remain_minTempWinter <- scoring_remain_minTempWinter %>% group_by(plotID) %>% 
    summarise(CIT_remain_R = weighted.mean(mean_minTempWinter,abundance_remain,na.rm=T))  
  
#Now the CITs per process will be compared  to the general CIT
  
#By visually comparing the CITs based on the processes (as density plots) and the general CIT for the baseline survey and the resurvey (mean as vertical lines)
  
#First extract the general CIT of the latest resurvey (now spread over multiple columns, preferably in a single column) with a nested ifelse statement
  CIT_minTempWinter$CIT_latest_resurvey <- ifelse(is.na(CIT_minTempWinter$R5_CIT_minTempWinter) == F,
                                                  CIT_minTempWinter$R5_CIT_minTempWinter,
                                                  ifelse(is.na(CIT_minTempWinter$R4_CIT_minTempWinter) == F,
                                                         CIT_minTempWinter$R4_CIT_minTempWinter,
                                                         ifelse(is.na(CIT_minTempWinter$R3_CIT_minTempWinter) == F,
                                                                CIT_minTempWinter$R3_CIT_minTempWinter,
                                                                ifelse(is.na(CIT_minTempWinter$R2_CIT_minTempWinter) == F,
                                                                       CIT_minTempWinter$R2_CIT_minTempWinter,
                                                                       CIT_minTempWinter$R1_CIT_minTempWinter))))
  
#Join the CITs of the three processes to combine them in one figure
  CIT_processes_minTempWinter <- full_join(CIT_disappear_minTempWinter,CIT_new_minTempWinter,by="plotID")
  CIT_processes_minTempWinter <- full_join(CIT_processes_minTempWinter,CIT_remain_minTempWinter, by="plotID")
  colnames(CIT_processes_minTempWinter) <- c("plotID","Species loss","Species gain","Changing abundance","CIT_remain_B")
    
#Convert to wide data format to plot multiple density plots on top of each other
  CIT_processes_minTempWinter <- gather(CIT_processes_minTempWinter,process,CIT,c("Species loss","Species gain","Changing abundance")) 
  colnames(CIT_processes_minTempWinter) <- c("plotID","CIT_remain_B","Species_process","CIT") 
  
#CIT disappearing species vs general CIT  
  ggplot(CIT_disappear_minTempWinter, aes(CIT_disappear)) + geom_density(fill='darkgreen',lwd=1.1,alpha=0.4)+
    theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_minTempWinter$B_CIT_minTempWinter,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    annotate("text",x=-8,y=0.28,label="mean CIT baseline survey",size=3) +
    geom_vline(aes(xintercept = mean(CIT_minTempWinter$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    annotate("text",x=-4,y=0.32,label="mean CIT latest resurvey",size=3) +
    xlab("CIT disappearing species (°C)") +ylab("Probability of occurrence") + ggtitle("Disappeared species") + theme_bw()
  
#CIT newly occurring species vs general CIT  
  ggplot(CIT_new_minTempWinter, aes(CIT_new)) + geom_density(fill='darkgreen',lwd=1.1,alpha=0.4)+
    theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_minTempWinter$B_CIT_minTempWinter,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    annotate("text",x=-8,y=0.28,label="mean CIT baseline survey",size=3) +
    geom_vline(aes(xintercept = mean(CIT_minTempWinter$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    annotate("text",x=-4,y=0.32,label="mean CIT latest resurvey",size=3) +
    xlab("CIT newly occuring species (°C)") +ylab("Probability of occurrence") + ggtitle("Newly occurring species") + theme_bw()
  
#CIT remaining species vs general CIT
  ggplot(CIT_remain_minTempWinter, aes(CIT_remain_R)) + geom_density(fill='darkgreen',lwd=1.1,alpha=0.4)+
    theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_minTempWinter$B_CIT_minTempWinter,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    annotate("text",x=-7.9,y=0.28,label="mean CIT baseline survey",size=3) +
    geom_vline(aes(xintercept = mean(CIT_minTempWinter$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    annotate("text",x=-4.2,y=0.32,label="mean CIT latest resurvey",size=3) +
    xlab("CIT remaining species (°C)") +ylab("Probability of occurrence") + ggtitle("Remaining species") + theme_bw()

#CIT of all processes vs general CIT
  ggplot(CIT_processes_minTempWinter, aes(CIT,color=Species_process,fill=Species_process,linetype=Species_process)) + geom_density(lwd=1.1,alpha=0.4)+
    xlim(-11.5,0) + theme(axis.title = element_text(size=15)) +
    geom_vline(aes(xintercept = mean(CIT_minTempWinter$B_CIT_minTempWinter,na.rm = T)), linetype = "solid", linewidth = 0.75) +
    geom_vline(aes(xintercept = mean(CIT_minTempWinter$CIT_latest_resurvey,na.rm = T)), linetype = "dashed", linewidth = 0.75) +
    xlab("Community-inferred temperature (°C)") + ylab("Probability of occurrence") + theme_bw() +
    scale_fill_manual(name = "Species process",values = c("blue2", "tomato","springgreen4"),breaks=c("Species loss","Species gain","Changing abundance")) + 
    scale_color_manual(name = "Species process",values = c("blue2", "tomato4","springgreen4"),breaks=c("Species loss","Species gain","Changing abundance")) +
    scale_linetype_manual(name = "Species process",values = c("longdash","dotdash","solid"),breaks=c("Species loss","Species gain","Changing abundance"))    
   
#By calculating and comparing the mean CIT per process and the mean general CIT for the baseline survey and resurvey
  mean_CIT_minTempWinter <- data.frame(mean(CIT_minTempWinter$B_CIT_minTempWinter,na.rm=T),mean(CIT_minTempWinter$CIT_latest_resurvey,na.rm=T),
                                       mean(CIT_disappear_minTempWinter$CIT_disappear,na.rm=T),mean(CIT_new_minTempWinter$CIT_new,na.rm=T),
                                       mean(CIT_remain_minTempWinter$CIT_remain_R,na.rm=T))
  colnames(mean_CIT_minTempWinter) <- c("mean_CIT_baseline_survey","mean_CIT_latest_resurvey","mean_CIT_disappear","mean_CIT_new","mean_CIT_remain")
  
#We also want to test whether the CIT significantly differ between the processes
#This is done with first defining a linear mixed model using the process as a fixed effect and the plot as a random effect  
  lme_minTempWinter <- lmer(CIT~process + (1|plotID),data=CIT_processes_minTempWinter)
  summary(lme_minTempWinter)  
  
#Then a posthoc Tukey test is performed to see which CITs of the processes significantly differ from each other    
  emmeans(lme_minTempWinter, list(pairwise ~ process), adjust = "tukey")  
  
#All CITs differ significantly from each other in this order: CIT newly occurring > CIT remaining > CIT disappeared species 
    
  
#The relative contribution of these three processes to temporal changes in the CIT will now be calculated
  
#The delta CIT for the disappeared species is equal to the CIT of the disappeared species since these species only have abundance values
#for the baseline survey and not for the resurvey (so subtraction by zero for delta CIT)
  
#Likewise, the delta CIT for the newly occurring species is equal to the CIT of the newly occurring species since these species only have abundance values  
#for the resurvey and not for the baseline survey (so subtraction by zero for delta CIT)   
  
#The delta CIT for the remaining species is equal to the CIT of the remaining species based on abundances from the resurvey subtracted by 
#the CIT of the remaining species based on abundances from the baseline survey  
  
#Calculate the CIT of the remaining species based on the abundance of the baseline survey
  CIT_remain_B_minTempWinter <- scoring_remain_minTempWinter %>% group_by(plotID) %>% 
    summarise(CIT_remain_B = weighted.mean(mean_minTempWinter,B_abundance,na.rm=T))  
  
#Join both CITs of the remaining species so it is easier to calculate the delta CIT
  CIT_remain_minTempWinter <- full_join(CIT_remain_minTempWinter,CIT_remain_B_minTempWinter,by="plotID")
  
#Subtract the CIT of the resurvey from the CIT of the baseline survey to obtain the delta CIT of the remaining species
  delta_CIT_remain_minTempWinter <- CIT_remain_minTempWinter %>% group_by(plotID) %>% 
    summarise(delta_CIT_remain = CIT_remain_R - CIT_remain_B)
  
#Join all three delta CITs into one data frame for easier further calculations and a more clear overview
  delta_CIT_processes_minTempWinter <- full_join(delta_CIT_remain_minTempWinter,CIT_disappear_minTempWinter,by="plotID")
  delta_CIT_processes_minTempWinter <- full_join(delta_CIT_processes_minTempWinter,CIT_new_minTempWinter,by="plotID") 
  colnames(delta_CIT_processes_minTempWinter) <- c("plotID","delta_CIT_remain_minTempWinter","delta_CIT_disappear_minTempWinter","delta_CIT_new_minTempWinter")  
  
#Relative contribution of disappeared species per plot
  rel_contr_processes_minTempWinter <- delta_CIT_processes_minTempWinter
  rel_contr_processes_minTempWinter$rel_contr_disappear <- abs(rel_contr_processes_minTempWinter$delta_CIT_disappear_minTempWinter)/
    (abs(rel_contr_processes_minTempWinter$delta_CIT_remain_minTempWinter)+abs(rel_contr_processes_minTempWinter$delta_CIT_disappear_minTempWinter)
     +abs(rel_contr_processes_minTempWinter$delta_CIT_new_minTempWinter))
  
#Relative contribution of newly occurring species per plot
  rel_contr_processes_minTempWinter$rel_contr_new <- abs(rel_contr_processes_minTempWinter$delta_CIT_new_minTempWinter)/
    (abs(rel_contr_processes_minTempWinter$delta_CIT_remain_minTempWinter)+abs(rel_contr_processes_minTempWinter$delta_CIT_disappear_minTempWinter)
     +abs(rel_contr_processes_minTempWinter$delta_CIT_new_minTempWinter))
  
#Relative contribution of remaining species per plot
  rel_contr_processes_minTempWinter$rel_contr_remain <- abs(rel_contr_processes_minTempWinter$delta_CIT_remain_minTempWinter)/
    (abs(rel_contr_processes_minTempWinter$delta_CIT_remain_minTempWinter)+abs(rel_contr_processes_minTempWinter$delta_CIT_disappear_minTempWinter)
     +abs(rel_contr_processes_minTempWinter$delta_CIT_new_minTempWinter))
  
#Mean contribution of the three processes for all plots
  mean_rel_contr_processes_minTempWinter <- data.frame(mean(rel_contr_processes_minTempWinter$rel_contr_disappear,na.rm=T),
                                                       mean(rel_contr_processes_minTempWinter$rel_contr_new,na.rm=T),
                                                       mean(rel_contr_processes_minTempWinter$rel_contr_remain,na.rm=T))
  colnames(mean_rel_contr_processes_minTempWinter) <- c("mean contribution disappearing","mean contribution newly occurring","mean contribution remaining")  
  

# ---- 8.4 Relationship between delta CIT components and all other variables ----

#Join the delta CIT data for the different processes with the rest of the plot data
  plotdata_ForestREplot_EU <- left_join(plotdata_ForestREplot_EU,delta_CIT_processes_maxTempGS,by="plotID")
  plotdata_ForestREplot_EU <- left_join(plotdata_ForestREplot_EU,delta_CIT_processes_minTempSpring,by="plotID")
  plotdata_ForestREplot_EU <- left_join(plotdata_ForestREplot_EU,delta_CIT_processes_minTempWinter,by="plotID")
    
#Visualise the relation between the delta CIT components for each of the three temperatures and canopy cover changes
  
#For the maximum temperature during growing season and disappearing species 
  ggplot(plotdata_ForestREplot_EU,aes(canopy_cover_changes,delta_CIT_disappear_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT (°C)")
  
#For the maximum temperature during growing season and remaining species 
  ggplot(plotdata_ForestREplot_EU,aes(canopy_cover_changes,delta_CIT_remain_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT (°C)")
  
#For the maximum temperature during growing season and newly occurring species 
  ggplot(plotdata_ForestREplot_EU,aes(canopy_cover_changes,delta_CIT_new_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT (°C)")
  
#For the minimum temperature during spring and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(canopy_cover_changes,delta_CIT_disappear_minTempSpring)) + geom_point() + 
    geom_smooth(method="glm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT (°C)") + theme_bw() 
  
#For the minimum temperature during spring and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(canopy_cover_changes,delta_CIT_remain_minTempSpring)) + geom_point() + 
    geom_smooth(method="glm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT (°C)") + theme_bw() 
  
#For the minimum temperature during spring and newly occuring species
  ggplot(plotdata_ForestREplot_EU,aes(canopy_cover_changes,delta_CIT_new_minTempSpring)) + geom_point() + 
    geom_smooth(method="glm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT (°C)") + theme_bw() 
  
#For the minimum temperature during winter and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(canopy_cover_changes,delta_CIT_disappear_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT (°C)") + theme_bw()
  
#For the minimum temperature during winter and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(canopy_cover_changes,delta_CIT_remain_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT (°C)") + theme_bw()
  
#For the minimum temperature during winter and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(canopy_cover_changes,delta_CIT_new_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("canopy cover changes (%)") +
    ylab("delta CIT (°C)") + theme_bw()
  
#Visualise the relation between delta CIT components for each of the three temperatures and macroclimate changes per year
  
#For the maximum temperature during growing season and disappearing species 
  ggplot(plotdata_ForestREplot_EU,aes(macroclimate_change_per_year,delta_CIT_disappear_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT (°C)")
  
#For the maximum temperature during growing season and remaining species 
  ggplot(plotdata_ForestREplot_EU,aes(macroclimate_change_per_year,delta_CIT_remain_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT (°C)")
  
#For the maximum temperature during growing season and newly occurring species 
  ggplot(plotdata_ForestREplot_EU,aes(macroclimate_change_per_year,delta_CIT_new_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT (°C)")
  
#For the minimum temperature during spring and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(macroclimate_change_per_year,delta_CIT_disappear_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during spring and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(macroclimate_change_per_year,delta_CIT_remain_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(macroclimate_change_per_year,delta_CIT_new_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(macroclimate_change_per_year,delta_CIT_disappear_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(macroclimate_change_per_year,delta_CIT_remain_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(macroclimate_change_per_year,delta_CIT_new_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("macroclimate change (°C/yr)") + 
    ylab("delta CIT (°C)") 
  
#Visualize the relationship between the delta CIT components for the three temperatures and MI based on climate offset (27 combinations)
  
#For the maximum temperature during growing season, disappearing species and offset based on mean annual temperature
  ggplot(plotdata_ForestREplot_EU,aes(MI_MatT_Offset,delta_CIT_disappear_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season, remaining species and offset based on mean annual temperature
  ggplot(plotdata_ForestREplot_EU,aes(MI_MatT_Offset,delta_CIT_remain_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season, newly occurring species and offset based on mean annual temperature
  ggplot(plotdata_ForestREplot_EU,aes(MI_MatT_Offset,delta_CIT_new_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring, disappearing species and offset based on mean annual temperature  
  ggplot(plotdata_ForestREplot_EU,aes(MI_MatT_Offset,delta_CIT_disappear_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during spring, remaining species and offset based on mean annual temperature  
  ggplot(plotdata_ForestREplot_EU,aes(MI_MatT_Offset,delta_CIT_remain_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring, newly occurring species and offset based on mean annual temperature  
  ggplot(plotdata_ForestREplot_EU,aes(MI_MatT_Offset,delta_CIT_new_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter, disappearing species and offset based on mean annual temperature  
  ggplot(plotdata_ForestREplot_EU,aes(MI_MatT_Offset,delta_CIT_disappear_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during winter, remaining species and offset based on mean annual temperature  
  ggplot(plotdata_ForestREplot_EU,aes(MI_MatT_Offset,delta_CIT_remain_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during winter, newly occurring species and offset based on mean annual temperature  
  ggplot(plotdata_ForestREplot_EU,aes(MI_MatT_Offset,delta_CIT_new_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the maximum temperature during growing season, disappearing species and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_ForestREplot_EU,aes(MI_MinT_Offset,delta_CIT_disappear_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season, remaining species and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_ForestREplot_EU,aes(MI_MinT_Offset,delta_CIT_remain_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season, newly occurring species and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_ForestREplot_EU,aes(MI_MinT_Offset,delta_CIT_new_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring, disappearing species and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_ForestREplot_EU,aes(MI_MinT_Offset,delta_CIT_disappear_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring, remaining species and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_ForestREplot_EU,aes(MI_MinT_Offset,delta_CIT_remain_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring, newly occurring species and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_ForestREplot_EU,aes(MI_MinT_Offset,delta_CIT_new_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter, disappearing species and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_ForestREplot_EU,aes(MI_MinT_Offset,delta_CIT_disappear_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during winter, remaining species and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_ForestREplot_EU,aes(MI_MinT_Offset,delta_CIT_remain_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during winter, newly occurring species and offset based on minimum temperature during winter and spring 
  ggplot(plotdata_ForestREplot_EU,aes(MI_MinT_Offset,delta_CIT_new_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the maximum temperature during growing season, disappearing species and offset based on maximum temperature during summer
  ggplot(plotdata_ForestREplot_EU,aes(MI_MaxT_Offset,delta_CIT_disappear_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season, remaining species and offset based on maximum temperature during summer
  ggplot(plotdata_ForestREplot_EU,aes(MI_MaxT_Offset,delta_CIT_remain_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season, newly occurring species and offset based on maximum temperature during summer
  ggplot(plotdata_ForestREplot_EU,aes(MI_MaxT_Offset,delta_CIT_new_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring, disappearing species and offset based on maximum temperature during summer
  ggplot(plotdata_ForestREplot_EU,aes(MI_MaxT_Offset,delta_CIT_disappear_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")
  
#For the minimum temperature during spring, remaining species and offset based on maximum temperature during summer
  ggplot(plotdata_ForestREplot_EU,aes(MI_MaxT_Offset,delta_CIT_remain_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during spring, newly occurring species and offset based on maximum temperature during summer
  ggplot(plotdata_ForestREplot_EU,aes(MI_MaxT_Offset,delta_CIT_new_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during winter, disappearing species and offset based on maximum temperature during summer
  ggplot(plotdata_ForestREplot_EU,aes(MI_MaxT_Offset,delta_CIT_disappear_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during winter, remaining species and offset based on maximum temperature during summer
  ggplot(plotdata_ForestREplot_EU,aes(MI_MaxT_Offset,delta_CIT_remain_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")
  
#For the minimum temperature during winter, newly occurring species and offset based on maximum temperature during summer
  ggplot(plotdata_ForestREplot_EU,aes(MI_MaxT_Offset,delta_CIT_new_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")

#Visualize the relationship between the delta CIT component for the three temperatures and the MI based on climate change magnitude
  
#For the maximum temperature during growing season and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(MI_Warming,delta_CIT_disappear_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(MI_Warming,delta_CIT_remain_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(MI_Warming,delta_CIT_new_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(MI_Warming,delta_CIT_disappear_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")     
  
#For the minimum temperature during spring and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(MI_Warming,delta_CIT_remain_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(MI_Warming,delta_CIT_new_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(MI_Warming,delta_CIT_disappear_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(MI_Warming,delta_CIT_remain_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(MI_Warming,delta_CIT_new_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")   

#Visualize the relationship between the delta CIT component for the three temperatures and the synthesis MI
  
#For the maximum temperature during growing season and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(MIs_synthesis,delta_CIT_disappear_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(MIs_synthesis,delta_CIT_remain_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(MIs_synthesis,delta_CIT_new_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(MIs_synthesis,delta_CIT_disappear_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)")     
  
#For the minimum temperature during spring and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(MIs_synthesis,delta_CIT_remain_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(MIs_synthesis,delta_CIT_new_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(MIs_synthesis,delta_CIT_disappear_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(MIs_synthesis,delta_CIT_remain_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during winter and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(MIs_synthesis,delta_CIT_new_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("MI value") + 
    ylab("delta CIT (°C)") 
  
#Visualize the relationship between the delta CIT component and the mean N deposition for the three temperatures  
  
#For the maximum temperature during growing season and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(N_mean,delta_CIT_disappear_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(N_mean,delta_CIT_remain_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(N_mean,delta_CIT_new_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(N_mean,delta_CIT_disappear_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT (°C)")     
  
#For the minimum temperature during spring and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(N_mean,delta_CIT_remain_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT (°C)")   
  
#For the minimum temperature during spring and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(N_mean,delta_CIT_new_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT (°C)")   
  
#For the minimum temperature during winter and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(N_mean,delta_CIT_disappear_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT (°C)")     
  
#For the minimum temperature during winter and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(N_mean,delta_CIT_remain_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT (°C)")  
  
#For the minimum temperature during winter and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(N_mean,delta_CIT_new_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("mean N deposition (kg/ha/year)") + 
    ylab("delta CIT (°C)")  
  
#Visualize the relationship between the delta CIT component and the plot size for the three temperatures  
  
#For the maximum temperature during growing season and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(plot_size,delta_CIT_disappear_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(plot_size,delta_CIT_remain_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT (°C)") 
  
#For the maximum temperature during growing season and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(plot_size,delta_CIT_new_maxTempGS)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT (°C)") 
  
#For the minimum temperature during spring and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(plot_size,delta_CIT_disappear_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT (°C)")
  
#For the minimum temperature during spring and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(plot_size,delta_CIT_remain_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT (°C)")    
  
#For the minimum temperature during spring and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(plot_size,delta_CIT_new_minTempSpring)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT (°C)")    
  
#For the minimum temperature during winter and disappearing species
  ggplot(plotdata_ForestREplot_EU,aes(plot_size,delta_CIT_disappear_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT (°C)")     
  
#For the minimum temperature during winter and remaining species
  ggplot(plotdata_ForestREplot_EU,aes(plot_size,delta_CIT_remain_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT (°C)")    
  
#For the minimum temperature during winter and newly occurring species
  ggplot(plotdata_ForestREplot_EU,aes(plot_size,delta_CIT_new_minTempWinter)) + geom_point() + 
    geom_smooth(method="lm" , color="red", fill="#69b3a2", se=TRUE) + xlab("plot size (m²)") + 
    ylab("delta CIT (°C)")    
    
  
# ---- 9. REGRESSION ----
  
#The generalized least squares (gls) regression will be used to investigate the relationship between the delta CIT per year (response variable)
#and the explanatory variables (MI, plot size, mean N deposition, canopy cover changes & macroclimate changes)  

#Missing data is removed so that only plots who have data for all variables remain
  plotdata_CIT <- filter(plotdata_CIT, is.nan(macroclimate_change_per_year) == F)
  plotdata_CIT <- filter(plotdata_CIT,is.na(canopy_cover_changes)==F)
  plotdata_CIT <- filter(plotdata_CIT,is.na(delta_CIT_per_year_maxTempGS)==F)
  plotdata_CIT <- filter(plotdata_CIT,is.na(delta_CIT_per_year_minTempSpring)==F)
  plotdata_CIT <- filter(plotdata_CIT,is.na(delta_CIT_per_year_minTempWinter)==F)
  plotdata_CIT <- filter(plotdata_CIT,is.na(MIs_synthesis)==F)
  plotdata_CIT <- filter(plotdata_CIT,is.na(N_mean)==F)
  plotdata_CIT <- filter(plotdata_CIT,is.na(plot_size)==F)
  
#Check if spatial autocorrelation is present, because this can and has to be taken into account in the generalized least squares regression  
  
#Before the variables are tested for spatial autocorrelation, it needs to be checked if there are any duplicate coordinates
#When duplicate coordinates are present, these need to be adapted otherwise tests for spatial autocorrelation cannot be performed
#and spatial autocorrelation cannot be incorporated in the regression (zero distance)
  duplicate_coordinates <- plotdata_CIT[duplicated(plotdata_CIT[,c("latitude","longitude")]),]
  
#There are 210 duplicate coordinates present, so these need to be adapted. This is done with an ifelse statement
#If duplicate coordinates are present in the latitude, introduce a very small coordinate change (0.000001 degree) in latitude so that the duplication is removed
  plotdata_CIT$latitude <- ifelse(duplicated(plotdata_CIT[,"latitude"])==T,plotdata_CIT$latitude+runif(210,0,0.000001),plotdata_CIT$latitude)
  
#Check again for duplicates
  duplicate_coordinates <- plotdata_CIT[duplicated(plotdata_CIT[,c("latitude","longitude")]),]  #No more duplicate coordinates present      
  
#The spatial autocorrelation is tested with the Moran's test: if significant, then the variable is spatially autocorrelated  
  
#Preparation for the Moran's test
#Organize the longitude and latitude variables  
  geo <- cbind(plotdata_CIT$longitude,plotdata_CIT$latitude)
#Produce a distance matrix (Euclidean) from the longitude and latitude
  samples.dist <- as.matrix(dist(geo))
#Convert this to a proximity matrix by inversing the distance matrix and give diagonal values zero   
  samples.dist.inv <- 1/samples.dist
  diag(samples.dist.inv) <- 0
  
#Here we perform the Moran's test for positive spatial autocorrelation (similar values are close to each other)    
  Moran.I(plotdata_CIT$delta_CIT_per_year_maxTempGS,samples.dist.inv,alternative="greater") 
  Moran.I(plotdata_CIT$delta_CIT_per_year_minTempSpring,samples.dist.inv,alternative="greater") 
  Moran.I(plotdata_CIT$delta_CIT_per_year_minTempWinter,samples.dist.inv,alternative="greater") 
  Moran.I(plotdata_CIT$canopy_cover_changes,samples.dist.inv,alternative="greater") 
  Moran.I(plotdata_CIT$macroclimate_change_per_year,samples.dist.inv,alternative="greater") 
  Moran.I(plotdata_CIT$MI_MatT_Offset,samples.dist.inv,alternative="greater") 
  Moran.I(plotdata_CIT$MI_MinT_Offset,samples.dist.inv,alternative="greater") 
  Moran.I(plotdata_CIT$MI_MaxT_Offset,samples.dist.inv,alternative="greater") 
  Moran.I(plotdata_CIT$MIs_synthesis,samples.dist.inv,alternative="greater") 
  Moran.I(plotdata_CIT$MI_Warming,samples.dist.inv,alternative="greater")
  Moran.I(plotdata_CIT$N_mean,samples.dist.inv,alternative = "greater")
  Moran.I(plotdata_CIT$plot_size,samples.dist.inv,alternative = "greater")
  
#Every Moran's test is significant, so all variables are positively spatially autocorrelated, so the residuals are (auto)correlated as well  

#Since the variables are spatially autocorrelated, the correlation structure needs to be defined.
#There are five options: exponential, Gaussian, linear, rational quadratics & spherical spatial correlation
#The correlation structure was chosen by first running five models, each with a different correlation structure
#Then the correlation structure of the best performing model (lowest AIC) was chosen
  
  gls_Exp <- gls(delta_CIT_per_year_maxTempGS ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming + macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, 
                 correlation = corExp(form = ~ longitude + latitude,nugget=T),
                 data = plotdata_CIT)

  gls_Gaus <- gls(delta_CIT_per_year_maxTempGS ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming + macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, 
                 correlation = corGaus(form = ~ longitude + latitude,nugget=T),
                 data = plotdata_CIT)
  
  gls_Lin <- gls(delta_CIT_per_year_maxTempGS ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming + macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, 
                 correlation = corLin(form = ~ longitude + latitude,nugget=T),
                 data = plotdata_CIT)
  
  gls_Ratio <- gls(delta_CIT_per_year_maxTempGS ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming + macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, 
                   correlation = corRatio(form = ~ longitude + latitude,nugget=T),
                   data = plotdata_CIT)
  
  gls_Spher <- gls(delta_CIT_per_year_maxTempGS ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming + macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, 
                   correlation = corSpher(form = ~ longitude + latitude,nugget=T),
                   data = plotdata_CIT)  

  gls_AIC <- AIC(gls_Exp,gls_Gaus,gls_Lin,gls_Ratio,gls_Spher)  
  
#Exponential spatial autocorrelation structure performs best, so this will be used in the models  
  
  
# ---- 9.1 Delta CIT per year vs MIs ----
      
#First check if the relationship between the three delta CITs and the different explanatory variables are linear, otherwise linearize them with transformations
#See section 7 for this -> all relationships are either linear or there is no clear relationship, so no transformations are needed
  
#Define a model per delta CIT including all MIs (except the synthesis MI, otherwise singular fit) together with the canopy cover changes, 
#macroclimate changes, mean N deposition and plot size as explanatory variables 
  gls_maxTempGS_1 <- gls(delta_CIT_per_year_maxTempGS ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming + MIs_synthesis+
                         macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size,
                         correlation = corExp(form = ~ longitude + latitude,nugget=T),
                         data = plotdata_CIT)
  
  gls_minTempSpring_1 <- gls(delta_CIT_per_year_minTempSpring ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                             macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size,
                             correlation = corExp(form = ~ longitude + latitude,nugget=T),
                             data = plotdata_CIT)
  
  gls_minTempWinter_1 <- gls(delta_CIT_per_year_minTempWinter ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                             macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size,
                             correlation = corExp(form = ~ longitude + latitude,nugget=T),
                             data = plotdata_CIT)
  
#Define a model per delta CIT with the synthesis MI, the canopy cover changes, 
#macroclimate changes, mean N deposition and plot size as explanatory variables   
  gls_maxTempGS_2 <- gls(delta_CIT_per_year_maxTempGS ~ MIs_synthesis + 
                         macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size,
                         correlation = corExp(form = ~ longitude + latitude,nugget=T),
                         data = plotdata_CIT)  

  gls_minTempSpring_2 <- gls(delta_CIT_per_year_minTempSpring ~ MIs_synthesis + 
                             macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size,
                             correlation = corExp(form = ~ longitude + latitude,nugget=T),
                             data = plotdata_CIT)
  
  gls_minTempWinter_2 <- gls(delta_CIT_per_year_minTempWinter ~ MIs_synthesis + 
                             macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size,
                             correlation = corExp(form = ~ longitude + latitude,nugget=T),
                             data = plotdata_CIT) 
    
#Check if the assumptions are met for every model
   
#The assumptions of gls regression are:
  #The variance of the residuals is constant (homoscedasticity) 
  #Residuals are normally distributed
  #Absence of multicollinearity

#Check for homoscedasticity
  plot(gls_maxTempGS_1)
  plot(gls_maxTempGS_2)
  plot(gls_minTempSpring_1)
  plot(gls_minTempSpring_2)
  plot(gls_minTempWinter_1)
  plot(gls_minTempWinter_2)
  
#There are no apparent violations of the homoscedasticity principle (variability among the variables seems stable for every model)
  
#Check for normal distribution of residuals
  
#By plotting the fitted values against the residuals  
  plot(fitted(gls_maxTempGS_1),residuals(gls_maxTempGS_1))
  abline(h=0,lty=3) 
  plot(fitted(gls_maxTempGS_2),residuals(gls_maxTempGS_2))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempSpring_1),residuals(gls_minTempSpring_1))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempSpring_2),residuals(gls_minTempSpring_2))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempWinter_1),residuals(gls_minTempWinter_1))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempWinter_2),residuals(gls_minTempWinter_2))
  abline(h=0,lty=3)  

#It can be seen that all residual values are close to zero for every model, meaning that it can be assumed that the residuals are normally distributed for all models   

#check for absence of multicollinearity

#This was tested by calculating the Variance Inflation Factor (VIF)  
  check_collinearity(gls_maxTempGS_1)
  check_collinearity(gls_maxTempGS_2)
  check_collinearity(gls_minTempSpring_1)
  check_collinearity(gls_minTempSpring_2)
  check_collinearity(gls_minTempWinter_1)
  check_collinearity(gls_minTempWinter_2)
  
#The VIF was close to 1 for all variables in each model, meaning that multicollinearity is not a problem for every model
    
#All assumptions are met, so the models can be used to investigate the relationships between the delta CITs and the MIs


# ---- 9.2 Delta CIT components vs MIs ----
  
#Remove duplicate coordinates
  plotdata_ForestREplot_EU$latitude <- ifelse(duplicated(plotdata_ForestREplot_EU[,"latitude"])==T,
                                              plotdata_ForestREplot_EU$latitude+runif(210,0,0.000001),plotdata_ForestREplot_EU$latitude)  
  
#First check if the relationship between the three delta CITs (and the three components for each CIT) and the different explanatory variables are linear, 
#otherwise linearize them with transformations
#See section 8.4 for this -> all relationships are either linear or there is no clear relationship, so no transformations are needed
  
#Define a model per delta CIT component and temperature including all MIs (except the synthesis MI) together with the canopy cover changes, 
#macroclimate changes, mean N deposition and plot size as explanatory variables 
  gls_maxTempGS_disappear_1 <- gls(delta_CIT_disappear_maxTempGS ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                                   macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                   correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                   data = plotdata_ForestREplot_EU)
  
  gls_maxTempGS_remain_1 <- gls(delta_CIT_remain_maxTempGS ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                                   macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                   correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                   data = plotdata_ForestREplot_EU)
  
  gls_maxTempGS_new_1 <- gls(delta_CIT_new_maxTempGS ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                                   macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                   correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                   data = plotdata_ForestREplot_EU)
  
  gls_minTempSpring_disappear_1 <- gls(delta_CIT_disappear_minTempSpring ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                                        macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                        correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                        data = plotdata_ForestREplot_EU)
  
  gls_minTempSpring_remain_1 <- gls(delta_CIT_remain_minTempSpring ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                                         macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                         correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                         data = plotdata_ForestREplot_EU)
  
  gls_minTempSpring_new_1 <- gls(delta_CIT_new_minTempSpring ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                                       macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                       correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                       data = plotdata_ForestREplot_EU)
  
  gls_minTempWinter_disappear_1 <- gls(delta_CIT_disappear_minTempWinter ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                                       macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                       correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                       data = plotdata_ForestREplot_EU)
  
  gls_minTempWinter_remain_1 <- gls(delta_CIT_remain_minTempWinter ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                                       macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                       correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                       data = plotdata_ForestREplot_EU)
  
  gls_minTempWinter_new_1 <- gls(delta_CIT_new_minTempWinter ~ MI_MaxT_Offset + MI_MinT_Offset + MI_MatT_Offset + MI_Warming +
                                       macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                       correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                       data = plotdata_ForestREplot_EU)
  
#Define a model per delta CIT component and temperature with the synthesis MI, the canopy cover changes, 
#macroclimate changes, mean N deposition and plot size as explanatory variables   
  gls_maxTempGS_disappear_2 <- gls(delta_CIT_disappear_maxTempGS ~ MIs_synthesis +
                                   macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                   correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                   data = plotdata_ForestREplot_EU)
  
  gls_maxTempGS_remain_2 <- gls(delta_CIT_remain_maxTempGS ~ MIs_synthesis +
                                macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                data = plotdata_ForestREplot_EU)
  
  gls_maxTempGS_new_2 <- gls(delta_CIT_new_maxTempGS ~ MIs_synthesis +
                             macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                             correlation = corExp(form = ~ longitude + latitude,nugget=T),
                             data = plotdata_ForestREplot_EU)
  
  gls_minTempSpring_disappear_2 <- gls(delta_CIT_disappear_minTempSpring ~ MIs_synthesis +
                                       macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                       correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                       data = plotdata_ForestREplot_EU)
  
  gls_minTempSpring_remain_2 <- gls(delta_CIT_remain_minTempSpring ~ MIs_synthesis +
                                    macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                    correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                    data = plotdata_ForestREplot_EU)
  
  gls_minTempSpring_new_2 <- gls(delta_CIT_new_minTempSpring ~ MIs_synthesis +
                                 macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                 correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                 data = plotdata_ForestREplot_EU)
  
  gls_minTempWinter_disappear_2 <- gls(delta_CIT_disappear_minTempWinter ~ MIs_synthesis +
                                       macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                       correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                       data = plotdata_ForestREplot_EU)
  
  gls_minTempWinter_remain_2 <- gls(delta_CIT_remain_minTempWinter ~ MIs_synthesis +
                                    macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                    correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                    data = plotdata_ForestREplot_EU)
  
  gls_minTempWinter_new_2 <- gls(delta_CIT_new_minTempWinter ~ MIs_synthesis +
                                 macroclimate_change_per_year + canopy_cover_changes + N_mean + plot_size, na.action = na.omit,
                                 correlation = corExp(form = ~ longitude + latitude,nugget=T),
                                 data = plotdata_ForestREplot_EU)
  
#Check if the assumptions are met for every model
  
#The assumptions of gls regression are:
#The variance of the residuals is constant (homoscedasticity) 
#Residuals are normally distributed
#Absence of multicollinearity
  
  #Check for homoscedasticity
  plot(gls_maxTempGS_disappear_1)
  plot(gls_maxTempGS_disappear_2)
  plot(gls_maxTempGS_remain_1)
  plot(gls_maxTempGS_remain_2)
  plot(gls_maxTempGS_new_1)
  plot(gls_maxTempGS_new_2)
  plot(gls_minTempSpring_disappear_1)
  plot(gls_minTempSpring_disappear_2)
  plot(gls_minTempSpring_remain_1)
  plot(gls_minTempSpring_remain_2)
  plot(gls_minTempSpring_new_1)
  plot(gls_minTempSpring_new_2)
  plot(gls_minTempWinter_disappear_1)
  plot(gls_minTempWinter_disappear_2)
  plot(gls_minTempWinter_remain_1)
  plot(gls_minTempWinter_remain_2)
  plot(gls_minTempWinter_new_1)
  plot(gls_minTempWinter_new_2)
  
  #There are no apparent violations of the homoscedasticity principle (variability among the variables seems stable for every model)
  
  #Check for normal distribution of residuals
  
  #By plotting the fitted values against the residuals  
  plot(fitted(gls_maxTempGS_disappear_1),residuals(gls_maxTempGS_disappear_1))
  abline(h=0,lty=3) 
  plot(fitted(gls_maxTempGS_disappear_2),residuals(gls_maxTempGS_disappear_2))
  abline(h=0,lty=3) 
  plot(fitted(gls_maxTempGS_remain_1),residuals(gls_maxTempGS_remain_1))
  abline(h=0,lty=3) 
  plot(fitted(gls_maxTempGS_remain_2),residuals(gls_maxTempGS_remain_2))
  abline(h=0,lty=3) 
  plot(fitted(gls_maxTempGS_new_1),residuals(gls_maxTempGS_new_1))
  abline(h=0,lty=3) 
  plot(fitted(gls_maxTempGS_new_2),residuals(gls_maxTempGS_new_2))
  abline(h=0,lty=3) 
  plot(fitted(gls_minTempSpring_disappear_1),residuals(gls_minTempSpring_disappear_1))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempSpring_disappear_2),residuals(gls_minTempSpring_disappear_2))
  abline(h=0,lty=3) 
  plot(fitted(gls_minTempSpring_remain_1),residuals(gls_minTempSpring_remain_1))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempSpring_remain_2),residuals(gls_minTempSpring_remain_2))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempSpring_new_1),residuals(gls_minTempSpring_new_1))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempSpring_new_2),residuals(gls_minTempSpring_new_2))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempWinter_disappear_1),residuals(gls_minTempWinter_disappear_1))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempWinter_disappear_2),residuals(gls_minTempWinter_disappear_2))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempWinter_remain_1),residuals(gls_minTempWinter_remain_1))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempWinter_remain_2),residuals(gls_minTempWinter_remain_2))
  abline(h=0,lty=3) 
  plot(fitted(gls_minTempWinter_new_1),residuals(gls_minTempWinter_new_1))
  abline(h=0,lty=3)  
  plot(fitted(gls_minTempWinter_new_2),residuals(gls_minTempWinter_new_2))
  abline(h=0,lty=3) 
  
  #It can be seen that all residual values are close to zero for every model, meaning that it can be assumed that the residuals are normally distributed for all models   
  
  #check for absence of multicollinearity
  
  #This was tested by calculating the Variance Inflation Factor (VIF)  
  check_collinearity(gls_maxTempGS_disappear_1)
  check_collinearity(gls_maxTempGS_disappear_2)
  check_collinearity(gls_maxTempGS_remain_1)
  check_collinearity(gls_maxTempGS_remain_2)
  check_collinearity(gls_maxTempGS_new_1)
  check_collinearity(gls_maxTempGS_new_2)
  check_collinearity(gls_minTempSpring_disappear_1)
  check_collinearity(gls_minTempSpring_disappear_2)
  check_collinearity(gls_minTempSpring_remain_1)
  check_collinearity(gls_minTempSpring_remain_2)
  check_collinearity(gls_minTempSpring_new_1)
  check_collinearity(gls_minTempSpring_new_2)
  check_collinearity(gls_minTempWinter_disappear_1)
  check_collinearity(gls_minTempWinter_disappear_2)
  check_collinearity(gls_minTempWinter_remain_1)
  check_collinearity(gls_minTempWinter_remain_2)
  check_collinearity(gls_minTempWinter_new_1)
  check_collinearity(gls_minTempWinter_new_2)
  
  #The VIF was close to 1 for all variables in each model, meaning that multicollinearity is not a problem for every model
  
  #All assumptions are met, so the models can be used to investigate the relationships between the delta CITs and the MIs  
  

  
  
  
  
  
