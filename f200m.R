# STARTING ALL OVER #
####Load libraries####
library(BBmisc)
library(ggplot2)
library("scales")
library(colorRamps)
library(relaimpo)
library(tidyverse)
library(car)
library(ape)
library(DAAG)
library(reshape2)
library(ggmap)
library(lubridate)
library(data.table) # v1.9.6+
library(tidyquant)
library(scales)
library(RColorBrewer)
library(pracma)
library(magrittr)
library(dplyr)
library(lubridate)
library(miscTools)
library(rgdal)
library(caTools)
library(zoo)
library(smooth)
library(pracma)
library(anytime)
library(randomcoloR)
# library(devtools)
# install_github("dkahle/ggmap")
# library(ggmap)

source(paste(getwd(),"functions",'make_lur_rebuilt.R', sep = "/"))
source(paste(getwd(),"functions",'run_LUR_all_rebuilt.R', sep = "/"))

#### data files 
datafile <- "Data_clip_200m.csv"
BCfile <- "BlackCarbon_200m_driveday.csv"
covarsfile <- "Covariates_clip_200m.csv"
centroidsfile <- "Fish_200m_select2.csv"
foodtrucksfile <- "FoodTruckCounts.csv"
inversedistHWYfile <- "InvDist_Hwy_200m.csv"
inversedistMAJfile <- "InvDist_maj_200m.csv"


#### mobile data importing
df <- read_csv(datafile) %>%
      drop_na(OID_1) %>%
      filter(OID_1 %in% cf$NEAR_FID) %>%
      rename(NEAR_FID = OID_1)

BCdf_driveday <- read.csv(BCfile) %>% # impport driveday-ed BC data to append
                 filter(NEAR_FID %in% unique(df_driveday$NEAR_FID)) %>%
                 mutate(BC = BC/1000) %>% # convert to ug from ng
                 select(NEAR_FID,BC)


#### covariate data importing 
cf <- read_csv(covarsfile) %>%
      select(-NEAR_FID) %>%
      rename(NEAR_FID = OID_2)
      
centroids <- read_csv(centroidsfile) %>%
             select(c(OID_1, Latitude, Longitude)) %>%
             rename(NEAR_FID = OID_1) %>%
             filter(NEAR_FID %in% cf$NEAR_FID)

### additional variables added later: try to do import / cleaning all in one step w dplyr ###
foodtrucks <- read_csv(foodtrucksfile) %>%                    #read file
              select(-c(X1, Latitude, Longitude)) %>%         #get rid of non-essential cols
              filter(OID_1 %in% cf[ , "NEAR_FID"]) %>%        #only grid cells in the big covar list
              rename(NEAR_FID = OID_1)                        #rename the grid cell id as "NEAR_FID"

invdistHwy <- read.csv(inversedistHWYfile) %>% 
              select(-OID) %>%
              filter(OID_1 %in% cf[ , "NEAR_FID"]) %>%
              rename(NEAR_FID = OID_1,
                     InvDistHwy = MEAN_InvDi)

invdistMaj <- read_csv(inversedistMAJfile) %>% 
              select(-OID) %>%
              filter(OID_1 %in% cf[ , "NEAR_FID"]) %>%
              rename(NEAR_FID = OID_1,
                     InvDistMaj = MEAN_InvDi)


##### rename all cells in each df, because of stupid 10 character thing #####
names <- colnames(read.csv("LURdf_good.csv"))
inds <- c(1,2,174,175,176,177,184,185,186, 192) #manually figured out which columns i dont want
for(ind in rev(inds)){ ##use rev so it starts with last and goes to first
  cf[[ind]] <- NULL
}
  names <- names[2:length(names)]
  oldnames <- colnames(cf)
  cf <- cf %>% rename_at(vars(oldnames), function(x) names) #the renaming

##calculate means for each
cf_byCell <- as.tbl(cf) %>%
      group_by(NEAR_FID) %>% 
      summarise_all(funs(mean))

#### joining one-by-one the added covariates i've constructed
      #### for any additional covariates, just clean the dataframes and then add like here to the 'clean' cf dataframe
cf_byCell <- left_join(cf_byCell, foodtrucks, by = "NEAR_FID") #add food truck data!
cf_byCell <- left_join(cf_byCell, invdistHwy, by = "NEAR_FID") 
cf_byCell <- left_join(cf_byCell, invdistMaj, by = "NEAR_FID")

## get rid of bad columns - data ####
inds_data <- c(1,2,27:33) ##manually figured out which columns I dont want
for(ind in rev(inds_data)){
  df[[ind]] <- NULL
}

### calculate some time stuff for the mobile df ####
df$datesOnly <- as.POSIXct(strptime(as.character(df$Local_time), "%m/%d/%Y"))
df$datetime <- as.POSIXct(strptime(as.character(df$Local_time), "%m/%d/%Y %H:%M:%S")) ###time switch not right..

df_grouped <- df %>% 
              group_by(datesOnly, NEAR_FID) %>%
              summarize(count = n(),
                        Lat = mean(Lat),
                        Lon = mean(Lon),
                        CO_20sMean = mean(CO_20sMean),
                        CO2_20sMea = mean(CO2_20sMea),
                        PN_20sMean = mean(PN_20sMean),
                        HROrg = mean(HROrg),
                        HRNH4 = mean(HRNH4),
                        HRSO4 = mean(HRSO4),
                        HRNO3 = mean(HRNO3),
                        HRChl = mean(HRChl),
                        HROrg44 = mean(HROrg44),
                        HROrg55 = mean(HROrg55),
                        C4H7 = mean(C4H7),
                        HROrg57 = mean(HROrg57),
                        C4H9 = mean(C4H9),
                        C6H10O = mean(C6H10O),
                        H_C = mean(H_C),
                        O_C = mean(O_C),
                        SVOOA = mean(SVOOA),
                        COA = mean(COA),
                        HOA = mean(HOA),
                        PM25ref = mean(PM25ref)
                        )

df_driveday <- df_grouped %>%
  group_by(NEAR_FID) %>%
  summarize(count = n(),
            Lat = median(Lat),
            Lon = median(Lon),
            CO_20sMean = median(CO_20sMean),
            CO2_20sMea = median(CO2_20sMea),
            PN_20sMean = median(PN_20sMean),
            HROrg = median(HROrg),
            HRNH4 = median(HRNH4),
            HRSO4 = median(HRSO4),
            HRNO3 = median(HRNO3),
            HRChl = median(HRChl),
            HROrg44 = median(HROrg44),
            HROrg55 = median(HROrg55),
            C4H7 = median(C4H7),
            HROrg57 = median(HROrg57),
            C4H9 = median(C4H9),
            C6H10O = median(C6H10O),
            H_C = median(H_C),
            O_C = median(O_C),
            SVOOA = median(SVOOA),
            COA = median(COA),
            HOA = median(HOA),
            PM25ref = median(PM25ref)
  )

df_driveday <- merge(df_driveday,centroids,by=c("NEAR_FID"))
df_driveday <- merge(df_driveday,BCdf_driveday,by=c("NEAR_FID"))
df_driveday <- select(df_driveday, -c(Shape_Area, Shape_Leng, OID, Lat, Lon))

#####
IndepVarsList <- colnames(cf[ , 3:ncol(cf)])
IndepVarsList <- IndepVarsList [! IndepVarsList %in% "NEAR_FID"]

##### starting with inputs ####
df_LUR_input <- df_driveday
Sites_df <- cf_byCell
remove <- "HRChl"
measurementSpeciesforModel <- c("COA", "HOA", "BC" ,"SVOOA", "HRSO4")#,"HOA")
measurementSpeciesforModel <- measurementSpeciesforModel [! measurementSpeciesforModel %in% remove]
statStr <- ""
measurementSpeciesforModel_stat <- paste(measurementSpeciesforModel,statStr,sep="")
AddSpeciesAsDVs <- c("")
FoodTruckDVs <- colnames(x = foodtrucks)[which(str_detect(string = colnames(foodtrucks), pattern = "FoodTruckCount_250"))] #names of food truck variables
DropSpeciesAsDVs <- c("RestCount_100m","RestCount_500m","RestCount_50m","RestCount_1000m","RestCount_2500m")
IndepVarsforModel <- c(IndepVarsList,FoodTruckDVs) #which IVs are you using for fitting #EDIT#
alwaysKeep_DV = c("count","Latitude", "Longitude", "NEAR_FID")
alwaysKeep_IV = c("NEAR_FID")
Sites_df_temp <- subset(Sites_df, select = c(IndepVarsforModel,alwaysKeep_IV))
if(nchar(AddSpeciesAsDVs[1]) > 1){
Sites_df_temp <- merge(Sites_df_temp, subset(df_driveday, select = c(AddSpeciesAsDVs,"NEAR_FID")), by = "NEAR_FID")}
if(nchar(AddSpeciesAsDVs[1]) == 1){
  Sites_df_temp <- merge(Sites_df_temp, subset(df_driveday, select = c("NEAR_FID")), by = "NEAR_FID")}
df_LUR_input <- merge(subset(df_LUR_input, select = c(alwaysKeep_DV,measurementSpeciesforModel)), Sites_df_temp, by = "NEAR_FID") 
if(nchar(AddSpeciesAsDVs[1]) > 1){
  IndepVarsforModel <- c(IndepVarsforModel ,AddSpeciesAsDVs, FoodTruckDVs)}
if(nchar(AddSpeciesAsDVs[1]) == 1){
  IndepVarsforModel <- c(IndepVarsforModel,FoodTruckDVs)}
IndepVarsforModel <- IndepVarsforModel [! IndepVarsforModel %in% DropSpeciesAsDVs]
IndepVarsforModel
colnames(df_LUR_input)

# stepSeq <- seq(from = 0.1, to = 1, by = 0.1)
  tots = 1
  repeats <- seq(from = 1, to = tots, by = 1)
  r2saves_goodMods <- r2saves_goodMods[0, ]
  species <- "HOA"
  dfmodelSaves <- data.frame(species=double(), 
                             speciesModel=double(), 
                             "NEAR_FID" = integer(), 
                             "ModelInd" = double())
for(rep in repeats){
  seq = 1
# for(seq in stepSeq){
  reps = 1
  drivedays = 3
  df_LUR_input <- filter(df_LUR_input, count >= drivedays) #run this line to make starting point the 394 sites with drive days >15
  df_LUR_input <- df_LUR_input[ ,colnames(unique(as.matrix(df_LUR_input), MARGIN=2))] #deal with columns that, for the subset, are identical
  IndepVarsforModel <- Reduce(intersect, list(IndepVarsforModel, colnames(df_LUR_input))) #same as line above
  sites = round(nrow(filter(df_LUR_input, count >= drivedays)) * seq) ##change equality if need be
  saveString = "test1"
  #### RUN LUR CODE ####
  r2results <- replicate(reps, 
                         run_LUR_all(df_LUR_input, 
                                     drivedays, 
                                     IndepVarsforModel, 
                                     measurementSpeciesforModel_stat, 
                                     measurementSpeciesforModel_stat, 
                                     sites, 
                                     saveString)
  )
  
  #### Match modeled conc. with sites, store in matrix ####
  # df_LUR_output <- merge(df_LUR_output, Polygon_df, by = "NEAR_FID")
  df_LUR_input_wModel <- df_LUR_input
  for(ind in 1:length(measurementSpeciesforModel)){
    print(paste("R2",AQmodels_sub[[ind]]$summary$r.squared,sep = " "))
    formula = formula(AQmodels_sub[[ind]])
    model = lm(formula, data = df_LUR_output)
    print(formula)
    print(seq)
    df_LUR_output[paste(measurementSpeciesforModel_stat[ind], "model", sep = "_")] <- predict(model, data = df_LUR_output)
    df_LUR_output[paste(measurementSpeciesforModel_stat[ind], "resid", sep = "_")] <- AQmodels_sub[[ind]]$summary$residuals
    df_LUR_input_wModel[paste(measurementSpeciesforModel_stat[ind], "model", sep = "_")] <- predict(model, df_LUR_input_wModel)
  }
}  
  dfmodelSavesTemp <- cbind(df_LUR_input_wModel[ ,str_detect(colnames(df_LUR_input_wModel),species)], 
                      "NEAR_FID" = df_LUR_input_wModel[ ,"NEAR_FID"])
  dfmodelSavesTemp$ModelInd <- rep
  dfmodelSaves <- rbind(dfmodelSaves,dfmodelSavesTemp)
}
  
  calcRelaImp_ESR <- function(ind, data = df_LUR_output){
    linmodFunc <- lm(formula = AQmodels_sub[[ind]]$formula, data = data) #'scrape' the regression formula
    metricsFunc <- calc.relimp(linmodFunc) #this calculates the relative contribution stuff
    strsplit(AQmodels_sub[[ind]]$formula, split = " ")[[1]][1] #scrape the species name
    storeFunc <<- metricsFunc@lmg #store the relative contribution numbers themselves
  }

  r2contributions_repeats <-calcRelaImp_ESR(ind = 1, data = df_LUR_output)
  shrt <- r2contributions_repeats

  tempdf <- data.frame(variable = character(length(names(shrt))),
                       r2contrib = numeric(length(names(shrt))),
                       modelNum = numeric(length(names(shrt))),
                       r2tot = numeric(length(names(shrt))))
  tempdf$variable <- names(shrt)
  tempdf$r2contrib <- as.numeric(shrt)
  tempdf$modelNum <- rep
  tempdf$r2tot <- sum(as.numeric(shrt))
  r2saves_goodMods <- rbind(r2saves_goodMods,tempdf)
  print(paste('done',rep,'...',sep=" "))


#### Saving R2 results to a dataframe #####
prepR2SaveDf <- function(){
  r2results <- aperm(r2results)
  rsquaredDf <- data.frame(r2results, stringsAsFactors = FALSE)
  colnames(rsquaredDf) <- c(measurementSpeciesforModel_stat,"DriveDays","NumSites",paste(measurementSpeciesforModel_stat,"RSE",sep="_"),"InputIVList")
  # colnames(rsquaredDf) <- c(measurementSpeciesforModel_stat,"DriveDays","NumSites","InputIVList")
  rsquaredDf
} #functions for building an r2 results dataframe
appendR2SaveDf <- function(){
  r2bigtemp <- R2_save_df
  r2results <- aperm(r2results)
  rsquaredDf <- data.frame(r2results, stringsAsFactors = FALSE)
  # colnames(rsquaredDf) <- c(measurementSpeciesforModel_stat,"DriveDays","NumSites","InputIVList")
  colnames(rsquaredDf) <- c(measurementSpeciesforModel_stat,"DriveDays","NumSites",paste(measurementSpeciesforModel_stat,"RSE",sep="_"),"InputIVList")
  rsquaredDf <- rbind(rsquaredDf,r2bigtemp)
  rsquaredDf
} #functions for building an r2 results dataframe
if(exists("R2_save_df") == FALSE){
  R2_save_df <- prepR2SaveDf()
  temp1 <- subset(R2_save_df, select=-c(InputIVList))
  temp2 <- subset(R2_save_df, select=c(InputIVList))
  temp1 %>% map_if(is.factor, as.character) %>% map_if(is.character, as.numeric) %>% as_data_frame -> temp1 
  temp2 %>% map_if(is.factor, as.character) %>% as_data_frame -> temp2 
  R2_save_df <- cbind(temp1,temp2)
} else {
  R2_save_df <- appendR2SaveDf()
  temp1 <- subset(R2_save_df, select=-c(InputIVList))
  temp2 <- subset(R2_save_df, select=c(InputIVList))
  temp1 %>% map_if(is.factor, as.character) %>% map_if(is.character, as.numeric) %>% as_data_frame -> temp1 
  temp2 %>% map_if(is.factor, as.character) %>% as_data_frame -> temp2 
  R2_save_df <- cbind(temp1,temp2)
}

}

  
  








####### plot stuff
  r2byFactor_oneSpecies_NumSites <- function(Variable, r2df, oneSpecies, Str){
    meltR2_oneFact <<- select(r2df, c(oneSpecies,Variable))
    meltR2_oneFact <<- melt(meltR2_oneFact, measure.vars = oneSpecies)
    r2byFactor_NumSites <<- ggplot(data = meltR2_oneFact) +#, aes(group = variable)) +
      geom_boxplot(aes(y = value, x = as.factor(NumSites))) +
      scale_y_continuous(limits = c(0,1))+
      #facet_wrap(Variable) +
      theme_classic() +
      ylab("R-squared") +
      xlab("Number of total sites  used for LUR model") +
      # ggtitle(oneSpecies) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      theme(panel.grid = element_blank(), panel.border = element_blank())
    print(r2byFactor_NumSites)

    date <- format(Sys.Date(), format = "%Y%m%d")
    saveStr <- c("R2by",Variable,"_",Str,"_",date,".pdf")
    saveStr <- paste(saveStr,collapse="")
    # ggsave(paste(getwd(),"plots",saveStr, sep = "/"),width = 4.5, height = 3.5)
  }
  r2byFactor_oneSpecies_NumSites("NumSites",r2df = R2_save_df, "COA" , "COA")
  
  
  
####r2 contributions---looking at stable models for low number of sites solutions
  n = length(unique(r2saves_goodMods$variable))
  palette <- distinctColorPalette(n)
  palette <- as.list(palette)
  names(palette) <- unique(r2saves_goodMods$variable)
  ggplot(r2saves_goodMods, aes(reorder(modelNum, -r2tot))) +
  geom_bar(aes(weight = r2contrib, fill = variable), color = "black") +
  geom_hline(yintercept = 0.5, linetype = 2 , color, size) +
  geom_hline(yintercept = 0.75, linetype = 2 , color, size) +
  scale_fill_manual(values = unlist(palette)) +
  xlab("Models") +
  ylab("R squared")
  
##### highlight restaurants only from R2 contrib #####
  transpoDVs <- "Daily|_Rail|Food|Hwy_|Road|EXP|Imperv"
  n_transpoDVs <- length(which(str_detect(unique(r2saves_goodMods$variable), transpoDVs)))
  palette <- distinctColorPalette(n_transpoDVs)
  palette <- as.list(palette)
  
  ggplot(data = r2saves_goodMods,
         aes(x = reorder(modelNum, -r2tot))) +
  geom_bar(data = r2saves_goodMods, aes(weight = r2contrib), color = "black") +
  geom_bar(data = filter(r2saves_goodMods, str_detect(variable, transpoDVs)),
        aes(x = reorder(modelNum, -r2tot), weight = r2contrib, fill = variable)) +
  scale_fill_manual(values = unlist(palette)) +
  geom_hline(yintercept = 0.5, linetype = 2 , color, size) +
  geom_hline(yintercept = 0.75, linetype = 2 , color, size) +
  xlab("Models") +
  ylab("R squared") +
  ggtitle("50 HOA models: stability of predictors w.r.t. source class")
  



mapPlotfunc <- function(variable, df = df_driveday, Str = ""){
  myLocation2 <- c((median(df[["Longitude"]])-0.01), (median(df[["Latitude"]])+0.003))
  lon_range <- extendrange( df[["Longitude"]] )
  lat_range <- extendrange( df[["Latitude"]] )
  calc <- calc_zoom(lon_range, lat_range)
  routemap <- get_map(location = myLocation2, source = "google", maptype = "terrain", zoom = calc - 0)
  mapPlot <<- ggmap(routemap) +
    geom_point(data = df,
               aes(x = Longitude, y = Latitude),
               alpha = 0, size = 3) +
    geom_point(data = df,
               aes_string(x = "Longitude", y = "Latitude",
                          fill = variable), size = 4.5, shape = 22, stroke =0.5) +
    scale_fill_gradientn(colours = matlab.like(10), limits = c(0.5,2), oob=squish) +
    ggtitle(Str)
  print(mapPlot)
  date <- format(Sys.Date(), format = "%Y%m%d")
  saveStr <- c("Map_",variable,"_",Str,"_",date,".pdf")
  saveStr <- paste(saveStr,collapse="")
  ggsave(paste(getwd(),"plots",saveStr, sep = "/"), width = 7, height = 7)
}
mapPlotfunc("BC", df = df_LUR_input, Str = "BCtest")

scatterPlot_measVsmodel_func <- function(variable, df = df_LUR_output, Str = "LowDDsHighR2"){
  grabMax <- max(df[[variable]],df[[paste(variable,"_model",sep="")]])
  print(grabMax)
  
  scatterPlot_measVsmodel <<- ggplot(data = df) +
    geom_point(aes_string(x = variable, y = paste(variable,"_model",sep = "")), size = 3, shape = 21) +
    geom_smooth(aes_string(x = variable, y = paste(variable,"_model",sep = "")), method='lm', se=FALSE) +
    coord_cartesian(ylim = c(0,grabMax), xlim = c(0,grabMax)) +
    geom_abline(intercept = 0, slope = 1) +
    
    theme_bw() +
    ggtitle(paste(variable,Str,sep="_"))
  
  print(scatterPlot_measVsmodel)
  date <- format(Sys.Date(), format = "%Y%m%d")
  saveStr <- c("Scat_MeasVModel_",variable,"_",Str,"_",date,".pdf")
  saveStr <- paste(saveStr,collapse="")
  ggsave(paste(getwd(),"plots",saveStr, sep = "/"))
}
scatterPlot_measVsmodel_func("COA", df_LUR_output, "halfSites")



##### look at distribution of model results for a subset of the sites used, comparing to the 'truth'
### COA
samps <- 208 #208 is all sites for the 200m model
samples <- sample(dfmodelSaves$NEAR_FID, samps)
dfmodelSaves_sub <- filter(dfmodelSaves, NEAR_FID %in% samples)
if(max(dfmodelSaves_sub$COA) > max(dfmodelSaves_sub$COA_model)){
  larger <- max(dfmodelSaves_sub$COA)
} else {
  larger <- max(dfmodelSaves_sub$COA_model)
}
  distPlot <- ggplot(data = dfmodelSaves_sub) + 
  geom_jitter(aes(x = as.factor(reorder(NEAR_FID, -COA)), y = COA_model), shape = 21, width = 0.2, alpha = 0.5) +
  geom_boxplot(aes(x = as.factor(reorder(NEAR_FID, -COA)), y = COA_model), color = "green", alpha = 0.1) +
  geom_point(aes(x = as.factor(reorder(NEAR_FID, -COA)), y = COA), color = "red") +
  coord_cartesian(ylim = c(0,max(larger))) +
  ylab("COA") + 
  xlab(paste("Subset of ",samps," sites (total ",length(unique(dfmodelSaves$NEAR_FID)),")",sep="")) +
  #theme_bw() +
  #theme(panel.grid = element_blank(), panel.border = element_blank()) +
  theme(axis.text.x = element_text(angle = -90, hjust = 1)) 
  print(distPlot)
  
  ### HOA
  samps <- 208 #208 is all sites for the 200m model
  samples <- sample(dfmodelSaves$NEAR_FID, samps)
  dfmodelSaves_sub <- filter(dfmodelSaves, NEAR_FID %in% samples)
  if(max(dfmodelSaves_sub$HOA) > max(dfmodelSaves_sub$HOA_model)){
    larger <- max(dfmodelSaves_sub$HOA)
  } else {
    larger <- max(dfmodelSaves_sub$HOA_model)
  }
  distPlot <- ggplot(data = dfmodelSaves_sub) + 
    geom_jitter(aes(x = as.factor(reorder(NEAR_FID, -HOA)), y = HOA_model), shape = 21, width = 0.2, alpha = 0.5) +
    geom_boxplot(aes(x = as.factor(reorder(NEAR_FID, -HOA)), y = HOA_model), color = "green", alpha = 0.1) +
    geom_point(aes(x = as.factor(reorder(NEAR_FID, -HOA)), y = HOA), color = "red") +
    coord_cartesian(ylim = c(0,max(larger))) +
    ylab("HOA") + 
    xlab(paste("Subset of ",samps," sites (total ",length(unique(dfmodelSaves$NEAR_FID)),")",sep="")) +
    #theme_bw() +
    #theme(panel.grid = element_blank(), panel.border = element_blank()) +
    theme(axis.text.x = element_text(angle = -90, hjust = 1)) 
    print(distPlot)
  
  ### SVOOA
  samps <- 208 #208 is all sites for the 200m model
  samples <- sample(dfmodelSaves$NEAR_FID, samps)
  dfmodelSaves_sub <- filter(dfmodelSaves, NEAR_FID %in% samples)
  if(max(dfmodelSaves_sub$SVOOA) > max(dfmodelSaves_sub$SVOOA_model)){
    larger <- max(dfmodelSaves_sub$SVOOA)
  } else {
    larger <- max(dfmodelSaves_sub$SVOOA_model)
  }
  distPlot <- ggplot(data = dfmodelSaves_sub) + 
    geom_jitter(aes(x = as.factor(reorder(NEAR_FID, -SVOOA)), y = SVOOA_model), shape = 21, width = 0.2, alpha = 0.5) +
    geom_boxplot(aes(x = as.factor(reorder(NEAR_FID, -SVOOA)), y = SVOOA_model), color = "green", alpha = 0.1) +
    geom_point(aes(x = as.factor(reorder(NEAR_FID, -SVOOA)), y = SVOOA), color = "red") +
    coord_cartesian(ylim = c(0,max(larger))) +
    ylab("SVOOA") + 
    xlab(paste("Subset of ",samps," sites (total ",length(unique(dfmodelSaves$NEAR_FID)),")",sep="")) +
    #theme_bw() +
    #theme(panel.grid = element_blank(), panel.border = element_blank()) +
    theme(axis.text.x = element_text(angle = -90, hjust = 1)) 
    print(distPlot)
    
    ### HRSO4
    samps <- 208 #208 is all sites for the 200m model
    samples <- sample(dfmodelSaves$NEAR_FID, samps)
    dfmodelSaves_sub <- filter(dfmodelSaves, NEAR_FID %in% samples)
    if(max(dfmodelSaves_sub$HRSO4) > max(dfmodelSaves_sub$HRSO4_model)){
      larger <- max(dfmodelSaves_sub$HRSO4)
    } else {
      larger <- max(dfmodelSaves_sub$HRSO4_model)
    }
    distPlot <- ggplot(data = dfmodelSaves_sub) + 
      geom_jitter(aes(x = as.factor(reorder(NEAR_FID, -HRSO4)), y = HRSO4_model), shape = 21, width = 0.2, alpha = 0.5) +
      geom_boxplot(aes(x = as.factor(reorder(NEAR_FID, -HRSO4)), y = HRSO4_model), color = "green", alpha = 0.1) +
      geom_point(aes(x = as.factor(reorder(NEAR_FID, -HRSO4)), y = HRSO4), color = "red") +
      coord_cartesian(ylim = c(0,max(larger))) +
      ylab("HRSO4") + 
      xlab(paste("Subset of ",samps," sites (total ",length(unique(dfmodelSaves$NEAR_FID)),")",sep="")) +
      #theme_bw() +
      #theme(panel.grid = element_blank(), panel.border = element_blank()) +
      theme(axis.text.x = element_text(angle = -90, hjust = 1)) 
    print(distPlot)
    
    ###compile measurements
    df_COA <- df_driveday %>% filter(count >=15) %>% select(COA) %>% arrange(desc(COA))
    df_HOA <- df_driveday %>% filter(count >=15) %>% select(HOA) %>% arrange(desc(HOA))
    df_SVOOA <- df_driveday %>% filter(count >=15) %>% select(SVOOA) %>% arrange(desc(SVOOA))
    df_HRSO4 <- df_driveday %>% filter(count >=15) %>% select(HRSO4) %>% arrange(desc(HRSO4))
    df_NormDistPlot <- cbind(df_COA,df_HOA,df_SVOOA,df_HRSO4)
    df_NormDistPlot$index <- seq(from=1,to=nrow(df_NormDistPlot),by=1)
    
    ####compile model medians
    df_COA <- dfmodelSaves_summaryCOA %>% select(COA_model_med) %>% arrange(desc(COA_model_med))
    df_HOA <- dfmodelSaves_summaryHOA %>% select(HOA_model_med) %>% arrange(desc(HOA_model_med))
    df_SVOOA <- dfmodelSaves_summarySVOOA %>% select(SVOOA_model_med) %>% arrange(desc(SVOOA_model_med))
    df_HRSO4 <- dfmodelSaves_summaryHRSO4 %>% select(HRSO4_model_med) %>% arrange(desc(HRSO4_model_med))
    df_NormDistPlotMods <- cbind(df_COA,df_HOA,df_SVOOA,df_HRSO4)
    df_NormDistPlotMods$index <- seq(from=1,to=nrow(df_NormDistPlotMods),by=1)
    
    
    
    
    NormalizedDistPlot_allSpeciesMods <- ggplot(data = df_NormDistPlotMods) + 
      geom_line(aes(x = index/max(index), y = HRSO4_model_med/min(HRSO4_model_med)), color = "red") +
      geom_line(aes(x = index/max(index), y = SVOOA_model_med/min(SVOOA_model_med)), color = "green") +
      geom_line(aes(x = index/max(index), y = COA_model_med/min(COA_model_med)), color = "blue") +
      geom_line(aes(x = index/max(index), y = HOA_model_med/min(HOA_model_med)), color = "gray") +
      geom_point(aes(x = index/max(index), y = HRSO4_model_med/min(HRSO4_model_med)), color = "red", alpha = 0.5, shape = 21, size = 3) +
      geom_point(aes(x = index/max(index), y = SVOOA_model_med/min(SVOOA_model_med)), color = "green", alpha = 0.5, shape = 21, size = 3) +
      geom_point(aes(x = index/max(index), y = COA_model_med/min(COA_model_med)), color = "blue", alpha = 0.5, shape = 21, size = 3) +
      geom_point(aes(x = index/max(index), y = HOA_model_med/min(HOA_model_med)), color = "gray", alpha = 1, shape = 21, size = 3) +
      xlab("Fraction of total grid cells (N = 208) ordered by grid cell concentration") +
      ylab("Grid cell normalized concentration (all normalized to species minimum)") + 
      expand_limits(y = 1) +
      theme_bw(base_size = 18) 
    print(NormalizedDistPlot_allSpeciesMods)
    
    
    NormalizedDistPlot_allSpecies <- ggplot(data = df_NormDistPlotMods) + 
      geom_line(aes(x = index/max(index), y = HRSO4/min(HRSO4)), color = "red") +
      geom_line(aes(x = index/max(index), y = SVOOA/min(SVOOA)), color = "green") +
      geom_line(aes(x = index/max(index), y = COA/min(COA)), color = "blue") +
      geom_line(aes(x = index/max(index), y = HOA/min(HOA)), color = "gray") +
      geom_point(aes(x = index/max(index), y = HRSO4/min(HRSO4)), color = "red", alpha = 0.5, shape = 21, size = 3) +
      geom_point(aes(x = index/max(index), y = SVOOA/min(SVOOA)), color = "green", alpha = 0.5, shape = 21, size = 3) +
      geom_point(aes(x = index/max(index), y = COA/min(COA)), color = "blue", alpha = 0.5, shape = 21, size = 3) +
      geom_point(aes(x = index/max(index), y = HOA/min(HOA)), color = "gray", alpha = 1, shape = 21, size = 3) +
      xlab("Fraction of total grid cells (N = 208) ordered by grid cell concentration") +
      ylab("Grid cell normalized concentration (all normalized to species minimum)") + 
      expand_limits(y = 1) +
      theme_bw(base_size = 18) 
    print(NormalizedDistPlot_allSpecies)
  

#### make summary df for map Plotting ####
dfmodelSaves_summaryCOA <- dfmodelSaves_COA %>% 
                        group_by(NEAR_FID) %>%
                        summarize(
                                  COA_meas = mean(COA),
                                  COA_model_med = median(COA_model),
                                  COA_model_sdev = sd(COA_model)
                                  #COA_model_sdevNorm = COA_model_sdev/mean(COA_meas)
                                  ) %>%
                        mutate(COA_error = COA_meas - COA_model_med,
                               COA_errorAbs = abs(COA_error))
dfmodelSaves_summaryCOA <- dfmodelSaves_summaryCOA %>%
                           mutate(COA_errorAbsNorm = COA_errorAbs/median(COA_meas),
                                  # mutate(COA_errorAbsNorm = COA_errorAbs/COA_meas,
                                  COA_model_sdevNorm = COA_model_sdev/mean(COA_meas))
                               #COA_errorAbsNorm = COA_errorAbs/mean(COA_meas))
    
    #### make summary df for map Plotting ####
    dfmodelSaves_summaryHRSO4 <- dfmodelSaves %>% 
      group_by(NEAR_FID) %>%
      summarize(
        HRSO4_meas = mean(HRSO4),
        HRSO4_model_med = median(HRSO4_model),
        HRSO4_model_sdev = sd(HRSO4_model)
      ) %>%
      mutate(HRSO4_error = HRSO4_meas - HRSO4_model_med,
             HRSO4_errorAbs = abs(HRSO4_error))    
    
    #### make summary df for map Plotting ####
    dfmodelSaves_summarySVOOA <- dfmodelSaves %>% 
      group_by(NEAR_FID) %>%
      summarize(
        SVOOA_meas = mean(SVOOA),
        SVOOA_model_med = median(SVOOA_model),
        SVOOA_model_sdev = sd(SVOOA_model)
      ) %>%
      mutate(SVOOA_error = SVOOA_meas - SVOOA_model_med,
             SVOOA_errorAbs = abs(SVOOA_error)) 
    
    #### make summary df for map Plotting ####
dfmodelSaves_summaryHOA <- dfmodelSaves_HOA %>% 
  group_by(NEAR_FID) %>%
  summarize(
    HOA_meas = mean(HOA),
    HOA_model_med = median(HOA_model),
    HOA_model_sdev = sd(HOA_model)
    #HOA_model_sdevNorm = HOA_model_sdev/mean(HOA_meas)
  ) %>%
  mutate(HOA_error = HOA_meas - HOA_model_med,
         HOA_errorAbs = abs(HOA_error))
dfmodelSaves_summaryHOA <- dfmodelSaves_summaryHOA %>%
  mutate(HOA_errorAbsNorm = HOA_errorAbs/median(HOA_meas),
         # mutate(HOA_errorAbsNorm = HOA_errorAbs/HOA_meas,
         HOA_model_sdevNorm = HOA_model_sdev/mean(HOA_meas))
#HOA_errorAbsNorm = HOA_errorAbs/mean(HOA_meas))
    
  
##plot to check if the summary worked correctly  
  distPlot + geom_point(data = filter(dfmodelSaves_summary, NEAR_FID %in% samples),
                        aes(x = as.factor(reorder(NEAR_FID, -COA_meas)),
                            y = COA_meas), shape = 8, color = "green")

##histogram of standard deviations    
    test <- as.data.frame(as.vector(summary(dfmodelSaves_summary$COA_model_sdev)))
    colnames(test) <- "testr"
    ggplot(data = dfmodelSaves_summary) + 
      geom_histogram(aes(x = COA_model_sdev), fill = "white", color = "black") +
      geom_vline(data = slice(test, c(2,3,5)), aes(xintercept = testr), color = "blue", size = 1)
    
  
#### map of sites - sized by sdev, colored by error...
  dfmodelSaves_formapCOA <- left_join(dfmodelSaves_summaryCOA, 
                                   select(centroids, -which(str_detect(colnames(centroids), pattern = "Shape|OID"))),
                                   by = "NEAR_FID")
  
  dfmodelSaves_formapHOA <- left_join(dfmodelSaves_summaryHOA, 
                                      select(centroids, -which(str_detect(colnames(centroids), pattern = "Shape|OID"))),
                                      by = "NEAR_FID")
  
  
varMapfunc <- function(df = dfmodelSaves_formap, sizer = "COA_model_sdev", colorer = "COA_errorAbs", Str = ""){
    myLocation2 <<- c((median(df[["Longitude"]])-0.01), (median(df[["Latitude"]])+0.003))
    lon_range <<- extendrange( df[["Longitude"]] )
    lat_range <<- extendrange( df[["Latitude"]] )
    # calc <<- calc_zoom(lon_range, lat_range)
    # routemap <- get_map(location = myLocation2, source = "osm", maptype = "terrain", zoom = calc - 0)
    # routemap2 <<- get_map(location = myLocation2, source = "osm", zoom = calc - 0)
    mapPlot <<- ggplot() +
      geom_point(data = df,
                 aes(x = Longitude, y = Latitude),
                 alpha = 0, size = 3) +
      geom_point(data = df,
                 aes_string(x = "Longitude", y = "Latitude",
                            fill = colorer, size = sizer), shape = 21) +
      scale_size_area(limits = c(0,0.25)) +
      scale_fill_gradientn(colours = matlab.like(10), limits = c(0,0.25), oob=squish)
      ggtitle(Str)
    print(mapPlot)
    # date <- format(Sys.Date(), format = "%Y%m%d")
    # saveStr <- c("Map_",variable,"_",Str,"_",date,".pdf")
    # saveStr <- paste(saveStr,collapse="")
    # ggsave(paste(getwd(),"plots",saveStr, sep = "/"), width = 7, height = 7)
  }     
varMapfunc(df = dfmodelSaves_formapCOA, sizer = "COA_errorAbsNorm", colorer = "COA_model_sdevNorm")
varMapfunc(df = dfmodelSaves_formapHOA, sizer = "HOA_meas", colorer = "HOA_meas")


#####scatterplot of 50 model results 
strPlot <- names(dfmodelSaves)[str_detect(names(dfmodelSaves), "COA|HOA|SVOOA|HRSO4")]
strPlot1 <- strPlot[1]
strPlot2 <- strPlot[2]
if(max(dfmodelSaves[[strPlot1]]) > max(dfmodelSaves[[strPlot2]])){
  max = max(dfmodelSaves[[strPlot1]])
} else {
  max = max(dfmodelSaves[[strPlot2]])
}

ggplot(dfmodelSaves) + 
  geom_point(aes_string(x = strPlot1, y = strPlot2), alpha = 0.4, shape = 1) + 
  coord_cartesian(xlim = c(0,max),ylim = c(0,max)) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  ggtitle("Results from 50 model runs")



#######Manual cross validation stuff for the base case models:
COA1 <- lm(formula(AQmodels_sub[[1]]$formula), df_LUR_input)
Tenfoldcv_COA <- cv.lm(COA1$model, COA1, m=10, legend.pos = "topleft")
summary(COA1)
cor(Tenfoldcv_COA$COA,Tenfoldcv_COA$cvpred)**2

HOA1 <- lm(formula(AQmodels_sub[[2]]$formula), df_LUR_input)
Tenfoldcv_HOA <- cv.lm(HOA1$model, HOA1, m=10, legend.pos = "topleft")
summary(HOA1)
cor(Tenfoldcv_HOA$HOA,Tenfoldcv_HOA$cvpred)**2

SVOOA1 <- lm(formula(AQmodels_sub[[3]]$formula), df_LUR_input)
Tenfoldcv_SVOOA <- cv.lm(SVOOA1$model, SVOOA1, m=10, legend.pos = "topleft")
summary(SVOOA1)
cor(Tenfoldcv_SVOOA$SVOOA,Tenfoldcv_SVOOA$cvpred)**2

HRSO41 <- lm(formula(AQmodels_sub[[4]]$formula), df_LUR_input)
Tenfoldcv_HRSO4 <- cv.lm(HRSO41$model, HRSO41, m=10, legend.pos = "topleft")
summary(HRSO41)
cor(Tenfoldcv_HRSO4$HRSO4,Tenfoldcv_HRSO4$cvpred)**2



