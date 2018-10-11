#This is rather messy right now with comments, but works as a way to iterate with Hugh's model over a list of DVs
run_LUR_all = function(data, #full data frame
                       DriveDays, #num visits to each site (only use sites with >n)
                       IVlist, #feed the list of the indep vars to use
                       DVlist_full, #a list of all of the "data" names in the DF
                       DVlist_forModel, #e.g. all OA factors and HR species, running this model for all things in this list
                       NumSites, #number of subsamples from the full to use
                       saveString) { #some identifying string to help understand the 
  #######
  datCopy <<- data
  datCopy <<- data[data$count >= DriveDays, ]
  df_LUR_output <<- datCopy[sample(nrow(datCopy),NumSites, replace = FALSE), ]
  df_LUR_output <<- df_LUR_output[c(IVlist,DVlist_forModel,"NEAR_FID","Latitude","Longitude")]
  df_LUR_output[df_LUR_output < 0] <- 0 #if there are any weird negatives..get rid
  AQmodels_sub <<- lapply(DVlist_forModel,
                          function(x) make_lur(df_LUR_output,
                                               response=x,
                                               dep_col = which(colnames(df_LUR_output)==x),
                                               exclude=c(DVlist_full[DVlist_full != x], "Longitude", "Latitude")))
  saveFormulas_sub <<- lapply(AQmodels_sub, function(x) x$formula) #store the formula
  saveR2_sub <<- lapply(AQmodels_sub, function(x) x$summary$r.squared)
  saveRSE_sub <<- lapply(AQmodels_sub, function(x) x$summary$sigma)#store the RSE
  saveSites <<- lapply(AQmodels_sub, function(x) NumSites)
  saveDriveDays <<-lapply(AQmodels_sub, function(x) DriveDays)
  saveStr <<-lapply(AQmodels_sub, function(x) saveString)
  R2_func <<- saveR2_sub
  RSE_func <<- saveRSE_sub
  fullResults <<- unlist(append(R2_func, c(DriveDays, NumSites, saveString)))
  fullResults <<- append(fullResults, unlist(RSE_func))
  # fullResults <<- unlist(append(R2_func, c(DriveDays, NumSites, saveString)))
  fullResultsSave <<- fullResults
  #print(saveFormulas_sub)
  #print(saveR2_sub)
  return(fullResults)
}