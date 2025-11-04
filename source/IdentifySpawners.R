
# This function identifies the hatchery broodstock (both wild and hatchery origin), 
# the wild fish that will spawn in the wild
# the hatchery fish that will spawn in the wild
# and the hatchery fish that will be culled. 
# All spawners (not culled) go into df:spawners
# All spawners and culled are marked in df:population


IdentifySpawners = function(population, pBroodstock, pNOB, pHOS,
                            pNOBinit, yrsCapInit,
                          maturity, perSpawn1, perSpawn2, perSpawn3, 
                          g, alldead){  

  spawners <- NULL
  possibleSpawners <- NULL
  
  # Pull the possible spawners based on age structure of run
  nYoungestSpawners <- nrow(population %>% filter(age==maturity, alive==1)) #calc number of youngest spawners
  
  youngestSpawners = population %>% filter(age==maturity, alive==1) %>% 
    ungroup() %>% 
    slice_sample(n = round(perSpawn1*nYoungestSpawners), replace = FALSE) #Randomly draw them from all possible youngest spawners
  
  middleSpawners = population %>% filter(age==maturity+1, alive==1) %>%
    ungroup() %>%
    slice_sample(n = round(perSpawn2*nYoungestSpawners), replace = FALSE) #Randomly draw middle age class from all possible (use nYoungest to approximate correct %)
  
  oldestSpawners = population %>% filter(age==maturity+2, alive==1) #Use 100% of the oldest age class (all remaining)
  
  possibleSpawners = bind_rows(youngestSpawners, middleSpawners, oldestSpawners) #Combine into a dataframe
  
  
   
  #IF HATCHERY IS HAPPENING:
  if(g >= runvars$startcap[r] & g <= runvars$endcap[r]){
    
    # Calc number of broodstock desired: 
    #sizecaptive = round(pBroodstock* nrow(possibleSpawners)) 
    sizecaptive = pBroodstock
    
    if(sizecaptive > nrow(possibleSpawners)){
      print("Error - Not enough spawners to make captive broodstock")
      alldead  <<- "No spawners for captive bs"
      datasets = list(spawners=NULL, population=population) 
      return(datasets)
    }
    
    ####IF YOU ARE IN INITIALIZATION YEARS####
    if(g >= runvars$startcap[r] & g <= runvars$startcap[r]+runvars$yrsCapInit[r]){
      
    # Put wildborn broodstock into a dataframe
    wildbornBS = possibleSpawners %>% 
      filter(HvsW==1) %>% 
      ungroup() %>%
      slice_sample(n=round(pNOBinit*sizecaptive), replace = FALSE)
    
    # Put hatchborn broodstock into a dataframe
    hatchbornBS = possibleSpawners %>% 
      filter(HvsW==0) %>% 
      ungroup() %>%
      slice_sample(n=round((1-pNOBinit)*sizecaptive), replace = FALSE)
    } else {  
      
      ####IF YOU ARE NOT IN INITIALIZATION YRS: #####
      
      # Put wildborn broodstock into a dataframe
      wildbornBS = possibleSpawners %>% 
        filter(HvsW==1) %>% 
        ungroup() %>%
        slice_sample(n=round(pNOB*sizecaptive), replace = FALSE)
      
      if(nrow(wildbornBS) < round(pNOB*sizecaptive)){
        print("Error - Not enough wild spawners to make pNOB")
        alldead  <<- "Not enough wild spawners to make pNOB"
        datasets = list(spawners=NULL, population=population) 
        return(datasets)
      }
      
      # Put hatchborn broodstock into a dataframe
      
      hatchbornBS = possibleSpawners %>% 
        filter(HvsW==0) %>% 
        ungroup() %>%
        slice_sample(n=round((1-pNOB)*sizecaptive), replace = FALSE)
      
      if(nrow(hatchbornBS) < round((1-pNOB)*sizecaptive)){
        print("Error - Not enough hatch spawners to make pNOB")
        alldead  <<- "Not enough hatch spawners to make pNOB"
        datasets = list(spawners=NULL, population=population) 
        return(datasets)
      }
      
      
    }
    
    #Combine the hatchery broodstock into a dataframe
    combBS  = bind_rows(hatchbornBS, wildbornBS)  
    
    ## Mark all broodstock in df:population 
    bsIDs <- unique(combBS$ID)
    
    population <- population %>% 
      mutate(alive = if_else(ID %in% bsIDs, 0, alive), 
             gendead = if_else(ID %in% bsIDs, g, gendead), 
             yearSpawned = if_else(ID %in% bsIDs, g, yearSpawned),
             fate = if_else(ID %in% bsIDs, "hatcheryBS", fate))
    
    
    ####ID the wild spawners:
    wildbornBS_ID <- unique(wildbornBS$ID) #First get ID's of broodstock, divided by wild and hatchery
    hatchbornBS_ID <- unique(hatchbornBS$ID)
    
    remainingWildSpawners = possibleSpawners %>% 
      filter(HvsW==1, !(ID %in% wildbornBS_ID)) 
      remainingWildSpawnersIDs <- unique(remainingWildSpawners$ID)
    
    remainingHatchSpawners = possibleSpawners %>% 
      filter(HvsW==0, !(ID %in% hatchbornBS_ID))
      remainingHatchSpawnerIDs <- unique(remainingHatchSpawners$ID)
      
    #Mark the wild spawners that spawn in the wild:
      population <- population %>% 
        mutate(alive = if_else(ID %in% remainingWildSpawnersIDs, 0, alive), 
               gendead = if_else(ID %in% remainingWildSpawnersIDs, g, gendead), 
               yearSpawned = if_else(ID %in% remainingWildSpawnersIDs, g, yearSpawned), 
               fate = if_else(ID %in% remainingWildSpawnersIDs, "wildSpawn", fate))
 
      
      
    # #IF YOU ARE IN INITIALIZATION YEARS
    #if(g >= runvars$startcap[r] & g <= runvars$startcap[r]+runvars$yrsCapInit[r]){
    #   
    # 
    #   
    #   
    #   #################ORIG#####################
    #   
    #       #All remaining hatchery spawners that were not broodstock spawn in the wild
    #       # pHOS is not set and there is no culling:
    #       
    #       #All remaining hatchery spawners spawn in the wild: 
    #       population <- population %>% 
    #         mutate(alive = if_else(ID %in% remainingHatchSpawnerIDs, 0, alive), 
    #                gendead = if_else(ID %in% remainingHatchSpawnerIDs, g, gendead), 
    #                yearSpawned = if_else(ID %in% remainingHatchSpawnerIDs, g, yearSpawned), 
    #                fate = if_else(ID %in% remainingHatchSpawnerIDs, "wildSpawn", fate))
    #       
    #   #Update spawners dataframe with SpawnLocation  
    #       spawners = possibleSpawners %>% 
    #         mutate(SpawnLocation = if_else(ID %in% remainingHatchSpawnerIDs | ID %in% remainingWildSpawnersIDs, "wild",
    #                                        if_else(ID %in% bsIDs, "hatchery", "ERROR")))
    # 
    #       #################ORIG#####################    
    #       
    # } else { 
    #   #IF YOU ARE IN REGULAR HATCHERY OPERATIONS YEARS

      
      #Hatchery spawners spawn in the wild determined by pHOS
           n_remainingHatchSpawnInWild = round((pHOS*nrow(remainingWildSpawners))/(1-pHOS))  
           
           
      #IF YOU ARE IN REGULAR HATCH OP YEARS (not initialization)
          if(g >= runvars$startcap[r]+runvars$yrsCapInit[r] & g <= runvars$endcap[r]){
           
             # Error out if we need more hatchery-origin spawners to make pHOS than we have
             if(n_remainingHatchSpawnInWild > nrow(remainingHatchSpawners)){
               print("Error - Not enough hatch spawners to make pHOS")
               alldead  <<- "Not enough hatch spawners to make pHOS"
               datasets = list(spawners=NULL, population=population) 
               return(datasets)
             } 
          }
           
           HatchSpawnInWild <- remainingHatchSpawners %>% 
                               ungroup() %>% 
                               slice_sample(n=n_remainingHatchSpawnInWild, replace = F) 
           
           HatchSpawnInWildIDs <- unique(HatchSpawnInWild$ID)
           
           population <- population %>% 
             mutate(alive = if_else(ID %in% HatchSpawnInWildIDs, 0, alive), 
                    gendead = if_else(ID %in% HatchSpawnInWildIDs, g, gendead), 
                    yearSpawned = if_else(ID %in% HatchSpawnInWildIDs, g, yearSpawned), 
                    fate = if_else(ID %in% HatchSpawnInWildIDs, "wildSpawn", fate))
           
           HatchCulled <- remainingHatchSpawners %>% 
                          filter(!(ID %in% HatchSpawnInWildIDs)) 
           
           HatchCulledIDs <- unique(HatchCulled$ID)
           
            #Mark culled fish in df:population
              population <- population %>% 
                mutate(alive = if_else(ID %in% HatchCulledIDs, 0, alive), 
                       gendead = if_else(ID %in% HatchCulledIDs, g, gendead), 
                       yearSpawned = if_else(ID %in% HatchCulledIDs, NA, yearSpawned), 
                       fate = if_else(ID %in% HatchCulledIDs, "culled", fate))
              

         #Update spawners dataframe with SpawnLocation and exclude culled fish: 
              spawners = possibleSpawners %>% 
                filter(!ID %in% HatchCulledIDs) %>% 
                mutate(SpawnLocation = if_else(ID %in% HatchSpawnInWildIDs | ID %in% remainingWildSpawnersIDs, "wild",
                                               if_else(ID %in% bsIDs, "hatchery", "ERROR")))
        #} 

  } else {
    
    ######################################################
    #IF HATCHERY IS NOT HAPPENING: 
    
    # Pull the possible spawners based on age structure of run
    # nYoungestSpawners <- as.numeric(population %>% filter(age==maturity, alive==1) %>% summarise(n())) #calc number of youngest spawners
    # 
    # youngestSpawners = population %>% filter(age==maturity, alive==1) %>% 
    #   ungroup %>% 
    #   slice_sample(n = round(perSpawn1*nYoungestSpawners), replace = FALSE) #Randomly draw them from all possible youngest spawners
    # 
    # middleSpawners = population %>% filter(age==maturity+1, alive==1) %>% 
    #   ungroup %>% 
    #   slice_sample(n = round(perSpawn2*nYoungestSpawners), replace = FALSE) #Randomly draw middle age class from all possible (use nYoungest to approximate correct %)
    # 
    # oldestSpawners = population %>% filter(age==maturity+2, alive==1) #Use 100% of the oldest age class (all remaining)
    
    #Save df:spawners with all possible spawners
    #spawners = bind_rows(youngestSpawners, middleSpawners, oldestSpawners) 
  
  ## Identify and mark wild spawners in df:population  
    wildSpawners = possibleSpawners %>% filter(HvsW==1)
      wildSpawnerIDs <- unique(wildSpawners$ID)
    
    population <- population %>% 
      mutate(alive = if_else(ID %in% wildSpawnerIDs, 0, alive), 
             gendead = if_else(ID %in% wildSpawnerIDs, g, gendead), 
             yearSpawned = if_else(ID %in% wildSpawnerIDs, g, yearSpawned), 
             fate = if_else(ID %in% wildSpawnerIDs, "wildSpawn", fate))
  
  ## Identify and mark (cull) hatchery-origin spawners in df:population   
    hatchSpawners = possibleSpawners %>% filter(HvsW==0)
      hatchSpawnerIDs <- unique(hatchSpawners$ID)
    
    ## Mark them in df:population
    population <- population %>% 
      mutate(alive = if_else(ID %in% hatchSpawnerIDs, 0, alive), 
             gendead = if_else(ID %in% hatchSpawnerIDs, g, gendead), 
             yearSpawned = if_else(ID %in% hatchSpawnerIDs, NA, yearSpawned), 
             fate = if_else(ID %in% hatchSpawnerIDs, "culled", fate))
    
    spawners = possibleSpawners %>% filter(HvsW==1) %>% 
                mutate(SpawnLocation = "wild")
    

  }
  

  datasets = list(spawners=spawners, population=population) 
  return(datasets)
}
