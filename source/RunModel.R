RunModel = function(runvars, r){

#### Initialize ####
  
  if(runvars$inputPop[r]==F){
  #set initial age structure of population (equal proportion of all ages) 
  agestage   = data.frame(age = (1:runvars$lifespan[r]), num = rep(runvars$N[r]/(runvars$lifespan[r]*runvars$N[r]), runvars$lifespan[r])) 
  population = StartingPop(runvars$N[r], agestage, runvars$nloci[r], runvars$sigp[r], runvars$alphaAB[r], runvars$alphaAA[r], runvars$alphaBB[r])
  # otherpop   = StartingPop(10000, agestage, runvars$nloci[r], runvars$sigp[r], runvars$omegaWild[r])
  #   otherpop$gen = 9999 #marker, otherwise all 0
  #otherpop   = otherpop[,13:(runvars$nloci[r]*2+12)]
  remove(agestage)

  } else{
    load("C:/Users/Haley.Ohms/OneDrive - Trout Unlimited/Documents/GitHub/captivebreeding-IBM-ohms/inputPop/PopulationFile.rda")
    #load("C:/Users/Haley.Ohms/OneDrive - Trout Unlimited/Documents/GitHub/captivebreeding-IBM-ohms/inputPop/OtherpopFile.rda")
  }
  
  #write starting population headers to file 
  write.table(population[0,], paste(runvars$outdir[r], "population_indvs_", 
                                    formatC(r, width = 4, format = "d", flag = "0"),  
                                    ".csv", sep=""), col.names=TRUE, row.names=FALSE, append=FALSE, sep=",")
  
  #set up variables and ID number counter
  totalinds  = length(population[,1]) + 1 #VariableForUniqueID
  alldead <<- "none"  #Error check holder; reset for each r

#### Simulate over years ####
      for(g in 1:(runvars$gens[r] + runvars$lifespan[r] + 1)){  #Says how long to simulate: generations (years) plus a single lifespan + 1

        ## Code to stop run for testing or validation:
        # if (g == 40){
        #   break
        # }
        
#### Add immigrants to the population ####
        
        ###  look for extinct populations (before immigration makes it no longer extinct) ####
        if(nrow(population %>% filter(alive==1))<2){
          print("pop went extinct")
          alldead <<- "pop went extinct"
          AllDead(alldead, population, runvars$outdir[r], r)
          break
        }
       
        ## Add migrants to the population 
        migrants = NULL
        
        if(runvars$nimmigrants[r]>0){
          #if(runvars$nimmigrants[r]>0 & g %% runvars$maturity[r] == 0){
          
          # add migrants for the year - all are mature adults
          migrants     = Immigrant(runvars$nimmigrants[r], runvars$maturity[r], runvars$lifespan[r], runvars$nloci[r], otherpop, runvars$sigp[r], population, totalinds)
          #migrants[,"RRS"] = mean(data.matrix(population[population[,9]==1,"RRS",drop=FALSE])) #assign migrants mean RRS as overall population
          population   = bind_rows(population, migrants)
          
          totalinds    = totalinds + runvars$nimmigrants[r]
        } else {
          migrants=NULL
        }        
        
        remove(migrants)
        
#### Identify the spawners for each location (hatch vs wild) based on pHOS and pNOB:

      datasets = IdentifySpawners(population, runvars$pBroodstock[r], runvars$pNOB[r], runvars$pHOS[r], 
                                  runvars$pNOBinit[r], runvars$yrsCapInit[r],
                                  runvars$maturity[r], runvars$perSpawn1[r], runvars$perSpawn2[r], runvars$perSpawn3[r],
                                  g, alldead)
      
      spawners     = datasets$spawners
      population = datasets$population
      remove(datasets)
      
      # check for no spawners; if it is write it to file and end run
      if(alldead == "No spawners for captive bs" | is.null(spawners)==T | 
         alldead=="Not enough wild spawners to make pNOB" | alldead=="Not enough hatch spawners to make pNOB" |
         alldead=="Not enough hatch spawners to make pHOS"){
        print("Not enough of some fish in IdentifySpawners")
        AllDead(alldead, population, runvars$outdir[r], r)
        break
      }
     

#### Wild reproduction #####   
    offspring = NULL

      if(nrow(spawners %>% filter(SpawnLocation=="wild"))>2){
      offspring  = Repro(spawners, runvars$fecundity[r], runvars$nloci[r], runvars$sigp[r], g, runvars$mu[r], runvars$alpha2[r])
      
      ##if no offspring are produced, end run
      if(is.null(offspring) | nrow(offspring)==0){
        print("no wild offspring generated")
        alldead <<- "no wild offspring generated"
        AllDead(alldead, population, runvars$outdir[r], r)
        break
      }
      
      #Mortality based on phenotype (selection)
      # offspring = WildSelection(offspring, g, runvars$sThetaHigh[r], runvars$sThetaMed[r], runvars$sThetaLow[r], runvars$omegaWild[r], runvars$sThetaVar[r])

      #Density-dependent, selective mortality 
      offspring = DensityDep(offspring, runvars$alphaAB[r], runvars$alphaAA[r], runvars$alphaBB[r], runvars$beta[r], g, runvars$l[r], totalinds)
      
      } else {
          print("no wild spawners")
          alldead <<- "no wild spawners"
          AllDead(alldead, population, runvars$outdir[r], r)
          break
      }
      
      #Update ID counter
      totalinds    = max(offspring$ID) + 1
      
      

    #remove(Nt, numberoffspring, n.wild.offspring, n.captive.offspring, migrants, spawners)
 

    
  ####### Captive reproduction ########
    coffspring  = NULL
    
    #IF HATCHERY IS HAPPENING:
    if(g >= runvars$startcap[r] & g <= runvars$endcap[r]){   
      if(nrow(spawners %>% filter(SpawnLocation=="hatchery"))>2){
        
        #Hatchery Reproduction AND selection happens here
        coffspring = CaptiveRepro(spawners, g, runvars$nloci[r], totalinds, runvars$sigp[r],
                                  runvars$coffprop[r], runvars$mu[r], 
                                  runvars$alpha2[r], 
                                  runvars$pHOS[r], runvars$capSelectAB[r], runvars$capSelectAA[r], runvars$capSelectBB[r], 
                                  runvars$fecundity[r])
          
        if(is.null(coffspring)){
          print("no hatchery offspring survived selection")
          alldead <<- "no hatchery offspring"
          AllDead(alldead, population, runvars$outdir[r], r)
          break
        }
        
        
      }else {
          print("no hatchery bs")
          alldead <<- "no hatchery bs"
          AllDead(alldead, population, runvars$outdir[r], r)
          break
        }
        
        # add captive born individuals to count (if there were any)
        #if(!is.null(nrow(coffspring))){totalinds = totalinds + length(coffspring[,1]) + 1}
        
        #Update ID counter
        if(!is.null(nrow(coffspring))){totalinds    = max(coffspring$ID) + 1}
    
    }
    
    
####  Add offspring produced in this year ####
    
    combOffspring <- rbind(offspring, coffspring)
    
    if(nrow(combOffspring)<1){
      print("still have a combOffspring error")
      alldead <<- "still have a combOffspring error"
      AllDead(alldead, population, runvars$outdir[r], r)
      break 
    }

    population = bind_rows(population, combOffspring)
    remove(offspring, coffspring, spawners)

    
#### Age and write to file ####
    # increase age by 1 year
    population = AgeUp(population, alldead, runvars$lifespan[r], g)
    
    if(alldead=="all dead at age up"){
      print("all dead at age up")
      AllDead(alldead, population, runvars$outdir[r], r)
      break
    }
    
#### Write info for all dead individuals, then remove all dead from population object ####
    # NOTE: This is the step where generation info is saved to individuals.csv
    population <- AllDead(alldead, population, runvars$outdir[r], r)

  } #Ends the for loop for generations
  
##########################################################
####  calculate and writeout pop leftovers and error codes  ####
  #SavePopLeftovers(population, runvars$outdir[r], r)
  SavePopLeftovers(population, runvars, r, alldead, runvars$outdir[r])
  
  #WriteOut(runvars, r)
}
