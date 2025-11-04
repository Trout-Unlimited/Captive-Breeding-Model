  
  rm(list = ls()) 
    library(tidyverse)
  
  
  # Preamble ###########################
  directory = #ENTER DIRECTORY HERE
  outdir    = paste(directory,"/output/",sep="")                    #directory to save model output
  source(paste(directory, "/source/FunctionSourcer.R", sep = ''))   #source functions and set source directory
  
  repeats = 1                                                       #number of time to repeat simulations
  species = "salmon"                                                #can vary species if desired
  
  ###species parameter definitions: 
   #fecundity.V: number offspring per pair
   #maturity: number of years to reach sexual maturity
   #lifespan: maximum age
   #repro1: can individuals reproduce multiple times in a lifetime? 1 = yes, 0 = no
   #K: carrying capacity (sets total pop size?)
   #spawn1.V: percent of spawners that is youngest mature age class (must be between 0-1)
   #spawn2.V: percent of spawners that is middle mature age (must be between 0-1)
   #spawn2.V: percent of spawners that is oldest mature age (must be between 0-1)
  
  
  if(species=="salmon") { fecundity.V = 2; maturity.V  = 1;  
                            lifespan.V  = maturity.V+0;  repro1.V = 0; 
                            spawn1.V = 1; spawn2.V = 0; spawn3.V = 1-(spawn1.V+spawn2.V)}   
  
  inputPop.V = F  #Use an input population file (T) or create one (F)
  
  gens.V       = 100                                      #number generations to simulate                           
  
  beta.V         = 50                                     #per capita growth rate (beta in B-H)
  alphaAB.V     = 500                                     #carrying-capacity of AB niche (alpha in B-H)
  alphaAA.V     = 1000                                    #carrying-capacity of AA niche (alpha in B-H)
  alphaBB.V    = 100                                      #carrying-capacity of BB niche (alpha in B-H)
  N.V          = alphaAB.V + alphaAA.V + alphaBB.V        #initial population size     
  l.V          = 1                                        #variance in density-dep mortality (standard deviation in normal draw)              
  nloci.V      = 31                                       #total number of loci; MUST STAY AS 31 (1 locus of lg effect, 30 neutral loci) unless you update code throughout                                                  
  sigp.V      =  0                                        #enviro component of phenotype (heritability) - percentage closer to optimum (0.1 means up to 10% closer to optimum)
  adultmort.V  = 1                                        #proportion of spawners that die after reproduction each year
  
  #variables for captive breeding
  startcap.V  = 25                                        #year to start hatchery
  endcap.V    = 75                                        #year to end hatchery
  pBroodstock.V  = c(25, 50, 200, 350, 500)               #number of broodstock
  pNOB.V  = c(0.1, 0.5, 0.9)                              #proportion of wild (natural-origin) spawners in broodstock
  pHOS.V  = c(0.1, 0.5, 0.9)                              #proportion of captive (hatchery-origin) spawners on spawning grounds; MUST be <1 because I don't cull wild fish
  yrsCapInit.V = 1                                        #number of years to 'initialize' hatchery program (where proportion wild=1 (i.e., pNOB=1))
  pNOBinit.V = 1                                          #proportion wild to initialize hatchery (must=1) 
  coffprop.V  = NA                                        #number of hatchery fish to make each gen; calc'd below
  
  #Captive (hatchery) selection strengths (Survival probs)
  capSelectAB.V = 0.3                                     #hatchery selection on AB 
  capSelectAA.V = 0                                       #hatchery selection on AA 
  capSelectBB.V = 1                                       #hatchery selection on BB 
  
  #other
  nimmigrants.V = 0                                       #number of immigrants per year 
  mu.V<-0.0                                               #mean of mutation rate as per Holt et al.; set to 0
  alpha2.V<- 0                                            #variance of mutation rate as per Holt et al; set to 0
  
  runvars = Replicates(repeats, alphaAB.V, alphaAA.V, alphaBB.V, 
                       N.V, gens.V, 
                       beta.V, l.V, nloci.V, sigp.V,
                       adultmort.V, fecundity.V, maturity.V, 
                       lifespan.V, repro1.V, spawn1.V, spawn2.V, spawn3.V, 
                       startcap.V, endcap.V, pBroodstock.V, pNOB.V, 
                       pHOS.V, yrsCapInit.V, pNOBinit.V, 
                       coffprop.V,
                       nimmigrants.V,  
                       directory, outdir, species, 
                       capSelectAB.V, capSelectAA.V, capSelectBB.V,
                       inputPop.V, 
                       mu.V, alpha2.V)
                       
  
  ## Save runvars table
  runvars <- runvars %>% 
              rename(run = Replicate) %>% 
              mutate(coffprop = as.integer(((pHOS*(alphaAB+alphaAA+alphaBB))/(1-pHOS))) + pBroodstock.V + 10) 
  
  
  save(runvars, file=paste(outdir, "runVars.rda", sep = ""))
  
  remove(repeats, alphaAB.V, alphaAA.V, alphaBB.V, 
         N.V, gens.V, 
         beta.V, l.V, nloci.V, sigp.V,
         adultmort.V, fecundity.V, maturity.V, 
         lifespan.V, repro1.V, spawn1.V, spawn2.V, spawn3.V, 
         startcap.V, endcap.V, pBroodstock.V, pNOB.V, 
         pHOS.V, yrsCapInit.V, pNOBinit.V, 
         coffprop.V,
         nimmigrants.V,  
         directory, outdir, species, 
         capSelectAB.V, capSelectAA.V, capSelectBB.V,
         inputPop.V, 
         mu.V, alpha2.V)
  
  
  
  # #run model iterating over parameters in Replicates
  for(r in 1:nrow(runvars)){
    RunModel(runvars, r)
  }
  
