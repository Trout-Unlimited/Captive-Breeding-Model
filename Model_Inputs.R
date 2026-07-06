
rm(list = ls()) #clear the deck
library(tidyverse)

# FOR LOCAL ###########################
directory = "C:/Users/Haley.Ohms/OneDrive - Trout Unlimited/Documents/GitHub/CaptiveBreedingModel-Public/"
  #Example: "C:/Users/Haley.Ohms/CaptiveBreedingModel/"
outdir    = paste(directory,"output/",sep="")                    #directory to save model output
source(paste(directory, "/source/FunctionSourcer.R", sep = ''))   #source functions and set source directory


species = "salmon"                          
###species parameter definitions: 
 #fecundity.V: number offspring per pair
 #maturity: number of years to reach sexual maturity
 #lifespan: maximum age
 #repro1: can individuals reproduce multiple times in a lifetime? 1 = yes, 0 = no
 #K: carrying capacity (sets total pop size?)
 #spawn1.V: percent of spawners that is youngest mature age class (must be between 0-1)
 #spawn2.V: percent of spawners that is middle mature age (must be between 0-1)
 #spawn2.V: percent of spawners that is oldest mature age (must be between 0-1)


if(species=="salmon") { fecundity.V = 100*2; #Fecundity must be >2 and must be an integer
                          maturity.V  = 1;  
                          lifespan.V  = maturity.V+0;  repro1.V = 0; 
                          spawn1.V = 1; spawn2.V = 0; spawn3.V = 1-(spawn1.V+spawn2.V)}   

inputPop.V = F  #Use an input population file (T) or create one (F)

repeats = 1                                #number of time to repeat simulations
gens.V       = 10                            #number generations to simulate; SINGLE VALUE ONLY, messes up stochastic matrix otherwise                           

  #Carrying capacities (for both density dep functions)
    ## SINGLE VALUES ONLY! Messes up starting population size otherwise
    #Bev Holt Params: 
    #alpha.V         = 1              #WE ASSUME ALPHA=1    #max survival at low density; leave as 1
    
      #Equilbrium in each niche: 
      nstarAA.V = 1000#1300#1000
      nstarAB.V = 500#650#500
      nstarBB.V = 100#140#100
      
      #Shepherd Params:
      #Betas calc'd. Shape and asymptote will change if fecundity changes, but N* wont 
      alpha.V = 0.03  ##ADJUST BASED ON FECUNDITY f=100, alpha=0.03; f=10, alpha=0.3 AND WHAT FORM OF DD you're using
        delta.V = 1.5     # Based on Foss-Grant et al. 2016 shape
        fec_corrected <- fecundity.V/2 #Need because it's a two sex model
          bShepAA.V = ((alpha.V*fec_corrected)-1)/((fec_corrected^delta.V)*(nstarAA.V^delta.V)) 
          bShepAB.V = ((alpha.V*fec_corrected)-1)/((fec_corrected^delta.V)*(nstarAB.V^delta.V)) 
          bShepBB.V = ((alpha.V*fec_corrected)-1)/((fec_corrected^delta.V)*(nstarBB.V^delta.V)) 
      
      stochNiches.V = F  #F is constant based on vals above, T uses draws generated in NicheVariation below
      cvAA.V    =   0  #coeff of variation, percent variation (must be between 0, 1)
      cvAB.V    = 0
      cvBB.V    = 0
      
 
  
N.V          = nstarAA.V+nstarAB.V+nstarBB.V    #initial population size, pop equilibrium     
l.V          = 1                                                #variance in density-dep mortality (standard deviation in normal draw)              
nloci.V      = 5                                              #Total number of loci; first will be locus of large effect, remaining are neutral              
nAlleles.V = 2                                                  #Number of alleles per locus at neutral loci
juvmort.V = 0         #proportion of juveniles that die before reaching adulthood
adultmort.V  = 1        #proportion of spawners that die after reproduction each year


#variables for captive breeding
startcap.V  = 15                                                 #year to start hatchery
endcap.V    = 25                                            #year to end hatchery
pBroodstock.V  = 200     #number of broodstock
pNOB.V  = 0.9                   #proportion of natural-origin spawners in broodstock
pHOS.V  = 0.1                    #proportion of hatchery-origin spawners on spawning grounds; MUST be <1 because I don't cull wild fish
yrsCapInit.V = 1                                                #number of years to 'initialize' hatchery program (where pNOB=1)
pNOBinit.V = 1                                                  #pNOB to initialize hatchery (must=1) 
coffprop.V  = NA                                                #Placeholder for number of hatchery fish to make each gen; LEAVE AS NA; calc'd below

#Hatchery selection strengths (Survival probs)
capSelectAB.V = 0.3                                           #hatchery selection on low phenotypes (prob of survival)
capSelectAA.V = 0                                              #hatchery selection on med phenotypes (prob of survival)
capSelectBB.V = 1                                             #hatchery selection on high phenotypes (prob of survival)



runvars = Replicates(repeats, 
                     alpha.V,  
                     nstarAA.V, nstarAB.V, nstarBB.V,
                     delta.V, bShepAA.V, bShepAB.V, bShepBB.V, 
                     N.V, gens.V, 
                     nloci.V, nAlleles.V, 
                     adultmort.V, juvmort.V,
                     fecundity.V, maturity.V, 
                     lifespan.V, repro1.V, spawn1.V, spawn2.V, spawn3.V, 
                     startcap.V, endcap.V, pBroodstock.V, pNOB.V, 
                     pHOS.V, yrsCapInit.V, pNOBinit.V, 
                     coffprop.V,
                     directory, outdir, species, 
                     capSelectAB.V, capSelectAA.V, capSelectBB.V,
                     inputPop.V, stochNiches.V)
                     

## Save runvars table
runvars <- runvars %>% 
            rename(run = Replicate) %>% 
            mutate(coffprop = as.integer((N/(1-pHOS))*1.2 + pBroodstock.V)) 


save(runvars, file=paste(outdir, "runVars.rda", sep = ""))

## Generate and save stochasticity in niche carrying capacities
if(stochNiches.V==T){
  nicheVars <- NicheVariation(gens.V, 
                              nstarAA.V, nstarAB.V, nstarBB.V, 
                              cvAA.V, cvAB.V, cvBB.V)
  
  save(nicheVars, file=paste(outdir, "nicheVars.rda", sep = ""))
  }

remove(repeats, 
       alpha.V,  
       nstarAA.V, nstarAB.V, nstarBB.V,
       delta.V, bShepAA.V, bShepAB.V, bShepBB.V, 
       N.V, gens.V, 
       nloci.V, nAlleles.V, 
       adultmort.V, juvmort.V,
       fecundity.V, maturity.V, 
       lifespan.V, repro1.V, spawn1.V, spawn2.V, spawn3.V, 
       startcap.V, endcap.V, pBroodstock.V, pNOB.V, 
       pHOS.V, yrsCapInit.V, pNOBinit.V, 
       coffprop.V,
       directory, outdir, 
       species, 
       capSelectAB.V, capSelectAA.V, capSelectBB.V,
       inputPop.V, stochNiches.V)



## run model iterating over parameters in Replicates
for(r in 1:nrow(runvars)){
  RunModel(runvars, r)
}

## read in error file, convert to rda 
  outdir=as.character(runvars$outdir[1])
  
  csv_path <- paste(outdir, "errors.csv", sep="")
  rda_path <- paste(outdir, "errors.rda", sep="")
  
    errors <- read.csv(csv_path, header=F)
    save(errors, file = rda_path)
    
  # Confirm error file exists before removing
  if (file.exists(rda_path)) {
    invisible(file.remove(csv_path))
  } else {
    warning("RDA file was not created; CSV not removed.")
  }

## run summarize data
  SummarizeData(outdir)    

