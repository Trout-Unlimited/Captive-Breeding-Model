#Set working directory, import packages, source functions, 
setwd(paste(directory,"/source/", sep = ''))    # set temp working directory 

#import packages
#library()

#source functions
source(paste(getwd(), "/AgeUp.R", sep = ''))
source(paste(getwd(), "/AllDead.R", sep = ''))
source(paste(getwd(), "/CaptiveRepro.R", sep = ''))
source(paste(getwd(), "/IdentifySpawners.R", sep = ''))
source(paste(getwd(), "/DensityDep.R", sep = ''))
source(paste(getwd(), "/Replicates.R", sep = ''))
source(paste(getwd(), "/Repro.R", sep = ''))
source(paste(getwd(), "/RunModel.R", sep = ''))
source(paste(getwd(), "/StartingPop.R", sep = ''))
source(paste(getwd(), "/SavePopLeftovers.R", sep = ''))
source(paste(getwd(), "/WriteOut.R", sep = ''))

#Old
# source(paste(getwd(), "/AdultMortality.R", sep = ''))
# source(paste(getwd(), "/CalculateNt.R", sep = ''))
# source(paste(getwd(), "/CaptivePairs.R", sep = ''))
# source(paste(getwd(), "/CaptiveSelection.R", sep = ''))
# source(paste(getwd(), "/Immigrant.R", sep = ''))
# source(paste(getwd(), "/NumberOffspring.R", sep = ''))
# source(paste(getwd(), "/Pairs.R", sep = ''))
# source(paste(getwd(), "/WildSelection.R", sep = ''))
