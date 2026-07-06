#Set working directory, import packages, source functions, 
setwd(paste(directory,"/source/", sep = ''))    # set temp working directory 

#import packages
#library()

#source functions
source(paste(getwd(), "/AgeUp.R", sep = ''))
source(paste(getwd(), "/AllDead.R", sep = ''))
source(paste(getwd(), "/CaptiveRepro.R", sep = ''))
source(paste(getwd(), "/IdentifySpawners.R", sep = ''))
source(paste(getwd(), "/DensityDepShepherd.R", sep = ''))
source(paste(getwd(), "/NicheVariation.R", sep = ''))
source(paste(getwd(), "/OffPhenos.R", sep = ''))
source(paste(getwd(), "/Replicates.R", sep = ''))
source(paste(getwd(), "/Repro.R", sep = ''))
source(paste(getwd(), "/RunModel.R", sep = ''))
source(paste(getwd(), "/StartingPop.R", sep = ''))
source(paste(getwd(), "/SavePopLeftovers.R", sep = ''))
source(paste(getwd(), "/WriteOut.R", sep = ''))
source(paste(getwd(), "/SummarizeData.R", sep = ''))


