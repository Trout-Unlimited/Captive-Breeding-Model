WriteOut = function(runvars, r){
  ## Write out any error codes:
  errorCodes = cbind(r, alldead)
  #colnames(errorCodes) <- c("run", "alldead")
  write.table(errorCodes, paste(outdir, "errors.csv", sep = ""), sep=",",col.names=F,append=T,quote=F,row.names=F) 
  
  ##Write out starting table
  runvars <- runvars %>% rename(run = Replicate)
  save(runvars, file=paste(outdir, "summary.rda", sep = ""))
  
  
  ############################################
  ## ORIGINAL
  ############################################
  
  # #get variables for run
  # gens          = runvars$gens[r] + runvars$lifespan[r] + 1
  # K             = runvars$K[r]
  # N             = runvars$N[r]
  # fecundity     = runvars$fecundity[r]
  # maturity      = runvars$maturity[r]
  # lifespan      = runvars$lifespan[r]
  # repro1        = runvars$repro1[r]
  # RRS           = runvars$RRS[r]
  # RRSvar        = runvars$RRSvar[r]
  # r0            = runvars$r0[r]
  # l             = runvars$l[r]
  # #perSpawn1     = runvars$perSpawn1[r]
  # #perSpawn2     = runvars$perSpawn2[r]
  # #perSpawn3     = runvars$perSpawn3[r]
  # #pBroodstock   = runvars$pBroodstock[r]
  # #pNOB          = runvars$pNOB[r]         
  # #pHOS          = runvars$pHOS[r]
  # #yrsCapInit    = runvars$yrsCapInit[r]   
  # #pNOBinit      = runvars$pNOBinit[r]
  # nloci         = runvars$nloci[r]
  # countimmat    = runvars$countimmat[r]
  # killimmat     = runvars$killimmat[r]
  # adultmort     = runvars$adultmort[r]
  # startcap      = runvars$startcap[r]
  # endcap        = runvars$endcap[r]
  # capfound      = runvars$capfound[r]
  # newfound      = runvars$newfound[r]
  # caplife       = runvars$caplife[r]
  # poorenv       = runvars$poorenv[r]
  # coffprop      = runvars$coffprop[r]
  # fixedenv      = runvars$fixedenv[r]
  # lossperct     = runvars$lossperct[r]
  # nimmigrants   = runvars$nimmigrants[r]
  # calcfitness   = runvars$calcfitness[r]
  # propkill      = runvars$propkill[r]
  # equalcaptive  = runvars$equalcaptive[r]
  # allonecaptive = runvars$allonecaptive[r]
  # capvariation  = runvars$capvariation[r]
  # directory     = runvars$directory[r]
  # outdir        = runvars$outdir[r]
  # plotit        = runvars$plotit[r]
  # species       = runvars$species[r]
  # 
  # population = read.csv(paste(outdir, "population_indvs_", 
  #                             formatC(r, width = 4, format = "d", flag = "0"), 
  #                             ".csv", sep=""), as.is=TRUE, header=TRUE) #Read back in the pop output
  # 
  # population = population[population[,10]!=0,,drop=FALSE] #Return everyone except for gendead=0; there should be no gendead=0 in here!
  # population = population[!duplicated(population[,1]),] #Exclude duplicated individuals
  # 
  # #check for individual ID numbers 
  # if(!length(population[,1])==length(unique(population[,1]))){
  #   print(r)
  #   print("NON UNIQUE ID NUMBERS")
  #   return()
  # }
  # #if no individuals for simulation, exit
  # if(length(population[,1])==0){ return() }
  # 
  # # set up for calculating fitness by birth year - isolate individuals who made it to maturity (for all years), set up dataframe
  # matpop  = population[population[,"age"]>=maturity,]     #####if 0, matpop is all, even though labeld like it is just mature individuals
  # maxage = 10
  # 
  # #calculate summary statistics and write to output
  # 
  # ## Set up 
  # OUT = matrix(nrow=gens-lifespan, ncol=9 + (maxage +1))
  # AGES = NULL
  # for(a in 0:maxage){AGES = c(AGES, paste("age", a, sep=""))}
  # colnames(OUT) = c("year", "propHatchery", "He", "Ho", "polyLoci", "Nalleles","meanRRSall", "meanRRSwild","meanRRScap",AGES)
  # 
  # #add generations and survival per year to summary matrix
  # OUT[ ,1] = c(1:nrow(OUT))
  # 
  # #create record for failed introductions, all values will be zero
  # if(dim(OUT)[1] == 0){
  #   OUT = matrix(nrow=1, ncol=9 + (maxage +1))
  #   AGES = NULL
  #   for(a in 0:maxage){AGES = c(AGES, paste("age", a, sep=""))}
  #   colnames(OUT) = c("year", "propHatchery", "He", "Ho", "polyLoci", "Nalleles","meanRRSall", "meanRRSwild","meanRRScap",AGES)
  #   OUT[1,] = 0
  # }else {
  #   for(g in 1:nrow(OUT)){
  #     year = OUT[g,1]
  #     
  #     #separate alive in current year (g)
  #     gendata = matpop[matpop[ ,8]<=year, , drop=FALSE]     #individuals born in or before current year
  #     gendata = gendata[gendata[ ,10]>=year, , drop=FALSE]  #individuals died in or after current year
  #     
  #     x = NULL
  #     x = try(length(gendata[,1]), silent=TRUE)
  #     if(is.null(x)){break}
  #     if(x<1){break}
  #     if(!is.numeric(x)){break}
  #     
  #     #Proportion of hatchery-born in population
  #     OUT[g,2] = 1 - sum(gendata[,6])/length(gendata[,1]) #note: wild=1, so 1-sum(wild)/total = %hatchery in cohort
  #     
  #     #He and Ho - neutral
  #     genos = gendata[,11:((nloci*2)+10), drop=FALSE] 
  #     SNPs  = rep(c(1,2),ncol(genos)/2)
  #     
  #     HEs   = NULL
  #     HOs   = NULL
  #     Als   = NULL
  #     
  #     loc.pos = seq(1, (nloci*2), 2)
  #     for(l in loc.pos){
  #       # per locus heterozygosity
  #       locus <- genos[, c(l, l+1), drop=FALSE]
  #       geno  <- length(locus[, 1])
  #       het   <- length(which(locus[, 1] != locus[, 2]))
  #       het.observed  <- het/geno
  #       HOs = c(HOs, het.observed)
  #       
  #       freqs <- table(locus)
  #       homozygous = NULL
  #       for(l in 1:length(freqs)){
  #         homozygous = c(homozygous, (freqs[l]/sum(freqs) * freqs[l]/sum(freqs)))
  #       }
  #       het.expected <- 1 - sum(homozygous)
  #       HEs = c(HEs, het.expected)
  #       
  #       catlocus = c(locus[,1], locus[,2])
  #       Als = c(Als, length(unique(catlocus)))
  #     }
  #     OUT[g,3 ] <- mean(HEs)
  #     OUT[g,4 ] <- mean(HOs)
  #     
  #     #polymorphic loci
  #     OUT[g,5 ] <- 1-(length(which(HEs == 0))/length(HEs))
  #     
  #     #number of alleles
  #     OUT[g,6 ] <- mean(Als)
  #     
  #     #JUST FIXED THIS, NEED TO MOVE DOWN
  #     OUT[g,7] = mean(gendata[,5])
  #     OUT[g,8] = mean(gendata[gendata[,6]==1,5])
  #     OUT[g,9] = mean(gendata[gendata[,6]==0,5])
  #     
  #     #calculate number of individuals per age class in current year
  #     ages = year - gendata[,8] 
  #     for(a in 0:maxage){
  #       A = subset(ages, ages==a)
  #       OUT[g,10+a] = length(A)
  #     }
  #   }
  # }
  # 
  # params = runvars[rep(r, nrow(OUT)),]
  # outall = cbind(OUT, params)
  # # colnames(outall) = c("year", "propHatchery", "He", "Ho", "polyLoci", "Nalleles","meanRRSall", "meanRRSwild","meanRRScap",AGES, 
  # #                      "K", "N", "gens", "RRS", "RRSvar", "r0", "l", 
  # #                      "nloci", "countimmat", "killimmat", "adultmort", "fecundity", "maturity", "lifespan", "repro1",
  # #                      "perSpawn1", "perSpawn2", "perSpawn3",  
  # #                      "startcap", "endcap", "pBroodstock", "pNOB", "pHOS", "yrsCapInit", "pNOBinit",
  # #                      "coffprop","poorenv", "fixedenv", "lossperct", 
  # #                      "nimmigrants", "propkill", "equalcaptive", "capvariation", "directory", "outdir", "plotit", "species", 
  # #                      "selection", "keep")
  # 
  # ## Write out summary data: 
  # write.table(outall, paste(outdir, "summary_",
  #                           formatC(r, width = 4, format = "d", flag = "0"), 
  #                           ".csv", sep = ""), sep=",",col.names=T,append=F,quote=F,row.names=F)  
  
 
  
}
