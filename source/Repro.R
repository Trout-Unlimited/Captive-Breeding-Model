Repro = function(spawners, fecundity, nloci, sigp, g, mu, alpha2){
  

  # generation starting ID number
  ALLOFFSPRING = NULL
  repros <- NULL
  
  # Make dataframe of parents that will spawn in the wild
  repros <- spawners %>% filter(SpawnLocation=="wild") %>% ungroup() %>% select(!SpawnLocation)

  # check that there are at least 2 individuals, one male and one female
  if(nrow(repros)==0 | is.null(repros)){return(ALLOFFSPRING)}
  if(length(repros[repros[,7]==0,1])<1){return(ALLOFFSPRING)} 
  if(length(repros[repros[,7]==1,1])<1){return(ALLOFFSPRING)}
  
  # randomly pair males and females
  size    <- min(table(repros[, "sex"])) #which sex has fewer - males or females?
  males   <- sample(which(repros[, "sex"] == 1), size, replace = FALSE)
  females <- sample(which(repros[, "sex"] == 0), size, replace = FALSE)
  pairs   <- as.matrix(cbind(males, females)) #NOTE: These are the rows these fish are located in; NOT their ID's

  if(length(pairs)>0){
    #pairings = matrix(nrow=length(pairs[,1]), ncol = 3)
    #pairings[,1] = pairs[,1]
    #pairings[,2] = pairs[,2]
    nOffsp = rep(fecundity, nrow(pairs))
    pairs = cbind(pairs, nOffsp)
    #times = nrow(pairings)
  }else {
    offspring = NULL
    return(offspring)
  }


  #ALLOFFSPRING <- NULL

  ############################
  # generate offspring
  ############################
  for(p in 1:nrow(pairs)) {  #times determined above, with pairing, and indicates the number of pairs
    if(nrow(pairs)>1){
      f = pairs[p, 2] #females
      m = pairs[p, 1] #males
      noff = pairs[p, 3]
    }
    if(nrow(pairs)==1){
      f = pairs[2]
      m = pairs[1]
      noff = pairs[3]
    }
    
    if(noff > 0){

      #population offpsring matrix
      offspring      = matrix(nrow=noff, ncol=10)
      offspring[,1]  = rep(0, noff)                                                                      #ID
      offspring[,2]  = rep(0, noff)                                                              #age at current time
      offspring[,3]  = rep(as.numeric(repros[f,1], noff))                                                    #mom ID
      offspring[,4]  = rep(as.numeric(repros[m,1], noff))                                                    #dad ID
      offspring[,5] = NA #Genotype placeholder
      offspring[,6]  = rep(1, noff)                                                              #wild born = 1, captive born = 0
      offspring[,7]  = sample(c(0,1), noff, replace=TRUE, prob=c(0.5, 0.5))                      #male=1, female=0
      offspring[,8]  = rep(g, noff)                                                              #generation born                                               
      offspring[,9]  = rep(1, noff)                                                              #1=alive,0=dead
      offspring[,10] = rep(0, noff)                                                              #generation individual died
      
      # assign genotypes to offspring: 
      
        # parent genotypes
        fg = data.matrix(repros[f, -c(1:12)])
        mg = data.matrix(repros[m, -c(1:12)])
        
        # fg = data.matrix(repros %>% filter(ID==f) %>% select(L1m:L31d))
        # mg = data.matrix(repros %>% filter(ID==m) %>% select(L1m:L31d))
        

        # prep offspring genotype matrix
        offspringG = matrix(nrow=noff, ncol=((nloci)*2))
 
      for(n in 1:noff){
        #Identify which loci mom and dad genes will come from (mom's mom or mom's dad)
        momgenes <- sample(c(0,31), nloci, replace=TRUE, prob=c(0.5, 0.5)) 
        dadgenes <- sample(c(0,31), nloci, replace=TRUE, prob=c(0.5, 0.5)) 
        
        # randomly select alleles from mom 
        for(i in 1:nloci) {
          #offspringG[n,i] <- fg[1,momgenes[i]+i]
          offspringG[n,i] <- fg[1,momgenes[i]+i]
        }
       
        # randomly select alleles from dad 
        for(j in 1:nloci) {
          offspringG[n,j+31] <- mg[1,dadgenes[j]+j]  
        }
        #Mutation
        #if (runif(1)<(nloci*mu)==T) {
        if (as.numeric(runif(1, min=0, max=1)<(nloci*mu))==1) {   #original
          u<-sample.int(20,1)
          offspringG[n,u] <- round(offspringG[n,u]+rnorm(1,0,sqrt(alpha2)),0)
        }
        
        #offspring[n,5] <- rnorm(1,sum(offspringG[n,]),sigp) #Phenotype with heritable variation
        #offspring[n,5] <- sum(offspringG[n,]) #Phenotype
        
        
      }
        mode(offspringG) <- "integer"
        
        offspring <- data.frame(offspring) #offspring metadata
          offspringG <- data.frame(offspringG) #offspring genotypes
          
          #Combine offspring metadata and genotypes
          offspring    = cbind(offspring, offspringG)
          
        if(p==1){ALLOFFSPRING = offspring} else{
          ALLOFFSPRING = rbind(ALLOFFSPRING, offspring)}
      
    }
  }
        
  colnames(ALLOFFSPRING) <- c("ID","age","momID","dadID","pheno","HvsW","sex","gen","alive",      
                               "gendead", "L1m", "L2m", "L3m", "L4m", "L5m", "L6m", "L7m", "L8m", "L9m", "L10m", "L11m", 
                              "L12m", "L13m", "L14m", "L15m", "L16m", "L17m", "L18m", "L19m", "L20m", "L21m",
                              "L22m", "L23m", "L24m", "L25m", "L26m", "L27m", "L28m", "L29m", "L30m", "L31m", 
                              "L1d", "L2d", "L3d", "L4d", "L5d", "L6d", "L7d", "L8d", "L9d", "L10d", "L11d", 
                              "L12d", "L13d", "L14d", "L15d", "L16d", "L17d", "L18d", "L19d", "L20d", "L21d",
                              "L22d", "L23d", "L24d", "L25d", "L26d", "L27d", "L28d", "L29d", "L30d", "L31d")
  
  ALLOFFSPRING <- ALLOFFSPRING %>% 
                  rowwise() %>% 
                  mutate(pheno = sum(L1m, L1d))
  
  ALLOFFSPRING$yearSpawned <- 0     
  ALLOFFSPRING$fate <- "0" 

  
  
  #Add in variation to phenotypes (heritability)
  # ALLOFFSPRING <- ALLOFFSPRING %>% 
  #   mutate(pheno = if_else(pheno>0 & pheno<100, pheno-(pheno*runif(1, min=0, max=sigp)), 
  #                          if_else(pheno<0 & pheno>-100, pheno+(pheno*runif(1, min=0, max=sigp)), 
  #                             if_else(pheno>99 & pheno<150, pheno+(pheno*runif(1, min=0, max=sigp)), 
  #                             if_else(pheno>150, pheno-(pheno*runif(1, min=0, max=sigp)),
  #                                 if_else(pheno< -99 & pheno> -150, pheno-(pheno*runif(1, min=0, max=sigp)),
  #                                                                          pheno+(pheno*runif(1, min=0, max=sigp))))))))

  return(ALLOFFSPRING)
}



#look for migrants and sub into list if present
## HO: commented out because migrants are added to population at an earlier step
# if(nrow(pairs)>0){
#   if(!is.null(migrants)){
#     numberIs = nrow(migrants)
#     sexes    = migrants[,7]
#     for(i in 1:nrow(migrants)){
#       if(nrow(pairs)>=nrow(migrants)){
#         if(sexes[i]==1){
#           pairs[i,1] = nrow(repros) - i + 1
#         }
#         if(sexes[i]==0){
#           pairs[i,2] = nrow(repros) - i + 1
#         }
#       }else{
#         if(sexes[i]==1){
#           pairs[1,1] = nrow(repros) - i + 1
#         }
#         if(sexes[i]==0){
#           pairs[1,2] = nrow(repros) - i + 1
#         }
#       }
#     }
#   }
# }else{
#   numberIs = nrow(migrants)
#   sexes    = migrants[,7]
#   ms = length(subset(sexes, sexes==1))
#   fs = length(subset(sexes, sexes==0))
#   pairs   = matrix(nrow=min(ms, fs), ncol = 2)
#   if(pairs>0){
#     for(i in 1:nrow(pairs)){
#       if(sexes[i]==1){
#         pairs[i,1] = nrow(repros) - i + 1
#       }
#       if(sexes[i]==0){
#         pairs[i,2] = nrow(repros) - i + 1
#       }
#     }
#   }else{
#     pairs = 0
#     return(pairs)
#   }
# }


# x = NULL
# x = try(length(pairs), silent = TRUE)



