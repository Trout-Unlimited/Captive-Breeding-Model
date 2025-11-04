CaptiveRepro = function(spawners, g, nloci, totalinds, sigp, coffprop, mu, alpha2, 
                        pHOS, capSelectAB, capSelectAA, capSelectBB, fecundity){

  #let captive population reproduce (randomly paired)
        repros = spawners %>% filter(SpawnLocation=="hatchery") %>% ungroup() %>% select(!SpawnLocation)
        gstartID = totalinds
        ALLOFFSPRING = NULL 
        
        # check that there are at least 2 individuals, one male and one female
        if(is.null(nrow(repros))) {return(ALLOFFSPRING)}
        if(length(repros[repros[, "sex"] == 0, 1]) < 1) {return(ALLOFFSPRING)} 
        if(length(repros[repros[, "sex"] == 1, 1]) < 1) {return(ALLOFFSPRING)}
        
        # randomly pair males and females
        size    <- min(table(repros[, "sex"]))  #Which sex has fewer
        males   <- sample(which(repros[, "sex"] == 1), size, replace = FALSE) #Randomly sample that number of males
        females <- sample(which(repros[, "sex"] == 0), size, replace = FALSE) #Randomly sample that number of females
        pairs   <- cbind(males, females) #Combine them
        
        
        #Make 10 offspring per pair, then sample (with duplication) the phenotypes I need
        totalCapOff <- fecundity*nrow(pairs)
        
        if(length(pairs)>0){
          perCapOff <- round(totalCapOff/nrow(pairs),0)
          nOffsp = rep(perCapOff, nrow(pairs))
          pairs = cbind(pairs, nOffsp)
          #times = nrow(pairings)
        }else {
          coffspring = NULL
          return(coffspring)
        }
        
        
        
        ############################
        # generate offspring
        ############################
        for(p in 1:nrow(pairs)) {  #times determined above, with pairing, and indicates the number of pairs
          if(nrow(pairs)>1){
            f = pairs[p, 2]
            m = pairs[p, 1]
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
            offspring[,6]  = rep(0, noff)                                                              #wild born = 1, captive born = 0
            offspring[,7]  = sample(c(0,1), noff, replace=TRUE, prob=c(0.5, 0.5))                      #male=1, female=0
            offspring[,8]  = rep(g, noff)                                                              #generation born                                               
            offspring[,9]  = rep(1, noff)                                                              #1=alive,0=dead
            offspring[,10] = rep(0, noff)                                                              #generation individual died
            
            # assign genotypes to offspring: 
            
            # parent genotypes
            fg = data.matrix(repros[f, -c(1:12)])
            mg = data.matrix(repros[m, -c(1:12)])
            
            # prep offspring genotype matrix
            offspringG = matrix(nrow=noff, ncol=((nloci)*2))
            
            for(n in 1:noff){
              #Identify which loci mom and dad genes will come from (mom's mom or mom's dad)
              momgenes <- sample(c(0,31), nloci, replace=TRUE, prob=c(0.5, 0.5)) 
              dadgenes <- sample(c(0,31), nloci, replace=TRUE, prob=c(0.5, 0.5)) 
              
              # randomly select alleles from mom 
              for(i in 1:nloci) {
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
        
        
        
        #####################################
        # find phenotype & genotype composition, calculate what percentage of each phenotype
        ## based on BS composition and survival probability
        
          offspComp <- ALLOFFSPRING %>%
          group_by(pheno) %>%
          summarise(n = n()) %>%
          mutate(survivors = if_else(pheno==100, as.integer(n*capSelectAA),
                                   if_else(pheno==5050, as.integer(n*capSelectAB),
                                           if_else(pheno==10000, as.integer(n*capSelectBB), NA))), 
                 newTot = sum(survivors)) %>% 
          filter(survivors>0)
        
        if(nrow(offspComp)<1){
            ALLOFFSPRING = NULL
            return(ALLOFFSPRING)
          }
        #}
        
        offspComp <- offspComp %>% 
                      group_by(pheno) %>% 
                      mutate(overallPer = survivors/newTot, 
                      nContrib = as.integer(coffprop*overallPer))
        
        #openxlsx::write.xlsx(offspComp, "C:/Users/Haley.Ohms/OneDrive - Trout Unlimited/Documents/Proj_Hatcheries/HatchProdTable.xlsx")
        
        if(100 %in% unique(offspComp$pheno)){
          nMed = offspComp %>% filter(pheno==100) %>% ungroup() %>% select(nContrib) %>% pull()
        } else {
          nMed = 0
        }
        
        if(5050 %in% unique(offspComp$pheno)){
          nLow = offspComp %>% filter(pheno==5050) %>% ungroup() %>% select(nContrib) %>% pull()
        } else {
          nLow = 0
        }
          
        if(10000 %in% unique(offspComp$pheno)){
          nHigh = offspComp %>% filter(pheno==10000) %>% ungroup() %>% select(nContrib) %>% pull()
        } else {
          nHigh = 0
        }
        
        # ## Now generate complete set of offspring based on phenotype and composition
        offspringLow <- ALLOFFSPRING %>%
                        filter(pheno==5050) %>%
                        ungroup() %>% 
                        slice_sample(n=nLow, replace=T)

        offspringMed <- ALLOFFSPRING %>%
          filter(pheno==100) %>%
          ungroup() %>% 
          slice_sample(n=nMed, replace=T)

        offspringHigh <- ALLOFFSPRING %>%
          filter(pheno==10000) %>%
          ungroup() %>% 
          slice_sample(n=nHigh, replace=T)
          
        ALLOFFSPRING = rbind(offspringLow, offspringMed, offspringHigh)
        
        if(nrow(ALLOFFSPRING)<1){
          ALLOFFSPRING = NULL
          return(ALLOFFSPRING)
        }

        ALLOFFSPRING$ID <- seq(from = gstartID, to = gstartID + nrow(ALLOFFSPRING)-1, by = 1)

        
        return(ALLOFFSPRING)
    }
