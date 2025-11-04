StartingPop = function(N, agestage, nloci, sigp, alphaAB, alphaAA, alphaBB){
  
  #Create pop details (minus genetic data)
  population = matrix(nrow=N, ncol=12)
  population[,1]  = c(1:N)                                                     #individualID.momID.dadID
  population[,2]  = rep(1,N)                                                   #age (everyone starts at spawning age)
  population[,3]  = rep(0, N)                                                  #place holder for momID
  population[,4]  = rep(0, N)                                                  #place holder for dadID
  population[,5]  = rep(5000, N)                                               #placeholder for phenotype (fitness)
  population[,6]  = rep(1, N)                                                  #wild born = 1, captive born = 0
  population[,7]  = sample(c(0,1), N, replace=TRUE, prob=c(0.5, 0.5))          #male=1, female=0
  population[,8]  = rep(0,N)                                                   #generation born
  population[,9]  = rep(1,N)                                                   #alive=1, dead=0
  population[,10] = rep(0,N)                                                   #generation died
  population[,11] = rep(0,N)                                                   #year spawned
  population[,12] = rep(0,N)                                             #fate (prespawnMort, culled, spawnedHatchery, spawnedWild)
  
  colnames(population) = c("ID","age","momID","dadID","pheno","HvsW","sex","gen","alive","gendead", "yearSpawned", "fate")
  
  population <- data.frame(population)
  population$fate <- as.character(population$fate)
  

  loci = c("L1")
  alleles = c("A1", "A2")
  
  ops_comb <- expand_grid(loci, alleles) %>% 
          mutate(val = NA) %>% 
          pivot_wider(names_from = loci, values_from = val)  
  
  ops_comb[1,"L1"] = 50 #Allele A
  ops_comb[2,"L1"] = 5000 #Allele B
  
  #Add allele options for second parent
  ops_comb <- cbind(ops_comb, ops_comb[,-1])
    
  colnames(ops_comb) <- c("alleles", "L1m", "L1d")
  
   
   allOptions <- ops_comb %>% 
     expand(L1m, L1d) %>% 
     rowwise() %>% 
     mutate(pheno = sum(L1m, L1d))
   
   #Pheno AA
   set1 <- allOptions %>% filter(pheno==100) %>% 
                          ungroup %>% 
                          slice_sample(n=alphaAA, replace=T)

   #Pheno BB
   set2 <- allOptions %>% filter(pheno==10000) %>% 
                          ungroup %>% 
                          slice_sample(n=alphaBB, replace=T)

   #Pheno AB
   set3 <- allOptions %>% filter(pheno==5050) %>% 
                          ungroup %>%
                          slice_sample(n=alphaAB, replace=T)

   
   genos <- rbind(set1, set2, set3)
   
   population$pheno <- genos$pheno
   
  
  ########################################################
   ## Add the neutral diversity (loci of small effect)
  ########################################################
   loci_neu = c("L2m", "L3m", "L4m", "L5m", "L6m", "L7m", "L8m", "L9m", "L10m", "L11m", 
                    "L12m", "L13m", "L14m", "L15m", "L16m", "L17m", "L18m", "L19m", "L20m", "L21m",
                    "L22m", "L23m", "L24m", "L25m", "L26m", "L27m", "L28m", "L29m", "L30m", "L31m")

   
   alleles_neu = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10")
   
   ops_neu <- expand_grid(loci_neu, alleles_neu) %>% 
     mutate(val = NA) %>% 
     pivot_wider(names_from = loci_neu, values_from = val)  
   
   #Create all options for alleles
   for(i in 2:ncol(ops_neu)){
     ops_neu[,i] = seq((i-1)*10, (i-1)*10+9)
   }
   
   #Add allele options for second parent
   ops_neu_comb <- as.matrix(cbind(ops_neu[,-1], ops_neu[,-1]))
   
   #Create neutral genotype matrix
     genos_neu <- matrix(data=0,nrow=N,ncol=(nloci-1)*2)

     for (i in 1:N){
       j<-sample.int(n=10,size=(2*(nloci-1)),replace=TRUE)

       #Maternal alleles
       for (k in 1:nloci-1){
         #genos_neu[i,k] <- ops_neu_comb[k,j[k]]
         genos_neu[i,k] <- ops_neu_comb[j[k], k]
       }

       #Paternal alleles
         for (h in 31:((nloci-1)*2)){
           genos_neu[i,h] <- ops_neu_comb[j[h], h]
           
         }

       mode(genos_neu) <- "integer"

     }
   
     loci_neu_dad = c("L2d", "L3d", "L4d", "L5d", "L6d", "L7d", "L8d", "L9d", "L10d", "L11d", 
                      "L12d", "L13d", "L14d", "L15d", "L16d", "L17d", "L18d", "L19d", "L20d", "L21d",
                      "L22d", "L23d", "L24d", "L25d", "L26d", "L27d", "L28d", "L29d", "L30d", "L31d")
     
     colnames(genos_neu) <- c(loci_neu, loci_neu_dad)
     
     #Separate data so I can cbind in correct order below
     momL1m <- genos[,"L1m"]
     dadL2d <- genos[,"L1d"]
      mom_neuGenos <- genos_neu[,1:30]
      dad_neuGenos <- genos_neu[,31:60]
     
     
     population = cbind(population, momL1m, mom_neuGenos, dadL2d, dad_neuGenos)
   
   
   return(population)
}



