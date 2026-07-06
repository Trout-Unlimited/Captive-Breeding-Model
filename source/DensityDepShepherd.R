DensityDepShepherd = function(offspring, g, totalinds, gens, r, 
                              alpha, delta, bShepAA, bShepAB, bShepBB, 
                              stochNiches){
  
  #for testing: gens<-runvars$gens[r]; stochNiches=runvars$stochNiches[r]
              # delta <- runvars$delta[r]; bShepAA <- runvars$bShepAA[r] 
              # bShepAB <- runvars$bShepAB[r]; bShepBB <- runvars$bShepBB[r]
              #alpha <- runvars$alpha[r]

  #retrive beta value for the year  
  # if(stochNiches==T){
  # betaAA <- nicheVars[r,g+1]
  #   betaAB <- nicheVars[r,gens+1+g]
  #   betaBB <- nicheVars[r,2*gens+1+g]
  # } else {
  #     betaAA=betaAA
  #     betaAB=betaAB
  #     betaBB=betaBB
  #   }
   
  #filter out alive and possible phenos   
  alive <- offspring$alive == 1
  pheno <- offspring$pheno
  
  #count each phenotype
  n_AA <- sum(alive & pheno == 100)
  n_AB <- sum(alive & pheno == 5050)
  n_BB <- sum(alive & pheno == 10000)
  
  #Compute survivors
  nAASurvive <- as.integer((alpha*n_AA) / (1 + bShepAA * n_AA^delta))
  nABSurvive <- as.integer((alpha*n_AB) / (1 + bShepAB * n_AB^delta))
  nBBSurvive <- as.integer((alpha*n_BB) / (1 + bShepBB * n_BB^delta))
 
  
  #Get row indices
  idx_AA <- which(alive & pheno == 100)
  idx_AB <- which(alive & pheno == 5050)
  idx_BB <- which(alive & pheno == 10000)
  
  #Sample individuals for next gen
  AA_keep <- if (nAASurvive > 0) sample(idx_AA, nAASurvive) else integer(0)
  AB_keep <- if (nABSurvive > 0) sample(idx_AB, nABSurvive) else integer(0)
  BB_keep <- if (nBBSurvive > 0) sample(idx_BB, nBBSurvive) else integer(0)
  
  #Subset surviving offspring
  keep <- c(AA_keep, AB_keep, BB_keep)
  offspring <- offspring[keep, ]
  
  #Assign IDs
  offspring$ID <- seq(from = totalinds, length.out = nrow(offspring))
  
  return(offspring)
}

