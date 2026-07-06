
NicheVariation = function(gens.V, 
                          nstarAA.V, nstarAB.V, nstarBB.V, 
                          cvAA.V, cvAB.V, cvBB.V){
  

  #Define function to draw from truncated normal distribution (lower bound=0, upper=infinity) 
    truncNorm <- function(n, mean, sd, lower = 0, upper = Inf) {
      out <- numeric(n)
      filled <- 0
      
      while (filled < n) {
        draws <- rnorm(n - filled, mean = mean, sd = sd)
        draws <- draws[draws >= lower & draws <= upper]
        
        n_keep <- length(draws)
        if (n_keep > 0) {
          out[(filled + 1):(filled + n_keep)] <- draws
          filled <- filled + n_keep
        }
      }
      
      return(out)
    }
    
  
    #empty matrix to fill
    stoch_mat <- matrix(NA, nrow=nrow(runvars), ncol=3*gens.V+1)
      stoch_mat[,1] <- c(1:nrow(runvars))
    
    #make draws for each
    for(i in 1:nrow(runvars)) {

      #stoch_mat[i, 1:gens.V+1] <- 
      K_AA <- truncNorm(n=gens.V, 
                  mean = nstarAA.V, 
                  sd = nstarAA.V * cvAA.V)
      
      #stoch_mat[i, (gens.V+2):(2*gens.V+1)] <- 
      K_AB <- truncNorm(n=gens.V, 
                  mean = nstarAB.V, 
                  sd = nstarAB.V * cvAB.V)
      
      #stoch_mat[i, (2*gens.V+2):(3*gens.V+1)] <- 
      K_BB <- truncNorm(n=gens.V, 
                  mean = nstarBB.V, 
                  sd = nstarBB.V * cvBB.V)
      
      # Convert back to beta
      beta_AA     = (fecundity.V-1)/K_AA                                            #carrying-capacity of AA niche (alpha in B-H)
      beta_AB     = (fecundity.V-1)/K_AB                                             #carrying-capacity of AB niche (alpha in B-H)
      beta_BB    = (fecundity.V-1)/K_BB 
      
      # Store beta values
      stoch_mat[i, 2:(gens.V + 1)] <- beta_AA
      stoch_mat[i, (gens.V + 2):(2 * gens.V + 1)] <- beta_AB
      stoch_mat[i, (2 * gens.V + 2):(3 * gens.V + 1)] <- beta_BB
      
    }
      
    stoch_df <- as.data.frame(stoch_mat)
    colnames(stoch_df) <- c(
      "run", 
      paste0("AA_", 1:gens.V), 
      paste0("AB_", 1:gens.V),
      paste0("BB_", 1:gens.V)
    )
    
    return(stoch_df)
    
}
    