Replicates = function(repeats, alphaAB.V, alphaAA.V, alphaBB.V, 
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
                      mu.V, alpha2.V) { 
  
 
  
  replicates = expand.grid(repeats, alphaAB.V, alphaAA.V, alphaBB.V, 
                           N.V, gens.V, 
                           beta.V, l.V, nloci.V, sigp.V,
                           adultmort.V, fecundity.V, maturity.V, 
                           lifespan.V, repro1.V, spawn1.V, spawn2.V, spawn3.V, 
                           startcap.V, endcap.V, pBroodstock.V, pNOB.V, pHOS.V, 
                           yrsCapInit.V, pNOBinit.V, 
                           coffprop.V,
                           nimmigrants.V,  
                           directory, outdir, species, 
                           capSelectAB.V, capSelectAA.V, capSelectBB.V,
                           inputPop.V, 
                           mu.V, alpha2.V)
  
  colnames(replicates) = c("Replicate", "alphaAB", "alphaAA", "alphaBB", 
                           "N", "gens", 
                           "beta", "l", "nloci", "sigp", 
                           "adultmort", "fecundity", "maturity", 
                           "lifespan", "repro1", "perSpawn1", "perSpawn2", "perSpawn3",  
                           "startcap", "endcap", "pBroodstock", "pNOB", "pHOS",
                           "yrsCapInit", "pNOBinit", 
                           "coffprop", 
                           "nimmigrants",
                           "directory", "outdir", "species", 
                           "capSelectAB", "capSelectAA", "capSelectBB",
                           "inputPop", "mu", "alpha2")
 

  reps = NULL
  for(r in 1:repeats){
    reps = rbind(reps, replicates)
  }
  replicates = reps
  replicates$Replicate <- seq(1:nrow(replicates))
  
  return(replicates)
}  


