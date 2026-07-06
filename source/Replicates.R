Replicates = function(repeats, 
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
                      inputPop.V, stochNiches.V) { 
  
 
  
  replicates = expand.grid(repeats, 
                           alpha.V,  
                           nstarAA.V, nstarAB.V, nstarBB.V,
                           delta.V, bShepAA.V, bShepAB.V, bShepBB.V, 
                           N.V, gens.V, 
                           nloci.V, nAlleles.V, 
                           adultmort.V, juvmort.V,
                           fecundity.V, maturity.V, 
                           lifespan.V, repro1.V, spawn1.V, spawn2.V, spawn3.V, 
                           startcap.V, endcap.V, pBroodstock.V, pNOB.V, pHOS.V, 
                           yrsCapInit.V, pNOBinit.V, 
                           coffprop.V,  
                           directory, outdir, species, 
                           capSelectAB.V, capSelectAA.V, capSelectBB.V,
                           inputPop.V, stochNiches.V)
  
  colnames(replicates) = c("Replicate", 
                           "alpha", 
                           "nstarAA", "nstarAB", "nstarBB",
                           "delta", "bShepAA", "bShepAB", "bShepBB", 
                           "N", "gens", 
                           "nloci", "nAlleles",
                           "adultmort", "juvmort", 
                           "fecundity", "maturity", 
                           "lifespan", "repro1", "perSpawn1", "perSpawn2", "perSpawn3",  
                           "startcap", "endcap", "pBroodstock", "pNOB", "pHOS",
                           "yrsCapInit", "pNOBinit", 
                           "coffprop",
                           "directory", "outdir", "species", 
                           "capSelectAB", "capSelectAA", "capSelectBB",
                           "inputPop", "stochNiches")
  
  reps = NULL
  for(r in 1:repeats){
    reps = rbind(reps, replicates)
  }
  replicates = reps
  replicates$Replicate <- seq(1:nrow(replicates))
  
  return(replicates)
}  


