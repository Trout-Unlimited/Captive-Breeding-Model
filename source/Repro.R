Repro = function(spawners, fecundity, nloci, g){
  #For testing:
  # fecundity=runvars$fecundity[1]; nloci=runvars$nloci[1]; nAlleles=runvars$nAlleles[1]
  # g=1

  # generation starting ID number
  ALLOFFSPRING = NULL
  #repros <- NULL
  
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
  pairs   <- cbind(males, females) #NOTE: These are the rows these fish are located in; NOT their ID's

  if(length(pairs)>0){
    nOffsp = rep(fecundity, nrow(pairs))
    total_off = sum(nOffsp)
    #times = nrow(pairings)
  }else {
    offspring = NULL
    return(offspring)
  }

  # --- Preallocate offspring metadata ---
  ALLOFFSPRING <- data.frame(
    ID = rep(0, total_off),
    age = rep(0, total_off),
    momID = rep(repros$ID[females], nOffsp),  #maps row numbers to IDs, repeats each female for her offspring
    dadID = rep(repros$ID[males], nOffsp),    #maps row numbers to IDs,  repeats each male for his offspring
    pheno = rep(NA, total_off),
    HvsW = rep(1, total_off),
    sex = sample(c(0,1), total_off, replace=TRUE),
    gen = rep(g, total_off),
    alive = rep(1, total_off),
    gendead = rep(0, total_off),
    yearSpawned = rep(0, total_off),
    fate = rep("0", total_off)
  )
  
  # --- Preallocate genotype matrix ---
  offspringG <- matrix(NA_integer_, nrow = total_off, ncol = nloci*2)
  
  # --- Get all parent genotypes ---
  fg_all <- repros[females, -c(1:12)]  # female alleles
  mg_all <- repros[males, -c(1:12)]    # male alleles
  
  # --- Vectorized allele sampling ---
  momgenes <- matrix(sample(c(0,1), total_off*nloci, replace=TRUE), nrow=total_off, ncol=nloci)
  dadgenes <- matrix(sample(c(0,1), total_off*nloci, replace=TRUE), nrow=total_off, ncol=nloci)
  
  # Fill genotype matrix without looping over pairs
  for(i in 1:nloci){
    offspringG[,i] <- fg_all[cbind(rep(1:nrow(fg_all), nOffsp), i + momgenes[,i]*nloci)]
    offspringG[,nloci+i] <- mg_all[cbind(rep(1:nrow(mg_all), nOffsp), i + dadgenes[,i]*nloci)]
  }
  
  # Combine metadata + genotypes
  ALLOFFSPRING <- cbind(ALLOFFSPRING, as.data.frame(offspringG))
  
  colnames(ALLOFFSPRING) <- c(
    "ID","age","momID","dadID","pheno","HvsW","sex","gen",
    "alive","gendead","yearSpawned","fate",
    paste0("L", 1:nloci, "m"), paste0("L", 1:nloci, "d")
  )
  
  # --- Vectorized phenotype from first locus only ---
  ALLOFFSPRING$pheno <- ALLOFFSPRING$L1m + ALLOFFSPRING$L1d
  
  return(ALLOFFSPRING)
}
    




