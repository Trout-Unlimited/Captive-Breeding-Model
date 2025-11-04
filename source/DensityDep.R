DensityDep = function(offspring, alphaAB, alphaAA, alphaBB, beta, g, l, totalinds){
  
  ### Three categories of density dependence (based on geno and phenotype, low, med, high)
  
  #NOTE: the low, med, and high designations have changed over time
  ## The genotype designations are correct. Within this section of code, however, 
  ## I have the corresponding with low, med, and high phenotypes that do not match
  ## the labels we gave them in the final version of the ms
  
  #NOTE: These say low but are the AB phenotype
  n_bhLow <- offspring %>% 
                  filter(alive==1, pheno==5050) %>% 
                  ungroup() %>% 
                  summarise(nLow = n(), 
                          nLowSurvive = as.integer((alphaAB*nLow)/(beta+nLow))) 
  
  lowIndivs <- offspring %>% 
    filter(alive==1, pheno==5050) %>% 
    ungroup() %>% 
    slice_sample(n = n_bhLow$nLowSurvive, replace = T)
                  
  #NOTE: these say 'med' but are the AA phenotype
  n_bhMed <- offspring %>% 
    filter(alive==1, pheno==100) %>% 
    ungroup() %>% 
    summarise(nMed = n(), 
              nMedSurvive = as.integer((alphaAA*nMed)/(beta+nMed))) 
  
  medIndivs <- offspring %>% 
    filter(alive==1, pheno==100) %>% 
    ungroup() %>% 
    slice_sample(n = n_bhMed$nMedSurvive, replace = T)
 
 #NOTE: these say 'high' but are the BB phenotype  
  n_bhHigh <- offspring %>% 
    filter(alive==1, pheno==10000) %>% 
    ungroup() %>% 
    summarise(nHigh = n(), 
              nHighSurvive = as.integer((alphaBB*nHigh)/(beta+nHigh))) 
  
  highIndivs <- offspring %>% 
    filter(alive==1, pheno==10000) %>% 
    ungroup() %>% 
    slice_sample(n = n_bhHigh$nHighSurvive, replace = T)
  
  offspring <- rbind(lowIndivs, medIndivs, highIndivs)
  

  offspring$ID <- seq(from = totalinds, to = totalinds + nrow(offspring)-1, by = 1) 
  
  
  return(offspring)
}

