AllDead = function(alldead, population, outdir, r){
  
  #Write out everyone that has died
  writeout = population %>% filter(alive==0)
  
  # writeout = rbind(population[population[,9]==-99,,drop=FALSE], 
  #                  population[population[,9]==-101,,drop=FALSE], 
  #                  population[population[,9]==0,,drop=FALSE])  
  x = nrow(writeout)
  if(x > 0){
    forwriteout = apply(writeout, 1, function(z){paste(z, collapse = ",")})
    write(forwriteout, paste(outdir, "population_indvs_", 
                             formatC(r, width = 4, format = "d", flag = "0"), #Give the number preceeding zeros
                             ".csv", sep=""), ncolumns=1, append=TRUE)  
  }
  
  population <- population %>% filter(alive==1)
  return(population)
  
}
