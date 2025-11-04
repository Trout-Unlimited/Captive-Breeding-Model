
## Saves all the alive individuals at end of loops

SavePopLeftovers = function(population, runvars, r, alldead, outdir){
  
  # write_csv(population, file=paste(outdir, "PopulationLeftOvers_",
  #                                  formatC(r, width = 4, format = "d", flag = "0"),
  #                                  ".csv", sep = ""), col_names=T)
  
  ## Write out any error codes:
  errorCodes = cbind(r, alldead)
  write.table(errorCodes, paste(outdir, "errors.csv", sep = ""), 
              sep=",",col.names=F,append=T,quote=F,row.names=F) 
  
  
  # save(population, file = paste(runvars$outdir[r], "PopulationLeftOvers_",
  #                           formatC(r, width = 4, format = "d", flag = "0"), 
  #                           ".rda", sep = ""))
}

