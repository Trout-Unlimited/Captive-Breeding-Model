SaveOffspring = function(offspring, outdir, r, g){
  
  #Summarize wild offspring phenotypes before selection

  nOffspring = nrow(offspring)
  
  if(nOffspring > 0){
    writeout <- offspring %>% 
                group_by(pheno) %>% 
                summarise(n = n(), 
                          relPer = n/nOffspring) %>% 
                mutate(gen = g)
                
    
    write.table(
      writeout,
      file = paste(outdir, "offspring_demos_", 
                   formatC(r, width = 4, format = "d", flag = "0"),
                   ".csv", sep = ""),
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(paste(outdir, "offspring_demos_", 
                                     formatC(r, width = 4, format = "d", flag = "0"),
                                     ".csv", sep = "")),
      append = TRUE
    )
    
    # forwriteout = apply(writeout, 1, function(z){paste(z, collapse = ",")})
    # 
    # write(forwriteout, paste(outdir, "offspring_demos_", 
    #                          formatC(r, width = 4, format = "d", flag = "0"), #Give the number proceeding zeros
    #                          ".csv", sep=""), ncolumns=1, append=TRUE)  
  }
  
}
