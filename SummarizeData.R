  
  rm(list = ls()) 
  library(tidyverse)
  
  ##############################################################  
  # Code to load the data and subset with analysis
  ##############################################################  
  
  #Note: terminology is specific to hatcheries; 
  #... 'spawners' = adults; 'hatcheryBS' = captive broodstock
  
  
  dir = #ADD YOUR DIRECTORY HERE 
  
  files = list.files(dir, '*.csv', recursive = F, full.names = TRUE)
  bnames = basename(files)
  
  #Load run variables; select variables of interest
  load(paste(dir, 'runVars.rda', sep = ""))
    runVarsSub = runvars %>% select(run, pNOB, pHOS, pBroodstock, capSelectAB, capSelectAA, capSelectBB) 
  

  for(i in 1:length(files)){
    
    #... Read in table
    tbl = read.csv(files[i], stringsAsFactors=FALSE, header = T)
    
    #... Assign run to dataframe
    bnPart = sub('.csv', '',bnames[i])
    run = as.numeric(sub('population_indvs_', '',bnPart))
    tbl$run = run
    
    #Filter out only wild spawners and hatchery broodstock 
    tbl <- tbl %>% filter(fate=="wildSpawn" | fate=="hatcheryBS") 
    

    #... Summarize loci of large effect
    # By all spawners (adults) together
    tbl_lociLA_spawners <- tbl %>%
      group_by(run, gen, pheno) %>%
      summarize(byPheno = n()) %>%
      mutate(total = sum(byPheno),
             perPheno = byPheno/total) %>%
      left_join(runVarsSub)

    # By origin
    tbl_lociLA_byOrigin <- tbl %>%
      group_by(run, gen, HvsW, pheno) %>%
      summarize(byPheno = n()) %>%
      mutate(total = sum(byPheno),
             perPheno = byPheno/total) %>%
      left_join(runVarsSub)


    #... Allelic Richness
    temp <- tbl %>%
      select(ID, run, gen, fate, HvsW, L2m, L3m, L4m,
             L5m, L6m, L7m, L8m, L9m, L10m, L11m, L12m,
             L13m, L14m, L15m, L16m, L17m, L18m, L19m, L20m,
             L21m, L22m, L23m, L24m, L25m, L26m, L27m, L28m,
             L29m, L30m, L31m,
             L2d, L3d, L4d, L5d,
             L6d, L7d, L8d, L9d, L10d, L11d, L12d, L13d,
             L14d, L15d, L16d, L17d, L18d, L19d, L20d, L21d,
             L22d, L23d, L24d, L25d, L26d, L27d, L28d, L29d,
             L30d, L31d) %>%
      pivot_longer(!c(ID, run, gen, fate, HvsW), names_to = "loci", values_to = "allele")


    tbl_alleles_pop <- temp %>%
      group_by(run, gen) %>%
      summarise(uniqueAlleles = n_distinct(allele), 
                percentRemain = uniqueAlleles/300) %>%
      left_join(runVarsSub)

    tbl_alleles_pop_origin <- temp %>%
      group_by(run, gen, HvsW) %>%
      summarise(uniqueAlleles = n_distinct(allele), 
                percentRemain = uniqueAlleles/300) %>%
      left_join(runVarsSub)

    #... Total by fate and origin
    tbl_n = tbl %>%
      group_by(run, gen, fate, HvsW) %>%
      summarise(n = n()) %>%
      left_join(runVarsSub)

    tbl_n_byPheno = tbl %>%
      group_by(run, gen, fate, HvsW, pheno) %>%
      summarise(n = n()) %>%
      left_join(runVarsSub)


    #...FITNESS COMPARISONS ####
    
     #... Here are all possible moms (everyone that spawned in the wild that we want to calc RS for)
      possMoms = tbl %>%
        filter(sex==0, fate=="wildSpawn") %>%
        select(gen, ID, HvsW, fate, L1m, L1d) %>%
        mutate(momPheno = L1m + L1d) %>% 
        select(!c(L1m, L1d)) %>% 
        rename(momID = ID, momHvsW = HvsW, momFate = fate, momGen = gen)

    #... Here are all the offspring of poss moms, plus NA for moms that had no offspring
     mom_offsp_linked <- possMoms %>%
        left_join(tbl, by="momID") %>% 
        select(momGen, momID, momHvsW, momFate, momPheno, ID, 
               pheno, HvsW, sex, gen)
     
   #... Now we want to identify the possMoms momIDs and their origin
     # Call these gmaID and gmaHvsW
     momIDs <- unique(mom_offsp_linked$momID) #all moms that spawned in the wild
     
      gmas <- tbl %>% filter(ID %in% momIDs) %>% #Pull all fish that became moms, ID their mom
        select(ID, momID) %>% 
        rename(momID=ID, gmaID=momID) 
      
      gmaIDs <- unique(gmas$gmaID)
      
      #Link momID with grandma ID and origin
      gmaOrigin <- tbl %>% filter(ID %in% gmaIDs) %>% 
        select(ID, HvsW, pheno) %>% 
        rename(gmaID=ID, gmaHvsW=HvsW, gmaPheno=pheno)  %>% 
        right_join(gmas) 
      
     #Link grandma data back to offspring data
      #THIS IS THE FULL DATA SET
      mom_offsp_linked2 <- mom_offsp_linked %>% 
        right_join(gmaOrigin)
      
    # Compare RS of wild spawners that had a wild or hatchery parent
      
      #ID moms with zero offspring and create df to combine with moms that had offspring
      zeroOffMoms <- mom_offsp_linked2 %>%
        filter(is.na(ID)) %>%
        group_by(momGen, momID, momHvsW, momFate, momPheno, gmaHvsW, gmaPheno) %>%
        summarise(n = n()) %>% 
        mutate(n = 0)

    
      #Number of offspring per mom
      momOffspN <- mom_offsp_linked2 %>%
        filter(!is.na(ID)) %>%
        group_by(momGen, momID, momHvsW, momFate, momPheno, gmaHvsW, gmaPheno) %>%
        summarise(n = n()) 

      momOffspN <- rbind(zeroOffMoms, momOffspN)

      #... Calc the average # offspring by wild and hatchery origin moms that spawned in the wild
      tbl_RRS_Gen1 <-  momOffspN %>%
        #mutate(momHvsW = if_else(momHvsW==1, "wild", "hatchery")) %>% 
        group_by(momGen, momHvsW) %>%
        summarise(meanN = mean(n))
        #pivot_wider(names_from = momHvsW, values_from = meanN) 
        #rename("hatchery" = `0`, "wild"=`1`) %>%
        # group_by(motherGen) %>%
        # mutate(RRS = hatchery/wild)  #NOTE: Not calc'ing RRS here because pops without hatcheries fail; call RRS in post processing

      tbl_RRS_Gen1$run <- tbl$run[1] #Assign run to the F1 table
      tbl_RRS_Gen1 <- tbl_RRS_Gen1 %>% left_join(runVarsSub)

      ##... RRS differences by grandparent origin
      tbl_RRS_Gen2 <-  momOffspN %>%
        filter(!is.na(gmaHvsW), momHvsW==1) %>% 
        #mutate(gmaHvsW = if_else(gmaHvsW==1, "wild", "hatchery")) %>% 
        group_by(momGen, gmaHvsW) %>%
        summarise(meanN = mean(n)) 
        #pivot_wider(names_from = gmaHvsW, values_from = meanN) 
      
      tbl_RRS_Gen2$run <- tbl$run[1] #Assign run to the F2 table
      tbl_RRS_Gen2 <- tbl_RRS_Gen2 %>% left_join(runVarsSub) #Add in the relevant metadata
      
      ####...%BB production comparisons by origin  #####

      #Number of offspring per mom
      ## NOTE: mom_offsp_linked2 is from code above
      tbl_momOffsp_byPheno <- mom_offsp_linked2 %>%
        filter(!is.na(ID)) %>%
        group_by(momGen, momID, momHvsW, momPheno, pheno) %>%
        summarise(byPheno = n()) %>%
        mutate(total = sum(byPheno),
               perPheno = byPheno/total) %>% 
        filter(pheno==10000) %>% 
        group_by(momGen, momHvsW) %>% 
        summarise(meanPerBB = mean(perPheno))
      
      tbl_momOffsp_byPheno$run <- tbl$run[1] #Assign run to the F1 table
      tbl_momOffsp_byPheno <- tbl_momOffsp_byPheno %>% left_join(runVarsSub)
      
      tbl_gmaOffsp_byPheno <- mom_offsp_linked2 %>%
        filter(!is.na(ID), momHvsW==1) %>%
        group_by(momGen, momID, gmaHvsW, momPheno, pheno) %>%
        summarise(byPheno = n()) %>%
        mutate(total = sum(byPheno),
               perPheno = byPheno/total) %>% 
        filter(pheno==10000) %>% 
        group_by(momGen, gmaHvsW) %>% 
        summarise(meanPerBB = mean(perPheno))
      
        tbl_gmaOffsp_byPheno$run <- tbl$run[1] #Assign run to the table
        tbl_gmaOffsp_byPheno <- tbl_gmaOffsp_byPheno %>% left_join(runVarsSub) #Add in the relevant metadata
      

    # } else {
    #   tbl_RRS_F1 = NULL
    #   tbl_RRS_F2 = NULL
    #   tbl_momOffsp_byPheno = NULL
    #   tbl_gmaOffsp_byPheno = NULL
    # }

    
    if(i == 1){
        dat_alleles_pop = tbl_alleles_pop
        dat_alleles_pop_origin = tbl_alleles_pop_origin
        
        dat_n = tbl_n
          dat_n_byPheno = tbl_n_byPheno

        dat_lociLA_spawners = tbl_lociLA_spawners
        dat_lociLA_byOrigin = tbl_lociLA_byOrigin
        
        dat_RRS_Gen1 = tbl_RRS_Gen1
        dat_RRS_Gen2 = tbl_RRS_Gen2 
        
        dat_momOffsp_byPheno = tbl_momOffsp_byPheno
        dat_gmaOffsp_byPheno = tbl_gmaOffsp_byPheno

      
      
    } else{
          dat_alleles_pop = rbind(dat_alleles_pop, tbl_alleles_pop)
            dat_alleles_pop_origin = rbind(dat_alleles_pop_origin, tbl_alleles_pop_origin)
        
        dat_n = rbind(dat_n, tbl_n)
          dat_n_byPheno = rbind(dat_n_byPheno, tbl_n_byPheno)

          dat_lociLA_byOrigin = rbind(dat_lociLA_byOrigin, tbl_lociLA_byOrigin)
          dat_lociLA_spawners = rbind(dat_lociLA_spawners, tbl_lociLA_spawners)
          
          dat_RRS_Gen1 = rbind(dat_RRS_Gen1, tbl_RRS_Gen1)
          dat_RRS_Gen2 = rbind(dat_RRS_Gen2, tbl_RRS_Gen2)
        
        dat_momOffsp_byPheno = rbind(dat_momOffsp_byPheno, tbl_momOffsp_byPheno)
        dat_gmaOffsp_byPheno = rbind(dat_gmaOffsp_byPheno, tbl_gmaOffsp_byPheno)
        

    }
  }
    
      save(dat_alleles_pop_origin, file=paste(dir, "dat_alleles_pop_origin.rda", sep = ""))
      save(dat_alleles_pop, file=paste(dir, "dat_alleles_pop.rda", sep = ""))
  
            save(dat_n, file=paste(dir, "dat_n.rda", sep = ""))
        save(dat_n_byPheno, file=paste(dir, "dat_n_byPheno.rda", sep = ""))

      save(dat_lociLA_byOrigin, file=paste(dir, "dat_lociLA_byOrigin.rda", sep = ""))
      save(dat_lociLA_spawners, file=paste(dir, "dat_lociLA_spawners.rda", sep = ""))
      
      save(dat_RRS_Gen1, file=paste(dir, "dat_RRS_Gen1.rda", sep = ""))
      save(dat_RRS_Gen2, file=paste(dir, "dat_RRS_Gen2.rda", sep = ""))
      
      save(dat_momOffsp_byPheno, file=paste(dir, "dat_momOffsp_byPheno.rda", sep = ""))
      save(dat_gmaOffsp_byPheno, file=paste(dir, "dat_gmaOffsp_byPheno.rda", sep = ""))
      
      
  