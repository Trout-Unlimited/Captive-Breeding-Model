CaptiveRepro = function(spawners, g, nloci, totalinds, coffprop, 
                        pHOS, capSelectAB, capSelectAA, capSelectBB, fecundity){

    # ----------------------------
    # 1. Subset repros
    # ----------------------------
    repros <- spawners[spawners$SpawnLocation == "hatchery", ]
    repros$SpawnLocation <- NULL
    
    if (nrow(repros) == 0) return(NULL)
    
    n_fem <- sum(repros$sex == 0)
    n_mal <- sum(repros$sex == 1)
    if (n_fem == 0 || n_mal == 0) return(NULL)
    
    # ----------------------------
    # 2. Pairing
    # ----------------------------
    size <- min(n_fem, n_mal)
    
    males <- sample(which(repros$sex == 1), size)
    females <- sample(which(repros$sex == 0), size)
    
    pairs <- data.frame(
      m = males,
      f = females
    )
    
    totalCapOff <- fecundity * nrow(pairs)
    perCapOff <- round(totalCapOff / nrow(pairs))
    pairs$nOffsp <- perCapOff
    
    # ----------------------------
    # 3. storage
    # ----------------------------
    offspring_list <- vector("list", nrow(pairs))
    
    # ----------------------------
    # 4. loop over pairs (ONLY outer loop)
    # ----------------------------
    for (p in seq_len(nrow(pairs))) {
      
      f <- pairs$f[p]
      m <- pairs$m[p]
      noff <- pairs$nOffsp[p]
      
      if (noff <= 0) {
        offspring_list[[p]] <- NULL
        next
      }
      
      # ----------------------------
      # 5. metadata (vectorized)
      # ----------------------------
      offspring <- data.frame(
        ID = integer(noff),
        age = integer(noff),
        momID = rep(repros[f, 1], noff),
        dadID = rep(repros[m, 1], noff),
        pheno = NA_integer_,
        HvsW = integer(noff),
        sex = sample(0:1, noff, replace = TRUE),
        gen = rep(g, noff),
        alive = rep(1L, noff),
        gendead = integer(noff),
        yearSpawned = rep(0, noff),
        fate = rep("0", noff),
        stringsAsFactors = FALSE
      )
      
      # ----------------------------
      # 6. parental genotype matrices
      # ----------------------------
      fg <- as.matrix(repros[f, -(1:12)]) #female mothers
      mg <- as.matrix(repros[m, -(1:12)]) #male fathers
      
      # ----------------------------
      # 7. vectorized inheritance (NO offspring loop)
      # ----------------------------
      # each entry chooses maternal/paternal allele
      mom_pick <- matrix(
        sample(c(1, nloci + 1), noff * nloci, replace = TRUE),
        nrow = noff
      )
      
      dad_pick <- matrix(
        sample(c(1, nloci + 1), noff * nloci, replace = TRUE),
        nrow = noff
      )
      
      offspringG <- matrix(0L, nrow = noff, ncol = 2 * nloci)
      
      for (i in seq_len(nloci)) {
        offspringG[, i] <- fg[1, mom_pick[, i] + (i - 1)]
        offspringG[, i + nloci] <- mg[1, dad_pick[, i] + (i - 1)]
      }
      
      # ----------------------------
      # 8. combine
      # ----------------------------
      offspring_list[[p]] <- cbind(offspring, offspringG)
    }
    
    # ----------------------------
    # 9. assemble offspring
    # ----------------------------
    ALLOFFSPRING <- do.call(rbind, Filter(Negate(is.null), offspring_list))
    
    if (is.null(ALLOFFSPRING) || nrow(ALLOFFSPRING) == 0) return(NULL)
    
    # ----------------------------
    # 10. phenotype (vectorized)
    # ----------------------------
    ALLOFFSPRING$pheno <- ALLOFFSPRING[[13]] + ALLOFFSPRING[[13+nloci]]
    
    # ----------------------------
    # 11. phenotype table + survival
    # ----------------------------
    tab <- table(ALLOFFSPRING$pheno)
    pheno_vals <- as.numeric(names(tab))
    n <- as.integer(tab)
    
    survivors <- ifelse(pheno_vals == 100, n * capSelectAA,
                        ifelse(pheno_vals == 5050, n * capSelectAB,
                               ifelse(pheno_vals == 10000, n * capSelectBB, 0)))
    
    keep <- survivors > 0
    if (!any(keep)) return(NULL)
    
    pheno_vals <- pheno_vals[keep]
    survivors <- as.integer(survivors[keep])
    
    newTot <- sum(survivors)
    overallPer <- survivors / newTot
    nContrib <- as.integer(coffprop * overallPer)
    
    # ----------------------------
    # 12. sample by phenotype
    # ----------------------------
    # safely extract counts
    nMed  <- nContrib[match(100, pheno_vals)]
    nLow  <- nContrib[match(5050, pheno_vals)]
    nHigh <- nContrib[match(10000, pheno_vals)]
    
    # replace NA with 0
    nMed[is.na(nMed)] <- 0
    nLow[is.na(nLow)] <- 0
    nHigh[is.na(nHigh)] <- 0
    
    # indices
    idx_100   <- which(ALLOFFSPRING$pheno == 100)
    idx_5050  <- which(ALLOFFSPRING$pheno == 5050)
    idx_10000 <- which(ALLOFFSPRING$pheno == 10000)
    
    # sampling (guard BOTH existence + size)
    offspringMed <- if (nMed > 0 && length(idx_100) > 0) {
      ALLOFFSPRING[sample(idx_100, nMed, replace = TRUE), ]
    } else {
      NULL
    }
    
    offspringLow <- if (nLow > 0 && length(idx_5050) > 0) {
      ALLOFFSPRING[sample(idx_5050, nLow, replace = TRUE), ]
    } else {
      NULL
    }
    
    offspringHigh <- if (nHigh > 0 && length(idx_10000) > 0) {
      ALLOFFSPRING[sample(idx_10000, nHigh, replace = TRUE), ]
    } else {
      NULL
    }
    
    ALLOFFSPRING <- do.call(rbind, 
                            Filter(Negate(is.null), 
                                   list(offspringLow, offspringMed, offspringHigh)))
    
    if (is.null(ALLOFFSPRING) || nrow(ALLOFFSPRING) == 0) return(NULL)
    
    
    # ----------------------------
    # 13. final IDs
    # ----------------------------
    ALLOFFSPRING$ID <- seq(from = totalinds, length.out = nrow(ALLOFFSPRING))
    
    colnames(ALLOFFSPRING) <- c(
      "ID","age","momID","dadID","pheno","HvsW","sex","gen",
      "alive","gendead","yearSpawned","fate",
      paste0("L", 1:nloci, "m"), paste0("L", 1:nloci, "d")
    )
    
    return(ALLOFFSPRING)
  }

