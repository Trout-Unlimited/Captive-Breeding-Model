 
  # ---- preface ---- 
  rm(list = ls()) 
    library(tidyverse)
    library(ggh4x) #from package "ggh4x"; makes plots consistent size
  
 
  # ---- add directory ---- 
  dir = #ADD YOUR DIRECTORY HERE 

  # ---- theme mine (plot format used in manuscript) ----
  theme_mine <- function(base_size = 18, base_family = "TT Arial") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 18, angle = 270),
        axis.text.x = element_text(size=14, angle =0,hjust=0.95,vjust=0.2),
        axis.text.y = element_text(size=14,hjust=1),
        axis.ticks =  element_line(colour = "black"), 
        axis.title.x= element_text(size=16),
        axis.title.y= element_text(size=16,angle=90, margin = margin(r=5)),
        panel.background = element_blank(), 
        #panel.border =element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(1.0, "lines"), 
        plot.background = element_blank(), 
        plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5), 
        #legend.title=element_blank()
      )
  }
  
  #---- Function to make categorize data by selection strength
  categorize <- function(data) {
    data %>%
      mutate(
          category = if_else(
          capSelectAB == 0.3, "med",
          if_else(capSelectAB == 0, "high",
                  if_else(capSelectAB == 1, "none", "error")
          )
        )
      ) %>%
      select(-c(capSelectAB, capSelectAA, capSelectBB))
  }
  
### PLOTS: 
  
# ---- Fitness Variation Across Scenarios ---- 
      load(paste(dir, 'dat_RRS_Gen1_Full.rda', sep = ""))
        dat_RRS_Gen1_Full <- categorize(dat_RRS_Gen1_Full)
      
# ---- Gen 1 RRS plot ----   
      # RRS Data
      dat_RRS_Gen1_RRScalc <- dat_RRS_Gen1_Full %>% 
        #filter(pBroodstock==200) %>% 
        mutate(momHvsW = if_else(momHvsW==1, "wild", "hatchery")) %>% 
        pivot_wider(names_from=momHvsW, values_from = meanN) %>% 
        mutate(RRS = hatchery/wild) %>% 
        filter(momGen>26, momGen<75) %>% 
        group_by(run, pNOB, pHOS, category, pBroodstock) %>% 
        summarise(meanRRS = mean(RRS))
      
       # Compute mean per group
      dat_means <- dat_RRS_Gen1_RRScalc %>%
        filter(pBroodstock == 200) %>%
        group_by(pNOB, pHOS, category) %>%
        summarise(meanRRS_group = mean(meanRRS, na.rm = TRUE), .groups = "drop")
      
      # Plot points (jittered) + mean lines
      dat_RRS_Gen1_RRScalc %>%
        filter(pBroodstock == 200) %>%
        ggplot(aes(x = pNOB, y = meanRRS, color = category)) +
        geom_point(position = position_jitter(width = 0.05, height = 0), alpha = 0.2, size=2.5) +
        # add mean lines on top
        geom_line(data = dat_means,
                  aes(x = pNOB, y = meanRRS_group, group = category),
                  size = 1) +
        geom_point(data = dat_means,
                  aes(x = pNOB, y = meanRRS_group, group = category),
                  size = 2.5) +
        facet_wrap(vars(pHOS), nrow = 1, labeller = label_both) +
        scale_color_manual(values=c("#c31e23", "#293a5b", "#67a9cf")) +
        ylab("Mean Relative Reproductive Success") +
        labs(color = "Selection\n strength") + 
        theme_mine() +
        force_panelsizes(rows = unit(4, "cm"), cols = unit(5, "cm"))

# ---- Gen 2 RRS plot ----     
      
      load(paste(dir, 'dat_RRS_Gen2_Full.rda', sep = ""))
        dat_RRS_Gen1_Full <- categorize(dat_RRS_Gen2_Full)

      # Calc RRS for the kids
      kidRRS <- dat_RRS_Gen2_Full %>% 
        filter(pBroodstock==200, category=="med") %>% 
        mutate(gmaHvsW = if_else(gmaHvsW==1, "wild", "hatchery")) %>% 
        pivot_wider(names_from=gmaHvsW, values_from = meanN) %>% 
        mutate(RRS = hatchery/wild) %>% 
        filter(momGen>26, momGen<75) %>% 
        group_by(run, pNOB, pHOS, category, pBroodstock) %>% 
        summarise(meanRRS = mean(RRS)) 
      
      # Compute mean per group
      kid_means <- kidRRS %>%
        group_by(pNOB, pHOS, category) %>%
        summarise(meanRRS_group = mean(meanRRS, na.rm = TRUE), .groups = "drop")
      
      
      # Plot points (jittered) + mean lines
        ggplot() +
        geom_point(data=dat_RRS_Gen1_RRScalc %>% filter(pBroodstock==200, category=="med"), 
                   aes(x = pNOB, y = meanRRS),
                   position = position_jitter(width = 0.05, height = 0), alpha = 0.3, size=2.5, color="#293a5b") +
        geom_point(data = dat_means %>% filter(category=="med"),
                   aes(x = pNOB, y = meanRRS_group),
                   size = 2, color="#293a5b") +
          geom_line(data = dat_means %>% filter(category=="med"),
                    aes(x = pNOB, y = meanRRS_group),
                    size = 1.2, color="#293a5b") +
          geom_point(data=kidRRS, 
                     aes(x = pNOB, y = meanRRS),
                     position = position_jitter(width = 0.05, height = 0), alpha = 0.3, size=2.5, color="#1c9099") +
          geom_point(data = kid_means,
                     aes(x = pNOB, y = meanRRS_group),
                     size = 2, color="#1c9099") +
          geom_line(data = kid_means,
                    aes(x = pNOB, y = meanRRS_group),
                    size = 1.2, color="#1c9099") +
          facet_wrap(vars(pHOS), nrow = 1, labeller = label_both) +
          ylab("Mean Relative Reproductive Success") +
          ylim(0, 1.15) + 
          geom_hline(yintercept=1, linewidth=1, color="darkgrey")+
          theme_mine() +
         force_panelsizes(rows = unit(4, "cm"), cols = unit(5, "cm"))



 #---- Demographic plots  ----
      
      load(paste(dir, 'dat_n_Full.rda', sep = ""))
        dat_n_Full <- categorize(dat_n_Full)  
      
      dat_n_Full_summed <- dat_n_Full %>% 
        filter(gen>27, gen<75, pBroodstock==200) %>% 
        group_by(run, gen, HvsW, pNOB, pHOS, pBroodstock, category) %>% 
        summarise(n_tot = sum(n)) %>% 
        group_by(run, HvsW, pNOB, pHOS, pBroodstock, category) %>% 
        summarise(meanN = mean(n_tot))
      
      n_mean_Summed <- dat_n_Full_summed %>% 
        filter(pBroodstock==200) %>% 
        group_by(HvsW, pNOB, pHOS, pBroodstock, category) %>% 
        summarise(meanN = mean(meanN))
      
      #Hatchery plot
      ggplot() +
        geom_point(data=dat_n_Full_summed %>% filter(HvsW==0), aes(x = pNOB, y = meanN, color=category),
                   position = position_jitter(width = 0.05, height = 100), alpha = 0.2, size=2.5) +
        geom_point(data=n_mean_Summed %>% filter(HvsW==0), aes(x = pNOB, y = meanN, color=category), size=2.5) + 
        geom_line(data=n_mean_Summed %>% filter(HvsW==0), aes(x = pNOB, y = meanN, color=category), linewidth=1) + 
        facet_wrap(vars(pHOS), nrow=1, labeller = label_both) +
        scale_color_manual(values=c("#c31e23", "#293a5b", "#67a9cf")) +
        ylab("Mean number of spawners") +
        geom_hline(yintercept = 1454, color="darkgrey") +
        theme_mine() +
        force_panelsizes(rows = unit(4, "cm"), cols = unit(5, "cm"))

      
      #Wild plot
      ggplot() +
        geom_point(data=dat_n_Full_summed %>% filter(HvsW==1), aes(x = pNOB, y = meanN, color=category),
                   position = position_jitter(width = 0.05, height = 5), alpha = 0.2, size=2.5) +
        geom_point(data=n_mean_Summed %>% filter(HvsW==1), aes(x = pNOB, y = meanN, color=category), size=2.5) + 
        geom_line(data=n_mean_Summed %>% filter(HvsW==1), aes(x = pNOB, y = meanN, color=category), size=1) + 
        facet_wrap(vars(pHOS), nrow=1, labeller = label_both) +
        scale_color_manual(values=c("#c31e23", "#293a5b", "#67a9cf")) + 
        ylab("Mean number of spawners") +
        geom_hline(yintercept = 1454, color="darkgrey") +
        theme_mine() + 
        force_panelsizes(rows = unit(4, "cm"), cols = unit(5, "cm"))


      
# ---- Adaptive locus plots  ----
         load(paste(dir, 'dat_lociLA_byOrigin_Full.rda', sep = ""))
          dat_lociLA_byOrigin_Full <- categorize(dat_lociLA_byOrigin_Full) 
          
        load(paste(dir, 'dat_lociLA_spawners_Full.rda', sep = ""))
          dat_lociLA_spawners_Full <- categorize(dat_lociLA_spawners_Full)
        
        
        #By origin
        meanLLA_origin_byRun <- dat_lociLA_byOrigin_Full %>% 
          filter(pheno==10000, pBroodstock==200, gen>26, gen<75) %>%
          group_by(run, pNOB, pHOS, category, HvsW) %>% 
          summarise(meanBB = mean(perPheno)) %>% 
          mutate(HvsW = if_else(HvsW==1, "Natural origin", "Hatchery origin"))
        
        meanLLA_origin <- dat_lociLA_byOrigin_Full %>% 
          filter(pheno==10000, pBroodstock==200, gen>26, gen<75) %>%
          group_by(pNOB, pHOS, category, HvsW) %>% 
          summarise(meanBB = mean(perPheno)) %>% 
          mutate(HvsW = if_else(HvsW==1, "Natural origin", "Hatchery origin"))
        
        #By Total
        meanLLA_all_byRun <- dat_lociLA_spawners_Full %>% 
          filter(pheno==10000, pBroodstock==200, gen>26, gen<75) %>%
          group_by(run, pNOB, pHOS, category) %>% 
          summarise(meanBB = mean(perPheno)) %>% 
          mutate(HvsW = "Total")
        
        meanLLA_all <- dat_lociLA_spawners_Full %>% 
          filter(pheno==10000, pBroodstock==200, gen>26, gen<75) %>%
          group_by(pNOB, pHOS, category) %>% 
          summarise(meanBB = mean(perPheno)) %>% 
          mutate(HvsW = "Total")
          
        
         ggplot() +
          geom_point(data=meanLLA_origin_byRun, aes(pNOB, meanBB, color=category), 
                     position = position_jitter(width = 0.03, height = 0.03), alpha = 0.2, size=2.5) +
            geom_point(data=meanLLA_all_byRun, aes(pNOB, meanBB, color=category), 
                       position = position_jitter(width = 0.03, height = 0.03), alpha = 0.2, size=2.5) +
            geom_point(data=meanLLA_origin, aes(pNOB, meanBB, color=category), size=2.5) +
            geom_point(data=meanLLA_all, aes(pNOB, meanBB, color=category), size=2.5) +
          geom_line(data=meanLLA_origin, aes(pNOB, meanBB, color=category), linewidth=1) +
          geom_line(data=meanLLA_all, aes(pNOB, meanBB, color=category), linewidth=1) +
          scale_color_manual(values=c("#c31e23", "#293a5b", "#67a9cf")) +
          facet_grid(rows=vars(pHOS), cols=vars(HvsW), labeller=label_both) +
            ylab("Proportion of high (BB) genotype") +
            labs(color = "Selection\n strength") + 
          theme_mine() + 
            force_panelsizes(rows = unit(4, "cm"), cols = unit(5, "cm"))

        
#---- Neutral allelic richness plots  ----
      
    load(paste(dir, 'dat_alleles_pop_Full.rda', sep = ""))
      dat_alleles_pop_Full <- categorize(dat_alleles_pop_Full) 
 
    meanAR <- dat_alleles_pop_Full %>% 
              filter(gen==75) %>% 
              group_by(pNOB, pHOS, pBroodstock, category) %>% 
              summarise(meanPer = mean(percentRemain))
          
  ## all the factors in pop alleles
       ggplot() + 
          geom_hline(yintercept = 1, color="darkgrey", linewidth=1) +
        geom_point(data=dat_alleles_pop_Full %>% filter(gen==75), 
                   aes(pBroodstock, percentRemain, color=category), 
                   position = position_jitter(width = 10, height = 0.0), alpha = 0.2, size=2.5) +
        geom_point(data=meanAR, aes(pBroodstock, meanPer, color=category), size=2.5) +
        geom_line(data=meanAR, aes(pBroodstock, meanPer, color=category), size=1) +
          scale_color_manual(values=c("#c31e23", "#293a5b", "#67a9cf")) +
        ylab("Percent of original alleles\n after 50-year hatchery") + xlab("Number of Broodstock") +
        facet_grid(cols=vars(pNOB), rows=vars(pHOS), labeller = label_both) +
          labs(color = "Selection\n strength") + 
        theme_mine() + 
         force_panelsizes(rows = unit(4, "cm"), cols = unit(5, "cm"))

        
  
 