# Captive Breeding Model

Code for modeling effects of releasing captive-born individuals into wild populations associated with the publication
Ohms HA, Long KM, Caudill CC, Marston Jr. GW, Neville H. Multi-niche selection resolves a paradox in captive breeding models. 

In review, DOI to come



The model code structure was based on Willoughby and Christie 2019 (DOI: 10.1111/cobi.13217), with their framework serving as a scaffold for our implementation. 



Use of the provided code requires a specific configuration of folders. 

The parent folder should contain: 

1. An 'output' folder
2. A 'source' folder that contains the following .R files: AgeUp.R, AllDead.R, CaptiveRepro.R, DensityDep.R, FunctionSourcer.R, IdentifySpawners.R, Replicates.R, Repro.R, RunModel.R, SavePopLeftovers.R, StartingPop.R, WriteOut.R
3. The following .R files: Model\_Inputs.R, SummarizeData.R, Plots.R



Model simulations begin by entering desired inputs into the Model\_Inputs.R file and starting the simulation on line 103. 

From there, the RunModel function will call functions and files from the source code folder. 

All output files will be directed into the 'output' directory. 



Once the simulations are complete, the SummarizeData.R file can be used to summarize the model outputs into parameters used in the publication (i.e., number of breeders by origin and year or allele frequencies at the locus of large effect by year). You must enter a directory location for your output files (line 13). 



The Plots.R file can be used to recreate the plots in the publication. You must enter a directory location for your output files (line 9).



Questions or issues can be directed to Haley Ohms at haley.ohms@tu.org

