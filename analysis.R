

  library(tidyverse)
  library(ggfortify)
  library(randomForest)
  
  # Q1: load and assemble the data from the two files. Which spp has the most records and the least records
  
  dat <- read_csv("HEMorphSpp.csv") %>%
    dplyr::left_join(read_csv("HEMorph.csv")) %>% # the spp are in a different file
    dplyr::mutate(Spp = fct_reorder(Spp,weight)) %>% # this turns Spp into a factor ordered by the weight
    tidyr::gather(variable,value,3:ncol(.)) %>% # create a tidy data set
    dplyr::add_count(Spp)
  
  # A1: NHH have the most records (1620) and SIH have the least (10)
  
  
  # Q2: Qualitatively, what morphological attributes are correlated with weight
    
  ggplot(dat, aes(Spp,value)) +
    geom_boxplot() +
    facet_wrap(~variable, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  # A2: claw, hallux, tail, tarsus, total are kind of proportional to weight whereas beak, gape, skull and wing less so
  
  
  # Q3: Which species are likely to have similar ecological niches, given these data?
  
  datWide <- dat %>% tidyr::spread(variable,value)
  
  # a)
  
  modPCA <- princomp(scale(datWide %>% dplyr::select(4:ncol(.))))
  
  autoplot(modPCA, data = datWide, colour = "Spp")
  
  # b)
  
  modRF <- randomForest(Spp ~ .
                        , data = datWide[-c(2:3)]
                        )
  
  
  # A3a: WNH & BHH plot on top of each other
  # A3b: WNH & BHH: 4 BHH were predicted to be WNH and 3 WNH were predicted to be BHH. Largest other misclassification was 2 CRH predicted to be NHH
  