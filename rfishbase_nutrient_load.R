library(rfishbase)
library(tidyverse)

tax<-load_taxa()
nut<-estimate(tax$Species) %>% 
    select(Species, SpecCode, Calcium:Zinc_u95) %>% 
    filter(!is.na(Calcium)) %>% 
    # janitor::clean_names() %>% 
    rename('Calcium_mu' = Calcium,
           'Iron_mu' = Iron,
           'Selenium_mu' = Selenium,
           'Zinc_mu' = Zinc,
           'Omega_3_mu' = Omega3,
           'Omega_3_u95' = Omega3_u95,
           'Omega_3_l95' = Omega3_l95,
           'Vitamin_a_mu' = VitaminA,
           'Vitamin_a_u95' = VitaminA_u95,
           'Vitamin_a_l95' = VitaminA_l95,
           'Protein_mu' = Protein,
           'species' = Species)

nut_old<-read.csv('Species_Nutrient_Predictions_muscle_wet.csv') 

write.csv(nut, file='Species_Nutrient_Predictions_muscle_wet_FB.csv')
