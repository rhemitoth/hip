library(corrplot)
source("/Users/rhemitoth/Documents/Lion_Movement/R/hip_lion_issa/scripts/issa_covs.R")

#Making table of all lions----

Fluffy_for_all_lions <- Fluffy_issa %>%
  mutate(distEdge_end = NA,
         distEdge_start = NA)

all_lions <- rbind(Babe_issa,
                   Fanbelt_issa,
                   Fluffy_for_all_lions,
                   iHlane_issa,
                   Koku_issa,
                   Madonna_issa,
                   Mellon_issa,
                   Murph_issa,
                   Ntombi_issa,
                   PokeJr_issa,
                   Roxy_issa,
                   Stud_issa,
                   Thembi_issa,
                   Zulu_issa)%>%
  select(c('Slope_end','Veg1m_end','distEdge_end','PreyAbundance_end','PreyCatchability_end'))%>%
  na.omit()

cor_matrix <- cor(all_lions)
corrplot(cor_matrix,
         type = "upper")
