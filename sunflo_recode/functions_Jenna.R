# Jenna Functions

# summarise timed output variables 
#' @export indicate
indicate <- function(x, integration="crop", Tb=4.8) {
  
  # Définition des périodes d'intégration
  # semis - levee
  SE <- x$PhenoStage == 1
  # levée - fin maturité
  EH <- (x$PhenoStage > 1 & x$PhenoStage < 6)
  # levée - debut maturité
  EM <- (x$PhenoStage > 1 & x$PhenoStage < 5)
  # levée - floraison
  EF <- (x$PhenoStage == 2 | x$PhenoStage == 3)
  # initiation florale - début maturité
  FIM <- (x$PhenoStage == 3 | x$PhenoStage == 4)
  # floraison - début maturité
  FM <- x$PhenoStage == 4
  # floraison - fin maturité
  FH <- (x$PhenoStage == 4 | x$PhenoStage == 5)
  # début maturité - fin maturité
  MH <- x$PhenoStage == 5
  
  
  switch(integration,
    
    crop = {
      # Calcul des indicateurs
      o <- data.frame(

        # Phenologie
        D_SE = sum(SE),
        D_EF = sum(EF),
        D_FM = sum(FM),
        D_MH = sum(MH),
        TT = sum((x$TM[EH] - Tb)[(x$TM[EH] - Tb) > 0]),
        
        # Ressources environnementales
        SGR = sum(x$GR[EH] * 0.48), # PAR
        SRR = sum(x$RR[EH]),
        SPET = sum(x$PET[EH]),
        SCWD = sum(x$RR[EH] - x$PET[EH]),
        
        # Contraintes hydriques
        ## basés sur FTSW
        SFTSW = sum(1 - x$FTSW[EH]),
        NET = sum(x$ETPET[EH] < 0.6),
        SFHTR = sum(1 - x$FHTR[EH]),
        SFHRUE = sum(1 - x$FHRUE[EH]), 
        
        # Contraintes azotées
        # NNIF = x[x$PhasePhenoPlante==4,"NNI"][1], # INN floraison
        SNNI = sum(1 - x$NNI[EH & x$NNI <1]), #  déficit d'azote 
        SNAB = last(x$NAB[EH]),  # quantité totale d'azote absorbé 
        SFNRUE = sum(1 - x$FNRUE[EH]),
        
        # Contraintes thermiques
        SFTRUE = sum(1 - x$FTRUE[EH]),
        NHT = sum(x$TM[EH] > 28),
        NLT = sum(x$TM[EH] < 20),
        SHT = sum(1 - curve_thermal_rue(x$TM[EH], type="high")),
        SLT = sum(1 - curve_thermal_rue(x$TM[EH], type="low")),
        
        # Évolution de la surface foliaire
        LAI = max(x$LAI[EH]),
        LAD = sum(x$LAI[EH]), 
        
        # Rayonnement intercepté (PAR)
        SIR = sum(x$RIE[EH] * x$GR[EH] * 0.48),
        
        # Photosynthèse
        MRUE = mean(x$RUE[EH]),
        
        # Biomasse accumulée
        STDM = max(x$TDM[EH]),
        
        # Performances
        GY = max(x$GY),
        OC = max(x$OC)
      ) 
    },
         
    phase = {       
       # Calcul des indicateurs
       o <- data.frame(
         
         # Phenologie
         D_SE = sum(SE),
         D_EF = sum(EF),
         D_FM = sum(FM),
         D_MH = sum(MH),
         
         # Ressources environnementales
         # Somme de rayonnement
         SGR = sum(x$GR[EH]),
         SGR_EF = sum(x$GR[EF]),
         SGR_FM = sum(x$GR[FM]),
         SGR_MH = sum(x$GR[MH]),
         
         # Cumul de précipitations
         SRR = sum(x$RR[EH]),
         SRR_EF = sum(x$RR[EF]),
         SRR_FM = sum(x$RR[FM]),
         SRR_MH = sum(x$RR[MH]),
         
         # Cumul d'évapotranspiration potentielle
         SPET = sum(x$PET[EH]),
         SPET_EF = sum(x$PET[EF]),
         SPET_FM = sum(x$PET[FM]),
         SPET_MH = sum(x$PET[MH]),
         
         # Déficit hydrique climatique : sum(P-PET)
         SCWD = sum((x$RR-x$PET)[EH]),
         SCWD_EF = sum((x$RR-x$PET)[EF]),
         SCWD_FM = sum((x$RR-x$PET)[FM]),
         SCWD_MH = sum((x$RR-x$PET)[MH]),
         
         # Déficit hydrique édaphique : mean(ET/PET)
         MET = mean(x$ETPET[EH]),
         MET_EF = mean(x$ETPET[EF]),
         MET_FM = mean(x$ETPET[FM]),
         MET_MH = mean(x$ETPET[MH]),
         
         # Déficit hydrique édaphique qualitatif : sum(ET/PET < 0.6)
         NET = sum(x$ETPET[EH] < 0.6),
         NET_EF = sum(x$ETPET[EF] < 0.6), 
         NET_FM = sum(x$ETPET[FM] < 0.6),
         NET_MH = sum(x$ETPET[MH] < 0.6),
         
         # Déficit hydrique édaphique quantitatif : sum(1-FTSW)
         SFTSW = sum(1 - x$FTSW[EH]),
         SFTSW_EF = sum(1 - x$FTSW[EF]), 
         SFTSW_FM = sum(1 - x$FTSW[FM]),
         SFTSW_MH = sum(1 - x$FTSW[MH]),
         
         # Effet de la contrainte hydrique sur la photosynthèse : sum(1-FHRUE)
         SFHRUE = sum(1 - x$FHRUE[EH]),
         SFHRUE_EF = sum(1 - x$FHRUE[EF]), 
         SFHRUE_FM = sum(1 - x$FHRUE[FM]),
         SFHRUE_MH = sum(1 - x$FHRUE[MH]),
         
         # Effet de la contrainte hydrique sur la transpiration : sum(1-FHTR)
         SFHTR = sum(1 - x$FHTR[EH]),
         SFHTR_EF = sum(1 - x$FHTR[EF]), 
         SFHTR_FM = sum(1 - x$FHTR[FM]),
         SFHTR_MH = sum(1 - x$FHTR[MH]),
         
         # Somme de température 
         TT = sum((x$TM[EH] - Tb)[(x$TM[EH] - Tb) > 0]),
         TT_SE = sum((x$TM[SE] - Tb)[(x$TM[SE] - Tb) > 0]),
         TT_EF = sum((x$TM[EF] - Tb)[(x$TM[EF] - Tb) > 0]),
         TT_FM = sum((x$TM[FM] - Tb)[(x$TM[FM] - Tb) > 0]),
         TT_MH = sum((x$TM[MH] - Tb)[(x$TM[MH] - Tb) > 0]),       
         
         # Contraintes thermiques
         SFTRUE = sum(1 - x$FTRUE[EH]),
         SFTRUE_EF = sum(1 - x$FTRUE[EF]), 
         SFTRUE_FM = sum(1 - x$FTRUE[FM]),
         SFTRUE_MH = sum(1 - x$FTRUE[MH]),
         
         # heat stress
         NHT = sum(x$TM[EH] > 28),
         NHT_EF = sum(x$TM[EF] > 28),
         NHT_FM = sum(x$TM[FM] > 28),
         NHT_MH = sum(x$TM[MH] > 28),
         SHT = sum(1 - curve_thermal_rue(x$TM[EH], type="high")),
         SHT_EF = sum(1 - curve_thermal_rue(x$TM[EF], type="high")),
         SHT_FM = sum(1 - curve_thermal_rue(x$TM[FM], type="high")),
         SHT_MH = sum(1 - curve_thermal_rue(x$TM[MH], type="high")),
         
         # cold stress
         NLT = sum(x$TM[EH] < 20),
         NLT_EF = sum(x$TM[EF] < 20),
         NLT_FM = sum(x$TM[FM] < 20),
         NLT_MH = sum(x$TM[MH] < 20),
         SLT = sum(1 - curve_thermal_rue(x$TM[EH], type="low")),
         SLT_EF = sum(1 - curve_thermal_rue(x$TM[EF], type="low")),
         SLT_FM = sum(1 - curve_thermal_rue(x$TM[FM], type="low")),
         SLT_MH = sum(1 - curve_thermal_rue(x$TM[MH], type="low")),
         
         # nitrogen stress 
         # NNIF = x[x$PhasePhenoPlante==4,"NNI"][1], # INN floraison
         # absorbed nitrogen
         SNAB = max(x$NAB[EH]),
         SNAB_EF = max(x$NAB[EF]),
         SNAB_FM = max(x$NAB[FM]),
         SNAB_EM = max(x$NAB[EM]),
         SNAB_MH = max(x$NAB[MH]),
         
         # nitrogen nutrition index 
         SNNI = sum(1 - x$NNI[EH & x$NNI <1]),
         SNNI_EF = sum(1 - x$NNI[EF & x$NNI <1]),
         SNNI_FM = sum(1 - x$NNI[FM & x$NNI <1]),
         SNNI_MH = sum(1 - x$NNI[MH & x$NNI <1]),
         
         # nitrogen impact on phytosynthesis
         SFNRUE = sum(1 - x$FNRUE[EH]),
         SFNRUE_EF = sum(1 - x$FNRUE[EF]),
         SFNRUE_FM = sum(1 - x$FNRUE[FM]),
         SFNRUE_MH = sum(1 - x$FNRUE[MH]),
      
         # NNI at flowering
         NNI_F = x$NNI[FM][1],
         
         # Nombre de jours INN < 0.8 jusqu'à M0
         NNNID_EM = sum(x$NNI[EM] < 0.8),
         NNNIE_EM = sum((x$NNI[EM] > 1.2) & (x$NNI[EM] < 2)),
         
         # Indice foliaire maximum
         LAI = max(x$LAI),
         
         # Durée de surface foliaire : sum(x$LAI)
         LAD = sum(x$LAI[EH]),
         
         # Rayonnement intercepté (PAR)
         SIR = sum(x$RIE[EH] * x$GR[EH] * 0.48),
         SIR_EF = sum(x$RIE[EF] * x$GR[EF] * 0.48),
         SIR_FM = sum(x$RIE[FM] * x$GR[FM] * 0.48),
         SIR_MH = sum(x$RIE[MH] * x$GR[MH] * 0.48),
         
         # Photosynthèse
         MRUE = mean(x$RUE[EH]),
         
         # Biomasse
         STDM = max(x$TDM[EH]),
         STDM_F = x$TDM[FM][1],
         
         # Performance
         GY = max(x$GY),
         OC = max(x$OC)
       ) 
     }         
  )
  return(o)
}


















# From the parametrization_default file, indicators_metadata sheet
# Defining functions for factors temperature and water

calc_TT  <- function() {}
calc_D_SE <- function() {}
calc_D_EF <- function() {}
calc_D_FM <- function() {}
calc_D_MH <- function() {}
calc_NHT <- function() {}
calc_NLT <- function() {}
calc_SHT <- function() {}
calc_SLT <- function() {}
calc_SFTRUE <- function() {}
calc_TT_SE <- function() {}
calc_TT_EF <- function() {}
calc_TT_FM <- function() {}
calc_TT_MH <- function() {}
calc_SFTRUE_EF <- function() {}
calc_SFTRUE_FM <- function() {}
calc_SFTRUE_MH <- function() {}
calc_NLT_EF <- function() {}
calc_NLT_FM <- function() {}
calc_NLT_MH <- function() {}
calc_NHT_EF <- function() {}
calc_NHT_FM <- function() {}
calc_NHT_MH <- function() {}
calc_SLT_EF <- function() {}
calc_SLT_FM <- function() {}
calc_SLT_MH <- function() {}
calc_SHT_EF <- function() {}
calc_SHT_FM <- function() {}
calc_SHT_MH <- function() {}
calc_SRR <- function() {}
calc_SPET <- function() {}
calc_SCWD <- function() {}
calc_SFTSW <- function() {}
calc_MET <- function() {}
calc_NET <- function() {}
calc_SFHTR <- function() {}
calc_SFHRUE <- function() {}
calc_SRR_EF <- function() {}
calc_SRR_FM <- function() {}
calc_SRR_MH <- function() {}
calc_SPET_EF <- function() {}
calc_SPET_FM <- function() {}
calc_SPET_MH <- function() {}
calc_SCWD_EF <- function() {}
calc_SCWD_FM <- function() {}
calc_SCWD_MH <- function() {}
calc_MET_EF <- function() {}
calc_MET_FM <- function() {}
calc_MET_MH <- function() {}
calc_NET_EF <- function() {}
calc_NET_FM <- function() {}
calc_NET_MH <- function() {}
calc_SFTSW_EF <- function() {}
calc_SFTSW_FM <- function() {}
calc_SFTSW_MH <- function() {}
calc_SFHRUE_EF <- function() {}
calc_SFHRUE_FM <- function() {}
calc_SFHRUE_MH <- function() {}
calc_SFHTR_EF <- function() {}
calc_SFHTR_FM <- function() {}
calc_SFHTR_MH <- function() {}
