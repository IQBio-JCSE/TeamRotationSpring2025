/**
 *
 * Copyright (C) 2009-2014 INRA.
 * Copyright (C) 2009-2014 CETIOM.
 *
 *
 * TODO licence
 */

#ifndef SUNFLO_PARAMETRESPLANTE_HPP_
#define SUNFLO_PARAMETRESPLANTE_HPP_

#include <vle/devs/InitEventList.hpp>
#include "SunfloUtils.hpp"

//#define VALEURPARDEFAUT_date_TT_germination 86.2 ///< valeur par defaut
//#define VALEURPARDEFAUT_dHE          1.19        ///< valeur par defaut
//#define VALEURPARDEFAUT_Eb_0         1.0         ///< valeur par defaut
//#define VALEURPARDEFAUT_Eb_c         4.5         ///< valeur par defaut
//#define VALEURPARDEFAUT_Eb_fin       0.015       ///< valeur par defaut
//#define VALEURPARDEFAUT_Eb_max       3.0         ///< valeur par defaut
//#define VALEURPARDEFAUT_LAI_a        400.0       ///< valeur par defaut
//#define VALEURPARDEFAUT_LAI_b        1.0         ///< valeur par defaut
//#define VALEURPARDEFAUT_LAI_c        153.0       ///< valeur par defaut
//#define VALEURPARDEFAUT_LAI_d        851.33      ///< valeur par defaut
//#define VALEURPARDEFAUT_LAI_e        -0.03783    ///< valeur par defaut
//#define VALEURPARDEFAUT_LAI_f        0.78469     ///< valeur par defaut
//#define VALEURPARDEFAUT_LAI_Kei      0.01379     ///< valeur par defaut
//#define VALEURPARDEFAUT_Phy1          71.43      ///< valeur par defaut
//#define VALEURPARDEFAUT_Phy2          16.34      ///< valeur par defaut
//#define VALEURPARDEFAUT_Tbase        4.8         ///< valeur par defaut
//#define VALEURPARDEFAUT_Tmax_PHS     37.0        ///< valeur par defaut
//#define VALEURPARDEFAUT_Topt1_PHS    20.0        ///< valeur par defaut
//#define VALEURPARDEFAUT_Topt2_PHS    28.0        ///< valeur par defaut
//#define VALEURPARDEFAUT_a_Pho       -25.0        ///< valeur par defaut
//#define VALEURPARDEFAUT_AA_a         0.1         ///< valeur par defaut
//#define VALEURPARDEFAUT_zRac_max     1800.0      ///< valeur par defaut
//#define VALEURPARDEFAUT_FNLEm        0.3         ///< valeur par defaut
//#define VALEURPARDEFAUT_INNseuil     0.6         ///< valeur par defaut
//#define VALEURPARDEFAUT_AP_a         0.1         ///< valeur par defaut
//#define VALEURPARDEFAUT_date_TT_F1M0 350.0       ///< valeur par defaut
//#define VALEURPARDEFAUT_VitCroiRac   0.7         ///< valeur par defaut
//#define VALEURPARDEFAUT_Kc           1.2         ///< valeur par defaut
//#define VALEURPARDEFAUT_PNCc_a       5.0         ///< valeur par defaut
//#define VALEURPARDEFAUT_PNCc_b       0.5         ///< valeur par defaut
//#define VALEURPARDEFAUT_TDMc_seuil   100.0       ///< valeur par defaut
//#define VALEURPARDEFAUT_PNCm_a       7.0         ///< valeur par defaut
//#define VALEURPARDEFAUT_PNCm_b       0.5         ///< valeur par defaut
//#define VALEURPARDEFAUT_TDMm_seuil   100.0       ///< valeur par defaut

namespace sunflo {

struct ParametresPlante {

    /// # Durée de la phase de germination (°Cd)
    //codeMMpourMemo parameter: date_TT_germination      86.2 0
    double date_TT_germination ;

    /// Vitesse de l'elongation de l'hypocotyle
    double dHE ;

    /// # Paramètres de cinétique de l’efficience biologique potentielle (valeur initiale)
    //codeMMpourMemo parameter: Eb_0   1     0
    double Eb_0 ;

    /// # Paramètres de cinétique de l’efficience biologique potentielle (valeur décroissance)
    //codeMMpourMemo parameter: Eb_c   4.5   0
    double Eb_c ;

    /// # Paramètres de cinétique de l’efficience biologique potentielle (valeur finale)
    //codeMMpourMemo parameter: Eb_fin 0.015 0
    double Eb_fin ;

    /// # Paramètres de cinétique de l’efficience biologique potentielle (valeur maximale)
    //codeMMpourMemo parameter: Eb_max 3     0
    double Eb_max ;

    /// ## Paramètres intervenant dans le module de surface folaire
    /// # Date thermique de fin d'expansion de la troisième paire de feuilles
    //codeMMpourMemo parameter: LAI_a 400    0
    double LAI_a ;

    /// # Constante intervenant dans le calcul des dates thermique de demi expansion pour tout les limbes.
    //codeMMpourMemo parameter: LAI_b 1      0
    double LAI_b ;

    /// # constante intervenant dans le calcul de la durée de vie des limbes (LL[i]) : asymptote de la courbe des durées de vie de limbes
    //codeMMpourMemo parameter: LAI_c 153    0
    double LAI_c ;

    /// # constante intervenant dans le calcul de la durée de vie des limbes (LL[i]) : agit comme la vitesse de sénescence, dans le modèle : plus cette valeur augmente, plus les feuilles meurent lentement (amplitude de la fonction de persistance)
    //codeMMpourMemo parameter: LAI_d 851.33       0
    double LAI_d ;

    /// # constante intervenant dans le calcul de la durée de vie des limbes LL[i]) (décalage de rang entre la feuille la plus grande et la plus persistante)
    //codeMMpourMemo parameter: LAI_e -0.03783     0
    double LAI_e ;

    /// # constante intervenant dans le calcul de la durée de vie des limbes (LL[i]) : détermination de la largeur du profil de "persistance"
    //codeMMpourMemo parameter: LAI_f 0.78469      0
    double LAI_f ;

    /// # Pente de la logistique représentant l'expansion et la senescence des limbes
    //codeMMpourMemo parameter: LAI_Kei      0.01379    0
    double LAI_Kei ;

    /// # Phyllotherme pour les 6 premières feuilles (°Cd)
    //codeMMpourMemo parameter: Phy1   71.43 0
    double Phy1 ;

    /// #Phyllotherme pour les limbes > rang 6 (°Cd)
    //codeMMpourMemo parameter: Phy2   16.34 0
    double Phy2 ;

    /// # Température de base [Granier1998]
    //codeMMpourMemo parameter: Tbase 4.8    0
    double Tbase ;

    /// # Température Maximale pour la capacité photosynthétique (°C)
    //codeMMpourMemo parameter: Tmax_PHS     37    0
    double Tmax_PHS ;

    /// # Température Optimale inférieure pour la capacité photosynthétique (°C)[Lecoeur2008]
    //codeMMpourMemo parameter: Topt1_PHS    20    0
    double Topt1_PHS ;
    /// # Température Optimale supérieure pour la capacité photosynthétique (°C)[Lecoeur2008]
    //codeMMpourMemo parameter: Topt2_PHS    28    0
    double Topt2_PHS ;

    ///# Décalage entre le seuil TR et le seuil de réponse de la photosynthèse.
    /// Seuil chute photosynthese (a partir du seuil TR)
    //codeMMpourMemo parameter: a_Pho -25    0
    double a_Pho ;

    ///# Taux d'absorption Active d'Azote (voir vAA)
    //codeMMpourMemo parameter: AA_a   0.1   0
    double AA_a ;

    ///# Profondeur maximale d’enracinement (mm)
    //codeMMpourMemo parameter: zRac_max     1800 0
    double zRac_max ;

    /// Effet maxi stress N sur LE
    double FNLEm ;

    /// Seuil d'INN en dessous duquel la reduction de LE est maximale
    double INNseuil ;

    /// Parametre d'acceleration thermique
    // parametre ajoute par rapport au code ModelMaker
    double AP_a;

    /// # Durée de la phase de floraison (°Cd)
    //codeMMpourMemo parameter: date_TT_F1M0 350    0
    double date_TT_F1M0 ;

    /// Vitesse de croissance des racines (mm/°Cj)
    // parametre ajoute par rapport au code ModelMaker
    double VitCroiRac;

    /// Coefficient cultural
    double Kc;

    /// Parametre a de la courbe de dilution critique de l'azote
    double PNCc_a;

    /// Parametre b de la courbe de dilution critique de l'azote
    double PNCc_b;

    /// Seuil TDM relatif a TNpcrit
    double TDMc_seuil;

    /// Parametre a pour TNpmax
    double PNCm_a;

    /// Parametre b pour TNpmax
    double PNCm_b;

    /// Seuil TDM relatif a TNpmax
    double TDMm_seuil;

    void initialiser(const vle::devs::InitEventList& evts)
    {
        date_TT_germination = Utils::extractDouble(evts, "date_TT_germination");
        dHE = Utils::extractDouble(evts, "dHE");
        Eb_0 = Utils::extractDouble(evts, "Eb_0");
        Eb_c = Utils::extractDouble(evts, "Eb_c");
        Eb_fin = Utils::extractDouble(evts, "Eb_fin");
        Eb_max = Utils::extractDouble(evts, "Eb_max");
        LAI_a = Utils::extractDouble(evts, "LAI_a");
        LAI_b = Utils::extractDouble(evts, "LAI_b");
        LAI_c = Utils::extractDouble(evts, "LAI_c");
        LAI_d = Utils::extractDouble(evts, "LAI_d");
        LAI_e = Utils::extractDouble(evts, "LAI_e");
        LAI_f = Utils::extractDouble(evts, "LAI_f");
        LAI_Kei = Utils::extractDouble(evts, "LAI_Kei");
        Phy1 = Utils::extractDouble(evts, "Phy1");
        Phy2 = Utils::extractDouble(evts, "Phy2");
        Tbase = Utils::extractDouble(evts, "Tbase");
        Tmax_PHS = Utils::extractDouble(evts, "Tmax_PHS");
        Topt1_PHS = Utils::extractDouble(evts, "Topt1_PHS");
        Topt2_PHS = Utils::extractDouble(evts, "Topt2_PHS");
        a_Pho = Utils::extractDouble(evts, "a_Pho");
        AA_a = Utils::extractDouble(evts, "AA_a");
        zRac_max = Utils::extractDouble(evts, "zRac_max");
        FNLEm = Utils::extractDouble(evts, "FNLEm");
        INNseuil = Utils::extractDouble(evts, "INNseuil");
        AP_a = Utils::extractDouble(evts, "AP_a");
        date_TT_F1M0 = Utils::extractDouble(evts, "date_TT_F1M0");
        VitCroiRac = Utils::extractDouble(evts, "VitCroiRac");
        Kc = Utils::extractDouble(evts, "Kc");
        PNCc_a = Utils::extractDouble(evts, "PNCc_a");
        PNCc_b = Utils::extractDouble(evts, "PNCc_b");
        TDMc_seuil = Utils::extractDouble(evts, "TDMc_seuil");
        PNCm_a = Utils::extractDouble(evts, "PNCm_a");
        PNCm_b = Utils::extractDouble(evts, "PNCm_b");
        TDMm_seuil = Utils::extractDouble(evts, "TDMm_seuil");
    }
};
} //namespace
#endif
