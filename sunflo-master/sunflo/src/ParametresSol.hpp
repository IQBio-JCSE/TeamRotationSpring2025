/**
 *
 * Copyright (C) 2009-2014 INRA.
 * Copyright (C) 2009-2014 CETIOM.
 *
 *
 * TODO licence
 */


#ifndef SUNFLO_PARAMETRESSOL_HPP
#define SUNFLO_PARAMETRESSOL_HPP

#include "SunfloUtils.hpp"

namespace sunflo {

//#define VALEURPARDEFAUT_da_C1 1.5    ///< valeur par defaut
//#define VALEURPARDEFAUT_da_C2 1.5    ///< valeur par defaut
//#define VALEURPARDEFAUT_Hcc_C1 19.7  ///< valeur par defaut
//#define VALEURPARDEFAUT_Hcc_C2 19.7  ///< valeur par defaut
//#define VALEURPARDEFAUT_Fpf    0.2   ///< valeur par defaut
//#define VALEURPARDEFAUT_Vp     0.5   ///< valeur par defaut
//#define VALEURPARDEFAUT_Hpf_C1 9.7   ///< valeur par defaut
//#define VALEURPARDEFAUT_Hpf_C2 9.7   ///< valeur par defaut
//#define VALEURPARDEFAUT_TC     0.0   ///< valeur par defaut

struct ParametresSol
{
    /// # profondeur : profondeur maximale d'enracinement (mm)
    /// profondeur accessible aux racines
    //codeMMpourMemoprofondeur Controlled by: usm Universal
    //codeMMpourMemo  Linear interpolation
    double profondeur ;

    /// # Densité apparente du sol dans l'horizon de surface (g.cm-3)
    //codeMMpourMemo parameter: da_C1 1.5    0
    double da_C1 ;

    /// # Densité apparente du sol dans l'horizon racinaire(g.cm-3)
    //codeMMpourMemo parameter: da_C2 1.5    0
    double da_C2 ;

    /// # Paramètres de texture : humidités volumiques (%) à la capacité au champ (horizon surface et racinaire), humidités au semis (2 horizons), humidité au point de flétrissement (2 horizons)
    //codeMMpourMemo parameter: Hcc_C1 19.7 0
    double Hcc_C1 ;

    /// # Paramètres de texture : humidités volumiques (%) à la capacité au champ (horizon surface et racinaire), humidités au semis (2 horizons), humidité au point de flétrissement (2 horizons)
    //codeMMpourMemo parameter: Hcc_C2 19.7 0
    double Hcc_C2 ;

    //codeMMpourMemo parameter: Hpf_C1 9.7   0
    double Hpf_C1 ;

    //codeMMpourMemo parameter: Hpf_C2 9.7   0
    double Hpf_C2 ;

    //ratio of water if 1 -> Hini = Hcc if 0 -> Hini = Hpf
    double Hini_C1 ;
    //codeMMpourMemo parameter: Hini_C2      19.7 0
    double Hini_C2 ;

    /// # Valeur de la fonction "humidité" pour la minéralisation au point de flétrissement [Vale2006]
    //codeMMpourMemo parameter: Fpf    0.2   0
    double Fpf ;

    /// # Vitesse Potentielle de minéralisation (Kg/Ha/JourNormalise) (0.5 - 0.6 en moyenne)
    //codeMMpourMemo parameter: Vp     0.5   0
    double Vp ;

    /// # Taux de cailloux
    //codeMMpourMemo parameter: TC     0     0
    double TC ;

    /// # Profondeur de l’horizon de surface (mm)
    //codeMMpourMemo define value: zC1 Unconditional Universal
    //codeMMpourMemo zC1 = 300
    double zC1;

    // Réserve Utile (mm d'eau ) par mm de sol dans l'horizon de surface (C1)
    // RUmmC1 = (Hcc_C1-Hpf_C1)/100 * da_C1 * (1-TC)
    double RUmmC1;

    // Réserve Utile (mm d'eau ) par mm de sol dans l'horizon de surface (C2)
    // RUmmC2 = (Hcc_C2-Hpf_C2)/100 * da_C2 * (1-TC)
    double RUmmC2;

    // if false water stress is de-activated (ie FTSW is 1).
    // default : true
    bool activate_WaterStress;


    void initialiser( const vle::devs::InitEventList& events ){

        profondeur = Utils::extractDouble(events, "profondeur");


        if (events.exist("da_C1") and events.exist("da_C2")) {
            da_C1 = Utils::extractDouble(events, "da_C1");
            da_C2 = Utils::extractDouble(events, "da_C2");
        } else if (events.exist("da")){
            da_C1 = Utils::extractDouble(events, "da");
            da_C2 = da_C1;
        } else {
            throw vle::utils::ModellingError("[ParametersSol] missing 'da_*'");
        }
        if (events.exist("Hcc_C1") and events.exist("Hcc_C2")) {
            Hcc_C1 = Utils::extractDouble(events, "Hcc_C1");
            Hcc_C2 = Utils::extractDouble(events, "Hcc_C2");
        } else if (events.exist("Hcc")){
            Hcc_C1 = Utils::extractDouble(events, "Hcc");
            Hcc_C2 = Hcc_C1;
        } else {
            throw vle::utils::ModellingError("[ParametersSol] missing 'Hcc_*'");
        }
        if (events.exist("Hpf_C1") and events.exist("Hpf_C2")) {
            Hpf_C1 = Utils::extractDouble(events, "Hpf_C1");
            Hpf_C2 = Utils::extractDouble(events, "Hpf_C2");
        } else if (events.exist("Hpf")){
            Hpf_C1 = Utils::extractDouble(events, "Hpf");
            Hpf_C2 = Hpf_C1;
        } else {
            throw vle::utils::ModellingError("[ParametersSol] missing 'Hpf_*'");
        }
        if (events.exist("Hini_C1") and events.exist("Hini_C2")) {
            Hini_C1 = Utils::extractDouble(events, "Hini_C1");
            Hini_C2 = Utils::extractDouble(events, "Hini_C2");
        } else if (events.exist("Hini")){
            Hini_C1 = Utils::extractDouble(events, "Hini");
            Hini_C2 = Hini_C1;
        } else {
            throw vle::utils::ModellingError("[ParametersSol] missing 'Hini_*'");
        }
        Fpf = Utils::extractDouble(events, "Fpf");

        Vp = Utils::extractDouble(events, "Vp");
        TC = Utils::extractDouble(events, "TC");
        zC1 = Utils::extractDouble(events, "zC1");

        if (events.exist("RUmmC1") and
                (Utils::extractDouble(events, "RUmmC1") != -1)) {
            RUmmC1 = Utils::extractDouble(events, "RUmmC1");
        } else {
            RUmmC1 = (Hcc_C1 - Hpf_C1 ) / 100.0 * da_C1 * ( 1.0 - TC );
        }
        if (events.exist("RUmmC2") and
                (Utils::extractDouble(events, "RUmmC2") != -1)) {
            RUmmC2 = Utils::extractDouble(events, "RUmmC2");
        } else {
            RUmmC2 = (Hcc_C2 - Hpf_C2 ) / 100.0 * da_C2 * ( 1.0 - TC );
        }
        if (events.exist("activate_WaterStress")) {
            activate_WaterStress = events.getBoolean("activate_WaterStress");
        } else {
            activate_WaterStress = true;
        }
    }
};

} // namespace

#endif

