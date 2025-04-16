/**
 *
 * Copyright (C) 2009-2014 INRA.
 * Copyright (C) 2009-2014 CETIOM.
 *
 *
 * TODO licence
 */


#ifndef SUNFLO_PARAMETRESVARIETE_HPP
#define SUNFLO_PARAMETRESVARIETE_HPP

#include "SunfloUtils.hpp"

namespace sunflo {

struct ParametresVariete
{
    /// date thermique du stade étoiles
    //codeMMpourMemo date_TT_E1 Controlled by: gen Universal
    //codeMMpourMemo   Linear interpolation
    double date_TT_E1 ;

    /// date thermique du stade floraison
    //codeMMpourMemo date_TT_F1 Controlled by: gen Universal
    //codeMMpourMemo   Linear interpolation
    double date_TT_F1 ;

    /// date thermique du stade fin floraison
    //codeMMpourMemo date_TT_M0 Controlled by: gen Universal
    //codeMMpourMemo   Linear interpolation
    double date_TT_M0 ;

    /// date thermique du stade maturité
    //codeMMpourMemo date_TT_M3 Controlled by: gen Universal
    //codeMMpourMemo   Linear interpolation
    double date_TT_M3 ;

    /// # Surface de la plus grande feuille (lien depuis la table « PG »)
    //codeMMpourMemo  variable: cSF Unconditional Universal
    //codeMMpourMemo SFimax Controlled by: gen Universal
    //codeMMpourMemo Linear interpolation
    //codeMMpourMemo taille de la plus grande feuille 
    // cSF remplace SFimax
    double cSF;

    /// # Position de la plus grande feuille (lien depuis la table « PG »)
    //codeMMpourMemo  variable: bSF Unconditional Universal
    //codeMMpourMemo n_SFimax Controlled by: gen Universal
    //codeMMpourMemo Linear interpolation
    //codeMMpourMemo position de la plus grande feuille 
    // bSF remplace n_SFimax
    double bSF;

    /// # Nombre maximal de limbes sur la plante
    //codeMMpourMemo  variable: TLN Unconditional Universal
    //codeMMpourMemo NFfinal Controlled by: gen Universal
    //codeMMpourMemo Linear interpolation
    //codeMMpourMemo nombre de feuille
    // TLN remplace NFfinal
    double TLN;

    /// # Coefficient d’extinction
    //codeMMpourMemo variable: ext Unconditional Universal
    //codeMMpourMemo ext = coeff_k
    //codeMMpourMemo coeff_k Controlled by: gen Universal
    //codeMMpourMemo Linear interpolation
    //codeMMpourMemo coefficient d’extinction 
    // ext remplace coeff_k
    double ext;

    /// photosynthèse maximum relative
    //codeMMpourMemo PHS Controlled by: gen Global
    //codeMMpourMemo   Linear interpolation
    double PHS;

    /// sensibilité hydrique de l’expansion
    //codeMMpourMemo a_LE Controlled by: gen Universal
    //codeMMpourMemo   Linear interpolation
    double a_LE;

    /// sensibilité hydrique de la transpiration
    //codeMMpourMemo a_TR Controlled by: gen Universal
    //codeMMpourMemo   Linear interpolation
    double a_TR;

    /// indice de récolte potentiel
    //codeMMpourMemo IRg Controlled by: gen Global
    //codeMMpourMemo   Linear interpolation
    double IRg;

    /// teneur en huile potentielle
    //codeMMpourMemo thp Controlled by: gen Global
    //codeMMpourMemo   Linear interpolation
    double thp;

    void initialiser( const vle::devs::InitEventList& evts ){

        date_TT_E1 = Utils::extractDouble(evts, "date_TT_E1");

        //date_TT_F1
        if ( evts.exist("date_TT_F1") ){
            if (evts.exist("reldate_TT_F1")) {
               throw vle::utils::ModellingError(
                       "[ParametersVariete] either 'date_TT_F1' or 'reldate_TT_F1'");
            }
            date_TT_F1 = Utils::extractDouble(evts, "date_TT_F1");
        } else if (evts.exist("reldate_TT_F1")) {
            date_TT_F1 = date_TT_E1+Utils::extractDouble(evts, "reldate_TT_F1");
        } else {
           throw vle::utils::ModellingError(
                   "[ParametersVariete] missing 'date_TT_F1' and 'reldate_TT_F1'");
        }
        
        //date_TT_M0
        if ( evts.exist("date_TT_M0") ){
            if (evts.exist("reldate_TT_M0")) {
              throw vle::utils::ModellingError(
                      "[ParametersVariete] either 'date_TT_M0' or 'reldate_TT_M0'");
            }
            date_TT_M0 = Utils::extractDouble(evts, "date_TT_M0");
        } else if (evts.exist("reldate_TT_M0")) {
            date_TT_M0 = date_TT_F1+Utils::extractDouble(evts, "reldate_TT_M0");
        } else {
            throw vle::utils::ModellingError(
                    "[ParametersVariete] missing 'date_TT_M0' and 'reldate_TT_M0'");
        }

        //date_TT_M3
        if ( evts.exist("date_TT_M3") ){
            if (evts.exist("reldate_TT_M3")) {
              throw vle::utils::ModellingError(
                      "[ParametersVariete] either 'date_TT_M3' or 'reldate_TT_M3'");
            }
            date_TT_M3 = Utils::extractDouble(evts, "date_TT_M3");
        } else if (evts.exist("reldate_TT_M3")) {
            date_TT_M3 = date_TT_M0+Utils::extractDouble(evts, "reldate_TT_M3");
        } else {
            throw vle::utils::ModellingError(
                    "[ParametersVariete] missing 'date_TT_M3' and 'reldate_TT_M3'");
        }

        //check dates
        if ( ( date_TT_E1 < date_TT_F1 )
                && ( date_TT_F1 < date_TT_M0 )
                && ( date_TT_M0 < date_TT_M3 ) ){
            // ok
        } else { // probleme
            throw vle::utils::ModellingError("[ParametersVariete] error date_TT_*");
        }
        cSF = Utils::extractDouble(evts, "cSF");
        bSF = Utils::extractDouble(evts, "bSF");
        TLN = Utils::extractDouble(evts, "TLN");
        ext = Utils::extractDouble(evts, "ext");
        PHS = Utils::extractDouble(evts, "PHS");
        a_LE = Utils::extractDouble(evts, "a_LE");
        a_TR = Utils::extractDouble(evts, "a_TR");
        IRg = Utils::extractDouble(evts, "IRg");
        thp = Utils::extractDouble(evts, "thp");

    }

};

} // namespace

#endif

