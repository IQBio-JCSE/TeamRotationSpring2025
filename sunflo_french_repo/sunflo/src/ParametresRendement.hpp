/**
 *
 * Copyright (C) 2009-2014 INRA.
 * Copyright (C) 2009-2014 CETIOM.
 *
 *
 * TODO licence
 */

#ifndef SUNFLO_PARAMETRESRENDEMENT_HPP
#define SUNFLO_PARAMETRESRENDEMENT_HPP


namespace sunflo {

//#define VALEURPARDEFAUT_SeuilETRETM 0.6  ///< valeur par defaut

struct ParametresRendement
{

    /// Seuil ETR/ETM pour calcul jours de stress
    double SeuilETRETM ;

    void initialiser( const vle::devs::InitEventList& events ){
        if ( events.exist("SeuilETRETM") ){
            SeuilETRETM = events.getDouble("SeuilETRETM");
        } else {
            throw vle::utils::ModellingError("[ParametersSimuInit] missing 'SeuilETRETM'");
        }
    }
};

} // namespace

#endif

