/**
 *
 * Copyright (C) 2009-2014 INRA.
 * Copyright (C) 2009-2014 CETIOM.
 *
 *
 * TODO licence
 */

#ifndef SUNFLO_PARAMETRESSIMUINIT_HPP
#define SUNFLO_PARAMETRESSIMUINIT_HPP

#include "SunfloUtils.hpp"

namespace sunflo {

//#define VALEURPARDEFAUT_zC1 300.0 ///< valeur par defaut
//#define VALEURPARDEFAUT_dateLevee_casForcee "00/00" ///< valeur par defaut
//#define VALEURPARDEFAUT_Hini_C1 19.7  ///< valeur par defaut
//#define VALEURPARDEFAUT_Hini_C2 19.7  ///< valeur par defaut
//#define VALEURPARDEFAUT_SeuilETRETM 0.6  ///< valeur par defaut

struct ParametresSimuInit
{


    //begin day corresponding to t=0 of simulation
    long begin_date;
    //sowing day in julian days
    long jsemis;
    //forcing rising day of the crop in julian day (-1 if rising is simulated)
    long date_levee ;

    void initialiser( const vle::devs::InitEventList& events ){
        begin_date = Utils::extractDate(events, "begin_date");
        jsemis = Utils::extractDate(events, "jsemis");
        date_levee = -1;
        if (events.exist("date_levee")){
            date_levee = Utils::extractDate(events, "date_levee");
        } else if(events.exist("reldate_levee")) {
            date_levee = jsemis +
                    (long) Utils::extractDouble(events, "reldate_levee");
        }
        if (date_levee <= jsemis){
            date_levee = -1;
        }
    }
};

} // namespace

#endif

