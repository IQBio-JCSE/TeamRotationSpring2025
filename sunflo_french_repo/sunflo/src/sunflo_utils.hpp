/**
 *
 * Copyright (C) 2009-2014 INRA.
 * Copyright (C) 2009-2014 CETIOM.
 *
 *
 * TODO licence
 */


#ifndef SUNFLO_SUNFLO_UTILS_HPP
#define SUNFLO_SUNFLO_UTILS_HPP

#include <string>
#include <sstream>
#include <vle/utils/DateTime.hpp>

#define VALEURDOUBLENONSIGNIFIANTE (-777.0)

namespace sunflo {

struct utils {

public :

    /// date_jjmm : "jj/mm"
    /// retour numerique : numero du jour correspondant dans son mois
    static unsigned int jj(const std::string& date_jjmm )
    {
        return ( atof( (date_jjmm.substr(0,2)).c_str() ) );
    }
    /// date_jjmm : "jj/mm"
    /// retour numerique : numero du mois correspondant
    static unsigned int mm(const std::string& date_jjmm )
    {
        return ( atof( (date_jjmm.substr(3,2)).c_str() ) );
    }

    /// date_jjmm : "jj/mm"
    /// retourne true si date_jjmm vaut "00/00"
    static bool nullite(const std::string& date_jjmm )
    {
        if ( ( jj(date_jjmm) == 0 ) && ( mm(date_jjmm) == 0 ) ){
        // valeur "00/00"
            return true;
        } else {
            return false;
        }
    }

    /// time de simulation
    /// retour numerique : numero du jour correspondant dans son mois
    static unsigned int jj( double time )
    {
        return ( vle::utils::DateTime::dayOfMonth( time ) );
    }

    /// time de simulation
    /// retour numerique : numero du mois correspondant
    static unsigned int mm( double time )
    {
        return ( vle::utils::DateTime::month( time ) );
    }

    /// time de simulation
    /// retour date_jjmm : "jj/mm"
    static std::string dateJJMM( double time )
    {
        std::string s1 = ""; if ( jj(time) <= 9 ){ s1 = "0"; }
        std::string s2 = ""; if ( mm(time) <= 9 ){ s2 = "0"; }
        return ( s1 + stringDe( (unsigned long)jj(time) ) + "/" + s2 + stringDe( (unsigned long)mm(time) ) );
    }

    static bool coincidence(const std::string& date_jjmm1,
            const std::string& date_jjmm2 )
    {
        if ( ( jj(date_jjmm1) == jj(date_jjmm2) )
                && ( mm(date_jjmm1) == mm(date_jjmm2) ) ){
            return true;
        } else {
            return false;
        }
    }

    static bool coincidence( double time, const std::string& date_jjmm )
    {
        if ( ( jj(time) == jj(date_jjmm) ) && ( mm(time) == mm(date_jjmm) ) ){
            return true;
        } else {
            return false;
        }
    }

    // retourne true si date_jjmm1 est anterieur a date_jjmm2 ou simultane
    static bool ordreChronologique(const std::string& date_jjmm1,
            const std::string& date_jjmm2 )
    {
        // julianDayNumber sert de critere de chronologie
        // 2004 choisie en tant qu'annee bissextile
        // date au format "aaaa-mm-jj"
        std::string date1 = "2004-" + ( date_jjmm1.substr(3,2) ) + "-" + ( date_jjmm1.substr(0,2) );
        std::string date2 = "2004-" + ( date_jjmm2.substr(3,2) ) + "-" + ( date_jjmm2.substr(0,2) );
        long julianDayNumber1 = vle::utils::DateTime::toJulianDayNumber( date1 );
        long julianDayNumber2 = vle::utils::DateTime::toJulianDayNumber( date2 );

        if ( julianDayNumber1 <= julianDayNumber2 ){
            return true;
        } else {
            return false;
        }
    }

    // retourne true si date_jjmm1 est strictement anterieur a date_jjmm2 (non simultane)
    static bool ordreChronologiqueStrict(const std::string& date_jjmm1,
            const std::string& date_jjmm2 )
    {
        return ( ( ordreChronologique( date_jjmm1, date_jjmm2 ) )
                && ( !coincidence( date_jjmm1, date_jjmm2 ) ) );
    }

    static std::string stringDe( double x)
    {
        std::ostringstream os;
        os.setf( std::ios::fixed, std::ios::floatfield );
        os << x;
        std::string str_x( os.str() );
        return str_x;
    }

    static std::string stringDe( int nbDeDecimales, double x )
    {
        std::ostringstream os;
        os.setf( std::ios::fixed, std::ios::floatfield );
        os.precision( nbDeDecimales );
        os << x;
        std::string str_x(os.str());
        return str_x;
    }

    static std::string stringDe( unsigned long x )
    {
        std::ostringstream os;
        os << x;
        std::string str_x( os.str() );
        return str_x;
    }
};

} // namespace

#endif

