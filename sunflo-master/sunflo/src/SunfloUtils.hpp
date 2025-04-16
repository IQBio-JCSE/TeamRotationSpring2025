/**
 *
 * Copyright (C) 2017-2017 INRA.
 *
 * TODO licence
 */

#ifndef SUNFLO_UTILS_HPP
#define SUNFLO_UTILS_HPP

#include <memory>
#include <sstream>
#include <vle/value/String.hpp>
#include <vle/value/Double.hpp>
#include <vle/value/Map.hpp>
#include <vle/value/Integer.hpp>
#include <vle/devs/InitEventList.hpp>
#include <vle/utils/DateTime.hpp>
#include <vle/utils/Tools.hpp>
#include <vle/utils/Exception.hpp>



namespace sunflo {

namespace vu = vle::utils;
namespace vd = vle::devs;
namespace vv = vle::value;

struct Utils
{

    /** @brief Get the a absolute date from a short date (month, day)
     * and a year
     * @param date_short (ex. 07-20)
     * @param year (ex. 2000)
     * @return absolute time corresponding to short_date whith year of time
     * or year of time +1 (ex: 2451746 which corresponds to 2000-07-20)
     */
    static long
    getAbsoluteDate(const std::string& date_short, long year) {
        std::stringstream ss;
        ss << year << "-" << date_short;
        return vu::DateTime::toJulianDayNumber(ss.str());
    }

    /** 
     * @brief Get a date and dose from a value in initialization
     * @param evts, init event list
     * @param port, name of the port in events
     * @param init, a vle value which can be either :
     *         - map containing a string with key 'date' and 
     *                           a double with key 'dose'
     *         - a string of the form "date=1992-02-26#dose=30"
     * @param[out] date the date computed in double
     * @param[out] dose the dose computed
     * @return true if ok
     */
    static bool
    extractApport(const vd::InitEventList& evts,
            const std::string& port, double& date, double& dose)
    {
        const std::shared_ptr<const vle::value::Value>& init = evts.get(port);
        if (init->isString()) {
            std::vector<std::string> toks;
            vle::utils::tokenize(init->toString().value(),toks,"$",true);
            if (toks.size() != 2) {
                date = 0;
                dose = 0;
                return false;
            }
            std::string date_str=toks[0];
            std::string dose_str=toks[1];
            toks.clear();
            vle::utils::tokenize(date_str,toks,"=",true);
            if (toks.size() != 2 or toks[0] != "date") {
                date = 0;
                dose = 0;
                return false;
            }
            date = vu::DateTime::toJulianDayNumber(toks[1]);
            toks.clear();
            vle::utils::tokenize(dose_str,toks,"=",true);
            if (toks.size() != 2 or toks[0] != "dose") {
                date = 0;
                dose = 0;
                return false;
            }
            dose = vle::utils::to<double>(toks[1]);
        } else if (init->isMap()) {
            const vv::Map& appli = init->toMap();
            date = vu::DateTime::toJulianDayNumber(appli.getString("date"));
            dose = appli.getDouble("dose");
        }
        return true;
    }

    /**
     * @brief Extract a double form an int or a double
     * @param evts, init event list
     * @param port, name of the port in events
     * @return a double
     */
    static double
    extractDouble(const vd::InitEventList& evts, const std::string& port)
    {
        if (not evts.exist(port)) {
           std::string mess = " parameter "+port;
           mess += " is missing";
           throw vu::ArgError(mess);
        }
        const std::shared_ptr<const vle::value::Value>& init = evts.get(port);
        if (init->isDouble()) {
            return init->toDouble().value();
        } else if (init->isInteger()) {
            return (double) init->toInteger().value();
        } else {
           std::string mess = " parameter "+port;
           mess += " has wrong format (expect Integer or Double)";
           throw vu::ArgError(mess);
        }
        return std::numeric_limits<double>::infinity();
    }


    /**
     * @brief Extract a date from a double, an integer, or a date "1999-01-01"
     * @param evts, init event list
     * @param port, name of the port in events
     * @return a long
     */
   static long
   extractDate(const vle::devs::InitEventList& evts,
            const std::string& port)
    {
       if (not evts.exist(port)) {
           std::string mess = " parameter "+port;
           mess += " is missing";
           throw vu::ArgError(mess);
       }
       const std::shared_ptr<const vle::value::Value>& init = evts.get(port);
       if (init->isString()) {
           return vu::DateTime::toJulianDayNumber(init->toString().value());
       }
       if (init->isDouble()) {
           return (long) init->toDouble().value();
       }
       if (init->isInteger()) {
           return (long) init->toInteger().value();
       } else {
           std::string mess = " parameter "+port;
           mess += " has wrong format (expect String or Double)";
           throw vu::ArgError(mess);
       }
       return -1;
    }
};

}//namespaces

#endif


