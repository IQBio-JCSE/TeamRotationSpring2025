// @@tagdynamic@@
// @@tagdepends: vle.discrete-time @@endtagdepends



#include <vle/DiscreteTime.hpp>
#include <ParametresSimuInit.hpp>

#include "SunfloUtils.hpp"

namespace vd = vle::devs;
namespace vv = vle::value;

namespace sunflo {

using namespace vle::discrete_time;

class Decision : public DiscreteTimeDyn
{
public:
    std::vector<std::pair<long, double> > fertilization;
    std::vector<std::pair<long, double> > irrigation;

    Decision(
            const vd::DynamicsInit& atom,
            const vd::InitEventList& events)
    : DiscreteTimeDyn(atom, events), fertilization(), irrigation()
    {
        psi.initialiser(events);

        ActionSemis.init(this, "ActionSemis", events);
        densite.init(this, "densite", events);
        zSemis.init(this, "zSemis", events);
        ActionFerti.init(this, "ActionFerti", events);
        DoseFerti.init(this, "DoseFerti", events);
        ActionIrrig.init(this, "ActionIrrig", events);
        DoseIrrig.init(this, "DoseIrrig", events);
        ActionRecolte.init(this, "ActionRecolte", events);

        densite_p = Utils::extractDouble(events, "densite");
        zSemis_p = Utils::extractDouble(events, "zSemis");
        jrecolte = Utils::extractDate(events, "jrecolte");

        //initialise fertilizations
        bool finish = false;
        unsigned int i=0;
        while (!finish) {
            i++;
            std::stringstream ss;
            ss << "fertilization_" << i;
            finish = !events.exist(ss.str());
            if (! finish) {

                double date =0;
                double dose =0;
                Utils::extractApport(events, ss.str(), date, dose);
                if (dose > 0) {
                    fertilization.push_back(std::make_pair(date, dose));
                }

            }
        }

        //initialise irrigations
        finish = false;
        i=0;
        while (!finish) {
            i++;
            std::stringstream ss;
            ss << "irrigation_" << i;
            finish = !events.exist(ss.str());
            if (! finish) {
                double date =0;
                double dose =0;
                Utils::extractApport(events, ss.str(), date, dose);
                if (dose > 0) {
                    irrigation.push_back(std::make_pair(date, dose));
                }
            }
        }
    }

    virtual ~Decision()
    {}

    virtual void compute(const vd::Time& j)
    {
        //sowing
        if (psi.jsemis == psi.begin_date + (long)j){
            ActionSemis = 1;
            densite = densite_p;
            zSemis = zSemis_p;
            ActionFerti = 0;
            DoseFerti = 0;
            ActionIrrig = 0 ;
            DoseIrrig = 0;
            ActionRecolte = 0;
            return;
        }
        //harvesting
        if (jrecolte == psi.begin_date + (long)j){
            ActionSemis = 0;
            densite = 0;
            zSemis = 0;
            ActionFerti = 0;
            DoseFerti = 0;
            ActionIrrig = 0 ;
            DoseIrrig = 0;
            ActionRecolte = 1;
            return;
        }
        //fertilizations
        bool found = false;
        for(unsigned int i=0;i < fertilization.size() and not found; i++) {
            const std::pair<long, double>& f = fertilization[i];
            found = (f.first == (long) psi.begin_date + j);
            if (found) {
                ActionFerti = 1;
                DoseFerti = f.second;
            }
        }
        if (not found) {
            ActionFerti = 0;
            DoseFerti = 0;
        }

        //irrigations
        found = false;
        for(unsigned int i=0;i < irrigation.size() and not found; i++) {
            const std::pair<long, double>& f = irrigation[i];
            found = (f.first == (long) psi.begin_date + j);
            if (found) {
                ActionIrrig = 1;
                DoseIrrig = f.second;
            }
        }
        if (not found) {
            ActionIrrig = 0;
            DoseIrrig = 0;
        }
        //default sowing and harvesting
        ActionSemis = 0;
        densite = 0;
        zSemis = 0;
        ActionRecolte = 0;
    }

    Var ActionSemis;
    Var densite;
    Var zSemis;
    Var ActionFerti;
    Var DoseFerti;
    Var ActionIrrig;
    Var DoseIrrig;
    Var ActionRecolte;


    ParametresSimuInit psi;
    double densite_p;
    double zSemis_p;
    long jrecolte;


};

} // namespace

DECLARE_DYNAMICS(sunflo::Decision);

