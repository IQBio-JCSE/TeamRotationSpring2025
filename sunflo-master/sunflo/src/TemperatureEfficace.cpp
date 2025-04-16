/**
 *
 * Copyright (C) 2009-2014 INRA.
 * Copyright (C) 2009-2014 CETIOM.
 *
 *
 * TODO licence
 */

// @@tagdynamic@@
// @@tagdepends: vle.discrete-time @@endtagdepends

#include <vle/DiscreteTime.hpp>
#include <ParametresPlante.hpp>



namespace sunflo {

using namespace vle::discrete_time;

class TemperatureEfficace : public vle::discrete_time::DiscreteTimeDyn
{

public :
    ParametresPlante p;

    //Var entrees
    /*Sync*/ Var Tmoy;

    //Var etat
    Var Teff;
public :

    TemperatureEfficace(const vle::devs::DynamicsInit& model,
             const vle::devs::InitEventList& events) :
    DiscreteTimeDyn(model, events)
    {
        p.initialiser(events);
        Tmoy.init(this, "Tmoy", events);
        Teff.init(this, "Teff", events );
        double Teff_tmp = 0.0;
        if ( Tmoy() < p.Tbase ){
            Teff_tmp = 0.0;
        } else {
            Teff_tmp = Tmoy() - p.Tbase;
        }
        Teff.init_value(Teff_tmp);
    }

    virtual ~TemperatureEfficace() { }

    virtual void compute(const vle::devs::Time& /*time*/ )
    {
        {
            double Teff_tmp = 0.0;
            if ( Tmoy() < p.Tbase ){
                Teff_tmp = 0.0;
            } else {
                Teff_tmp = Tmoy() - p.Tbase;
            }
            Teff = Teff_tmp;
        }
    }
};

} // namespace

DECLARE_DYNAMICS(sunflo::TemperatureEfficace);

