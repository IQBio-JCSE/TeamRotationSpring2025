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


#include <math.h>
#include <vle/DiscreteTime.hpp>
#include <sunflo_utils.hpp>
#include <Phenologie.hpp>
#include <ParametresPlante.hpp>

namespace sunflo {

using namespace vle::discrete_time;

class ContrainteTemperature : public DiscreteTimeDyn
{
public :
    ParametresPlante pp;

    /*Sync*/ Var PhasePhenoPlante;
    /*Sync*/ Var Tmoy;
    Var FT;
    Var FTHN;
    bool first_compute;

    ContrainteTemperature(const vle::devs::DynamicsInit& model,
            const vle::devs::InitEventList& events) :
                DiscreteTimeDyn(model, events)
    {
        pp.initialiser( events );
        FT.init(this, "FT", events);
        FTHN.init(this, "FTHN", events);
        PhasePhenoPlante.init(this, "PhasePhenoPlante", events);
        Tmoy.init(this, "Tmoy", events);
        first_compute = true;
    }

    virtual ~ContrainteTemperature() { }

    virtual void compute(const vle::devs::Time& /*time*/ )
    {
        if (first_compute) {
            first_compute = false;
            FTHN = 36.0 / (1.0 + (36.0 - 1.0) * exp(-0.119 * (/* removed Tmoy()*/-15.0)));
        } else {
            double FT_tmp = 0.0;
            if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                    || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                FT_tmp = 1.0;
            } else if ( Tmoy() < pp.Tbase ){
                FT_tmp = 0.0;
            } else if (Tmoy() < pp.Topt1_PHS ){
                FT_tmp = Tmoy() * ( 1 / (pp.Topt1_PHS-pp.Tbase) )
                                -( pp.Tbase / (pp.Topt1_PHS-pp.Tbase) );
            } else if ( Tmoy() > pp.Topt2_PHS ){
                FT_tmp = Tmoy() * ( 1 / (pp.Topt2_PHS-pp.Tmax_PHS) )
                                - ( pp.Tmax_PHS / (pp.Topt2_PHS-pp.Tmax_PHS) );
            } else if (Tmoy() > pp.Tmax_PHS ){
                FT_tmp = 0.0;
            } else {
                FT_tmp = 1.0;
            }
            FT = FT_tmp;
        }
        FTHN = 36.0 / (1.0 + (36.0 -1.0 ) * exp( -0.119 * (Tmoy() -15.0)));
    }
};

} // namespace

DECLARE_DYNAMICS(sunflo::ContrainteTemperature);

