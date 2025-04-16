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

namespace sunflo {

using namespace vle::discrete_time;

class ContrainteLumiere : public DiscreteTimeDyn
{
public :

    /*Sync*/ Var RG;
    /*Nosync*/ Var SFp;
    /*Nosync*/ Var Ei;
    /*Sync*/ Var ActionSemis;
    /*Sync*/ Var densite;
    /*Sync*/ Var TT_A2;
    /*Sync*/ Var PhasePhenoPlante;
    Var niPAR;
    Var iPAR;
    Var FLe;
    double memo_densite;

    ContrainteLumiere(const vle::devs::DynamicsInit& model,
            const vle::devs::InitEventList& events) :
                DiscreteTimeDyn(model, events)
    {

        RG.init(this, "RG", events);

        SFp.init(this, "SFp", events);
        Ei.init(this, "Ei", events);

        ActionSemis.init(this, "ActionSemis", events);
        densite.init(this, "densite", events);

        TT_A2.init(this, "TT_A2", events);
        PhasePhenoPlante.init(this, "PhasePhenoPlante", events);


        niPAR.init(this, "niPAR", events);
        iPAR.init(this, "iPAR", events);
        FLe.init(this, "FLe", events);

        memo_densite = (double)VALEURDOUBLENONSIGNIFIANTE;
        niPAR = 0.0;
        iPAR = 0.0;

        {
            double FLe_tmp = 0.0;
            if ( TT_A2() < 50.0 ){
                FLe_tmp = 1.0;
            } else {
                FLe_tmp = 2.5*(-0.139+1.128/(1+exp(-(niPAR()-4.134)/2.093)));
            }
            FLe = FLe_tmp;
        }
    }

    virtual ~ContrainteLumiere() { }

    virtual void compute(const vle::devs::Time& /*time*/)
    {
        if ( ( (int)PhasePhenoPlante(-1) == PHASEPHENOPLANTE_NONSEMEE )
                && ( (int)ActionSemis() == 1)){
            memo_densite = densite();
        }

        {
            double iPAR_tmp = 0.0;

            if ( SFp(-1) == 0.0 ){
                iPAR_tmp = 0.0;

            } else if ( PhasePhenoPlante() <= PHASEPHENOPLANTE_NONSEMEE ){
                iPAR_tmp = 0.0;

            } else {
                iPAR_tmp = 0.48 * RG() * Ei(-1);

            }
            iPAR = iPAR_tmp;
        }
        {
            double niPAR_tmp = 0.0;

            if ( SFp(-1) == 0.0 ){
                niPAR_tmp = 0.0;

            } else if ( PhasePhenoPlante() <= PHASEPHENOPLANTE_NONSEMEE ){
                niPAR_tmp = 0.0;
            } else {
                if ( memo_densite == (double)VALEURDOUBLENONSIGNIFIANTE ){
                    throw ("erreur");
                } else {
                    niPAR_tmp = 0.48*RG()*Ei(-1)/(SFp(-1)*memo_densite)*10000.0;
                }
            }
            niPAR = niPAR_tmp;
        }


        {
            double FLe_tmp = 0.0;
            if ( TT_A2() < 50.0 ){
                FLe_tmp = 1.0;
            } else {
                FLe_tmp = 2.5*(-0.139+1.128/(1+exp(-(niPAR()-4.134)/2.093)));
            }
            FLe = FLe_tmp;
        }
    }
};

} // namespace

DECLARE_DYNAMICS(sunflo::ContrainteLumiere);

