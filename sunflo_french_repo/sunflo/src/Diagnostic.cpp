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
#include <ParametresVariete.hpp>
#include <ParametresRendement.hpp>

namespace sunflo {

using namespace vle::discrete_time;

class Diagnostic : public DiscreteTimeDyn
{
public :
    ParametresVariete pv;
    ParametresPlante pp;
    ParametresRendement pr;
    bool first_compute;

    /*Sync*/ Var TT_A2;
    /*Sync*/ Var ETRETM;


    /// # Jours de stress jusqu'a la floraison (estimationTTentreSemisEtLevee_casPhaseSemisLeveeSimulee - F1)
    /// # (pas utilisé dans le modèle, sortie de Diagnostic)
    Var ISH1;

    /// # Jours de stress Autour de la floraison (F1 + 350°Cd)
    /// # (pas utilisé dans le modèle, sortie de Diagnostic)
    Var ISH2;

    /// # Jours de stress après la floraison (F1+350 °Cd – M3)
    /// # (pas utilisé dans le modèle, sortie de Diagnostic)
    Var ISH3;

    Diagnostic(const vle::devs::DynamicsInit& model,
            const vle::devs::InitEventList& events) :
                DiscreteTimeDyn(model, events)
    {
        pv.initialiser(events);
        pr.initialiser(events);
        pp.initialiser(events);

        TT_A2.init(this, "TT_A2", events);
        ETRETM.init(this, "ETRETM", events);
        ISH1.init(this, "ISH1" , events);
        ISH2.init(this, "ISH2" , events);
        ISH3.init(this, "ISH3" , events);

        first_compute = true;
    }

    virtual ~Diagnostic() { }

    virtual void compute(const vle::devs::Time& /*time*/ )
    {
        if (first_compute) {
            first_compute = false;
            ISH1 = 0.0; //codeMMpourMemo Initial Value = 0.0
            ISH2 = 0.0; //codeMMpourMemo Initial Value = 0.0
            ISH3 = 0.0; //codeMMpourMemo Initial Value = 0.0
        } else {
            {
                double ddt = 0.0;
                if ( ETRETM() > pr.SeuilETRETM ){
                    ddt = 0.0;
                } else if ( TT_A2() > pv.date_TT_F1 ){
                    ddt = 0.0;
                } else if ( TT_A2() <= 0.0 ){
                    // traduit condition TT_A2 <= 0
                    ddt = 0.0;
                } else {
                    ddt = 1.0;
                }
                ISH1 = ISH1(-1) + ddt;
            }

            {
                double ddt = 0.0;

                if ( ETRETM() > pr.SeuilETRETM ){
                    ddt = 0.0;
                } else if ( TT_A2() > (pv.date_TT_F1 + pp.date_TT_F1M0) ){
                    ddt = 0.0;
                } else if ( TT_A2() < pv.date_TT_F1 ){
                    ddt = 0.0;
                } else {
                    ddt = 1.0;
                }
                ISH2 = ISH2(-1) + ddt;
            }


            {
                double ddt = 0.0;

                if ( ETRETM() > pr.SeuilETRETM ){
                    ddt = 0.0;
                } else if ( TT_A2() > pv.date_TT_M3 ){
                    ddt = 0.0;
                } else if ( TT_A2() < (pv.date_TT_F1 + pp.date_TT_F1M0) ){
                    ddt = 0.0;
                } else {
                    ddt = 1.0;
                }
                ISH3 = ISH3(-1) + ddt;
            }
        }
    }
};

} // namespace sunflo

DECLARE_DYNAMICS(sunflo::Diagnostic);

