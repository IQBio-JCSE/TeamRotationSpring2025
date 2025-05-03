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
#include <Phenologie.hpp>
#include <ParametresPlante.hpp>
#include <ParametresVariete.hpp>
#include <ParametresSimuInit.hpp>
#include <sunflo_utils.hpp>
#include <iostream>

namespace sunflo {

using namespace vle::discrete_time;

class Phenologie : public vle::discrete_time::DiscreteTimeDyn
{
public :
    //parameters
    double estimationTTentreSemisEtLevee_casPhaseSemisLeveeSimulee;
    ParametresPlante pp;
    ParametresVariete pv;
    ParametresSimuInit psi;
    bool simulationPhaseSemisLevee;

    //var entrees
    /*Sync*/ Var Teff;
    /*Nosync*/ Var FHTR;
    /*Sync*/ Var ActionSemis;
    /*Sync*/ Var zSemis;
    /*Sync*/ Var ActionRecolte;

    //var internes
    Var TT_A0;
    Var AP;
    Var TT_A2;
    Var PhasePhenoPlante;
    Var TT_F1;
    Var date_SEMIS;
    Var date_CROISSANCEACTIVE;
    Var date_FLORAISON;
    Var date_MATURATION;
    Var date_DESSICATION;

    Phenologie(const vle::devs::DynamicsInit& model,
             const vle::devs::InitEventList& events) :
    vle::discrete_time::DiscreteTimeDyn(model, events)
    {
        pp.initialiser(events);
        pv.initialiser(events);
        psi.initialiser(events);


        if ( psi.date_levee < 0 ){
            simulationPhaseSemisLevee = true;
        } else {
            simulationPhaseSemisLevee = false;
        }

        /*Sync*/ Teff.init(this, "Teff", events);
        /*NoSync*/ FHTR.init(this, "FHTR", events);
        /*Sync*/ ActionSemis.init(this, "ActionSemis", events);
        /*Sync*/ zSemis.init(this, "zSemis", events);
        /*Sync*/ ActionRecolte.init(this, "ActionRecolte", events);
        TT_A2.init(this, "TT_A2", events);
        PhasePhenoPlante.init(this, "PhasePhenoPlante", events);
        TT_F1.init(this, "TT_F1", events);
        date_SEMIS.init(this, "date_SEMIS", events);
        date_CROISSANCEACTIVE.init(this, "date_CROISSANCEACTIVE", events);
        date_FLORAISON.init(this, "date_FLORAISON", events);
        date_MATURATION.init(this, "date_MATURATION", events);
        date_DESSICATION.init(this, "date_DESSICATION", events);


        TT_A0.init(this, "TT_A0", events);
        AP.init(this, "AP", events);

        double FHTR_valeurInitiale = 1.0;

        estimationTTentreSemisEtLevee_casPhaseSemisLeveeSimulee =
                (double)VALEURDOUBLENONSIGNIFIANTE;
        TT_A0.init_value(0.0);
        TT_A2.init_value(0.0);
        TT_F1.init_value(0.0);
        PhasePhenoPlante.init_value((double)PHASEPHENOPLANTE_NONSEMEE);
        AP.init_value(pp.AP_a * Teff() * ( 1 - FHTR_valeurInitiale ));


    }

    virtual ~Phenologie() { }

    // Il s'agit de traitements du paragraphe "2.1. Phenology" de la publi
    virtual void compute(const vle::devs::Time& time)
    {
        if ( ( (int)PhasePhenoPlante(-1) == PHASEPHENOPLANTE_NONSEMEE )
                && ( (int)ActionSemis() == 1 ) ){
            estimationTTentreSemisEtLevee_casPhaseSemisLeveeSimulee =
                    pp.date_TT_germination + pp.dHE * zSemis();
            date_SEMIS = time;
        }
        {
            if ( ( (int)PhasePhenoPlante(-1) > PHASEPHENOPLANTE_NONSEMEE )
                    && ( (int)PhasePhenoPlante(-1) < PHASEPHENOPLANTE_RECOLTEE )
                    && ( (int)ActionRecolte() == 1 ) ){
                TT_A0 = 0.0;
            } else {
                double ddt = 0.0;

                if ( ( (int)PhasePhenoPlante(-1) > PHASEPHENOPLANTE_NONSEMEE )
                        && ( (int)PhasePhenoPlante(-1) < PHASEPHENOPLANTE_RECOLTEE ) ){
                    ddt = Teff() + AP(-1);
                } else {
                    ddt = 0.0;
                }
                TT_A0 = TT_A0(-1) + ddt;
            }
        }
        {
            if ( ( (int)PhasePhenoPlante(-1) > PHASEPHENOPLANTE_NONSEMEE )
                    && ( (int)PhasePhenoPlante(-1) < PHASEPHENOPLANTE_RECOLTEE )
                    && ( (int)ActionRecolte() == 1 ) ){
                TT_A2 = 0.0;
            } else {
                double ddt = 0.0;

                if ( ( (int)PhasePhenoPlante(-1) > PHASEPHENOPLANTE_NONSEMEE )
                        and ( (int)PhasePhenoPlante(-1) < PHASEPHENOPLANTE_RECOLTEE ) ){
                    if ( ( simulationPhaseSemisLevee )
                            and ( TT_A0() < estimationTTentreSemisEtLevee_casPhaseSemisLeveeSimulee ) ){
                        ddt = 0.0;
                    } else if (( not simulationPhaseSemisLevee ) and
                            (psi.date_levee >  psi.begin_date + (long) time)){
                        ddt = 0.0;
                    } else {
                        ddt = Teff() + AP(-1);
                    }

                } else {
                    ddt = 0.0;
                }
                TT_A2 = TT_A2(-1) + ddt;
            }
        }
        {
            double ddt = 0.0;

            if ( (int)PhasePhenoPlante(-1) >= PHASEPHENOPLANTE_RECOLTEE){
                // traduit condition t >= jrecolte
                ddt = 0.0;
            } else if ( ( (int)PhasePhenoPlante(-1) >= PHASEPHENOPLANTE_FLORAISON)
                    && ( (int)PhasePhenoPlante(-1) < PHASEPHENOPLANTE_RECOLTEE) ){
                // traduit condition TT_A2 > date_TT_F1
                ddt = Teff() + AP(-1);
            } else {
                ddt = 0.0;
            }
            TT_F1 = TT_F1(-1) + ddt;
        }
        {
            double PhasePhenoPlante_tmp = PhasePhenoPlante(-1); // par defaut

            switch ( (int)PhasePhenoPlante(-1) ){

            case PHASEPHENOPLANTE_NONSEMEE :

                if ((int)ActionSemis() == 1 ){ // instant de semis

                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_GERMINATION ;
                }

                if ((int)ActionRecolte() == 1 ){
                    throw vle::utils::ModellingError("[Phenologie] Error recolte avant semis");
                }
                break;

            case PHASEPHENOPLANTE_GERMINATION :
                if ((int)ActionRecolte() == 1 ){
                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_RECOLTEE ;
                    throw vle::utils::ModellingError("[Phenologie] Error recolte en germination ");
                } else if ( simulationPhaseSemisLevee ){
                    if ( TT_A0() >= estimationTTentreSemisEtLevee_casPhaseSemisLeveeSimulee ){
                        PhasePhenoPlante_tmp = PHASEPHENOPLANTE_JUVENILE ;
                    }
                } else { // not p.simulationPhaseSemisLevee
                    if (psi.date_levee ==  psi.begin_date + (long) time){
                        // date levee (dateLevee_casForcee) atteinte
                        PhasePhenoPlante_tmp = PHASEPHENOPLANTE_JUVENILE ;
                    }
                }
                break;

            case PHASEPHENOPLANTE_JUVENILE :
                if ((int)ActionRecolte() == 1 ){
                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_RECOLTEE ;
                    throw vle::utils::ModellingError("[Phenologie] Error recolte pendant juvenile");

                } else if ( TT_A2() >= pv.date_TT_E1 ){ // date thermique du stade etoiles atteinte
                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_CROISSANCEACTIVE ;
                    date_CROISSANCEACTIVE = time - date_SEMIS();
                }
                break;

            case PHASEPHENOPLANTE_CROISSANCEACTIVE :

                if ((int)ActionRecolte() == 1 ){
                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_RECOLTEE ;
                    throw vle::utils::ModellingError("[Phenologie] Error recolte pendant croissance active");
                } else if ( TT_A2() >= pv.date_TT_F1 ){ // date thermique du stade floraison atteinte
                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_FLORAISON ;
                    date_FLORAISON = time - date_SEMIS();
                }
                break;

            case PHASEPHENOPLANTE_FLORAISON :
                if ((int)ActionRecolte() == 1 ){
                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_RECOLTEE ;
                    std::cout << "[Phenologie] Error recolte avant floraison" << "\n";
                    throw vle::utils::ModellingError("[Phenologie] Error recolte avant floraison");

                } else if ( TT_A2() >= pv.date_TT_M0 ){ // date thermique du stade fin floraison atteinte
                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_MATURATION ;
                    date_MATURATION = time - date_SEMIS();
                }
                break;

            case PHASEPHENOPLANTE_MATURATION :

                if ((int)ActionRecolte() == 1 ){
                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_RECOLTEE ;

                } else if ( TT_A2() >= pv.date_TT_M3 ){ // date
                    // thermique
                    // du stade
                    // maturite
                    // atteinte

                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_DESSICATION ;
                    date_DESSICATION = time - date_SEMIS();

                }
                break;

            case PHASEPHENOPLANTE_DESSICATION :
                if ((int)ActionRecolte() == 1 ){
                    PhasePhenoPlante_tmp = PHASEPHENOPLANTE_RECOLTEE ;
                }

                break;

            case PHASEPHENOPLANTE_RECOLTEE :
                break;

            default :
                throw vle::utils::ModellingError("[Phenologie] Error State");
                break;
            }
            PhasePhenoPlante = PhasePhenoPlante_tmp;
        }
        AP = pp.AP_a * Teff() * ( 1 - FHTR(-1) );
    }
};

} // namespace sunflo

DECLARE_DYNAMICS(sunflo::Phenologie);

