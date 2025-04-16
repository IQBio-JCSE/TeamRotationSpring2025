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
#include <ParametresVariete.hpp>
#include <ParametresRendement.hpp>


namespace sunflo {

using namespace vle::discrete_time;

class ElaborationRendement : public DiscreteTimeDyn
{
public :
    ParametresRendement pr;
    ParametresVariete pv;
    bool first_compute;

    /*Sync*/ Var PhasePhenoPlante;
    /*Sync*/ Var TT_F1;
    /*Sync*/ Var TDM;
    /*Sync*/ Var ETRETM;
    /*Sync*/ Var TRPF;
    /*Sync*/ Var INN;
    /// # Rendement (0% humidité, 0% impuretés)
    Var RDT;
    /// # Jours de stress végétatifs (E1 - F1) (covariables statistiques)
    Var JSE;
    /// # Jours de stress à la floraison (F1 - M0) (covariables statistiques)
    Var JSF;
    /// # Jours de stress après la floraison (M0 - M3) (covariables statistiques)
    Var JSM;
    /// # Modèle linéaire expliquant l’indice de récolte
    Var IRs;
    /// Capture de INN a un instant donne (ex INNF1)
    Var photo_INN_CROISSANCEACTIVE_A_FLORAISON;
    /// Capture de TDM a un instant donne (ex TDMF1)
    Var photo_TDM_CROISSANCEACTIVE_A_FLORAISON;
    /// Capture de IRs a un instant donne (ex IR)
    Var photo_IRs_aFinMATURATION;
    /// Capture de RDT a un instant donne
    Var photo_RDT_aFinMATURATION;


    ElaborationRendement(const vle::devs::DynamicsInit& model,
            const vle::devs::InitEventList& events) :
                DiscreteTimeDyn(model, events)
    {
        pr.initialiser( events );
        pv.initialiser( events );


        PhasePhenoPlante.init(this, "PhasePhenoPlante", events);
        TT_F1.init(this, "TT_F1", events);
        TDM.init(this, "TDM", events);
        ETRETM.init(this, "ETRETM", events);
        TRPF.init(this, "TRPF", events);
        INN.init(this, "INN", events);
        RDT.init(this, "RDT" , events);
        JSE.init(this, "JSE" , events);
        JSF.init(this, "JSF" , events);
        JSM.init(this, "JSM" , events);
        IRs.init(this, "IRs" , events);
        photo_INN_CROISSANCEACTIVE_A_FLORAISON.init(this, "photo_INN_CROISSANCEACTIVE_A_FLORAISON" , events);
        photo_TDM_CROISSANCEACTIVE_A_FLORAISON.init(this, "photo_TDM_CROISSANCEACTIVE_A_FLORAISON" , events);
        photo_IRs_aFinMATURATION.init(this, "photo_IRs_aFinMATURATION" , events);
        photo_RDT_aFinMATURATION.init(this, "photo_RDT_aFinMATURATION" , events);
        first_compute = true;
    }

    virtual ~ElaborationRendement() { }

    /**********************************************************************//**
     *
     * Traitement effectue a chaque pas de temps.
     *
     * Description de la facon dont le code ModelMaker de depart a ete interprete, traduit sous VLE : voir TRADUCTION_DE_MODELMAKER_EN_VLE dans la classe de documentation DOCsunfloV1.
     *
     **************************************************************************/
    virtual void compute(const vle::devs::Time& /*time*/)
    {
        if (first_compute) {
            first_compute = false;
            JSE = 0.0; //codeMMpourMemo Initial Value = 0.0
            JSF = 0.0; //codeMMpourMemo Initial Value = 0.0
            JSM = 0.0; //codeMMpourMemo Initial Value = 0.0
            photo_TDM_CROISSANCEACTIVE_A_FLORAISON = 0.0; //codeMMpourMemo  TDMF1 = 0
            photo_INN_CROISSANCEACTIVE_A_FLORAISON = 0.0; //codeMMpourMemo  INNF1 = 0
            IRs = 0.0;
            photo_IRs_aFinMATURATION = 0.0;
            {
                double RDT_tmp = 0.0;
                if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION )
                        && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 >= date_TT_M3
                    RDT_tmp = TDM() * photo_IRs_aFinMATURATION() / 10.0;
                } else {
                    RDT_tmp = TDM() * IRs() / 10.0;
                }
                RDT = RDT_tmp;
            }
            photo_RDT_aFinMATURATION = 0.0;
        } else {

            {
                double ddt = 0.0;

                if ( ETRETM() > pr.SeuilETRETM ){
                    ddt = 0.0;
                } else if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_FLORAISON)
                        && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 > date_TT_F1
                    ddt = 0.0;
                } else if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_CROISSANCEACTIVE)
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 < date_TT_E1
                    ddt = 0.0;
                } else {
                    ddt = 1.0;
                }
                JSE = JSE(-1) + ddt;
            }

            {
                double ddt = 0.0;

                if ( ETRETM() > pr.SeuilETRETM ){
                    ddt = 0.0;

                } else if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_MATURATION)
                        && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 > date_TT_M0
                    ddt = 0.0;
                } else if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_FLORAISON)
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 < date_TT_F1
                    ddt = 0.0;
                } else {
                    ddt = 1.0;
                }
                JSF = JSF(-1) + ddt;
            }

            {
                double ddt = 0.0;

                if ( ETRETM() > pr.SeuilETRETM ){
                    ddt = 0.0;

                } else if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION)
                        && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 > date_TT_M3
                    ddt = 0.0;
                } else if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_MATURATION)
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 < date_TT_M0
                    ddt = 0.0;
                } else {
                    ddt = 1.0;
                }
                JSM = JSM(-1) + ddt;
            }

            if ((PhasePhenoPlante(-1) == PHASEPHENOPLANTE_CROISSANCEACTIVE) and
                    (PhasePhenoPlante() == PHASEPHENOPLANTE_FLORAISON)){
                photo_TDM_CROISSANCEACTIVE_A_FLORAISON = TDM(-1); // derniere valeur en CROISSANCEACTIVE
                photo_INN_CROISSANCEACTIVE_A_FLORAISON = INN(-1); // derniere valeur en CROISSANCEACTIVE
            } else if ( PhasePhenoPlante() >= PHASEPHENOPLANTE_FLORAISON ){
                photo_TDM_CROISSANCEACTIVE_A_FLORAISON = photo_TDM_CROISSANCEACTIVE_A_FLORAISON(-1);
                photo_INN_CROISSANCEACTIVE_A_FLORAISON = photo_INN_CROISSANCEACTIVE_A_FLORAISON(-1);
            } else {
                photo_TDM_CROISSANCEACTIVE_A_FLORAISON = photo_TDM_CROISSANCEACTIVE_A_FLORAISON(-1);
                photo_INN_CROISSANCEACTIVE_A_FLORAISON = photo_INN_CROISSANCEACTIVE_A_FLORAISON(-1);
            }

            {
                double IRs_tmp = 0.0;

                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_FLORAISON)
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 < date_TT_F1
                    IRs_tmp = 0.0;
                } else {
                    IRs_tmp = 9.370e-02 + (-1.552e-04) * photo_TDM_CROISSANCEACTIVE_A_FLORAISON()
                                    + (-2.828e-03) * JSE() + (-2.557e-03) * JSF()
                                    + (-1.940e-03) * JSM() + (-3.907e-04) * TRPF()
                                    + 1.274e-04 * TT_F1() + 8.189e-01 * pv.IRg;
                }
                IRs = IRs_tmp;
            }

            if ((PhasePhenoPlante(-1) == PHASEPHENOPLANTE_MATURATION) and
                    (PhasePhenoPlante() == PHASEPHENOPLANTE_DESSICATION)){

                photo_IRs_aFinMATURATION = IRs(-1); // derniere valeur en MATURATION et non pas 1ere en DESSICATION

            } else if ((PhasePhenoPlante(-1) == PHASEPHENOPLANTE_MATURATION) and
                    (PhasePhenoPlante() == PHASEPHENOPLANTE_RECOLTEE)){

                photo_IRs_aFinMATURATION = IRs(-1); // derniere valeur en MATURATION et non pas 1ere en RECOLTEE

            } else if ( PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION ){
                photo_IRs_aFinMATURATION = photo_IRs_aFinMATURATION(-1);
            } else {
                photo_IRs_aFinMATURATION = photo_IRs_aFinMATURATION(-1);
            }

            {
                double RDT_tmp = 0.0;

                if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION )
                        && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 >= date_TT_M3
                    RDT_tmp = TDM() * photo_IRs_aFinMATURATION() / 10.0;
                } else {
                    double dI = IRs() - IRs(-1);
                    double dT = TDM() - TDM(-1);
                    //RDT_tmp = TDM() * IRs() / 10.0;
                    RDT_tmp = RDT(-1) + (TDM(-1)*dI + IRs(-1)*dT +dI*dT)/ 10.0;
                }
                RDT = RDT_tmp;

                if ((PhasePhenoPlante(-1) == PHASEPHENOPLANTE_MATURATION) and
                        (PhasePhenoPlante() == PHASEPHENOPLANTE_DESSICATION)){

                    photo_RDT_aFinMATURATION = RDT(-1); // derniere valeur en MATURATION et non pas 1ere en DESSICATION
                } else if ((PhasePhenoPlante(-1) == PHASEPHENOPLANTE_MATURATION) and
                        (PhasePhenoPlante() == PHASEPHENOPLANTE_RECOLTEE)){

                    photo_RDT_aFinMATURATION = RDT(-1); // derniere valeur en MATURATION et non pas 1ere en RECOLTEE
                } else {
                    photo_RDT_aFinMATURATION = photo_RDT_aFinMATURATION(-1);
                }
            }
        }
    }
};

} // namespace sunflo

DECLARE_DYNAMICS(sunflo::ElaborationRendement);

