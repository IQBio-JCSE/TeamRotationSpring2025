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

namespace sunflo {

using namespace vle::discrete_time;

class ElaborationQualite : public DiscreteTimeDyn
{
public :

    ParametresVariete pv;
    double densite;
    bool first_compute;

    /*Sync*/ Var PhasePhenoPlante;
    /*Sync*/ Var Tx;
    /*Sync*/ Var Eb;
    /*Sync*/ Var LAI;
    /*Sync*/ Var FTSW;
    /*Sync*/ Var JSM;
    /*Sync*/ Var iPAR;
    /*Sync*/ Var INN;
    /*Sync*/ Var vNabs;

    /// Teneur en huile (0%), modèle linéaire [Casadebaig2008]
    Var TH;
    // duration of reproductive phase
    Var D_MH;
    // sum of RUE on reproductive phase
    Var SRUE_MH;

    // Water deficit
    Var SFTSW_FIM;
    Var SFTSW_MH;

    // Nitrogen deficit
    Var SNNIE_FIH;
    Var NAB_MH;
    // Thermal stress
    Var NHT34_MH;
    // Crop functionning
    Var LAD_MH;
    Var MRUE_MH;
    /// Capture de TH a un instant donne
    Var photo_TH_aFinMATURATION;




    ElaborationQualite(const vle::devs::DynamicsInit& model,
            const vle::devs::InitEventList& events) :
                DiscreteTimeDyn(model, events)
    {
        pv.initialiser(events);
        densite = events.getDouble("densite");
        first_compute = true;
        PhasePhenoPlante.init(this, "PhasePhenoPlante", events);
        Tx.init(this, "Tx", events);
        Eb.init(this, "Eb", events);
        LAI.init(this, "LAI", events);
        FTSW.init(this, "FTSW", events);
        JSM.init(this, "JSM", events);
        iPAR.init(this, "iPAR", events);
        INN.init(this, "INN", events);
        vNabs.init(this, "vNabs", events);

        TH.init(this, "TH" , events);
        D_MH.init(this, "D_MH" , events);
        SRUE_MH.init(this, "SRUE_MH" , events);
        SFTSW_FIM.init(this, "SFTSW_FIM" , events);
        SFTSW_MH.init(this, "SFTSW_MH" , events);
        SNNIE_FIH.init(this, "SNNIE_FIH" , events);
        NAB_MH.init(this, "NAB_MH" , events);
        NHT34_MH.init(this, "NHT34_MH.init" , events);
        LAD_MH.init(this, "LAD_MH" , events);
        MRUE_MH.init(this, "MRUE_MH" , events);
        photo_TH_aFinMATURATION.init(this, "photo_TH_aFinMATURATION" , events);
    }

    virtual ~ElaborationQualite() { }

    virtual void compute(const vle::devs::Time& /*time*/)
    {
        if (first_compute) {
            first_compute = false;
            TH = 0.0;
            D_MH = 0.0;
            SRUE_MH = 0.0;
            SFTSW_FIM = 0.0;
            SFTSW_MH = 0.0;
            SNNIE_FIH = 0.0;
            NAB_MH = 0.0;
            NHT34_MH = 0.0;
            LAD_MH = 0.0;
            MRUE_MH = 0.0;
            photo_TH_aFinMATURATION = 0.0;
        } else {
            // calcul de D_MH
            {
                double ddt = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_MATURATION )
                        // traduit condition TT_A2 < date_TT_M0
                        || ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE )){
                    // traduit condition TT_A2 = 0
                    ddt = 0.0;
                } else {
                    ddt = 1;
                }
                D_MH = D_MH(-1) + ddt;
            }

            // calcul de SRUE_MH
            {
                double ddt = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_MATURATION )
                        // traduit condition TT_A2 < date_TT_M0
                        || ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    // traduit condition TT_A2 = 0
                    ddt = 0.0;
                } else {
                    ddt = Eb();
                }
                SRUE_MH = SRUE_MH(-1) + ddt;
            }

            // calcul de MRUE_MH
            MRUE_MH = SRUE_MH() / D_MH();

            // calcul de NHT34_MH
            {
                double ddt = 0.0;
                if ( Tx() < 34.0 ){
                    ddt = 0.0;
                } else if (( PhasePhenoPlante() < PHASEPHENOPLANTE_MATURATION )
                        // traduit condition TT_A2 < date_TT_M0
                        || ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE )){
                    // traduit condition TT_A2 > date_TT_M3
                    ddt = 0.0;
                } else {
                    ddt = 1.0;
                }

                NHT34_MH = NHT34_MH(-1) + ddt;
            }


            // calcul de SFTSW_FIM
            {
                double ddt = 0.0;
                if ( ( PhasePhenoPlante() > PHASEPHENOPLANTE_FLORAISON )
                        // traduit condition TT_A2 > date_TT_M0
                        || ( PhasePhenoPlante() <= PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE )){
                    // traduit condition TT_A2 = 0
                    ddt = 0.0;
                } else {
                    ddt = 1.0 - FTSW();
                }
                SFTSW_FIM = SFTSW_FIM(-1) + ddt;
            }

            // calcul de SFTSW_MH
            {
                double ddt = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_MATURATION )
                        // traduit condition TT_A2 < date_TT_M0
                        || ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE )){
                    // traduit condition TT_A2 = 0
                    ddt = 0.0;
                } else {
                    ddt = 1.0 - FTSW();
                }
                SFTSW_MH = SFTSW_MH(-1) + ddt;
            }

            // calcul de SNNIE_FIH
            {
                double ddt = 0.0;
                if ( INN() <= 1.0 ){
                    ddt = 0.0;
                } else if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_CROISSANCEACTIVE )
                        // traduit condition TT_A2 < date_TT_E1
                        || ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE )){
                    // traduit condition TT_A2 = 0
                    ddt = 0.0;

                } else {
                    ddt = INN() - 1.0 ;
                }
                SNNIE_FIH = SNNIE_FIH(-1) + ddt;
            }

            // calcul de NAB_MH
            {
                double ddt = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_MATURATION )
                        // traduit condition TT_A2 < date_TT_M0
                        || ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    // traduit condition TT_A2 = 0
                    ddt = 0.0;
                } else {
                    ddt = vNabs();
                }
                NAB_MH = NAB_MH(-1) + ddt;
            }


              // calcul de LAD_MH
            {
                double ddt = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_MATURATION )
                        // traduit condition TT_A2 < date_TT_M0
                        || ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    // traduit condition TT_A2 = 0
                    ddt = 0.0;
                } else {
                    ddt = LAI();
                }
                LAD_MH = LAD_MH(-1) + ddt;
            }

            {
                double TH_tmp = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_FLORAISON )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                    TH_tmp = 0.0;
                } else {
                    TH_tmp = -18.70 + 0.996 * pv.thp + 0.111 * SFTSW_FIM()
                             + 0.126 * SFTSW_MH() - 0.068 * SNNIE_FIH()
                             - 0.036 * NAB_MH() - 0.236 * NHT34_MH()
                             + 0.007 * LAD_MH() + 21.053 * MRUE_MH()
                             + 0.832 * densite;
                }
                TH = TH_tmp;

                if ((PhasePhenoPlante(-1) == PHASEPHENOPLANTE_MATURATION) and
                        (PhasePhenoPlante() == PHASEPHENOPLANTE_DESSICATION)){
                    photo_TH_aFinMATURATION = TH(-1);
                } else if ((PhasePhenoPlante(-1) == PHASEPHENOPLANTE_MATURATION) and
                        (PhasePhenoPlante() == PHASEPHENOPLANTE_RECOLTEE)){
                    photo_TH_aFinMATURATION = TH(-1);
                } else {
                    photo_TH_aFinMATURATION = photo_TH_aFinMATURATION(-1);
                }
            }
        }
    }
};
} // namespace sunflo

DECLARE_DYNAMICS(sunflo::ElaborationQualite);

