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
#include <ParametresSol.hpp>
#include <ParametresSimuInit.hpp>

#include <vle/utils/DateTime.hpp>
#include <vle/utils/Exception.hpp>
#include <iomanip>

namespace sunflo {

using namespace vle::discrete_time;

class ContrainteAzote : public vle::discrete_time::DiscreteTimeDyn
{


public :

    ParametresPlante pp;
    ParametresSol ps;

    // provenant de phenologie
    /*Sync*/ Var PhasePhenoPlante;

    // provenant de temperature_efficace
    /*Sync*/ Var Teff;

    // provenant de croissance_plante
    /*Nosync*/ Var Ei;
    /*Nosync*/ Var DBP;
    /*Nosync*/ Var Ebp;
    /*Nosync*/ Var TDM;

    // provenant de contrainte_temperature
    /*Sync*/ Var FT;
    /*Sync*/ Var FTHN;

    // provenant de contrainte_eau
    /*Sync*/ Var vRac;
    /*Sync*/ Var STOCKC1;
    /*Sync*/ Var STOCKC2;
    /*Sync*/ Var STOCKC3;
    /*Sync*/ Var FHRUE;
    /*Sync*/ Var D1;
    /*Sync*/ Var D2;
    /*Sync*/ Var D3;
    /*Sync*/ Var RWCC1;
    /*Sync*/ Var vTRC1;
    /*Sync*/ Var vTRC2;
    /*Sync*/ Var zC2;
    /*Sync*/ Var zC3;
    /*Sync*/ Var zRac;

    // provenant de sunflo_climat
    /*Sync*/ Var Pluie;
    /*Sync*/ Var RG;
    /*Sync*/ Var Tmoy;

    // provenant de sunflo_itk
    /*Sync*/ Var ActionIrrig; ///< prend valeur dans ValeurActionConduite
    /*Sync*/ Var DoseIrrig;
    /*Sync*/ Var ActionFerti; ///< prend valeur dans ValeurActionConduite
    /*Sync*/ Var DoseFerti;

    //variables d'etats (sortie)
    Var FNLE;
    Var FNIRUE;
    Var INN;
    Var vNabs;
    Var Nabs;
    Var vMine;

    //variables d'etats (internes)
    Var CCN;
    Var CGR;
    //mineral nitrogen content of soil layers (kg.ha^(-1)): var NC1 eq 21.
    Var N1;
    Var N2;
    Var N3;
    Var CN1;
    Var CN2;
    Var CN3;
    Var CRU;
    Var Engrais;
    Var CRUm;
    Var DDM;
    Var Ea;
    Var F1;
    Var F2;
    Var F3;
    Var FHN;
    Var vMF1;
    Var vMF2;
    Var vMF;
    Var TNpmax;
    Var Nmax;
    Var vAA1;
    Var vAA2;
    Var vAA;
    Var vNA;
    Var vNAc;
    Var INNI;
    Var TNpcrit;
    Var Ncrit;
    Var FE;
    Var FR;
    Var Ndenit;
    Var Nmine;
    Var Nsol;
    Var TNp;
    Var vDenit;

    bool first_compute;

    /**********************************************************************//**
     * @brief Equation de calcul de teneur en N critique.
     * Il s'agit de l'equation EQ29 de la publi
     **************************************************************************/
    double equationTeneurNcritique( double PNC_a, double PNC_b, double TDM )
    {
        return ( PNC_a * std::pow( (TDM/100.0), (-PNC_b) ) );
    }

    /**********************************************************************//**
     * @brief des controles/verifications effectues en debut de compute
     * TODO toremove
     **************************************************************************/
    bool desControles( void ){

        bool cr = true; // par defaut

//        if ((int) ActionFerti() == ACTIONCONDUITE_ACTIVE ){
//            if ( DoseFerti() <= 0.0 ){
//                cr = false;
//            }
//        }
//        if ((int) ActionIrrig() == ACTIONCONDUITE_ACTIVE ){
//            if ( DoseIrrig() <= 0.0 ){
//                cr = false;
//
//            }
//        }
        return cr;
    }

    /**********************************************************************//**
     *
     * Construction, configuration, initialisation du modele ContrainteAzote
     *
     **************************************************************************/
    ContrainteAzote(const vle::devs::DynamicsInit& model,
            const vle::devs::InitEventList& events) :
                vle::discrete_time::DiscreteTimeDyn(model, events)
    {

        pp.initialiser(events);
        ps.initialiser(events);

        Teff.init(this, "Teff", events);
        PhasePhenoPlante.init(this, "PhasePhenoPlante", events);
        Ei.init(this, "Ei", events);
        DBP.init(this, "DBP", events);
        Ebp.init(this, "Ebp", events);
        TDM.init(this, "TDM", events);
        FT.init(this, "FT", events);
        FTHN.init(this, "FTHN", events);
        vRac.init(this, "vRac", events);
        STOCKC1.init(this, "STOCKC1", events);
        STOCKC2.init(this, "STOCKC2", events);
        STOCKC3.init(this, "STOCKC3", events);
        FHRUE.init(this, "FHRUE", events);
        D1.init(this, "D1", events);
        D2.init(this, "D2", events);
        D3.init(this, "D3", events);
        RWCC1.init(this, "RWCC1", events);
        vTRC1.init(this, "vTRC1", events);
        vTRC2.init(this, "vTRC2", events);
        zC2.init(this, "zC2", events);
        zC3.init(this, "zC3", events);
        zRac.init(this, "zRac", events);
        Pluie.init(this, "Pluie", events);
        RG.init(this, "RG", events);
        Tmoy.init(this, "Tmoy", events);
        ActionIrrig.init(this, "ActionIrrig", events);
        DoseIrrig.init(this, "DoseIrrig", events);
        ActionFerti.init(this, "ActionFerti", events);
        DoseFerti.init(this, "DoseFerti", events);
        FNLE.init(this, "FNLE", events);
        FNIRUE.init(this, "FNIRUE", events);
        INN.init(this, "INN", events);
        vNabs.init(this, "vNabs", events);
        Nabs.init(this, "Nabs", events);
        vMine.init(this, "vMine", events);
        CCN.init(this, "CCN", events);
        CGR.init(this, "CGR", events);
        N1.init(this, "N1", events);
        N2.init(this, "N2", events);
        N3.init(this, "N3", events);
        CN1.init(this, "CN1", events);
        CN2.init(this, "CN2", events);
        CN3.init(this, "CN3", events);
        CRU.init(this, "CRU", events);
        Engrais.init(this, "Engrais", events);
        CRUm.init(this, "CRUm", events);
        DDM.init(this, "DDM", events);
        Ea.init(this, "Ea", events);
        F1.init(this, "F1", events);
        F2.init(this, "F2", events);
        F3.init(this, "F3", events);
        FHN.init(this, "FHN", events);
        vMF1.init(this, "vMF1", events);
        vMF2.init(this, "vMF2", events);
        vMF.init(this, "vMF", events);
        TNpmax.init(this, "TNpmax", events);
        Nmax.init(this, "Nmax", events);
        vAA1.init(this, "vAA1", events);
        vAA2.init(this, "vAA2", events);
        vAA.init(this, "vAA", events);
        vNA.init(this, "vNA", events);
        vNAc.init(this, "vNAc", events);
        INNI.init(this, "INNI", events);
        TNpcrit.init(this, "TNpcrit", events);
        Ncrit.init(this, "Ncrit", events);
        FE.init(this, "FE", events);
        FR.init(this, "FR", events);
        Ndenit.init(this, "Ndenit", events);
        Nmine.init(this, "Nmine", events);
        Nsol.init(this, "Nsol", events);
        TNp.init(this, "TNp", events);
        vDenit.init(this, "vDenit", events);

        first_compute = true;
    }

    virtual ~ContrainteAzote() { }

    virtual void compute(const vle::devs::Time& /*time*/)
    {
        if (first_compute) {
            first_compute = false;
            //Overwrite initialization
            TDM =0.0001;

            N1 = N1(-1);//no evolution at first day
            N2 = 0.0;
            N3 = N3(-1);;//no evolution at first day
            {
                double FHN_tmp = 0.0;
                FHN_tmp = 1.0 - ( 1.0 - ps.Fpf ) * ( 1.0 - RWCC1());
                if ( FHN_tmp < 0.0 ){
                    FHN_tmp = 0.0;
                }
                FHN = FHN_tmp;
            }
            vNabs = 0.0;
            vMine = ps.Vp * FHN() * FTHN();
            vDenit = 6.0 * exp( 0.07738 * /*replace Tmoy() by 0*/0 - 6.593);
            FE = 0.0;
            FR = 0.0;
            {
                double CN1_tmp = 0.0;

                if ( STOCKC1() == 0.0 ){
                    //TODO pb synchro?
                    /*throw "todo";*/
                } else {
                    CN1_tmp = N1() / STOCKC1();
                    if ( CN1_tmp < 0.0 ){
                        CN1_tmp = 0.0;
                    }
                }
                CN1 = CN1_tmp;
            }

            {
                double CN2_tmp = 0.0;
                if ( STOCKC2() == 0.0 ){
                    //TODO pb synchro?
                    /*throw "todo";*/
                } else {
                    CN2_tmp = N2() / STOCKC2();
                    if ( CN2_tmp < 0.0 ){
                        CN2_tmp = 0.0;
                    }
                }
                CN2 = CN2_tmp;
            }
            {
                double CN3_tmp = 0.0;
                if ( STOCKC3() == 0.0 ){
                    //TODO pb synchro?
                    /*throw "todo";*/
                } else {
                    CN3_tmp = N3() / STOCKC3();
                    if ( CN3_tmp < 0.0 ){
                        CN3_tmp = 0.0;
                    }
                }
                CN3 = CN3_tmp;
            }

            F1 = D1() * CN1();
            F2 = D2() * CN2();
            F3 = D3() * CN3();
            Engrais = 0.0;
            Ndenit = 0.0;
            Nmine = 0.0;

            Nabs = 0.0;
            {
                double Ea_tmp = 0.0;
                if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION )
                        && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                    Ea_tmp = 0.0;
                } else if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_MATURATION )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                    Ea_tmp = 1.0;
                } else {
                    Ea_tmp = 1.0;
                }
                Ea = Ea_tmp;
            }
            {
                double vMF1_tmp = 0.0; double vMF2_tmp = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    vMF1_tmp = 0.0;
                    vMF2_tmp = 0.0;
                } else {
                    vMF1_tmp = vTRC1() * CN1() * Ea();
                    vMF2_tmp = vTRC2() * CN2() * Ea();
                }
                vMF1 = vMF1_tmp;
                vMF2 = vMF2_tmp;
            }
            vMF = vMF1() + vMF2();
            vAA1 = 0.0;
            vAA2 = 0.0;

            vAA = vAA1() + vAA2();
            {
                double TNpcrit_tmp = 0.0;

                if ( TDM() < pp.TDMc_seuil ){
                    TNpcrit_tmp = equationTeneurNcritique( pp.PNCc_a, pp.PNCc_b, pp.TDMc_seuil );
                } else {
                    TNpcrit_tmp = equationTeneurNcritique( pp.PNCc_a, pp.PNCc_b, TDM() );
                }
                TNpcrit = TNpcrit_tmp;
            }

            Ncrit = TNpcrit() / 100.0  * TDM() * 10.0;

            {
                double INN_tmp = 0.0;
                if (Ncrit() == 0.0 ){
                    //TODO pb synchro?
                    /*throw "todo";*/
                } else {
                    INN_tmp = Nabs() / Ncrit();
                    if ( INN_tmp > 1.0 ){
                        INN_tmp = 1.0;
                    }
                }
                INN = INN_tmp;
            }
            CCN = 0.0;
        } else {
            if ( not desControles() ){
                throw vle::utils::ModellingError(" erreur ContrainteAzote ");
            }
            // Traitement  traitementNcontentOfSoilLayers
            // inclus dans le paragraphe "2.2.4. Nitrogen" de la publi
            {
                double ddt = 0.0; double N1_tmp = 0.0;
                ddt = vMine(-1) + FR(-1) - vMF1(-1) - vAA1(-1) - F1(-1) - vDenit(-1);
                N1_tmp = N1(-1) + ddt;
                if ( N1_tmp < 0.0 ){
                    N1_tmp = 0.0;
                }
                N1 = N1_tmp;
            }

            {
                double ddt = 0.0;
                if ( zC3() == 0.0 ){
                    throw vle::utils::ModellingError(" div by 0: zC3");
                } else {
                    ddt = F1(-1) - F2(-1) - vMF2(-1) - vAA2(-1) + vRac() * ( N3(-1) / zC3() );
                }
                N2 = N2(-1) + ddt;
            }

            {
                double ddt = 0.0;
                if ( zC3() == 0.0 ){
                    throw "erreur";
                } else {
                    ddt = F2(-1) - F3(-1) - vRac() * ( N3(-1) / zC3() );
                }
                N3 = N3(-1) + ddt;
            }

            {
                double CGR_tmp = 0.0;
                if ( Teff() == 0.0 ){
                    CGR_tmp = 0.0;
                } else {
                    CGR_tmp = DBP(-1) /Teff();
                }
                CGR = CGR_tmp;
            }

            CRU = 30.0 + 0.34 * CGR() * 100.0;

            {
                double FHN_tmp = 0.0;
                FHN_tmp = 1.0 - ( 1.0 - ps.Fpf ) * ( 1.0 - RWCC1() );
                if ( FHN_tmp < 0.0 ){
                    FHN_tmp = 0.0;
                }
                FHN = FHN_tmp;
            }

            vMine = ps.Vp * FHN() * FTHN();
            vDenit = 6.0 * std::exp( 0.07738 * Tmoy() - 6.593 );


            {
                double FE_tmp = FE(-1);
                double FR_tmp = FR(-1);
                if ((int) ActionFerti() == 1 ){
                    FE_tmp = DoseFerti();
                }
                double pluieEtIrrig_tmp = Pluie();
                if ((int) ActionIrrig() == 1 ){
                    pluieEtIrrig_tmp = pluieEtIrrig_tmp + DoseIrrig();
                }
                if ( pluieEtIrrig_tmp > 5.0 ) {
                    FR_tmp = FE_tmp * CRU() / 100.0;
                    FE_tmp = 0.0;
                } else {
                    FR_tmp = 0.0;
                }
                FE = FE_tmp;
                FR = FR_tmp;
            }

            {
                double CN1_tmp = 0.0;
                if ( STOCKC1() == 0.0 ){
                    throw "erreur";
                } else {
                    CN1_tmp = N1() / STOCKC1();
                    if ( CN1_tmp < 0.0 ){
                        CN1_tmp = 0.0;
                    }
                }
                CN1 = CN1_tmp;
            }

            {
                double CN2_tmp = 0.0;
                if ( STOCKC2() == 0.0 ){
                    throw vle::utils::ModellingError(" div by 0: STOCKC2");
                } else {
                    CN2_tmp = N2() / STOCKC2();
                    if ( CN2_tmp < 0.0 ){
                        CN2_tmp = 0.0;
                    }
                }
                CN2 = CN2_tmp;
            }

            {
                double CN3_tmp = 0.0;
                if ( STOCKC3() == 0.0 ){
                    throw vle::utils::ModellingError(" div by 0: STOCKC3");
                } else {
                    CN3_tmp = N3() / STOCKC3();
                    if ( CN3_tmp < 0.0 ){
                        CN3_tmp = 0.0;
                    }
                }
                CN3 = CN3_tmp;
            }

            F1 = D1() * CN1();
            F2 = D2() * CN2();
            F3 = D3() * CN3();

            {
                double ddt = 0.0;
                if ((int) ActionFerti() == 1 ){
                    ddt = DoseFerti();
                }
                Engrais = Engrais(-1) + ddt;
            }

            Nsol = N1() + N2();
            {
                double ddt = vDenit(-1);
                Ndenit = Ndenit(-1) + ddt;
            }

            {
                double ddt = vMine(-1);
                Nmine = Nmine(-1) + ddt;
            }

            //Traitement  traitementPlantNabsorption
            //inclus dans le paragraphe "2.2.4. Nitrogen" de la publi
            vNabs = vMF(-1) + vAA(-1);
            Nabs = Nabs(-1) + vNabs();

            {
                double Ea_tmp = 0.0;
                if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION )
                        && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                    Ea_tmp = 0.0;
                } else if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_MATURATION )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                    Ea_tmp = 1.0;
                } else {
                    Ea_tmp = 1.0;
                }
                Ea = Ea_tmp;
            }
            {
                double vMF1_tmp = 0.0; double vMF2_tmp = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    vMF1_tmp = 0.0;
                    vMF2_tmp = 0.0;
                } else {
                    vMF1_tmp = vTRC1() * CN1() * Ea();
                    vMF2_tmp = vTRC2() * CN2() * Ea();
                }
                vMF1 = vMF1_tmp;
                vMF2 = vMF2_tmp;
            }

            vMF = vMF1() + vMF2();

            {
                double TNpmax_tmp = 0.0;
                if ( TDM(-1) < pp.TDMm_seuil ){
                    TNpmax_tmp = equationTeneurNcritique( pp.PNCm_a, pp.PNCm_b, pp.TDMm_seuil );
                } else {
                    TNpmax_tmp = equationTeneurNcritique( pp.PNCm_a, pp.PNCm_b, TDM(-1) );
                }
                TNpmax = TNpmax_tmp;
            }

            Nmax = ( TNpmax() / 100.0 ) * TDM(-1) * 10.0;

            {
                double vAA1_tmp = 0.0; double vAA2_tmp = 0.0;
                if ( Nabs() > Nmax() ){
                    vAA1_tmp = 0.0;
                    vAA2_tmp = 0.0;
                } else if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    vAA1_tmp = 0.0;
                    vAA2_tmp = 0.0;
                } else if ( zRac() > 300.0 ){
                    double p1_tmp = CN1() * 100.0 * 31.0;
                    vAA1_tmp = ( (0.0018 * p1_tmp) / (50.0 + p1_tmp) )
                             + ( (0.05 * p1_tmp) / (25000.0 + p1_tmp) ) * 24.0 * pp.AA_a * Ea() * ps.zC1;
                    double p2_tmp = CN2() * 100.0 * 31.0;
                    vAA2_tmp = ( (0.0018 * p2_tmp) / (50.0 + p2_tmp) )
                             + ( (0.05 * p2_tmp) / (25000.0 + p2_tmp)) * 24.0 * zC2() * pp.AA_a * Ea();
                } else {
                    double p1_tmp = CN1() * 100.0 * 31.0;
                    vAA1_tmp = ( (0.0018 * p1_tmp) / (50.0 + p1_tmp) )
                             + ( (0.05 * p1_tmp) / (25000.0 + p1_tmp)) * 24.0 * pp.AA_a * Ea() * zRac();
                    vAA2_tmp = 0.0;
                }
                vAA1 = vAA1_tmp;
                vAA2 = vAA2_tmp;
            }

            vAA = vAA1() + vAA2();

            {
                double TNp_tmp = 0.0;

                TNp_tmp = ( Nabs() / 10.0 / TDM(-1) ) * 100.0;
                if ( TNp_tmp > TNpmax() ){
                    TNp_tmp = TNpmax();
                } else if ( TNp_tmp < 0.0 ){
                    TNp_tmp = 0.0;
                }
                TNp = TNp_tmp;
            }

            {
                double CRUm_tmp = 0.0;
                if ( Nabs() <= 0.0 ){
                    CRUm_tmp = 0.0;
                } else {
                    CRUm_tmp = ( Engrais() / Nabs() ) * 100.0;
                }
                CRUm = CRUm_tmp;
            }
            //Traitement traitementStressN
            //inclus dans le paragraphe "2.2.4. Nitrogen" de la publi
            {
                double vNA_tmp = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    vNA_tmp = 0.0;
                } else {
                    vNA_tmp = vMF() + vAA();
                }
                vNA = vNA_tmp;
            }

            DDM = 0.48 * RG() * Ei(-1) * Ebp(-1) * FHRUE() * FT();
            {
                double vNAc_tmp = 0.0;
                if ( DDM() <= 0.0 ){
                    vNAc_tmp = 1.0;
                } else {

                    double nc = 5.0 * std::pow( (DDM()/100.0), (-0.5) );
                    if ( nc > 5.0 ){
                        nc = 5.0;
                    }

                    vNAc_tmp = nc * DDM() / 100.0;
                    if ( vNAc_tmp < 0.01 ){
                        vNAc_tmp = 1.0;
                    }
                }
                vNAc = vNAc_tmp;
            }

            {
                double INNI_tmp = 0.0;
                if ( vNAc() == 0.0 ){
                    throw vle::utils::ModellingError(" div by 0 ");
                } else {
                    INNI_tmp = vNA() / vNAc();
                    if ( INNI_tmp > 1.0 ){
                        INNI_tmp = 1.0;
                    }
                }
                INNI = INNI_tmp;
            }

            {
                double FNIRUE_tmp = 0.0;

                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    FNIRUE_tmp = 1.0;
                } else {
                    FNIRUE_tmp = INNI();
                    if ( FNIRUE_tmp > 1.0 ){
                        FNIRUE_tmp = 1.0;
                    }
                }
                FNIRUE = FNIRUE_tmp;
            }

            {
                double TNpcrit_tmp = 0.0;
                if ( TDM(-1) < pp.TDMc_seuil ){
                    TNpcrit_tmp = equationTeneurNcritique( pp.PNCc_a, pp.PNCc_b, pp.TDMc_seuil );
                } else {
                    TNpcrit_tmp = equationTeneurNcritique( pp.PNCc_a, pp.PNCc_b, TDM(-1) );
                }
                TNpcrit = TNpcrit_tmp;
            }
            Ncrit = ( TNpcrit() / 100.0 ) * TDM(-1) * 10.0;
            {
                double INN_tmp = 0.0;

                if ( Ncrit() == 0.0 ){
                    //TODO pb synchro?
                    /*throw "todo";*/
                } else {
                    INN_tmp = Nabs() / Ncrit();
                }
                if (INN_tmp > 2.0) {
                    INN = 2.0;
                } else {
                    INN = INN_tmp;
                }

            }
            {
                double FNLE_tmp = 0.0;
                if ( INN() < pp.INNseuil ){
                    FNLE_tmp = pp.FNLEm;
                } else if ( ( 1.75 * INN() - 0.75 ) > 1.0 ){
                    FNLE_tmp = 1.0;
                } else {
                    FNLE_tmp = 1.75 * INN() - 0.75;
                }
                FNLE = FNLE_tmp;
            }
            {
                double ddt = 0.0;

                if ( INN(-1) > 1.0 ){
                    ddt = 0.0;
                } else if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_FLORAISON )
                        && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                    // traduit condition TT_A2 > date_TT_F1
                    ddt = 0.0;
                } else if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    // traduit condition TT_A2 <= 0
                    ddt = 0.0;
                } else {
                    ddt = 1.0 - INN(-1);
                }
                CCN = CCN(-1) + ddt;
            }
        }
    }

};

} // namespace

DECLARE_DYNAMICS(sunflo::ContrainteAzote);

