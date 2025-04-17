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

namespace sunflo {

using namespace vle::discrete_time;

class CroissancePlante : public DiscreteTimeDyn
{
public :
    //parameters
    ParametresPlante pp;
    ParametresVariete pv;

    /// Parametre interne \n
    /// Paramètre (asymétrie) du profil de surface foliaire potentiel
    //codeMMpourMemo  variable: b Unconditional
    // equation b = 1.5 - 0.2210304 * bSF - 0.0003529 * cSF + 0.0825307 * TLN issue de
    // calages statistiques ou references de la biblio.
    double b;

    /// Parametre interne \n
    /// Paramètre (largeur) du profil de surface foliaire potentiel
    //codeMMpourMemo variable: a Unconditional
    // equation a = - 2.313409 + 0.018158 * bSF - 0.001637 * cSF + 0.019968 * TLN + 0.920874 * b issue de
    // calages statistiques ou references de la biblio.
    double a;

    /*Sync*/ Var RG;
    /*Sync*/ Var Teff;
    /*Sync*/ Var FT;
    /*Sync*/ Var FLe;
    /*Sync*/ Var FHLE;
    /*Sync*/ Var FHRUE;
    /*Sync*/ Var FNLE;
    /*Sync*/ Var FNIRUE;
    /*Sync*/ Var ActionSemis;
    /*Sync*/ Var densite;
    /*Sync*/ Var TT_A2;
    /*Sync*/ Var PhasePhenoPlante;

    Var Ei;
    Var DBP;
    Var Ebp;
    Var TDM;
    Var LAI;
    Var SFp;
    Var Eb;
    Var ddts;
    Var ddte;

    Vect Ae; // sera de taille 46 pour 45 elements utiles (indice 0 inutilise)
    Vect SFe;
    Vect SFs;
    Vect SF;
    Vect Ke;
    Vect Ti;
    Vect Te;
    Vect GRe;
    Vect As;
    Vect LL;
    Vect Ts;
    Vect GRs;
    double memo_densite;
    bool first_compute;


    CroissancePlante(const vle::devs::DynamicsInit& model,
            const vle::devs::InitEventList& events) :
                DiscreteTimeDyn(model, events)
    {

        pp.initialiser( events );
        pv.initialiser( events );
        a = -2.05;
        b = 0.049;

        RG.init(this, "RG", events);
        Teff.init(this, "Teff", events);
        FT.init(this, "FT", events);
        FLe.init(this, "FLe", events);
        FHLE.init(this, "FHLE", events);
        FHRUE.init(this, "FHRUE", events);
        FNLE.init(this, "FNLE", events);
        FNIRUE.init(this, "FNIRUE", events);
        ActionSemis.init(this, "ActionSemis", events);
        densite.init(this,  "densite" , events);
        TT_A2.init(this, "TT_A2", events);
        PhasePhenoPlante.init(this, "PhasePhenoPlante", events);

        Ei.init(this,  "Ei", events);
        DBP.init(this,  "DBP", events);
        Ebp.init(this,  "Ebp", events);
        TDM.init(this,  "TDM", events);
        LAI.init(this,  "LAI", events);
        SFp.init(this,  "SFp", events);
        ddts.init(this, "ddts", events);
        ddte.init(this, "ddte", events);
        Ae.init(this, "Ae", events);
        SFe.init(this, "SFe", events);
        SFs.init(this, "SFs", events);
        SF.init(this, "SF", events);
        Eb.init(this,  "Eb", events);
        Ke.init(this, "Ke", events);
        Ti.init(this, "Ti", events);
        Te.init(this, "Te", events);
        GRe.init(this, "GRe", events);
        As.init(this, "As", events);
        LL.init(this, "LL", events);
        Ts.init(this, "Ts", events);
        GRs.init(this, "GRs", events);

        Ae.dim(46);
        SFe.dim(46);
        SFs.dim(46);
        SF.dim(46);
        Ke.dim(46);
        Ti.dim(46);
        Te.dim(46);
        GRe.dim(46);
        As.dim(46);
        LL.dim(46);
        Ts.dim(46);
        GRs.dim(46);

        memo_densite = (double)VALEURDOUBLENONSIGNIFIANTE;
        first_compute = true;
    }

    virtual ~CroissancePlante() { }

    void traitementTimingOfLeafAppearanceAndSenescence()
    {
        for ( int i=1; i<46; i++){
            Ke[i] = pp.LAI_Kei;
        }
        for ( int i=1; i<46; i++){
            double Ti_tmp = 0.0;
            if ( i < 7 ){
                Ti_tmp = i * pp.Phy1;
            } else {
                Ti_tmp = (i - 5) * pp.Phy2 + pp.LAI_a;
            }
            Ti[i] = Ti_tmp;
        }

        if (PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE){
            for ( int i=1; i<46; i++){
                Te[i] = 0.0;
            }
        } else {
            for ( int i=1; i<46; i++){
                Te[i] = Ti[i]() + pp.LAI_b / Ke[i]();
            }
        }

        for ( int i=1; i<46; i++){
            LL[i] = pp.LAI_c + pp.LAI_d * exp(
                    - (std::pow(i-(pv.bSF-pp.LAI_e), 2)) /
                    (std::pow(pp.LAI_f * pv.TLN, 2)));
        }

        for ( int i=1; i<46; i++){
            Ts[i] = Te[i]() + LL[i]();
        }
    }

    virtual void compute(const vle::devs::Time& /*time*/)
    {
        if (first_compute) {

            first_compute = false;
            traitementTimingOfLeafAppearanceAndSenescence();

            for ( int i=1; i<46; i++){
                double Ae_tmp = 0.0;
                if ( i > pv.TLN ){
                    Ae_tmp = 0.0;
                } else {
                    Ae_tmp = pv.cSF
                            * exp(a*pow((i-pv.bSF)/(pv.bSF-1.0), 2)
                    + b*pow( (i-pv.bSF)/(pv.bSF-1.0), 3));
                }
                Ae[i] = Ae_tmp;
            }
            for ( int i=1; i<46; i++){
                GRe[i] = 0;
            }
            for ( int i=1; i<46; i++){
                SFe[i] = 0.0;
            }

            for ( int i=1; i<46; i++){
                double As_tmp = 0.0;
                if ( i > pv.TLN ){
                    As_tmp = 0.0;
                } else {
                    As_tmp = SFe[i]();
                }
                As[i] = As_tmp;
            }
            for ( int i=1; i<46; i++){
                GRs[i] = Teff() * (As[i]() * Ke[i]())
                      * exp(-Ke[i]()*(TT_A2()-Ts[i]()))
                      / pow(1+exp(-Ke[i]()*(TT_A2()-Ts[i]())), 2);
            }
            for ( int i=1; i<46; i++){
                SFs[i] = 0.0;
            }

            for ( int i=1; i<46; i++){
                SF[i] = SFe[i]() - SFs[i]();
            }
            {
                double SFp_tmp = 0.0;
                for ( int i=1; i<46; i++){
                    SFp_tmp = SFp_tmp + SF[i]();
                }
                SFp = SFp_tmp;
            }
            Ei = 0.0;
            Ebp = 0.0;
            Eb = 0.0;
            DBP = 0.48 * RG() * Ei() * Eb();
            TDM = 0.0001;
            LAI = 0.0;


        } else {

            if ( ( (int) PhasePhenoPlante(-1) == PHASEPHENOPLANTE_NONSEMEE)
                    && ( (int) ActionSemis() == 1)){
                memo_densite = densite();
            }
            traitementTimingOfLeafAppearanceAndSenescence();

            //traitementLeafExpansionAndSenescence
            for ( int i=1; i<46; i++){
                double Ae_tmp = 0.0;
                if ( i > pv.TLN ){
                    Ae_tmp = 0.0;
                } else {
                    Ae_tmp = pv.cSF * exp(
                            a * pow( (i-pv.bSF)/(pv.bSF-1.0),2)
                    + b * pow( (i-pv.bSF)/(pv.bSF-1.0),3));
                }
                Ae[i] = Ae_tmp;
            }

            for ( int i=1; i<46; i++){
                GRe[i] = Teff()*(Ae[i]()*Ke[i]())
                        * exp(-Ke[i]()*(TT_A2()-Te[i]()))
                        / pow(1.0+exp(-Ke[i]()*(TT_A2()-Te[i]())), 2);
            }

            {
                double ddt = 0.0;
                double ddte_tmp = 0.0;
                for ( int i=1; i<46; i++){
                    ddt = GRe[i](-1) * FHLE() * FNLE() * FLe();
                    SFe[i] = SFe[i](-1) + ddt;
                    ddte_tmp = ddte_tmp + ddt;
                }
                ddte = ddte_tmp;
            }

            for ( int i=1; i<46; i++){
                double As_tmp = 0.0;
                if ( i > pv.TLN ){
                    As_tmp = 0.0;
                } else {
                    As_tmp = SFe[i]();
                }
                As[i] = As_tmp;
            }

            for ( int i=1; i<46; i++){
                GRs[i] = Teff()*(As[i]()*Ke[i]())
                        *exp(-Ke[i]()*(TT_A2()-Ts[i]()))
                        / pow(1.0 + exp(-Ke[i]()*(TT_A2()-Ts[i]())), 2);
            }

            {
                double ddt = 0.0;
                double ddts_tmp=0.0;
                for ( int i=1; i<46; i++){
                    if ( (PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE)
                            || (PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE)
                            || ((PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION)
                                    && (PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE))){
                        ddt = 0.0;
                    } else if (SFs[i](-1) > SF[i](-1) ){
                        ddt = SF[i](-1);
                    } else {
                        ddt = GRs[i](-1);
                    }
                    SFs[i] = SFs[i](-1) + ddt;
                    ddts_tmp = ddts_tmp + ddt;
                }
                ddts = ddts_tmp;
            }

            for (int i=1; i<46; i++){
                SF[i] = SFe[i]() - SFs[i]();
            }

            {
                double SFp_tmp = 0.0;
                for ( int i=1; i<46; i++){
                    SFp_tmp = SFp_tmp + SF[i]();
                }
                SFp = SFp_tmp;
            }



            //traitementLightInterception
            {
                if ( (PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || (PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE  )
                        || ((PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION )
                                && (PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE))
                                || ( SFp() < 0 ) ){
                    LAI = 0.0;
                } else {
                    if ( memo_densite == (double)VALEURDOUBLENONSIGNIFIANTE ){
                        throw "erreur";
                    } else {
                        if((PhasePhenoPlante(-1) == PHASEPHENOPLANTE_GERMINATION) and
                                (PhasePhenoPlante() == PHASEPHENOPLANTE_JUVENILE)){
                            if (SFp() == 0) {
                                LAI = 0;
                            } else {
                                LAI = LAI(-1) + (SFp() /10000.0* memo_densite);
                            }

                        } else {
                            if (ddte() - ddts() == 0) {
                                LAI = 0.0;
                            } else {
                                LAI = LAI(-1)
                                   +((ddte()- ddts()) /10000.0* memo_densite);
                            }
                        }
                    }
                }
            }

            {
                double Ei_tmp = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    Ei_tmp = 0.0;
                } else {
                    Ei_tmp = 1.0 - exp( - pv.ext * LAI() );
                }
                Ei = Ei_tmp;
            }

            //traitementBiomassAccumulation
            {
                double Ebp_tmp = 0.0;
                if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    Ebp_tmp = 0.0;
                } else if (TT_A2() < 300.0 ){
                    Ebp_tmp = pp.Eb_0;
                } else if ((PhasePhenoPlante() < PHASEPHENOPLANTE_FLORAISON)
                        || (PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                    Ebp_tmp = pp.Eb_0 + ((TT_A2()-300) * 2 / (pv.date_TT_F1-300));
                } else if  (TT_A2() < (pv.date_TT_M0 - 100.0) ){
                    Ebp_tmp = pp.Eb_max;
                } else if ((PhasePhenoPlante() < PHASEPHENOPLANTE_DESSICATION )
                        || (PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                    Ebp_tmp = pp.Eb_fin * exp(pp.Eb_c *
                            (1-((TT_A2()-pv.date_TT_M0)
                                    /(pv.date_TT_M3-pv.date_TT_M0))));
                } else {
                    Ebp_tmp = 0.0;
                }
                Ebp = Ebp_tmp;
            }

            {
                double Eb_tmp = 0.0;
                if ((PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION )
                        && (PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                    Eb_tmp = 0.0;
                } else if ( (PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || (PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    Eb_tmp = 0.0;
                } else {
                    Eb_tmp = Ebp() * FHRUE() * FNIRUE() * FT() * pv.PHS;
                }

                Eb = Eb_tmp;
            }

            DBP = 0.48 * RG() * Ei() * Eb();

            {
                double ddt = 0.0;
                ddt = DBP(-1);
                if ( (PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE )
                        || (PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                    TDM = 0.0;
                } else {
                    TDM = TDM(-1) + ddt;
                }
            }

            //useless variables
            Ae[0] = 0;
            SFe[0] = 0;
            SFs[0] = 0;
            SF[0] = 0;
            Ke[0] = 0;
            Ti[0] = 0;
            Te[0] = 0;
            GRe[0] = 0;
            As[0] = 0;
            LL[0] = 0;
            Ts[0] = 0;
            GRs[0] = 0;

        }
    }
};

} // namespace

DECLARE_DYNAMICS(sunflo::CroissancePlante);
