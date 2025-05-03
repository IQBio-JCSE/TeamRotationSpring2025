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
#include <ParametresSol.hpp>
#include <ParametresSimuInit.hpp>

#define VALEURNONSIGNIFIANTE_ETRETM (0)

namespace sunflo {

using namespace vle::discrete_time;

class ContrainteEau  : public DiscreteTimeDyn
{
public :
    ParametresPlante pp;
    ParametresVariete pv;
    ParametresSol ps;

    /// Parametre interne \n
    ///# Profondeur sur laquelle on effectue le bilan hydrique (mm)
    //codeMMpourMemo variable: zBilan Unconditional Universal
    //codeMMpourMemo zBilan = min(zRac_max, profondeur)
    double zBilan;

    bool first_compute;


    /*Sync*/ Var PhasePhenoPlante;
    /*Nosync*/ Var Ei;
    /*Sync*/ Var Pluie;
    /*Sync*/ Var ETPP;
    /*Sync*/ Var Tmoy;
    /*Sync*/ Var ActionIrrig; ///< prend valeur dans ValeurActionConduite
    /*Sync*/ Var DoseIrrig;
    /*Sync*/ Var ActionSemis; ///< prend valeur dans ValeurActionConduite
    /*Sync*/ Var zSemis;

    /// # Facteur de stress multiplicatif [0-1] : effet contrainte hydrique sur la transpiration
    Var FHTR;
    /// # Facteur de stress multiplicatif [0-1] : effet contrainte hydrique sur l'expansion foliaire
    Var FHLE;
    /// # Facteur de stress multiplicatif [0-1] : effet contrainte hydrique sur la photosynthèse (Eb) : courbe de réponse en décalage depuis celle de la transpiration (paramètre a_PHO)
    Var FHRUE;
    /// # Transpiration cumulée entre la floraison et la maturité (covariable statistique)
    Var TRPF;
    /// # Profondeur du front racinaire (mm)
    Var zRac;
    /// # Vitesse de progression du front racinaire (mm/j)
    Var vRac;
    /// # Profondeur de l’horizon racinaire
    Var zC2;
    /// # Profondeur de l’horizon sans racines (mm)
    Var zC3;
    /// # Stock d'eau (liée+utilisable) dans C1, C2, C3 (mm)
    Var STOCKC1;
    Var STOCKC2;
    Var STOCKC3;
    /// # Drainage de C1 à C2 (mm)
    Var D1;
    /// # Drainage de C2 à C3 (mm)
    Var D2;
    /// # Drainage en dessous du profil (mm)
    Var D3;
    // Réserve Utile (mm d'eau ) par mm de sol dans l'horizon de surface (C1)
    // equals the parameter in ParametersSol
    Var RUmmC1;
    // Réserve Utile (mm d'eau ) par mm de sol dans l'horizon de surface (C2)
    // equals the parameter in ParametersSol
    Var RUmmC2;

    /// # Teneur en eau relative dans l'horizon de surface. Utilisé pour la fonction de minéralisation.
    Var RWCC1;
    /// # Vitesse d'extraction d'eau par les racines dans C1 (mm/j)
    Var vTRC1;
    /// # Vitesse d'extraction d'eau par les racine dans C2 (mm/j)
    Var vTRC2;
    /// # Rapport ETR / ETM, indice de stress
    Var ETRETM;
    /// ETR cumule depuis la levee, calcule jusqu'a la recolte
    Var ETRcumulDepuisLevee;
    /// ETM cumule depuis la levee, calcule jusqu'a la recolte
    Var ETMcumulDepuisLevee;
    /// Pluie cumule depuis la levee, calcule jusqu'a la recolte
    Var PluieCumulDepuisLevee;
    /// ETP cumule depuis la levee, calcule jusqu'a la recolte
    Var ETPcumulDepuisLevee;
    /// Water content of soil layer 1 (0-30cm) : var WC1 eq 8
    Var C1;
    /// Water content of soil layer 2 (30cm - root front): var WC2 eq 9
    Var C2;
    /// Water content of soil layer 2 (root front  - max prof): var WC3 eq 10
    Var C3;
    /// # Jours Sans Eau, variable intervenant dans le calcul de l'évaporation
    Var JS;
    /// # Transpiration du couvert (mm)
    Var TR;
    /// # Vitesse de Transpiration (mm/j)
    Var vTR;
    /// # Vitesse d'évaporation du sol (mm/j)
    Var EVj;
    /// # Teneur en eau transpirable journalière du sol (Actual Transpirable Soil Water)
    Var ATSW;
    /// # Teneur en eau totale transpirable du sol (Total Transpirable Soil Water)
    Var TTSW;
    /// # Fraction d'eau transpirable du sol (Fraction of Transpirable Soil Water)
    Var FTSW;
    /// # Teneur en eau du compartiment C1, C2, C3 (mm/mm)
    Var TEC1;
    /// # Evapotranspiration (fichier meteo)
    Var ETP;
    /// # Vitesse de transpiration potentielle (mm/j)
    Var vTRp;
    /// # proportion du front racinaire représentée par zC1, utilisée pour partitionner la transpiration entre C1 et C2
    Var fRacC1;

    ContrainteEau(const vle::devs::DynamicsInit& model,
            const vle::devs::InitEventList& events) :
                DiscreteTimeDyn(model, events)
    {
        pp.initialiser( events );
        pv.initialiser( events );
        ps.initialiser( events );

        if ( pp.zRac_max <= ps.profondeur ){
            zBilan = pp.zRac_max;
        } else {
            zBilan = ps.profondeur;
        }
        first_compute = true;

        PhasePhenoPlante.init(this, "PhasePhenoPlante", events);
        Ei.init(this, "Ei", events);
        Pluie.init(this, "Pluie", events);
        ETPP.init(this, "ETPP", events);
        Tmoy.init(this, "Tmoy", events);
        ActionIrrig.init(this, "ActionIrrig", events);
        DoseIrrig.init(this, "DoseIrrig", events);
        ActionSemis.init(this, "ActionSemis", events);
        zSemis.init(this, "zSemis", events);
        FHTR.init(this,  "FHTR" , events);
        FHLE.init(this,  "FHLE" , events);
        FHRUE.init(this,  "FHRUE" , events);
        TRPF.init(this,  "TRPF" , events);
        zRac.init(this,  "zRac" , events);
        vRac.init(this,  "vRac" , events);
        zC2.init(this,  "zC2" , events);
        zC3.init(this,  "zC3" , events);
        STOCKC1.init(this,  "STOCKC1" , events);
        STOCKC2.init(this,  "STOCKC2" , events);
        STOCKC3.init(this,  "STOCKC3" , events);
        D1.init(this,  "D1" , events);
        D2.init(this,  "D2" , events);
        D3.init(this,  "D3" , events);
        RUmmC1.init(this,  "RUmmC1" , events);
        RUmmC2.init(this,  "RUmmC2" , events);
        RWCC1.init(this,  "RWCC1" , events);
        vTRC1.init(this,  "vTRC1" , events);
        vTRC2.init(this,  "vTRC2" , events);
        ETRETM.init(this,  "ETRETM" , events);
        ETRcumulDepuisLevee.init(this,  "ETRcumulDepuisLevee" , events);
        ETMcumulDepuisLevee.init(this,  "ETMcumulDepuisLevee" , events);
        PluieCumulDepuisLevee.init(this,  "PluieCumulDepuisLevee" , events);
        ETPcumulDepuisLevee.init(this,  "ETPcumulDepuisLevee" , events);
        C1.init(this,  "C1" , events);
        C2.init(this,  "C2" , events);
        C3.init(this,  "C3" , events);
        JS.init(this,  "JS" , events);
        TR.init(this,  "TR" , events);
        vTR.init(this,  "vTR" , events);
        EVj.init(this,  "EVj" , events);
        ATSW.init(this,  "ATSW" , events);
        TTSW.init(this,  "TTSW" , events);
        FTSW.init(this,  "FTSW" , events);
        TEC1.init(this,  "TEC1" , events);
        ETP.init(this,  "ETP" , events);
        vTRp.init(this,  "vTRp" , events);
        fRacC1.init(this,  "fRacC1" , events);

        RUmmC1.init_value(ps.RUmmC1);
        RUmmC2.init_value(ps.RUmmC2);

    }

    virtual ~ContrainteEau() { }

    virtual void compute(const vle::devs::Time& time )
    {
        if (first_compute) {
            first_compute = false;
            double Ei_valeurInitiale = 0.0;
            C1 = (ps.Hini_C1 * (ps.Hcc_C1 - ps.Hpf_C1)) / 100.0 * ps.da_C1 * ps.zC1  * ( 1.0 - ps.TC );
            C2 = 0;      //codeMMpourMemo Initial Value = 0.01
            C3 = (ps.Hini_C2 * (ps.Hcc_C2 - ps.Hpf_C2)) / 100.0 * ps.da_C2 * (zBilan - ps.zC1  ) * ( 1.0 - ps.TC );
            zRac = 0.0;     //codeMMpourMemo Initial Value = 0
            vRac = 0.0;

            zC2 = 1.0;
            {
                zC3 = std::max(1.0, zBilan - ps.zC1  - zC2());
            }
            {
                if ( C1() >= ( ps.zC1  * RUmmC1() ) ){
                    D1 = C1() - ( ps.zC1  * RUmmC1() ) ;
                } else {
                    D1 = 0.0;
                }
            }
            {

                if ( C2() >= ( zC2() * RUmmC2() ) ){
                    D2 = C2() - ( zC2() * RUmmC2() );
                } else {
                    D2 = 0.0;
                }
            }
            {
                if ( C3() > ( zC3() * RUmmC2() ) ){
                    D3 = C3() - ( zC3() * RUmmC2() );
                } else {
                    D3 = 0.0;
                }
            }
            {
                double STOCKC1_tmp = 0.0;

                STOCKC1_tmp = ( ( ps.Hpf_C1 / 100.0 ) * ps.da_C1 * ps.zC1  ) + C1();
                if ( STOCKC1_tmp <= 0.0 ){
                    STOCKC1_tmp = 0.1;
                }
                STOCKC1 = STOCKC1_tmp;
            }
            {
                double STOCKC2_tmp = 0.0;

                STOCKC2_tmp = ( ( ps.Hpf_C2 / 100.0 ) * ps.da_C2 * zC2() ) + C2();
                if ( STOCKC2_tmp <= 0.0 ){
                    STOCKC2_tmp = 0.1;
                }
                STOCKC2 = STOCKC2_tmp;
            }
            {
                double STOCKC3_tmp = 0.0;
                STOCKC3_tmp = ( ( ps.Hpf_C2 / 100.0 ) * ps.da_C2 * zC3() ) + C3();
                if ( STOCKC3_tmp <= 0.0 ){
                    STOCKC3_tmp = 0.1;
                }
                STOCKC3 = STOCKC3_tmp;
            }
            {
                TEC1 = STOCKC1() / ps.zC1 ;
            }

            {
                RWCC1 = ( TEC1() - ( ps.da_C1 * ps.Hpf_C1 / 100.0 ) ) / 
                    (ps.da_C1 * ( ps.Hcc_C1 - ps.Hpf_C1 ) / 100.0);
                
            }
            JS = 0.0; //codeMMpourMemo Initial Value = 0
            {
//                double ETP_tmp = 0.0;
//
//                ETP_tmp = ETPP();
//                if ( ETP_tmp == 0.0 ){
//                    ETP_tmp = 0.001;
//                }
//                ETP = ETP_tmp;
                ETP = 0.001;
            }
            {
                EVj = std::min(C1(), 
                    ETP()* ( pow((JS()+1.0),0.5) - pow(JS(),0.5) )
                               * ( 1.0 - Ei_valeurInitiale ));
            }
            traitementStressEau();

            vTRp = pp.Kc * ETP() * Ei_valeurInitiale;
            {
                fRacC1 = ps.zC1  / (ps.zC1  + zC2());
            }
            {
                if ( C1() <= 0.0 ){
                    vTRC1 = 0.0;
                } else if ( zRac() > ps.zC1  ){
                    vTRC1 = fRacC1() * vTRp() * FHTR();
                } else {
                    vTRC1 = vTRp() * FHTR();
                }
            }
            {
                if ( zRac() > ps.zC1  ){
                    vTRC2 = ( 1.0 - fRacC1() ) * vTRp() * FHTR();
                } else {
                    vTRC2= 0;    
                }
            }
            vTR = vTRC1() + vTRC2();
            TR = 0.001; //codeMMpourMemo Initial Value = 0.001
            TRPF = 0.001;   //codeMMpourMemo Initial Value = 0.001
            ETRETM = (double)VALEURNONSIGNIFIANTE_ETRETM;
            ETRcumulDepuisLevee = 0.0;
            ETMcumulDepuisLevee = 0.0;
            PluieCumulDepuisLevee = 0.0;
            ETPcumulDepuisLevee = 0.0;

        } else {

            traitementRootAndSoilLayers( time );
            traitementSoilEvaporation();
            traitementStressEau();
            traitementPlantTranspiration();
            traitementETRsurETM( time );
            calculCumulsPluieEtETP();
        }
    }


    bool traitementRootAndSoilLayers( const vle::devs::Time& /*time*/ ){

        // C1, EQ8 de la publi
        {
            // double ddt = 0.0;

            // ddt = Pluie() - vTRC1(-1) - EVj(-1) - D1(-1);
            // if ( (int)ActionIrrig() == 1 ){
            //     ddt = ddt + DoseIrrig();
            // }
            C1 = C1(-1) + Pluie() - vTRC1(-1) - EVj(-1) - D1(-1) + DoseIrrig();
        }

        // C2, EQ9 de la publi
        {
            if ( zC2(-1) == 1.0 ){
                C2 = C2(-1) + D1(-1) - D2(-1) - vTRC2(-1);
            } else {
                C2 = C2(-1) + D1(-1) - D2(-1) - vTRC2(-1) + vRac(-1) * ( C3(-1) / zC3(-1) );
            }
        }
        
        // C3, EQ10 de la publi
        {

            if ( zC2(-1) == 1.0 ){
                C3 = C3(-1) +  D2(-1) - D3(-1);
            } else {
                C3 = C3(-1) +  D2(-1) - D3(-1) - vRac(-1) * ( C3(-1) / zC3(-1) );
            }
        }

        // zRac, EQ7 de la publi
        if ( (int)ActionSemis() == 1 ){
            zRac = zSemis(); // initialisation
        } else {
            zRac = zRac(-1) + vRac(-1);
        }

        {

            if ( ( PhasePhenoPlante() <= PHASEPHENOPLANTE_NONSEMEE)
                    || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                // traduit condition t<=jsemis or t>=jrecolte
                vRac = 0.0;
            } else if ( Tmoy() <= 0.0 ){
                vRac = 0.0;
            } else if ( zRac() >=zBilan ){
                vRac = 0.0;
            } else {
                vRac = pp.VitCroiRac * Tmoy();
            }
        }

        {
            if ( ( PhasePhenoPlante() <= PHASEPHENOPLANTE_NONSEMEE )
                    || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                // traduit condition t < jsemis or t > jrecolte
                zC2 = 1.0;
            } else if ( zRac() < ps.zC1  ){
                zC2 = 1.0;
            } else if ( ( zRac() - ps.zC1  ) >zBilan ){
                zC2 =zBilan - 1.0;
            } else if ( zRac() >zBilan ){
                zC2 =zBilan - ps.zC1  - 1.0;
            } else {
                zC2 = zRac() - ps.zC1 ;
            }
        }

        {
            zC3 = std::max(1.0, zBilan - ps.zC1  - zC2());
        }

        {
            if ( C1() >= ( ps.zC1  * RUmmC1() ) ){
                D1 = C1() - ( ps.zC1  * RUmmC1() );
            } else {
                D1 = 0.0;
            }
        }

        {
            if ( C2() >= ( zC2() * RUmmC2() ) ){
                D2 = C2() - ( zC2() * RUmmC2() );
            } else {
                D2 = 0.0;
            }
        }

        {
            if ( C3() > ( zC3() * RUmmC2() ) ){
                D3 = C3() - ( zC3() * RUmmC2() );
            } else {
                D3 = 0.0;
            }
        }

        {
            double STOCKC1_tmp = 0.0;

            STOCKC1_tmp = ( ( ps.Hpf_C1 / 100.0 ) * ps.da_C1 * ps.zC1  ) + C1();
            if ( STOCKC1_tmp <= 0.0 ){
                STOCKC1_tmp = 0.1;
            }

            STOCKC1 = STOCKC1_tmp;
        }

        {
            double STOCKC2_tmp = 0.0;

            STOCKC2_tmp = ( ( ps.Hpf_C2 / 100.0 ) * ps.da_C2 * zC2() ) + C2();
            if ( STOCKC2_tmp <= 0.0 ){
                STOCKC2_tmp = 0.1;
            }
            STOCKC2 = STOCKC2_tmp;
        }

        {
            double STOCKC3_tmp = 0.0;

            STOCKC3_tmp = ( ( ps.Hpf_C2 / 100.0 ) * ps.da_C2 * zC3() ) + C3();
            if ( STOCKC3_tmp <= 0.0 ){
                STOCKC3_tmp = 0.1;
            }
            STOCKC3 = STOCKC3_tmp;
        }

        {
            TEC1 = STOCKC1() / ps.zC1 ;
        }

        {
            RWCC1 = ( TEC1() - ( ps.da_C1 * ps.Hpf_C1 / 100.0 ) ) /
                (ps.da_C1 * ( ps.Hcc_C1 - ps.Hpf_C1 ) / 100.0);
        }

        return true;
    }

    /**********************************************************************//**
     *
     * Traitement inclus dans le paragraphe "2.2.3. Water" de la publi
     *
     * Calcule JS, ETP, EVj
     *
     **************************************************************************/
    bool traitementSoilEvaporation( void ){

        {
            // pluieEtIrrig = Pluie + DoseIrrig
            double pluieEtIrrig_tmp = Pluie();
            if ((int)ActionIrrig() == 1 ){
                pluieEtIrrig_tmp = pluieEtIrrig_tmp + DoseIrrig();
            }
            if ( pluieEtIrrig_tmp > 4.0 ) { // pluieEtIrrig > 4 mm
                JS = 0.0; // raz
            } else {
                double ddt = 1.0;
                JS = JS(-1) + ddt;
            }
        }

        {
            double ETP_tmp = 0.0;

            ETP_tmp = ETPP();
            if ( ETP_tmp == 0.0 ){
                ETP_tmp = 0.001;
            }
            ETP = ETP_tmp;
        }

        // EVj, EQ12 de publi
        {
            EVj = std::min(C1(), 
                ETP()*( pow((JS()+1.0),0.5) - pow(JS(),0.5) )
                           * ( 1.0 - Ei(-1) ));
        }


        return true;
    }

    /**********************************************************************//**
     *
     * Traitement inclus dans le paragraphe "2.2.3. Water" de la publi
     *
     * Calcule ATSW, TTSW, FTSW
     * Calcule FHLE, FHRUE, FHTR
     *
     **************************************************************************/
    bool traitementStressEau( void ){


        {
            ATSW = std::max(0.0, C1() + C2());
        }

        TTSW = ( RUmmC1() * ps.zC1  ) + ( RUmmC2() * zC2() );

        // FTSW, EQ17 de la publi
        {
            if ( ( PhasePhenoPlante() <= PHASEPHENOPLANTE_NONSEMEE )
                    || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE ) ){
                FTSW = 0.0;
            } else if (ps.activate_WaterStress){
                FTSW = std::min(1.1, ATSW() / TTSW());
            } else {
                FTSW = 1;
            }
        }

        // FHLE, FHRUE, FHTR, EQ19 de la publi
        {
            if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE)
                    || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                // traduit condition TT_A2 = 0
                FHLE = 1.0;
            } else {
                FHLE = -1.0 + 2.0 / (1.0 + exp( pv.a_LE * FTSW() ) );
            }
        }

        {

            if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE)
                    || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                // traduit condition TT_A2 = 0
                FHRUE = 1.0;
            } else {
                FHRUE = -1.0 + 2.0 /( 1.0 + exp( ( pp.a_Pho + pv.a_TR ) * FTSW() ) );
            }
        }

        {
            
            if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_JUVENILE)
                    || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                // traduit condition TT_A2 = 0
                FHTR = 1.0;
            } else {
                FHTR = -1.0 + 2.0 / ( 1.0 + exp( pv.a_TR * FTSW() ) );
            }
        }

        return true;
    }


    /**********************************************************************//**
     *
     * Traitement inclus dans le paragraphe "2.2.3. Water" de la publi
     *
     * Calcule vTRp, fRacC1, vTRC1, vTRC2
     * Calcule vTR, TR, TRPF
     *
     **************************************************************************/
    bool traitementPlantTranspiration( void ){

        // vTRp, EQ13 de la publi
        vTRp = pp.Kc * ETP() * Ei(-1);

        // fRacC1_tmp, EQ14 de la publi
        {
            fRacC1 = ps.zC1 / (ps.zC1  + zC2());
        }

        // vTRC1, EQ15 de la publi
        {

            if ( C1() <= 0.0 ){
                vTRC1 = 0.0;
            } else if ( zRac() > ps.zC1  ){
                vTRC1 = fRacC1() * vTRp() * FHTR();
            } else {
                vTRC1 = vTRp() * FHTR();
            }
        }

        // vTRC2, EQ16 de la publi
        {
            if ( zRac() > ps.zC1  ){
                vTRC2 = ( 1.0 - fRacC1() ) * vTRp() * FHTR();
            } else {
                vTRC2 = 0;

            }
        }

        vTR = vTRC1() + vTRC2();

        {

            TR = TR(-1) + vTR(-1);
        }

        {
            if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_DESSICATION)
                    && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
                // traduit condition TT_A2 > date_TT_M3
                TRPF = TRPF(-1) +0.0;
            } else if ( ( PhasePhenoPlante() < PHASEPHENOPLANTE_FLORAISON)
                    || ( PhasePhenoPlante() >= PHASEPHENOPLANTE_RECOLTEE) ){
                // traduit condition TT_A2 < date_TT_F1
                TRPF = TRPF(-1) +0.0;
            } else {
                TRPF = TRPF(-1) + vTR(-1);
            }
        }

        return true;
    }

    bool traitementETRsurETM( const vle::devs::Time& /*time*/ ){
        double ETRETM_tmp = 0.0;
        double ETR_tmp = 0.0; double ETM_tmp = 0.0; // par defaut

        switch ( (int)PhasePhenoPlante() ){

        // avant levee ou a partir dessication
        case PHASEPHENOPLANTE_NONSEMEE :
        case PHASEPHENOPLANTE_GERMINATION :
        case PHASEPHENOPLANTE_DESSICATION :
        case PHASEPHENOPLANTE_RECOLTEE :

            ETRETM_tmp = (double)VALEURNONSIGNIFIANTE_ETRETM;
            break;

            // des levee et jusqu'a maturation incluse
        case PHASEPHENOPLANTE_JUVENILE :
        case PHASEPHENOPLANTE_CROISSANCEACTIVE :
        case PHASEPHENOPLANTE_FLORAISON :
        case PHASEPHENOPLANTE_MATURATION :

            if ( ( EVj() == 0.0 ) && (  Ei(-1) == 0.0 ) ){
                ETRETM_tmp = 0.0;

            } else {

                double div = EVj() + ( pp.Kc * ETP() * Ei(-1) );

                if ( div == 0.0 ){ // controle
                    ETRETM_tmp = 0.0;

                } else { // div non nul

                    ETR_tmp = EVj() + vTR();
                    ETM_tmp = div;

                    ETRETM_tmp = ETR_tmp / ETM_tmp;
                }
            }

            break;

        default :
            throw "erreur";

        } // switch ( PhasePhenoPlante() )

        ETRETM = ETRETM_tmp;
        ETRcumulDepuisLevee = ETRcumulDepuisLevee(-1) + ETR_tmp;
        ETMcumulDepuisLevee = ETMcumulDepuisLevee(-1) + ETM_tmp;


        return true;
    }

    /**********************************************************************//**
     *
     * Calcul des cumuls de Pluie et ETP depuis la levee,
     * ces cumuls sont calcules jusqu'a la recolte
     *
     **************************************************************************/
    bool calculCumulsPluieEtETP( void ){

        double Pluie_tmp = 0.0;
        double ETP_tmp = 0.0;

        if ( ( PhasePhenoPlante() >= PHASEPHENOPLANTE_JUVENILE)
                && ( PhasePhenoPlante() < PHASEPHENOPLANTE_RECOLTEE) ){
            Pluie_tmp = Pluie();
            ETP_tmp = ETPP();
        }
        PluieCumulDepuisLevee = PluieCumulDepuisLevee(-1) + Pluie_tmp;
        ETPcumulDepuisLevee = ETPcumulDepuisLevee(-1) + ETP_tmp;
        return true;
    }

};

} // namespace

DECLARE_DYNAMICS(sunflo::ContrainteEau);

