/*=====================================================================
  Labels and formats for the full analysis dataset (variables 1-271)

  Replace LIB.INDATA / LIB.OUTDATA with your own library and dataset.

  Labels are inferred from the variable names (Swedish National Patient
  Register, Total Population Register, SWEDEHEART/SCR, transplant and
  derived analysis variables). Lines marked VERIFY are guesses - check
  them against the data specification. Code values in the formats for
  register variables follow standard NPR coding; confirm with
  PROC FREQ before relying on them.
=====================================================================*/

/*---------------------------------------------------------------------
  1. Value formats
---------------------------------------------------------------------*/
proc format;
  /* Numeric 0/1 flags */
  value noyes
    0 = "No"
    1 = "Yes"
    . = "Missing";

  /* Character 0/1 flags                                       VERIFY */
  value $noyesc
    "0"   = "No"
    "1"   = "Yes"
    " "   = "Missing"
    other = "Other/unknown";

  /* Sex (KON) */
  value sexf
    1 = "Male"
    2 = "Female";

  /* Competing-risk status (*_COMP)                            VERIFY */
  value compf
    0 = "Censored"
    1 = "Event of interest"
    2 = "Competing event (death)"
    . = "Missing";

  /* NPR: admission type (INSATT)                              VERIFY */
  value insattf
    1 = "From other clinic, same hospital"
    2 = "From other hospital"
    3 = "From special housing"
    4 = "From ordinary housing";

  /* NPR: discharge type (UTSATT)                              VERIFY */
  value utsattf
    1 = "To other clinic, same hospital"
    2 = "To other hospital"
    3 = "To special housing"
    4 = "To ordinary housing"
    5 = "Deceased";

  /* NPR: planned care (PVARD)                                 VERIFY */
  value pvardf
    1 = "Unplanned (acute)"
    2 = "Planned";

  /* Marital status (CIVIL)                                    VERIFY */
  value $civilf
    "OG" = "Unmarried"
    "G"  = "Married"
    "S"  = "Divorced"
    "Ä"  = "Widowed"
    "RP" = "Registered partner"
    "SP" = "Divorced partner"
    "EP" = "Surviving partner";

  /* NYHA class                                                VERIFY */
  value $nyhaf
    "1" = "NYHA I"
    "2" = "NYHA II"
    "3" = "NYHA III"
    "4" = "NYHA IV";
run;

/*---------------------------------------------------------------------
  2. Macro that writes labels for numbered variable series
     e.g. %series(OPD, 1, 30, Procedure code with date)
     gives OPD1 = "Procedure code with date 1" ... OPD30
---------------------------------------------------------------------*/
%macro series(prefix, from, to, text);
  %do _i = &from %to &to;
    &prefix.&_i = "&text &_i"
  %end;
%mend series;

/*---------------------------------------------------------------------
  3. Labels and formats
---------------------------------------------------------------------*/
data LIB.OUTDATA;
  set LIB.INDATA;

  label
    /*--- Identification and demographics --------------------------*/
    hdia        = "Main diagnosis (ICD-10)"
    LopNr       = "Personal serial number (pseudonymised ID)"
    AR          = "Year of hospital stay"
    FODDAT      = "Date of birth"
    KON         = "Sex"
    ALDER       = "Age (years)"
    ALDER_S     = "Age at baseline (years)"                       /* VERIFY */
    CIVIL       = "Marital status"
    NATION      = "Citizenship"                                   /* VERIFY */
    FLAND       = "Country of birth"
    SENINV      = "Date of latest immigration"
    SENUTV      = "Date of latest emigration"

    /*--- Hospital stay (National Patient Register) ----------------*/
    LKF         = "County of residence"
    LKF_IN      = "County of residence at admission"              /* VERIFY */
    lk          = "County and municipality code"
    SJUKHUS     = "Hospital code"
    MVO         = "Medical specialty (MVO)"
    LT_KLIN     = "County council, clinic"                        /* VERIFY */
    LT_IN       = "County council at admission"                   /* VERIFY */
    AVTAL       = "Care under agreement (contract care)"
    INDATUMA    = "Date of admission"
    UTDATUMA    = "Date of discharge"
    VTID        = "Length of stay (days)"
    INSATT      = "Admission type"
    UTSATT      = "Discharge type"
    PVARD       = "Planned care"
    ANE1        = "Anaesthesia code 1"
    ANE2        = "Anaesthesia code 2"
    MDC         = "Major diagnostic category (MDC)"
    DRG         = "Diagnosis-related group (DRG)"
    RTC         = "DRG grouper return code"
    DIA_ANT     = "Number of diagnoses"
    OP_ANT      = "Number of procedures"

    %series(OPD, 1, 30, Procedure code with date)
    %series(DIA, 1, 30, Diagnosis code (ICD-10))
    %series(op,  1, 30, Procedure code (KVÅ))

    /*--- Cohort definition ----------------------------------------*/
    Diagnose          = "Diagnosis group"
    Reg               = "Data source (register)"                  /* VERIFY */
    BASELINE_DATE     = "Baseline date"
    IMPUTED_BASELINE  = "Baseline date imputed"
    excl_age          = "Excluded: age criterion"
    excl_date         = "Excluded: date criterion"
    excl_firstLVAD    = "Excluded: not first LVAD"
    excl_explant      = "Excluded: explant"
    FAS               = "Full analysis set"
    excl_SCR          = "Excluded: SCR criterion"                 /* VERIFY */
    FAS_SCR           = "Full analysis set, SCR cohort"           /* VERIFY */
    BASELINE_OUTDATE  = "Discharge date of baseline stay"
    BASELINE_INDATE   = "Admission date of baseline stay"
    OPDATUM           = "Date of operation"

    /*--- Preoperative status (cardiac surgery register) -----------*/
    HEIGHT                    = "Height (cm)"
    WEIGHT                    = "Weight (kg)"
    BMI                       = "Body mass index (kg/m2)"
    DIALYS                    = "Preoperative dialysis"
    CIRKULATIONSSTOD_PRE      = "Preoperative circulatory support"
    AORTABALLONGPUMP_PRE      = "Preoperative intra-aortic balloon pump"
    ECMO_PRE                  = "Preoperative ECMO"
    CIRKUNDERSTOD_ANNAT_PRE   = "Preoperative other circulatory support"
    TIDPCI                    = "Previous PCI"
    TIDTAVI                   = "Previous TAVI"
    TIDSTROKE                 = "Previous stroke"
    DIABETES                  = "Diabetes"
    DIABETESINSULIN           = "Diabetes treated with insulin"
    DIABETESPERORAL           = "Diabetes treated with oral agents"
    DIABETES_SUBCUT           = "Diabetes treated with non-insulin injectables"
    HYPERTENSION              = "Hypertension"
    SMOKING                   = "Smoking status"
    PREVAF                    = "Previous atrial fibrillation"
    FRAILTY                   = "Frailty"

    /*--- EuroSCORE components -------------------------------------*/
    EURO_VK_EJFRACT2   = "EuroSCORE II: LV ejection fraction category"
    EURO_HJINF         = "EuroSCORE II: recent myocardial infarction"
    EURO_PULMONCHOICE  = "EuroSCORE II: pulmonary hypertension"
    EURO_URGENCY       = "EuroSCORE II: urgency"
    EURO_ANNANOP       = "EuroSCORE II: weight of intervention"     /* VERIFY */
    EURO_THORAKAL      = "EuroSCORE II: surgery on thoracic aorta"
    EURO_POSTINFARKT   = "EuroSCORE: post-infarct septal rupture"   /* VERIFY */
    EURO_CCS4          = "EuroSCORE II: CCS class 4 angina"
    EURO_NYHA          = "EuroSCORE II: NYHA class"

    /*--- Transplant / VAD register --------------------------------*/
    TRANSPL_HOSPITAL             = "Transplant hospital"
    TRANSPL_DIAGNOSIS            = "Transplant: underlying diagnosis"
    TRANSPL_OPERATIONTYPE        = "Transplant: operation type"
    TRANSPL_LISTINGDAYS          = "Transplant: days on waiting list"
    TRANSPL_CARELEVEL            = "Transplant: level of care at operation"
    TRANSPL_RESPIRATOR           = "Transplant: on ventilator"
    TRANSPL_INTROPIN             = "Transplant: on inotropes"
    TRANSPL_PVR                  = "Transplant: pulmonary vascular resistance"
    TRANSPL_BLOODTYPE            = "Transplant: blood type"
    TRANSPL_TRANSPLANTEDBEFORE   = "Transplant: previously transplanted"
    TRANSPL_VADBEFORE            = "Transplant: previous VAD"
    TRANSPL_MANUFACTUREREXPL     = "Transplant: manufacturer of explanted device"
    TRANSPL_WAITINGLISTSTATUS    = "Transplant: waiting list status"
    TRANSPL_INTERMACS            = "Transplant: INTERMACS profile"
    TRANSPL_CVP                  = "Transplant: central venous pressure"
    TRANSPL_TAPSE                = "Transplant: TAPSE"
    TRANSPL_MACHINESUPPORT       = "Transplant: mechanical circulatory support"
    TRANSPL_INDICATION           = "Transplant/VAD: indication"
    TRANSPL_VADTYPE              = "VAD type"
    TRANSPL_MANUFACTURERIMPL     = "Manufacturer of implanted device"

    %series(OPCODE,              1, 8, Operation code)
    %series(INOTROP_BEHANDLING,  1, 4, Inotropic treatment)

    /*--- Preoperative labs and region -----------------------------*/
    HEMOGLOBIN_PREOP  = "Preoperative hemoglobin (g/L)"
    BNP_PREOP         = "Preoperative BNP (ng/L)"
    NT_PRO_BNP_PREOP  = "Preoperative NT-proBNP (ng/L)"
    region            = "Region"
    EOS_Date          = "End of study date"

    /*--- Baseline comorbidities -----------------------------------*/
    bl_IHD             = "Baseline: ischemic heart disease"
    bl_HYPERTENSION    = "Baseline: hypertension"
    bl_DIABETES        = "Baseline: diabetes mellitus"
    bl_ISCHEMIC_STROKE = "Baseline: ischemic stroke"
    bl_AF_FLUTTER      = "Baseline: atrial fibrillation/flutter"
    bl_HEART_FAILURE   = "Baseline: heart failure"
    bl_DCM             = "Baseline: dilated cardiomyopathy"
    bl_HCM             = "Baseline: hypertrophic cardiomyopathy"
    bl_RCM             = "Baseline: restrictive cardiomyopathy"
    bl_PERIPARTUM_CM   = "Baseline: peripartum cardiomyopathy"
    bl_OTHER_CM        = "Baseline: other cardiomyopathy"
    bl_PACEMAKER       = "Baseline: pacemaker"
    bl_ICD_DEVICE      = "Baseline: ICD"
    bl_CANCER          = "Baseline: cancer"

    /*--- Death, emigration, explant, transplant -------------------*/
    Death_Dt    = "Date of death"
    ULORSAK     = "Underlying cause of death (ICD-10)"
    ev_do       = "Death indicator (registry)"                    /* VERIFY */
    Imig_Dt     = "Emigration indicator"                          /* VERIFY */
    EXPLANT_Dt  = "Date of device explant"
    HTX_Dt      = "Date of heart transplantation"

    /*--- All-cause death ------------------------------------------*/
    CENSOR_DATE     = "Censoring date"
    EV_DEATH        = "All-cause death"
    EV_DEATH_DAYS   = "Time to all-cause death or censoring (days)"
    EV_DEATH_YEARS  = "Time to all-cause death or censoring (years)"
    END_DATE        = "End of follow-up date"
    EV_DEATH_COMP   = "All-cause death, competing-risk status"

    STAY_LENGTH       = "Length of hospital stay (days)"
    STAY_LENGTH_LVAD  = "Length of stay after LVAD implantation (days)"

    /*--- Heart transplantation ------------------------------------*/
    EV_HTX_DAYS   = "Time to heart transplantation or censoring (days)"
    EV_HTX_YEARS  = "Time to heart transplantation or censoring (years)"
    EV_HTX_COMP   = "Heart transplantation, competing-risk status"

    /*--- CV death and 30-day death --------------------------------*/
    EV_CVDEATH       = "Cardiovascular death"
    EV_DEATH30_DAYS  = "Time to death within 30 days (days)"
    EV_DEATH30       = "Death within 30 days"
    EV_DEATH30_COMP  = "Death within 30 days, competing-risk status"

    /*--- Death, IRR analysis                                VERIFY -*/
    CENSOR_DATE2        = "Censoring date (IRR analysis)"
    EV_DEATH_IRR        = "All-cause death (IRR analysis)"
    EV_DEATH_IRR_DAYS   = "Time to death or censoring, IRR analysis (days)"
    EV_DEATH_IRR_YEARS  = "Time to death or censoring, IRR analysis (years)"
    END_DATE2           = "End of follow-up date (IRR analysis)"
    EV_DEATH_IRR_COMP   = "All-cause death, IRR analysis, competing-risk status"

    /*--- Follow-up events -----------------------------------------*/
    EV_CEREBRAL_HEMO_DATE      = "Date of cerebral hemorrhage"
    EV_CEREBRAL_HEMO           = "Cerebral hemorrhage"
    EV_CEREBRAL_HEMO_DAYS      = "Time to cerebral hemorrhage or censoring (days)"
    EV_CEREBRAL_HEMO_YEARS     = "Time to cerebral hemorrhage or censoring (years)"

    EV_CEREBRAL_INFARCT_DATE   = "Date of cerebral infarction"
    EV_CEREBRAL_INFARCT        = "Cerebral infarction"
    EV_CEREBRAL_INFARCT_DAYS   = "Time to cerebral infarction or censoring (days)"
    EV_CEREBRAL_INFARCT_YEARS  = "Time to cerebral infarction or censoring (years)"

    EV_GI_BLEEDING_DATE        = "Date of gastrointestinal bleeding"
    EV_GI_BLEEDING             = "Gastrointestinal bleeding"
    EV_GI_BLEEDING_DAYS        = "Time to GI bleeding or censoring (days)"
    EV_GI_BLEEDING_YEARS       = "Time to GI bleeding or censoring (years)"

    EV_DEVICES_RELATED_COMPL_INFECT = "Device-related complication: infection"

    VAR257  = "Unknown variable 257"                              /* VERIFY */
    VAR258  = "Unknown variable 258"                              /* VERIFY */
    VAR259  = "Unknown variable 259"                              /* VERIFY */

    EV_DIALYS_DATE    = "Date of dialysis"
    EV_DIALYS         = "Dialysis"
    EV_DIALYS_DAYS    = "Time to dialysis or censoring (days)"
    EV_DIALYS_YEARS   = "Time to dialysis or censoring (years)"

    EV_CV_HOSP_DATE   = "Date of cardiovascular hospitalization"
    EV_CV_HOSP        = "Cardiovascular hospitalization"
    EV_CV_HOSP_DAYS   = "Time to CV hospitalization or censoring (days)"
    EV_CV_HOSP_YEARS  = "Time to CV hospitalization or censoring (years)"

    EV_HOSP_DATE      = "Date of hospitalization"
    EV_HOSP           = "All-cause hospitalization"
    EV_HOSP_DAYS      = "Time to hospitalization or censoring (days)"
    EV_HOSP_YEARS     = "Time to hospitalization or censoring (years)"
  ;

  format
    /* Register codes */
    KON                                                       sexf.
    INSATT                                                    insattf.
    UTSATT                                                    utsattf.
    PVARD                                                     pvardf.
    CIVIL                                                     $civilf.
    EURO_NYHA                                                 $nyhaf.

    /* Numeric dates */
    BASELINE_DATE EOS_Date CENSOR_DATE END_DATE
    CENSOR_DATE2 END_DATE2                                    yymmdd10.

    /* Numeric 0/1 flags */
    IMPUTED_BASELINE excl_age excl_date excl_firstLVAD excl_explant
    FAS excl_SCR FAS_SCR
    bl_IHD bl_HYPERTENSION bl_DIABETES bl_ISCHEMIC_STROKE
    bl_AF_FLUTTER bl_HEART_FAILURE bl_DCM bl_HCM bl_RCM
    bl_PERIPARTUM_CM bl_OTHER_CM bl_PACEMAKER bl_ICD_DEVICE bl_CANCER
    EV_DEATH EV_CVDEATH EV_DEATH30 EV_DEATH_IRR
    EV_CEREBRAL_HEMO EV_CEREBRAL_INFARCT EV_GI_BLEEDING
    EV_DIALYS EV_CV_HOSP EV_HOSP                              noyes.

    /* Competing-risk status */
    EV_DEATH_COMP EV_HTX_COMP EV_DEATH30_COMP
    EV_DEATH_IRR_COMP                                         compf.

    /* Character 0/1 flags                                     VERIFY */
    DIALYS CIRKULATIONSSTOD_PRE AORTABALLONGPUMP_PRE ECMO_PRE
    CIRKUNDERSTOD_ANNAT_PRE TIDPCI TIDTAVI TIDSTROKE
    DIABETES DIABETESINSULIN DIABETESPERORAL DIABETES_SUBCUT
    HYPERTENSION PREVAF
    EURO_HJINF EURO_THORAKAL EURO_POSTINFARKT EURO_CCS4
    TRANSPL_RESPIRATOR TRANSPL_INTROPIN
    TRANSPL_TRANSPLANTEDBEFORE TRANSPL_VADBEFORE
    ev_do EV_DEVICES_RELATED_COMPL_INFECT                     $noyesc.

    /* Follow-up time */
    EV_DEATH_DAYS EV_HTX_DAYS EV_DEATH30_DAYS EV_DEATH_IRR_DAYS
    EV_CEREBRAL_HEMO_DAYS EV_CEREBRAL_INFARCT_DAYS EV_GI_BLEEDING_DAYS
    EV_DIALYS_DAYS EV_CV_HOSP_DAYS EV_HOSP_DAYS               8.0
    EV_DEATH_YEARS EV_HTX_YEARS EV_DEATH_IRR_YEARS
    EV_CEREBRAL_HEMO_YEARS EV_CEREBRAL_INFARCT_YEARS EV_GI_BLEEDING_YEARS
    EV_DIALYS_YEARS EV_CV_HOSP_YEARS EV_HOSP_YEARS            8.2
  ;
run;

/*---------------------------------------------------------------------
  4. Check: confirm codes behind the formats before trusting them
---------------------------------------------------------------------*/
proc freq data=LIB.INDATA;
  tables KON INSATT UTSATT PVARD CIVIL EURO_NYHA
         DIALYS DIABETES SMOKING FRAILTY
         EV_DEATH_COMP EV_HTX_COMP ev_do / missing;
run;

proc contents data=LIB.OUTDATA varnum; run;
