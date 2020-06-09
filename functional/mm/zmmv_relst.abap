@AbapCatalog.sqlViewName: 'ZMMV_RELST_001'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Onay stratejisi listesi'
define view ZMMV_RELST 
  with parameters
    iv_werks : werks_d,
    iv_Datum : datum
  as 
  select from 
    t16fs 
    left outer join t16ft on
      t16ft.spras = $session.system_language and
      t16ft.frggr = t16fs.frggr and
      t16ft.frgsx = t16fs.frgsx
      
    left outer join zmmv_relst_frgco( 
        iv_werks: $parameters.iv_werks, 
        iv_Datum: $parameters.iv_datum
      ) as zfrgco1 on
      zfrgco1.frggr = t16fs.frggr and
      zfrgco1.frgco = t16fs.frgc1

    left outer join zmmv_relst_frgco( 
        iv_werks: $parameters.iv_werks, 
        iv_Datum: $parameters.iv_datum
      ) as zfrgco2 on
      zfrgco2.frggr = t16fs.frggr and
      zfrgco2.frgco = t16fs.frgc2

    left outer join zmmv_relst_frgco( 
        iv_werks: $parameters.iv_werks, 
        iv_Datum: $parameters.iv_datum
      ) as zfrgco3 on
      zfrgco3.frggr = t16fs.frggr and
      zfrgco3.frgco = t16fs.frgc3

    left outer join zmmv_relst_frgco( 
        iv_werks: $parameters.iv_werks, 
        iv_Datum: $parameters.iv_datum
      ) as zfrgco4 on
      zfrgco4.frggr = t16fs.frggr and
      zfrgco4.frgco = t16fs.frgc4

    left outer join zmmv_relst_frgco( 
        iv_werks: $parameters.iv_werks, 
        iv_Datum: $parameters.iv_datum
      ) as zfrgco5 on
      zfrgco5.frggr = t16fs.frggr and
      zfrgco5.frgco = t16fs.frgc5

    left outer join zmmv_relst_frgco( 
        iv_werks: $parameters.iv_werks, 
        iv_Datum: $parameters.iv_datum
      ) as zfrgco6 on
      zfrgco6.frggr = t16fs.frggr and
      zfrgco6.frgco = t16fs.frgc6

    left outer join zmmv_relst_frgco( 
        iv_werks: $parameters.iv_werks, 
        iv_Datum: $parameters.iv_datum
      ) as zfrgco7 on
      zfrgco7.frggr = t16fs.frggr and
      zfrgco7.frgco = t16fs.frgc7

    left outer join zmmv_relst_frgco( 
        iv_werks: $parameters.iv_werks, 
        iv_Datum: $parameters.iv_datum
      ) as zfrgco8 on
      zfrgco8.frggr = t16fs.frggr and
      zfrgco8.frgco = t16fs.frgc8

  {
    key t16fs.frggr,
    key t16fs.frgsx,
    t16fs.frgc1,
    t16fs.frgc2,
    t16fs.frgc3,
    t16fs.frgc4,
    t16fs.frgc5,
    t16fs.frgc6,
    t16fs.frgc7,
    t16fs.frgc8,
    t16ft.frgxt,
    zfrgco1.frgct as frgc1_t,
    zfrgco1.otype as frgc1_otype,
    zfrgco1.objid as frgc1_objid,
    zfrgco1.stext as frgc1_stext,
    zfrgco1.sobid as frgc1_Sobid,
    zfrgco2.frgct as frgc2_t,
    zfrgco2.otype as frgc2_otype,
    zfrgco2.objid as frgc2_objid,
    zfrgco2.stext as frgc2_stext,
    zfrgco2.sobid as frgc2_Sobid,
    zfrgco3.frgct as frgc3_t,
    zfrgco3.otype as frgc3_otype,
    zfrgco3.objid as frgc3_objid,
    zfrgco3.stext as frgc3_stext,
    zfrgco3.sobid as frgc3_Sobid,
    zfrgco4.frgct as frgc4_t,
    zfrgco4.otype as frgc4_otype,
    zfrgco4.objid as frgc4_objid,
    zfrgco4.stext as frgc4_stext,
    zfrgco4.sobid as frgc4_Sobid,
    zfrgco5.frgct as frgc5_t,
    zfrgco5.otype as frgc5_otype,
    zfrgco5.objid as frgc5_objid,
    zfrgco5.stext as frgc5_stext,
    zfrgco5.sobid as frgc5_Sobid,
    zfrgco6.frgct as frgc6_t,
    zfrgco6.otype as frgc6_otype,
    zfrgco6.objid as frgc6_objid,
    zfrgco6.stext as frgc6_stext,
    zfrgco6.sobid as frgc6_Sobid,
    zfrgco7.frgct as frgc7_t,
    zfrgco7.otype as frgc7_otype,
    zfrgco7.objid as frgc7_objid,
    zfrgco7.stext as frgc7_stext,
    zfrgco7.sobid as frgc7_Sobid,
    zfrgco8.frgct as frgc8_t,
    zfrgco8.otype as frgc8_otype,
    zfrgco8.objid as frgc8_objid,
    zfrgco8.stext as frgc8_stext,
    zfrgco8.sobid as frgc8_Sobid
  }
