@AbapCatalog.sqlViewName: 'ZMMV_RELST_002'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'FRGCO bazında alanlar'
define view zmmv_relst_frgco 
  with parameters
    iv_werks : werks_d,
    iv_Datum : datum
  as 
  select from 
    t16fd
    left outer join t16fw on
      t16fw.frggr = t16fd.frggr and
      t16fw.frgco = t16fd.frgco and
      t16fw.werks = $parameters.iv_werks
    left outer join hrp1000 on
      hrp1000.plvar = '01' and
      hrp1000.otype = t16fw.otype and
      hrp1000.objid = t16fw.objid and
      hrp1000.istat = '1' and
      hrp1000.langu = $session.system_language
    left outer join hrp1001 on
      hrp1001.otype = hrp1000.otype and
      hrp1001.objid = hrp1000.objid and
      hrp1001.plvar = hrp1000.plvar and
      hrp1001.rsign = 'A' and
      hrp1001.relat = '008' and
      hrp1001.istat = '1'
  {
    t16fd.frggr,
    t16fd.frgco,
    t16fd.frgct,
    t16fw.otype,
    t16fw.objid,
    hrp1000.stext,
    hrp1001.sobid
  }
  where
    t16fd.spras = $session.system_language and
    (
      hrp1000.begda is null or
      (
        hrp1000.begda is not null and
        hrp1000.begda <= $parameters.iv_Datum and
        hrp1000.endda >= $parameters.iv_datum
      )
    ) and
    (
      hrp1001.begda is null or
      (
        hrp1001.begda is not null and
        hrp1001.begda <= $parameters.iv_Datum and
        hrp1001.endda >= $parameters.iv_datum
      )
    ) 