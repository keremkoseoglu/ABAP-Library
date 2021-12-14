@AbapCatalog.sqlViewName: 'ZBCV_009'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Domain değerleri'
define view ZI_BC_DOMAIN_VALUE
  as select distinct from dd07l
  association [0..1] to dd07t on
    dd07t.domname    = $projection.domname      and
    dd07t.ddlanguage = $session.system_language and
    dd07t.as4local   = $projection.as4local     and
    dd07t.valpos     = $projection.valpos       and
    dd07t.as4vers    = $projection.as4vers {
  key domname,
  key as4local,
  key valpos,
  key as4vers,
  domvalue_l,
  dd07t
}
where
  as4local = 'A' and
  as4vers  = '0000'