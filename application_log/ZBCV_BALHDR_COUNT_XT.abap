@AbapCatalog.sqlViewName: 'ZBCV_005'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'BALHDR kayıt sayısı + ek alanlar'
define view ZBCV_BALHDR_COUNT_XT
  as select from    ZBCV_BALHDR_COUNT as _count
    left outer join balobjt                   on
    balobjt.spras  = $session.system_language and
    balobjt.object = _count.object
    left outer join balsubt                      on
    balsubt.spras     = $session.system_language and
    balsubt.object    = _count.object            and
    balsubt.subobject = _count.subobject {
  key _count.object,
  key _count.subobject,
  _count.ENTRY_COUNT,
  balobjt.objtxt,
  balsubt.subobjtxt
}