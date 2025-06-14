INTERFACE zif_bc_email_adr_set_factory
  PUBLIC.

  METHODS create_adr_set_via_email
    IMPORTING email_address TYPE ad_smtpadr
    RETURNING VALUE(result) TYPE REF TO zif_bc_email_address_set
    RAISING   ycx_addict_email_address.

ENDINTERFACE.