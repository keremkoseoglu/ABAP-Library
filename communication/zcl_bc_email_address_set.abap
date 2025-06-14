CLASS zcl_bc_email_address_set DEFINITION
  PUBLIC
  CREATE PROTECTED
  GLOBAL FRIENDS zif_bc_email_adr_set_factory.

  PUBLIC SECTION.
    INTERFACES zif_bc_email_address_set.

  PROTECTED SECTION.
    DATA email_addresses TYPE bcsy_smtpa.

    METHODS constructor IMPORTING email_addresses TYPE bcsy_smtpa.
ENDCLASS.


CLASS zcl_bc_email_address_set IMPLEMENTATION.
  METHOD constructor.
    me->email_addresses = VALUE #( FOR _ea IN email_addresses
                                   WHERE ( table_line IS NOT INITIAL )
                                   ( to_lower( _ea ) ) ).

    SORT me->email_addresses BY table_line.
    DELETE ADJACENT DUPLICATES FROM me->email_addresses COMPARING table_line.
  ENDMETHOD.

  METHOD zif_bc_email_address_set~to_itab.
    result = me->email_addresses.
  ENDMETHOD.

ENDCLASS.