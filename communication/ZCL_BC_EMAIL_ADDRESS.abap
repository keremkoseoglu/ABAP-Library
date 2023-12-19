CLASS zcl_bc_email_address DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    DATA email_address TYPE ad_smtpadr READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING email_address TYPE ad_smtpadr
      RETURNING VALUE(result) TYPE REF TO zcl_bc_email_address.

    METHODS get_cased_range RETURNING VALUE(result) TYPE /cfg/t_email.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             email_address TYPE ad_smtpadr,
             obj           TYPE REF TO zcl_bc_email_address,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS email_address.

    CLASS-DATA multitons TYPE multiton_set.

    DATA cased_range TYPE /cfg/t_email.

    METHODS constructor IMPORTING email_address TYPE ad_smtpadr.
ENDCLASS.


CLASS zcl_bc_email_address IMPLEMENTATION.
  METHOD get_instance.
    TRY.
        DATA(mt) = REF #( zcl_bc_email_address=>multitons[ KEY primary_key COMPONENTS email_address = email_address ] ).
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( email_address = email_address
                        obj           = NEW #( email_address ) )
               INTO TABLE zcl_bc_email_address=>multitons REFERENCE INTO mt.
    ENDTRY.

    result = mt->obj.
  ENDMETHOD.

  METHOD get_cased_range.
    IF me->cased_range IS INITIAL.
      me->cased_range = VALUE #( sign   = ycl_addict_toolkit=>sign-include
                                 option = ycl_addict_toolkit=>option-eq
                                 ( low = me->email_address )
                                 ( low = to_upper( me->email_address ) )
                                 ( low = to_lower( me->email_address ) ) ).

      SORT me->cased_range BY table_line.
      DELETE ADJACENT DUPLICATES FROM me->cased_range COMPARING table_line.
    ENDIF.

    result = me->cased_range.
  ENDMETHOD.

  METHOD constructor.
    me->email_address = email_address.
  ENDMETHOD.
ENDCLASS.