CLASS zcl_bc_email_address DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    DATA email_address TYPE ad_smtpadr READ-ONLY.

    CLASS-METHODS get_instance
      IMPORTING email_address TYPE ad_smtpadr
      RETURNING VALUE(result) TYPE REF TO zcl_bc_email_address.

    METHODS get_cased_range RETURNING VALUE(result) TYPE /cfg/t_email.

    METHODS get_domain
      RETURNING VALUE(result) TYPE zbcd_email_domain
      RAISING   ycx_addict_email_address.

    METHODS get_local_part
      RETURNING VALUE(result) TYPE string
      RAISING   ycx_addict_email_address.

    METHODS populate_with_alt_domains
      IMPORTING domains       TYPE zbctt_email_domain
      RETURNING VALUE(result) TYPE bcsy_smtpa
      RAISING   ycx_addict_email_address.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_dict,
             email_address TYPE ad_smtpadr,
             obj           TYPE REF TO zcl_bc_email_address,
           END OF multiton_dict,

           multiton_set TYPE HASHED TABLE OF multiton_dict
                        WITH UNIQUE KEY primary_key COMPONENTS email_address.

    CLASS-DATA multitons TYPE multiton_set.

    DATA: cased_range TYPE /cfg/t_email,
          domain      TYPE zbcd_email_domain,
          local_part  TYPE string.

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

  METHOD get_domain.
    DATA dummy1 TYPE string ##NEEDED.

    IF me->domain IS INITIAL.
      SPLIT me->email_address AT '@' INTO dummy1 me->domain.

      IF me->domain IS INITIAL.
        RAISE EXCEPTION NEW ycx_addict_email_address( textid = ycx_addict_email_address=>invalid_address
                                                      email  = me->email_address ).
      ENDIF.
    ENDIF.

    result = me->domain.
  ENDMETHOD.

  METHOD get_local_part.
    DATA dummy1 TYPE string ##NEEDED.

    IF me->local_part IS INITIAL.
      SPLIT me->email_address AT '@' INTO me->local_part dummy1.

      IF me->local_part IS INITIAL.
        RAISE EXCEPTION NEW ycx_addict_email_address( textid = ycx_addict_email_address=>invalid_address
                                                      email  = me->email_address ).
      ENDIF.

      me->local_part = to_lower( me->local_part ).
    ENDIF.

    result = me->local_part.
  ENDMETHOD.

  METHOD populate_with_alt_domains.
    APPEND to_lower( me->email_address ) TO result.

    CHECK domains IS NOT INITIAL.
    DATA(own_domain) = CONV zbcd_email_domain( to_lower( get_domain( ) ) ).
    DATA(local_part) = get_local_part( ).

    LOOP AT domains REFERENCE INTO DATA(domain).
      DATA(low_domain) = to_lower( domain->* ).
      CHECK low_domain <> own_domain.
      APPEND |{ local_part }@{ low_domain }| TO result.
    ENDLOOP.

    SORT result BY table_line.
    DELETE ADJACENT DUPLICATES FROM result COMPARING table_line.
  ENDMETHOD.

  METHOD constructor.
    me->email_address = email_address.
  ENDMETHOD.
ENDCLASS.