CLASS zcl_bc_email_address_set DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA rows TYPE bcsy_smtpa READ-ONLY.

    METHODS constructor IMPORTING email_addresses TYPE bcsy_smtpa
                        RAISING   RESUMABLE(ycx_addict_email_address).

    METHODS merge           IMPORTING merge_addresses TYPE REF TO zcl_bc_email_address_set.

    METHODS conv_to_txlines RETURNING VALUE(result)   TYPE catsxt_longtext_itab.

    METHODS set_rows IMPORTING email_addresses TYPE bcsy_smtpa
                     RAISING   RESUMABLE(ycx_addict_email_address).

  PROTECTED SECTION.

ENDCLASS.


CLASS zcl_bc_email_address_set IMPLEMENTATION.
  METHOD constructor.
    TRY.
        set_rows( email_addresses ).

      CATCH BEFORE UNWIND ycx_addict_email_address INTO DATA(adr_error).
        RAISE RESUMABLE EXCEPTION adr_error.
        RESUME.
    ENDTRY.
  ENDMETHOD.

  METHOD merge.
    LOOP AT merge_addresses->rows REFERENCE INTO DATA(mr).
      CHECK NOT line_exists( me->rows[ table_line = mr->* ] ).
      INSERT mr->* INTO TABLE me->rows.
    ENDLOOP.
  ENDMETHOD.

  METHOD conv_to_txlines.
    result = VALUE #( FOR _row IN rows ( CONV #( _row ) ) ).
  ENDMETHOD.

  METHOD set_rows.
    DATA(new_rows) = VALUE bcsy_smtpa( FOR _ea IN email_addresses
                                       WHERE ( table_line IS NOT INITIAL )
                                       ( to_lower( _ea ) ) ).

    SORT new_rows BY table_line.
    DELETE ADJACENT DUPLICATES FROM new_rows COMPARING table_line.

    LOOP AT new_rows REFERENCE INTO DATA(new_row).
      TRY.
          zcl_bc_email_address=>get_instance( new_row->* ). " Validasyon

        CATCH ycx_addict_email_address INTO DATA(adr_error).
          RAISE RESUMABLE EXCEPTION adr_error.
          DELETE new_rows.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    me->rows = new_rows.
  ENDMETHOD.
ENDCLASS.