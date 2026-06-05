CLASS zcl_bc_t100_msg DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS msg_to_txt
      IMPORTING !id           TYPE symsgid
                !no           TYPE symsgno
                langu         TYPE sylangu DEFAULT sy-langu
                v1            TYPE any     OPTIONAL
                v2            TYPE any     OPTIONAL
                v3            TYPE any     OPTIONAL
                v4            TYPE any     OPTIONAL
      RETURNING VALUE(result) TYPE bapi_msg.

    CLASS-METHODS get_instance
      IMPORTING !id           TYPE symsgid
                !no           TYPE symsgno
      RETURNING VALUE(result) TYPE REF TO zcl_bc_t100_msg.

    METHODS show
      IMPORTING !type TYPE symsgty
                langu TYPE sylangu DEFAULT sy-langu
                v1    TYPE any     OPTIONAL
                v2    TYPE any     OPTIONAL
                v3    TYPE any     OPTIONAL
                v4    TYPE any     OPTIONAL.

    METHODS to_txt
      IMPORTING langu         TYPE sylangu DEFAULT sy-langu
                v1            TYPE any     OPTIONAL
                v2            TYPE any     OPTIONAL
                v3            TYPE any     OPTIONAL
                v4            TYPE any     OPTIONAL
      RETURNING VALUE(result) TYPE bapi_msg.

  PRIVATE SECTION.
    TYPES: BEGIN OF multiton_map,
             id  TYPE symsgid,
             no  TYPE symsgno,
             obj TYPE REF TO zcl_bc_t100_msg,
           END OF multiton_map,

           multiton_set TYPE HASHED TABLE OF multiton_map WITH UNIQUE KEY id no.

    TYPES: BEGIN OF t100_text_in_langu_map,
             langu     TYPE sylangu,
             t100_text TYPE natxt,
           END OF t100_text_in_langu_map,

           t100_text_in_langu_set TYPE HASHED TABLE OF t100_text_in_langu_map WITH UNIQUE KEY langu.

    TYPES: BEGIN OF gen_cache_val_map,
             t100s TYPE t100_text_in_langu_set,
           END OF gen_cache_val_map,

           gen_cache_map TYPE gen_cache_val_map WITH INDICATORS is_set TYPE abap_bool.

    CLASS-DATA multitons TYPE multiton_set.

    DATA: id                        TYPE symsgid,
          no                        TYPE symsgno,
          t100_texts_in_langu_cache TYPE t100_text_in_langu_set,
          gen_cache                 TYPE gen_cache_map.

    METHODS constructor
      IMPORTING !id TYPE symsgid
                !no TYPE symsgno.

    METHODS obtain_t100_text_in_langu
      IMPORTING langu         TYPE sylangu
      RETURNING VALUE(result) TYPE REF TO natxt.

    METHODS obtain_t100s RETURNING VALUE(result) TYPE REF TO t100_text_in_langu_set.

    METHODS replace_v
      IMPORTING v        TYPE any
                v_in_msg TYPE char2
      CHANGING  msg      TYPE bapi_msg.
ENDCLASS.


CLASS zcl_bc_t100_msg IMPLEMENTATION.
  METHOD msg_to_txt.
    DATA(msg) = get_instance( id = id
                              no = no ).

    result = msg->to_txt( langu = langu
                          v1    = v1
                          v2    = v2
                          v3    = v3
                          v4    = v4 ).
  ENDMETHOD.

  METHOD get_instance.
    ASSIGN multitons[ KEY primary_key id = id no = no ] TO FIELD-SYMBOL(<mt>).
    IF sy-subrc <> 0.
      INSERT VALUE #( id  = id
                      no  = no
                      obj = NEW #( id = id
                                   no = no ) )
             INTO TABLE multitons ASSIGNING <mt>.
    ENDIF.

    result = <mt>-obj.
  ENDMETHOD.

  METHOD show.
    IF langu = sy-langu.
      MESSAGE ID me->id
              TYPE type
              NUMBER me->no
              WITH v1 v2 v3 v4.

      RETURN.
    ENDIF.

    DATA(text) = to_txt( langu = langu
                         v1    = v1
                         v2    = v2
                         v3    = v3
                         v4    = v4 ).

    MESSAGE text TYPE type.
  ENDMETHOD.

  METHOD to_txt.
    IF langu = sy-langu.
      MESSAGE ID me->id
              TYPE ycl_simbal=>msgty-status
              NUMBER me->no
              WITH v1 v2 v3 v4
              INTO result.

      RETURN.
    ENDIF.

    result = obtain_t100_text_in_langu( langu )->*.

    replace_v( EXPORTING v        = v1
                         v_in_msg = '&1'
               CHANGING  msg      = result ).

    replace_v( EXPORTING v        = v2
                         v_in_msg = '&2'
               CHANGING  msg      = result ).

    replace_v( EXPORTING v        = v3
                         v_in_msg = '&3'
               CHANGING  msg      = result ).

    replace_v( EXPORTING v        = v4
                         v_in_msg = '&4'
               CHANGING  msg      = result ).
  ENDMETHOD.

  METHOD constructor.
    me->id = id.
    me->no = no.
  ENDMETHOD.

  METHOD obtain_t100_text_in_langu.
    ASSIGN me->t100_texts_in_langu_cache[ KEY primary_key
                                          langu = langu ] TO FIELD-SYMBOL(<cache>).
    IF sy-subrc <> 0.
      DATA(new_cache) = VALUE t100_text_in_langu_map( langu = langu ).

      DATA(t100s) = obtain_t100s( ).

      new_cache-t100_text = VALUE #( t100s->*[ KEY primary_key
                                               langu = langu ]-t100_text
                                     DEFAULT VALUE #( t100s->*[ KEY primary_key
                                                                langu = 'E' ]
                                                      DEFAULT VALUE #( t100s->*[ KEY primary_key
                                                                                 langu = 'T' ]
                                                                       DEFAULT space ) ) ).

      IF new_cache-t100_text IS INITIAL AND t100s IS NOT INITIAL.
        LOOP AT t100s->* REFERENCE INTO DATA(t) WHERE t100_text IS NOT INITIAL. "#EC CI_HASHSEQ
          new_cache-t100_text = t->t100_text.
          EXIT.
        ENDLOOP.
      ENDIF.

      INSERT new_cache INTO TABLE me->t100_texts_in_langu_cache ASSIGNING <cache>.
    ENDIF.

    result = REF #( <cache>-t100_text ).
  ENDMETHOD.

  METHOD obtain_t100s.
    result = REF #( me->gen_cache-t100s ).
    CHECK me->gen_cache-is_set-t100s = abap_false.

    SELECT FROM t100 "#EC CI_GENBUFF
           FIELDS sprsl AS langu,
                  text  AS t100_text
           WHERE arbgb = @me->id
             AND msgnr = @me->no
           INTO CORRESPONDING FIELDS OF TABLE @result->*.

    me->gen_cache-is_set-t100s = abap_true.
  ENDMETHOD.

  METHOD replace_v.
    DATA v_text TYPE bapi_msg.

    WRITE v TO v_text LEFT-JUSTIFIED.
    REPLACE ALL OCCURRENCES OF v_in_msg IN msg WITH v_text.
  ENDMETHOD.
ENDCLASS.