CLASS zcl_fi_duplicate_doc_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_doc_head,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        gjahr TYPE bkpf-gjahr,
        xblnr TYPE bkpf-xblnr,
        bstat TYPE bkpf-bstat,
      END OF t_doc_head,

      BEGIN OF t_doc_item,
        koart TYPE bseg-koart,
        bschl TYPE bseg-bschl,
        lifnr TYPE bseg-lifnr,
        gsber TYPE bseg-gsber,
      END OF t_doc_item,

      tt_doc_item TYPE STANDARD TABLE OF t_doc_item WITH DEFAULT KEY.

    METHODS:
      execute
        IMPORTING
          !is_doc_head TYPE t_doc_head
          !it_doc_item TYPE tt_doc_item
          !iv_tcode    TYPE sytcode DEFAULT sy-tcode
        RAISING
          zcx_fi_duplicate_doc.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_state,
        head          TYPE t_doc_head,
        item          TYPE tt_doc_item,
        item_filtered TYPE tt_doc_item,
      END OF t_state.

    CONSTANTS:
      c_fat_giris           TYPE awtyp VALUE 'RMRP',
      c_koart_satici_kalemi TYPE koart VALUE 'K',

      c_tcode_fb08          TYPE sy-tcode VALUE 'FB08',
      c_tcode_f80           TYPE sy-tcode VALUE 'F.80',
      c_tcode_mr8m          TYPE sy-tcode VALUE 'MR8M',
      c_tcode_fbra          TYPE sy-tcode VALUE 'FBRA',

      c_setclass            TYPE setclass VALUE '0000',
      c_subclass            TYPE subclass VALUE space,
      c_setname             TYPE setnamenew VALUE 'ZFI_IST_KULLANICI'.

    DATA gs_state TYPE t_state.

    METHODS:

      check_duplicate
        IMPORTING
          !is_item TYPE t_doc_item
        RAISING
          zcx_bc_table_content
          zcx_fi_duplicate_doc,

      check_duplicates RAISING zcx_fi_duplicate_doc,
      filter_items.
ENDCLASS.



CLASS zcl_fi_duplicate_doc_check IMPLEMENTATION.


  METHOD check_duplicate.

    DATA: lv_stblg_mm TYPE re_stblg. "Ters kayıt belge numarası

    " ______________________________
    " Ön kontroller

    CHECK gs_state-head-xblnr IS NOT INITIAL.

    " ______________________________
    " Girişi yapılan belgenin satıcı kaleminin VKN değeri ile aynı VKN değerine sahip satıcılar seçilecek

    DATA(lv_vkn) = zcl_mm_vendor=>get_instance( is_item-lifnr )->gs_def-stcd2.
    CHECK lv_vkn IS NOT INITIAL.

    " ______________________________
    " Önceki adımda bulunan VKN’ye sahip satıcıların
    " Mükerrer belgelere ilişkin satıcı kontrol endeks belgeleri seçilir
    SELECT bsip~belnr, bsip~gjahr, bsip~buzei, bkpf~awtyp, bkpf~awkey, bkpf~stblg
      FROM lfa1
      INNER JOIN bsip ON bsip~lifnr EQ lfa1~lifnr
      INNER JOIN bkpf ON bkpf~bukrs EQ bsip~bukrs
                      AND bkpf~belnr EQ bsip~belnr
                      AND bkpf~gjahr EQ bsip~gjahr
      INTO TABLE @DATA(lt_mukerrer)
      WHERE lfa1~stcd2 EQ @lv_vkn
        AND bsip~bukrs EQ @gs_state-head-bukrs
        AND bsip~xblnr EQ @gs_state-head-xblnr.

    DELETE lt_mukerrer WHERE belnr EQ gs_state-head-belnr
                         AND gjahr EQ gs_state-head-gjahr.

    LOOP AT lt_mukerrer ASSIGNING FIELD-SYMBOL(<ls_mukerrer>).

      CLEAR lv_stblg_mm.

      IF <ls_mukerrer>-awtyp EQ c_fat_giris.
        "Orjinal MM belgesi ters kaydı alınmış mı kontrolü
        SELECT SINGLE stblg INTO lv_stblg_mm FROM rbkp
             WHERE belnr = <ls_mukerrer>-awkey(10)
               AND gjahr = <ls_mukerrer>-awkey+10.
      ENDIF.


      " Muhasebe belgesi veya mm belgesinin ters kaydı alınmışsa mukerrer belge listesinden çıkar.

      IF lv_stblg_mm IS NOT INITIAL OR
         <ls_mukerrer>-stblg IS NOT INITIAL.
        DELETE lt_mukerrer.
        CONTINUE.
      ENDIF.

    ENDLOOP.

    IF lt_mukerrer IS INITIAL.
      RETURN.
    ENDIF.

    SELECT belnr,gjahr FROM bseg INTO TABLE @DATA(lt_belge)
      FOR ALL ENTRIES IN @lt_mukerrer
      WHERE bukrs EQ @gs_state-head-bukrs
        AND belnr EQ @lt_mukerrer-belnr
        AND gjahr EQ @lt_mukerrer-gjahr
        AND buzei EQ @lt_mukerrer-buzei
        AND gsber EQ @is_item-gsber.                    "#EC CI_NOORDER

    IF lt_belge IS INITIAL.
      RETURN.
    ENDIF.

    " ______________________________
    " Bu aşamaya gelindiyse daha önceden kaydedilmiş başka bir belge vardır

    ASSIGN lt_belge[ 1 ] TO FIELD-SYMBOL(<ls_belge>).

    RAISE EXCEPTION TYPE zcx_fi_duplicate_doc
      EXPORTING
        bukrs = gs_state-head-bukrs
        belnr = <ls_belge>-belnr
        gjahr = <ls_belge>-gjahr.

  ENDMETHOD.


  METHOD check_duplicates.

    LOOP AT gs_state-item_filtered ASSIGNING FIELD-SYMBOL(<ls_if>).

      TRY.
          check_duplicate( <ls_if> ).
        CATCH zcx_bc_table_content ##no_handler .
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD execute.
    DATA: lr_usnam TYPE RANGE OF usnam.

    CHECK NOT (
        iv_tcode EQ c_tcode_fb08 OR
        iv_tcode EQ c_tcode_f80  OR
        iv_tcode EQ c_tcode_mr8m OR
        iv_tcode EQ c_tcode_fbra
    ).

    SELECT * FROM setleaf INTO TABLE @DATA(lt_setleaf)
      WHERE setclass EQ @c_setclass
        AND subclass EQ @c_subclass
        AND setname  EQ @c_setname.

    LOOP AT lt_setleaf ASSIGNING FIELD-SYMBOL(<ls_setleaf>).
      APPEND INITIAL LINE TO lr_usnam REFERENCE INTO DATA(lrv_usnam).
      lrv_usnam->sign   = <ls_setleaf>-valsign.
      lrv_usnam->option = <ls_setleaf>-valoption.
      lrv_usnam->low    = <ls_setleaf>-valfrom.
      lrv_usnam->high   = <ls_setleaf>-valto.
    ENDLOOP.

    IF lr_usnam IS NOT INITIAL AND
       sy-uname IN lr_usnam.
      RETURN.
    ENDIF.

    CLEAR gs_state.
    gs_state-head = is_doc_head.
    gs_state-item = it_doc_item.

    filter_items( ).
    check_duplicates( ).

  ENDMETHOD.


  METHOD filter_items.

    " ______________________________
    " Sadece satıcı kalemlerini istiyoruz

    gs_state-item_filtered = VALUE #(
        FOR ls_item IN gs_state-item
        WHERE ( koart EQ c_koart_satici_kalemi )
        ( CORRESPONDING #( ls_item ) )
    ).

    IF gs_state-item_filtered IS INITIAL.
      RETURN.
    ENDIF.

    " ______________________________
    " Sadece satış ilişkili kayıt anahtarlarını istiyoruz

    zcl_fi_posting_key=>cache( VALUE #(
        FOR ls_if IN gs_state-item_filtered
        ( ls_if-bschl )
    ) ).

    LOOP AT gs_state-item_filtered ASSIGNING FIELD-SYMBOL(<ls_if>).

      DATA(lv_del) = abap_false.

      TRY.
          lv_del = xsdbool( zcl_fi_posting_key=>get_instance( <ls_if>-bschl )->gs_def-xumsw EQ abap_false ).
        CATCH zcx_bc_table_content.
          lv_del = abap_true.
      ENDTRY.

      CHECK lv_del EQ abap_true.
      DELETE gs_state-item_filtered.
      CONTINUE.

    ENDLOOP.

    IF gs_state-item_filtered IS INITIAL.
      RETURN.
    ENDIF.

    " ______________________________
    " Sadece çift kayıt kontrolü etkin satıcıları istiyoruz

    LOOP AT gs_state-item_filtered ASSIGNING <ls_if>.

      lv_del = abap_false.

      TRY.

          lv_del = xsdbool(
              zcl_mm_vendor=>get_instance(
                      <ls_if>-lifnr
                  )->get_company_code_data(
                      gs_state-head-bukrs
                  )-reprf EQ abap_false
          ).

        CATCH zcx_bc_table_content.
          lv_del = abap_true.
      ENDTRY.

      CHECK lv_del EQ abap_true.
      DELETE gs_state-item_filtered.
      CONTINUE.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.