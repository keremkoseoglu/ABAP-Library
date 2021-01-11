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

    METHODS execute
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

    METHODS check_duplicate
      IMPORTING !is_item TYPE t_doc_item
      RAISING   zcx_bc_table_content
                zcx_fi_duplicate_doc.

    METHODS check_duplicates RAISING zcx_fi_duplicate_doc.
    METHODS filter_items.
ENDCLASS.



CLASS zcl_fi_duplicate_doc_check IMPLEMENTATION.
  METHOD check_duplicate.
    DATA lv_stblg_mm TYPE re_stblg. "Ters kayıt belge numarası

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

    SELECT bsik~belnr, bsik~gjahr, bsik~buzei, bkpf~awtyp, bkpf~awkey, bkpf~stblg
           FROM lfa1
           INNER JOIN bsik ON bsik~lifnr = lfa1~lifnr
           INNER JOIN bkpf ON bkpf~bukrs = bsik~bukrs AND
                              bkpf~belnr = bsik~belnr AND
                              bkpf~gjahr = bsik~gjahr
           WHERE lfa1~stcd2 = @lv_vkn AND
                 bsik~bukrs = @gs_state-head-bukrs AND
                 bsik~xblnr = @gs_state-head-xblnr AND
                 ( NOT ( bsik~belnr = @gs_state-head-belnr AND
                         bsik~gjahr = @gs_state-head-gjahr ) )
           INTO TABLE @DATA(lt_mukerrer).

    SELECT bsak~belnr, bsak~gjahr, bsak~buzei, bkpf~awtyp, bkpf~awkey, bkpf~stblg
           FROM lfa1
           INNER JOIN bsak ON bsak~lifnr = lfa1~lifnr
           INNER JOIN bkpf ON bkpf~bukrs = bsak~bukrs AND
                              bkpf~belnr = bsak~belnr AND
                              bkpf~gjahr = bsak~gjahr
           WHERE lfa1~stcd2 = @lv_vkn AND
                 bsak~bukrs = @gs_state-head-bukrs AND
                 bsak~xblnr = @gs_state-head-xblnr AND
                 ( NOT ( bsak~belnr = @gs_state-head-belnr AND
                         bsak~gjahr = @gs_state-head-gjahr ) )
           APPENDING CORRESPONDING FIELDS OF TABLE @lt_mukerrer.

    LOOP AT lt_mukerrer ASSIGNING FIELD-SYMBOL(<ls_mukerrer>).
      CLEAR lv_stblg_mm.

      IF <ls_mukerrer>-awtyp = c_fat_giris.
        "Orjinal MM belgesi ters kaydı alınmış mı kontrolü
        SELECT SINGLE stblg FROM rbkp
             WHERE belnr = @<ls_mukerrer>-awkey(10) AND
                   gjahr = @<ls_mukerrer>-awkey+10
             INTO @lv_stblg_mm .
      ENDIF.

      " Muhasebe belgesi veya mm belgesinin ters kaydı alınmışsa mukerrer belge listesinden çıkar.
      IF lv_stblg_mm IS NOT INITIAL OR <ls_mukerrer>-stblg IS NOT INITIAL.
        DELETE lt_mukerrer.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    IF lt_mukerrer IS INITIAL.
      RETURN.
    ENDIF.

    IF is_item-gsber IS NOT INITIAL.
      DATA(lt_gsber_rng) =  VALUE range_gsber_in_t( ( sign   = zcl_bc_ddic_toolkit=>c_sign_i
                                                      option = zcl_bc_ddic_toolkit=>c_option_eq
                                                      low    = is_item-gsber ) ).
    ENDIF.

    SELECT belnr,gjahr FROM bseg
           FOR ALL ENTRIES IN @lt_mukerrer
           WHERE bukrs = @gs_state-head-bukrs AND
                 belnr = @lt_mukerrer-belnr AND
                 gjahr = @lt_mukerrer-gjahr AND
                 buzei = @lt_mukerrer-buzei AND
                 gsber IN @lt_gsber_rng
           INTO TABLE @DATA(lt_belge).                  "#EC CI_NOORDER

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
    DATA lr_usnam TYPE RANGE OF usnam.

    CHECK NOT ( iv_tcode = c_tcode_fb08 OR
                iv_tcode = c_tcode_f80  OR
                iv_tcode = c_tcode_mr8m OR
                iv_tcode = c_tcode_fbra ).

    SELECT valsign, valoption, valfrom, valto
           FROM setleaf
           WHERE setclass = @c_setclass AND
                 subclass = @c_subclass AND
                 setname  = @c_setname
           INTO TABLE @DATA(lt_setleaf).

    lr_usnam = CORRESPONDING #( lt_setleaf MAPPING
                                sign   = valsign
                                option = valoption
                                low    = valfrom
                                high   = valto ).

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

    gs_state-item_filtered = VALUE #( FOR ls_item IN gs_state-item
                                      WHERE ( koart = c_koart_satici_kalemi )
                                      ( CORRESPONDING #( ls_item ) ) ).

    IF gs_state-item_filtered IS INITIAL.
      RETURN.
    ENDIF.

    " ______________________________
    " Sadece satış ilişkili kayıt anahtarlarını istiyoruz

    zcl_fi_posting_key=>cache( VALUE #( FOR ls_if IN gs_state-item_filtered
                                        ( ls_if-bschl ) ) ).

    LOOP AT gs_state-item_filtered ASSIGNING FIELD-SYMBOL(<ls_if>).
      DATA(lv_del) = abap_false.

      TRY.
          lv_del = xsdbool( zcl_fi_posting_key=>get_instance( <ls_if>-bschl )->gs_def-xumsw = abap_false ).
        CATCH zcx_bc_table_content.
          lv_del = abap_true.
      ENDTRY.

      CHECK lv_del = abap_true.
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
          lv_del = xsdbool( zcl_mm_vendor=>get_instance(
                                <ls_if>-lifnr
                            )->get_company_code_data(
                                gs_state-head-bukrs
                            )-reprf = abap_false ).

        CATCH zcx_bc_table_content.
          lv_del = abap_true.
      ENDTRY.

      CHECK lv_del = abap_true.
      DELETE gs_state-item_filtered.
      CONTINUE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.