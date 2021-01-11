CLASS zcl_bc_balhdr_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES count_list TYPE STANDARD TABLE OF zbcs_balhdr_count WITH KEY object subobject.
    DATA result TYPE count_list READ-ONLY.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS read_balhdr_stats.
    METHODS calc_percentage.
ENDCLASS.



CLASS zcl_bc_balhdr_count IMPLEMENTATION.
  METHOD constructor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Sayma işlemini gerçekleştirir
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    read_balhdr_stats( ).
    calc_percentage( ).
  ENDMETHOD.


  METHOD read_balhdr_stats.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Ham BALHDR istatistiklerini okur
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT * FROM zbcv_balhdr_count_xt
             ORDER BY entry_count DESCENDING
             INTO CORRESPONDING FIELDS OF TABLE @me->result.
  ENDMETHOD.


  METHOD calc_percentage.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Oranları hesaplar
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->result IS NOT INITIAL.

    DATA(entry_sum) = REDUCE zbcs_balhdr_count-entry_count(
                               INIT _sum TYPE zbcs_balhdr_count-entry_count
                               FOR _result IN me->result
                               NEXT _sum = _sum + _result-entry_count ).

    IF entry_sum IS INITIAL. " Paranoya
      RETURN.
    ENDIF.

    LOOP AT me->result ASSIGNING FIELD-SYMBOL(<result>).
      <result>-percentage = <result>-entry_count * 100 / entry_sum.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.