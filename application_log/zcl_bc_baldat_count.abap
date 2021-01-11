CLASS zcl_bc_baldat_count DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES count_list TYPE STANDARD TABLE OF zbcs_baldat_count WITH KEY object subobject.
    DATA result TYPE count_list READ-ONLY.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS read_baldat_stats.
ENDCLASS.



CLASS zcl_bc_baldat_count IMPLEMENTATION.
  METHOD constructor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Sayma işlemini gerçekleştirir
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    read_baldat_stats( ).
  ENDMETHOD.


  METHOD read_baldat_stats.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Ham BALDAT istatistiklerini okur
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT * FROM zbcv_baldat_count
             ORDER BY total_size DESCENDING
             INTO CORRESPONDING FIELDS OF TABLE @me->result.
  ENDMETHOD.
ENDCLASS.