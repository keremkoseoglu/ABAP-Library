CLASS zcl_bc_multiton_demo_2 DEFINITION
  PUBLIC
  FINAL
  CREATE private .

  PUBLIC SECTION.

    data:
      gv_id type char10 read-only.

    class-methods:
      get_instance
        importing !iv_id type char10
        returning value(ro_obj) type ref to zcl_bc_multiton_Demo_2.

  PROTECTED SECTION.
  PRIVATE SECTION.

    types:
      begin of t_multiton,
        id  type char10,
        obj type ref to zcl_Bc_multiton_demo_2,
      end of t_multiton,

      tt_multiton
        type hashed table of t_multiton
        with unique key primary_key components id.

    class-data gt_multiton type tt_multiton.

    class-methods create_instance
      importing !iv_id type char10
      returning value(ro_obj) type ref to zcl_bc_multiton_Demo_2.

    methods constructor importing !iv_id type char10.

ENDCLASS.



CLASS zcl_bc_multiton_demo_2 IMPLEMENTATION.

  method constructor.

    gv_id = iv_id.

    insert value #(
        id  = gv_id
        obj = me
      ) into table gt_multiton..

  endmethod.

  method create_instance.
    ro_obj = new #( iv_id ).
  endmethod.

  method get_instance.

    ro_obj = value #(
      gt_multiton[ key primary_key components id = iv_id ]-obj
      default create_instance( iv_id )
    ).

  endmethod.


ENDCLASS.