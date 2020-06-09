interface ZIF_BC_MULTITON
  public .

  class-methods:
      get_instance
        importing
          !iv_objectid type CDOBJECTV
        returning
          value(ro_obj) type ref to ZIF_BC_MULTITON
        raising
          CX_SY_CREATE_OBJECT_ERROR.

endinterface.