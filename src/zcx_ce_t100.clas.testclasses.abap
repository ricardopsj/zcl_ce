*"* use this source file for your ABAP unit test classes

class ltcl_test definition for testing
  duration short
  risk level harmless final.

  private section.
    class-methods: class_setup
      , class_teardown.

    methods: setup
           , teardown
           , try_catch for testing raising cx_static_check
           , raise for testing raising cx_static_check.
endclass.

class ltcl_test implementation.
  method class_setup.
  endmethod.
  method class_teardown.
  endmethod.

  method setup.
  endmethod.

  method teardown.
  endmethod.

  method try_catch.
    try.
        raise exception type zcx_ce_t100.
      catch zcx_ce_t100 into data(ref_t100).
        cl_abap_unit_assert=>assert_true( act = abap_true
                                          msg = ref_t100->get_text( ) ).
    endtry.
  endmethod.


  method raise.
    try.
        zcx_ce_t100=>raise( msgid = 'ZCL_CE' msgno = '000' msgv1 = 'input' ).
        cl_abap_unit_assert=>assert_true( act = abap_false
                                          msg = 'No falló' ).
      catch zcx_ce_t100 into data(ref_t100).
        cl_abap_unit_assert=>assert_true( act = boolc( ref_t100->get_msgid_msgno( ) eq 'ZCL_CE000' )
                                          msg = 'Mensaje Inesperado' ).
    endtry.
  endmethod.



endclass.
