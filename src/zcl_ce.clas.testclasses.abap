*"* use this source file for your ABAP unit test classes
class ltcl_test definition for testing
  duration short
  risk level harmless final.

  private section.
    class-data: begin of user
              , group_sep type c
              , decimal_sep type c
              , end of user.

    class-methods: class_setup
      , class_teardown.


    methods: setup
           , teardown
           , quan_both for testing raising cx_static_check
           , curr_both for testing raising cx_static_check
           , gjahr_both for testing raising cx_static_check
           , dec_both for testing raising cx_static_check
           , dec_to_dec_both for testing raising cx_static_check
           , int_to_int_both for testing raising cx_static_check
           , float_both for testing raising cx_static_check
           , date_both for testing raising cx_static_check
           , time_both for testing raising cx_static_check
           , numc_both for testing raising cx_static_check
           , char_both for testing raising cx_static_check
           , string_both for testing raising cx_static_check
           , curr_overflow for testing raising cx_static_check
           , object_not_suported for testing raising cx_static_check
           , structure_not_suported for testing raising cx_static_check
           , action_not_suported_both importing test_name type string
                                                    value type any
                                          expected_string type any
                                         changing conv_to type any
                                                value_new type any
           , generic_both  importing test_name type string
                                                    value type any
                                          expected_string type any
                                         changing conv_to type any
                                                value_new type any .
endclass.

class ltcl_test implementation.
  method class_setup.
    user-group_sep = zcl_ce=>get_user_group_sep( ).
    user-decimal_sep = zcl_ce=>get_user_decimal_sep( ).
  endmethod.
  method class_teardown.
    user-group_sep = zcl_ce=>get_user_group_sep( ).
    user-decimal_sep = zcl_ce=>get_user_decimal_sep( ).
  endmethod.

  method setup.
  endmethod.

  method teardown.
  endmethod.


  method generic_both.
    try.
        zcl_ce=>output( exporting input = value
                            changing output = conv_to ).
      catch zcx_ce_t100 into data(ref_t100).
        cl_abap_unit_assert=>fail( test_name && ': Error Calling OUTPUT: ' && ref_t100->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_true( act = boolc( conv_to eq expected_string )
                                      msg = test_name && ': Unexpected display value' ).

    try.
        zcl_ce=>input( exporting input = conv_to
                           changing output = value_new ).
      catch zcx_ce_t100 into ref_t100.
        cl_abap_unit_assert=>fail( test_name && ': Error Calling INPUT: ' && ref_t100->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_true( act = boolc( value eq value_new )
                                      msg = test_name && ': Unexpected internal value' ).
  endmethod.


  method quan_both.
    data: lv_test_name type string value 'quan_both'
        , lv_value type p length 10 decimals 3 value '1.003'
        , lv_expected_string type string value '1,003'
        , lv_string type string
        , lv_value_new like lv_value
        , lv_msehi type t006-msehi value 'BOT'.

    try.
        zcl_ce=>output( exporting input = lv_value
                                     msehi = lv_msehi
                           changing output = lv_string ).
      catch zcx_ce_t100 into data(ref_t100).
        cl_abap_unit_assert=>fail( lv_test_name && ': Error Calling OUTPUT: ' && ref_t100->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_true( act = boolc( lv_string eq lv_expected_string )
                                      msg = lv_test_name && ': Unexpected display value' ).

    try.
        zcl_ce=>input( exporting input = lv_string
                                     msehi = lv_msehi
                           changing output = lv_value_new ).
      catch zcx_ce_t100 into ref_t100.
        cl_abap_unit_assert=>fail( 'Error Calling INPUT: ' && ref_t100->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_true( act = boolc( lv_value eq lv_value_new )
                                      msg = lv_test_name && ': Unexpected internal value' ).
  endmethod.

  method curr_both.
    data: lv_test_name type string value 'curr_both'
        , lv_value type p length 16 decimals 2
        , lv_expected_string type string
        , lv_string type string
        , lv_value_new like lv_value
        , lv_waers type waers value 'CLP'.
    lv_expected_string = '1' && user-group_sep && '234'.
    lv_value = 1234 / 100.
    try.
        zcl_ce=>output( exporting input = lv_value
                                      waers = lv_waers
                            changing output = lv_string ).
      catch zcx_ce_t100 into data(ref_t100).
        cl_abap_unit_assert=>fail( lv_test_name && ': Error Calling OUTPUT: ' && ref_t100->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_true( act = boolc( lv_string eq lv_expected_string )
                                      msg = lv_test_name && ': Unexpected display value' ).

    try.
        zcl_ce=>input( exporting input = lv_string
                                     waers = lv_waers
                           changing output = lv_value_new ).
      catch zcx_ce_t100 into ref_t100.
        cl_abap_unit_assert=>fail( 'Error Calling INPUT: ' && ref_t100->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_true( act = boolc( lv_value eq lv_value_new )
                                      msg = lv_test_name && ': Unexpected internal value' ).
  endmethod.

  method gjahr_both.
    data: lv_value type gjahr value '2022'
        , lv_value_new like lv_value
        , lv_expected_string type c length 4 value '2022'
        , lv_string type string.

    generic_both( exporting test_name = 'gjahr_both' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.

  method dec_both.
    data: lv_value  type p length 10 decimals 3 value '-12345.678'
        , lv_value_new like lv_value
        , lv_expected_string type string value '-12.345,678'
        , lv_string type string.

    generic_both( exporting test_name = 'dec_both_negative' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).

    ##LITERAL
    lv_value  = '12345.678'.
    ##LITERAL
    lv_expected_string = '12.345,678'.
    generic_both( exporting test_name = 'dec_both_positive' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.

  method float_both.
    data: lv_value  type f value '-12345.678'
        , lv_value_new like lv_value
        , lv_expected_string type string
        , lv_string type string.

    lv_expected_string = '-1' && user-decimal_sep && '2345678000000000E+04'.
    generic_both( exporting test_name = 'float_both_negative' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).

    ##LITERAL
    lv_value  = '12345.678'.
    lv_expected_string = '1' && user-decimal_sep && '2345678000000000E+04'.
    generic_both( exporting test_name = 'float_both_positive' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.


  method date_both.
    data: lv_value  type d value '20220330'
        , lv_value_new type d
        , lv_expected_string type c length 10
        , lv_string type string.

    write lv_value to lv_expected_string.

    generic_both( exporting test_name = 'string_both' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.

  method time_both.
    data: lv_value  type t value '235959'
        , lv_value_new type t
        , lv_expected_string type c length 8
        , lv_string type string.

    write lv_value to lv_expected_string.

    generic_both( exporting test_name = 'string_both' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.


  method numc_both.
    data: lv_value  type n length 10 value '123456'
        , lv_value_new like lv_value
        , lv_expected_string type c length 6 value '123456'
        , lv_string type string.

    generic_both( exporting test_name = 'string_both' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.


  method char_both.
    data: lv_value type c length 20 value 'Hello There'
        , lv_value_new like lv_value
        , lv_expected_string type c length 11 value 'Hello There'
        , lv_string type string.

    generic_both( exporting test_name = 'string_both' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.

  method string_both.
    data: lv_value type string value 'Hello There'
        , lv_value_new like lv_value
        , lv_expected_string type c length 11 value 'Hello There'
        , lv_string type string.

    generic_both( exporting test_name = 'string_both' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.

  method curr_overflow.
    data: lv_test_name type string value 'curr_both'
        , lv_value type p length 16 value 1234567891234567891234567891234
        , lv_expected_string type string
        , lv_string type string
        , lv_value_new like lv_value
        , lv_waers type waers value 'CLP'.
    try.
        zcl_ce=>output( exporting input = lv_value
                                      waers = lv_waers
                            changing output = lv_string ).
      catch zcx_ce_t100 into data(ref_t100).
        cl_abap_unit_assert=>assert_true( act = boolc( ref_t100->get_msgid_msgno( ) eq 'ZCL_CE003' )
                                  msg = 'Un expedted error: ' && ref_t100->get_text( ) ).
      catch cx_root into data(ref_root).
        message ref_root->get_text( ) type 'A'.
    endtry.
  endmethod.
  method dec_to_dec_both.
    data: lv_value type p length 10 decimals 3 value '-12345.678'
        , lv_value_new like lv_value
        , lv_string like lv_value
        , lv_expected_string like lv_value value '-12345.678'.

    generic_both( exporting test_name = 'dec_both_negative' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).

    lv_value  = '12345.678'.
    lv_expected_string = '12345.678'.
    generic_both( exporting test_name = 'dec_both_positive' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.

  method int_to_int_both.
    data: lv_value type int4 value '-12345'
        , lv_value_new like lv_value
        , lv_string like lv_value
        , lv_expected_string like lv_value value '-12345'.

    generic_both( exporting test_name = 'int_both_negative' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).

    lv_value  = '12345'.
    lv_expected_string = '12345'.
    generic_both( exporting test_name = 'int_both_positive' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.


  method action_not_suported_both.
    try.
        zcl_ce=>output( exporting input = value
                            changing output = conv_to ).
        cl_abap_unit_assert=>fail( 'Error Calling OUTPUT: Didn''t fail' ).
      catch zcx_ce_t100 into data(ref_t100).
        cl_abap_unit_assert=>assert_true( act = boolc( ref_t100->get_msgid_msgno( ) eq 'ZCL_CE000' )
                                          msg = 'Un expedted error: ' && ref_t100->get_text( ) ).
    endtry.
    try.
        zcl_ce=>output( exporting input = conv_to
                           changing output = value_new ).
        cl_abap_unit_assert=>fail( 'Error Calling OUTPUT: Didn''t fail' ).
      catch zcx_ce_t100.
        cl_abap_unit_assert=>assert_true( act = boolc( ref_t100->get_msgid_msgno( ) eq 'ZCL_CE000' )
                                          msg = 'Un expedted error: ' && ref_t100->get_text( ) ).
    endtry.

    try.
        zcl_ce=>input( exporting input = conv_to
                            changing output = value_new ).
        cl_abap_unit_assert=>fail( 'Error Calling INPUT: Didn''t fail' ).
      catch zcx_ce_t100 into ref_t100.
        cl_abap_unit_assert=>assert_true( act = boolc( ref_t100->get_msgid_msgno( ) eq 'ZCL_CE000' )
                                          msg = 'Un expedted error: ' && ref_t100->get_text( ) ).
    endtry.
    try.
        zcl_ce=>input( exporting input = value
                           changing output = conv_to ).
        cl_abap_unit_assert=>fail( 'Error Calling INPUT: Didn''t fail' ).
      catch zcx_ce_t100.
        cl_abap_unit_assert=>assert_true( act = boolc( ref_t100->get_msgid_msgno( ) eq 'ZCL_CE000' )
                                          msg = 'Un expedted error: ' && ref_t100->get_text( ) ).
    endtry.
  endmethod.

  method object_not_suported.
    data: lv_value type ref to zcl_ce
        , lv_value_new like lv_value
        , lv_string type string
        , lv_expected_string type string.

    action_not_suported_both( exporting test_name = 'object_not_suported' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.

  method structure_not_suported.
    data: lv_value type usr01
        , lv_value_new like lv_value
        , lv_string type string
        , lv_expected_string type c length 11 value ''.
    action_not_suported_both( exporting test_name = 'object_not_suported' value = lv_value expected_string = lv_expected_string changing conv_to = lv_string value_new = lv_value_new ).
  endmethod.
endclass.
