class ZCL_CE definition
  public
  final
  create public .

public section.

  types TY_SHIFT type I .

  class-methods CALL_CE_FUNCTION
    importing
      !CONVEXIT type CLIKE
      !DIR type CLIKE
      !INPUT type CLIKE
    exporting
      !OUTPUT type CLIKE .
  class-methods CALL_CE_INPUT
    importing
      !CONVEXIT type CLIKE
      !INPUT type CLIKE
    exporting
      !OUTPUT type CLIKE .
  class-methods CALL_CE_OUTPUT
    importing
      !CONVEXIT type CLIKE
      !INPUT type CLIKE
    exporting
      !OUTPUT type CLIKE .
  class-methods GET_USER_DECIMAL_SEP
    returning
      value(DECIMAL_SEP) type CHAR1 .
  class-methods GET_USER_GROUP_SEP
    returning
      value(GROUP_SEP) type CHAR1 .
  class-methods GET_WAERS_DEC
    importing
      !WAERS type CLIKE
    returning
      value(CURRDEC) type CURRDEC
    raising
      ZCX_CE_T100 .
  class-methods GET_WAERS_SHIFT_IN
    importing
      !WAERS type CLIKE
    returning
      value(SHIFT) type TY_SHIFT
    raising
      ZCX_CE_T100 .
  class-methods GET_WAERS_SHIFT_OUT
    importing
      !WAERS type CLIKE
    returning
      value(SHIFT) type TY_SHIFT
    raising
      ZCX_CE_T100 .
  class-methods INPUT
    importing
      !INPUT type ANY
      !WAERS type ANY optional
      !MSEHI type ANY optional
    changing
      !OUTPUT type ANY
    raising
      ZCX_CE_T100 .
  class-methods OUTPUT_STRING
    importing
      !INPUT type ANY
      !WAERS type ANY optional
      !MSEHI type ANY optional
    returning
      value(OUTPUT) type STRING
    raising
      ZCX_CE_T100 .
  class-methods OUTPUT
    importing
      !INPUT type ANY
      !WAERS type ANY optional
      !MSEHI type ANY optional
    changing
      !OUTPUT type ANY
    raising
      ZCX_CE_T100 .
  protected section.
  private section.

    class-data:
      begin of s_usr_default
                    , x_dcpfm type xsdboolean
                    , dcpfm type xudcpfm
                    , decimal_sep type c
                    , group_sep type c
                    , end of s_usr_default .
    class-data:
      t_tcurx type table of tcurx .

    class-methods get_user_dec_notation
      returning
        value(dcpfm) type xudcpfm .
    class-methods input_packed
      importing
        !ref_ed_in type ref to cl_abap_elemdescr
        !input     type any
        !waers     type any optional
      exporting
        !output    type any
      raising
        zcx_ce_t100 .
    class-methods input_packed_curr
      importing
        !input  type any
        !waers  type any
      exporting
        !output type any
      raising
        zcx_ce_t100 .
    class-methods input_to_number
      importing
        !input  type any
      exporting
        !output type any .
    class-methods output_from_number
      importing
        !ref_td_out type ref to cl_abap_typedescr
        !input      type any
      exporting
        !output     type any
      raising
        zcx_ce_t100 .
    class-methods output_packed
      importing
        !input      type any
        !waers      type any optional
        !msehi      type any optional
        !ref_ed_out type ref to cl_abap_elemdescr
      exporting
        !output     type any
      raising
        zcx_ce_t100 .
    class-methods output_packed_curr
      importing
        !input  type any
        !waers  type any
      exporting
        !output type any
      raising
        zcx_ce_t100 .
    class-methods output_packed_other
      importing
        !input      type any
        !ref_ed_out type ref to cl_abap_elemdescr
      exporting
        !output     type any
      raising
        zcx_ce_t100 .
    class-methods output_packed_quan
      importing
        !input  type any
        !msehi  type any
      exporting
        !output type any
      raising
        zcx_ce_t100 .
ENDCLASS.



CLASS ZCL_CE IMPLEMENTATION.


  method call_ce_function.
    data(lv_funcname) = 'CONVERSION_EXIT_' && convexit && '_' && dir.

    call function lv_funcname
      exporting
        input  = input
      importing
        output = output.
  endmethod.


  method call_ce_input.
    call_ce_function( exporting input = input convexit = convexit dir = 'INPUT' importing output = output ).
  endmethod.


  method call_ce_output.
    call_ce_function( exporting input = input convexit = convexit dir = 'OUTPUT' importing output = output ).
    shift output right deleting trailing space.
    shift output left deleting leading space.
  endmethod.


  method get_user_decimal_sep.
    get_user_dec_notation( ).
    decimal_sep = s_usr_default-decimal_sep.
  endmethod.


  method get_user_dec_notation.
    if s_usr_default-x_dcpfm eq abap_false.
      call function 'SUSR_USER_DEFAULT_DEC_NOTATION'
        importing
          dcpfm = s_usr_default-x_dcpfm.
    endif.
    dcpfm = s_usr_default-x_dcpfm.
    case dcpfm.
      when 'X'.
        s_usr_default-decimal_sep = '.'.
        s_usr_default-group_sep = ','.
      when 'Y'.
        s_usr_default-decimal_sep = '.'.
        s_usr_default-group_sep = ' '.
      when others.
        s_usr_default-decimal_sep = ','.
        s_usr_default-group_sep = '.'.
    endcase.
  endmethod.


  method get_user_group_sep.
    get_user_dec_notation( ).
    group_sep = s_usr_default-group_sep.
  endmethod.


  method get_waers_dec.
    read table t_tcurx
     assigning field-symbol(<ls_tcurx>)
          with key currkey = waers.
    if sy-subrc ne 0.
      append initial line to t_tcurx assigning <ls_tcurx>.
      call function 'DD_GET_CURRENCY_INFO'
        exporting
          currency_key  = waers
        importing
          currency_info = <ls_tcurx>
        exceptions
          not_found     = 1
          others        = 2.
      case sy-subrc.
        when 0.
          "OK
        when 1.
          <ls_tcurx>-currkey = waers.
          <ls_tcurx>-currdec = 2.
        when others.
          zcx_ce_t100=>raise( msgid = 'ZCL_DE' msgno = '001' msgv1 = 'DD_GET_CURRENCY_INFO' ). "Un expected error calling function module &
      endcase.
    endif.
    currdec = <ls_tcurx>-currdec.
  endmethod.


  method get_waers_shift_in.
    shift = get_waers_dec( waers ) - 2.
  endmethod.


  method get_waers_shift_out.
    shift = 2 - get_waers_dec( waers ).
  endmethod.


  method input.
    data: ref_td_in type ref to cl_abap_typedescr
        , ref_ed_in type ref to cl_abap_elemdescr
        , ref_td_out type ref to cl_abap_typedescr
        , ref_ed_out type ref to cl_abap_elemdescr.

    ref_td_out = cl_abap_tabledescr=>describe_by_data( output ).
    ref_td_in  = cl_abap_tabledescr=>describe_by_data( input ).

    if ref_td_out->kind ne cl_abap_typedescr=>kind_elem.
      ##NO_TEXT
      zcx_ce_t100=>raise( msgid = 'ZCL_CE' msgno = '000' msgv1 = 'input' ).
    elseif  ref_td_in->kind ne cl_abap_typedescr=>kind_elem.
      ##NO_TEXT
      zcx_ce_t100=>raise( msgid = 'ZCL_CE' msgno = '000' msgv1 = 'output' ).
    else.
      ref_ed_out  ?= ref_td_out.
      ref_ed_in  ?= ref_td_in.
      if ref_ed_out->edit_mask is not initial.
        call_ce_input( exporting input = input convexit = ref_ed_out->edit_mask+2 importing output = output ).
      else.
        case ref_td_out->type_kind.
          when cl_abap_tabledescr=>typekind_char
               or cl_abap_tabledescr=>typekind_clike
               or cl_abap_tabledescr=>typekind_string.
            output = input.
          when cl_abap_tabledescr=>typekind_num.
            call_ce_input( exporting input = input convexit = 'ALPHA' importing output = output ).
          when cl_abap_tabledescr=>typekind_int
               or cl_abap_tabledescr=>typekind_int1
               or cl_abap_tabledescr=>typekind_int2
               or cl_abap_tabledescr=>typekind_int8
               or cl_abap_tabledescr=>typekind_float
               or cl_abap_tabledescr=>typekind_decfloat
               or cl_abap_tabledescr=>typekind_decfloat16
               or cl_abap_tabledescr=>typekind_decfloat34.
            input_to_number( exporting input = input importing output = output ).
          when cl_abap_tabledescr=>typekind_packed.
            input_packed( exporting       ref_ed_in = ref_ed_in
                                              input = input
                                              waers = waers
                                   importing output = output ).
          when cl_abap_tabledescr=>typekind_date.
            call_ce_input( exporting convexit = 'BCVDT' input = input importing output = output ).
          when cl_abap_tabledescr=>typekind_time.
            call_ce_input( exporting convexit = 'BCVTM' input = input importing output = output ).
        endcase.
      endif.
    endif.
  endmethod.


  method input_packed.
    data: lv_str_input type string
        , lv_amount_display type p length 16 decimals 4
        , lv_amount_internal type p length 16 decimals 4.
    case ref_ed_in->type_kind.
      when cl_abap_tabledescr=>typekind_char
     or cl_abap_tabledescr=>typekind_clike
     or cl_abap_tabledescr=>typekind_string.
        get_user_dec_notation( ).
        lv_str_input = input.
        replace all occurrences of s_usr_default-group_sep in lv_str_input with ''.
        replace all occurrences of s_usr_default-decimal_sep in lv_str_input with '.'.
        lv_amount_display = lv_str_input.
      when others.
        lv_amount_display = input.
    endcase.
    if waers is supplied and waers is not initial and waers ne ''.
      input_packed_curr( exporting input = lv_amount_display waers = waers importing output = lv_amount_internal ).
    else.
      lv_amount_internal = lv_amount_display.
    endif.
    output = lv_amount_internal.
  endmethod.


  method input_packed_curr.
    output = input * ipow( base = 10 exp = get_waers_shift_in( waers ) ).
  endmethod.


  method input_to_number.
    data: lv_str_input type string.

    lv_str_input = input.
    shift lv_str_input left deleting leading space.
    replace all occurrences of s_usr_default-group_sep in lv_str_input with ''.
    replace all occurrences of s_usr_default-decimal_sep in lv_str_input with '.'.
    output = lv_str_input.
  endmethod.


  method output.
    data: ref_td_in type ref to cl_abap_typedescr
        , ref_ed_in type ref to cl_abap_elemdescr
        , ref_td_out type ref to cl_abap_typedescr
        , ref_ed_out type ref to cl_abap_elemdescr.

    ref_td_out = cl_abap_typedescr=>describe_by_data( output ).
    ref_td_in  = cl_abap_typedescr=>describe_by_data( input ).

    if ref_td_out->kind ne cl_abap_typedescr=>kind_elem.
      ##NO_TEXT
      zcx_ce_t100=>raise( msgid = 'ZCL_CE' msgno = '000' msgv1 = 'input' ).
    elseif  ref_td_in->kind ne cl_abap_typedescr=>kind_elem.
      ##NO_TEXT
      zcx_ce_t100=>raise( msgid = 'ZCL_CE' msgno = '000' msgv1 = 'output' ).
    else.
      ref_ed_out  ?= ref_td_out.
      ref_ed_in  ?= ref_td_in.
      if ref_ed_in->edit_mask is not initial.
        call_ce_output( exporting input = input convexit = ref_ed_in->edit_mask+2 importing output = output ).
      else.
        case ref_td_in->type_kind.
          when cl_abap_typedescr=>typekind_char
               or cl_abap_typedescr=>typekind_clike
               or cl_abap_typedescr=>typekind_string.
            output = input.
          when cl_abap_typedescr=>typekind_num.
            call_ce_output( exporting input = input convexit = 'ALPHA' importing output = output ).
          when cl_abap_typedescr=>typekind_int
               or cl_abap_typedescr=>typekind_int1
               or cl_abap_typedescr=>typekind_int2
               or cl_abap_typedescr=>typekind_int8
               or cl_abap_typedescr=>typekind_float
               or cl_abap_typedescr=>typekind_decfloat
               or cl_abap_typedescr=>typekind_decfloat16
               or cl_abap_typedescr=>typekind_decfloat34.
            output_from_number( exporting ref_td_out = ref_td_out
                                                     input = input
                                          importing output = output ).
          when cl_abap_typedescr=>typekind_packed.
            output_packed( exporting     input = input
                                         waers = waers
                                         msehi = msehi
                                    ref_ed_out = ref_ed_out
                                    importing output = output ).
          when cl_abap_typedescr=>typekind_date.
            call_ce_output( exporting convexit = 'BCVDT' input = input importing output = output ).
          when cl_abap_typedescr=>typekind_time.
            call_ce_output( exporting convexit = 'BCVTM' input = input importing output = output ).
        endcase.
      endif.
    endif.
  endmethod.


  method output_from_number.
    data: lv_char128 type c length 128.
    if ref_td_out->type_kind eq cl_abap_elemdescr=>typekind_char
       or ref_td_out->type_kind eq cl_abap_elemdescr=>typekind_clike
           or ref_td_out->type_kind eq cl_abap_elemdescr=>typekind_string.
      write input to lv_char128 left-justified.
      shift lv_char128 left deleting leading space.
      output = lv_char128.
    else.
      output = input.
    endif.
  endmethod.


  method output_packed.
    if waers is supplied and waers is not initial and waers <> ''.
      output_packed_curr( exporting input = input waers = waers importing output = output ).
    elseif msehi is supplied and msehi is not initial and msehi <> ''.
      output_packed_quan( exporting     input = input
                                        msehi = msehi
                             importing output = output ).
    else.
      output_packed_other( exporting      input = input
                                     ref_ed_out = ref_ed_out
                          importing      output = output ).
    endif.
  endmethod.


  method output_packed_curr.
    data: lv_output_p   type p length 16 decimals 2
          , lv_output_str type c length 128
          , lv_dec type i.
    try.
        lv_output_p = input * ipow( base = 10 exp = get_waers_shift_out( waers ) ).
        lv_dec = get_waers_dec( waers ).
        write lv_output_p decimals lv_dec to lv_output_str.
        shift lv_output_str left deleting leading space.
        output = lv_output_str.
      catch cx_sy_arithmetic_overflow.
        zcx_ce_t100=>raise( msgid = 'ZCL_CE' msgno = '003' ). "msgv1 = input msgv2 = waers ). "Overflow output converting curr & &
    endtry.
  endmethod.


  method output_packed_other.
    if ref_ed_out->type_kind eq cl_abap_elemdescr=>typekind_char
       or ref_ed_out->type_kind eq cl_abap_elemdescr=>typekind_clike
           or ref_ed_out->type_kind eq cl_abap_elemdescr=>typekind_string.
      data: lv_char128 type c length 128.
      write input to lv_char128 left-justified style cl_abap_format=>o_simple.
      output = lv_char128.
    else.
      output = input.
    endif.
  endmethod.


  method output_packed_quan.
    data: lv_char128  type c length 128.

    write input unit msehi to lv_char128.
    shift lv_char128 left deleting leading space.
    output = lv_char128.
  endmethod.


  method output_string.
    try.
        output( exporting input = input
                          waers = waers
                          msehi = msehi
                 changing output = output ).
      catch zcx_ce_t100 into data(ref_ce_t100).
    endtry.
  endmethod.
ENDCLASS.
