class ZCX_CE_T100 definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  interfaces IF_T100_DYN_MSG .

  data MSGV1 type SYMSGV read-only .
  data MSGV2 type SYMSGV read-only .
  data MSGV3 type SYMSGV read-only .
  data MSGV4 type SYMSGV read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
  class-methods RAISE
    importing
      !MSGID type SYMSGID default SY-MSGID
      !MSGNO type SYMSGNO default SY-MSGNO
      !MSGV1 type SYMSGV default SY-MSGV1
      !MSGV2 type SYMSGV default SY-MSGV2
      !MSGV3 type SYMSGV default SY-MSGV3
      !MSGV4 type SYMSGV default SY-MSGV4
    raising
      ZCX_CE_T100 .
  methods GET_MSGID_MSGNO
    returning
      value(MSGID_MSGNO) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CE_T100 IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  method get_msgid_msgno.
    msgid_msgno = if_t100_message~t100key-msgid && if_t100_message~t100key-msgno.
  endmethod.


  method raise.
    data: lv_msgv1  type symsgv
        , lv_msgv2  type symsgv
        , lv_msgv3  type symsgv
        , lv_msgv4  type symsgv.

    zcl_ce=>output( exporting input = msgv1 changing output = lv_msgv1 ).
    zcl_ce=>output( exporting input = msgv2 changing output = lv_msgv2 ).
    zcl_ce=>output( exporting input = msgv3 changing output = lv_msgv3 ).
    zcl_ce=>output( exporting input = msgv4 changing output = lv_msgv4 ).

    raise exception type zcx_ce_t100
      exporting
        textid = value #( msgid = msgid msgno = msgno attr1 = 'MSGV1' attr2 = 'MSGV2' attr3 = 'MSGV3' attr4 = 'MSGV4' )
        msgv1  = lv_msgv1
        msgv2  = lv_msgv2
        msgv3  = lv_msgv3
        msgv4  = lv_msgv4.
  endmethod.
ENDCLASS.
