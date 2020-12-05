class ZCX_BC_ASYNC_NO_RESOURCES definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

*"* public components of class ZCX_SB_D7737_RESOURCE
*"* do not include other source files here!!!
  interfaces IF_T100_MESSAGE .

  constants:
    begin of RFC_START_ERROR,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MV_MESSAGE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RFC_START_ERROR .
  constants:
    begin of TIMEOUT,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TIMEOUT .
  constants:
    begin of ATTEMPTS_EXCEEDED,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ATTEMPTS_EXCEEDED .
  data MV_MESSAGE type TEXT255 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MESSAGE type TEXT255 optional .
protected section.
*"* protected components of class ZCX_SB_D7737_RESOURCE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_SB_D7737_RESOURCE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_BC_ASYNC_NO_RESOURCES IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_MESSAGE = MV_MESSAGE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
