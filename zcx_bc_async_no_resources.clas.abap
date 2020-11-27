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
      msgid type symsgid value 'ZHR_S0654_PARAL',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RFC_START_ERROR .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
protected section.
*"* protected components of class ZCX_SB_D7737_RESOURCE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_SB_D7737_RESOURCE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_BC_ASYNC_NO_RESOURCES IMPLEMENTATION.


method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.
ENDCLASS.
