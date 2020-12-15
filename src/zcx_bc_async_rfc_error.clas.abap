class ZCX_BC_ASYNC_RFC_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

*"* public components of class ZCX_SB_D7737_RESOURCE
*"* do not include other source files here!!!
  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF rfc_start_error,
        msgid TYPE symsgid VALUE 'ZBC_ASYNC',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MV_MESSAGE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF rfc_start_error .
  constants:
    BEGIN OF timeout,
        msgid TYPE symsgid VALUE 'ZBC_ASYNC',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF timeout .
  constants:
    BEGIN OF attempts_exceeded,
        msgid TYPE symsgid VALUE 'ZBC_ASYNC',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF attempts_exceeded .
  constants:
    begin of RFC_RECEIVE_ERROR,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MV_MESSAGE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RFC_RECEIVE_ERROR .
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



CLASS ZCX_BC_ASYNC_RFC_ERROR IMPLEMENTATION.


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
