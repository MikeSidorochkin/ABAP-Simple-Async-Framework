class ZCX_BC_ASYNC_BASE definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of GROUP_NOT_FOUND,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MV_GROUP',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of GROUP_NOT_FOUND .
  constants:
    begin of RESOURCE_ERROR,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RESOURCE_ERROR .
  constants:
    begin of INITIAL_NAME,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INITIAL_NAME .
  constants:
    begin of INITIAL_CONTROLLER,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INITIAL_CONTROLLER .
  constants:
    begin of INITIALIZATION_ERROR,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INITIALIZATION_ERROR .
  constants:
    begin of MIN_WPS_RESOURCE_ERROR,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MIN_WPS_RESOURCE_ERROR .
  data MV_GROUP type RZLLITAB-CLASSNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_GROUP type RZLLITAB-CLASSNAME optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_BC_ASYNC_BASE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_GROUP = MV_GROUP .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
