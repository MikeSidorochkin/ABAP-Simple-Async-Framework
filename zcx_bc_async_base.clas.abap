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
    begin of INITIALIZE_ERROR,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INITIALIZE_ERROR .
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
    begin of INITIAL_TIMEOUT,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INITIAL_TIMEOUT .
  constants:
    begin of INITIAL_ATTEMPTS,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INITIAL_ATTEMPTS .
  constants:
    begin of RFC_SYSTEM_FAILURE,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'IV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RFC_SYSTEM_FAILURE .
  constants:
    begin of RFC_COMMUNICATION_FAILURE,
      msgid type symsgid value 'ZBC_ASYNC',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'IV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RFC_COMMUNICATION_FAILURE .
  data MV_GROUP type RZLLITAB-CLASSNAME .
  data IV_MSGV1 type MSGV1 .
  data IV_MSGV2 type MSGV2 .
  data IV_MSGV3 type MSGV3 .
  data IV_MSGV4 type MSGV4 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_GROUP type RZLLITAB-CLASSNAME optional
      !IV_MSGV1 type MSGV1 optional
      !IV_MSGV2 type MSGV2 optional
      !IV_MSGV3 type MSGV3 optional
      !IV_MSGV4 type MSGV4 optional .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCX_BC_ASYNC_BASE IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->mv_group = mv_group .
    me->iv_msgv1 = iv_msgv1 .
    me->iv_msgv2 = iv_msgv2 .
    me->iv_msgv3 = iv_msgv3 .
    me->iv_msgv4 = iv_msgv4 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
