CLASS zcl_bc_async_task_base DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_bc_async_controller .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_name       TYPE guid_32
        !io_controller TYPE REF TO zcl_bc_async_controller
      RAISING
        zcx_bc_async_base .
    METHODS receive_internal
      IMPORTING
        !p_task TYPE guid_32 .
    METHODS get_name
      RETURNING
        VALUE(rv_name) TYPE guid_32 .
protected section.

  data MV_TASK_NAME type GUID_32 .
  data MO_CONTROLLER type ref to ZCL_BC_ASYNC_CONTROLLER .
  data MR_TASK type ref to ZCL_BC_ASYNC_CONTROLLER=>TY_TASK .
  data MV_SERVER_GROUP type RZLLITAB-CLASSNAME .

  methods START
    exporting
      value(EV_SUBRC) type SYSUBRC
      !EV_MESSAGE type TEXT255
    raising
      ZCX_BC_ASYNC_BASE
      ZCX_BC_ASYNC_NO_RESOURCES .
  methods RECEIVE
    raising
      CX_DYNAMIC_CHECK .
  methods START_INTERNAL
    importing
      !IR_TASK type ref to ZCL_BC_ASYNC_CONTROLLER=>TY_TASK
    raising
      ZCX_BC_ASYNC_BASE
      ZCX_BC_ASYNC_NO_RESOURCES .
  methods EXCLUDE_SERVER .
  methods CHECK_RFC_EXCEPTION
    importing
      !IV_SUBRC type SYSUBRC
      !IV_MESSAGE type TEXT255
    raising
      ZCX_BC_ASYNC_BASE
      ZCX_BC_ASYNC_NO_RESOURCES .
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_BC_ASYNC_TASK_BASE IMPLEMENTATION.


  METHOD check_rfc_exception.
    CASE iv_subrc.
      WHEN 1 OR 2.
        exclude_server( ).
      WHEN 0.
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_bc_async_no_resources
          EXPORTING
            textid     = zcx_bc_async_no_resources=>rfc_start_error
            mv_message = iv_message.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    IF iv_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_async_base
        EXPORTING
          textid = zcx_bc_async_base=>initial_name.
    ENDIF.

    IF io_controller IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_async_base
        EXPORTING
          textid = zcx_bc_async_base=>initial_controller.
    ENDIF.

    mv_task_name = iv_name.
    mv_server_group = io_controller->get_group( ).
    mo_controller = io_controller.
  ENDMETHOD.


  METHOD exclude_server.
    DATA:
      lv_rfc TYPE rfcdes-rfcdest.

    CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
      IMPORTING
        rfcdest = lv_rfc.

    CALL FUNCTION 'SPBT_DO_NOT_USE_SERVER'
      EXPORTING
        server_name = lv_rfc
      EXCEPTIONS
        OTHERS      = 0.
  ENDMETHOD.


  METHOD get_name.
    rv_name = mv_task_name.
  ENDMETHOD.


  METHOD receive.
  ENDMETHOD.


  METHOD receive_internal.
    FIELD-SYMBOLS:
      <ls_task> TYPE zcl_bc_async_controller=>ty_task.

    mo_controller->task_complete( ).

    ASSIGN mr_task->* TO <ls_task>.
    IF sy-subrc = 0.
      GET TIME STAMP FIELD <ls_task>-end_time.
    ENDIF.

    TRY.
        receive( ).

      CATCH cx_dynamic_check INTO DATA(lo_exception).
        IF <ls_task> IS ASSIGNED.
          <ls_task>-exception = lo_exception.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD start.
  ENDMETHOD.


  METHOD start_internal.
    mr_task = ir_task.
    start( IMPORTING ev_subrc   = DATA(lv_subrc)
                     ev_message = DATA(lv_message) ).

    check_rfc_exception( iv_subrc   = lv_subrc
                         iv_message = lv_message  ).
  ENDMETHOD.
ENDCLASS.
