CLASS ZCL_BC_ASYNC_TASK_BASE DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_bc_async_controller .

  PUBLIC SECTION.
*"* public components of class ZCL_SB_D7737_ASYNC_TASK_BASE
*"* do not include other source files here!!!

    METHODS constructor
      IMPORTING
        !iv_name TYPE guid_32
        !io_controller TYPE REF TO zcl_bc_async_controller
      RAISING
        zcx_bc_async_base .
    METHODS receive_internal
      IMPORTING
        !p_task TYPE guid_32 .
    METHODS get_name
      RETURNING
        value(rv_name) TYPE guid_32 .
protected section.

*"* protected components of class ZCL_SB_D7737_ASYNC_TASK_BASE
*"* do not include other source files here!!!
  data MV_TASK_NAME type GUID_32 .
  data MO_CONTROLLER type ref to ZCL_BC_ASYNC_CONTROLLER .
  data MR_TASK type ref to ZCL_BC_ASYNC_CONTROLLER=>TY_TASK .
  data MV_SERVER_GROUP type RZLLITAB-CLASSNAME .

  methods START
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
  PRIVATE SECTION.
*"* private components of class ZCL_SB_D7737_ASYNC_TASK_BASE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_BC_ASYNC_TASK_BASE IMPLEMENTATION.


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
  ENDMETHOD.                    "constructor


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
  ENDMETHOD.                    "GET_NAME


  METHOD receive.
  ENDMETHOD.                    "RECEIVE


  METHOD receive_internal.
    DATA:
      lo_exception TYPE REF TO cx_dynamic_check.

    FIELD-SYMBOLS:
      <ls_task> TYPE zcl_bc_async_controller=>ty_task.

    mo_controller->task_complete( ).

    ASSIGN mr_task->* TO <ls_task>.
    IF sy-subrc = 0.
      <ls_task>-end_date = sy-datum.
      <ls_task>-end_time = sy-uzeit.
    ENDIF.

    TRY.
        receive( ).

      CATCH cx_dynamic_check INTO lo_exception.
        " При исключении записываем текст ошибки
        IF <ls_task> IS ASSIGNED.
          <ls_task>-text = lo_exception->get_text( ).
          <ls_task>-exception = lo_exception.
        ENDIF.
    ENDTRY.

  ENDMETHOD.                    "receive_internal


  METHOD start.
  ENDMETHOD.                    "START


  METHOD start_internal.
    mr_task = ir_task.

    start( ).
  ENDMETHOD.                    "start_internal
ENDCLASS.
