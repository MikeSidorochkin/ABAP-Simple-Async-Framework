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
      ZCX_BC_ASYNC_RFC_ERROR .
  methods RECEIVE
    exporting
      !EV_SUBRC type SYSUBRC
      !EV_MESSAGE type TEXT255 .
  methods START_INTERNAL
    importing
      !IR_TASK type ref to ZCL_BC_ASYNC_CONTROLLER=>TY_TASK
    raising
      ZCX_BC_ASYNC_RFC_ERROR .
  methods EXCLUDE_SERVER .
  methods CHECK_START_RFC_EXCEPTION
    importing
      !IV_SUBRC type SYSUBRC
      !IV_MESSAGE type TEXT255
    raising
      ZCX_BC_ASYNC_RFC_ERROR .
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_BC_ASYNC_TASK_BASE IMPLEMENTATION.


  METHOD check_start_rfc_exception.
    FIELD-SYMBOLS:
      <ls_task> TYPE zcl_bc_async_controller=>ty_task.

    CASE iv_subrc.
      WHEN 0.
        ASSIGN mr_task->* TO <ls_task>.
        CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
          IMPORTING
            rfcdest = <ls_task>-rfcdest.
      WHEN zcl_bc_async_controller=>gc_rfc_errors-communication_failure OR
           zcl_bc_async_controller=>gc_rfc_errors-system_failure.
        exclude_server( ).
      WHEN zcl_bc_async_controller=>gc_rfc_errors-resource_failure.
        RAISE EXCEPTION TYPE zcx_bc_async_rfc_error
          EXPORTING
            textid     = zcx_bc_async_rfc_error=>rfc_start_error
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
      lv_rfc         TYPE rfcdes-rfcdest,
      lv_server_name TYPE pbtsrvname.

    CHECK mo_controller->is_need_to_exclude_servers( ) = abap_true.

    CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
      IMPORTING
        rfcdest = lv_rfc.

    lv_server_name = lv_rfc.

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
        receive( IMPORTING ev_subrc   = DATA(lv_subrc)
                           ev_message = DATA(lv_message) ).

        IF lv_subrc <> 0.
          IF lv_subrc = zcl_bc_async_controller=>gc_rfc_errors-communication_failure.
            exclude_server( ).
          ENDIF.

          RAISE EXCEPTION TYPE zcx_bc_async_rfc_error
            EXPORTING
              textid     = zcx_bc_async_rfc_error=>rfc_receive_error
              mv_message = lv_message.
        ENDIF.

      CATCH zcx_bc_async_rfc_error INTO DATA(lo_exception).
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

    check_start_rfc_exception( iv_subrc   = lv_subrc
                               iv_message = lv_message  ).
  ENDMETHOD.
ENDCLASS.
