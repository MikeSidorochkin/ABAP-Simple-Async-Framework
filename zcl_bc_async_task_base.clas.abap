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
  PROTECTED SECTION.

    DATA mv_task_name TYPE guid_32 .
    DATA mo_controller TYPE REF TO zcl_bc_async_controller .
    DATA mr_task TYPE REF TO zcl_bc_async_controller=>ty_task .
    DATA mv_server_group TYPE rzllitab-classname .

    METHODS start
      RETURNING
        VALUE(rv_subrc) TYPE sysubrc
      RAISING
        zcx_bc_async_base
        zcx_bc_async_no_resources .
    METHODS receive
      RAISING
        cx_dynamic_check .
    METHODS start_internal
      IMPORTING
        !ir_task TYPE REF TO zcl_bc_async_controller=>ty_task
      RAISING
        zcx_bc_async_base
        zcx_bc_async_no_resources .
    METHODS exclude_server .
    METHODS check_rfc_exception
      IMPORTING
        !iv_subrc TYPE sysubrc
      RAISING
        zcx_bc_async_base
        zcx_bc_async_no_resources .
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
            textid = zcx_bc_async_no_resources=>rfc_start_error.
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
    check_rfc_exception( start( ) ).
  ENDMETHOD.
ENDCLASS.
