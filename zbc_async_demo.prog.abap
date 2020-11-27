REPORT  zbc_async_demo.

CLASS lcl_task DEFINITION INHERITING FROM zcl_bc_async_task_base.
  PROTECTED SECTION.
    METHODS:
      start REDEFINITION,
      receive REDEFINITION.
ENDCLASS.

PARAMETERS:
  p_s_grp TYPE rzlli_apcl DEFAULT 'parallel_generators' MATCHCODE OBJECT spta_server_group,
  p_perc  TYPE i DEFAULT 75.

SELECTION-SCREEN: SKIP.

PARAMETERS:
  p_calls TYPE i DEFAULT 20.

CLASS lcl_task IMPLEMENTATION.
  METHOD start.
    " Starting a new task
    CALL FUNCTION 'ZSB_PARALELL_TEST'
      STARTING NEW TASK mv_task_name
      DESTINATION IN GROUP mv_server_group
      CALLING receive_internal ON END OF TASK
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        resource_failure      = 3
        OTHERS                = 4.

    CASE sy-subrc.
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

  METHOD receive.
    FIELD-SYMBOLS:
      <ls_task> TYPE zcl_bc_async_controller=>ty_task.

    ASSIGN mr_task->* TO <ls_task>.

    RECEIVE RESULTS FROM FUNCTION 'ZSB_PARALELL_TEST'
      IMPORTING
        pid    = <ls_task>-pid.
  ENDMETHOD.
ENDCLASS.

FIELD-SYMBOLS:
  <ls_task> TYPE zcl_bc_async_controller=>ty_task.

START-OF-SELECTION.
  DATA(go_timer) = cl_abap_runtime=>create_hr_timer( ).
  DATA(gv_start_time) = go_timer->get_runtime( ).

  TRY.
      DATA(go_controller) = NEW zcl_bc_async_controller( it_server_groups = VALUE zsbt_d7737_server_group( ( p_s_grp ) )
                                                         iv_max_percent   = p_perc ).

      DO p_calls TIMES.
        go_controller->add_task( NEW lcl_task( io_controller = go_controller
                                               iv_name       = |{ sy-index }| ) ).
      ENDDO.

      go_controller->start( ).
    CATCH zcx_bc_async_base INTO DATA(go_exception).
      WRITE go_exception->get_text( ).
      STOP.
  ENDTRY.

  DATA(gt_tasks) = go_controller->get_tasks( ).

  WRITE: / 'Processes started: ', p_calls.
  ULINE.

  LOOP AT gt_tasks ASSIGNING <ls_task>.
    WRITE: / 'Task name: ',   <ls_task>-name(5),
             'PID: ',         <ls_task>-pid,
             'Start time: ',  <ls_task>-start_time,
             'End time: ' ,   <ls_task>-end_time.
  ENDLOOP.

  ULINE.
  WRITE: /(50) 'Total time (sec)', |{ ( go_timer->get_runtime( ) - gv_start_time ) / 1000000 }|.
