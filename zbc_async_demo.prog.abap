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
    CALL FUNCTION 'ZBC_PARALELL_TEST'
      STARTING NEW TASK mv_task_name
      DESTINATION IN GROUP mv_server_group
      CALLING receive_internal ON END OF TASK
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        resource_failure      = 3
        OTHERS                = 4.
    rv_subrc = sy-subrc.
  ENDMETHOD.

  METHOD receive.
    RECEIVE RESULTS FROM FUNCTION 'ZBC_PARALELL_TEST'.
  ENDMETHOD.
ENDCLASS.

FIELD-SYMBOLS:
  <ls_task> TYPE zcl_bc_async_controller=>ty_task.

START-OF-SELECTION.
  DATA(go_timer) = cl_abap_runtime=>create_hr_timer( ).
  DATA(gv_start_time) = go_timer->get_runtime( ).

  TRY.
      DATA(go_controller) = NEW zcl_bc_async_controller( it_server_groups = VALUE #( ( CONV #( p_s_grp ) ) )
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
    WRITE: / 'Task name: ',   <ls_task>-name(5) ,
             'Start time: ',  |{ <ls_task>-start_time TIMESTAMP = ENVIRONMENT }|,
             'End time: ' ,   |{ <ls_task>-end_time TIMESTAMP = ENVIRONMENT }|.
  ENDLOOP.

  ULINE.
  WRITE: /(50) 'Total time (sec)', |{ ( go_timer->get_runtime( ) - gv_start_time ) / 1000000 }|.
