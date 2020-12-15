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
*      EXPORTING it_data = lt_demo_data " The demo scenario does not require data transfer
      EXCEPTIONS
        communication_failure = zcl_bc_async_controller=>gc_rfc_errors-communication_failure MESSAGE ev_message
        system_failure        = zcl_bc_async_controller=>gc_rfc_errors-system_failure        MESSAGE ev_message
        resource_failure      = zcl_bc_async_controller=>gc_rfc_errors-resource_failure
        OTHERS                = 4.
    ev_subrc = sy-subrc.
  ENDMETHOD.

  METHOD receive.
    RECEIVE RESULTS FROM FUNCTION 'ZBC_PARALELL_TEST'
      EXCEPTIONS
        communication_failure = zcl_bc_async_controller=>gc_rfc_errors-communication_failure MESSAGE ev_message
        system_failure        = zcl_bc_async_controller=>gc_rfc_errors-system_failure        MESSAGE ev_message.

    ev_subrc = sy-subrc.
*      IMPORTING et_data = lt_demo_data " The demo scenario does not require data transfer
  ENDMETHOD.
ENDCLASS.

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

      go_controller->start( 'Running asynchronous tasks...'(001) ).
    CATCH zcx_bc_async_base INTO DATA(go_exception).
      WRITE go_exception->get_text( ).
      STOP.
  ENDTRY.

  DATA(gt_tasks) = go_controller->get_tasks( ).

  WRITE: / 'Processes started: ', p_calls.
  ULINE.

  LOOP AT gt_tasks ASSIGNING FIELD-SYMBOL(<ls_task>).
    WRITE: / 'Task name: ',   <ls_task>-name(5),
             'RFC Dest: ',    <ls_task>-rfcdest,
             'Start time: ',  |{ <ls_task>-start_time TIMESTAMP = ENVIRONMENT }|,
             'End time: ' ,   |{ <ls_task>-end_time TIMESTAMP = ENVIRONMENT }|,
             'Error: ',       COND string( WHEN <ls_task>-exception IS BOUND THEN <ls_task>-exception->get_text( ) ELSE `-` ).
  ENDLOOP.

  ULINE.

  DATA(gv_time) = VALUE t( ).
  gv_time = ( go_timer->get_runtime( ) - gv_start_time ) / 1000000.

  WRITE: /(50) 'Total time:', |{ gv_time TIME = USER }|.
