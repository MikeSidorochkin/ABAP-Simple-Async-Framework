REPORT  zbc_async_demo.

TABLES: rzllitab.

CLASS lcl_task DEFINITION INHERITING FROM zcl_bc_async_task_base.
  PROTECTED SECTION.
    METHODS:
      start REDEFINITION,
      receive REDEFINITION.
ENDCLASS.                    "lcl_task DEFINITION

DATA:
  go_controller TYPE REF TO zcl_bc_async_controller,
  gt_tasks      TYPE zcl_bc_async_controller=>tt_tasks,
  go_task       TYPE REF TO lcl_task,
  gv_inc        TYPE i,
  gv_text       TYPE string,
  go_exception  TYPE REF TO zcx_bc_async_base.

DATA:
  gv_start_time TYPE sy-uzeit,
  gv_end_time   TYPE sy-uzeit,
  gv_diff       TYPE sy-uzeit.

PARAMETERS:
  p_s_grp TYPE rzllitab-classname DEFAULT 'parallel_generators' MATCHCODE OBJECT spta_server_group,
  p_tout  TYPE i DEFAULT 60,
  p_maxts TYPE i DEFAULT 0,
  p_perc  TYPE i DEFAULT 75.

SELECTION-SCREEN: SKIP.

PARAMETERS:
  p_calls TYPE i DEFAULT 20.

CLASS lcl_task IMPLEMENTATION.
  METHOD start.

    DATA lv_message TYPE msgv1.

    " Starting a new task
    CALL FUNCTION 'ZSB_PARALELL_TEST'
      STARTING NEW TASK mv_task_name
      DESTINATION IN GROUP mv_server_group
      CALLING receive_internal ON END OF TASK
      EXCEPTIONS
        communication_failure = 1 MESSAGE lv_message
        system_failure        = 2 MESSAGE lv_message
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

    gv_inc = gv_inc + 1.

  ENDMETHOD.                    "start

  METHOD receive.
    FIELD-SYMBOLS:
      <ls_task> TYPE zcl_bc_async_controller=>ty_task.

    ASSIGN mr_task->* TO <ls_task>.

    RECEIVE RESULTS FROM FUNCTION 'ZSB_PARALELL_TEST'
      IMPORTING
        pid    = <ls_task>-pid.
  ENDMETHOD.                    "receive
ENDCLASS.                    "lcl_task IMPLEMENTATION

FIELD-SYMBOLS:
  <ls_task> TYPE zcl_bc_async_controller=>ty_task.

START-OF-SELECTION.
  GET TIME FIELD gv_start_time.
  TRY.
      go_controller = NEW #( it_server_groups = VALUE zsbt_d7737_server_group( ( p_s_grp ) )
                             iv_max_wps       = p_maxts   " Maximum number of processes used (optional)
                             iv_max_percent   = p_perc    " Maximum load percentage (if iv_max_wps is not set)
                             iv_timeout       = p_tout ). " Resource availability timeout

      DO p_calls TIMES.
        CREATE OBJECT go_task
          EXPORTING
            io_controller = go_controller
            iv_name       = |{ sy-index }|.

        go_controller->add_task( go_task ).
      ENDDO.

      go_controller->start( ).
    CATCH zcx_bc_async_base INTO go_exception.
      gv_text = go_exception->get_text( ).
      WRITE gv_text.
      STOP.
  ENDTRY.

  gt_tasks = go_controller->get_tasks( ).

  WRITE: / 'Processes started: ', gv_inc.
  ULINE.

  LOOP AT gt_tasks ASSIGNING <ls_task>.
    WRITE: / 'Task name: ', <ls_task>-name(5), 'PID: ', <ls_task>-pid,  ' Start time: ', <ls_task>-start_time, ' End time: ' , <ls_task>-end_time,
             'Attempts: ', <ls_task>-count.
  ENDLOOP.

  GET TIME FIELD gv_end_time.
  gv_diff = gv_end_time - gv_start_time.
  ULINE.
  WRITE: /(50) 'Total time', gv_diff.
