*----------------------------------------------------------------------*
*       CLASS ZCL_BC_ASYNC_CONTROLLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_bc_async_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_bc_async_task_base .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_task,
        task       TYPE REF TO zcl_bc_async_task_base,
        name       TYPE guid_32,
        pid        TYPE wpinfo-wp_pid,
        start_time TYPE t,
        start_date TYPE d,
        end_time   TYPE t,
        end_date   TYPE d,
        exception  TYPE REF TO cx_root,
        text       TYPE string,
        count      TYPE i,
      END OF ty_task .
    TYPES:
      tt_tasks TYPE STANDARD TABLE OF ty_task WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !it_server_groups TYPE zsbt_d7737_server_group OPTIONAL
        !iv_timeout       TYPE i DEFAULT 60
        !iv_max_wps       TYPE i OPTIONAL
        !iv_max_attempts  TYPE i DEFAULT 10
        !iv_max_percent   TYPE i DEFAULT 0
        !iv_reserved_wps  TYPE i DEFAULT 0
      RAISING
        zcx_bc_async_base .
    METHODS add_task
      IMPORTING
        !io_task TYPE REF TO zcl_bc_async_task_base .
    METHODS clear_tasks .
    METHODS start
      RAISING
        zcx_bc_async_base .
    METHODS get_tasks
      RETURNING
        VALUE(rt_tasks) TYPE tt_tasks .
    METHODS get_group
      RETURNING
        VALUE(rv_group) TYPE rzllitab-classname .
  PROTECTED SECTION.
    DATA mt_tasks TYPE tt_tasks .
  PRIVATE SECTION.
    DATA mv_server_group TYPE rzllitab-classname .
    DATA mv_running_tasks TYPE i .
    DATA mv_finished_tasks TYPE i .
    DATA mv_task_timeout TYPE i .
    DATA mv_max_tasks TYPE decfloat16 .
    DATA mv_max_attempts TYPE i .

    METHODS task_complete .
    METHODS get_next_task
      RETURNING
        VALUE(rs_task) TYPE REF TO zcl_bc_async_controller=>ty_task .
ENDCLASS.



CLASS ZCL_BC_ASYNC_CONTROLLER IMPLEMENTATION.


  METHOD add_task.
    FIELD-SYMBOLS:
      <ls_task> TYPE ty_task.

    CHECK io_task IS BOUND.

    APPEND INITIAL LINE TO mt_tasks ASSIGNING <ls_task>.
    <ls_task>-task = io_task.
    <ls_task>-name = io_task->get_name( ).
  ENDMETHOD.                    "add_task


  METHOD clear_tasks.
    CLEAR mt_tasks.
  ENDMETHOD.                    "CLEAR_TASKS


  METHOD constructor.
    DATA: lv_free_wps      TYPE i,
          lt_server_groups LIKE it_server_groups,
          lv_group_index   TYPE i.

    FIELD-SYMBOLS <lv_server_group> LIKE LINE OF lt_server_groups.

    lt_server_groups = it_server_groups.
    IF lt_server_groups IS INITIAL.
      APPEND 'parallel_generators' TO lt_server_groups.
    ENDIF.

    LOOP AT lt_server_groups ASSIGNING <lv_server_group>.
      lv_group_index = sy-tabix.

      CALL FUNCTION 'SPBT_INITIALIZE'
        EXPORTING
          group_name                     = <lv_server_group>
        IMPORTING
          free_pbt_wps                   = lv_free_wps
        EXCEPTIONS
          invalid_group_name             = 1
          internal_error                 = 2
          pbt_env_already_initialized    = 3
          currently_no_resources_avail   = 4
          no_pbt_resources_found         = 5
          cant_init_different_pbt_groups = 6
          OTHERS                         = 7.

      CASE sy-subrc.
        WHEN 0.
          EXIT.
        WHEN 1.
          RAISE EXCEPTION TYPE zcx_bc_async_base
            EXPORTING
              textid = zcx_bc_async_base=>group_not_found.
        WHEN 3.
          CALL FUNCTION 'SPBT_GET_CURR_RESOURCE_INFO'
            IMPORTING
              free_pbt_wps = lv_free_wps
            EXCEPTIONS
              OTHERS       = 1.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_bc_async_base
              EXPORTING
                textid = zcx_bc_async_base=>initialize_error.
          ENDIF.
        WHEN 2 OR 6.
          RAISE EXCEPTION TYPE zcx_bc_async_base
            EXPORTING
              textid = zcx_bc_async_base=>initialize_error.
        WHEN OTHERS.
          IF lv_group_index = lines( lt_server_groups ).
            RAISE EXCEPTION TYPE zcx_bc_async_base
              EXPORTING
                textid = zcx_bc_async_base=>resource_error.
          ELSE.
            CONTINUE.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    DATA(lv_unused_wps) = COND i( WHEN iv_reserved_wps IS NOT INITIAL THEN iv_reserved_wps ELSE 5 ).
    DATA(lv_max_safe_wps) = nmax( val1 = ( lv_free_wps / 2 ) val2 = ( lv_free_wps - lv_unused_wps ) ).

    IF iv_max_wps > 0.
      mv_max_tasks = iv_max_wps.
    ELSEIF iv_max_percent IS NOT INITIAL AND iv_max_percent BETWEEN 1 AND 100.
      mv_max_tasks = round( val = ( lv_free_wps / 100 ) * iv_max_percent dec = 0 mode = cl_abap_math=>round_down ).
    ELSE.
      mv_max_tasks = lv_max_safe_wps.
    ENDIF.

    IF iv_reserved_wps > 0 AND iv_reserved_wps >= mv_max_tasks.
      RAISE EXCEPTION TYPE zcx_bc_async_base
        EXPORTING
          textid = zcx_bc_async_base=>resource_error.
    ENDIF.

    mv_server_group = <lv_server_group>.

    IF iv_max_attempts IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_async_base
        EXPORTING
          textid = zcx_bc_async_base=>initial_attempts.
    ENDIF.

    mv_max_attempts = iv_max_attempts.

    IF iv_timeout IS INITIAL.
      RAISE EXCEPTION TYPE zcx_bc_async_base
        EXPORTING
          textid = zcx_bc_async_base=>initial_timeout.
    ENDIF.

    mv_task_timeout = iv_timeout.
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_group.
    rv_group = mv_server_group.
  ENDMETHOD.                    "GET_GROUP


  METHOD get_next_task.
    FIELD-SYMBOLS: <ls_task> TYPE zcl_bc_async_controller=>ty_task.

    LOOP AT mt_tasks ASSIGNING <ls_task> WHERE start_date IS INITIAL.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc = 0.

    GET REFERENCE OF <ls_task> INTO rs_task.
  ENDMETHOD.                    "get_next_task


  METHOD get_tasks.
    rt_tasks = mt_tasks.
  ENDMETHOD.                    "GET_TASKS


  METHOD start.
    DATA:
      lv_started_tasks TYPE i,
      lv_max_wps       TYPE i,
      lo_resource      TYPE REF TO zcx_bc_async_no_resources,
      lr_task          TYPE REF TO ty_task.

    FIELD-SYMBOLS:
      <ls_task> TYPE ty_task.

    mv_running_tasks = 0.
    mv_finished_tasks = 0.

    DEFINE close_task.
      <ls_task>-start_date = sy-datum.
      <ls_task>-start_time = sy-uzeit.
      <ls_task>-end_date = sy-datum.
      <ls_task>-end_time = sy-uzeit.
      lv_started_tasks = lv_started_tasks + 1.
      mv_finished_tasks = mv_finished_tasks + 1.
    END-OF-DEFINITION.

    "*  check_quota( ).

    WHILE lv_started_tasks <> lines( mt_tasks ).
      CLEAR lr_task.

      IF mv_running_tasks >= mv_max_tasks AND mv_max_tasks IS NOT INITIAL.
        WAIT FOR ASYNCHRONOUS TASKS UNTIL mv_running_tasks < mv_max_tasks.
      ENDIF.

      lr_task = get_next_task( ).
      IF lr_task IS INITIAL.
        EXIT.
      ENDIF.
      ASSIGN lr_task->* TO <ls_task>.

      TRY.
          <ls_task>-task->start_internal( lr_task ).
          <ls_task>-start_date = sy-datum.
          <ls_task>-start_time = sy-uzeit.
          mv_running_tasks = mv_running_tasks + 1.
          lv_started_tasks = lv_started_tasks + 1.
        CATCH zcx_bc_async_no_resources INTO lo_resource.

          WAIT FOR ASYNCHRONOUS TASKS UNTIL mv_finished_tasks >= lv_started_tasks
               UP TO mv_task_timeout SECONDS.

          IF sy-subrc = 0.

            <ls_task>-count = <ls_task>-count + 1.
            IF <ls_task>-count >= mv_max_attempts.
              close_task.
              <ls_task>-exception = lo_resource.
              <ls_task>-text = <ls_task>-exception->get_text( ).
            ENDIF.
          ELSE.
            close_task.
            <ls_task>-exception = lo_resource.
            <ls_task>-text = <ls_task>-exception->get_text( ).
          ENDIF.

        CATCH cx_root INTO <ls_task>-exception.
          <ls_task>-text = <ls_task>-exception->get_text( ).

      ENDTRY.

    ENDWHILE.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL mv_finished_tasks >= lv_started_tasks.
  ENDMETHOD.                    "start


  METHOD task_complete.
    mv_finished_tasks = mv_finished_tasks + 1.
    mv_running_tasks = mv_running_tasks - 1.
  ENDMETHOD.                    "TASK_COMPLETE
ENDCLASS.
