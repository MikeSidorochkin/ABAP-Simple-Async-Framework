*----------------------------------------------------------------------*
*       CLASS ZCL_BC_ASYNC_CONTROLLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_BC_ASYNC_CONTROLLER definition
  public
  final
  create public

  global friends ZCL_BC_ASYNC_TASK_BASE .

public section.

  types:
    BEGIN OF ty_task,
        task       TYPE REF TO zcl_bc_async_task_base,
        name       TYPE guid_32,
        start_time TYPE t,
        start_date TYPE d,
        end_time   TYPE t,
        end_date   TYPE d,
        exception  TYPE REF TO cx_root,
        attempts   TYPE i,
      END OF ty_task .
  types:
    tt_tasks TYPE STANDARD TABLE OF ty_task WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IT_SERVER_GROUPS type STRING_TABLE optional
      !IV_TIMEOUT type I default 60
      !IV_MAX_WPS type I optional
      !IV_MAX_ATTEMPTS type I default 10
      !IV_MAX_PERCENT type I default 0
      !IV_RESERVED_WPS type I default 0
    raising
      ZCX_BC_ASYNC_BASE .
  methods ADD_TASK
    importing
      !IO_TASK type ref to ZCL_BC_ASYNC_TASK_BASE .
  methods CLEAR_TASKS .
  methods START
    raising
      ZCX_BC_ASYNC_BASE .
  methods GET_TASKS
    returning
      value(RT_TASKS) type TT_TASKS .
  methods GET_GROUP
    returning
      value(RV_GROUP) type RZLLITAB-CLASSNAME .
  PROTECTED SECTION.
    DATA mt_tasks TYPE tt_tasks .
private section.

  data MV_SERVER_GROUP type RZLLITAB-CLASSNAME .
  data MV_RUNNING_TASKS type I .
  data MV_FINISHED_TASKS type I .
  data MV_TASK_TIMEOUT type I .
  data MV_MAX_TASKS type DECFLOAT16 .
  data MV_MAX_ATTEMPTS type I .

  methods CLOSE_TASK
    importing
      !IO_EXCEPTION type ref to CX_ROOT
    changing
      !CS_TASK type TY_TASK .
  methods TASK_COMPLETE .
  methods GET_NEXT_TASK
    returning
      value(RS_TASK) type ref to ZCL_BC_ASYNC_CONTROLLER=>TY_TASK .
ENDCLASS.



CLASS ZCL_BC_ASYNC_CONTROLLER IMPLEMENTATION.


  METHOD add_task.
    CHECK io_task IS BOUND.

    APPEND VALUE #( task = io_task
                    name = io_task->get_name( ) ) TO mt_tasks.
  ENDMETHOD.


  METHOD clear_tasks.
    CLEAR mt_tasks.
  ENDMETHOD.


  METHOD close_task.
    cs_task-start_date  = sy-datum.
    cs_task-start_time  = sy-uzeit.
    cs_task-end_date    = sy-datum.
    cs_task-end_time    = sy-uzeit.
    mv_finished_tasks   = mv_finished_tasks + 1.
    cs_task-exception   = io_exception.
  ENDMETHOD.


  METHOD constructor.
    DATA: lv_free_wps      TYPE i,
          lt_server_groups TYPE STANDARD TABLE OF rzlli_apcl,
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
  ENDMETHOD.


  METHOD get_group.
    rv_group = mv_server_group.
  ENDMETHOD.


  METHOD get_next_task.
    LOOP AT mt_tasks ASSIGNING FIELD-SYMBOL(<ls_task>) WHERE start_date IS INITIAL.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc = 0.

    GET REFERENCE OF <ls_task> INTO rs_task.
  ENDMETHOD.


  METHOD get_tasks.
    rt_tasks = mt_tasks.
  ENDMETHOD.


  METHOD start.
    DATA:
      lv_started_tasks TYPE i,
      lv_max_wps       TYPE i.

    FIELD-SYMBOLS:
      <ls_task> TYPE ty_task.

    mv_running_tasks = 0.
    mv_finished_tasks = 0.

    WHILE lv_started_tasks <> lines( mt_tasks ).
      IF mv_running_tasks >= mv_max_tasks AND mv_max_tasks IS NOT INITIAL.
        WAIT FOR ASYNCHRONOUS TASKS UNTIL mv_running_tasks < mv_max_tasks.
      ENDIF.

      DATA(lr_task) = get_next_task( ).
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
        CATCH zcx_bc_async_no_resources INTO DATA(lo_exception).

          WAIT FOR ASYNCHRONOUS TASKS UNTIL mv_finished_tasks >= lv_started_tasks
               UP TO mv_task_timeout SECONDS.

          IF sy-subrc = 0.
            <ls_task>-attempts = <ls_task>-attempts + 1.
            IF <ls_task>-attempts >= mv_max_attempts.
              close_task(
                EXPORTING
                  io_exception = lo_exception
                CHANGING
                  cs_task      = <ls_task> ).
              lv_started_tasks = lv_started_tasks + 1.
            ENDIF.
          ELSE.
            close_task(
              EXPORTING
                io_exception = lo_exception
              CHANGING
                cs_task      = <ls_task> ).
            lv_started_tasks = lv_started_tasks + 1.
          ENDIF.

        CATCH cx_root INTO <ls_task>-exception.
      ENDTRY.

    ENDWHILE.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL mv_finished_tasks >= lv_started_tasks.
  ENDMETHOD.


  METHOD task_complete.
    mv_finished_tasks = mv_finished_tasks + 1.
    mv_running_tasks = mv_running_tasks - 1.
  ENDMETHOD.
ENDCLASS.
