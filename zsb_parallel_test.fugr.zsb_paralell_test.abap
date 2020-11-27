FUNCTION ZSB_PARALELL_TEST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(PID) TYPE  WPINFO-WP_PID
*"----------------------------------------------------------------------


  DO 88888898 TIMES.
    sy-index = sy-index * 2.
  ENDDO.

  CALL FUNCTION 'TH_GET_OWN_WP_NO'
    IMPORTING
      wp_pid = pid.



ENDFUNCTION.
