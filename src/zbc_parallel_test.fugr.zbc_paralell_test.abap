FUNCTION ZBC_PARALELL_TEST.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IV_DUMMY) TYPE  CHAR10 OPTIONAL
*"----------------------------------------------------------------------

  DO 88888898 TIMES.
    sy-index = sy-index * 2.
  ENDDO.

ENDFUNCTION.
