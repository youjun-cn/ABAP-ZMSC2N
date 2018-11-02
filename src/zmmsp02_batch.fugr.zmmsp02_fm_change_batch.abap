FUNCTION zmmsp02_fm_change_batch.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_TEST) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_MCHA) TYPE  MCHA OPTIONAL
*"     REFERENCE(I_LGORT) TYPE  MCHB-LGORT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_SUBRC) TYPE  SY-SUBRC
*"     REFERENCE(E_MESSAGE) TYPE  BAPIRET2-MESSAGE
*"  TABLES
*"      T_CLBATCH STRUCTURE  CLBATCH OPTIONAL
*"      T_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
  DATA:
      lt_changed_batch TYPE TABLE OF mcha,
      lt_zimseg        TYPE TABLE OF  imseg,
      lt_return        TYPE TABLE OF  bapiret2,
      l_subrc LIKE sy-subrc.
  DATA : lv_loop   TYPE i,
         lv_line_s TYPE i VALUE 0,
         lv_line_e TYPE i,
         lv_objek  TYPE objnum,
         lv_obtab  TYPE tabelle,
         lv_klart  TYPE klassenart,
         lv_class  TYPE klasse_d,
         lv_flag   TYPE c.
  DATA lv_mess TYPE bapiret2-message.
  DATA lv_mess1 TYPE bapiret2-message.
  DATA lv TYPE i.
  DATA : lv_index1 TYPE i VALUE 0,
         lv_index2 TYPE i VALUE 0,
         lv_index  TYPE i VALUE 0.
  APPEND  i_mcha TO lt_changed_batch.
  CALL FUNCTION 'VB_CHANGE_BATCH'
    EXPORTING
      ymcha                       = i_mcha
      change_lgort                = i_lgort
      kzcla                       = '1'
      xkcfc                       = 'X'
    TABLES
      char_of_batch               = t_clbatch
      changed_batch               = lt_changed_batch
      zimseg                      = lt_zimseg
      return                      = lt_return
    EXCEPTIONS
      no_material                 = 1
      no_batch                    = 2
      no_plant                    = 3
      material_not_found          = 4
      plant_not_found             = 5
      lock_on_material            = 6
      lock_on_plant               = 7
      lock_on_batch               = 8
      lock_system_error           = 9
      no_authority                = 10
      batch_not_exist             = 11
      no_class                    = 12
      error_in_classification     = 13
      error_in_valuation_change   = 14
      error_in_status_change      = 15
      region_of_origin_not_found  = 16
      country_of_origin_not_found = 17
      OTHERS                      = 18.
  l_subrc = sy-subrc.
  t_return[] = lt_return[].
*  LOOP AT t_return WHERE type ='E' or type ='W'.
*
*  ENDLOOP.
  READ TABLE t_return WITH KEY type ='E'.
  IF sy-subrc = 0 OR l_subrc <> 0..
*获取错误消息
    SELECT SINGLE text
     FROM t100
     INTO lv_mess
     WHERE arbgb = sy-msgid
     AND msgnr = sy-msgno
     AND sprsl = sy-langu.
*拼接错误消息
    lv = strlen( lv_mess ) .
    lv_mess1 = lv_mess.
    DO lv TIMES.
      lv_index1 = lv_index1 + 1.
      lv_index = lv_index1 - 1.
      IF lv_mess+lv_index(1) = '&' .
        lv_index2 = lv_index2 + 1.
        IF lv_index2 = 1.
          REPLACE '&'  IN lv_mess1 WITH sy-msgv1.
        ELSEIF lv_index2 = 2.
          REPLACE '&'  IN lv_mess1 WITH sy-msgv2.
        ELSEIF lv_index2 = 3.
          REPLACE '&'  IN lv_mess1 WITH sy-msgv3.
        ELSEIF lv_index2 = 4.
          REPLACE '&'  IN lv_mess1 WITH sy-msgv4.
        ENDIF.
      ENDIF.
    ENDDO.
    " e_message = lv_mess1.
    LOOP AT t_return WHERE type = 'E' AND NOT ( id = sy-msgid AND  number = sy-msgno ) .
      CONCATENATE lv_mess1 ';' t_return-message INTO lv_mess1.
    ENDLOOP.
    e_message = lv_mess1.
    e_subrc = 4.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSEIF l_subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    e_subrc = 0.
  ENDIF.




ENDFUNCTION.
