FUNCTION zmmsp02_fm_char_ausp2out.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_CABN) TYPE  CABN
*"     REFERENCE(I_AUSPDATA) TYPE  AUSPDATA
*"  EXPORTING
*"     REFERENCE(E_ATWRT) TYPE  ATWRT
*"----------------------------------------------------------------------
  DATA: lv_numb TYPE int4,
        l_date LIKE sy-datum,
        l_date_2 LIKE sy-datum,
        l_time TYPE string,
        l_time_2  TYPE string,
        l_tttt LIKE sy-uzeit,
        l_t TYPE t,
        l_i TYPE i,
        l_char10(10),l_char10_2(10),
        l_char30(30), l_char30_2(30),l_str(40),l_unit LIKE t006a-mseh6.

  CASE i_auspdata-atcod.
    WHEN '1'.   "EQ
      CASE i_cabn-atfor.
        WHEN 'CHAR' .
          e_atwrt = i_auspdata-atwrt.
        WHEN 'NUM' OR 'CURR'.
*                "E_ATWRT =  I_AUSPDATA-atflv.
*                <fs_field_d> = I_AUSPDATA-atflv.
*                l_char30 = <fs_field_d>.
*                CONDENSE l_char30 NO-GAPS.
*                CONCATENATE l_char30 l_unit INTO E_ATWRT SEPARATED BY ' ' .
          CALL FUNCTION 'CTBP_CONVERT_VALUE_INT_TO_EXT'
            EXPORTING
              charactname       = i_cabn-atnam
              value_from        = i_auspdata-atflv
              value_to          = i_auspdata-atflb
              value_relation    = i_auspdata-atcod
*             CHARACTDETAIL     =
            IMPORTING
              value_external    = e_atwrt
            EXCEPTIONS
              charact_not_found = 1
              no_authority      = 2
              wrong_data_type   = 3
              internal_error    = 4
              wrong_input       = 5
              wrong_format      = 6
              OTHERS            = 7.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                e_atwrt = 'charact not found'.
              WHEN 2.
                e_atwrt = 'no_authority'.
              WHEN 3.
                e_atwrt = 'wrong_data_type'.
              WHEN 4.
                e_atwrt = 'internal_error'.
              WHEN 5.
                e_atwrt = 'wrong_input'.
              WHEN 6.
                e_atwrt = 'wrong_format'.
              WHEN 7.
                e_atwrt = 'others error'.
            ENDCASE.
          ENDIF.

        WHEN 'DATE' .
          lv_numb =  i_auspdata-atflv.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date.
          ENDIF.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date
            IMPORTING
              date_external            = e_atwrt
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
        WHEN  'TIME'.
          lv_numb = i_auspdata-atflv.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time ) .
          e_atwrt = l_time.
      ENDCASE.
    WHEN 2. "GE  LT
      CASE i_cabn-atfor.
        WHEN 'CHAR' .
          " E_ATWRT = I_AUSPDATA-atwrt.
        WHEN 'NUM' OR 'CURR'.
*                "E_ATWRT =  I_AUSPDATA-atflv.
*                <fs_field_d> = I_AUSPDATA-atflv.
*                l_char30 = <fs_field_d>.
*                CONDENSE l_char30 NO-GAPS.
*
*                <fs_field_d> = I_AUSPDATA-atflb.
*                l_char30_2 = <fs_field_d>.
*                CONDENSE l_char30_2 NO-GAPS.
*
*                CONCATENATE l_char30 '- <' l_char30_2 l_unit INTO E_ATWRT SEPARATED BY ' ' .
          CALL FUNCTION 'CTBP_CONVERT_VALUE_INT_TO_EXT'
            EXPORTING
              charactname       = i_cabn-atnam
              value_from        = i_auspdata-atflv
              value_to          = i_auspdata-atflb
              value_relation    = i_auspdata-atcod
*             CHARACTDETAIL     =
            IMPORTING
              value_external    = e_atwrt
            EXCEPTIONS
              charact_not_found = 1
              no_authority      = 2
              wrong_data_type   = 3
              internal_error    = 4
              wrong_input       = 5
              wrong_format      = 6
              OTHERS            = 7.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                e_atwrt = 'charact not found'.
              WHEN 2.
                e_atwrt = 'no_authority'.
              WHEN 3.
                e_atwrt = 'wrong_data_type'.
              WHEN 4.
                e_atwrt = 'internal_error'.
              WHEN 5.
                e_atwrt = 'wrong_input'.
              WHEN 6.
                e_atwrt = 'wrong_format'.
              WHEN 7.
                e_atwrt = 'others error'.
            ENDCASE.
          ENDIF.
        WHEN 'DATE' .
          lv_numb =  i_auspdata-atflv.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date.
          ENDIF.
          lv_numb =  i_auspdata-atflb.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date_2.
          ENDIF.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date
            IMPORTING
              date_external            = l_char10
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date_2
            IMPORTING
              date_external            = l_char10_2
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CONCATENATE l_char10 '- <' l_char10_2  INTO e_atwrt SEPARATED BY ' ' .
        WHEN  'TIME'.
          lv_numb = i_auspdata-atflv.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time ) .
          lv_numb = i_auspdata-atflb.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time_2 ) .
          CONCATENATE l_time '- <' l_time_2  INTO e_atwrt SEPARATED BY ' ' .
      ENDCASE.
    WHEN 3. " GE  LE
      CASE i_cabn-atfor.
        WHEN 'CHAR' .
          " E_ATWRT = I_AUSPDATA-atwrt.
        WHEN 'NUM' OR 'CURR'.
*                "E_ATWRT =  I_AUSPDATA-atflv.
*                <fs_field_d> = I_AUSPDATA-atflv.
*                l_char30 = <fs_field_d>.
*                CONDENSE l_char30 NO-GAPS.
*
*                <fs_field_d> = I_AUSPDATA-atflb.
*                l_char30_2 = <fs_field_d>.
*                CONDENSE l_char30_2 NO-GAPS.
*
*                CONCATENATE l_char30 '-' l_char30_2 l_unit INTO E_ATWRT SEPARATED BY ' ' .
          CALL FUNCTION 'CTBP_CONVERT_VALUE_INT_TO_EXT'
            EXPORTING
              charactname       = i_cabn-atnam
              value_from        = i_auspdata-atflv
              value_to          = i_auspdata-atflb
              value_relation    = i_auspdata-atcod
*             CHARACTDETAIL     =
            IMPORTING
              value_external    = e_atwrt
            EXCEPTIONS
              charact_not_found = 1
              no_authority      = 2
              wrong_data_type   = 3
              internal_error    = 4
              wrong_input       = 5
              wrong_format      = 6
              OTHERS            = 7.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                e_atwrt = 'charact not found'.
              WHEN 2.
                e_atwrt = 'no_authority'.
              WHEN 3.
                e_atwrt = 'wrong_data_type'.
              WHEN 4.
                e_atwrt = 'internal_error'.
              WHEN 5.
                e_atwrt = 'wrong_input'.
              WHEN 6.
                e_atwrt = 'wrong_format'.
              WHEN 7.
                e_atwrt = 'others error'.
            ENDCASE.
          ENDIF.
        WHEN 'DATE' .
          lv_numb =  i_auspdata-atflv.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date.
          ENDIF.
          lv_numb =  i_auspdata-atflb.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date_2.
          ENDIF.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date
            IMPORTING
              date_external            = l_char10
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date_2
            IMPORTING
              date_external            = l_char10_2
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CONCATENATE l_char10 '-' l_char10_2  INTO e_atwrt SEPARATED BY ' ' .
        WHEN  'TIME'.
          lv_numb = i_auspdata-atflv.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time ) .
          lv_numb = i_auspdata-atflb.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time_2 ) .

          CONCATENATE l_time '-' l_time_2  INTO e_atwrt SEPARATED BY ' ' .
      ENDCASE.
    WHEN 4. "GT  LT
      CASE i_cabn-atfor.
        WHEN 'CHAR' .
          " E_ATWRT = I_AUSPDATA-atwrt.
        WHEN 'NUM' OR 'CURR'.
*                "E_ATWRT =  I_AUSPDATA-atflv.
*                <fs_field_d> = I_AUSPDATA-atflv.
*                l_char30 = <fs_field_d>.
*                CONDENSE l_char30 NO-GAPS.
*
*                <fs_field_d> = I_AUSPDATA-atflb.
*                l_char30_2 = <fs_field_d>.
*                CONDENSE l_char30_2 NO-GAPS.
*
*                CONCATENATE '>' l_char30 '- <' l_char30_2 l_unit INTO E_ATWRT SEPARATED BY ' ' .
          CALL FUNCTION 'CTBP_CONVERT_VALUE_INT_TO_EXT'
            EXPORTING
              charactname       = i_cabn-atnam
              value_from        = i_auspdata-atflv
              value_to          = i_auspdata-atflb
              value_relation    = i_auspdata-atcod
*             CHARACTDETAIL     =
            IMPORTING
              value_external    = e_atwrt
            EXCEPTIONS
              charact_not_found = 1
              no_authority      = 2
              wrong_data_type   = 3
              internal_error    = 4
              wrong_input       = 5
              wrong_format      = 6
              OTHERS            = 7.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                e_atwrt = 'charact not found'.
              WHEN 2.
                e_atwrt = 'no_authority'.
              WHEN 3.
                e_atwrt = 'wrong_data_type'.
              WHEN 4.
                e_atwrt = 'internal_error'.
              WHEN 5.
                e_atwrt = 'wrong_input'.
              WHEN 6.
                e_atwrt = 'wrong_format'.
              WHEN 7.
                e_atwrt = 'others error'.
            ENDCASE.
          ENDIF.
        WHEN 'DATE' .
          lv_numb =  i_auspdata-atflv.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date.
          ENDIF.
          lv_numb =  i_auspdata-atflb.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date_2.
          ENDIF.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date
            IMPORTING
              date_external            = l_char10
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date_2
            IMPORTING
              date_external            = l_char10_2
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CONCATENATE '>' l_char10 '- <' l_char10_2  INTO e_atwrt SEPARATED BY ' ' .
        WHEN  'TIME'.
          lv_numb = i_auspdata-atflv.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time ) .
          lv_numb = i_auspdata-atflb.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time_2 ) .
          CONCATENATE '>' l_time '- <' l_time_2  INTO e_atwrt SEPARATED BY ' ' .
      ENDCASE.
    WHEN 5. "GT  LE
      CASE i_cabn-atfor.
        WHEN 'CHAR' .
          " E_ATWRT = I_AUSPDATA-atwrt.
        WHEN 'NUM' OR 'CURR'.
*                "E_ATWRT =  I_AUSPDATA-atflv.
*                <fs_field_d> = I_AUSPDATA-atflv.
*                l_char30 = <fs_field_d>.
*                CONDENSE l_char30 NO-GAPS.
*
*                <fs_field_d> = I_AUSPDATA-atflb.
*                l_char30_2 = <fs_field_d>.
*                CONDENSE l_char30_2 NO-GAPS.
*
*                CONCATENATE '>' l_char30 '-' l_char30_2 l_unit INTO E_ATWRT SEPARATED BY ' ' .
          CALL FUNCTION 'CTBP_CONVERT_VALUE_INT_TO_EXT'
            EXPORTING
              charactname       = i_cabn-atnam
              value_from        = i_auspdata-atflv
              value_to          = i_auspdata-atflb
              value_relation    = i_auspdata-atcod
*             CHARACTDETAIL     =
            IMPORTING
              value_external    = e_atwrt
            EXCEPTIONS
              charact_not_found = 1
              no_authority      = 2
              wrong_data_type   = 3
              internal_error    = 4
              wrong_input       = 5
              wrong_format      = 6
              OTHERS            = 7.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                e_atwrt = 'charact not found'.
              WHEN 2.
                e_atwrt = 'no_authority'.
              WHEN 3.
                e_atwrt = 'wrong_data_type'.
              WHEN 4.
                e_atwrt = 'internal_error'.
              WHEN 5.
                e_atwrt = 'wrong_input'.
              WHEN 6.
                e_atwrt = 'wrong_format'.
              WHEN 7.
                e_atwrt = 'others error'.
            ENDCASE.
          ENDIF.
        WHEN 'DATE' .
          lv_numb =  i_auspdata-atflv.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date.
          ENDIF.
          lv_numb =  i_auspdata-atflb.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date_2.
          ENDIF.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date
            IMPORTING
              date_external            = l_char10
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date_2
            IMPORTING
              date_external            = l_char10_2
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CONCATENATE '>' l_char10 '-' l_char10_2  INTO e_atwrt SEPARATED BY ' ' .
        WHEN  'TIME'.
          lv_numb = i_auspdata-atflv.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time ) .
          lv_numb = i_auspdata-atflb.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time_2 ) .

          CONCATENATE '>' l_time '-' l_time_2  INTO e_atwrt SEPARATED BY ' ' .
      ENDCASE.
    WHEN 6. "LT
      CASE i_cabn-atfor.
        WHEN 'CHAR' .
          " E_ATWRT = I_AUSPDATA-atwrt.
        WHEN 'NUM' OR 'CURR'.
*                "E_ATWRT =  I_AUSPDATA-atflv.
**                <fs_field_d> = I_AUSPDATA-atflv.
**                l_char30 = <fs_field_d>.
**                CONDENSE l_char30 NO-GAPS.
*
*                <fs_field_d> = I_AUSPDATA-atflb.
*                l_char30_2 = <fs_field_d>.
*                CONDENSE l_char30_2 NO-GAPS.
*
*                CONCATENATE '<' l_char30_2 l_unit INTO E_ATWRT SEPARATED BY ' ' .
          CALL FUNCTION 'CTBP_CONVERT_VALUE_INT_TO_EXT'
            EXPORTING
              charactname       = i_cabn-atnam
              value_from        = i_auspdata-atflv
              value_to          = i_auspdata-atflb
              value_relation    = i_auspdata-atcod
*             CHARACTDETAIL     =
            IMPORTING
              value_external    = e_atwrt
            EXCEPTIONS
              charact_not_found = 1
              no_authority      = 2
              wrong_data_type   = 3
              internal_error    = 4
              wrong_input       = 5
              wrong_format      = 6
              OTHERS            = 7.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                e_atwrt = 'charact not found'.
              WHEN 2.
                e_atwrt = 'no_authority'.
              WHEN 3.
                e_atwrt = 'wrong_data_type'.
              WHEN 4.
                e_atwrt = 'internal_error'.
              WHEN 5.
                e_atwrt = 'wrong_input'.
              WHEN 6.
                e_atwrt = 'wrong_format'.
              WHEN 7.
                e_atwrt = 'others error'.
            ENDCASE.
          ENDIF.
        WHEN 'DATE' .
          lv_numb =  i_auspdata-atflb.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date_2.
          ENDIF.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date_2
            IMPORTING
              date_external            = l_char10_2
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CONCATENATE  '<' l_char10_2  INTO e_atwrt SEPARATED BY ' ' .
        WHEN  'TIME'.
          lv_numb = i_auspdata-atflb.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time_2 ) .

          CONCATENATE '<' l_time_2  INTO e_atwrt SEPARATED BY ' ' .
      ENDCASE.
    WHEN 7. "LE
      CASE i_cabn-atfor.
        WHEN 'CHAR' .
          " E_ATWRT = I_AUSPDATA-atwrt.
        WHEN 'NUM' OR 'CURR'.
*                "E_ATWRT =  I_AUSPDATA-atflv.
**                <fs_field_d> = I_AUSPDATA-atflv.
**                l_char30 = <fs_field_d>.
**                CONDENSE l_char30 NO-GAPS.
*
*                <fs_field_d> = I_AUSPDATA-atflb.
*                l_char30_2 = <fs_field_d>.
*                CONDENSE l_char30_2 NO-GAPS.
*
*                CONCATENATE '<=' l_char30_2 l_unit INTO E_ATWRT SEPARATED BY ' ' .
          CALL FUNCTION 'CTBP_CONVERT_VALUE_INT_TO_EXT'
            EXPORTING
              charactname       = i_cabn-atnam
              value_from        = i_auspdata-atflv
              value_to          = i_auspdata-atflb
              value_relation    = i_auspdata-atcod
*             CHARACTDETAIL     =
            IMPORTING
              value_external    = e_atwrt
            EXCEPTIONS
              charact_not_found = 1
              no_authority      = 2
              wrong_data_type   = 3
              internal_error    = 4
              wrong_input       = 5
              wrong_format      = 6
              OTHERS            = 7.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                e_atwrt = 'charact not found'.
              WHEN 2.
                e_atwrt = 'no_authority'.
              WHEN 3.
                e_atwrt = 'wrong_data_type'.
              WHEN 4.
                e_atwrt = 'internal_error'.
              WHEN 5.
                e_atwrt = 'wrong_input'.
              WHEN 6.
                e_atwrt = 'wrong_format'.
              WHEN 7.
                e_atwrt = 'others error'.
            ENDCASE.
          ENDIF.
        WHEN 'DATE' .
          lv_numb =  i_auspdata-atflb.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date_2.
          ENDIF.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date_2
            IMPORTING
              date_external            = l_char10_2
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
          CONCATENATE  '<=' l_char10_2  INTO e_atwrt SEPARATED BY ' ' .
        WHEN  'TIME'.

          lv_numb = i_auspdata-atflb.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time_2 ) .

          CONCATENATE '<=' l_time_2  INTO e_atwrt SEPARATED BY ' ' .
      ENDCASE.

    WHEN 8. "GT

      CASE i_cabn-atfor.
        WHEN 'CHAR' .
          " E_ATWRT = I_AUSPDATA-atwrt.
        WHEN 'NUM' OR 'CURR'.
*                "E_ATWRT =  I_AUSPDATA-atflv.
*                <fs_field_d> = I_AUSPDATA-atflv.
*                l_char30 = <fs_field_d>.
*                CONDENSE l_char30 NO-GAPS.
*
**                <fs_field_d> = I_AUSPDATA-ATFLB.
**                l_char30_2 = <fs_field_d>.
**                CONDENSE l_char30_2 NO-GAPS.
*
*                CONCATENATE '>' l_char30 l_unit INTO E_ATWRT SEPARATED BY ' ' .
          CALL FUNCTION 'CTBP_CONVERT_VALUE_INT_TO_EXT'
            EXPORTING
              charactname       = i_cabn-atnam
              value_from        = i_auspdata-atflv
              value_to          = i_auspdata-atflb
              value_relation    = i_auspdata-atcod
*             CHARACTDETAIL     =
            IMPORTING
              value_external    = e_atwrt
            EXCEPTIONS
              charact_not_found = 1
              no_authority      = 2
              wrong_data_type   = 3
              internal_error    = 4
              wrong_input       = 5
              wrong_format      = 6
              OTHERS            = 7.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                e_atwrt = 'charact not found'.
              WHEN 2.
                e_atwrt = 'no_authority'.
              WHEN 3.
                e_atwrt = 'wrong_data_type'.
              WHEN 4.
                e_atwrt = 'internal_error'.
              WHEN 5.
                e_atwrt = 'wrong_input'.
              WHEN 6.
                e_atwrt = 'wrong_format'.
              WHEN 7.
                e_atwrt = 'others error'.
            ENDCASE.
          ENDIF.
        WHEN 'DATE' .
          lv_numb =  i_auspdata-atflv.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date.
          ENDIF.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date
            IMPORTING
              date_external            = l_char10
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
*                lv_numb =  I_AUSPDATA-atflb.
*                IF lv_numb IS NOT INITIAL.
*                  WRITE lv_numb TO  l_date_2.
*                ENDIF.
          CONCATENATE  '>' l_char10  INTO e_atwrt SEPARATED BY ' ' .
        WHEN  'TIME'.
          lv_numb = i_auspdata-atflv.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time ) .

          CONCATENATE '>' l_time  INTO e_atwrt SEPARATED BY ' ' .
      ENDCASE.
    WHEN 9. "GE
      CASE i_cabn-atfor.
        WHEN 'CHAR' .
          " E_ATWRT = I_AUSPDATA-atwrt.
        WHEN 'NUM' OR 'CURR'.
*                "E_ATWRT =  I_AUSPDATA-atflv.
*                <fs_field_d> = I_AUSPDATA-atflv.
*                l_char30 = <fs_field_d>.
*                CONDENSE l_char30 NO-GAPS.
*
**                <fs_field_d> = I_AUSPDATA-ATFLB.
**                l_char30_2 = <fs_field_d>.
**                CONDENSE l_char30_2 NO-GAPS.
*
*                CONCATENATE '>=' l_char30 l_unit INTO E_ATWRT SEPARATED BY ' ' .
          CALL FUNCTION 'CTBP_CONVERT_VALUE_INT_TO_EXT'
            EXPORTING
              charactname       = i_cabn-atnam
              value_from        = i_auspdata-atflv
              value_to          = i_auspdata-atflb
              value_relation    = i_auspdata-atcod
*             CHARACTDETAIL     =
            IMPORTING
              value_external    = e_atwrt
            EXCEPTIONS
              charact_not_found = 1
              no_authority      = 2
              wrong_data_type   = 3
              internal_error    = 4
              wrong_input       = 5
              wrong_format      = 6
              OTHERS            = 7.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                e_atwrt = 'charact not found'.
              WHEN 2.
                e_atwrt = 'no_authority'.
              WHEN 3.
                e_atwrt = 'wrong_data_type'.
              WHEN 4.
                e_atwrt = 'internal_error'.
              WHEN 5.
                e_atwrt = 'wrong_input'.
              WHEN 6.
                e_atwrt = 'wrong_format'.
              WHEN 7.
                e_atwrt = 'others error'.
            ENDCASE.
          ENDIF.
        WHEN 'DATE' .
          lv_numb =  i_auspdata-atflv.
          IF lv_numb IS NOT INITIAL.
            WRITE lv_numb TO  l_date.
          ENDIF.
          CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
            EXPORTING
              date_internal            = l_date
            IMPORTING
              date_external            = l_char10
            EXCEPTIONS
              date_internal_is_invalid = 1               "1804629
              OTHERS                   = 2.
*                lv_numb =  I_AUSPDATA-atflb.
*                IF lv_numb IS NOT INITIAL.
*                  WRITE lv_numb TO  l_date_2.
*                ENDIF.
          CONCATENATE  '>=' l_char10  INTO e_atwrt SEPARATED BY ' ' .
        WHEN  'TIME'.
          lv_numb = i_auspdata-atflb.
          WRITE lv_numb TO l_tttt.
          l_i = strlen( l_tttt ).
          IF l_i < 8.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = l_tttt
              IMPORTING
                output = l_tttt.
          ENDIF.
          l_t = l_tttt.
          CALL METHOD cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = l_t IMPORTING time_ext = l_time_2 ) .

          CONCATENATE '>=' l_time  INTO e_atwrt SEPARATED BY ' ' .
      ENDCASE.
  ENDCASE.



ENDFUNCTION.
