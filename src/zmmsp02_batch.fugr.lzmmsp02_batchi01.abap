*----------------------------------------------------------------------*
***INCLUDE LZMMSP02_BATCHI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  gv_code = ok_code.
  CLEAR ok_code.
  CASE gv_code.
    WHEN 'NO'.
      g_subrc = 5.
      LEAVE TO SCREEN 0.
    WHEN 'YES'.
      CLEAR:  g_attributes[],g_values[].

      CALL FUNCTION 'CTMS_DDB_CHAR_HAS_ATTRIBUTES'
*   EXPORTING
*     EXCL_KNOWLEDGE            = ' '
*     EXCL_DOCUMENTS            = ' '
*     CONSIDER_SELCOND          =
        TABLES
          imp_characteristics = g_characteristics
          exp_attributes      = g_attributes.
      IF sy-subrc <> 0.
        g_subrc = 3.
      ENDIF.

      CALL FUNCTION 'CTMS_DDB_HAS_VALUES'
        EXPORTING
          assigned_values     = 'X'
          allowed_values      = ' '
*         VALID_VALUES        = ' '
*         INCONSISTENT_VALUES = ' '
*         FIRST_ASSIGNED_VALUE       = ' '
*         DEFAULT_VALUES      = ' '
*         INCL_AUTHOR         = ' '
*         AUTHORITY_DISPLAY   = ' '
        TABLES
          imp_characteristics = g_characteristics
          exp_values          = g_values
        EXCEPTIONS
          not_found           = 4
          OTHERS              = 99.
      g_subrc = sy-subrc.
      LEAVE TO SCREEN 0.
   when OTHERS.
     CALL FUNCTION 'CTMS_DDB_EXECUTE_FUNCTION'
       EXPORTING
         okcode                     = gv_code
"      IMPORTING
      EXCEPTIONS
        RAISE_INVALID_OKCODE       = 6
        RAISE_INCONSISTENCY        = 7
        RAISE_INCOMPLETE           = 8
        RAISE_VERIFICATION         = 9
        RAISE_NOT_ASSIGNED         = 10
        RAISE_ANOTHER_OBJECT       = 11
        RAISE_OTHER_OBJECTS        = 12
        RAISE_OTHERS               = 99
               .
     g_subrc = sy-subrc.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
