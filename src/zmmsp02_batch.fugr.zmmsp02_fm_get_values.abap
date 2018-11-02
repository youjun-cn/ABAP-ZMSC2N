FUNCTION zmmsp02_fm_get_values.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(CLASS) LIKE  KLAH-CLASS
*"     REFERENCE(CLASSTYPE) LIKE  KLAH-KLART
*"     REFERENCE(MODE) DEFAULT ' '
*"     REFERENCE(DISPLAY_MODE) DEFAULT ''
*"  EXPORTING
*"     REFERENCE(E_SUBRC) LIKE  SY-SUBRC
*"  TABLES
*"      EXP_VALUES STRUCTURE  API_VALUE OPTIONAL
*"      EXP_ATTRIBUTES STRUCTURE  API_CH_ATT OPTIONAL
*"      IMP_CHARACTERISTICS_INACT STRUCTURE  API_CHAR OPTIONAL
*"      IMP_VALUES STRUCTURE  API_VAL_I OPTIONAL
*"  EXCEPTIONS
*"      CANCEL
*"      CLASS_NOT_FOUND
*"      DDB_HAS_NO_CLASS
*"      NO_ATTRIBUTES
*"      VALUE_NOT_FOUND
*"      SET_VALUE_ERROR
*"      NO_CHARACTERISTICS
*"      OTHERS
*"----------------------------------------------------------------------
  FIELD-SYMBOLS:<iausb> TYPE ANY TABLE.
  CLEAR g_subrc.

  CALL FUNCTION 'CTMS_DDB_INIT'.
  ASSIGN ('(SAPLCTMS)IAUSB[]') TO <iausb>.
  IF <iausb> IS ASSIGNED.
    REFRESH <iausb>.
  ENDIF.

  CALL FUNCTION 'CTMS_CLASS_DDB'
    EXPORTING
      class         = class
      classtype     = classtype
      mode          = mode
*     LANGUAGE      = SY-LANGU
*     KEY_DATE      = SY-DATUM
*     OBJECTID      =
*     OBJECT        =
*     I_ADD_ON_CHAR = ' '
      i_tabs_active = 'X'
      init_conf     = ' '
*     READONLY      = ' '
*     I_CALLED_FROM_DDB       = ' '
    EXCEPTIONS
      not_found     = 1
      OTHERS        = 99.

  e_subrc = sy-subrc.
  IF sy-subrc = 1.
    g_subrc = 1.
    RAISE class_not_found.
  ELSEIF sy-subrc = 99.
    g_subrc = 99.
    RAISE others.
  ENDIF.
  IF imp_characteristics_inact[] IS NOT INITIAL.
    CALL FUNCTION 'CTMS_DDB_SET_CHAR_INACTIVE'
      TABLES
        imp_characteristics = imp_characteristics_inact
      EXCEPTIONS
        no_characteristics  = 1
        OTHERS              = 99.
    e_subrc = sy-subrc.
    IF sy-subrc = 1.
      RAISE no_characteristics.
    ELSEIF sy-subrc = 99.
      RAISE others.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'CTMS_DDB_HAS_CLASS'
* IMPORTING
*   T_KLAH                 =
*   T_SWOR                 =
    EXCEPTIONS
      ddb_has_no_class = 2
      OTHERS           = 99.
  e_subrc = sy-subrc.
  IF sy-subrc = 2.
    g_subrc = 2.
    RAISE ddb_has_no_class.
  ELSEIF sy-subrc = 99.
    g_subrc = 99.
    RAISE others.
  ENDIF.

  IF display_mode = 'X'.
    CALL FUNCTION 'CTMS_DDB_SET_DISPLAY_MODE'.
  ENDIF.

  CALL FUNCTION 'CTMS_DDB_SET_VALUE'
*   EXPORTING
*     INSTANCE                            =
*     MESSAGE                             = ' '
*     UDEF_INSTANCE                       =
    TABLES
      imp_values                    = imp_values
*     EXP_VALUES_ERROR              =
    EXCEPTIONS
      currency_check                = 1
      date_check                    = 2
      format_check                  = 3
      illegal_internal_baseunit     = 4
      interval_check                = 5
      pattern_check                 = 6
      time_check                    = 7
      unit_check                    = 8
      value_not_found               = 9
      no_valid_dimension            = 10
      interval_not_allowed          = 11
      display_mode                  = 12
      characteristic_not_found      = 13
      value_not_possible            = 14
      characteristic_enqueue        = 15
      objectcharacteristic          = 16
      only_one_value_allowed        = 17
      characteristic_not_selectable = 18
      input_to_long                 = 19
      value_contradiction           = 20
      OTHERS                        = 99.
  e_subrc = sy-subrc.
  IF sy-subrc <> 0 .
    g_subrc = sy-subrc.
    RAISE set_value_error.
  ENDIF.

  klah-class =  class.
  klah-klart =  classtype.
  CLEAR :g_values[],g_attributes[].
  CALL SCREEN 100 STARTING AT 20 2 ENDING AT 100 22.
  e_subrc = g_subrc.
  CASE g_subrc.
    WHEN 3.
      RAISE value_not_found.
    WHEN 4.
      RAISE no_attributes.

    WHEN 5.
      RAISE cancel.
    WHEN 99.
      RAISE others.
    WHEN OTHERS.

  ENDCASE.
  exp_values[] = g_values[].
  exp_attributes[] = g_attributes[].
ENDFUNCTION.
