*&---------------------------------------------------------------------*
*& Report  ZMMSP02_BATCH_UPDATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zmmsp02_batch_update LINE-SIZE 80
LINE-COUNT 64
"MESSAGE-ID zz
NO STANDARD PAGE HEADING.

TABLES:mch1,mcha,zvmmbat01,zvmmbat02,zvmmcl01,sscrfields,t001w.

CONSTANTS:c_icon_w             VALUE '@5D@'             TYPE icon-id .
CONSTANTS:c_icon_s             VALUE '@5B@'             TYPE icon-id .
CONSTANTS:c_icon_e             VALUE '@5C@'             TYPE icon-id .
CONSTANTS:c_icon_i             VALUE '@BZ@'             TYPE icon-id .

DATA: gd_fieldcat  TYPE lvc_t_fcat  WITH HEADER LINE,"slis_t_fieldcat_alv
      gd_tab_group TYPE slis_t_sp_group_alv,
      gd_layout    TYPE lvc_s_layo, "slis_layout_alv,
      gd_repid     LIKE sy-repid,
      g_grid TYPE REF TO  cl_gui_alv_grid,
      gv_temp,
      ch_temp(20).


TYPES: BEGIN OF ty_chars.
        INCLUDE TYPE api_value.
TYPES:xdelete TYPE clbatch-xdelete.
TYPES:  END OF ty_chars.

TYPES: BEGIN OF ty_mchx,
        matnr TYPE mcha-matnr,
        charg TYPE mcha-charg,
        werks TYPE mcha-werks,
        maktx TYPE makt-maktx,
        box TYPE c,
        icon  TYPE icon-id,
        message(100) TYPE c,
        t_chars TYPE ty_chars OCCURS 0,
       END OF ty_mchx.

TYPES: BEGIN OF ty_mchy,
        matnr TYPE mcha-matnr,
        charg TYPE mcha-charg,
        werks TYPE mcha-werks,
       END OF ty_mchy.


DATA:  gt_mchx   TYPE TABLE OF ty_mchx WITH HEADER LINE.
DATA   gt_zvmmbat TYPE TABLE OF zvmmbat01 WITH HEADER LINE.
DATA:  g_klart TYPE klah-klart.
DATA:  gs_tcuch LIKE tcuch.
FIELD-SYMBOLS :<fst_mchx> TYPE STANDARD TABLE.

DATA:BEGIN OF gt_class OCCURS 0.
        INCLUDE TYPE zvmmcl01.
DATA box TYPE c.
DATA onf4 TYPE c.
DATA atbez LIKE cabnt-atbez.
DATA:fieldname LIKE gd_fieldcat-fieldname.
DATA t_cawn TYPE cawn OCCURS 0 .
DATA:END OF gt_class.


DATA: "dref_str TYPE REF TO data,
      dref_str_d TYPE REF TO data,
      dref_tab TYPE REF TO data,
      "dref_i TYPE REF TO data,
  itab_type TYPE REF TO cl_abap_tabledescr,
  struct_type TYPE REF TO cl_abap_structdescr,
  struct_type_d TYPE REF TO cl_abap_structdescr,
  elem_type TYPE REF TO cl_abap_elemdescr,     "char30
  elem_type_d TYPE REF TO cl_abap_elemdescr,   "real data type
  table_type TYPE REF TO cl_abap_tabledescr,
  comp_tab TYPE cl_abap_structdescr=>component_table WITH HEADER LINE,
  comp_tab_d TYPE cl_abap_structdescr=>component_table WITH HEADER LINE.



DEFINE add_field.
  clear  gd_fieldcat.
  gd_fieldcat-fieldname      =  &1.
  gd_fieldcat-seltext      =  &2.
  gd_fieldcat-scrtext_m =  &2.
  "gd_fieldcat-key            =  &3.
  gd_fieldcat-edit           = &4.
  gd_fieldcat-checkbox       = &5.
*  gd_fieldcat-no_zero = 'x'.
  gd_fieldcat-just = &6.
  gd_fieldcat-ref_table = &7.
  gd_fieldcat-ref_field = &8.
  "gd_fieldcat-qtabname = &7.
  gd_fieldcat-qfieldname = &8.

  gd_fieldcat-f4availabl = &3.
  gd_fieldcat-hotspot = &9.

  "gd_fieldcat-qfieldname = &9.
  "gd_fieldcat-decimals_out = &9.
  "ch_temp = &10.
*  CASE &9.
*    WHEN 'CURR'.
*       gd_fieldcat-datatype      = 'CURR' .
*
*    WHEN 'NUM'.
*      gd_fieldcat-datatype      = 'QUAN' .  " 指定数据类型
*      gd_fieldcat-inttype         = 'C' .       "这个是指定字段的类型为C
*
*    WHEN 'DATE'.
*      gd_fieldcat-ref_table = 'SYST'.
*     gd_fieldcat-ref_field = 'DATUM'.
*
*    WHEN 'TIME'.
*      gd_fieldcat-ref_table = 'SYST'.
*     gd_fieldcat-ref_field = 'UZEIT'.
*
*  ENDCASE.
  append gd_fieldcat to gd_fieldcat.
END-OF-DEFINITION.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
*  METHODS handle_modify
*        FOR EVENT data_changed_finished OF cl_gui_alv_grid
*       IMPORTING e_modified et_good_cells.
    METHODS handle_f4
      FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname
                e_fieldvalue "  Type  LVC_VALUE
                es_row_no
                er_event_data
                et_bad_cells.
ENDCLASS. "LCL_EVENT_RECEIVER DEFINITION

"PARAMETERS:p_class LIKE rmclm-class OBLIGATORY.
SELECT-OPTIONS:s_matnr FOR mcha-matnr.
SELECT-OPTIONS:s_charg FOR mcha-charg.
SELECT-OPTIONS:s_werks FOR t001w-werks MODIF ID p1.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
"skip,
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN   COMMENT 1(5) text-003.
PARAMETERS:p_klart LIKE rmclm-klart OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(5) text-002.
PARAMETERS :p_class LIKE rmclm-class OBLIGATORY MEMORY ID kla. "Table name
SELECTION-SCREEN : PUSHBUTTON 30(12) p_btn USER-COMMAND clk. "Select-options

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
PARAMETERS :p_client TYPE c RADIOBUTTON GROUP g1 MODIF ID g1 .
PARAMETERS :p_plant TYPE c RADIOBUTTON GROUP g1 MODIF ID g1 .
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.

  PERFORM refresh_button.
  PERFORM get_tcuch.


AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

AT SELECTION-SCREEN.
  PERFORM select_chars.



START-OF-SELECTION.
  PERFORM get_characters.
  PERFORM build_itab.
  PERFORM get_data.
  PERFORM build_fieldcatalog.
  PERFORM build_layout.
  PERFORM display_alv_report.
*----------------------------------------------------------------------
*At User Command *
*----------------------------------------------------------------------

AT USER-COMMAND.
  DATA:c_temp.
  CASE sy-ucomm.
    WHEN 'OK'.
      "REFRESH gt_temp.
      CLEAR gt_class.
*      move 'S' to wa_temp-type.
*      move gv_tbname to wa_temp-tablename.
      DO.
        READ LINE sy-index FIELD VALUE gt_class-box INTO c_temp.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
        "IF gt_class-box EQ 'X'.
        sy-index = sy-index - 1.
        READ TABLE gt_class INDEX sy-index.
        IF sy-subrc = 0.
          gt_class-box = c_temp.
          MODIFY gt_class INDEX sy-index TRANSPORTING box.
*            MOVE gt_class TO gt_temp.
*            APPEND gt_temp.
*            CLEAR gv_temp.
        ENDIF.
        "ENDIF.
      ENDDO.
*      DESCRIBE TABLE gt_class.
*      IF sy-tfill GT 200.
***--More than 70 fields can not be selected
*        MESSAGE i001 WITH 'Cannot select more than 200 Parameters'(006).
*        REFRESH gt_temp[]. CLEAR gt_temp.
*      ELSE.
      PERFORM refresh_button.
      LEAVE LIST-PROCESSING.
*  ENDIF.
    WHEN 'CANCEL'.
      PERFORM refresh_button.
      LEAVE LIST-PROCESSING.
    WHEN 'SELALL'.
**--Select all fields
      "clear wa_dd03l-fieldname.
      DO.
        READ LINE sy-index FIELD VALUE gt_class-box .
        IF sy-subrc EQ 0 AND
        gt_class-atnam NE space .
          gt_class-box = 'X'.
          MODIFY LINE sy-index INDEX 0 FIELD VALUE gt_class-box.
          HIDE gt_class-box.
          "CLEAR gt_class-atnam.
        ELSEIF sy-subrc NE 0.
          EXIT.
        ENDIF.
      ENDDO.
      PERFORM refresh_button.
    WHEN 'DSELALL'.
**--Deselect all fields
      CLEAR gt_class.
      DO.
        READ LINE sy-index FIELD VALUE gt_class-box.
        IF sy-subrc EQ 0.
          CLEAR: gt_class-box.
          MODIFY LINE sy-index INDEX 0 FIELD VALUE gt_class-box.
          HIDE gt_class-box.
        ELSEIF sy-subrc NE 0.
          EXIT.
        ENDIF.
      ENDDO.
      PERFORM refresh_button.
  ENDCASE.

  sy-lsind = 0.
*&---------------------------------------------------------------------*
*&      Form  BUILD_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_itab .
  DATA: l_i TYPE i.
  DATA: l_i1 TYPE i.
  DATA: l_flag TYPE c.
  DATA:l_char3(3) TYPE c.
  DATA: l_tabix LIKE sy-tabix.



  struct_type ?= cl_abap_typedescr=>describe_by_data( gt_mchx )."结构类型
  comp_tab[] = struct_type->get_components( )."组成结构体的各个字段组件
  comp_tab_d[] = struct_type->get_components( )."组成结构体的各个字段组件

  LOOP AT gt_class.
    l_char3 = sy-tabix.
    CONDENSE l_char3.
**=========动态创建基本类型
    CASE gt_class-atfor.
      WHEN 'CHAR'.
        l_i = gt_class-anzst.
        elem_type_d ?= cl_abap_elemdescr=>get_c( l_i ).
        elem_type ?= cl_abap_elemdescr=>get_c( 30 ).
      WHEN 'DATE'.
        elem_type_d ?= cl_abap_elemdescr=>get_d( ).
        elem_type ?= cl_abap_elemdescr=>get_c( 30 ).
      WHEN 'TIME'.
        elem_type_d ?= cl_abap_elemdescr=>get_t( ).
        elem_type ?= cl_abap_elemdescr=>get_c( 30 ).
      WHEN 'NUM'.
        l_i = gt_class-anzst.
        l_i1 = gt_class-anzdz.
        IF l_i1 = 0.
          elem_type_d ?= cl_abap_elemdescr=>get_i( ).
        ELSE.
          elem_type_d ?= cl_abap_elemdescr=>get_p( p_length = l_i  p_decimals = l_i1 ).
        ENDIF.
        elem_type ?= cl_abap_elemdescr=>get_c( 30 ).
      WHEN 'CURR'.
        elem_type_d ?= cl_abap_elemdescr=>get_p( p_length = 13  p_decimals = 2 ).
        elem_type ?= cl_abap_elemdescr=>get_c( 30 ).
      WHEN OTHERS.
        l_flag = 'X'.  "not support
    ENDCASE.
    IF l_flag = 'X'.
    ELSE.
      "CREATE DATA dref_i TYPE HANDLE elem_type ."动态的创建基本类型数据对象

**=========动态创建结构类型

* 向结构中动态的新增一个成员
      CONCATENATE 'F' l_char3 INTO gt_class-fieldname.
      CONDENSE gt_class-fieldname.
      comp_tab-name = gt_class-fieldname." gt_class-atnam."为结构新增一个成员
      comp_tab-type = elem_type."新增成员的类型对象
      "INSERT comp_tab INTO comp_tab .
      APPEND comp_tab.
      FREE elem_type.

* 向结构中动态的新增一个成员
      comp_tab_d-name = gt_class-fieldname. "gt_class-atnam."为结构新增一个成员
      comp_tab_d-type = elem_type_d."新增成员的类型对象
      "INSERT comp_tab INTO comp_tab .
      APPEND comp_tab_d.
      FREE elem_type_d.

    ENDIF.
    MODIFY gt_class.
  ENDLOOP.

* 动态创建结构类型对象
  struct_type_d = cl_abap_structdescr=>create( comp_tab_d[] ).
  CREATE DATA dref_str_d TYPE HANDLE struct_type_d."使用结构类型对象来创建结构对象

  struct_type = cl_abap_structdescr=>create( comp_tab[] ).


**=========动态创建内表
* 基于结构类型对象创建内表类型对象
  itab_type = cl_abap_tabledescr=>create( struct_type ).
  CREATE DATA dref_tab TYPE HANDLE itab_type."使用内表类型对象来创建内表类型
  ASSIGN dref_tab->* TO <fst_mchx>."将字段符号指向新创建出来的内表对象

  "BREAK-POINT.

ENDFORM.                    " BUILD_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_CHARACTERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_characters .
  DATA:  lt_cawn TYPE TABLE OF cawn WITH HEADER LINE.
  IF gt_class[] IS NOT INITIAL.
    READ TABLE gt_class INDEX 1.
    IF sy-subrc = 0.
      IF gt_class-class NE p_class.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_class  FROM zvmmcl01 WHERE class = p_class AND klart = p_klart.

        IF gt_class[] IS NOT INITIAL.
          SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_cawn  FROM cawn
                                               FOR ALL ENTRIES IN gt_class
                                               WHERE atinn = gt_class-atinn.
          LOOP AT gt_class.
            LOOP AT lt_cawn WHERE atinn = gt_class-atinn.
              APPEND lt_cawn TO gt_class-t_cawn.
              gt_class-onf4 ='X'.
            ENDLOOP.
            SELECT SINGLE atbez INTO gt_class-atbez FROM cabnt WHERE atinn = gt_class-atinn AND spras = sy-langu..
            IF sy-subrc NE 0.
              gt_class-atbez = gt_class-atnam.
            ENDIF.
            MODIFY gt_class.
          ENDLOOP.
        ENDIF.

      ENDIF.
    ENDIF.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_class  FROM zvmmcl01 WHERE class = p_class AND klart = p_klart.

    IF gt_class[] IS NOT INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_cawn  FROM cawn
                                           FOR ALL ENTRIES IN gt_class
                                           WHERE atinn = gt_class-atinn.
      LOOP AT gt_class.
        LOOP AT lt_cawn WHERE atinn = gt_class-atinn.
          APPEND lt_cawn TO gt_class-t_cawn.
          gt_class-onf4 ='X'.
        ENDLOOP.
        SELECT SINGLE atbez INTO gt_class-atbez FROM cabnt WHERE atinn = gt_class-atinn AND spras = sy-langu..
        IF sy-subrc NE 0.
          gt_class-atbez = gt_class-atnam.
        ENDIF.
        MODIFY gt_class.
      ENDLOOP.
    ENDIF.

  ENDIF.
ENDFORM.                    " GET_CHARACTER
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA:wa_new_line TYPE REF TO data.
  DATA:l_cabn LIKE cabn,
       l_auspdata LIKE auspdata,
       lt_chars TYPE ty_chars OCCURS 0 WITH HEADER LINE,
       lt_zvmmbat LIKE gt_zvmmbat OCCURS 0 WITH HEADER LINE,
       lt_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
       lt_valuesdescr LIKE bapicharactvaluesdescr OCCURS 0 WITH HEADER LINE,
       lt_makt LIKE makt OCCURS 0 WITH HEADER LINE,
       lt_mchy TYPE ty_mchy OCCURS 0 WITH HEADER LINE .

  FIELD-SYMBOLS:<wa_mchx>,<wa_mchx_d>,<fs_field>,<fs_field_d>,<fst_chars>.

  IF p_plant = 'X'.  "plant level.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_mchy FROM mcha
      INNER JOIN zvmmclass01 ON mcha~matnr = zvmmclass01~inob_objek
      WHERE  mcha~matnr IN s_matnr AND
             mcha~charg IN s_charg AND
             mcha~werks IN s_werks AND
             zvmmclass01~obtab = 'MARA' AND
             zvmmclass01~class = p_class AND
             zvmmclass01~klart = p_klart.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_zvmmbat
   FROM zvmmbat01 FOR ALL ENTRIES IN lt_mchy
   WHERE matnr = lt_mchy-matnr AND
         charg = lt_mchy-charg AND
         werks = lt_mchy-werks AND
         class = p_class AND klart = p_klart.
    LOOP AT lt_mchy.
      READ TABLE gt_zvmmbat WITH  KEY matnr = lt_mchy-matnr
                                      charg = lt_mchy-charg
                                      werks = lt_mchy-werks.
      IF sy-subrc NE 0.
        CLEAR gt_zvmmbat.
        MOVE-CORRESPONDING lt_mchy TO gt_zvmmbat.
        APPEND gt_zvmmbat.
      ENDIF.
    ENDLOOP.

  ELSEIF p_client = 'X'.                     "client level

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_mchy FROM mch1
      INNER JOIN zvmmclass01 ON mch1~matnr = zvmmclass01~inob_objek
      WHERE  mch1~matnr IN s_matnr AND
             mch1~charg IN s_charg AND
             zvmmclass01~obtab = 'MARA' AND
             zvmmclass01~class = p_class AND
             zvmmclass01~klart = p_klart.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_zvmmbat
     FROM zvmmbat02 FOR ALL ENTRIES IN lt_mchy
   WHERE matnr = lt_mchy-matnr AND
         charg = lt_mchy-charg AND
         class = p_class AND klart = p_klart.
    LOOP AT lt_mchy.
      READ TABLE gt_zvmmbat WITH  KEY matnr = lt_mchy-matnr
                                      charg = lt_mchy-charg.
      IF sy-subrc NE 0.
        CLEAR gt_zvmmbat.
        MOVE-CORRESPONDING lt_mchy TO gt_zvmmbat.
        APPEND gt_zvmmbat.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF gt_zvmmbat[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_makt FROM makt
        FOR ALL ENTRIES IN gt_zvmmbat
           WHERE matnr  = gt_zvmmbat-matnr AND spras = sy-langu.
  ENDIF.

  SORT gt_zvmmbat
  BY matnr charg werks atnam.

  SORT lt_makt BY matnr.

  lt_zvmmbat[] = gt_zvmmbat[].
  DELETE ADJACENT DUPLICATES FROM lt_zvmmbat COMPARING matnr charg werks.
  LOOP AT lt_zvmmbat.
    CREATE DATA wa_new_line LIKE LINE OF <fst_mchx>.  " 建立一个与动态内表结构相同的数据对象，且数据对象为是一个结构
    ASSIGN wa_new_line->* TO <wa_mchx>.
    CHECK <wa_mchx> IS ASSIGNED.

    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <wa_mchx> TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.
    <fs_field> = lt_zvmmbat-matnr.
    UNASSIGN :<fs_field>.

    ASSIGN COMPONENT 'CHARG' OF STRUCTURE <wa_mchx> TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.
    <fs_field> = lt_zvmmbat-charg.
    UNASSIGN :<fs_field>.

    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <wa_mchx> TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.
    <fs_field> = lt_zvmmbat-werks.
    UNASSIGN :<fs_field>.

    ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <wa_mchx> TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.
    READ TABLE lt_makt WITH KEY matnr = lt_zvmmbat-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_field> = lt_makt-maktx.
    ENDIF.
    UNASSIGN :<fs_field>.

    APPEND <wa_mchx> TO <fst_mchx>.
    "insert <wa_mchx> into <fst_mchx>.
    UNASSIGN :<wa_mchx>.
  ENDLOOP.

  ASSIGN dref_str_d->* TO <wa_mchx_d>.
  CHECK <wa_mchx_d> IS ASSIGNED.

  LOOP AT <fst_mchx> ASSIGNING <wa_mchx>.
    MOVE-CORRESPONDING <wa_mchx> TO gt_mchx.
    ASSIGN COMPONENT 'T_CHARS' OF STRUCTURE <wa_mchx> TO <fst_chars>.
    CLEAR lt_chars[].
    LOOP AT gt_class.

      ASSIGN COMPONENT gt_class-fieldname OF STRUCTURE <wa_mchx> TO <fs_field>.
      CHECK <fs_field> IS ASSIGNED.

      ASSIGN COMPONENT gt_class-fieldname OF STRUCTURE <wa_mchx_d> TO <fs_field_d>.
      CHECK <fs_field_d> IS ASSIGNED.

      READ TABLE gt_zvmmbat WITH KEY matnr = gt_mchx-matnr
                                     charg = gt_mchx-charg
                                     atnam = gt_class-atnam
                                     werks = gt_mchx-werks  BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING gt_class TO l_cabn.
        MOVE-CORRESPONDING gt_zvmmbat TO l_auspdata.
        CALL FUNCTION 'ZMMSP02_FM_CHAR_AUSP2OUT'
          EXPORTING
            i_cabn     = l_cabn
            i_auspdata = l_auspdata
          IMPORTING
            e_atwrt    = <fs_field>.
      ENDIF.
      "      IF gt_class-atein  = ' '.      20181017mark
      CLEAR lt_valuesdescr[].
      CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
        EXPORTING
          charactname        = gt_class-atnam
*         KEYDATE            = SY-DATUM
          language           = sy-langu
        TABLES
          charactvaluesdescr = lt_valuesdescr
          return             = lt_return.

      LOOP AT gt_zvmmbat WHERE matnr = gt_mchx-matnr AND
                                    charg = gt_mchx-charg AND
                                  atnam = gt_class-atnam AND
                                    werks = gt_mchx-werks .
        MOVE-CORRESPONDING gt_zvmmbat TO lt_chars.
        lt_chars-atbez = gt_class-atbez .

        MOVE-CORRESPONDING gt_class TO l_cabn.
        MOVE-CORRESPONDING gt_zvmmbat TO l_auspdata.
        CALL FUNCTION 'ZMMSP02_FM_CHAR_AUSP2OUT'
          EXPORTING
            i_cabn     = l_cabn
            i_auspdata = l_auspdata
          IMPORTING
            e_atwrt    = lt_chars-atwrt.

        READ TABLE lt_valuesdescr WITH KEY value_char = lt_chars-atwrt.
        IF  sy-subrc = 0.
          lt_chars-atwtb = lt_valuesdescr-description.
        ELSE.
          lt_chars-atwtb = lt_chars-atwrt.
        ENDIF.
        APPEND lt_chars.
      ENDLOOP.
      <fst_chars> = lt_chars[].
      "      ENDIF.
      UNASSIGN :<fs_field>,<fs_field_d>.
    ENDLOOP.
    MODIFY <fst_mchx> FROM <wa_mchx>.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  DATA:c_temp(40) TYPE c,
       c_output(10) TYPE c.
  gd_repid = sy-repid.
  add_field    'BOX' '选择' '' 'X' 'X' 'C' '' '' '' .
  add_field    'ICON' '状态' '' '' '' 'L' '' ''  ''.
  add_field    'MESSAGE' '消息' '' '' '' 'L' '' ''  ''.
  add_field    'MATNR' '物料' '' '' '' 'L' 'MCHA' 'MATNR' ''.
  add_field    'CHARG' '批次' '' '' '' 'L' 'MCHA' 'CHARG' 'X'.
  add_field    'MAKTX' '物料描述' '' '' '' 'L' '' '' ''.
  IF p_plant = 'X'.  "plant level.
    add_field    'WERKS' '工厂' '' '' '' 'L' 'MCHA' 'WERKS' ''.
  ENDIF.
  LOOP AT gt_class.
    IF gt_class-atein = 'X'.
      add_field    gt_class-fieldname gt_class-atbez 'X'  gt_class-box '' 'C' '' ''  '' .
    ELSE.
      IF gt_class-box = 'X'.
        CONCATENATE gt_class-atbez '[*]' INTO c_temp.
      ELSE.
        CONCATENATE gt_class-atbez '*' INTO c_temp.
      ENDIF.
      add_field    gt_class-fieldname c_temp 'X'  '' '' 'C' '' ''  '' .
    ENDIF.
  ENDLOOP.
ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .
  " gd_layout-no_input          = ' '.
  gd_layout-cwidth_opt = 'X'.
*  gd_layout-totals_text       = 'Totals'(201).
  gd_layout-zebra             = 'X'.
  "gd_layout-detail_popup      = 'X'.
  "  gd_layout-stylefname = 'FIELD_STYLE'.
*  gd_layout-totals_only        = 'X'.
*  gd_layout-f2code            = 'DISP'.  "Sets fcode for when double
*                                         "click(press f2)
*  gd_layout-zebra             = 'X'.
*  gd_layout-group_change_edit = 'X'.
*  gd_layout-header_text       = 'helllllo'.

  "gd_layout-box_fieldname = 'BOX'.

  "ST_LAYO-ZEBRA = 'X'.“显示成斑马纹样式
  "ST_LAYO-DETAIL_POPUP = 'X'.“是否弹出详细信息窗口
  "ST_LAYO-F2CODE = '&ETA'. “设置触发弹出详细信息窗口的功能码，这里是双击
  "ST_LAYO-COLWIDTH_OPTIMIZE = 'X'. “优化列宽选项是否设置
  "ST_LAYO-DETAIL_INITIAL_LINES  = 'X'.
  "ST_LAYO -no_vline = 'X'.“这个用来设置列间隔线
  "ST_LAYO -detail_titlebar = '详细内容'. “设置弹出窗口的标题栏
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_report .
  DATA:lt_events TYPE slis_t_event WITH HEADER LINE,
       title   TYPE lvc_title,
       lc_glay TYPE lvc_s_glay.

  lc_glay-edt_cll_cb = 'X'.  "自动刷新内表

  lt_events-name = 'CALLER_EXIT'. "slis_ev_caller_exit_at_start事件
  lt_events-form = 'F4_FORM'.
  APPEND lt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = gd_repid
*     i_callback_top_of_page   = 'TOP-OF-PAGE' "see FORM
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_PF_STATUS'
      "i_grid_title             = title
      i_grid_settings          = lc_glay
      is_layout_lvc            = gd_layout
      it_fieldcat_lvc          = gd_fieldcat[]
*     it_special_groups        = gd_tabgroup
      it_events                = lt_events[]
      i_save                   = 'A'
*     is_variant               = z_template
    TABLES
      t_outtab                 = <fst_mchx>
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



ENDFORM.                    " DISPLAY_ALV_REPORT


*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EXTAB      text
*----------------------------------------------------------------------*
FORM set_pf_status USING extab TYPE slis_t_extab.
  " SET PF-STATUS 'ALV001'. " EXCLUDING EXTAB.
  SET PF-STATUS 'STANDARD_FULLSCREEN'. " EXCLUDING EXTAB.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                  selfield  TYPE slis_selfield..
*  selfield TYPE slis_selfield.
*  READ TABLE gt_mbst INDEX rs_selfield-tabindex.
*  CHECK sy-subrc = 0.
  DATA: l_valid TYPE c.
  DATA stbl TYPE lvc_s_stbl. "稳定刷新

  DATA:it_rows TYPE lvc_t_fidx.
  DATA:wa_rows LIKE LINE OF it_rows.

  FIELD-SYMBOLS:<wa_mchx>,<fs_field> .

  stbl-row = 'X'. "基于行的稳定刷新
  stbl-col = 'X'. "基于列稳定刷新

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_grid.



  CASE ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM data_save.
      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = stbl
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.
    WHEN 'SALL'.
      CALL METHOD g_grid->get_filtered_entries
        IMPORTING
          et_filtered_entries = it_rows.
      LOOP AT <fst_mchx> ASSIGNING <wa_mchx>.
        READ TABLE it_rows INTO wa_rows WITH KEY table_line = sy-tabix.
        CHECK sy-subrc NE 0.
        ASSIGN COMPONENT 'BOX' OF STRUCTURE <wa_mchx> TO <fs_field>.
        CHECK <fs_field> IS ASSIGNED.
        <fs_field> = 'X'.
        UNASSIGN :<fs_field>.
      ENDLOOP.
      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = stbl
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.
    WHEN 'DSEL'.
      LOOP AT <fst_mchx> ASSIGNING <wa_mchx>.
        ASSIGN COMPONENT 'BOX' OF STRUCTURE <wa_mchx> TO <fs_field>.
        CHECK <fs_field> IS ASSIGNED.
        <fs_field> = ' '.
        UNASSIGN :<fs_field>.
      ENDLOOP.
      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = stbl
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.

    WHEN '&IC1'.

      CASE selfield-fieldname.
        WHEN 'CHARG'.
          READ TABLE <fst_mchx> ASSIGNING <wa_mchx> INDEX selfield-tabindex.
          CHECK sy-subrc = 0.
          MOVE-CORRESPONDING <wa_mchx> TO gt_mchx.
          SET PARAMETER ID: 'MAT' FIELD gt_mchx-matnr.
          SET PARAMETER ID: 'CHA' FIELD gt_mchx-charg.
          SET PARAMETER ID: 'WRK' FIELD gt_mchx-werks.
          AUTHORITY-CHECK OBJECT 'M_MATE_CHG'
                   ID 'ACTVT' FIELD '02'
                   ID 'BEGRU' FIELD '*'.
          IF sy-subrc =  0.
            CALL TRANSACTION 'MSC2N' AND SKIP FIRST SCREEN.
          ELSE.
            CALL TRANSACTION 'MSC3N' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN OTHERS.

      ENDCASE.
    WHEN '&QCHG'.
      PERFORM mass_change.

  ENDCASE.


ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  SELECT_CHARS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_chars .
  CASE sscrfields-ucomm.
    WHEN 'CLK'.
**--Display Screen with the list of fields
      PERFORM genr_chars.
  ENDCASE.
ENDFORM.                    " SELECT_CHARS
*&---------------------------------------------------------------------*
*&      Form  GENR_CHARS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM genr_chars .
  CALL SCREEN 100 STARTING AT 10 2
  ENDING AT 70 22..

ENDFORM.                    " GENR_CHARS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: lv_txt TYPE string.
  lv_txt = '选择需要更新的特性值'(008).
  SET PF-STATUS 'DIALOG_100'.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  SUPPRESS DIALOG.
  MOVE lv_txt TO sy-title.
  PERFORM get_characters .

  LOOP AT gt_class.
    AT FIRST.
      WRITE: (60) sy-uline.
    ENDAT.
    IF gt_class-attab CP 'MCH*'.
      WRITE:/ '|', ' '.
    ELSE.
      WRITE:/ '|' ,gt_class-box AS CHECKBOX .
    ENDIF.
    WRITE: (30) gt_class-atnam,'|', gt_class-atbez, AT 60 '|'.
    HIDE: gv_temp, gt_class-atnam, gt_class-atbez.

    AT LAST.
      WRITE: (60) sy-uline.
    ENDAT.
  ENDLOOP..

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_BUTTON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_button .
  DATA:l_count TYPE i,
       l_c(3) TYPE c,
       lc_button(20).
  LOOP AT gt_class WHERE box = 'X'.
    l_count  = l_count + 1.
  ENDLOOP.
  l_c = l_count.

  lc_button = text-005.
  REPLACE ALL OCCURRENCES OF '$' IN lc_button WITH l_c.
  p_btn = lc_button."(006).


ENDFORM.                    " REFRESH_BUTTON
*&---------------------------------------------------------------------*
*&      Form  DATA_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_save .
  DATA:ls_clbatch         TYPE  clbatch,
       lt_clbatch         TYPE TABLE OF clbatch,
       ls_mcha            TYPE mcha,
       lv_subrc           TYPE  sy-subrc,
       lv_message         TYPE  bapiret1-message,
       lt_return          TYPE TABLE OF bapiret2,
       ls_return          TYPE  bapiret2,
       l_str(100),
       ls_chars TYPE  ty_chars,
       l_tabix LIKE sy-tabix .

  DATA:it_rows TYPE lvc_t_fidx.
  DATA:wa_rows LIKE LINE OF it_rows.

  FIELD-SYMBOLS:
  "<fs_table>    TYPE ANY TABLE,
  <fs_field>    TYPE any,
  <wa_mchx>,
  <fst_chars> TYPE STANDARD TABLE.

  DEFINE mcr_input_data.
    ASSIGN COMPONENT &1 OF STRUCTURE  <wa_mchx> TO <fs_field>.
    IF <fs_field> IS ASSIGNED.
      &2 = <fs_field>.
      unassign <fs_field>.
    ENDIF.
  END-OF-DEFINITION.

  CALL METHOD g_grid->get_filtered_entries
    IMPORTING
      et_filtered_entries = it_rows.

  LOOP AT <fst_mchx> ASSIGNING <wa_mchx>.

    READ TABLE it_rows INTO wa_rows WITH KEY table_line = sy-tabix.
    CHECK sy-subrc NE 0.
    MOVE-CORRESPONDING <wa_mchx> TO gt_mchx.
    CHECK gt_mchx-box = 'X' AND gt_mchx-icon IS INITIAL.

    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <wa_mchx> TO <fs_field>.
    IF <fs_field> IS ASSIGNED.
      ls_mcha-matnr = <fs_field>.
      UNASSIGN <fs_field>.
    ENDIF.

    ASSIGN COMPONENT 'CHARG' OF STRUCTURE <wa_mchx> TO <fs_field>.
    IF <fs_field> IS ASSIGNED.
      ls_mcha-charg = <fs_field>.
      UNASSIGN <fs_field>.
    ENDIF.

    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <wa_mchx> TO <fs_field>.
    IF <fs_field> IS ASSIGNED.
      ls_mcha-werks = <fs_field>.
      UNASSIGN <fs_field>.
    ENDIF.

    LOOP AT gt_class WHERE box = 'X'.
      IF gt_class-atein  = 'X'.
        ls_clbatch-atnam = gt_class-atnam.
        mcr_input_data gt_class-fieldname ls_clbatch-atwtb.
        IF ls_clbatch-atwtb IS NOT INITIAL.
          APPEND ls_clbatch TO lt_clbatch.
          CLEAR ls_clbatch.
        ELSE.       "20181017mark
          ASSIGN COMPONENT 'T_CHARS' OF STRUCTURE <wa_mchx> TO <fst_chars>.
          IF <fst_chars> IS ASSIGNED.
            LOOP AT <fst_chars> INTO  ls_chars .
              IF ls_chars-atnam  = gt_class-atnam.
                l_tabix = sy-tabix.
                ls_clbatch-atnam = ls_chars-atnam.
                ls_clbatch-atwtb = ls_chars-atwtb.
                ls_clbatch-xdelete = 'X'.
                APPEND ls_clbatch TO lt_clbatch.
                CLEAR ls_clbatch.
              ENDIF.
            ENDLOOP.
            UNASSIGN <fst_chars>.
          ENDIF.

        ENDIF.
      ELSE.
        ASSIGN COMPONENT 'T_CHARS' OF STRUCTURE <wa_mchx> TO <fst_chars>.
        IF <fst_chars> IS ASSIGNED.
          LOOP AT <fst_chars> INTO  ls_chars.
            IF ls_chars-atnam  = gt_class-atnam.
              l_tabix = sy-tabix.
              ls_clbatch-atnam = ls_chars-atnam.
              ls_clbatch-atwtb = ls_chars-atwtb.
              ls_clbatch-xdelete = ls_chars-xdelete.
              APPEND ls_clbatch TO lt_clbatch.
              CLEAR ls_clbatch.
            ENDIF.
          ENDLOOP.
          UNASSIGN <fst_chars>.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lt_clbatch IS NOT INITIAL.
      CLEAR:lv_subrc   ,
            lv_message .

      CALL FUNCTION 'ZMMSP02_FM_CHANGE_BATCH'
        EXPORTING
*         I_TEST    =
          i_mcha    = ls_mcha
*         i_lgort   = ''
        IMPORTING
          e_subrc   = lv_subrc
          e_message = lv_message
        TABLES
          t_clbatch = lt_clbatch
          t_return  = lt_return.
      IF lv_subrc <> 0.

        ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <wa_mchx> TO <fs_field>.
        IF <fs_field> IS ASSIGNED.
*          CLEAR l_str.
*          LOOP AT lt_return INTO ls_return WHERE type = 'E'.
*            CONCATENATE l_str  ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4 ';'
*                        INTO l_str SEPARATED BY ' '.
*          ENDLOOP.
          CONCATENATE  lv_message  l_str  INTO <fs_field> SEPARATED BY ' ' .
          UNASSIGN <fs_field>.
        ENDIF.

        ASSIGN COMPONENT 'ICON' OF STRUCTURE <wa_mchx> TO <fs_field>.
        IF <fs_field> IS ASSIGNED.
          <fs_field> = c_icon_e.
          UNASSIGN <fs_field>.
        ENDIF.

      ELSE.
        ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <wa_mchx> TO <fs_field>.
        IF <fs_field> IS ASSIGNED.
          CONCATENATE '批次更新' '成功' INTO <fs_field>.
          UNASSIGN <fs_field>.
        ENDIF.

        ASSIGN COMPONENT 'ICON' OF STRUCTURE <wa_mchx> TO <fs_field>.
        IF <fs_field> IS ASSIGNED.
          <fs_field> = c_icon_s.
          UNASSIGN <fs_field>.
        ENDIF.
      ENDIF.
    ENDIF.
    REFRESH:lt_clbatch.
    MODIFY <fst_mchx> FROM <wa_mchx>.
  ENDLOOP.


ENDFORM.                    " DATA_SAVE


*&---------------------------------------------------------------------*
*&      Form  DATA_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_form USING e_grid TYPE slis_data_caller_exit.
  DATA: lv_event_receiver TYPE REF TO lcl_event_receiver,
        lt_f4 TYPE lvc_t_f4,
        ls_f4 TYPE lvc_s_f4.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = g_grid.
  LOOP AT gt_class." WHERE onf4 = 'X'.
    ls_f4-fieldname = gt_class-fieldname. "窗口时间参数（需要定义f4帮助按钮的字段）
    ls_f4-register = 'X'.
    ls_f4-getbefore = 'X'.
    ls_f4-chngeafter = 'X'.
    INSERT ls_f4 INTO TABLE lt_f4.
  ENDLOOP.
  CREATE OBJECT lv_event_receiver.
  SET HANDLER lv_event_receiver->handle_f4 FOR g_grid.

  CALL METHOD g_grid->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4[].
ENDFORM.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_f4.
* 窗口时间参数的自定义f4检索帮助
    PERFORM f4_help_characters USING e_fieldname
                                     e_fieldvalue "  Type  LVC_VALUE
                                     es_row_no.
* 设置后，alv稳定刷新
    PERFORM refresh_table_alv.
  ENDMETHOD. "HANDLE_F4

ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  REFRESH_TABLE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_table_alv .
  DATA: stbl TYPE lvc_s_stbl.
*
  stbl-row = 'X'."基于行的稳定刷新
  stbl-col = 'X'." 基于列稳定刷新
  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = stbl.
ENDFORM.                    " REFRESH_TABLE_ALV
*&---------------------------------------------------------------------*
*&      Form  F4_HELP_CHARACTERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM f4_help_characters  USING   p_fieldname TYPE lvc_fname
                                  p_fieldvalue  TYPE  lvc_value
                                  p_row_no TYPE lvc_s_roid
                                 .
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        ls_return TYPE ddshretval,
        l_retfield TYPE dfies-fieldname,
        l_dynprofield TYPE help_info-dynprofld,
        lt_values  TYPE api_value OCCURS 0,
        ls_values  TYPE api_value,
        lt_chars TYPE ty_chars OCCURS 0 WITH HEADER LINE,
        l_value TYPE atwtb,
        lt_imp_values TYPE TABLE OF api_value WITH HEADER LINE,
        l_display TYPE c,
        l_mode TYPE c,
        l_cabn LIKE cabn,
        l_auspdata LIKE auspdata.

  DATA:BEGIN OF lt_t001w OCCURS 0,
        werks LIKE t001w-werks,
        name1 LIKE t001w-name1,
       END OF lt_t001w.

  FIELD-SYMBOLS:
   "<fs_table>    TYPE ANY TABLE,
   <fs_field>    TYPE any,
   <wa_mchx>,
   <fst_chars> TYPE STANDARD TABLE,
   <wa_chars>.

  READ TABLE <fst_mchx> ASSIGNING <wa_mchx> INDEX p_row_no-row_id.
  CHECK sy-subrc = 0.
  MOVE-CORRESPONDING <wa_mchx> TO gt_mchx.
  l_retfield = p_fieldname.
  l_dynprofield = p_fieldname.
  l_value = p_fieldvalue.

  ASSIGN COMPONENT 'T_CHARS' OF STRUCTURE <wa_mchx> TO <fst_chars>.
  CHECK <fst_chars> IS ASSIGNED.
  CLEAR:lt_chars[].
  LOOP AT <fst_chars> INTO lt_chars .
    APPEND lt_chars.
  ENDLOOP.

  LOOP AT gt_class WHERE fieldname = p_fieldname." AND onf4 = 'X'.
    CLEAR:lt_imp_values[].
    IF gt_class-box  = 'X'.
      l_display = space.
    ELSE.
      l_display = 'X'.
    ENDIF.

    IF gt_class-atein  = ' '.
      l_mode = 'S'.
      CLEAR l_value.
    ELSE.
      l_mode = space.
    ENDIF.
    IF gt_class-atein  = ' '.
      LOOP AT  lt_chars WHERE atnam = gt_class-atnam.
        MOVE-CORRESPONDING lt_chars TO lt_imp_values.
        IF lt_imp_values-atinn = gt_class-atinn AND lt_chars-xdelete = ' '.
          APPEND lt_imp_values.
        ENDIF.
      ENDLOOP.
    ENDIF.
*    CALL FUNCTION 'RMSA950_CTMS_CHAR_VALUE_F4'
*      EXPORTING
*        i_characteristic = gt_class-atinn
*        i_mode           = l_mode
*        i_display        = l_display
**       I_LANGUAGE       = SY-LANGU
**       I_DATE           = SY-DATUM
*        i_value          = l_value
*      IMPORTING
*        et_values        = lt_values.

*    CALL FUNCTION 'CTMS_CHAR_VALUE_F4'
*      EXPORTING
*        imp_characteristic       = gt_class-atinn
**       IMP_MODE                 = ' '
**       IMP_DISPLAY              = ' '
**       IMP_LANGUAGE             = SY-LANGU
**       IMP_DATE                 = SY-DATUM
*      tables
*        exp_values               = lt_values
    .

    CALL FUNCTION 'C107VAT_CTMS_CHAR_VALUE_F4'
      EXPORTING
        imp_characteristic = gt_class-atinn
        imp_mode           = l_mode
        imp_display        = l_display
*       IMP_LANGUAGE       = SY-LANGU
*       IMP_DATE           = SY-DATUM
        imp_value          = l_value
*       IMP_FLG_MULTIPLE_VALUES       = ESP1_FALSE
*       I_ESTCAT           =
*       I_ATFOR            =
*       I_FLG_IS_PHRASED   = ' '
        i_inst_tabix       = 0
      TABLES
        exp_values         = lt_values
        imp_values         = lt_imp_values
      EXCEPTIONS
        cancelled_by_user  = 1
        OTHERS             = 2.

    IF sy-subrc = 0 AND gt_class-box  = 'X'.
      READ TABLE lt_values INTO ls_values INDEX 1.
      IF sy-subrc = 0.
        ASSIGN COMPONENT p_fieldname OF STRUCTURE <wa_mchx> TO <fs_field>.
        IF  <fs_field> IS ASSIGNED.
          <fs_field> = ls_values-atwrt.
        ENDIF.
      ELSE.
        ASSIGN COMPONENT p_fieldname OF STRUCTURE <wa_mchx> TO <fs_field>.
        IF  <fs_field> IS ASSIGNED.
          <fs_field> = ''.
        ENDIF.
      ENDIF.

      IF gt_class-atein  = ' '.
        LOOP AT lt_chars WHERE atnam = gt_class-atnam..
          READ TABLE lt_values INTO ls_values WITH KEY atnam = lt_chars-atnam  atwrt = lt_chars-atwrt .
          IF sy-subrc = 0.
            lt_chars-xdelete = ' '.
          ELSE.
            lt_chars-xdelete = 'X'.
          ENDIF.
          MODIFY lt_chars.
        ENDLOOP.
        LOOP AT lt_values INTO ls_values.
          READ TABLE lt_chars WITH KEY atnam = ls_values-atnam  atwrt = ls_values-atwrt.
          IF sy-subrc <> 0.
            CLEAR lt_chars.
            MOVE-CORRESPONDING ls_values TO lt_chars.
            APPEND lt_chars.
          ELSE.
          ENDIF.
        ENDLOOP.
        " ASSIGN COMPONENT 'T_CHARS' OF STRUCTURE <wa_mchx> TO <fst_chars>.
        <fst_chars> = lt_chars[].
      ENDIF.
    ENDIF.
    "ENDIF.
  ENDLOOP.
ENDFORM.                    " F4_HELP_CHARACTERS
*&---------------------------------------------------------------------*
*&      Form  GET_TCUCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tcuch .

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF gs_tcuch FROM tcuch.
  IF gs_tcuch-kzdch = '0'.
    p_klart = '022'.
    p_plant =  'X'.
  ELSE.
    p_klart = '023'.
    p_client = 'X'.
  ENDIF.
  PERFORM modify_screen.

ENDFORM.                    " GET_TCUCH
*&---------------------------------------------------------------------*
*&      Form  MASS_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mass_change .
  DATA:it_columns TYPE lvc_t_col .
  DATA:wa_columns LIKE LINE OF it_columns.
  DATA:l_subrc LIKE sy-subrc,
       char_values LIKE  api_value OCCURS 0 WITH HEADER LINE,
       char_inact LIKE api_char  OCCURS 0 WITH HEADER LINE,
       lt_chars TYPE ty_chars OCCURS 0 WITH HEADER LINE,
       l_selected TYPE c.
  DATA:it_rows TYPE lvc_t_fidx.
  DATA:wa_rows LIKE LINE OF it_rows..
  FIELD-SYMBOLS:
    <fs_field>    TYPE any,
    <wa_mchx>,
    <fst_chars> TYPE STANDARD TABLE.

  CALL METHOD g_grid->get_selected_columns
    IMPORTING
      et_index_columns = it_columns.

  IF  it_columns[] IS INITIAL.
    MESSAGE '无选中的列!' TYPE 'S'.
    RETURN.
  ENDIF.
  LOOP AT gt_class  .
    READ TABLE it_columns INTO wa_columns WITH KEY fieldname = gt_class-fieldname.
    IF sy-subrc = 0 AND gt_class-box = 'X' .
    ELSE.
      char_inact-atinn =  gt_class-atinn.
      char_inact-atnam = gt_class-atnam.
      APPEND char_inact.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'ZMMSP02_FM_GET_VALUES'
    EXPORTING
      class                     = p_class
      classtype                 = p_klart
*     MODE                      = 'S'
    IMPORTING
      e_subrc                   = l_subrc
    TABLES
      exp_values                = char_values
*     EXP_ATTRIBUTES            =
      imp_characteristics_inact = char_inact
    EXCEPTIONS
      cancel                    = 5
      class_not_found           = 1
      ddb_has_no_class          = 2
      no_attributes             = 3
      value_not_found           = 4
      no_characteristics        = 6
      OTHERS                    = 99.
  IF l_subrc = 0.
    LOOP AT <fst_mchx> ASSIGNING <wa_mchx>.
      READ TABLE it_rows INTO wa_rows WITH KEY table_line = sy-tabix.
      CHECK sy-subrc NE 0.
      MOVE-CORRESPONDING <wa_mchx> TO gt_mchx.
      CHECK gt_mchx-box = 'X'.
      l_selected = 'X'.
      ASSIGN COMPONENT 'T_CHARS' OF STRUCTURE <wa_mchx> TO <fst_chars>.
      CHECK <fst_chars> IS ASSIGNED.
      CLEAR:lt_chars[].
      LOOP AT <fst_chars> INTO lt_chars .
        APPEND lt_chars.
      ENDLOOP.

      LOOP AT char_values.
        READ TABLE gt_class WITH KEY atinn = char_values-atinn.
        IF sy-subrc = 0.
          ASSIGN COMPONENT gt_class-fieldname OF STRUCTURE <wa_mchx> TO <fs_field>.
          IF  <fs_field> IS ASSIGNED.
            <fs_field> = char_values-atwrt.
            UNASSIGN <fs_field>.
          ENDIF.

          IF gt_class-atein  = ' '."多值情况
            READ TABLE lt_chars WITH KEY atnam = char_values-atnam  atwrt = char_values-atwrt.
            IF sy-subrc <> 0.
              CLEAR lt_chars.
              MOVE-CORRESPONDING char_values TO lt_chars.
              APPEND lt_chars.
            ELSE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT lt_chars WHERE atnam = gt_class-atnam.  "多值情况
        READ TABLE char_values  WITH KEY atnam = lt_chars-atnam  atwrt = lt_chars-atwrt.
        IF sy-subrc = 0.
          lt_chars-xdelete = ' '.
        ELSE.
          lt_chars-xdelete = 'X'.
        ENDIF.
        MODIFY lt_chars.
      ENDLOOP.

      <fst_chars> = lt_chars[].
    ENDLOOP.
    IF l_selected = 'X'.
      PERFORM refresh_table_alv .
    ELSE.
      MESSAGE '无选中的行！' TYPE 'S'.
    ENDIF.
  ELSEIF l_subrc = 5.
    MESSAGE '已取消!' TYPE 'S'.
  ELSEIF l_subrc = 4.
    MESSAGE '无修改!' TYPE 'S'.
  ENDIF.


ENDFORM.                    " MASS_CHANGE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-group1 = 'G1'.
      screen-input = '0'.
    ENDIF.
    IF p_client = 'X' AND screen-group1 = 'P1'.
      "screen-active = '0'.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
