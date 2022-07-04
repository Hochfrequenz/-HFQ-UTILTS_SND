*&---------------------------------------------------------------------*
*& Report /HFQ/RP_DISP_CALC_FORM
*&---------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Dieser Report dient zum Anzeigen der Berechnungs-Formel-Konstrukte
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-09-16  Hochfrequenz    angelegt
*&---------------------------------------------------------------------*
REPORT /hfq/rp_disp_calc_form.

TYPES: BEGIN OF l_t_outtab,
         calc_form_stat TYPE /hfq/calc_form_h-calc_form_stat,
         math_operator  TYPE /hfq/calc_form-math_operator,
         flow_direction TYPE /hfq/calc_form-flow_direction,
         lossfact_ext   TYPE /hfq/calc_form-lossfact_ext,
         lossfact_line  TYPE /hfq/calc_form-lossfact_line,
         melo_extui     TYPE /hfq/calc_form-melo_extui,
       END OF l_t_outtab.

TYPES: BEGIN OF l_t_malo,
         malo                  TYPE /hfq/calc_form_h-malo_extui,
         cf_guid               TYPE /hfq/calc_form_h-cf_guid,
         calc_step_amount_malo TYPE /hfq/calc_form_h-calc_step_amount_malo,
       END OF l_t_malo.

DATA: lo_tree      TYPE REF TO cl_salv_tree,
      lo_nodes     TYPE REF TO cl_salv_nodes,
      lo_node      TYPE REF TO cl_salv_node,
      lo_item      TYPE REF TO cl_salv_item,
      lo_columns   TYPE REF TO cl_salv_columns_tree,
      lo_column    TYPE REF TO cl_salv_column,
      lo_functions TYPE REF TO cl_salv_functions_tree,
      lo_settings  TYPE REF TO cl_salv_tree_settings,
      lo_events    TYPE REF TO cl_salv_events_tree,
      lo_layout    TYPE REF TO cl_salv_layout.
DATA: lt_nodes      TYPE salv_t_nodes,
      ls_node       TYPE salv_s_nodes,
      lv_layout_key TYPE salv_s_layout_key,
      lv_title      TYPE salv_de_tree_text,
      lv_title_part TYPE salv_de_tree_text.

DATA: lv_value             TYPE lvc_value,
      lv_malo_key          TYPE lvc_nkey,
      lv_timeslice_key     TYPE lvc_nkey,
      lv_top_calc_step_key TYPE lvc_nkey,
      lv_calc_step_key     TYPE lvc_nkey,
      lv_sub_calc_step_key TYPE lvc_nkey.
DATA: lt_outtab        TYPE TABLE OF l_t_outtab,
      ls_outtab        TYPE l_t_outtab,
      lt_sourcetab     TYPE /hfq/t_cf_disp_sourcetab,
      ls_sourcetab     TYPE /hfq/s_cf_disp_sourcetab,
      lt_malo          TYPE TABLE OF l_t_malo,
      ls_malo          TYPE l_t_malo,
      lt_timeslices    TYPE /hfq/t_cf_disp_timeslice,
      ls_timeslices    TYPE /hfq/s_cf_disp_timeslice,
      lt_top_calc_step TYPE /hfq/t_cf_disp_calc_step,
      ls_top_calc_step TYPE /hfq/s_cf_disp_calc_step,
      lt_calc_step     TYPE /hfq/t_cf_disp_calc_step,
      ls_calc_step     TYPE /hfq/s_cf_disp_calc_step,
      lt_sub_calc_step TYPE /hfq/t_cf_disp_calc_step,
      ls_sub_calc_step TYPE /hfq/s_cf_disp_calc_step.

DATA: lv_to_date    TYPE dats,
      lv_from_date  TYPE dats,
      lt_malo_range TYPE isu_ranges_tab,
      lv_malo_step  TYPE numc5.

DATA: lt_batchdata TYPE STANDARD TABLE OF bdcdata.

DATA: lo_badi_utilts_disp TYPE REF TO /hfq/badi_utilts_disp.

DATA lv_data TYPE REF TO data.
FIELD-SYMBOLS <fs_data> TYPE any.

CONSTANTS: lc_root_key TYPE lvc_nkey VALUE '',
           lc_enddate  TYPE datum VALUE '99991231'.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-DATA:
      tree TYPE REF TO cl_salv_tree.
    INTERFACES if_salv_events_tree.
    CLASS-METHODS expand
      FOR EVENT expand_nc
      OF cl_gui_alv_tree IMPORTING node_key.
    CLASS-METHODS double_click
      FOR EVENT double_click OF cl_salv_events_tree IMPORTING columnname node_key.
    CLASS-METHODS node_key
     FOR EVENT node_keypress OF cl_gui_alv_tree IMPORTING node_key key.
    CLASS-METHODS on_function_click
  FOR EVENT if_salv_events_functions~added_function
    OF cl_salv_events_tree IMPORTING e_salv_function.
  PROTECTED SECTION.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD expand.
    IF 1 = 2.
      DATA: lt_nodes TYPE salv_t_nodes,
            ls_nodes TYPE salv_s_nodes.

      TRY.
          lt_nodes = lo_nodes->get_all_nodes( ).
          LOOP AT lt_nodes INTO ls_nodes.
            IF ls_nodes-node->is_visible( ) EQ abap_true.
              lv_data = ls_nodes-node->get_data_row( ).
              ASSIGN lv_data->* TO <fs_data>.
              ls_outtab = <fs_data>.
            ENDIF.
          ENDLOOP.
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD node_key.
    IF 1 = 2.

    ENDIF.
  ENDMETHOD.
  METHOD on_function_click.
    CASE e_salv_function.
*       do action
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
  METHOD double_click.
    DATA: lo_nodes   TYPE REF TO cl_salv_nodes,
          lo_node    TYPE REF TO cl_salv_node,
          ls_outtab  TYPE l_t_outtab,
          lv_data    TYPE REF TO data,
          rspar_tab  TYPE TABLE OF rsparams,
          rspar_line LIKE LINE OF rspar_tab.

    FIELD-SYMBOLS <fs_data> TYPE any.
    TRY.
        lo_nodes = tree->get_nodes( ).
        lo_node = lo_nodes->get_node( node_key ).
        lv_data = lo_node->get_data_row( ).
        ASSIGN lv_data->* TO <fs_data>.
        ls_outtab = <fs_data>.

        CASE columnname.
          WHEN '&Hierarchy'.
            CLEAR: lt_batchdata.
            lt_batchdata = VALUE #(
                        ( program = 'SAPLEEDM_DLG_FRAME' dynpro = '0100' dynbegin = 'X' fnam = '' fval = '' )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'BDC_CURSOR' fval = 'S_OBJ-EXTUI-UISTRUTYP' )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'S_OBJ-EXTUI-UISTRUTYP' fval = '01' )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'BDC_OKCODE' fval = '=OKAY' )
                        ( program = 'SAPLEEDM_DLG_FRAME' dynpro = '0100' dynbegin = 'X' fnam = '' fval = '' )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'BDC_CURSOR' fval = 'EUI_EXT_TYP_FLAT_SEL-EXT_UI' )
                        ( program = 'SAPLEEDM_DLG_FRAME' dynpro = '0100' dynbegin = 'X' fnam = 'EUI_EXT_TYP_FLAT_SEL-EXT_UI' fval = ls_malo-malo )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'EEDM_TREE_DATA_FINDER-AB' fval = |{ lv_from_date DATE = ENVIRONMENT }| )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'EEDM_TREE_DATA_FINDER-BIS' fval = |{ lv_to_date DATE = ENVIRONMENT }| )
                        ).

            CALL TRANSACTION 'EEDM11' WITH AUTHORITY-CHECK
              USING lt_batchdata
                    MODE 'E'.

          WHEN 'MELO_EXTUI'.
            CLEAR: lt_batchdata.
            lt_batchdata = VALUE #(
                        ( program = 'SAPLEEDM_DLG_FRAME' dynpro = '0100' dynbegin = 'X' fnam = '' fval = '' )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'BDC_CURSOR' fval = 'S_OBJ-EXTUI-UISTRUTYP' )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'S_OBJ-EXTUI-UISTRUTYP' fval = '01' )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'BDC_OKCODE' fval = '=OKAY' )
                        ( program = 'SAPLEEDM_DLG_FRAME' dynpro = '0100' dynbegin = 'X' fnam = '' fval = '' )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'BDC_CURSOR' fval = 'EUI_EXT_TYP_FLAT_SEL-EXT_UI' )
                        ( program = 'SAPLEEDM_DLG_FRAME' dynpro = '0100' dynbegin = 'X' fnam = 'EUI_EXT_TYP_FLAT_SEL-EXT_UI' fval = ls_outtab-melo_extui )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'EEDM_TREE_DATA_FINDER-AB' fval = |{ lv_from_date DATE = ENVIRONMENT }| )
                        ( program = '' dynpro = '' dynbegin = '' fnam = 'EEDM_TREE_DATA_FINDER-BIS' fval = |{ lv_to_date DATE = ENVIRONMENT }| )
                        ).

            CALL TRANSACTION 'EEDM11' WITH AUTHORITY-CHECK
              USING lt_batchdata
                    MODE 'E'.
          WHEN OTHERS.

        ENDCASE.
      CATCH cx_salv_msg.

    ENDTRY.

  ENDMETHOD.


ENDCLASS.


SELECT-OPTIONS so_malo FOR ls_sourcetab-malo_extui MATCHCODE OBJECT /idxgl/sh_ext_malo.
PARAMETERS:
  p_von TYPE /hfq/calc_form_h-validstart_date DEFAULT sy-datum MATCHCODE OBJECT bu_date_char,
  p_bis TYPE /hfq/calc_form_h-validend_date DEFAULT lc_enddate.

START-OF-SELECTION.

  lt_malo_range = so_malo[].

  IF p_von IS INITIAL.
    lv_from_date = sy-datum.
  ELSE.
    lv_from_date = p_von.
  ENDIF.
  IF p_bis IS INITIAL.
    lv_to_date = lc_enddate.
  ELSE.
    lv_to_date = p_bis.
  ENDIF.

  TRY.
      cl_salv_tree=>factory(
        IMPORTING
          r_salv_tree = lo_tree
        CHANGING
          t_table      = lt_outtab ).
    CATCH cx_salv_no_new_data_allowed cx_salv_error.
      EXIT.
  ENDTRY.
  lo_events = lo_tree->get_event( ).
  SET HANDLER lcl_event_handler=>double_click FOR lo_events.
  SET HANDLER lcl_event_handler=>on_function_click FOR lo_events.
  SET HANDLER lcl_event_handler=>expand FOR ALL INSTANCES.
  SET HANDLER lcl_event_handler=>node_key FOR ALL INSTANCES.
  lcl_event_handler=>tree = lo_tree.

  lo_functions = lo_tree->get_functions( ).

  lo_functions->set_group_layout( abap_true ).
  lo_functions->set_expand( abap_true ).
  lo_layout = lo_tree->get_layout( ).

  lv_layout_key-report = sy-repid.
  lo_layout->set_key( lv_layout_key ).
  lo_layout->set_save_restriction( ).


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
*     PERCENTAGE       = 0
      text = 'Lese Hierarchie.'.

  GET BADI lo_badi_utilts_disp.
  TRY.
      CALL BADI lo_badi_utilts_disp->fill_sourcetab
        EXPORTING
          it_malo      = lt_malo_range
          iv_startdate = lv_from_date
          iv_enddate   = p_bis
        IMPORTING
          et_sourcetab = lt_sourcetab.
      IF lt_sourcetab IS INITIAL.
        MESSAGE |Quellstruktur nach Aufruf des BAdI nicht befüllt. Keine Einträge zu finden.| TYPE 'I'.
        RETURN.
      ENDIF.

      CALL BADI lo_badi_utilts_disp->fill_timeslices
        EXPORTING
          it_malo       = lt_malo_range
          iv_startdate  = lv_from_date
          iv_enddate    = p_bis
        IMPORTING
          et_timeslices = lt_timeslices.
      IF lt_timeslices IS INITIAL.
        MESSAGE |Zeitscheiben nach Aufruf des BAdI nicht befüllt.| TYPE 'E'.
      ENDIF.

      CALL BADI lo_badi_utilts_disp->fill_calc_steps
        EXPORTING
          it_malo          = lt_malo_range
          iv_startdate     = lv_from_date
          iv_enddate       = p_bis
        IMPORTING
          et_top_calc_step = lt_top_calc_step.
      IF lt_top_calc_step IS INITIAL.
        MESSAGE |Keine Berechnungsschritte nach Aufruf des BAdI.| TYPE 'E'.
      ENDIF.

      CALL BADI lo_badi_utilts_disp->fill_calc_sub_steps
        EXPORTING
          it_malo          = lt_malo_range
          iv_startdate     = lv_from_date
          iv_enddate       = p_bis
        IMPORTING
          et_sub_calc_step = lt_sub_calc_step.
      IF lt_sub_calc_step IS INITIAL.
        "Unterschritte können leer bleiben.
*       MESSAGE |Quellstruktur nach Aufruf des BAdI nicht befüllt.| TYPE 'E'.
      ENDIF.

    CATCH /idxgc/cx_general.

  ENDTRY.

* Bereite Baum vor
  LOOP AT lt_sourcetab INTO ls_sourcetab
    WHERE calc_step_amount_malo IS NOT INITIAL.
    IF line_exists( lt_malo[ cf_guid = ls_sourcetab-cf_guid ] ).
      "Zu dieser Zeitscheibe schon einen Eintrag angelegt.
      CONTINUE.
    ENDIF.
    ls_malo-malo = ls_sourcetab-malo_extui.
    ls_malo-cf_guid = ls_sourcetab-cf_guid.
    ls_malo-calc_step_amount_malo = ls_sourcetab-calc_step_amount_malo.
    APPEND ls_malo TO lt_malo.
  ENDLOOP.

  SORT lt_malo.
  DELETE ADJACENT DUPLICATES FROM lt_malo.
  SORT lt_top_calc_step.
  DELETE ADJACENT DUPLICATES FROM lt_top_calc_step.
  SORT lt_sub_calc_step.
  DELETE ADJACENT DUPLICATES FROM lt_sub_calc_step.

  SORT lt_sourcetab BY malo_extui calc_step_id calc_step_id_subnum ref_other_calc_step.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
*     PERCENTAGE       = 0
      text = 'Erstelle Baumstruktur.'.
  " Erstelle den Baum
  lo_nodes = lo_tree->get_nodes( ).
  TRY.
      LOOP AT lt_malo INTO ls_malo.
        CLEAR: ls_outtab, ls_sourcetab.

        ls_sourcetab = VALUE #( lt_sourcetab[ malo_extui = ls_malo-malo
                                              validend_date = lc_enddate ] OPTIONAL ).
        CHECK ls_sourcetab IS NOT INITIAL.
        ls_outtab-calc_form_stat = ls_sourcetab-calc_form_stat.

        lo_node = lo_nodes->add_node( related_node = lc_root_key
                                      data_row     = ls_outtab
                                      relationship = cl_gui_column_tree=>relat_last_child
                                      collapsed_icon = '@A1@'
                                      expanded_icon = '@A1@' ).
        lv_value = ls_malo-malo.
        lo_node->set_text( lv_value ).
        lv_malo_key = lo_node->get_key( ).

        LOOP AT lt_timeslices INTO ls_timeslices
          WHERE malo_extui = ls_malo-malo.
          CLEAR: ls_outtab, ls_sourcetab.

          ls_sourcetab = VALUE #( lt_sourcetab[ cf_guid = ls_timeslices-cf_guid
                                                calc_step_id = ls_malo-calc_step_amount_malo ] OPTIONAL ).
          CHECK ls_sourcetab IS NOT INITIAL.
          CLEAR: ls_sourcetab-flow_direction,
                 ls_sourcetab-lossfact_ext,
                 ls_sourcetab-lossfact_line,
                 ls_sourcetab-math_operator,
                 ls_sourcetab-melo_extui,
                 ls_sourcetab-calc_form_stat.
          MOVE-CORRESPONDING ls_sourcetab TO ls_outtab.

          lo_node = lo_nodes->add_node( related_node = lv_malo_key
                                        data_row     = ls_outtab
                                        relationship = cl_gui_column_tree=>relat_last_child
                                        collapsed_icon = '@1U@'
                                        expanded_icon = '@1U@' ).
          lv_value = |Gültig ab: { ls_sourcetab-validstart_date DATE = ENVIRONMENT } |.
          lo_node->set_text( lv_value ).
          lv_timeslice_key = lo_node->get_key( ).

          LOOP AT lt_top_calc_step INTO ls_top_calc_step
            WHERE cf_guid = ls_timeslices-cf_guid
              AND calc_step_id = ls_malo-calc_step_amount_malo.
            CLEAR: ls_outtab, ls_sourcetab.

            ls_sourcetab = VALUE #( lt_sourcetab[ cf_guid = ls_top_calc_step-cf_guid
                                                  calc_step_id = ls_top_calc_step-calc_step_id
                                                  ref_other_calc_step = ls_top_calc_step-ref_other_calc_step
                                                  ] OPTIONAL ).
            CHECK ls_sourcetab IS NOT INITIAL.
            CLEAR: ls_sourcetab-flow_direction,
                   ls_sourcetab-lossfact_ext,
                   ls_sourcetab-lossfact_line,
                   ls_sourcetab-math_operator,
                   ls_sourcetab-melo_extui,
                   ls_sourcetab-calc_form_stat.

            MOVE-CORRESPONDING ls_sourcetab TO ls_outtab.
            lv_top_calc_step_key = lv_timeslice_key.

            lo_node = lo_nodes->add_node( related_node = lv_top_calc_step_key
                                    data_row     = ls_outtab
                                    relationship = cl_gui_column_tree=>relat_last_child
                                    collapsed_icon = '@5V@'
                                    expanded_icon = '@5V@' ).
            lv_value = |Hauptschritt: { ls_sourcetab-calc_step_id } |.
            lo_node->set_text( lv_value ).
            lv_top_calc_step_key = lo_node->get_key( ).

            LOOP AT lt_top_calc_step INTO ls_calc_step
              WHERE cf_guid EQ ls_timeslices-cf_guid
                AND ( calc_step_id EQ ls_top_calc_step-ref_other_calc_step OR calc_step_id EQ ls_top_calc_step-calc_step_id ).
              CLEAR: ls_outtab, ls_sourcetab.
              ls_sourcetab = VALUE #( lt_sourcetab[ cf_guid = ls_top_calc_step-cf_guid
                                                    calc_step_id = ls_calc_step-calc_step_id
                                                    calc_step_id_subnum = ls_calc_step-calc_step_id_subnum ] OPTIONAL ).
              CHECK ls_sourcetab IS NOT INITIAL.
              CLEAR: ls_sourcetab-flow_direction,
                     ls_sourcetab-lossfact_ext,
                     ls_sourcetab-lossfact_line,
                     ls_sourcetab-math_operator,
                     ls_sourcetab-melo_extui,
                     ls_sourcetab-calc_form_stat.

              MOVE-CORRESPONDING ls_sourcetab TO ls_outtab.
              lv_calc_step_key = lv_top_calc_step_key.
              lo_node = lo_nodes->add_node( related_node = lv_calc_step_key
                                  data_row     = ls_outtab
                                  relationship = cl_gui_column_tree=>relat_last_child
                                  collapsed_icon = '@0M@'
                                  expanded_icon = '@0M@' ).
              lv_value = |Schritt: { ls_sourcetab-calc_step_id } |.
              lo_node->set_text( lv_value ).
              lv_calc_step_key = lo_node->get_key( ).

              LOOP AT lt_sub_calc_step INTO ls_sub_calc_step
                WHERE cf_guid = ls_top_calc_step-cf_guid
                  AND calc_step_id EQ ls_calc_step-calc_step_id
                  .
                CLEAR: ls_outtab, ls_sourcetab.
                ls_sourcetab = VALUE #( lt_sourcetab[ cf_guid = ls_top_calc_step-cf_guid
                                                      calc_step_id = ls_sub_calc_step-calc_step_id
                                                      calc_step_id_subnum = ls_sub_calc_step-calc_step_id_subnum ] OPTIONAL ).
                CHECK ls_sourcetab IS NOT INITIAL.
                MOVE-CORRESPONDING ls_sourcetab TO ls_outtab.
                lv_sub_calc_step_key = lv_calc_step_key.
                lo_node = lo_nodes->add_node( related_node = lv_sub_calc_step_key
                                    data_row     = ls_outtab
                                    relationship = cl_gui_column_tree=>relat_last_child
                                    collapsed_icon = '@0M@'
                                    expanded_icon = '@0M@' ).
                lv_value = |Unterschritt: { ls_sourcetab-calc_step_id }.{ ls_sourcetab-calc_step_id_subnum } |.
                lo_node->set_text( lv_value ).
                lv_sub_calc_step_key = lo_node->get_key( ).

              ENDLOOP. " Unterschritte
            ENDLOOP. " Zwischenschritte
          ENDLOOP. " Hauptschritt
        ENDLOOP. " Zeitscheiben
      ENDLOOP. " Malo
    CATCH cx_salv_msg.
  ENDTRY.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
*     PERCENTAGE       = 0
      text = 'Gebe Struktur aus.'.
  " Äußerlichkeiten der Ausgabe
  lo_settings = lo_tree->get_tree_settings( ).
  "lo_settings->set_hierarchy_header( text-hd1 ).
  "lo_settings->set_hierarchy_tooltip( text-ht1 ).
  lo_settings->set_hierarchy_size( 60 ). " Breite der Hierarchiespalte

*... set the columns technical
  lo_columns = lo_tree->get_columns( ).
  "  lo_columns->set_optimize( abap_true ).

** those columns which should not be seen by the user at all are set technical
  TRY.
      lo_column = lo_columns->get_column( to_upper( 'malo' ) ).
      lo_column->set_technical( if_salv_c_bool_sap=>true ).

      lo_column = lo_columns->get_column( to_upper( 'calc_step_id' ) ).
      lo_column->set_technical( if_salv_c_bool_sap=>true ).

      lo_column = lo_columns->get_column( to_upper( 'calc_step_id_subnum' ) ).
      lo_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lo_column = lo_columns->get_column( to_upper( 'calc_form_stat' ) ).
      lo_column->set_optimized( if_salv_c_bool_sap=>false ).
      lo_column->set_output_length( 12 ).
      lo_column = lo_columns->get_column( to_upper( 'math_operator' ) ).
      lo_column->set_optimized( if_salv_c_bool_sap=>false ).
      lo_column->set_output_length( 12 ).
      lo_column = lo_columns->get_column( to_upper( 'flow_direction' ) ).
      lo_column->set_optimized( if_salv_c_bool_sap=>false ).
      lo_column->set_output_length( 12 ).

      lo_column = lo_columns->get_column( to_upper( 'lossfact_ext' ) ).
      lo_column->set_short_text( 'Verl.Trafo' ).
      lo_column->set_medium_text( 'Verlust Trafo' ).
      lo_column->set_long_text( 'Verlustfaktor Trafo' ).
      lo_column->set_optimized( if_salv_c_bool_sap=>false ).
      lo_column->set_output_length( 16 ).

      lo_column = lo_columns->get_column( to_upper( 'lossfact_line' ) ).
      lo_column->set_short_text( 'Verl.Leit.' ).
      lo_column->set_medium_text( 'Verlust Leitung' ).
      lo_column->set_long_text( 'Verlustfaktor Leitung' ).
      lo_column->set_optimized( if_salv_c_bool_sap=>false ).
      lo_column->set_output_length( 12 ).

      lo_column = lo_columns->get_column( to_upper( 'melo_extui' ) ).
      lo_column->set_output_length( 50 ).

      lo_column = lo_columns->get_column( to_upper( 'validstart_date' ) ).
      lo_column->set_output_length( 10 ).

      lo_column = lo_columns->get_column( to_upper( 'validstart_time' ) ).
      lo_column->set_output_length( 8 ).


    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

*... §4 display the table
  lo_tree->display( ).
