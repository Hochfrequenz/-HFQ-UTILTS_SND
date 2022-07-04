*&---------------------------------------------------------------------*
*& Report /HFQ/RP_GEN_CALC_FORM
*&---------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Dieser Report dient zum Erstellen eines Berechnungsformel-Eintrags
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-09-22  Hochfrequenz    angelegt
REPORT /hfq/rp_gen_calc_form.

PARAMETERS: p_malo   TYPE ext_ui,
            p_datfr  TYPE /idxgc/de_validstart_date DEFAULT sy-datum,
            p_datto  TYPE /idxgc/de_date_to DEFAULT '99991231',
            p_status TYPE /hfq/de_calc_form_stat DEFAULT /hfq/cl_dp_utilts=>gc_calcform_stat_z33,
            p_supdir TYPE /idxgc/de_supply_direct DEFAULT 'Z07'.

DATA: lt_outtab TYPE TABLE OF /hfq/calc_form,
      ls_outtab TYPE /hfq/calc_form,
      lv_subnum TYPE /idxgl/de_calc_step_id_subnum,
      ls_return TYPE bapiret2,
      lt_return TYPE bapiret2_t.

DATA: lt_sval   TYPE TABLE OF sval,
      lv_return TYPE char01,
      lv_answer TYPE char01.

DATA: lo_gen_calc_form TYPE REF TO /hfq/cl_gen_calc_form,
      ls_calc_form_h   TYPE /hfq/calc_form_h.

CREATE OBJECT lo_gen_calc_form.

CALL METHOD lo_gen_calc_form->gen_malo_timeslice(
  EXPORTING
    iv_malo_ext_ui   = p_malo     " MaLo Zählpunktbezeichnung
    iv_startdate     = p_datfr    " Gültig-ab-Datum
    iv_starttime     = '000000'   " Gültige Startzeit
    iv_enddate       = p_datto    " Gültig-bis-Datum
    iv_endtime       = '235959'   " Gültige Endezeit
    iv_status        = p_status   " Kennzeichen für Bilaterale Klärung/Übermittlung
    iv_supply_direct = p_supdir   " Lieferrichtung
  IMPORTING
    es_calc_form_h   = ls_calc_form_h " Kopfdaten: Berechnungsformel: MaLo und Zeitscheiben
  CHANGING
    ct_return        = lt_return      " Returntabelle
  EXCEPTIONS
    error            = 1              " allg. Fehler
    OTHERS           = 2
).
IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

WHILE lv_return = ''.
  CLEAR: lt_sval.

  PERFORM generate_sval CHANGING lt_sval.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      no_value_check  = 'X' "space            " Schaltet Prüfungen des jeweiligen Datentyps aus
      popup_title     = 'Berechnungsschritte' " Text der Titelzeile
    IMPORTING
      returncode      = lv_return                " Antwort des Anwenders
    TABLES
      fields          = lt_sval                 " Tabellenfelder, Werte und Attribute
    EXCEPTIONS
      error_in_fields = 1                " FIELDS wurde fehlerhaft übergeben
      OTHERS          = 2.
  IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF lv_return = ''.
    "Unterschritt-Nummer setzen:
    LOOP AT lt_outtab ASSIGNING FIELD-SYMBOL(<fs_outtab>)
      WHERE calc_step_id = lt_sval[ fieldname = 'CALC_STEP_ID' ]-value.
      lv_subnum = lv_subnum + 1.

      IF <fs_outtab>-calc_step_id_subnum IS INITIAL.
        <fs_outtab>-calc_step_id_subnum = 1.
      ENDIF.
    ENDLOOP.

    PERFORM fill_outtab USING lt_sval
                              ls_calc_form_h
                              lv_subnum
                        CHANGING ls_outtab.

    APPEND ls_outtab TO lt_outtab.
    SORT lt_outtab.

    CALL METHOD lo_gen_calc_form->add_calc_form_entry
      EXPORTING
        iv_calc_step_id        = ls_outtab-calc_step_id
        iv_calc_step_id_subnum = ls_outtab-calc_step_id_subnum
        iv_ref_other_calc_step = ls_outtab-ref_other_calc_step
        iv_melo_extui          = ls_outtab-melo_extui
        iv_math_operator       = ls_outtab-math_operator
        iv_flow_direction      = ls_outtab-flow_direction
        iv_lossfact_ext        = ls_outtab-lossfact_ext
        iv_lossfact_line       = ls_outtab-lossfact_line
      CHANGING
        ct_return              = lt_return
      EXCEPTIONS
        error                  = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      ls_return = lt_return[ type = 'E' ].
      MESSAGE ID ls_return-id TYPE 'E' NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'UA_SM1_POPUP_WITH_ALV_GRID'
    EXPORTING
      i_structure_name = '/HFQ/CALC_FORM'
      i_title          = 'Berechnungsschritte'
    TABLES
      it_outtab        = lt_outtab.
ENDWHILE.

IF lt_outtab IS NOT INITIAL
   OR ls_calc_form_h-calc_form_stat = /hfq/cl_dp_utilts=>gc_calcform_stat_z34.

  IF lt_outtab IS NOT INITIAL
     AND ls_calc_form_h-calc_step_amount_malo IS INITIAL.
    DATA: lv_selected_value TYPE string.
    CALL FUNCTION 'POPUP_GET_STRING'
      EXPORTING
        label = 'MaLo-Berechnungsschritt Nummer'
      IMPORTING
        value = lv_selected_value.

    IF line_exists( lt_outtab[ calc_step_id = lv_selected_value ] ).
      CALL METHOD lo_gen_calc_form->set_main_calc_step
        EXPORTING
          iv_calc_step_malo = lv_selected_value " Berechnungsschritt für die Menge der Marktlokation
        CHANGING
          ct_return         = lt_return
        EXCEPTIONS
          error             = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        ls_return = lt_return[ type = 'E' ].
        MESSAGE ID ls_return-id TYPE 'E' NUMBER ls_return-number
          WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = 'Neuen Formeleintrag erzeugen'
      text_question  = 'Formel speichern?'
      text_button_1  = 'Ja'
      text_button_2  = 'Nein'
    IMPORTING
      answer         = lv_answer                 " Rückgabewerte: '1', '2', 'A'
    EXCEPTIONS
      text_not_found = 1                " Diagnosetext wurde nicht gefunden
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF lv_answer = '1'.
    CALL METHOD lo_gen_calc_form->save
      CHANGING
        ct_return = lt_return.                 " Returntabelle
  ENDIF.

ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GENERATE_SVAL
*&---------------------------------------------------------------------*
*       Erstellt die Strukturtabelle
*----------------------------------------------------------------------*
*      -->P_LT_SVAL  text
*----------------------------------------------------------------------*
FORM generate_sval CHANGING p_lt_sval LIKE lt_sval.
  p_lt_sval = VALUE #( ( tabname = '/HFQ/CALC_FORM' fieldname = 'CALC_STEP_ID' value = '' field_attr = '' field_obl = '' comp_code = '' fieldtext = 'Schritt' comp_tab = '' comp_field  ='' novaluehlp = 'X' )
                       ( tabname = '/HFQ/CALC_FORM' fieldname = 'REF_OTHER_CALC_STEP' value = '' field_attr = '' field_obl = '' comp_code = '' fieldtext = 'Referenzschritt' comp_tab = '' comp_field  ='' novaluehlp = 'X' )
                       ( tabname = '/HFQ/CALC_FORM' fieldname = 'MELO_EXTUI' value = '' field_attr = '' field_obl = '' comp_code = '' fieldtext = 'MeLo' comp_tab = '' comp_field  ='' novaluehlp = 'X' )
                       ( tabname = '/HFQ/CALC_FORM' fieldname = 'MATH_OPERATOR' value = '' field_attr = '' field_obl = 'X' comp_code = '' fieldtext = 'Operand' comp_tab = '' comp_field  ='' novaluehlp = 'X' )
                       ( tabname = '/HFQ/CALC_FORM' fieldname = 'FLOW_DIRECTION' value = '' field_attr = '' field_obl = 'X' comp_code = '' fieldtext = 'Flussrichtung' comp_tab = '' comp_field  ='' novaluehlp = 'X' )
                       ( tabname = '/HFQ/CALC_FORM' fieldname = 'LOSSFACT_EXT' value = '' field_attr = '' field_obl = '' comp_code = '' fieldtext = 'Verlust: Trafo' comp_tab = '' comp_field  ='' novaluehlp = '' )
                       ( tabname = '/HFQ/CALC_FORM' fieldname = 'LOSSFACT_LINE' value = '' field_attr = '' field_obl = '' comp_code = '' fieldtext = 'Verlust' comp_tab = '' comp_field  ='' novaluehlp = '' ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_OUTTAB
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*      -->LT_SVAL  text
*      -->LS_CALC_FORM_H  text
*      <--P_LS_OUTTAB  text
*----------------------------------------------------------------------*
FORM fill_outtab  USING    lt_sval LIKE lt_sval
                           ls_calc_form_h TYPE /hfq/calc_form_h
                           lv_subnum TYPE /idxgl/de_calc_step_id_subnum
                  CHANGING p_ls_outtab TYPE /hfq/calc_form.

  p_ls_outtab = VALUE #(  cf_guid = ls_calc_form_h-cf_guid
                          calc_step_id = lt_sval[ fieldname = 'CALC_STEP_ID' ]-value
                          calc_step_id_subnum = lv_subnum
                          ref_other_calc_step = lt_sval[ fieldname = 'REF_OTHER_CALC_STEP' ]-value
                          melo_extui = lt_sval[ fieldname = 'MELO_EXTUI' ]-value
                          math_operator = lt_sval[ fieldname = 'MATH_OPERATOR' ]-value
                          flow_direction = lt_sval[ fieldname = 'FLOW_DIRECTION' ]-value
                          lossfact_ext = lt_sval[ fieldname = 'LOSSFACT_EXT' ]-value
                          lossfact_line = lt_sval[ fieldname = 'LOSSFACT_LINE' ]-value
                         ).

ENDFORM.
