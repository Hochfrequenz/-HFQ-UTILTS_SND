class /HFQ/CL_GEN_CALC_FORM definition
  public
  create public .

public section.

  data GT_CALC_FORM type /HFQ/T_CALC_FORM .

  methods GEN_MALO_TIMESLICE
    importing
      !IV_MALO_EXT_UI type /HFQ/DE_EXT_UI_MALO
      !IV_STARTDATE type /IDXGC/DE_VALIDSTART_DATE default SY-DATUM
      !IV_STARTTIME type /IDXGC/DE_VALIDSTART_TIME default '000000'
      !IV_ENDDATE type /IDXGC/DE_DATE_TO default '99991231'
      !IV_ENDTIME type /IDXGC/DE_TIMETO default '235959'
      !IV_STATUS type /HFQ/DE_CALC_FORM_STAT default 'Z33'
      !IV_SUPPLY_DIRECT type /IDXGC/DE_SUPPLY_DIRECT default 'Z07'
    exporting
      !ES_CALC_FORM_H type /HFQ/CALC_FORM_H
    changing
      !CT_RETURN type BAPIRET2_T
    exceptions
      ERROR .
  methods CONSTRUCTOR .
  methods ADD_CALC_FORM_ENTRY
    importing
      !IV_CALC_STEP_ID type /HFQ/CALC_FORM-CALC_STEP_ID optional
      !IV_CALC_STEP_ID_SUBNUM type /HFQ/CALC_FORM-CALC_STEP_ID_SUBNUM optional
      !IV_REF_OTHER_CALC_STEP type /HFQ/CALC_FORM-REF_OTHER_CALC_STEP optional
      !IV_MELO_EXTUI type /HFQ/CALC_FORM-MELO_EXTUI optional
      !IV_MATH_OPERATOR type /HFQ/CALC_FORM-MATH_OPERATOR
      !IV_FLOW_DIRECTION type /HFQ/CALC_FORM-FLOW_DIRECTION optional
      !IV_LOSSFACT_EXT type /HFQ/CALC_FORM-LOSSFACT_EXT optional
      !IV_LOSSFACT_LINE type /HFQ/CALC_FORM-LOSSFACT_LINE optional
    changing
      !CT_RETURN type BAPIRET2_T
    exceptions
      ERROR .
  methods SAVE
    changing
      !CT_RETURN type BAPIRET2_T
    exceptions
      ERROR_HEAD
      ERROR_LINES .
  methods SET_MAIN_CALC_STEP
    importing
      !IV_CALC_STEP_MALO type STRING
    changing
      !CT_RETURN type BAPIRET2_T
    exceptions
      ERROR .
protected section.

  constants GC_TYP_STATUS type BAPIRETURN-TYPE value 'S' ##NO_TEXT.
  constants GC_TYP_ERROR type BAPIRETURN-TYPE value 'E' ##NO_TEXT.
  constants GC_TYP_WARNING type BAPIRETURN-TYPE value 'W' ##NO_TEXT.
  constants GC_CALCFORM_MSGID like SY-MSGID value '/HFQ/CALC_FORM' ##NO_TEXT.
  data GS_CALC_FORM_H_NEW type /HFQ/CALC_FORM_H .
  constants GC_DATE_FINITE type SYDATUM value '19000101' ##NO_TEXT.
  constants GC_TIME_MIN type SYTIME value '000000' ##NO_TEXT.
  constants GC_DATE_INFINITE type SYDATUM value '99991231' ##NO_TEXT.
  constants GC_TIME_MAX type SYTIME value '235959' ##NO_TEXT.

  methods GEN_CALC_FORM_ENTRY
    changing
      !CT_RETURN type BAPIRET2_T
    exceptions
      ERROR .
  methods CHECK_EXISTING_TIMESLICES
    exporting
      !ET_CALC_FORM_H_EXIST type /HFQ/T_CALC_FORM_H
    changing
      !CT_RETURN type BAPIRET2_T
      !CS_CALC_FORM_H_NEW type /HFQ/CALC_FORM_H .
  methods CHECK_CALC_FORM_ENTRY
    importing
      !IS_CALC_FORM type /HFQ/CALC_FORM
      !IKZ_COMPLETE type KENNZX default ABAP_FALSE
    changing
      !CT_RETURN type BAPIRET2_T
    exceptions
      ERROR .
private section.
ENDCLASS.



CLASS /HFQ/CL_GEN_CALC_FORM IMPLEMENTATION.


  METHOD add_calc_form_entry.
    DATA: ls_calc_form_new TYPE /hfq/calc_form,
          ls_return        TYPE bapiret2.

    ls_calc_form_new = VALUE #( cf_guid               = gs_calc_form_h_new-cf_guid
                                calc_step_id          = iv_calc_step_id
                                calc_step_id_subnum   = iv_calc_step_id_subnum
                                ref_other_calc_step   = iv_ref_other_calc_step
                                melo_extui            = iv_melo_extui
                                math_operator         = iv_math_operator
                                flow_direction        = iv_flow_direction
                                lossfact_ext          = iv_lossfact_ext
                                lossfact_line         = iv_lossfact_line
                                ).


*--------------------------------------------------------------------*
*     Prüfen:
    CALL METHOD check_calc_form_entry
      EXPORTING
        is_calc_form = ls_calc_form_new
      CHANGING
        ct_return    = ct_return
      EXCEPTIONS
        error        = 1                " Fehler
        OTHERS       = 2.
    IF sy-subrc <> 0.
      "Fehler: Formeleintrag fehlerhaft und wird nicht übernommen.
      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '047'
*                           message_v1 = is_calc_form-calc_step_id
                          ).
      APPEND ls_return TO ct_return.
      RAISE error.
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
*    Prüfungen abgeschlossen, übernehmen:
    APPEND ls_calc_form_new TO gt_calc_form.

  ENDMETHOD.


  METHOD check_calc_form_entry.
    DATA: ls_return type bapiret2.

    SELECT SINGLE * FROM  /hfq/calc_form_h
        WHERE cf_guid = @is_calc_form-cf_guid
        INTO @DATA(ls_calc_form_h).
    IF sy-subrc NE 0
       AND gs_calc_form_h_new-cf_guid NE is_calc_form-cf_guid.
      "Fehler: Zugehörige Zeitscheibe zur Formel nicht vorhanden.
      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '040' ).
      APPEND ls_return TO ct_return.
      RAISE error.
    ENDIF.
*--------------------------------------------------------------------*
*     Schritte prüfen
*--------------------------------------------------------------------*

    IF is_calc_form-calc_step_id IS INITIAL AND is_calc_form-ref_other_calc_step IS INITIAL.
      "Fehler: Schritt nicht eingeordnet, Schrittnummer oder Referenz fehlt.
      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '041' ).
      APPEND ls_return TO ct_return.
      RAISE error.
    ENDIF.

    IF ikz_complete = abap_true.
      "Schrittdaten vollständig, also Beziehungen prüfen:
      IF is_calc_form-ref_other_calc_step IS NOT INITIAL.
        IF NOT line_exists( gt_calc_form[ calc_step_id = is_calc_form-ref_other_calc_step ] ).
          "Fehler: Der Referenzschritt &1 aus Schritt &2 fehlt!
          ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '042'
                               message_v1 = is_calc_form-ref_other_calc_step
                               message_v2 = is_calc_form-calc_step_id    ).
          APPEND ls_return TO ct_return.
          RAISE error.
        ENDIF.
      ENDIF.

      "Prüfung unklar: Unterschritte ab dem ersten Eintrag oder erst ab dem zweitern?
*      IF is_calc_form-calc_step_id_subnum IS NOT INITIAL.
*        IF NOT line_exists( it_calc_form[ calc_step_id = is_calc_form-calc_step_id calc_step_id_subnum = ''] ).
*          "Fehler: Der Hauptschritt &1 für den Unterschritt &2 fehlt.
*          ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '043'
*                                message_v1 = is_calc_form-calc_step_id
*                                message_v2 = is_calc_form-calc_step_id_subnum    ).
*          APPEND ls_return TO ct_return.
*          RAISE error.
*        ENDIF.
*      ENDIF.

    ENDIF.


*--------------------------------------------------------------------*
*     Sonstiges:
*--------------------------------------------------------------------*
    IF is_calc_form-math_operator IS INITIAL.
      "Fehler: Der mathematische Operator in Schritt &1 muss gefüllt sein!
      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '044'
                           message_v1 = is_calc_form-calc_step_id ).
          APPEND ls_return TO ct_return.
          RAISE error.
    ENDIF.

*    IF is_calc_form-supply_direct IS INITIAL.
*      "Fehler.: Die Lieferrichtung in Schritt &1 muss gefüllt sein!
*      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '045'
*                           message_v1 = is_calc_form-calc_step_id ).
*          APPEND ls_return TO ct_return.
*          RAISE error.
*    ENDIF.

    IF is_calc_form-melo_extui IS NOT INITIAL.
*        "Prüfen, ob melo korrekt und bekannt.
      SELECT SINGLE int_ui from euitrans WHERE ext_ui = @is_calc_form-melo_extui
        INTO @DATA(lv_melo).
        IF sy-subrc NE 0.
          "Fehler: Die übergebene MeLo &1 ist im System nicht bekannt.
          ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '046'
                           message_v1 = is_calc_form-melo_extui ).
          APPEND ls_return TO ct_return.
          RAISE error.
        ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_existing_timeslices.
    DATA: lt_calc_form_h TYPE TABLE OF /hfq/calc_form_h.
    DATA: ls_return        TYPE bapiret2,
          lv_kz_abgrenzung TYPE kennzx.

    lv_kz_abgrenzung = abap_false.


    SELECT * FROM /hfq/calc_form_h INTO TABLE lt_calc_form_h
      WHERE malo_extui = cs_calc_form_h_new-malo_extui
        AND validstart_date LE cs_calc_form_h_new-validstart_date
        AND validend_date GE cs_calc_form_h_new-validend_date.
    IF sy-subrc = 0.
      SORT lt_calc_form_h.
      DELETE ADJACENT DUPLICATES FROM lt_calc_form_h.
      "Alle bestehenden Einträge außerhalb der Zeitscheibe entfernen:
      DELETE lt_calc_form_h WHERE validstart_date EQ cs_calc_form_h_new-validend_date AND validstart_time GT cs_calc_form_h_new-validend_time.
      DELETE lt_calc_form_h WHERE validend_date EQ cs_calc_form_h_new-validstart_date AND validend_time LT cs_calc_form_h_new-validstart_time.

      CHECK lines( lt_calc_form_h ) LT 2.
    ELSE.
      "Zu Malo &1 ist im Zeitraum &2 - &3 keine Zeitscheibe vorhanden.
      ls_return = VALUE #( type = gc_typ_status id = gc_calcform_msgid number = '020'
                           message_v1 = cs_calc_form_h_new-malo_extui
                           message_v2 = |{ cs_calc_form_h_new-validstart_date DATE = ENVIRONMENT } { cs_calc_form_h_new-validstart_time TIME = ENVIRONMENT }|
                           message_v3 = |{ cs_calc_form_h_new-validend_date DATE = ENVIRONMENT } { cs_calc_form_h_new-validend_time TIME = ENVIRONMENT }| ).
      APPEND ls_return TO ct_return.
      RETURN.
    ENDIF.

    " Prüfen:
    IF lt_calc_form_h IS NOT INITIAL.
      "Es sind Einträge zu Malo &1 im Zeitraum &2 - &3 vorhanden.
      ls_return = VALUE #( type = gc_typ_status id = gc_calcform_msgid number = '021'
                           message_v1 = cs_calc_form_h_new-malo_extui
                           message_v2 = |{ cs_calc_form_h_new-validstart_date DATE = ENVIRONMENT } { cs_calc_form_h_new-validstart_time TIME = ENVIRONMENT }|
                           message_v3 = |{ cs_calc_form_h_new-validend_date DATE = ENVIRONMENT } { cs_calc_form_h_new-validend_time TIME = ENVIRONMENT }| ).
      APPEND ls_return TO ct_return.

      LOOP AT lt_calc_form_h ASSIGNING FIELD-SYMBOL(<fs_calc_form_h>).
        "Existierender Eintrag mit Zeitraum &1 - &2.
        ls_return = VALUE #( type = gc_typ_status id = gc_calcform_msgid number = '022'
                             message_v1 = |{ <fs_calc_form_h>-validstart_date DATE = ENVIRONMENT } { <fs_calc_form_h>-validstart_time TIME = ENVIRONMENT }|
                             message_v2 = |{ <fs_calc_form_h>-validend_date DATE = ENVIRONMENT } { <fs_calc_form_h>-validend_time TIME = ENVIRONMENT }| ).
        APPEND ls_return TO ct_return.

        IF     <fs_calc_form_h>-validstart_date GT cs_calc_form_h_new-validstart_date
          OR ( <fs_calc_form_h>-validstart_date EQ cs_calc_form_h_new-validstart_date AND <fs_calc_form_h>-validstart_time GT cs_calc_form_h_new-validstart_time ).
          "Fall 1: Zeitscheibe vorhanden, die später startet.

          IF     <fs_calc_form_h>-validend_date LT cs_calc_form_h_new-validend_date
            OR ( <fs_calc_form_h>-validend_date EQ cs_calc_form_h_new-validend_date AND <fs_calc_form_h>-validend_time LT cs_calc_form_h_new-validend_time ).
            "Fall 1.1: WARNUNG: Existierende Zeitscheibe liegt innerhalb der geplanten Neuen!
            ls_return = VALUE #( type = gc_typ_warning id = gc_calcform_msgid number = '023' ).
            APPEND ls_return TO ct_return.
            "Aussteuern mit neuem Start?

          ELSE.
            "Fall 1.2: WARNUNG: Existierende Zeitscheibe läuft länger als die geplante Neue.
            ls_return = VALUE #( type = gc_typ_warning id = gc_calcform_msgid number = '024' ).
            APPEND ls_return TO ct_return.

            "Drum herum anlegen (in beide Richtungen beschränken) oder nur Fehler werfen?
*
          ENDIF.
        ELSE.
          "Fall 2: Existierende Zeitscheibe startet vor der geplanten Neuen.
          ls_return = VALUE #( type = gc_typ_status id = gc_calcform_msgid number = '025' ).
          APPEND ls_return TO ct_return.

          IF <fs_calc_form_h>-validend_date = gc_date_infinite.
            "Fall 2.1: Die existierende Zeitscheibe ist nicht abgegrenzt. Also mit neuem Startdatum abgrenzen:
            IF lv_kz_abgrenzung = abap_true.
              "Fehler: schon einmal eine Zeitscheibe abgegrenzt. Datensätze zur Malo unsauber!
            ENDIF.
            "Existierende Zeitscheibe ist nicht abgegrenzt.
            ls_return = VALUE #( type = gc_typ_status id = gc_calcform_msgid number = '026' ).
            APPEND ls_return TO ct_return.

            <fs_calc_form_h>-validend_time = cs_calc_form_h_new-validstart_time - 1.
            IF cs_calc_form_h_new-validstart_time = gc_time_min.
              <fs_calc_form_h>-validend_date = cs_calc_form_h_new-validstart_date - 1.
            ELSE.
              <fs_calc_form_h>-validend_date = cs_calc_form_h_new-validstart_date.
            ENDIF.
            lv_kz_abgrenzung = abap_true.
            "Abgrenzung der existierende Zeitscheibe zum &1.
            ls_return = VALUE #( type = gc_typ_status id = gc_calcform_msgid number = '027'
                                  message_v1 = |{ <fs_calc_form_h>-validend_date DATE = ENVIRONMENT } { <fs_calc_form_h>-validend_time TIME = ENVIRONMENT }| ).
            APPEND ls_return TO ct_return.

          ELSEIF  <fs_calc_form_h>-validend_date LT cs_calc_form_h_new-validend_date
             OR ( <fs_calc_form_h>-validend_date EQ cs_calc_form_h_new-validend_date AND <fs_calc_form_h>-validend_time LT cs_calc_form_h_new-validend_time ).
            "Fall 2.2: Die existierende Zeitscheibe endet vor der geplanten.
            ls_return = VALUE #( type = gc_typ_warning id = gc_calcform_msgid number = '028'
                                  message_v1 = |{ <fs_calc_form_h>-validend_date DATE = ENVIRONMENT } { <fs_calc_form_h>-validend_time TIME = ENVIRONMENT }| ).
            APPEND ls_return TO ct_return.

            "Neues Startdatum verschieben?

          ELSE.
            "Fall 2.3: Die geplante Zeitscheibe liegt komplett innerhalb der Existierenden.
            ls_return = VALUE #( type = gc_typ_warning id = gc_calcform_msgid number = '029'
                                  message_v1 = |{ <fs_calc_form_h>-validend_date DATE = ENVIRONMENT } { <fs_calc_form_h>-validend_time TIME = ENVIRONMENT }| ).
            APPEND ls_return TO ct_return.

            "Drum herum anlegen (in beide Richtungen beschränken) oder nur Fehler werfen?
*
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

    et_calc_form_h_exist = lt_calc_form_h.

  ENDMETHOD.


  METHOD constructor.


  ENDMETHOD.


  METHOD gen_calc_form_entry.

    DATA: ls_calc_form TYPE /hfq/calc_form,
          ls_return    TYPE bapiret2.
*--------------------------------------------------------------------*
*   Prüfungen auf Datenkonsistenz
*--------------------------------------------------------------------*
    LOOP AT gt_calc_form INTO ls_calc_form.

      CALL METHOD check_calc_form_entry
        EXPORTING
          is_calc_form = ls_calc_form
          ikz_complete = abap_true
        CHANGING
          ct_return    = ct_return
        EXCEPTIONS
          error        = 1                " Fehler
          OTHERS       = 2.
      IF sy-subrc <> 0.
        "Fehler: Formeleintrag fehlerhaft und wird nicht übernommen.
        ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '047'
*                           message_v1 = is_calc_form-calc_step_id
                            ).
        APPEND ls_return TO ct_return.
        RAISE error.
        RETURN.
      ENDIF.

    ENDLOOP.

*--------------------------------------------------------------------*
*   Prüfungen abgeschlossen. Übernehmen:
*--------------------------------------------------------------------*
    MODIFY /hfq/calc_form FROM TABLE gt_calc_form.
    IF sy-subrc NE 0.
      "Fehler: Import der Berechnungsschritte fehlgeschlagen.
      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '051' ).
      APPEND ls_return TO ct_return.
      RAISE error.
    ELSE.
      "Berechnungsschritte erfolgreich gespeichert.
      ls_return = VALUE #( type = gc_typ_status id = gc_calcform_msgid number = '052' ).
      APPEND ls_return TO ct_return.
    ENDIF.

  ENDMETHOD.


  METHOD gen_malo_timeslice.

    DATA: lt_calc_form_h       TYPE TABLE OF /hfq/calc_form_h,
          lt_calc_form_h_exist TYPE TABLE OF /hfq/calc_form_h,
          ls_calc_form_h       TYPE /hfq/calc_form_h.

    DATA: lv_startdate TYPE /idxgc/de_validstart_date,
          lv_starttime TYPE /idxgc/de_validstart_time,
          lv_enddate   TYPE /idxgc/de_date_to,
          lv_endtime   TYPE /idxgc/de_timeto.

    DATA: lv_kz_abgrenzung TYPE kennzx.
    DATA: ls_return TYPE bapiret2.

    FIELD-SYMBOLS: <fs_calc_form_h> TYPE /hfq/calc_form_h.

    "initiale Befüllung:
    lv_startdate = iv_startdate.
    lv_starttime = iv_starttime.
    lv_enddate = iv_enddate.
    lv_endtime = iv_endtime.
*--------------------------------------------------------------------*
* Existierende Einträge zur Zeitscheibe ermitteln:
*--------------------------------------------------------------------*

    " Neuen Eintrag schreiben
    CLEAR: ls_calc_form_h.
    ls_calc_form_h-malo_extui = iv_malo_ext_ui.
    ls_calc_form_h-validstart_date = lv_startdate.
    ls_calc_form_h-validstart_time = lv_starttime.
    ls_calc_form_h-validend_date = lv_enddate.
    ls_calc_form_h-validend_time = lv_endtime.
    ls_calc_form_h-calc_form_stat = iv_status.
    ls_calc_form_h-supply_direct = iv_supply_direct.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = ls_calc_form_h-cf_guid.

    CALL METHOD check_existing_timeslices
      IMPORTING
        et_calc_form_h_exist = lt_calc_form_h_exist
      CHANGING
        cs_calc_form_h_new   = ls_calc_form_h
        ct_return            = ct_return.

*    APPEND ls_calc_form_h TO lt_calc_form_h.
*    MODIFY /hfq/calc_form_h FROM TABLE lt_calc_form_h.
    IF ls_calc_form_h IS NOT INITIAL.
      gs_calc_form_h_new = ls_calc_form_h.
      es_calc_form_h = ls_calc_form_h.

    ELSE.
      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '011' ).
      APPEND ls_return TO ct_return.
    ENDIF.

  ENDMETHOD.


  METHOD save.

    DATA: lt_calc_form_h TYPE TABLE OF /hfq/calc_form_h,
          ls_return      TYPE bapiret2.

    " erneut ausführen, damit die Zeitscheiben angepasst werden können:
    CALL METHOD check_existing_timeslices
      IMPORTING
        et_calc_form_h_exist = lt_calc_form_h
      CHANGING
        cs_calc_form_h_new   = gs_calc_form_h_new
        ct_return            = ct_return.

    "Prüfung auf vorhandene Schrittreferenz:
    IF gs_calc_form_h_new-calc_form_stat = /hfq/cl_dp_utilts=>gc_calcform_stat_z33.
      CHECK gs_calc_form_h_new-calc_step_amount_malo IS NOT INITIAL.
    ENDIF.

    APPEND gs_calc_form_h_new TO lt_calc_form_h.
    MODIFY /hfq/calc_form_h FROM TABLE lt_calc_form_h.
    IF sy-subrc NE 0.
      "Fehler: Speichern der Zeitscheibe fehlgeschlagen.
      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '050' ).
      APPEND ls_return TO ct_return.
      RAISE error_head.
    ENDIF.

    CALL METHOD gen_calc_form_entry
      CHANGING
        ct_return = ct_return
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
*    "Fehler: Speichern der Berechnungsschritte fehlgeschlagen.
      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '051' ).
      APPEND ls_return TO ct_return.
      RAISE error_lines.
    ENDIF.

  ENDMETHOD.


  METHOD set_main_calc_step.

    DATA: ls_return       TYPE bapiret2.

    "Prüfungen:
    IF gs_calc_form_h_new-calc_form_stat = /hfq/cl_dp_utilts=>gc_calcform_stat_z34.
      "Zeitscheibe enthält Bilaterale Absprache, kein MaLo-Summenschritt nötig
      ls_return = VALUE #( type = gc_typ_status id = gc_calcform_msgid number = '048' ).
      APPEND ls_return TO ct_return.
      RETURN.
    ENDIF.

    IF NOT line_exists( gt_calc_form[ calc_step_id = iv_calc_step_malo ] ).
      "FEHLER: Referenz &1 als MaLo-Summenschritt fehlt in der Schrittliste.
      ls_return = VALUE #( type = gc_typ_error id = gc_calcform_msgid number = '049'
                           message_v1 = iv_calc_step_malo ).
      APPEND ls_return TO ct_return.
      RAISE error.
    ENDIF.

    "Alles korrekt, also übertragen:
    gs_calc_form_h_new-calc_step_amount_malo = iv_calc_step_malo.

  ENDMETHOD.
ENDCLASS.
