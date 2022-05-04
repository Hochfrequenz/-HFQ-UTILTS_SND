*&---------------------------------------------------------------------*
*& Report /HFQ/RP_UTILTS_SND
*&---------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Dieser Report dient zum Auslösen des UTILTS-Prozesses im
*&            System des Netzbetreibers. Nach ersten Prüfungen wird
*&            ein PE-Prozess angestoßen.
*&            Über die BAdI-Implementierung des /HFQ/BADI_UTILTS_SND
*&            kann die Datenquelle für die Konstrukte angepasst werden.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-06-20  Hochfrequenz    angelegt
*&---------------------------------------------------------------------*
REPORT /hfq/rp_utilts_snd.

DATA: lv_single_proc     TYPE kennzx,
      lt_malo_selected   TYPE isu_ranges_tab,
      ls_malo_selected   TYPE isu_ranges,
      lt_malo_extui      TYPE TABLE OF ext_ui,
      lt_melo_extui      TYPE TABLE OF ext_ui,
      lv_malo            TYPE ext_ui,
      lv_melo            TYPE ext_ui,
      lv_int_ui          TYPE int_ui,
      lt_calc_form       TYPE TABLE OF /hfq/calc_form,
      lt_calc_form_h     TYPE TABLE OF /hfq/calc_form_h,
      lt_calc_form_tmp   TYPE TABLE OF /hfq/calc_form,
      lt_calc_form_h_tmp TYPE TABLE OF /hfq/calc_form_h,
      lv_keydate         TYPE datum,
      lv_keytime         TYPE /idxgc/de_validstart_time,
      lt_all_services    TYPE iallservices,
      ls_all_services    TYPE allservices,
      ls_serviceprovider TYPE /idxgc/s_servprov_details,
      ls_marketpartner   TYPE /idxgc/s_markpar_details,
      ls_proc_data       TYPE /idxgc/s_proc_data,
      ls_step_all        TYPE /idxgc/s_proc_step_data_all,
      ls_step            TYPE /idxgc/s_proc_step_data,
      lt_proc_step_link  TYPE /idxgc/t_proc_step_link,
      lt_multiple        TYPE /hfq/t_calc_form_h.

DATA: lr_badi_utilts_snd TYPE REF TO /hfq/badi_utilts_snd.

SELECTION-SCREEN BEGIN OF BLOCK init WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS: so_malo FOR lv_malo NO INTERVALS MATCHCODE OBJECT /idxgl/sh_ext_malo.
PARAMETERS: p_date TYPE datum DEFAULT sy-datum.
PARAMETERS: p_time TYPE uzeit DEFAULT '000000'.
PARAMETERS: p_mpar TYPE service_prov MATCHCODE OBJECT e_servprov.
SELECTION-SCREEN END OF BLOCK init.




CONSTANTS: co_utilts_mss_proc_id    TYPE /idxgc/de_proc_id VALUE '/HFQ/UTILTS_MSS', " Statische Prozess-ID
           co_utilts_snd_proc_id    TYPE /idxgc/de_proc_id VALUE '/HFQ/UTILTS_SND', " Statische Prozess-ID
           co_utilts_init_proc_step TYPE /idxgc/de_proc_step_id VALUE '0010'.       " Nummer des Prozessschritts

*--------------------------------------------------------------------*
* Suche nach Konstrukten über MaLo und Datum im BAdI /HFQ/BADI_UTILTS_SND
*--------------------------------------------------------------------*
IF p_date IS INITIAL.
  lv_keydate = sy-datum.
ELSE.
  lv_keydate = p_date.
ENDIF.
IF p_time IS INITIAL.
  lv_keytime = '000000'.
ELSE.
  lv_keytime = p_time.
ENDIF.

GET BADI lr_badi_utilts_snd.

*--------------------------------------------------------------------*
* Datenbeschaffung über den BAdI:
*--------------------------------------------------------------------*
LOOP AT so_malo ASSIGNING FIELD-SYMBOL(<fs_malo>).
  CLEAR: lt_calc_form_h_tmp, lt_calc_form_tmp.

  TRY.
      CALL BADI lr_badi_utilts_snd->select_relevant_data
        EXPORTING
          iv_malo            = <fs_malo>-low " Zählpunktbezeichnung
          iv_keydate         = lv_keydate    " Datum
          iv_keytime         = lv_keytime    " Uhrzeit
          iv_receiver        = p_mpar        " Empfänger
        IMPORTING
          et_calc_form_h     = lt_calc_form_h_tmp " Tabelle für MaLos mit Berechnungsformel
          et_calc_form       = lt_calc_form_tmp   " Tabellentyp zu Tabelle /HFQ/CALC_FORM
        EXCEPTIONS
          no_construct_found = 1                " Kein Konstrukt zur Auswahl gefunden.
          no_calc_form_found = 2                " Keine Berechnungsschritte gefunden.
          OTHERS             = 3.
      IF sy-subrc = 1 OR lt_calc_form_h IS INITIAL.
        "Keine Einträge in Tabelle &1 zu MaLo &2 ab Datum &3 gefunden.
        MESSAGE i001(/hfq/utilts_snd) WITH '/hfq/calc_form_h' <fs_malo>-low lv_keydate.
        EXIT.
      ELSEIF line_exists( lt_calc_form_h[ calc_form_stat = /hfq/cl_dp_utilts=>gc_calcform_stat_z33 ] ) AND ( sy-subrc = 2 OR lt_calc_form IS INITIAL ).
        "Keine Berechnungssschritte in Tabelle &1 zu MaLo &2 gefunden.
        MESSAGE e002(/hfq/utilts_snd) WITH '/hfq/calc_form' <fs_malo>-low.
        EXIT.
      ELSEIF sy-subrc = 2.
        "Nichts tun
      ELSEIF sy-subrc NE 0.
        "Fehler bei Aufruf des BAdI &1.
        MESSAGE e003(/hfq/utilts_snd) WITH '/hfq/badi_utilts_snd->select_relevant_data'.
        EXIT.
      ENDIF.
    CATCH /idxgc/cx_general.
      "Fehler bei Aufruf des BAdI &1.
      MESSAGE e003(/hfq/utilts_snd) WITH '/hfq/badi_utilts_snd->select_relevant_data'.
      EXIT.
  ENDTRY.
  APPEND LINES OF lt_calc_form_h_tmp TO lt_calc_form_h.
  APPEND LINES OF lt_calc_form_tmp TO lt_calc_form.

ENDLOOP.

IF so_malo IS INITIAL.
  TRY.
      CALL BADI lr_badi_utilts_snd->select_relevant_data
        EXPORTING
          iv_malo            = ''
          iv_keydate         = lv_keydate    " Datum
          iv_keytime         = lv_keytime    " Uhrzeit
          iv_receiver        = p_mpar        " Empfänger
        IMPORTING
          et_calc_form_h     = lt_calc_form_h " Tabelle für MaLos mit Berechnungsformel
          et_calc_form       = lt_calc_form   " Tabellentyp zu Tabelle /HFQ/CALC_FORM
        EXCEPTIONS
          no_construct_found = 1                " Kein Konstrukt zur Auswahl gefunden.
          no_calc_form_found = 2                " Keine Berechnungsschritte gefunden.
          OTHERS             = 3.
      IF sy-subrc = 1 OR lt_calc_form_h IS INITIAL.
        "Keine Einträge in Tabelle &1 zu MaLo &2 ab Datum &3 gefunden.
        MESSAGE i001(/hfq/utilts_snd) WITH '/hfq/calc_form_h' space lv_keydate.
        EXIT.
      ELSEIF line_exists( lt_calc_form_h[ calc_form_stat = /hfq/cl_dp_utilts=>gc_calcform_stat_z33 ] ) AND ( sy-subrc = 2 OR lt_calc_form IS INITIAL ).
        "Keine Berechnungssschritte in Tabelle &1 zu MaLo &2 gefunden.
        MESSAGE e002(/hfq/utilts_snd) WITH '/hfq/calc_form' space.
        EXIT.
      ELSEIF sy-subrc = 2.
        "Nichts tun
      ELSEIF sy-subrc NE 0.
        "Fehler bei Aufruf des BAdI &1.
        MESSAGE e003(/hfq/utilts_snd) WITH '/hfq/badi_utilts_snd->select_relevant_data'.
        EXIT.
      ENDIF.
    CATCH /idxgc/cx_general.
      "Fehler bei Aufruf des BAdI &1.
      MESSAGE e003(/hfq/utilts_snd) WITH '/hfq/badi_utilts_snd->select_relevant_data'.
      EXIT.
  ENDTRY.
ENDIF.

*--------------------------------------------------------------------*
* Bei Filter auf den Marktpartner, diesen im ausgewählten Konstrukt ermitteln:
*--------------------------------------------------------------------*
IF p_mpar IS NOT INITIAL.
  SELECT SINGLE * FROM eservprov INTO @DATA(ls_eservprov)
    WHERE serviceid = @p_mpar
      AND own_log_sys = @abap_true.
  IF sy-subrc = 0.
    "Serviceanbieter &1 ist im eigenen System, kein Versand nötig.
    MESSAGE s010(/hfq/utilts_snd) WITH p_mpar.
    EXIT.
  ENDIF.

  IF so_malo IS INITIAL.
    "Bei Filter auf Marktpartner &1 muss eine MaLo definiert sein.
    MESSAGE e004(/hfq/utilts_snd) WITH p_mpar.
    EXIT.
  ENDIF.

  lt_melo_extui = VALUE #( FOR ls_calc_form IN lt_calc_form WHERE ( melo_extui IS NOT INITIAL ) ( ls_calc_form-melo_extui ) ).
  SORT lt_melo_extui.
  DELETE ADJACENT DUPLICATES FROM lt_melo_extui.

  LOOP AT lt_melo_extui INTO lv_melo.

    SELECT SINGLE int_ui FROM euitrans
      INTO @lv_int_ui
      WHERE ext_ui = @lv_melo.

    CALL FUNCTION 'ISU_ALL_SERVICES_SELECT'
      EXPORTING
        x_int_pod               = lv_int_ui    " Interner Schlüssel des Versorgungspunkts
        x_keydate               = lv_keydate   " Datum
      TABLES
        t_all_services          = lt_all_services  " Enthält alle Service- und Vertragsdaten für einen Zählpunkt
      EXCEPTIONS
        not_found               = 1                " Keine Services gefunden
        servprov_not_found      = 2                " Service enthält ungültigen Serviceanbieter
        programming_error       = 3                " Funktionsbaustein wurde falsch aufgerufen
        system_error            = 4                " Inkonsistente Daten im System
        service_type_duplicates = 5                " Servicearten kommen doppelt vor
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lt_all_services INTO ls_all_services
      WHERE serviceid = p_mpar.

      ls_serviceprovider-service_id = ls_all_services-serviceid.
      ls_serviceprovider-service    = ls_all_services-service.
      ls_serviceprovider-date_from  = ls_all_services-service_start.
      ls_serviceprovider-ext_ui = lv_melo.
    ENDLOOP.

    IF ls_serviceprovider IS NOT INITIAL.
      lv_single_proc = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF ls_serviceprovider IS INITIAL.
    "Der Marktpartner &1 ist dem Konstrukt nicht zugeordnet.
    MESSAGE e005(/hfq/utilts_snd) WITH p_mpar.
    EXIT.
  ENDIF.
ENDIF.

*--------------------------------------------------------------------*
* Prozessdaten für ausgewählte MaLos befüllen und PE-Prozesse starten
* Pro MaLo ein Prozess:
*--------------------------------------------------------------------*
lt_malo_extui = VALUE #( FOR ls_calc_form_h IN lt_calc_form_h ( ls_calc_form_h-malo_extui ) ).
SORT lt_malo_extui.
DELETE ADJACENT DUPLICATES FROM lt_malo_extui.

PERFORM list_multiple_calcforms TABLES lt_calc_form_h
                                       lt_malo_extui
                                       lt_multiple.

LOOP AT lt_malo_extui INTO lv_malo.
  CLEAR: ls_proc_data, ls_step.
  ls_proc_data-proc_date = lv_keydate.  " Prozessdatum setzen
  ls_step-date_from = lv_keydate.
  ls_step-time_from = lv_keytime.

  IF lv_single_proc = abap_true.
    ls_proc_data-proc_id = co_utilts_snd_proc_id.  " Prozess setzen
  ELSE.
    ls_proc_data-proc_id = co_utilts_mss_proc_id.  " Prozess setzen
  ENDIF.

  " Zählpunkt setzen
  SELECT SINGLE int_ui FROM euitrans INTO @ls_proc_data-int_ui
    WHERE ext_ui = @lv_malo.
  IF sy-subrc NE 0.
    "Stammdatenfehler: Zu &1 fehlt &2.
    MESSAGE e006(/hfq/utilts_snd) WITH |MaLo { lv_malo }| |INT_UI|.
    EXIT.
  ENDIF.

  "Prozesskonfiguration initial befüllen
  SELECT SINGLE proc_type, proc_view, spartyp FROM /idxgc/proc
   WHERE proc_id = @ls_proc_data-proc_id
     AND active = @abap_true
    INTO ( @ls_proc_data-proc_type, @ls_proc_data-proc_view, @ls_proc_data-spartyp ).
  IF sy-subrc NE 0.
    "Prozesskonfiguration zu Prozess &1 nicht gefunden.
    MESSAGE e008(/hfq/utilts_snd) WITH ls_proc_data-proc_id.
    EXIT.
  ENDIF.
  " Schrittdaten initial befüllen:
  SELECT SINGLE proc_step_no FROM /idxgc/prstep INTO @ls_step-proc_step_no
   WHERE proc_id = @ls_proc_data-proc_id
     AND active = @abap_true
     AND category = 'INIT'.
  IF sy-subrc NE 0.
    "Kein Initialer Prozessschritt zu Prozess &1 gefunden.
    MESSAGE e007(/hfq/utilts_snd) WITH ls_proc_data-proc_id.
    EXIT.
  ENDIF.
  ls_step-ext_ui = lv_malo.

  " eigener Serviceanbieter = VNB
  CALL FUNCTION '/ISIDEX/ISU_SWD_DISTR_SERVPROV'
    EXPORTING
      x_pod         = ls_proc_data-int_ui " Interner Schlüssel des Zählpunkts
      x_swtview     = '01'                " Wechselsicht
      x_swtdate     = lv_keydate          " Termin oder Frist im Lieferantenwechselprozess
    IMPORTING
      y_distributor = ls_step-own_servprov " Serviceanbieter Verteilnetzbetreiber
    EXCEPTIONS
      not_found     = 1                " no values found
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  ls_proc_data-distributor = ls_step-own_servprov.

  " Bei Prozess an nur einen Marktpartner, diesen Empfänger ermitteln:
  IF lv_single_proc = abap_true.
    SELECT SINGLE serviceid, externalid, externalidtyp FROM eservprov
      WHERE serviceid = @ls_serviceprovider-service_id
      INTO (@ls_marketpartner-serviceid, @ls_marketpartner-party_identifier, @ls_marketpartner-codelist_agency).
    IF sy-subrc = 0.
      ls_marketpartner-party_func_qual = /idxgc/if_constants_ide=>gc_nad_qual_mr.
      ls_proc_data-service_prov_new = ls_marketpartner-serviceid.
      ls_step-assoc_servprov = ls_marketpartner-serviceid.
      APPEND ls_marketpartner TO ls_step-marketpartner.
      APPEND ls_serviceprovider TO ls_step-serviceprovider.
    ENDIF.
  ENDIF.

  APPEND ls_step TO ls_proc_data-steps .
*--------------------------------------------------------------------*
* Alles notwendige befüllt, also den neuen Prozess anstoßen
*--------------------------------------------------------------------*
  TRY.
      CALL METHOD /idxgc/cl_process_trigger=>start_process
        EXPORTING
          iv_pdoc_display = space
        CHANGING
          cs_process_data = ls_proc_data.
    CATCH /idxgc/cx_general.
  ENDTRY.
  IF sy-subrc NE 0.
    "Fehler bei Erstellung des Prozesses &1 zu MaLo &2.
    MESSAGE e009(/hfq/utilts_snd) WITH ls_proc_data-proc_id lv_malo.
  ENDIF.

ENDLOOP.

IF lt_multiple IS NOT INITIAL.

  DATA go_alv TYPE REF TO cl_salv_table.
  DATA go_functions TYPE REF TO cl_salv_functions_list.
  DATA go_columns TYPE REF TO cl_salv_columns_table.
  DATA go_display TYPE REF TO cl_salv_display_settings.
*-----------------------------------------------------------------------
* Beginn ALV-Ausgabe
*-----------------------------------------------------------------------

* Instanz der Klasse cl_salv_table erzeugen
  cl_salv_table=>factory(
    IMPORTING r_salv_table = go_alv
    CHANGING t_table = lt_multiple ).

* Funktionstasten (Sortieren, Filtern, Excel-Export etc.)
  go_functions = go_alv->get_functions( ).
  go_functions->set_all( abap_true ).

* optimale Spaltenbreite
  go_columns = go_alv->get_columns( ).
  go_columns->set_optimize( abap_true ).

* Titel und/oder Streifenmuster
  go_display = go_alv->get_display_settings( ).
  go_display->set_list_header( value = 'Zusätzliche Berechnungsformeln' ).
  go_display->set_striped_pattern( abap_true ).

* Liste anzeigen
  go_alv->display( ).
ENDIF.

"Abschlussmeldung:
MESSAGE 'Prozess zum Versand der UTILTS an die Marktpartner ausgelöst.' TYPE 'S'.
*&---------------------------------------------------------------------*
*&      Form  LIST_MULTIPLE_CALCFORMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_CALC_FORM_H  text
*      -->PT_MALO_EXTUI  text
*      -->Pt_MULTIPLE  text
*----------------------------------------------------------------------*
FORM list_multiple_calcforms  TABLES   pt_calc_form_h TYPE /hfq/t_calc_form_h
                                       pt_malo_extui
                                       pt_multiple.
  DATA: lv_first TYPE kennzx.

  SORT pt_calc_form_h BY malo_extui validstart_date validend_date ASCENDING.

  LOOP AT lt_malo_extui ASSIGNING FIELD-SYMBOL(<fs_malo>).
    CLEAR: lv_first.

    LOOP AT pt_calc_form_h ASSIGNING FIELD-SYMBOL(<fs_h>)
      WHERE malo_extui = <fs_malo>.
      IF lv_first IS INITIAL.
        lv_first = abap_true.
        CONTINUE.
      ENDIF.
      APPEND <fs_h> TO pt_multiple.
    ENDLOOP.

  ENDLOOP.

ENDFORM.
