class /HFQ/CL_UTILTS_RCV definition
  public
  create public .

public section.

  class-data GR_PREVIOUS type ref to /IDXGC/CX_GENERAL .

  class-methods CHECK_CALCULATION_FORMULA
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods CHECK_DATE
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods CHECK_MELO_CONFIG
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods CHECK_FLOW
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods SET_RESPONSE_STATUS
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods CHECK_LOSSFACTOR
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods CHECK_ERG
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods SAVE_MSGRESP_STATUS
    importing
      !IV_RESPSTATUS type /IDXGC/DE_RESPSTATUS
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods CHECK_STORED_DATA
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods CHECK_UPDATE_MALO
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
protected section.

  constants CO_ACCEPT type /IDXGC/DE_CHECK_RESULT value 'ACCEPT' ##NO_TEXT.
  constants CO_ERROR type /IDXGC/DE_CHECK_RESULT value 'ERROR' ##NO_TEXT.
  constants CO_EXISTS type /IDXGC/DE_CHECK_RESULT value 'EXISTS' ##NO_TEXT.
  constants CO_WRONG type /IDXGC/DE_CHECK_RESULT value 'WRONG' ##NO_TEXT.
  constants CO_OK type /IDXGC/DE_CHECK_RESULT value 'OK' ##NO_TEXT.
  constants CO_REFUSE type /IDXGC/DE_CHECK_RESULT value 'REFUSE' ##NO_TEXT.
  constants CO_UNKNOWN type /IDXGC/DE_CHECK_RESULT value 'UNKNOWN' ##NO_TEXT.
  constants GC_RESPSTATUS_ZK3 type /IDXGC/DE_RESPSTATUS value 'ZK3' ##NO_TEXT.
  constants GC_RESPSTATUS_ZK4 type /IDXGC/DE_RESPSTATUS value 'ZK4' ##NO_TEXT.
  constants GC_RESPSTATUS_ZK5 type /IDXGC/DE_RESPSTATUS value 'ZK5' ##NO_TEXT.
  constants GC_RESPSTATUS_ZK6 type /IDXGC/DE_RESPSTATUS value 'ZK6' ##NO_TEXT.
  constants GC_RESPSTATUS_ZK7 type /IDXGC/DE_RESPSTATUS value 'ZK7' ##NO_TEXT.
  constants GC_RESPSTATUS_ZQ3 type /IDXGC/DE_RESPSTATUS value 'ZQ3' ##NO_TEXT.
  constants GC_RESPSTATUS_ZQ4 type /IDXGC/DE_RESPSTATUS value 'ZQ4' ##NO_TEXT.
private section.
ENDCLASS.



CLASS /HFQ/CL_UTILTS_RCV IMPLEMENTATION.


  METHOD check_calculation_formula.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Prüft die eingehende Berechnungsformel.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-08-07  Hochfrequenz    angelegt
*&              2020-02-03  Hochfrequenz    Vergleich der Berechnungsformel hinzugefügt
*&-------------------------------------------------------------------*

    " Allgemeine Daten definieren
    DATA: ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
          lv_calc_form_stat     TYPE /idxgc/de_calc_form_stat,
          lv_calc_step_amount   TYPE /idxgc/de_cacl_stp_amount_malo,
          ls_check_details      TYPE /idxgc/s_check_details.

    DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
          lr_badi_utilts_rcv TYPE REF TO /hfq/badi_utilts_rcv.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->* TO <fr_proc_data_extern>.
    lr_proc_step_data ?= <fr_proc_data_extern>.
    " Prozessdaten lesen:
    TRY.
        CALL METHOD lr_proc_step_data->get_process_step_data
          EXPORTING
            is_process_step_key  = is_process_step_key
*           iv_proc_step_timestamp =
          RECEIVING
            rs_process_step_data = ls_proc_step_data_all.
*      CATCH /idxgc/cx_process_error.
*        ls_proc_step_data_all = lr_proc_step_data->get_process_step_data(  ).
      CATCH /idxgc/cx_process_error INTO gr_previous .

        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Datenverarbeitung
*--------------------------------------------------------------------*
    TRY.
        GET BADI lr_badi_utilts_rcv.
        CALL BADI lr_badi_utilts_rcv->select_cf_details
          EXPORTING
            iv_malo             = ls_proc_step_data_all-ext_ui
            iv_keydate          = ls_proc_step_data_all-proc_date
          IMPORTING
            ev_calc_form_stat   = lv_calc_form_stat
            ev_calc_step_amount = lv_calc_step_amount
          EXCEPTIONS
            no_construct_found  = 1
            no_calc_form_found  = 2
            OTHERS              = 3.
        IF sy-subrc EQ 3.
          append /hfq/cl_utilts_rcv=>co_error to et_check_result.
          RETURN.
        ELSEIF sy-subrc EQ 1 OR sy-subrc EQ 2.
          APPEND /hfq/cl_utilts_rcv=>co_unknown to et_check_result.
          RETURN.
        ENDIF.
      CATCH /idxgc/cx_utility_error.
    ENDTRY.

    READ TABLE ls_proc_step_data_all-diverse ASSIGNING FIELD-SYMBOL(<fs_diverse>) INDEX 1.
    IF lv_calc_form_stat NE <fs_diverse>-calc_form_stat
      OR lv_calc_step_amount NE <fs_diverse>-calc_step_amount_malo.
            " Ablehnung mit 'E14: Sonstiges'
      CALL METHOD save_msgresp_status
        EXPORTING
          iv_respstatus       = /idxgc/if_constants_ide=>gc_respstatus_e14
          is_process_step_key = is_process_step_key
        IMPORTING
          et_check_result    = et_check_result
        CHANGING
          cr_data             = cr_data
          cr_data_log         = cr_data_log.

      append  /hfq/cl_utilts_rcv=>co_wrong to et_check_result.
      return.
    ELSE.
      append /hfq/cl_utilts_rcv=>co_exists to et_check_result.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD check_date.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Prüft das Datum der eingehenden Berechnungsformel.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-06  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

    " Allgemeine Daten definieren
    DATA: ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
          lv_dateto             TYPE datum,
          lv_datefrom           TYPE datum,
          lv_timeto             TYPE uzeit,
          lv_timefrom           TYPE uzeit,
          ls_check_details      TYPE /idxgc/s_check_details,
          ls_diverse            TYPE /idxgc/s_diverse_details.

    DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
          lr_badi_utilts_rcv TYPE REF TO /hfq/badi_utilts_rcv.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->* TO <fr_proc_data_extern>.
    lr_proc_step_data ?= <fr_proc_data_extern>.

    " Prozessdaten lesen:
    TRY.
        CALL METHOD lr_proc_step_data->get_process_step_data
          EXPORTING
            is_process_step_key  = is_process_step_key
*           iv_proc_step_timestamp =
          RECEIVING
            rs_process_step_data = ls_proc_step_data_all.
*      CATCH /idxgc/cx_process_error.
*        ls_proc_step_data_all = lr_proc_step_data->get_process_step_data(  ).
      CATCH /idxgc/cx_process_error INTO gr_previous .

        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Prüfung
*--------------------------------------------------------------------*
    TRY.
        GET BADI lr_badi_utilts_rcv.
        CALL BADI lr_badi_utilts_rcv->select_cf_details
          EXPORTING
            iv_malo            = ls_proc_step_data_all-ext_ui
            iv_keydate         = ls_proc_step_data_all-proc_date
          IMPORTING
            ev_dateto          = lv_dateto
            ev_datefrom        = lv_datefrom
            ev_timeto          = lv_timeto
            ev_timefrom        = lv_timefrom
          EXCEPTIONS
            no_construct_found = 1
            no_calc_form_found = 2
            OTHERS             = 3.
        IF sy-subrc EQ 3.
          APPEND  /hfq/cl_utilts_rcv=>co_error TO et_check_result.
          RETURN.
        ENDIF.
      CATCH /idxgc/cx_utility_error.
    ENDTRY.

    READ TABLE ls_proc_step_data_all-diverse INDEX 1 INTO ls_diverse.
    IF sy-subrc EQ 0.

      SELECT SINGLE * FROM /hfq/calc_form_h INTO @DATA(ls_calc_form_h)
        WHERE malo_extui EQ @ls_proc_step_data_all-ext_ui
          AND validstart_date GT @ls_diverse-validstart_date.
      IF ls_calc_form_h IS NOT INITIAL.
        APPEND 'FUTURE_CF' TO et_check_result.
        RETURN.
      ENDIF.

    ENDIF.

    "Hier angekommen alles in Ordnung
    APPEND /hfq/cl_utilts_rcv=>co_ok TO et_check_result.
    RETURN.

    "wenn Datum falsch ist, Antwortstatus setzen
    " Ablehnung mit 'ZK3':
*    CALL METHOD save_msgresp_status
*      EXPORTING
*        iv_respstatus       = /hfq/cl_utilts_rcv=>gc_respstatus_zk3
*        is_process_step_key = is_process_step_key
*      IMPORTING
*        et_check_result     = et_check_result
*      CHANGING
*        cr_data             = cr_data
*        cr_data_log         = cr_data_log.

  ENDMETHOD.


METHOD check_erg.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Prüft das Datum der eingehenden Berechnungsformel.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-06  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

  " Allgemeine Daten definieren
  DATA: ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
        lv_result             TYPE /idxgc/de_check_result,
        lt_pod_data           TYPE /idxgl/t_pod_data_details.

  DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
        lr_badi_utilts_rcv TYPE REF TO /hfq/badi_utilts_rcv.

  FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                 <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

  " Referenzen und Log zuordnen:
  ASSIGN cr_data_log->* TO <fr_process_log>.
  ASSIGN cr_data->* TO <fr_proc_data_extern>.
  lr_proc_step_data ?= <fr_proc_data_extern>.

  " Prozessdaten lesen:
  TRY.
      CALL METHOD lr_proc_step_data->get_process_step_data
        EXPORTING
          is_process_step_key  = is_process_step_key
*         iv_proc_step_timestamp =
        RECEIVING
          rs_process_step_data = ls_proc_step_data_all.
*      CATCH /idxgc/cx_process_error.
*        ls_proc_step_data_all = lr_proc_step_data->get_process_step_data(  ).
    CATCH /idxgc/cx_process_error INTO gr_previous .

      CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
        EXPORTING
          ir_previous = gr_previous.
  ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Prüfung
*--------------------------------------------------------------------*
  TRY.
      GET BADI lr_badi_utilts_rcv.
      CALL BADI lr_badi_utilts_rcv->select_cf_details
        EXPORTING
          iv_malo            = ls_proc_step_data_all-ext_ui
          iv_keydate         = ls_proc_step_data_all-proc_date
        IMPORTING
          et_pod_data        = lt_pod_data
        EXCEPTIONS
          no_construct_found = 1
          no_calc_form_found = 2
          OTHERS             = 3.
      IF sy-subrc EQ 3.
        APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
        RETURN.
      ENDIF.
    CATCH /idxgc/cx_utility_error.
  ENDTRY.

  lv_result = /hfq/cl_utilts_rcv=>co_ok.

  IF lt_pod_data IS NOT INITIAL.
    READ TABLE ls_proc_step_data_all-diverse ASSIGNING FIELD-SYMBOL(<fs_diverse_rcv>) INDEX 1.
    IF sy-subrc NE 0.
      APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
      RETURN.
    ENDIF.
    LOOP AT lt_pod_data ASSIGNING FIELD-SYMBOL(<fs_pod_data>).
      IF <fs_pod_data>-supply_direct NE <fs_diverse_rcv>-supply_direct.
        lv_result = /hfq/cl_utilts_rcv=>co_wrong.
      ENDIF.
    ENDLOOP.
  ELSE.
    APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
    RETURN.
  ENDIF.

  APPEND lv_result TO et_check_result.

  IF lv_result EQ /hfq/cl_utilts_rcv=>co_wrong.
    CALL METHOD save_msgresp_status
      EXPORTING
        iv_respstatus       = /hfq/cl_utilts_rcv=>gc_respstatus_zq3
        is_process_step_key = is_process_step_key
      IMPORTING
        et_check_result     = et_check_result
      CHANGING
        cr_data             = cr_data
        cr_data_log         = cr_data_log.
  ENDIF.
ENDMETHOD.


METHOD check_flow.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Prüft das Datum der eingehenden Berechnungsformel.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-06  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

  " Allgemeine Daten definieren
  DATA: ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
        lv_result             TYPE /idxgc/de_check_result,
        lt_pod_data           TYPE /idxgl/t_pod_data_details.

  DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
        lr_badi_utilts_rcv TYPE REF TO /hfq/badi_utilts_rcv.

  FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                 <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

  " Referenzen und Log zuordnen:
  ASSIGN cr_data_log->* TO <fr_process_log>.
  ASSIGN cr_data->* TO <fr_proc_data_extern>.
  lr_proc_step_data ?= <fr_proc_data_extern>.

  " Prozessdaten lesen:
  TRY.
      CALL METHOD lr_proc_step_data->get_process_step_data
        EXPORTING
          is_process_step_key  = is_process_step_key
*         iv_proc_step_timestamp =
        RECEIVING
          rs_process_step_data = ls_proc_step_data_all.
*      CATCH /idxgc/cx_process_error.
*        ls_proc_step_data_all = lr_proc_step_data->get_process_step_data(  ).
    CATCH /idxgc/cx_process_error INTO gr_previous .

      CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
        EXPORTING
          ir_previous = gr_previous.
  ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Prüfung
*--------------------------------------------------------------------*

  TRY.
      GET BADI lr_badi_utilts_rcv.
      CALL BADI lr_badi_utilts_rcv->select_cf_details
        EXPORTING
          iv_malo            = ls_proc_step_data_all-ext_ui
          iv_keydate         = ls_proc_step_data_all-proc_date
        IMPORTING
          et_pod_data        = lt_pod_data
        EXCEPTIONS
          no_construct_found = 1
          no_calc_form_found = 2
          OTHERS             = 3.
      IF sy-subrc EQ 3.
        APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
        RETURN.
      ENDIF.
    CATCH /idxgc/cx_utility_error.
  ENDTRY.

  IF lt_pod_data IS NOT INITIAL AND ls_proc_step_data_all-/idxgl/pod_data IS NOT INITIAL.
    LOOP AT ls_proc_step_data_all-/idxgl/pod_data ASSIGNING FIELD-SYMBOL(<fs_pod_data_rcv>).
      READ TABLE lt_pod_data ASSIGNING FIELD-SYMBOL(<fs_pod_data>) WITH KEY ext_ui = <fs_pod_data_rcv>-ext_ui calc_step_id = <fs_pod_data_rcv>-calc_step_id.
      IF sy-subrc NE 0.
        APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
        RETURN.
      ELSE.
        IF <fs_pod_data_rcv>-flow_direction NE <fs_pod_data>-flow_direction.
          APPEND /hfq/cl_utilts_rcv=>co_wrong TO et_check_result.
          CALL METHOD save_msgresp_status
            EXPORTING
              iv_respstatus       = /hfq/cl_utilts_rcv=>gc_respstatus_zk7
              is_process_step_key = is_process_step_key
            IMPORTING
              et_check_result    = et_check_result
            CHANGING
              cr_data             = cr_data
              cr_data_log         = cr_data_log.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
    RETURN.
  ENDIF.

  APPEND /hfq/cl_utilts_rcv=>co_ok TO et_check_result.

ENDMETHOD.


METHOD check_lossfactor.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Prüft das Datum der eingehenden Berechnungsformel.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-06  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

  " Allgemeine Daten definieren
  DATA: ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
        lt_pod_data           TYPE /idxgl/t_pod_data_details,
        ls_check_details      TYPE /idxgc/s_check_details.

  DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
        lr_badi_utilts_rcv TYPE REF TO /hfq/badi_utilts_rcv.

  FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                 <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

  " Referenzen und Log zuordnen:
  ASSIGN cr_data_log->* TO <fr_process_log>.
  ASSIGN cr_data->* TO <fr_proc_data_extern>.
  lr_proc_step_data ?= <fr_proc_data_extern>.

  " Prozessdaten lesen:
  TRY.
      CALL METHOD lr_proc_step_data->get_process_step_data
        EXPORTING
          is_process_step_key  = is_process_step_key
        RECEIVING
          rs_process_step_data = ls_proc_step_data_all.
    CATCH /idxgc/cx_process_error INTO gr_previous .

      CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
        EXPORTING
          ir_previous = gr_previous.
  ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Prüfung
*--------------------------------------------------------------------*
  TRY.
      GET BADI lr_badi_utilts_rcv.
      CALL BADI lr_badi_utilts_rcv->select_cf_details
        EXPORTING
          iv_malo            = ls_proc_step_data_all-ext_ui
          iv_keydate         = ls_proc_step_data_all-proc_date
        IMPORTING
          et_pod_data        = lt_pod_data
        EXCEPTIONS
          no_construct_found = 1
          no_calc_form_found = 2
          OTHERS             = 3.
      IF sy-subrc EQ 3.
        APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
        RETURN.
      ENDIF.
    CATCH /idxgc/cx_utility_error.
  ENDTRY.


  " Wir setzen das Prüfresultat standardmäßig auf 'WRONG'.
  " Der anschließende Vergleich setzt das Ergebnis auf 'OK' falls Unterschiede
  " in den Verlustfaktoren der Berechnungsformel gefunden wurden und hängt das Ergebnis
  " an die ET_Check_Results.
  IF lt_pod_data IS NOT INITIAL
    AND ls_proc_step_data_all-/idxgl/pod_data IS NOT INITIAL.
    LOOP AT ls_proc_step_data_all-/idxgl/pod_data ASSIGNING FIELD-SYMBOL(<fs_pod_data_rcv>).
      READ TABLE lt_pod_data ASSIGNING FIELD-SYMBOL(<fs_pod_data>) WITH KEY ext_ui = <fs_pod_data_rcv>-ext_ui calc_step_id = <fs_pod_data_rcv>-calc_step_id.
      IF sy-subrc NE 0.
        APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
        RETURN.
      ELSE.
        IF <fs_pod_data_rcv>-lossfact_ext NE <fs_pod_data>-lossfact_ext.
          " Unterschied gefunden.
          APPEND /hfq/cl_utilts_rcv=>co_ok TO et_check_result.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
    RETURN.
  ENDIF.

  APPEND /hfq/cl_utilts_rcv=>co_wrong TO et_check_result.
  CALL METHOD save_msgresp_status
    EXPORTING
      iv_respstatus       = /hfq/cl_utilts_rcv=>gc_respstatus_zq4
      is_process_step_key = is_process_step_key
    IMPORTING
      et_check_result     = et_check_result
    CHANGING
      cr_data             = cr_data
      cr_data_log         = cr_data_log.

ENDMETHOD.


  METHOD check_melo_config.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Prüft das Datum der eingehenden Berechnungsformel.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-06  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*
    DATA: ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
          lt_pod_data           TYPE /idxgl/t_pod_data_details,
          lv_miss               TYPE flag VALUE abap_false,
          lv_add                TYPE flag VALUE abap_false,
          lv_result             TYPE /idxgc/de_check_result,
          lv_respstatus         TYPE /idxgc/de_respstatus.

    DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
          lr_badi_utilts_rcv TYPE REF TO /hfq/badi_utilts_rcv.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->* TO <fr_proc_data_extern>.
    lr_proc_step_data ?= <fr_proc_data_extern>.

    " Prozessdaten lesen:
    TRY.
        CALL METHOD lr_proc_step_data->get_process_step_data
          EXPORTING
            is_process_step_key  = is_process_step_key
*           iv_proc_step_timestamp =
          RECEIVING
            rs_process_step_data = ls_proc_step_data_all.
*      CATCH /idxgc/cx_process_error.
*        ls_proc_step_data_all = lr_proc_step_data->get_process_step_data(  ).
      CATCH /idxgc/cx_process_error INTO gr_previous .

        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Prüfung
*--------------------------------------------------------------------*
    TRY.
        GET BADI lr_badi_utilts_rcv.
        CALL BADI lr_badi_utilts_rcv->select_cf_details
          EXPORTING
            iv_malo            = ls_proc_step_data_all-ext_ui
            iv_keydate         = ls_proc_step_data_all-proc_date
          IMPORTING
            et_pod_data        = lt_pod_data
          EXCEPTIONS
            no_construct_found = 1
            no_calc_form_found = 2
            OTHERS             = 3.
        IF sy-subrc EQ 3.
          APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
          RETURN.
        ENDIF.
      CATCH /idxgc/cx_utility_error.
    ENDTRY.

    IF lt_pod_data IS NOT INITIAL AND ls_proc_step_data_all-/idxgl/pod_data IS NOT INITIAL.
*       i) prüfen ob Melos alle in Nachricht vorhanden
      LOOP AT lt_pod_data ASSIGNING FIELD-SYMBOL(<fs_pod_data>).
        READ TABLE ls_proc_step_data_all-/idxgl/pod_data TRANSPORTING NO FIELDS WITH KEY ext_ui = <fs_pod_data>-ext_ui.
        IF sy-subrc NE 0.
          lv_miss = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

*       ii) prüfen ob zusätzliche Melo in Nachricht vorhanden
      LOOP AT ls_proc_step_data_all-/idxgl/pod_data ASSIGNING FIELD-SYMBOL(<fs_pod_data_rcv>).
        READ TABLE lt_pod_data TRANSPORTING NO FIELDS WITH KEY ext_ui = <fs_pod_data_rcv>-ext_ui.
        IF sy-subrc NE 0.
          lv_add = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_miss EQ abap_true
        AND lv_add EQ abap_false.
        " min. eine Melo fehlt
        lv_result = /hfq/cl_utilts_rcv=>co_wrong.
        lv_respstatus = /hfq/cl_utilts_rcv=>gc_respstatus_zk4.
      ELSEIF lv_miss EQ abap_false
        AND lv_add EQ abap_true.
        " min. eine Melo zu viel
        lv_result = /hfq/cl_utilts_rcv=>co_wrong.
        lv_respstatus = /hfq/cl_utilts_rcv=>gc_respstatus_zk5.
      ELSEIF lv_miss EQ abap_true
        AND lv_add EQ abap_true.
        " min. eine Melo zu viel und zu wenig, d.h. min. 1 Melo-ID stimmt nicht überein.
        lv_result = /hfq/cl_utilts_rcv=>co_wrong.
        lv_respstatus = /hfq/cl_utilts_rcv=>gc_respstatus_zk6.
      ELSE.
        " Melo stimmen überein.
        lv_result = /hfq/cl_utilts_rcv=>co_ok.
      ENDIF.

    ELSE.
      APPEND /hfq/cl_utilts_rcv=>co_error TO et_check_result.
      RETURN.
    ENDIF.

    APPEND lv_result TO et_check_result.

    IF lv_result EQ /hfq/cl_utilts_rcv=>co_wrong.
      CALL METHOD save_msgresp_status
        EXPORTING
          iv_respstatus       = lv_respstatus
          is_process_step_key = is_process_step_key
        IMPORTING
          et_check_result     = et_check_result
        CHANGING
          cr_data             = cr_data
          cr_data_log         = cr_data_log.
    ENDIF.

  ENDMETHOD.


METHOD CHECK_STORED_DATA.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Prüft das Datum der eingehenden Berechnungsformel.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-06  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

  " Allgemeine Daten definieren
  DATA: ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
        lv_result             TYPE /idxgc/de_check_result,
        lt_pod_data           TYPE /idxgl/t_pod_data_details.

  DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
        lr_badi_utilts_rcv TYPE REF TO /hfq/badi_utilts_rcv.

  FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                 <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

  " Referenzen und Log zuordnen:
  ASSIGN cr_data_log->* TO <fr_process_log>.
  ASSIGN cr_data->* TO <fr_proc_data_extern>.
  lr_proc_step_data ?= <fr_proc_data_extern>.

  " Prozessdaten lesen:
  TRY.
      CALL METHOD lr_proc_step_data->get_process_step_data
        EXPORTING
          is_process_step_key  = is_process_step_key
*         iv_proc_step_timestamp =
        RECEIVING
          rs_process_step_data = ls_proc_step_data_all.
*      CATCH /idxgc/cx_process_error.
*        ls_proc_step_data_all = lr_proc_step_data->get_process_step_data(  ).
    CATCH /idxgc/cx_process_error INTO gr_previous .

      CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
        EXPORTING
          ir_previous = gr_previous.
  ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Prüfung
*--------------------------------------------------------------------*

  get badi lr_badi_utilts_rcv.
  call badi lr_badi_utilts_rcv->check_data_storage
    EXPORTING
      iv_malo    = ls_proc_step_data_all-ext_ui
      iv_keydate = ls_proc_step_data_all-proc_date
    IMPORTING
      ev_result  = lv_result.

  APPEND lv_result TO et_check_result.

ENDMETHOD.


  METHOD check_update_malo.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Updatet die Schrittdaten mit der Marktlokation
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-06  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

    DATA lr_badi_data_access  TYPE REF TO /idxgl/badi_data_access.
    DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
          lr_badi_utilts_rcv TYPE REF TO /hfq/badi_utilts_rcv.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->* TO <fr_proc_data_extern>.
    lr_proc_step_data ?= <fr_proc_data_extern>.

    " Prozessdaten lesen:
    TRY.
        CALL METHOD lr_proc_step_data->get_process_step_data
          EXPORTING
            is_process_step_key  = is_process_step_key
          RECEIVING
            rs_process_step_data = DATA(ls_proc_step_data_all).
      CATCH /idxgc/cx_process_error INTO gr_previous .

        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Prüfung
*--------------------------------------------------------------------*
    IF ls_proc_step_data_all-ext_ui IS INITIAL
      OR ls_proc_step_data_all-int_ui IS INITIAL.
      GET BADI lr_badi_data_access.
      LOOP AT ls_proc_step_data_all-pod ASSIGNING FIELD-SYMBOL(<ls_pod>).
        TRY.
            CALL BADI lr_badi_data_access->is_pod_malo
              EXPORTING
*               iv_int_ui      =
                iv_ext_ui      = <ls_pod>-ext_ui
*               iv_key_date    = SY-DATUM
              RECEIVING
                rv_pod_is_malo = DATA(lv_malo).
            IF lv_malo EQ abap_true.
              SELECT SINGLE ext_ui, int_ui FROM euitrans INTO (@ls_proc_step_data_all-ext_ui, @ls_proc_step_data_all-int_ui)
                WHERE datefrom LE @ls_proc_step_data_all-proc_date
                AND dateto     GE @ls_proc_step_data_all-proc_date
                AND ext_ui     EQ @<ls_pod>-ext_ui.
              IF sy-subrc NE 0.
                APPEND /idxgc/if_constants_add=>gc_cr_error TO et_check_result.
                RETURN.
              ENDIF.
              EXIT.
            ENDIF.
            "Fehler einfach nächsten Eintrag.
          CATCH /idxgc/cx_general.
            CONTINUE.
        ENDTRY.

      ENDLOOP.
      lr_proc_step_data->update_process_step_data( is_process_step_data = ls_proc_step_data_all ).
      commit WORK and wait.
      "LUW nochmal expliziet wechseln, damit die Folgeprüfung nichts falsch machen kann !
      wait up to 1 SECONDS.
    ENDIF.
    append  /idxgc/if_constants_add=>gc_cr_ok TO et_check_result.

  ENDMETHOD.


  METHOD save_msgresp_status.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Speichert den Antwortstatus in den Prozessdaten.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-11  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

    FIELD-SYMBOLS: <fs_ref_process_data_extern> TYPE REF TO /idxgc/if_process_data_extern,
                   <fs_ref_process_log>         TYPE REF TO /idxgc/if_process_log.

    DATA: ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
          ls_proc_step_data     TYPE /idxgc/s_proc_step_data,
          ls_proc_data          TYPE /idxgc/s_proc_data,
          ls_msgrespstatus      TYPE /idxgc/s_msgsts_details,
          lr_process_data       TYPE REF TO /idxgc/if_process_data,
          lv_text               TYPE zgc_answtext.

*--------------------------------------------------------------------*

    ASSIGN cr_data->*     TO  <fs_ref_process_data_extern> .
    ASSIGN cr_data_log->* TO  <fs_ref_process_log>.

    lr_process_data ?= <fs_ref_process_data_extern>.
*----------------------------------------------------------------------*
* Get process data & step data
    TRY.
        CALL METHOD <fs_ref_process_data_extern>->get_process_step_data
          EXPORTING
            is_process_step_key  = is_process_step_key
          RECEIVING
            rs_process_step_data = ls_proc_step_data_all.

        CALL METHOD <fs_ref_process_data_extern>->get_process_data
          IMPORTING
            es_process_data = ls_proc_data.

      CATCH /idxgc/cx_process_error INTO gr_previous.
        <fs_ref_process_log>->add_message_to_process_log( ).
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = gr_previous ).
    ENDTRY.
*----------------------------------------------------------------------*

*   Setzen des Langtexts der Antwort
    CASE iv_respstatus.
      WHEN /idxgc/if_constants_ide=>gc_respstatus_e14.
        lv_text = 'Ablehnung Sonstiges'.
      WHEN gc_respstatus_zk3.
        lv_text = 'Gültig ab Datum unplausibel'.
      WHEN gc_respstatus_zk4.
        lv_text = 'Melo in der Berechnungsformel fehlen'.
      WHEN gc_respstatus_zk5.
        lv_text = ' zu viele Melo in der Berechnungsformel'.
      WHEN gc_respstatus_zk6.
        lv_text = ' ID der Melo stimmen nicht überein'.
      WHEN gc_respstatus_zk7.
        lv_text = 'Flussrichtung ist nicht korrekt'.
      WHEN gc_respstatus_zq3.
        lv_text = 'Lieferrichtung ist nicht korrekt'.
      WHEN gc_respstatus_zq4.
        lv_text = 'Leitungs- oder Trafoverlust hat sich nicht geändert'.
    ENDCASE.

    " item_id setzen, sodass nur einmal vergeben.
    IF ls_proc_step_data_all-msgrespstatus IS NOT INITIAL.
      READ TABLE ls_proc_step_data_all-msgrespstatus ASSIGNING FIELD-SYMBOL(<fs_msgrespstatus>) INDEX lines( ls_proc_step_data_all-msgrespstatus ).
      ls_msgrespstatus-item_id = 1 + <fs_msgrespstatus>-item_id.
    ELSE.
      ls_msgrespstatus-item_id = 1.
    ENDIF.
    ls_msgrespstatus-respstatus = iv_respstatus.
    ls_msgrespstatus-text_msgantwort = lv_text.
    APPEND ls_msgrespstatus TO ls_proc_step_data_all-msgrespstatus.

    TRY.
        MOVE-CORRESPONDING ls_proc_step_data_all TO ls_proc_step_data.
        CALL METHOD lr_process_data->update_process_step_data
          EXPORTING
            is_process_step_data = ls_proc_step_data.
      CATCH /idxgc/cx_process_error  INTO gr_previous.
        <fs_ref_process_log>->add_message_to_process_log( ).
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = gr_previous ).
    ENDTRY.

  ENDMETHOD.


METHOD set_response_status.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Prüft das Datum der eingehenden Berechnungsformel.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2020-02-06  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

  " Allgemeine Daten definieren
  DATA: ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
        ls_proc_step_data     TYPE /idxgc/s_proc_step_data,
        ls_check_details      TYPE /idxgc/s_check_details,
        ls_response           TYPE /idxgc/s_msgsts_details,
        lt_response           TYPE /idxgc/t_msgsts_details.

  FIELD-SYMBOLS: <fs_check> TYPE /idxgc/s_check_details.

  DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step.

  FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                 <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

  " Referenzen und Log zuordnen:
  ASSIGN cr_data_log->* TO <fr_process_log>.
  ASSIGN cr_data->* TO <fr_proc_data_extern>.
  lr_proc_step_data ?= <fr_proc_data_extern>.

  " Prozessdaten lesen:
  TRY.
      CALL METHOD lr_proc_step_data->get_process_step_data
        EXPORTING
          is_process_step_key  = is_process_step_key
*         iv_proc_step_timestamp =
        RECEIVING
          rs_process_step_data = ls_proc_step_data_all.
*      CATCH /idxgc/cx_process_error.
*        ls_proc_step_data_all = lr_proc_step_data->get_process_step_data(  ).
    CATCH /idxgc/cx_process_error INTO gr_previous .

      CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
        EXPORTING
          ir_previous = gr_previous.
  ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Prüfung
*--------------------------------------------------------------------*

  DESCRIBE TABLE ls_proc_step_data_all-msgrespstatus LINES DATA(lv_lines).

  IF lv_lines EQ 0.
    " Wenn der Antwortstatus leer ist, wurde kein Ablehngrund gesetzt, d.h. die Berechnungsformel wird angenommen.
    ls_response = VALUE #( respstatus = /idxgc/if_constants_ide=>gc_respstatus_e15
                           item_id = 1 ).
    APPEND /hfq/cl_utilts_rcv=>co_accept TO et_check_result.
  ELSE.
    " Bei gefülltem Antwortstatus wird die Berechnungsformel abgelehnt.
    " Der Antwortstatus 'E14' wird später nur übertragen, wenn kein eindeutiger Antwortstatus gesetzt werden kann (s.u.).
    ls_response = VALUE #( respstatus = /idxgc/if_constants_ide=>gc_respstatus_e14
                           item_id = 1 ).
    APPEND /hfq/cl_utilts_rcv=>co_refuse TO et_check_result.
  ENDIF.

  APPEND ls_response TO lt_response.

  " Wenn es genau einen Antwortstatus gibt, brauchen wir diesen nicht überschreiben.
  IF lv_lines NE 1.
    ls_proc_step_data_all-msgrespstatus = lt_response.
  ENDIF.

  TRY.
      MOVE-CORRESPONDING ls_proc_step_data_all TO ls_proc_step_data.
      CALL METHOD lr_proc_step_data->update_process_step_data
        EXPORTING
          is_process_step_data = ls_proc_step_data_all.
    CATCH /idxgc/cx_process_error  INTO gr_previous.
      <fr_process_log>->add_message_to_process_log( ).
      CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = gr_previous ).
  ENDTRY.
ENDMETHOD.
ENDCLASS.
