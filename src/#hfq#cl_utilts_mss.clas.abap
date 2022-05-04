class /HFQ/CL_UTILTS_MSS definition
  public
  create public .

public section.

  class-data GR_PREVIOUS type ref to /IDXGC/CX_GENERAL .
  constants CO_UTILTS_SND_PROC_ID type /IDXGC/DE_PROC_ID value '/HFQ/UTILTS_SND' ##NO_TEXT.
  constants CO_UTILTS_SND_INIT_STEP type /IDXGC/DE_PROC_STEP_NO value '0010' ##NO_TEXT.
  constants CO_UTILTS_MSS_PROC_ID type /IDXGC/DE_PROC_ID value '/HFQ/UTILTS_MSS' ##NO_TEXT.
  constants CO_UTILTS_MSS_INIT_STEP type /IDXGC/DE_PROC_STEP_NO value '0010' ##NO_TEXT.

  class-methods GET_CORRESPONDING_MALOS
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods GET_CORRESPONDING_MELOS
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods GET_SERV_PROV_LIST
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods INFORM_SERV_PROV
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR
      /IDXGC/CX_PROCESS_ERROR .
  class-methods CHECK_SUB_PROCESS_STATUS
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR
      /IDXGC/CX_PROCESS_ERROR
      /IDXGC/CX_GENERAL .
protected section.

  constants CO_ERROR type /IDXGC/DE_CHECK_RESULT value 'ERROR' ##NO_TEXT.
  constants CO_SUCCESS type /IDXGC/DE_CHECK_RESULT value 'SUCCESS' ##NO_TEXT.
  constants CO_EMPTY type /IDXGC/DE_CHECK_RESULT value 'EMPTY' ##NO_TEXT.
  constants CO_MULTIPLE type /IDXGC/DE_CHECK_RESULT value 'MULTIPLE' ##NO_TEXT.
  constants CO_ACTIVE type /IDXGC/DE_CHECK_RESULT value 'ACTIVE' ##NO_TEXT.
  constants CO_WARNING type /IDXGC/DE_CHECK_RESULT value 'WARNING' ##NO_TEXT.
  constants CO_USER_DECI type /IDXGC/DE_CHECK_RESULT value 'USER_DECI' ##NO_TEXT.
  constants CO_COMPLETE type /IDXGC/DE_CHECK_RESULT value 'COMPLETE' ##NO_TEXT.
  constants CO_OBSOLETE type /IDXGC/DE_CHECK_RESULT value 'OBSOLETE' ##NO_TEXT.

  class-methods GET_SERV_PROV
    importing
      !IV_SERVICE_TYP type INTCODE
      !IV_INT_UI type INT_UI
      !IV_PROC_DATE type DATUM default SY-DATUM
    exporting
      !ET_SERVICEPROVIDER type /IDXGC/T_SERVPROV_DETAILS
    changing
      !CT_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods CREATE_NEW_PDOC
    importing
      !IV_PROC_ID type /IDXGC/DE_PROC_ID
      !IV_PROC_STEP_NO type /IDXGC/DE_PROC_STEP_NO
    changing
      !CS_PROC_DATA_NEW type /IDXGC/S_PROC_DATA
      !CR_CTX type ref to /IDXGC/CL_PD_DOC_CONTEXT
    raising
      /IDXGC/CX_PROCESS_ERROR
      /IDXGC/CX_GENERAL .
private section.

  class-methods PROVIDE_INTUI_FROM_EXTUI
    importing
      !IV_EXT_UI type EXT_UI
      !IV_KEYDATE type DATUM
    returning
      value(RV_INT_UI) type INT_UI .
  class-methods RECEIVER_IS_LIEF
    importing
      !IV_RECEIVER type E_DEXSERVPROV
    returning
      value(RV_IS_LIEF) type KENNZX .
  class-methods UPDATE_PROC_DATE
    importing
      !IV_PROC_DATE_OLD type /IDXGC/DE_PROC_DATE
      !IV_START_DATE_SERVICE type /IDXGC/DE_DATE_FROM
    returning
      value(RV_PROC_DATE_UPD) type /IDXGC/DE_PROC_DATE .
ENDCLASS.



CLASS /HFQ/CL_UTILTS_MSS IMPLEMENTATION.


  METHOD check_sub_process_status.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Prüft den Status der zugehörigen Sub-Prozesse /HFQ/UTILTS_SND
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-06-24  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

    " Allgemeine Daten definieren
    DATA: ls_step_all       TYPE /idxgc/s_proc_step_data_all,
          lr_proc_step_data TYPE REF TO /idxgc/if_process_data_step,
          ls_proc_data      TYPE /idxgc/s_proc_data,
          lr_ctx            TYPE REF TO /idxgc/cl_pd_doc_context.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->* TO <fr_proc_data_extern>.
    lr_proc_step_data ?= <fr_proc_data_extern>.

    " spezifische Daten definieren:
    DATA: lt_proc_link          TYPE TABLE OF /idxgc/proc_lnk,
          lt_subproc_stat_range TYPE RANGE OF /idxgc/proc_lnk.

    " Prozessdaten lesen:
    TRY.
        ls_step_all = lr_proc_step_data->get_process_step_data( is_process_step_key ).
      CATCH /idxgc/cx_process_error INTO gr_previous.
        CALL METHOD /idxgc/cx_process_error=>raise_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

    CALL METHOD <fr_proc_data_extern>->get_process_data
      IMPORTING
        es_process_data = ls_proc_data.

*--------------------------------------------------------------------*
* Kontext-Instanz generieren
*--------------------------------------------------------------------*
    TRY .
        CALL METHOD /idxgc/cl_pd_doc_context=>get_instance
          EXPORTING
            iv_pdoc_no = ls_proc_data-proc_ref
            iv_wmode   = cl_isu_wmode=>co_change
          RECEIVING
            rr_ctx     = lr_ctx.
      CATCH /idxgc/cx_process_error INTO gr_previous.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
      CATCH /idxgc/cx_general INTO gr_previous.
        CALL METHOD /idxgc/cx_general=>raise_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Datenverarbeitung
*--------------------------------------------------------------------*
    SELECT * FROM /idxgc/proc_lnk INTO TABLE lt_proc_link
      WHERE proc_ref = ls_proc_data-proc_ref
        AND proc_id = ls_proc_data-proc_id
        AND assoc_proc_id = '/HFQ/UTILTS_SND'.
    IF sy-subrc NE 0.
      APPEND /hfq/cl_utilts_mss=>co_error TO et_check_result.
      "Keine Untergeordneten Prozesse zu Prozess &1 gefunden.
      MESSAGE e020(/hfq/utilts_snd) WITH ls_proc_data-proc_ref.
      RETURN.
    ENDIF.

    IF line_exists( lt_proc_link[ link_status = /idxgc/if_constants_add=>gc_act_status_error ] ).
      APPEND /hfq/cl_utilts_mss=>co_warning TO et_check_result.
    ELSEIF line_exists( lt_proc_link[ link_status = /idxgc/if_constants_add=>gc_proc_status_bpem_case ] ).
      APPEND /hfq/cl_utilts_mss=>co_user_deci TO et_check_result.
    ELSEIF line_exists( lt_proc_link[ link_status = /idxgc/if_constants_add=>gc_act_status_active ] ).
      APPEND /hfq/cl_utilts_mss=>co_active TO et_check_result.
    ELSE.
      LOOP AT lt_proc_link TRANSPORTING NO FIELDS
        WHERE link_status NE /idxgc/if_constants_add=>gc_act_status_ok.
        APPEND /hfq/cl_utilts_mss=>co_error TO et_check_result.
        RETURN.
      ENDLOOP.
      APPEND /hfq/cl_utilts_mss=>co_complete TO et_check_result.
    ENDIF.

  ENDMETHOD.


    METHOD create_new_pdoc.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Die Methode erzeugt ein neues PDOC zum Prozess.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-06-20  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

      DATA: lv_activity        TYPE eideswtact,
            lv_act_var1        TYPE eideswtattrvalue,
            lv_mtext           TYPE string,
            ls_objectreference TYPE eideswtdocrefstruc,
            lt_objectreference TYPE teideswtdocrefstruc.

      DATA: lx_previous     TYPE REF TO /idxgc/cx_general.

      TRY.
          CALL METHOD /idxgc/cl_process_trigger=>start_process
            EXPORTING
              iv_pdoc_display = space
            CHANGING
              cs_process_data = cs_proc_data_new.
        CATCH /idxgc/cx_process_error INTO gr_previous.
          CALL METHOD /idxgc/cx_process_error=>raise_exception_from_msg
            EXPORTING
              ir_previous = gr_previous.
      ENDTRY.

      " Aktivität aktualisieren
      lv_activity = /idxgc/if_constants_add=>gc_activity_i14.
      lv_act_var1 = iv_proc_id.

      " Referenz hinzufügen
      CLEAR: ls_objectreference,lt_objectreference.
      ls_objectreference-object    = /idxgc/if_constants=>gc_object_pdoc_bor.
      ls_objectreference-objectkey = cs_proc_data_new-proc_ref.
      APPEND ls_objectreference TO lt_objectreference.

      TRY.
          CALL METHOD cr_ctx->update_activity
            EXPORTING
              iv_proc_step_no    = iv_proc_step_no
              iv_activity        = lv_activity
              iv_status          = /idxgc/if_constants_add=>gc_act_status_ok
              iv_act_var1        = lv_act_var1
              it_objectreference = lt_objectreference.

          cr_ctx->close_and_save( ).

        CATCH /idxgc/cx_general INTO gr_previous.
          CALL METHOD /idxgc/cx_general=>raise_exception_from_msg
            EXPORTING
              ir_previous = gr_previous.
      ENDTRY.

    ENDMETHOD.


  method GET_CORRESPONDING_MALOS.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Sammelt alle ZP, die dem Haupt-ZP des Prozesses zugeordnet sind
*&            und schreibt diese in die POD-Tabelle der Schrittdaten.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2021-03-19  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

    " Allgemeine Daten definieren
    DATA: ls_step    TYPE /idxgc/s_proc_step_data_all,
          lv_result  TYPE /idxgc/de_check_result,
          lt_adtel   TYPE TABLE OF adtel,
          lv_valdate TYPE sy-datlo.

    DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
          lr_badi_utilts_snd TYPE REF TO /hfq/badi_utilts_snd.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->* TO <fr_proc_data_extern>.
    lr_proc_step_data ?= <fr_proc_data_extern>.

    " spezifische Daten definieren:
    DATA: lt_pod TYPE /idxgc/t_pod_info_details.

    " Prozessdaten lesen:
    TRY.
        ls_step = lr_proc_step_data->get_process_step_data( is_process_step_key ).
      CATCH /idxgc/cx_process_error INTO gr_previous .

        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Datenverarbeitung
*--------------------------------------------------------------------*

    GET BADI lr_badi_utilts_snd.

    TRY.
        CALL BADI lr_badi_utilts_snd->select_corresponding_malos
          EXPORTING
            iv_malo_int_ui = ls_step-int_ui    " Zählpunktbezeichnung
            iv_keydate     = ls_step-proc_date " Datum
          IMPORTING
            et_malo_info   = lt_pod.           " Liste der zugehörigen MeLos

      CATCH /idxgc/cx_general INTO gr_previous.
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

    IF lt_pod IS NOT INITIAL.
      APPEND LINES OF lt_pod TO ls_step-pod.
      "Update der Prozessdaten:
      lr_proc_step_data->update_process_step_data( ls_step ).
      "Erfolg
      APPEND /hfq/cl_utilts_mss=>co_success TO et_check_result.
    ELSE.
      "Fehler: keine MeLos zur MaLo gefunden!
      APPEND /hfq/cl_utilts_mss=>co_empty TO et_check_result.
    ENDIF.
  endmethod.


  METHOD get_corresponding_melos.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Sammelt alle ZP, die dem Haupt-ZP des Prozesses zugeordnet sind
*&            und schreibt diese in die POD-Tabelle der Schrittdaten.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-06-20  Hochfrequenz    angelegt
*&  0002        2020-03-04  Hochfrequenz    allg. Korrekturen
*&-------------------------------------------------------------------*

    " Allgemeine Daten definieren
    DATA: ls_step    TYPE /idxgc/s_proc_step_data_all,
          lv_result  TYPE /idxgc/de_check_result,
          lt_adtel   TYPE TABLE OF adtel,
          lv_valdate TYPE sy-datlo.

    DATA: lr_proc_step_data  TYPE REF TO /idxgc/if_process_data_step,
          lr_badi_utilts_snd TYPE REF TO /hfq/badi_utilts_snd.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->* TO <fr_proc_data_extern>.
    lr_proc_step_data ?= <fr_proc_data_extern>.

    " spezifische Daten definieren:
    DATA: lt_pod TYPE /idxgc/t_pod_info_details.

    " Prozessdaten lesen:
    TRY.
        ls_step = lr_proc_step_data->get_process_step_data( is_process_step_key ).
      CATCH /idxgc/cx_process_error INTO gr_previous .

        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Datenverarbeitung
*--------------------------------------------------------------------*

    GET BADI lr_badi_utilts_snd. "0002

    TRY.

        CALL BADI lr_badi_utilts_snd->select_corresponding_melos "0002
          EXPORTING
            iv_malo_int_ui = ls_step-int_ui    " Zählpunktbezeichnung
            iv_keydate     = ls_step-proc_date " Datum
          IMPORTING
            et_melo_info   = lt_pod.           " Liste der zugehörigen MeLos

      CATCH /idxgc/cx_general INTO gr_previous.
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

    IF lt_pod IS NOT INITIAL.
      APPEND LINES OF lt_pod TO ls_step-pod.
      "Update der Prozessdaten:
      lr_proc_step_data->update_process_step_data( ls_step ).
      "Erfolg
      APPEND /hfq/cl_utilts_mss=>co_success TO et_check_result.
    ELSE.
      "Fehler: keine MeLos zur MaLo gefunden!
      APPEND /hfq/cl_utilts_mss=>co_empty TO et_check_result.
    ENDIF.
  ENDMETHOD.


  METHOD get_serv_prov.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Ermittelt den Marktpartner zum Zählpunkt und gibt
*&            diesen in der /IDXGC/S_SERVPROV_DETAILS zurück.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-06-20  Hochfrequenz    angelegt
*&  0002        2020-03-04  Hochfrequenz    allg. Korrekturen
*&-------------------------------------------------------------------*

    DATA: lt_servprov_info   TYPE /idxgc/t_servprov_info,
          ls_servprov_info   TYPE /idxgc/s_servprov_info,
          lt_serv_type_range TYPE isu_ranges_tab,
          lt_all_services    TYPE iallservices,
          ls_all_services    TYPE  allservices,
          ls_serviceprovider TYPE /idxgc/s_servprov_details,
          lv_duplicate       TYPE kennzx,
          ls_service_prov TYPE /idxgc/s_servprov_details,
          lv_invalid        TYPE kennzx.

    DATA: lr_badi_utilts_snd TYPE REF TO /hfq/badi_utilts_snd.
    DATA: lr_badi_determine_sp TYPE REF TO /idxgc/badi_determine_sp.

    GET BADI lr_badi_utilts_snd.
    GET BADI lr_badi_determine_sp.

    TRY.
        "Servicearten für die Marktrolle bestimmen:
        CALL BADI lr_badi_determine_sp->determine_srvp_at_pod
          EXPORTING
            iv_int_ui        = iv_int_ui
            iv_keydate       = iv_proc_date
            iv_srv_cat       = iv_service_typ
*           iv_meter_srv     =
           iv_srvp_at_keydate = 'X'
           iv_srvp_in_future  = 'X'
          IMPORTING
            et_servprov_info = lt_servprov_info.

      CATCH /idxgc/cx_utility_error.
        APPEND /hfq/cl_utilts_mss=>co_error TO ct_check_result.
        "Fehler bei Aufruf des BAdI &1.
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            iv_msgid          = '/hfq/utilts_snd'
            iv_msgno          = 003
            iv_msgv1          = '/idxgc/badi_determine_sp->determine_srvp_at_pod'
            iv_exception_code = /idxgc/if_constants=>gc_exception_technical_error.
        IF 1 = 2. MESSAGE e003(/hfq/utilts_snd) WITH '/idxgc/badi_determine_sp->determine_srvp_at_pod'. ENDIF.
        RETURN.
    ENDTRY.

    TRY .
        CALL BADI lr_badi_utilts_snd->select_service_types  "0002
          EXPORTING
            iv_service_typ     = iv_service_typ     " Servicetyp
          RECEIVING
            rt_serv_type_range = lt_serv_type_range
          EXCEPTIONS
            no_servtype_found  = 1
            OTHERS             = 2.

        IF sy-subrc <> 0. "Keine Serviceart zum Typ &1 gefunden.
          APPEND /hfq/cl_utilts_mss=>co_error TO ct_check_result.
          CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
            EXPORTING
              iv_msgid          = '/hfq/utilts_snd'
              iv_msgno          = 012
              iv_msgv1          = CONV symsgv( iv_service_typ )
              iv_exception_code = /idxgc/if_constants=>gc_exception_technical_error.
          IF 1 = 2. MESSAGE e012(/hfq/utilts_snd) WITH iv_service_typ. ENDIF.
          RETURN.
        ENDIF.

      CATCH /idxgc/cx_general. "Fehler bei Aufruf des BAdI &1.
        APPEND /hfq/cl_utilts_mss=>co_error TO ct_check_result.
        "Fehler bei Aufruf des BAdI &1.
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            iv_msgid          = '/hfq/utilts_snd'
            iv_msgno          = 003
            iv_msgv1          = '/hfq/badi_utilts_snd->select_service_types'
            iv_exception_code = /idxgc/if_constants=>gc_exception_technical_error.
        IF 1 = 2. MESSAGE e003(/hfq/utilts_snd) WITH '/idxgc/badi_determine_sp->determine_srvp_at_pod'. ENDIF.
        RETURN.
    ENDTRY.

* wir müssen auch mehrere Service-Anbieter informieren können, bei Wechsel in der Zukunft!

    LOOP AT lt_servprov_info INTO ls_servprov_info
      WHERE service IN lt_serv_type_range.

      TRY.
          CALL BADI lr_badi_utilts_snd->is_on_blacklist
            EXPORTING
              iv_servprov = ls_servprov_info-service_id
            RECEIVING
              rv_invalid    = lv_invalid.

          IF lv_invalid = abap_false.
            MOVE-CORRESPONDING ls_servprov_info TO ls_service_prov.

            CALL FUNCTION 'ISU_GET_POD_EXT_UI'
              EXPORTING
                x_pod_int_ui = iv_int_ui
                x_keydate    = iv_proc_date
              IMPORTING
                y_pod_ext_ui = ls_service_prov-ext_ui.

            APPEND ls_service_prov TO et_serviceprovider.
          ENDIF.

        CATCH /idxgc/cx_general. " Fehler bei Aufruf des BAdI.
          APPEND /hfq/cl_utilts_mss=>co_error TO ct_check_result.
          CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
            EXPORTING
              ir_previous = gr_previous.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_serv_prov_list.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Sammelt alle Marktpartner, die einem der ZP zugeordnet sind
*&            und speichert diese in den Schrittdaten
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-06-20  Hochfrequenz    angelegt
*&  0002        2020-03-04  Hochfrequenz    allg. überarbeitet
*&  0003        2020-12-16  Hochfrequenz    Lieferanten hinzugefügt
*&  0004        2021-02-19  Hochfrequenz    BAdI für LIEF/MSB als Empfänger
*&-------------------------------------------------------------------*

    " Allgemeine Daten definieren
    DATA: ls_step           TYPE /idxgc/s_proc_step_data_all,
          lr_proc_step_data TYPE REF TO /idxgc/if_process_data_step.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->* TO <fr_proc_data_extern>.
    lr_proc_step_data ?= <fr_proc_data_extern>.

    " spezifische Daten definieren:
    DATA: lt_serviceid       TYPE TABLE OF serviceid,
          lv_serviceid       TYPE serviceid,
          lt_pod             TYPE TABLE OF /idxgc/s_pod_info_details,
          ls_pod             TYPE /idxgc/s_pod_info_details,
          lt_serv_type_range TYPE isu_ranges_tab,
          lv_serv_art        TYPE sercode,
          lt_servprov_all    TYPE /idxgc/t_servprov_details, "0002
          lt_serviceprovider TYPE /idxgc/t_servprov_details,
          lt_all_services    TYPE iallservices,
          ls_all_services    TYPE  allservices,
          lv_own_services    TYPE kennzx,
          lv_not_to_lief     TYPE kennzx,
          lv_not_to_msb      TYPE kennzx.

    DATA: lr_badi_utilts_snd TYPE REF TO /hfq/badi_utilts_snd.

    " Prozessdaten lesen:
    TRY.
        ls_step = lr_proc_step_data->get_process_step_data( is_process_step_key ).
      CATCH /idxgc/cx_process_error INTO gr_previous .
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

    " 0004
    TRY.
        GET BADI lr_badi_utilts_snd.

        CALL BADI lr_badi_utilts_snd->select_relevant_receiver_cat
          EXPORTING
            is_proc_step_data_all = ls_step
          IMPORTING
            ev_not_to_lief        = lv_not_to_lief
            ev_not_to_msb         = lv_not_to_msb.

      CATCH cx_badi.
        " catch all kind of badi-related exceptions
    ENDTRY.
*--------------------------------------------------------------------*
* Beginn der Datenverarbeitung
*--------------------------------------------------------------------*
    " alle ZP verarbeiten
    lt_pod = ls_step-pod.

    "Lieferanten ermitteln:
    IF lv_not_to_lief = abap_false. " 0004: nur wenn gewünscht

      " prozessierte MaLo in POD enthalten?
      READ TABLE lt_pod TRANSPORTING NO FIELDS
      WITH KEY int_ui = ls_step-int_ui.

      IF sy-subrc <> 0.
        " fehlt noch
        ls_pod-int_ui = ls_step-int_ui.
        ls_pod-ext_ui = ls_step-ext_ui.
        ls_pod-pod_type = /idxgc/if_constants_ide=>gc_pod_type_z30.  " 'Z30' = MaLo

        APPEND ls_pod TO lt_pod.
      ENDIF.

      LOOP AT lt_pod INTO ls_pod
        WHERE pod_type NE /idxgc/if_constants_ide=>gc_pod_type_z31.    " keine 'Z31' MeLo, also MaLo oder Tranche
        CLEAR: lt_serviceprovider.

        TRY.  "0003
            CALL METHOD /hfq/cl_utilts_mss=>get_serv_prov
              EXPORTING
                iv_service_typ     = /hfq/if_badi_utilts_snd=>gc_intcode_lief " '02' ... Lieferant
                iv_int_ui          = ls_pod-int_ui
                iv_proc_date       = ls_step-proc_date
              IMPORTING
                et_serviceprovider = lt_serviceprovider
              CHANGING
                ct_check_result    = et_check_result.

          CATCH /idxgc/cx_utility_error.
            IF et_check_result IS INITIAL.
              APPEND /hfq/cl_utilts_mss=>co_error TO et_check_result.
              RETURN.
            ENDIF.

        ENDTRY.

        IF lt_serviceprovider IS NOT INITIAL. "0003
          APPEND LINES OF lt_serviceprovider TO lt_servprov_all.
        ENDIF.
      ENDLOOP.

    ENDIF. " 0004: LIEF als Empfänger
*--------------------------------------------------------------------*

    IF lv_not_to_msb = abap_false. " 0004 nur wenn gewünscht
      CLEAR ls_pod.
      "Alle MSB der ZP müssen bestimmt werden:
      LOOP AT lt_pod INTO ls_pod
        WHERE pod_type = /idxgc/if_constants_ide=>gc_pod_type_z31.    " 'Z31' MeLo
        CLEAR: lt_serviceprovider.

        "MSBs ermitteln:
        TRY.  "0003
            CALL METHOD /hfq/cl_utilts_mss=>get_serv_prov
              EXPORTING
                iv_service_typ     = /hfq/if_badi_utilts_snd=>gc_intcode_msb   " 'M1' ... MSB
                iv_int_ui          = ls_pod-int_ui
                iv_proc_date       = ls_step-proc_date
              IMPORTING
                et_serviceprovider = lt_serviceprovider
              CHANGING
                ct_check_result    = et_check_result.

          CATCH /idxgc/cx_utility_error.
            IF et_check_result IS INITIAL.
              APPEND /hfq/cl_utilts_mss=>co_error TO et_check_result.
              RETURN.
            ENDIF.

        ENDTRY.

        IF lt_serviceprovider IS NOT INITIAL. "0002
          APPEND LINES OF lt_serviceprovider TO lt_servprov_all.
        ENDIF.
      ENDLOOP.
    ENDIF. " 0004: MSB als Empfänger
*--------------------------------------------------------------------*

    SORT lt_servprov_all BY service_id date_from. "0002
    DELETE ADJACENT DUPLICATES FROM lt_servprov_all
           COMPARING service_id. "0002

    CLEAR lv_own_services.
    " Löschen der Serviceanbieter im eigenen System:
    LOOP AT lt_servprov_all ASSIGNING FIELD-SYMBOL(<fs_serviceprovider>)
      WHERE own_service = abap_true.
      lv_own_services = abap_true.
      DELETE lt_servprov_all.
      CONTINUE.
    ENDLOOP.

    IF lt_servprov_all IS NOT INITIAL.
      ls_step-serviceprovider = lt_servprov_all.

      "Update der Prozessdaten:
      lr_proc_step_data->update_process_step_data( ls_step ).
      "Erfolg
      APPEND /hfq/cl_utilts_mss=>co_success TO et_check_result.
    ELSEIF lv_own_services = abap_true.
      "Nur eigener Serviceanbieter, kein Versand.
      APPEND /hfq/cl_utilts_mss=>co_obsolete TO et_check_result.
    ELSE.
      "Fehler: keine Daten gefunden!
      APPEND /hfq/cl_utilts_mss=>co_empty TO et_check_result.
    ENDIF.

  ENDMETHOD.


  METHOD inform_serv_prov.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung:  Erzeugt für jeden zuständigen Marktpartner einen neuen Prozess
*&            vom Typ /HFQ/UTILTS_SND, welcher die UTILTS verschickt.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-06-24  Hochfrequenz    angelegt
*&  0002        2020-03-04  Hochfrequenz    allg. Korrekturen
*&-------------------------------------------------------------------*

    " Allgemeine Daten definieren
    DATA: ls_step_old       TYPE /idxgc/s_proc_step_data_all,
          ls_step           TYPE /idxgc/s_proc_step_data,
          lr_proc_step_data TYPE REF TO /idxgc/if_process_data_step,
          ls_proc_data_old  TYPE /idxgc/s_proc_data,
          ls_proc_data_new  TYPE /idxgc/s_proc_data,
          lr_ctx            TYPE REF TO /idxgc/cl_pd_doc_context.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->* TO <fr_proc_data_extern>.
    lr_proc_step_data ?= <fr_proc_data_extern>.

    " spezifische Daten definieren:
    FIELD-SYMBOLS: <ls_serviceprov> TYPE /idxgc/s_servprov_details.

    DATA: lt_serviceprovider TYPE /idxgc/t_servprov_details,
          lv_proc_date_upd   TYPE /idxgc/de_proc_date,
          lv_serviceid       TYPE serviceid,
          ls_marketpartner   TYPE /idxgc/s_markpar_details,
          ls_proc_link       TYPE /idxgc/s_proc_link.

    " Prozessdaten lesen:
    TRY.
        ls_step_old = lr_proc_step_data->get_process_step_data( is_process_step_key ).
      CATCH /idxgc/cx_process_error INTO gr_previous.
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

    CALL METHOD <fr_proc_data_extern>->get_process_data
      IMPORTING
        es_process_data = ls_proc_data_old.

*--------------------------------------------------------------------*
* Kontext-Instanz generieren
*--------------------------------------------------------------------*
    TRY .
        CALL METHOD /idxgc/cl_pd_doc_context=>get_instance
          EXPORTING
            iv_pdoc_no = ls_proc_data_old-proc_ref
            iv_wmode   = cl_isu_wmode=>co_change
          RECEIVING
            rr_ctx     = lr_ctx.
      CATCH /idxgc/cx_process_error INTO gr_previous.
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Datenverarbeitung
*--------------------------------------------------------------------*

    "Marktpartner disjunken:
    lt_serviceprovider = ls_step_old-serviceprovider.
    SORT lt_serviceprovider BY service_id date_from.
    DELETE ADJACENT DUPLICATES FROM lt_serviceprovider COMPARING service_id.

    "Für jeden Marktpartner einen Prozess anstoßen:
    LOOP AT lt_serviceprovider ASSIGNING <ls_serviceprov>.
      IF <ls_serviceprov> IS ASSIGNED.
        CLEAR: ls_marketpartner.
        lv_serviceid = <ls_serviceprov>-service_id.

*--------------------------------------------------------------------*
* Befüllung neue PDOC Struktur:
*--------------------------------------------------------------------*
        ls_proc_data_new = ls_proc_data_old.
        CLEAR: ls_proc_data_new-proc_ref,
               ls_proc_data_new-proc_type,
               ls_proc_data_new-proc_view,
               ls_proc_data_new-status,
               ls_proc_data_new-proc_id,
               ls_proc_data_new-proc_version,
               ls_proc_data_new-proc_uid,
               ls_proc_data_new-proc_cluster,
               ls_proc_data_new-step_links,
               ls_proc_data_new-steps
               .
        "0002
        ls_proc_data_new-proc_date = /hfq/cl_utilts_mss=>update_proc_date(
                                     EXPORTING
                                       iv_proc_date_old      = ls_proc_data_old-proc_date
                                       iv_start_date_service = <ls_serviceprov>-date_from
                                     ).
*--------------------------------------------------------------------*
* Neuer Prozess mit Initalschritt:
        ls_proc_data_new-proc_id = co_utilts_snd_proc_id.
        " Prozess-Konfig initial befüllen
        SELECT SINGLE proc_type, proc_view, spartyp FROM /idxgc/proc    ##WARN_OK
          INTO ( @ls_proc_data_new-proc_type, @ls_proc_data_new-proc_view,
                 @ls_proc_data_new-spartyp )
          WHERE proc_id = @ls_proc_data_new-proc_id
          AND   active  = @abap_true.

        IF sy-subrc <> 0.
          CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
            EXPORTING
              iv_msgid = '/HFQ/CL_UTILTS_SND'
              iv_msgno = '008'
              iv_msgv1 = CONV #( ls_proc_data_new-proc_id ).
        ENDIF.
*>>>
*--------------------------------------------------------------------*
* initiale Schrittdaten
*--------------------------------------------------------------------*
        ls_step-proc_step_no = co_utilts_snd_init_step.
        ls_step-pod = ls_step_old-pod.

        " ext_ui abhängig von Empfänger-Art
        IF receiver_is_lief( <ls_serviceprov>-service_id ) = abap_true.
          ls_step-ext_ui = <ls_serviceprov>-ext_ui.
        ELSE.
          " Empfänger ist MSB
          ls_step-ext_ui = ls_step_old-ext_ui.
        ENDIF.

        ls_step-own_servprov = ls_step_old-own_servprov.
        ls_step-assoc_servprov = lv_serviceid.

        " Gültigkeit der Formel
        IF ls_proc_data_new-proc_date = ls_proc_data_old-proc_date.
          ls_step-date_from = ls_step_old-date_from.
          ls_step-time_from = ls_step_old-time_from.
        ELSE.
          " verschobene Gültigkeit bei späterem Beginn des Service
          " gültig ab auf neues PROC_DATE und Uhrzeit 00:00:00
          ls_step-date_from = ls_proc_data_new-proc_date.
          ls_step-time_from = /idxgc/if_constants=>gc_time_midnight.
        ENDIF.

* Marktpartner:
        " Empfänger:
        SELECT SINGLE serviceid, externalid, externalidtyp FROM eservprov
          WHERE serviceid = @lv_serviceid
          INTO (@ls_marketpartner-serviceid, @ls_marketpartner-party_identifier, @ls_marketpartner-codelist_agency).
        IF sy-subrc = 0.
          ls_marketpartner-party_func_qual = /idxgc/if_constants_ide=>gc_nad_qual_mr.
          APPEND ls_marketpartner TO ls_step-marketpartner.
          ls_proc_data_new-service_prov_new = ls_marketpartner-serviceid.
          CLEAR: ls_marketpartner.
        ENDIF.
        " Sender
        SELECT SINGLE serviceid, externalid, externalidtyp FROM eservprov
          WHERE serviceid = @ls_step-own_servprov
          INTO (@ls_marketpartner-serviceid, @ls_marketpartner-party_identifier, @ls_marketpartner-codelist_agency).
        IF sy-subrc = 0.
          ls_marketpartner-party_func_qual = /idxgc/if_constants_ide=>gc_nad_qual_ms.
          APPEND ls_marketpartner TO ls_step-marketpartner.
          ls_proc_data_new-distributor = ls_marketpartner-serviceid.
          CLEAR: ls_marketpartner.
        ENDIF.

*--------------------------------------------------------------------*
        APPEND ls_step TO ls_proc_data_new-steps.

*--------------------------------------------------------------------*
* Prozessverknüpfung setzen:
        ls_proc_link-trigger_step = ls_step_old-proc_step_ref.
        ls_proc_link-proc_id = ls_proc_data_old-proc_id.
        ls_proc_link-assoc_proc_id = co_utilts_snd_proc_id.  " SND_PROC_ID
        ls_proc_link-proc_ref = ls_proc_data_old-proc_ref.
        ls_proc_link-link_type = 'B'.
        APPEND ls_proc_link TO ls_proc_data_new-process_links.
*--------------------------------------------------------------------*

        TRY.
            CALL METHOD /hfq/cl_utilts_mss=>create_new_pdoc
              EXPORTING
                iv_proc_id       = co_utilts_snd_proc_id
                iv_proc_step_no  = co_utilts_snd_init_step
              CHANGING
                cs_proc_data_new = ls_proc_data_new
                cr_ctx           = lr_ctx.

            APPEND /hfq/cl_utilts_mss=>co_success TO et_check_result.

            lr_ctx->close_and_save( ).

          CATCH /idxgc/cx_process_error INTO gr_previous.
            APPEND /hfq/cl_utilts_mss=>co_error TO et_check_result.
            RETURN.
          CATCH /idxgc/cx_general INTO gr_previous.
            MESSAGE e044(/idxgc/process_add) WITH co_utilts_snd_proc_id
                                                  ls_proc_data_old-proc_ref.
            lr_ctx->add_message_log( ).
            lr_ctx->close( ).
            MESSAGE e044(/idxgc/process_add) WITH co_utilts_snd_proc_id
                                                  ls_proc_data_old-proc_ref.

            APPEND /hfq/cl_utilts_mss=>co_error TO et_check_result.
            RETURN.
        ENDTRY.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD provide_intui_from_extui.

    CHECK iv_ext_ui IS NOT INITIAL
      AND iv_keydate IS NOT INITIAL.

    SELECT SINGLE int_ui FROM euitrans
      INTO @DATA(lv_int_ui)
      WHERE ext_ui EQ @iv_ext_ui
        AND datefrom LE @iv_keydate
        AND dateto   GE @iv_keydate.

    IF sy-subrc = 0.
      rv_int_ui = lv_int_ui.
    ENDIF.
  ENDMETHOD.


  METHOD receiver_is_lief.
    DATA: ls_eservprov TYPE eservprov,
          ls_tecde     TYPE tecde.

    CHECK iv_receiver IS NOT INITIAL.

* get service provider
    CALL FUNCTION 'ISU_DB_ESERVPROV_SINGLE'
      EXPORTING
        x_serviceid = iv_receiver
      IMPORTING
        y_eservprov = ls_eservprov
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'ISU_DB_TECDE_SINGLE'
      EXPORTING
        x_service    = ls_eservprov-service
      IMPORTING
        y_service    = ls_tecde
      EXCEPTIONS
        not_found    = 1
        system_error = 2
        OTHERS       = 3.

    CHECK sy-subrc = 0.

    IF ls_tecde-intcode = /idxgc/if_constants_add=>gc_srvcat_supp.  " '02' - LIEF
      rv_is_lief = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD update_proc_date.

    " Formelversand erst ab Beginn des Service relevant
    " Service ist älter als Versand
    IF iv_proc_date_old > iv_start_date_service.
      rv_proc_date_upd = iv_proc_date_old.

      " Service ist jünger als Versand
    ELSE.
      rv_proc_date_upd = iv_start_date_service.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
