class /HFQ/CL_UTILTS_SND definition
  public
  final
  create public .

public section.

  class-data GR_PREVIOUS type ref to /IDXGC/CX_GENERAL .
  constants CO_WAIT type /IDXGC/DE_CHECK_RESULT value 'WAIT' ##NO_TEXT.
  constants CO_START type /IDXGC/DE_CHECK_RESULT value 'START' ##NO_TEXT.

  class-methods CHECK_CORRESPONDING_MELOS
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
  class-methods CHECK_FLG_VERSAND_ASYNC
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
  class-methods CHECK_ANSWER_STATUS
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

  constants CO_OBSOLET type /IDXGC/DE_CHECK_RESULT value 'OBSOLETE' ##NO_TEXT.
private section.
ENDCLASS.



CLASS /HFQ/CL_UTILTS_SND IMPLEMENTATION.


  method CHECK_ANSWER_STATUS.
*{   INSERT         EN1K921276                                        1
  DATA:
      lv_result             TYPE /idxgc/de_check_result,
      ls_proc_step_data_all TYPE /idxgc/s_proc_step_data_all,
      ls_process_data       TYPE /idxgc/s_proc_data,
*      ls_ref_to_msg         TYPE /idxgc/s_ref2msg_details,
*      ls_euitrans           TYPE euitrans,
      lr_process_data_step  TYPE REF TO /idxgc/if_process_data_step,
      lr_process_data       TYPE REF TO /idxgc/if_process_data,
*      ls_ref2msg_details    TYPE /idxgc/s_ref2msg_details,
*      ls_msconsref          TYPE /idxgc/msconsref,
*      ls_objectref          TYPE eideswtdocref,
*      lv_ext_ui             TYPE ext_ui,
*      ls_activity_data      TYPE /idxgc/s_activity_data,
*      ls_attribute          TYPE eideswtdocattr,
*      lt_engy_val           TYPE /idxgl/t_engy_val,
*      lr_access_engyamt     TYPE REF TO /idxgl/cl_access_engyamt,
*      lr_fact_engyamt       TYPE REF TO /idxgl/cl_fact_engyamt,
*      lv_bill_doc_no        TYPE e_belnr,
*      lv_dlv_note_status    TYPE /idxgl/de_dlv_note_status,
*      lt_erchc              TYPE STANDARD TABLE OF erchc,
*      ls_bill_doc           TYPE isu2a_bill_doc
      lx_previous           TYPE REF TO /idxgc/cx_general.

    FIELD-SYMBOLS:
      <lr_ref_process_data_extern> TYPE REF TO /idxgc/if_process_data_extern,
      <lr_ref_process_log>         TYPE REF TO /idxgc/if_process_log.

    ASSIGN cr_data->* TO <lr_ref_process_data_extern>.
    ASSIGN cr_data_log->* TO <lr_ref_process_log>.

    IF <lr_ref_process_data_extern> IS ASSIGNED.
      lr_process_data_step ?= <lr_ref_process_data_extern>.
      lr_process_data ?= <lr_ref_process_data_extern>.
    ELSE.
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
* Get process step data
    TRY.
        CALL METHOD lr_process_data_step->get_process_step_data
          EXPORTING
            is_process_step_key  = is_process_step_key
          RECEIVING
            rs_process_step_data = ls_proc_step_data_all.

        CALL METHOD <lr_ref_process_data_extern>->get_process_data
          IMPORTING
            es_process_data = ls_process_data.
      CATCH /idxgc/cx_process_error INTO lx_previous.
        IF <lr_ref_process_log> IS ASSIGNED.
          <lr_ref_process_log>->add_message_to_process_log(
            EXPORTING
              is_process_step_key =  is_process_step_key   " Proc. Key: (Dynamc) Proc. Step Ref. + (Static) Proc. Step ID
          ).
        ENDIF.
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = lx_previous ).
    ENDTRY.
*--------------------------------------------------------------------*


    LOOP AT ls_proc_step_data_all-msgrespstatus ASSIGNING FIELD-SYMBOL(<fs_msgresp>).
      CASE <fs_msgresp>-/idxgl/status_reason.
        WHEN /idxgl/if_constants_ide=>gc_status_reason_a08
          OR /idxgl/if_constants_ide=>gc_status_reason_a09
          OR /idxgl/if_constants_ide=>gc_status_reason_a11. "Zustimmung
          lv_result = /idxgm/if_constants=>gc_cr_confirm.
        WHEN OTHERS. " Ablehnung, sobald ein Eintrag diese enthält aussteigen.
          lv_result = /idxgm/if_constants=>gc_cr_reject.
          EXIT.
      ENDCASE.
    ENDLOOP.

    IF lv_result is not initial.
      APPEND lv_result TO et_check_result.
    ELSE.
      APPEND /idxgm/if_constants=>gc_cr_confirm TO et_check_result.
    ENDIF.

*}   INSERT
  endmethod.


  METHOD check_corresponding_melos.
*&-------------------------------------------------------------------*
*&  Autor: Hochfrequenz
*&  Nutzung: Prüft ob der Prozessdatencontainer zu den ZP schon befüllt ist.
*            Falls nicht, wird versucht, diesen zu befüllen.
*&-------------------------------------------------------------------*
*&  Versionen:  Datum       Bearbeiter      Kommentar
*&              2019-08-01  Hochfrequenz    angelegt
*&-------------------------------------------------------------------*

    " Allgemeine Daten definieren
    DATA: ls_step    TYPE /idxgc/s_proc_step_data_all,
          lt_result  TYPE TABLE OF /idxgc/de_check_result,
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

    " Spezifische Daten definieren:
    DATA: lv_malo_ext_ui TYPE ext_ui,
          lt_calc_form   TYPE TABLE OF /hfq/calc_form,
          lt_melo_ext_ui TYPE TABLE OF ext_ui,
          lv_melo_ext_ui TYPE ext_ui,
          ls_pod         TYPE /idxgc/s_pod_info_details,
          lt_pod         TYPE TABLE OF /idxgc/s_pod_info_details.

    " Prozess zwischenspeichern:
    COMMIT WORK AND WAIT.

    " Prozessdaten lesen:
    TRY.
        ls_step = lr_proc_step_data->get_process_step_data( is_process_step_key ).
      CATCH /idxgc/cx_process_error INTO gr_previous .
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

*--------------------------------------------------------------------*
* Beginn der Datenverarbeitung
*--------------------------------------------------------------------*
    IF ls_step-pod IS INITIAL.
      " Noch nicht befüllt, also nachholen.
      " Sollte nur bei Einzelprozessen (ohne MSS) der Fall sein!
      TRY.
          CALL METHOD /hfq/cl_utilts_mss=>get_corresponding_melos
            EXPORTING
              is_process_step_key = is_process_step_key
            IMPORTING
              et_check_result     = lt_result
            CHANGING
              cr_data             = cr_data
              cr_data_log         = cr_data_log.
        CATCH /idxgc/cx_utility_error INTO gr_previous.
          CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
            EXPORTING
              ir_previous = gr_previous.
      ENDTRY.
      APPEND LINES OF lt_result TO et_check_result.
    ELSE.
      " Schon vorhanden, obsolet
      APPEND /hfq/cl_utilts_snd=>co_obsolet TO et_check_result.
    ENDIF.


  ENDMETHOD.


  METHOD check_flg_versand_async.
*" Allgemeine Daten definieren
    DATA: ls_step          TYPE /idxgc/s_proc_step_data_all.
    DATA: lv_has_wait_step TYPE kennzx.

    DATA: lr_proc_step_data      TYPE REF TO /idxgc/if_process_data_step.
    DATA: go_badi_hfq_utilts_snd TYPE REF TO /hfq/badi_utilts_snd.

    FIELD-SYMBOLS: <fr_process_log>      TYPE REF TO /idxgc/if_process_log,
                   <fr_proc_data_extern> TYPE REF TO /idxgc/if_process_data_extern.

    " Referenzen und Log zuordnen:
    ASSIGN cr_data_log->* TO <fr_process_log>.
    ASSIGN cr_data->*     TO <fr_proc_data_extern>.
    lr_proc_step_data     ?= <fr_proc_data_extern>.

    " Prozessdaten lesen:
    TRY.
        ls_step = lr_proc_step_data->get_process_step_data( is_process_step_key ).
      CATCH /idxgc/cx_process_error INTO gr_previous .
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg
          EXPORTING
            ir_previous = gr_previous.
    ENDTRY.

    TRY .
        GET BADI go_badi_hfq_utilts_snd.
        CALL BADI go_badi_hfq_utilts_snd->has_wait_step
          EXPORTING
            is_step          = ls_step
          RECEIVING
            rv_has_wait_step = lv_has_wait_step.                 " Kennzeichen
*    CATCH .
    ENDTRY.

    IF lv_has_wait_step EQ abap_true.
      APPEND /hfq/cl_utilts_snd=>co_wait TO et_check_result.
    ELSE.
      APPEND /hfq/cl_utilts_snd=>co_start TO et_check_result.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
