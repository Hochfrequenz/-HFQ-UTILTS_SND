class /HFQ/CL_UTILTS_CREATE definition
  public
  create public .

public section.

  class-methods IS_PROCESS_STEP_TRIGGERED
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    returning
      value(RS_PROCESS_STEP_TRIGGERED) type /IDXGC/S_PROC_STEP_DATA_ALL .
  class-methods TRIGGER_CHECK
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ET_CHECK_RESULT type /IDXGC/T_CHECK_RESULT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    exceptions
      /IDXGC/CX_UTILITY_ERROR .
  class-methods GET_CURRENT_PROCESS_STEP_DATA
    importing
      !IS_PROCESS_STEP_KEY type /IDXGC/S_PROC_STEP_KEY
    exporting
      !ES_PROC_STEP_DATA_ALL type /IDXGC/S_PROC_STEP_DATA_ALL
      !ER_CTX type ref to /IDXGC/CL_PD_DOC_CONTEXT
    changing
      !CR_DATA type DATA
      !CR_DATA_LOG type DATA
    raising
      /IDXGC/CX_UTILITY_ERROR
      /IDXGC/CX_PROCESS_ERROR .
  class-methods PREPARE_PDOC
    importing
      !IV_INT_UI type INT_UI
      !IV_PROC_DATE type /IDXGC/DE_PROC_DATE
      !IV_RECEIVER type SERVICEID optional
      !IS_SOURCE_STEP_DATA_ALL type /IDXGC/S_PROC_STEP_DATA_ALL optional
    raising
      /IDXGC/CX_UTILITY_ERROR .
protected section.

  constants CO_UTILTS_MSS_PROC_ID type /IDXGC/DE_PROC_ID value '/HFQ/UTILTS_MSS' ##NO_TEXT.
  constants CO_UTILTS_PROC_TYPE type CHAR02 value '94' ##NO_TEXT.
  constants CO_UTILTS_MSS_INIT_STEP type /IDXGC/DE_PROC_STEP_NO value '0010' ##NO_TEXT.
  constants CO_ERROR type /IDXGC/DE_CHECK_RESULT value 'ERROR' ##NO_TEXT.
  constants CO_SUCCESS type /IDXGC/DE_CHECK_RESULT value 'SUCCESS' ##NO_TEXT.
  constants CO_NOT_RELEVANT type /IDXGC/DE_CHECK_RESULT value 'NOT_RELEVANT' ##NO_TEXT.

  class-methods CREATE_NEW_PDOC
    importing
      !IR_CTX type ref to /IDXGC/CL_PD_DOC_CONTEXT
      !IV_INIT_PROC_STEP type /IDXGC/DE_PROC_STEP_NO
    changing
      !CS_PROC_DATA_NEW type /IDXGC/S_PROC_DATA
    raising
      /IDXGC/CX_PROCESS_ERROR .
  class-methods FILL_UP_NEW_PROCESS_DATA
    importing
      !IS_PROCESS_STEP_DATA_OLD type /IDXGC/S_PROC_STEP_DATA_ALL optional
      !IV_RECEIVER type SERVICEID optional
      !IV_INT_UI type INT_UI
      !IV_PROC_DATE type /IDXGC/DE_PROC_DATE
    returning
      value(RS_PROC_DATA_NEW) type /IDXGC/S_PROC_DATA
    raising
      /IDXGC/CX_UTILITY_ERROR .
  class-methods GET_SERVPROV_DETAILS
    importing
      !IV_RECEIVER type SERVICEID
      !IV_INT_UI type INT_UI
      !IV_PROC_DATE type /IDXGC/DE_PROC_DATE
      !IV_EXT_UI type EXT_UI
    returning
      value(RS_SERVICEPROVIDER) type /IDXGC/S_SERVPROV_DETAILS
    raising
      /IDXGC/CX_UTILITY_ERROR .
private section.
ENDCLASS.



CLASS /HFQ/CL_UTILTS_CREATE IMPLEMENTATION.


  METHOD create_new_pdoc.

    TRY.
        CALL METHOD /idxgc/cl_process_trigger=>start_process
          EXPORTING
            iv_pdoc_display = space
          CHANGING
            cs_process_data = cs_proc_data_new.
        CATCH /idxgc/cx_process_error INTO DATA(lx_previous).
          /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = lx_previous ).
    ENDTRY.

    TRY.
        CALL METHOD ir_ctx->update_activity
          EXPORTING
            iv_proc_step_no    = iv_init_proc_step
            iv_activity        = /idxgc/if_constants_add=>gc_activity_i14
            iv_status          = /idxgc/if_constants_add=>gc_act_status_ok
            iv_act_var1        = CONV #( cs_proc_data_new-proc_id )
            it_objectreference = VALUE #(  ( object    = /idxgc/if_constants=>gc_object_pdoc_bor
                                             objectkey = cs_proc_data_new-proc_ref  ) ).
      CATCH /idxgc/cx_process_error INTO lx_previous.
          /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = lx_previous ).
    ENDTRY.
    ir_ctx->close_and_save( ).
  ENDMETHOD.


  METHOD fill_up_new_process_data.

    DATA: ls_process_step_data_new TYPE /idxgc/s_proc_step_data,
          ls_marketpartner         TYPE /idxgc/s_markpar_details,
          ls_serviceprovider       TYPE /idxgc/s_servprov_details.


*--------------------------------------------------------------------*
* Schrittdaten befüllen:
    ls_process_step_data_new = VALUE #( date_from     = iv_proc_date
                                        time_from     = /idxgc/if_constants_add=>gc_time_finite
                                        proc_step_no  = co_utilts_mss_init_step ).

    "externe Bezeichnung Zählpunkt:
    CALL FUNCTION 'ISU_GET_POD_EXT_UI'
      EXPORTING
        x_pod_int_ui = iv_int_ui
        x_keydate    = iv_proc_date
      IMPORTING
        y_pod_ext_ui = ls_process_step_data_new-ext_ui.

*--------------------------------------------------------------------*
* Empfänger befüllen, falls definiert:
    IF iv_receiver IS NOT INITIAL.
      CLEAR: ls_marketpartner, ls_serviceprovider.

      SELECT SINGLE serviceid, externalid, externalidtyp FROM eservprov
        WHERE serviceid = @iv_receiver
        INTO (@ls_marketpartner-serviceid, @ls_marketpartner-party_identifier, @ls_marketpartner-codelist_agency).
      IF sy-subrc = 0.
        ls_marketpartner-party_func_qual = /idxgc/if_constants_ide=>gc_nad_qual_mr.
        ls_process_step_data_new-assoc_servprov = ls_marketpartner-serviceid.
        APPEND ls_marketpartner TO ls_process_step_data_new-marketpartner.
      ENDIF.

      TRY.
          CALL METHOD get_servprov_details
            EXPORTING
              iv_receiver        = iv_receiver
              iv_ext_ui          = ls_process_step_data_new-ext_ui
              iv_int_ui          = iv_int_ui
              iv_proc_date       = iv_proc_date
            RECEIVING
              rs_serviceprovider = ls_serviceprovider.
        CATCH /idxgc/cx_utility_error INTO DATA(lx_previous).
          /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = lx_previous ).
      ENDTRY.
      APPEND ls_serviceprovider TO ls_process_step_data_new-serviceprovider.
    ENDIF.

*--------------------------------------------------------------------*
    " eigener Serviceanbieter = VNB
    CALL FUNCTION '/ISIDEX/ISU_SWD_DISTR_SERVPROV'
      EXPORTING
        x_pod         = iv_int_ui
        x_swtview     = '01'
        x_swtdate     = iv_proc_date
      IMPORTING
        y_distributor = ls_process_step_data_new-own_servprov " Serviceanbieter Verteilnetzbetreiber
      EXCEPTIONS
        not_found     = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
        EXPORTING
          iv_msgid          = sy-msgid
          iv_msgno          = sy-msgno
          iv_msgv1          = sy-msgv1
          iv_msgv2          = sy-msgv2
          iv_msgv3          = sy-msgv3
          iv_msgv4          = sy-msgv4.
    ENDIF.

*--------------------------------------------------------------------*
* Prozessdaten befüllen:

    rs_proc_data_new  = VALUE #( proc_id     = co_utilts_mss_proc_id
                                 proc_type   = co_utilts_proc_type
                                 proc_date   = iv_proc_date
                                 int_ui      = iv_int_ui
                                 distributor = ls_process_step_data_new-own_servprov
                                 steps = VALUE #( ( ls_process_step_data_new ) )
                                ).

    SELECT SINGLE proc_type, proc_view, spartyp FROM /idxgc/proc
      WHERE proc_id = @co_utilts_mss_proc_id
        AND active = @abap_true
       INTO ( @rs_proc_data_new-proc_type, @rs_proc_data_new-proc_view, @rs_proc_data_new-spartyp ).
    IF sy-subrc NE 0.
      "Prozesskonfiguration zu Prozess &1 nicht gefunden.
      CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg
        EXPORTING
          iv_msgid = '/hfq/utilts_snd'
          iv_msgno = 008
          iv_msgv1 = CONV #( co_utilts_mss_proc_id ).
    ENDIF.

    IF iv_receiver IS NOT INITIAL.
      rs_proc_data_new-service_prov_new = iv_receiver.
    ENDIF.

  ENDMETHOD.


  METHOD get_current_process_step_data.

    FIELD-SYMBOLS: <fs_ref_process_data_extern> TYPE REF TO /idxgc/if_process_data_extern,
                   <fs_ref_process_log>         TYPE REF TO /idxgc/if_process_log.

    ASSIGN cr_data->*      TO <fs_ref_process_data_extern>.
    ASSIGN cr_data_log->*  TO <fs_ref_process_log>.

    TRY.
        CALL METHOD <fs_ref_process_data_extern>->get_process_step_data
          EXPORTING
            is_process_step_key  = is_process_step_key
          RECEIVING
            rs_process_step_data = es_proc_step_data_all.

      CATCH /idxgc/cx_process_error INTO DATA(lx_previous).
        <fs_ref_process_log>->add_message_to_process_log( ).
        CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = lx_previous ).
    ENDTRY.

  ENDMETHOD.


  method GET_SERVPROV_DETAILS.

    DATA: lt_all_services    TYPE iallservices.

    CALL FUNCTION 'ISU_ALL_SERVICES_SELECT'
        EXPORTING
          x_int_pod               = iv_int_ui
          x_keydate               = iv_proc_date
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

      LOOP AT lt_all_services ASSIGNING FIELD-SYMBOL(<fs_allservices>)
        WHERE serviceid = iv_receiver.

        rs_serviceprovider = VALUE #(  service_id = <fs_allservices>-serviceid
                                       service    = <fs_allservices>-service
                                       date_from  = <fs_allservices>-service_start
                                       ext_ui = iv_ext_ui ).
      ENDLOOP.

  endmethod.


  METHOD is_process_step_triggered.

    DATA: ls_proc_step_key TYPE /idxgc/s_proc_step_key.

    SELECT * FROM /hfq/utilts_trig
      INTO TABLE @DATA(lt_proc_trig)
      WHERE proc_id = @is_process_step_key-proc_id
        AND trigger_flag = 'X'.

    LOOP AT lt_proc_trig ASSIGNING FIELD-SYMBOL(<fs_proc_trig>).
      ls_proc_step_key = VALUE #( proc_id       = is_process_step_key-proc_id
                                  proc_ref      = is_process_step_key-proc_ref
                                  proc_step_no  = <fs_proc_trig>-proc_step_no
                                ).
      TRY.
          CALL METHOD get_current_process_step_data
            EXPORTING
              is_process_step_key   = ls_proc_step_key
            IMPORTING
              es_proc_step_data_all = rs_process_step_triggered
            CHANGING
              cr_data               = cr_data
              cr_data_log           = cr_data_log.
        CATCH /idxgc/cx_utility_error.
        CATCH /idxgc/cx_process_error INTO DATA(lx_previous).
      ENDTRY.

      IF rs_process_step_triggered IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDLOOP.



  ENDMETHOD.


  METHOD prepare_pdoc.

    DATA: lr_ctx TYPE REF TO /idxgc/cl_pd_doc_context.

    TRY .
        CALL METHOD /idxgc/cl_pd_doc_context=>get_instance
          EXPORTING
            iv_pdoc_no = is_source_step_data_all-proc_ref
            iv_wmode   = cl_isu_wmode=>co_change
          RECEIVING
            rr_ctx     = lr_ctx.
      CATCH /idxgc/cx_process_error INTO DATA(lx_previous).
        /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = lx_previous ).
    ENDTRY.

    TRY.
        DATA(ls_proc_data_new) = fill_up_new_process_data(
                                   is_process_step_data_old = is_source_step_data_all
                                   iv_receiver              = iv_receiver
                                   iv_int_ui                = iv_int_ui
                                   iv_proc_date             = iv_proc_date ).
      CATCH /idxgc/cx_utility_error INTO DATA(lx_utl_previous).
        /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = lx_utl_previous ).
    ENDTRY.

    READ TABLE ls_proc_data_new-steps INDEX 1 INTO DATA(ls_init_step).
    IF sy-subrc NE 0.
      /idxgc/cx_utility_error=>raise_util_exception_from_msg( ).
    ENDIF.

    TRY.
        CALL METHOD create_new_pdoc
          EXPORTING
            ir_ctx            = lr_ctx
            iv_init_proc_step = ls_init_step-proc_step_no
          CHANGING
            cs_proc_data_new  = ls_proc_data_new.

      CATCH /idxgc/cx_process_error.
        /idxgc/cx_utility_error=>raise_util_exception_from_msg( ).
      CATCH /idxgc/cx_general INTO DATA(lx_general).
        lr_ctx->close( ).
        /idxgc/cx_utility_error=>raise_util_exception_from_msg(
                                    EXPORTING
                                      iv_msgid          = '/idxgc/process_add'
                                      iv_msgno          = 044
                                      iv_msgv1          = CONV #( co_utilts_mss_proc_id )
                                      iv_msgv2          = CONV #( is_source_step_data_all-proc_ref )
                                  ).
    ENDTRY.

  ENDMETHOD.


  METHOD trigger_check.

    DATA(ls_source_step_data_all) = is_process_step_triggered(
                                      EXPORTING
                                        is_process_step_key = is_process_step_key
                                      CHANGING
                                        cr_data             = cr_data
                                        cr_data_log         = cr_data_log ).

    "Sicherstellen, dass nur für Strom ausgelöst wird.
    IF ls_source_step_data_all IS NOT INITIAL
     AND ls_source_step_data_all-spartyp EQ '01'.

      TRY.
          CALL METHOD prepare_pdoc
            EXPORTING
              iv_int_ui               = ls_source_step_data_all-int_ui
              iv_proc_date            = ls_source_step_data_all-proc_date
*             iv_receiver             =                  " Serviceanbieter (Empfänger)
              is_source_step_data_all = ls_source_step_data_all.
        CATCH /idxgc/cx_utility_error.
          APPEND co_error TO et_check_result.
          RETURN.
      ENDTRY.

      APPEND co_success TO et_check_result.
    ELSE.
      APPEND co_not_relevant TO et_check_result.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
