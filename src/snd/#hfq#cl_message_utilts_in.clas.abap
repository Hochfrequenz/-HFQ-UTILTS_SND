class /HFQ/CL_MESSAGE_UTILTS_IN definition
  public
  inheriting from /IDXGL/CL_MESSAGE
  create public .

public section.
protected section.

  methods HANDLE_REFERENCE_RESP_REV
    changing
      !CS_PROCESS_STEP_DATA type /IDXGC/S_PROC_STEP_DATA
    raising
      /IDXGC/CX_IDE_ERROR .

  methods DETERMINE_BMID
    redefinition .
private section.
ENDCLASS.



CLASS /HFQ/CL_MESSAGE_UTILTS_IN IMPLEMENTATION.


  METHOD determine_bmid.
    DATA:
      lt_amid_conf TYPE TABLE OF /idxgc/amid_conf,
      ls_amid_conf TYPE /idxgc/amid_conf,
      lv_msg       TYPE string,
      lv_lines     TYPE i,
      lv_key_date  TYPE begda,
      ls_amid      TYPE /idxgc/s_amid_details,
      lx_previous  TYPE REF TO /idxgc/cx_general.

* Get process step data
    LOOP AT cs_proc_data-steps ASSIGNING FIELD-SYMBOL(<fs_step>).
      CHECK sy-tabix = 1.
      READ TABLE <fs_step>-amid INTO ls_amid INDEX 1.

      IF <fs_step> IS INITIAL OR ls_amid IS INITIAL .
        MESSAGE e355(/idxgc/utility_add) WITH ls_amid-amid <fs_step>-msg_date INTO lv_msg.
        /idxgc/cx_ide_error=>raise_ide_exception_from_msg( ).
      ENDIF.

      IF NOT <fs_step>-msg_date IS INITIAL.
        lv_key_date = <fs_step>-msg_date.
      ELSE.
        lv_key_date = sy-datlo.
      ENDIF.

*   Determine the BMID by reading the DB table /IDXGC/AMID_CONF and passing the ABMID and
*   the message creation date as the key date a key date  to the WHERE-condition.
      SELECT * FROM /idxgc/amid_conf INTO TABLE lt_amid_conf
        WHERE amid    =   ls_amid-amid
          AND begda   <=  lv_key_date
          AND endda   >=  lv_key_date.

      DESCRIBE TABLE lt_amid_conf LINES lv_lines.
      IF lv_lines = 1.
        READ TABLE lt_amid_conf INTO ls_amid_conf INDEX 1.
        <fs_step>-bmid = ls_amid_conf-bmid.
      ELSEIF lv_lines > 1.
        READ TABLE lt_amid_conf INTO DATA(ls_bmid) WITH KEY view_rec = '05'.
        IF sy-subrc NE 0.
          SORT lt_amid_conf BY begda DESCENDING. "Aktuellster Eintrag zuerst
          READ TABLE lt_amid_conf INTO ls_amid_conf INDEX 1.
          <fs_step>-bmid = ls_amid_conf-bmid.
        ELSE.
          <fs_step>-bmid = ls_bmid-bmid.
        ENDIF.
      ENDIF.

      IF <fs_step>-bmid IS INITIAL.
        MESSAGE e355(/idxgc/utility_add) WITH ls_amid-amid  lv_key_date INTO lv_msg.
        /idxgc/cx_ide_error=>raise_ide_exception_from_msg( ).
      ENDIF.

      TRY.
          "Deal with inbound response or reversal
          CALL METHOD me->handle_reference_resp_rev
            CHANGING
              cs_process_step_data = <fs_step>.

        CATCH /idxgc/cx_ide_error INTO lx_previous.
          MESSAGE e023(/idxgc/ide_add) WITH <fs_step>-docname_code INTO lv_msg.
          CALL METHOD /idxgc/cx_ide_error=>raise_ide_exception_from_msg
            EXPORTING
              ir_previous       = lx_previous
              iv_exception_code = lx_previous->exception_code.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  method HANDLE_REFERENCE_RESP_REV.
*------------------------------------------------------------------------------*
** Author: SAP Custom Development, Mar. 2017
**
** Usage: Handle reference for response and reversal message.
**
*------------------------------------------------------------------------------*
*  Change History:
*  Mar. 2017: Redefined
*             It is copied from IDXGC class /IDXGC/CL_MESSAGE_UTILMD_IN_01
*  Nov 2019: Copied to UTILTS
*------------------------------------------------------------------------------*

  DATA: lv_response_msg   TYPE boolean,
        lv_reversal_msg   TYPE boolean,
        ls_diverse        TYPE /idxgc/s_diverse_details,
        lv_transaction_no	TYPE /idxgc/de_transaction_no,
        lv_direction      TYPE e_dexdirection,
        lv_assoc_servprov	TYPE e_dexservprov,
        lv_proc_id        TYPE /idxgc/de_proc_id,
        ls_proc_step_data TYPE /idxgc/s_proc_step_data,
        lx_previous       TYPE REF TO /idxgc/cx_general.


* Get deverse data
  READ TABLE cs_process_step_data-diverse INTO ls_diverse INDEX 1.

* Response message
  IF cs_process_step_data-msgrespstatus IS NOT INITIAL.
    lv_response_msg = abap_true.
  ENDIF.

  IF lv_response_msg <> abap_true.
    RETURN.
  ENDIF.

  IF lv_response_msg = abap_true.
    lv_transaction_no = ls_diverse-refnr_transreq.
    lv_direction      = cl_isu_datex_process=>co_dexdirection_export.
    lv_assoc_servprov = cs_process_step_data-assoc_servprov.
  ENDIF.

* Get Process ID by Alternative Process ID
  TRY.
      CALL METHOD /idxgc/cl_cust_access=>/idxgc/if_cust_access~get_proc_id_for_uid
        EXPORTING
          iv_process_uid = /idxgc/if_constants_add=>gc_altprocid_err_pdoc
        IMPORTING
          ev_process_id  = lv_proc_id.

    CATCH /idxgc/cx_config_error INTO lx_previous.
      MESSAGE e102(/idxgc/config) INTO gv_mtext WITH /idxgc/if_constants_add=>gc_altprocid_err_pdoc.
      CALL METHOD /idxgc/cx_ide_error=>raise_ide_exception_from_msg
        EXPORTING
          ir_previous = lx_previous.
  ENDTRY.

  TRY .
* Read the preceding message via reference number
      CALL METHOD /idxgc/cl_process_document=>/idxgc/if_process_document~get_preceding_step_data
        EXPORTING
          iv_transaction_no     = lv_transaction_no
          iv_direction          = lv_direction
          iv_assoc_servprov     = lv_assoc_servprov
          iv_proc_id_error_pdoc = lv_proc_id
        IMPORTING
          es_proc_step_data     = ls_proc_step_data.

* Take over the process reference to current step in case of response
* Fill step data into attributes in case of response
      IF lv_response_msg = abap_true.

        cs_process_step_data-proc_ref = ls_proc_step_data-proc_ref.

        CALL METHOD me->fill_orig_msg_data_into_attr
          EXPORTING
            is_proc_step_data_orig = ls_proc_step_data
            iv_scenario_id         = /idxgc/if_constants_add=>gc_scenario_id_response
          CHANGING
            cs_process_step_data   = cs_process_step_data.

      ENDIF.

    CATCH /idxgc/cx_process_error INTO lx_previous.
      CALL METHOD /idxgc/cx_ide_error=>raise_ide_exception_from_msg
        EXPORTING
          ir_previous       = lx_previous
          iv_exception_code = lx_previous->exception_code.

  ENDTRY.
  endmethod.
ENDCLASS.
