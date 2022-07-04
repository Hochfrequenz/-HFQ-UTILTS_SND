class /HFQ/CL_DP_UTILTS definition
  public
  inheriting from /IDXGL/CL_DP
  create public .

public section.

  constants GC_BMID_UTTS0 type /IDXGC/DE_BMID value '/HFQ/UTTS0' ##NO_TEXT.
  constants GC_BMID_UTTS1 type /IDXGC/DE_BMID value '/HFQ/UTTS1' ##NO_TEXT.
  constants GC_BMID_UTTS2 type /IDXGC/DE_BMID value '/HFQ/UTTS2' ##NO_TEXT.
  constants GC_ANSWER_STAT_ZQ3 type /IDXGC/DE_RESPSTATUS value 'ZQ3' ##NO_TEXT.
  constants GC_ANSWER_STAT_ZK3 type /IDXGC/DE_RESPSTATUS value 'ZK3' ##NO_TEXT.
  constants GC_ANSWER_STAT_ZQ4 type /IDXGC/DE_RESPSTATUS value 'ZQ4' ##NO_TEXT.
  constants GC_ANSWER_STAT_ZK4 type /IDXGC/DE_RESPSTATUS value 'ZK4' ##NO_TEXT.
  constants GC_ANSWER_STAT_ZK5 type /IDXGC/DE_RESPSTATUS value 'ZK5' ##NO_TEXT.
  constants GC_ANSWER_STAT_ZK6 type /IDXGC/DE_RESPSTATUS value 'ZK6' ##NO_TEXT.
  constants GC_ANSWER_STAT_ZK7 type /IDXGC/DE_RESPSTATUS value 'ZK7' ##NO_TEXT.
  constants GC_ANSWER_STAT_E14 type /IDXGC/DE_RESPSTATUS value 'E14' ##NO_TEXT.
  constants GC_ANSWER_STAT_E15 type /IDXGC/DE_RESPSTATUS value 'E15' ##NO_TEXT.
  constants GC_CALCFORM_STAT_Z33 type /HFQ/DE_CALC_FORM_STAT value 'Z33' ##NO_TEXT.
  constants GC_CALCFORM_STAT_Z34 type /HFQ/DE_CALC_FORM_STAT value 'Z34' ##NO_TEXT.
  constants GC_MESSAGE_CAT_Z36 type /IDXGC/DE_KEY_DOCNAME_CODE value 'Z36' ##NO_TEXT.
  constants GC_MESSAGE_CAT_Z59 type /IDXGC/DE_KEY_DOCNAME_CODE value 'Z59' ##NO_TEXT.
  constants GC_MESSAGE_CAT_Z60 type /IDXGC/DE_KEY_DOCNAME_CODE value 'Z60' ##NO_TEXT.
  constants GC_MATH_OPERATOR_Z69 type /IDXGL/DE_MATH_OPERATOR value 'Z69' ##NO_TEXT.
  constants GC_MATH_OPERATOR_Z70 type /IDXGL/DE_MATH_OPERATOR value 'Z70' ##NO_TEXT.
  constants GC_MATH_OPERATOR_Z80 type /IDXGL/DE_MATH_OPERATOR value 'Z80' ##NO_TEXT.
  constants GC_MATH_OPERATOR_Z81 type /IDXGL/DE_MATH_OPERATOR value 'Z81' ##NO_TEXT.
  constants GC_MATH_OPERATOR_Z82 type /IDXGL/DE_MATH_OPERATOR value 'Z82' ##NO_TEXT.
  constants GC_MATH_OPERATOR_Z83 type /IDXGL/DE_MATH_OPERATOR value 'Z83' ##NO_TEXT.
  constants GC_MSGTP_UTILTS type EDI_MESTYP value '/IDXGL/UTILTS' ##NO_TEXT.
  constants GC_CALCFORM_STAT_Z40 type /HFQ/DE_CALC_FORM_STAT value 'Z40' ##NO_TEXT.
  constants GC_CALCFORM_STAT_Z41 type /HFQ/DE_CALC_FORM_STAT value 'Z41' ##NO_TEXT.
  constants GC_BMID_TOUO1 type /IDXGC/DE_BMID value '/HFQ/TOUO1' ##NO_TEXT.
  constants GC_BMID_TOUR1 type /IDXGC/DE_BMID value '/HFQ/TOUR1' ##NO_TEXT.
  constants GC_TOU_TYPE_ELSE type CHAR3 value 'Z32' ##NO_TEXT.

  methods ADDITIONAL_TEXT
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods CALC_STEP_SEQ
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods COMPLETION_DATE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods CONSTRUCTOR
    importing
      !IS_PROCESS_DATA_SRC type /IDXGC/S_PROC_STEP_DATA_ALL
      !IS_PROCESS_DATA_SRC_ADD type /IDXGC/S_PROC_STEP_DATA_ALL optional .
  methods ENERGY_DIRECTION
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods LOSSFACTOR_LINE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods LOSSFACTOR_TRANSFORMATOR
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods MALO_ENERGYAMOUNT_SEQ
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods MARKET_LOCATION_ID
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods MATHEMATICAL_OPERATOR
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods MR_RELEVANCE_AND_USE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods REFERENCE_TO_CALC_STEP
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods REFERENCE_TO_POD
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods REFERENCE_TO_TRANSACTION
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods STATUS_OF_ANSWER
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods STATUS_OF_CALCFORM
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods SUPPLY_DIRECTION
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_DEFINITION_CODE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_DEF_TYPE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_HP_USAGE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_LIGHTLOAD_COMPATIBLE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_ORDERABLE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_REGISTER_CODE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_REGISTER_REF
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_R_ACTIVE_REGISTERS
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_R_ORDER_REFERENCE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_R_VALID_FROM_DATE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_R_VALID_TO_DATE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_TRANSMISSION_FREQ
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_TRANSMISSION_WAY
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TOU_USAGE
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods TRANSACTION_ID
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods VALID_FROM_DATE
    raising
      /IDXGC/CX_PROCESS_ERROR .

  methods /IDXGL/IF_DP_OUT~PROCESS_CONFIGURATION_STEPS
    redefinition .
  methods CONTACT_PERSON
    redefinition .
  methods INSTANTIATE
    redefinition .
  methods MESSAGE_CATEGORY
    redefinition .
protected section.

  data SIV_DATA_NOT_FILLED type /IDXGL/DE_DATA_NOT_FILLED .
  data SIV_CONTEXT_SEQ type CHAR3 .
  data SIV_METER_PROC type /IDXGC/DE_METER_PROC_1 .
  data SIS_CALC_FORM_H type /HFQ/CALC_FORM_H .
  data SIT_CALC_FORM type /HFQ/T_CALC_FORM .
  data MT_TOU_DATA type /HFQ/T_TOU_DATA .

  methods GET_METERING_PROCEDURE_DETAILS
    changing
      !CT_REG_CODE_DETAILS type /IDXGC/T_REG_CODE_DETAILS optional
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods GET_TIME_OF_USE_DATA
    raising
      /IDXGC/CX_PROCESS_ERROR .
  methods PROVIDE_SUPPLY_DIRECT
    returning
      value(RV_SUPPLY_DIRECT) type /IDXGC/DE_SUPPLY_DIRECT .
private section.

  methods GET_UTC_OFFSET
    importing
      !IV_DATE type DATS
      !IV_TIME type TIMS
      !IV_ZONLO type SYST_ZONLO default SY-ZONLO
    returning
      value(RV_OFFSET) type CHAR3 .
ENDCLASS.



CLASS /HFQ/CL_DP_UTILTS IMPLEMENTATION.


  METHOD /idxgl/if_dp_out~process_configuration_steps.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: Process configuration step to fill data container
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*
    DATA: lt_bmid_config           TYPE /idxgc/t_bmid_conf,
          ls_bmid_config           TYPE /idxgc/bmid_conf,
          ls_bmid_config_group_ide TYPE /idxgc/bmid_conf,
          lt_bmid_config_group_ide TYPE /idxgc/t_bmid_conf,
          lx_previous              TYPE REF TO /idxgc/cx_general,
          ls_process_data_src      TYPE /idxgc/s_proc_step_data_all,
          ls_diverse               TYPE /idxgc/s_diverse_details.
    DATA: lv_non_proc_condition     TYPE /idxgc/de_non_pro_condition.

    TRY .
        CALL METHOD /idxgl/if_dp_out~get_configuration
          IMPORTING
            et_bmid_config = lt_bmid_config.
      CATCH /idxgc/cx_process_error INTO lx_previous.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ir_previous = lx_previous ).
    ENDTRY.

    CALL METHOD me->instantiate( ).

    SORT lt_bmid_config BY step_index.

* Field "Non Processing Condition" is used to distinguish different processes(GPKE & GENF),
* if it is set to initial, the segment will be used to all processes.
    CASE sis_process_step_data-sup_direct_int.
      WHEN /idxgc/if_constants_add=>gc_sup_direct_supply.
        lv_non_proc_condition = /idxgc/if_constants_add=>gc_non_proc_condition_01.
      WHEN /idxgc/if_constants_add=>gc_sup_direct_feeding.
        lv_non_proc_condition = /idxgc/if_constants_add=>gc_non_proc_condition_02.
      WHEN OTHERS.
    ENDCASE.

    LOOP AT lt_bmid_config INTO ls_bmid_config
      WHERE ( non_process_cond IS INITIAL ) OR
            ( non_process_cond = lv_non_proc_condition ).

* Fill the flag siv_data_from_source and siv_data_from_add_source in case customer use them
      siv_data_from_source = ls_bmid_config-data_from_source.
      siv_data_from_add_source = ls_bmid_config-data_add_source.

      IF ls_bmid_config-data_from_source IS NOT INITIAL.
        siv_data_processing_mode = /idxgc/if_constants_add=>gc_data_from_source.
      ELSEIF ls_bmid_config-data_add_source IS NOT INITIAL.
        siv_data_processing_mode = /idxgc/if_constants_add=>gc_data_from_add_source.
      ELSE.
        siv_data_processing_mode = /idxgc/if_constants_add=>gc_default_processing.
      ENDIF.

* Check whether the field is mandatory
      siv_mandatory_data = ls_bmid_config-mandatory.

      IF ls_bmid_config-edifact_group EQ /idxgc/if_constants_add=>gc_edifact_group_ide.
        IF sis_process_data_src IS NOT INITIAL.
          ls_process_data_src = sis_process_data_src.
        ELSE.
          ls_process_data_src = sis_process_data_src_add.
        ENDIF.

* Collect relevant provision methods based on dependency of EDIFACT and group
        CLEAR lt_bmid_config_group_ide.
        APPEND ls_bmid_config TO lt_bmid_config_group_ide.
        LOOP AT lt_bmid_config INTO ls_bmid_config_group_ide "#EC CI_NESTED
          WHERE ( non_process_cond IS INITIAL
            OR non_process_cond = lv_non_proc_condition )
            AND edifact_group EQ /idxgc/if_constants_add=>gc_edifact_group_ide
            AND dependent_str = ls_bmid_config-edifact_structur..
          APPEND ls_bmid_config_group_ide TO lt_bmid_config_group_ide.
        ENDLOOP.
        SORT lt_bmid_config_group_ide BY step_index.
        DELETE lt_bmid_config
          WHERE dependent_str = ls_bmid_config-edifact_structur
            AND edifact_group EQ /idxgc/if_constants_add=>gc_edifact_group_ide.

* Process all relevant provision methods for each diverse item
        LOOP AT ls_process_data_src-diverse INTO ls_diverse. "#EC CI_NESTED
          siv_itemid = ls_diverse-item_id.
          LOOP AT lt_bmid_config_group_ide INTO ls_bmid_config_group_ide. "#EC CI_NESTED
*       If the division category doesn't set in the configuration, we consider it as the same meaning
*       with value 99(For all sectors).
            IF ( ls_bmid_config_group_ide-spartyp = sis_process_step_data-spartyp ) OR
               ( ls_bmid_config_group_ide-spartyp IS INITIAL ) OR
               ( ls_bmid_config_group_ide-spartyp = /idxgc/if_constants=>gc_divcat_all ).
*       Call specific method
              TRY .
                  CALL METHOD me->(ls_bmid_config_group_ide-method).
                CATCH cx_sy_dyn_call_illegal_method.
                  MESSAGE e060(/idxgc/process_add) INTO siv_mtext
                    WITH ls_bmid_config_group_ide-method ls_bmid_config_group_ide-edifact_structur.
                  CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg.
                CATCH /idxgc/cx_process_error INTO lx_previous.
                  IF ls_bmid_config_group_ide-mandatory IS NOT INITIAL.
                    CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ir_previous = lx_previous ).
                  ELSE.
                    DELETE lt_bmid_config WHERE dependent_str = ls_bmid_config-edifact_structur.
                    DELETE lt_bmid_config_group_ide WHERE dependent_str = ls_bmid_config-edifact_structur.
                    CONTINUE.
                  ENDIF.
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ELSE.
        siv_itemid = 1.
*   If the division category doesn't set in the configuration, we consider it as the same meaning
*   with value 99(For all sectors).
        IF ( ls_bmid_config-spartyp = sis_process_step_data-spartyp ) OR
           ( ls_bmid_config-spartyp IS INITIAL ) OR
           ( ls_bmid_config-spartyp = /idxgc/if_constants=>gc_divcat_all ).
*   Call specific method
          TRY .
              CALL METHOD me->(ls_bmid_config-method).
            CATCH cx_sy_dyn_call_illegal_method.
              MESSAGE e060(/idxgc/process_add) INTO siv_mtext WITH ls_bmid_config-method ls_bmid_config-edifact_structur.
              CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg.
            CATCH /idxgc/cx_process_error INTO lx_previous.
              IF ls_bmid_config-mandatory IS NOT INITIAL.
                CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ir_previous = lx_previous ).
              ELSE.
                DELETE lt_bmid_config WHERE dependent_str = ls_bmid_config-edifact_structur.
                CONTINUE.
              ENDIF.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD additional_text.
    "Implementation needed
  ENDMETHOD.


  METHOD calc_step_seq.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: this method fills all necessary data for the calculation step-sequences.
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

    DATA: ls_calc_form TYPE /hfq/calc_form,
          ls_pod_data  TYPE /idxgl/s_pod_data_details.

    READ TABLE sis_process_step_data-diverse ASSIGNING FIELD-SYMBOL(<fs_diverse>)
        WITH KEY item_id = siv_itemid
                 calc_form_stat = gc_calcform_stat_z34.
    IF sy-subrc = 0.
      "No Calc-Steps for bilateral sign
      RETURN.
    ENDIF.

    siv_context_seq = /idxgl/if_constants_ide=>gc_seq_action_code_z37.

    DELETE sis_process_step_data-/idxgl/pod_data
      WHERE item_id = siv_itemid
        AND data_type_qual = siv_context_seq.

    " Fill in all the data from the CALC_FORM-Table:
    LOOP AT sit_calc_form INTO ls_calc_form.
      CLEAR: ls_pod_data.

      ls_pod_data-item_id = siv_itemid.
      ls_pod_data-data_type_qual = siv_context_seq.

      MOVE-CORRESPONDING ls_calc_form TO ls_pod_data.

      IF ls_calc_form-melo_extui IS NOT INITIAL.
        ls_pod_data-ext_ui = ls_calc_form-melo_extui.
      ENDIF.

      APPEND ls_pod_data TO sis_process_step_data-/idxgl/pod_data.
    ENDLOOP.

  ENDMETHOD.


  METHOD completion_date.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set valid start date
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_diverse TYPE /idxgc/s_diverse_details.

    FIELD-SYMBOLS <ls_diverse> TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS <ls_diverse_src> TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS <ls_diverse_src_add> TYPE /idxgc/s_diverse_details.
*
    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src> INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc EQ 0.
            <ls_diverse>-version_date = <ls_diverse_src>-version_date.
            <ls_diverse>-version_time = <ls_diverse_src>-version_time.
            <ls_diverse>-version_offs = <ls_diverse_src>-version_offs.
          ELSE.
            ls_diverse = VALUE #(
              item_id = siv_itemid
              version_date = <ls_diverse_src>-version_date
              version_time = <ls_diverse_src>-version_time
              version_offs = <ls_diverse_src>-version_offs
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.

*  GET data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src_add> INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc EQ 0.
            <ls_diverse>-version_date = <ls_diverse_src_add>-version_date.
            <ls_diverse>-version_time = <ls_diverse_src_add>-version_time.
            <ls_diverse>-version_offs = <ls_diverse_src_add>-version_offs.
          ELSE.
            ls_diverse = VALUE #(
              item_id = siv_itemid
              version_date = <ls_diverse_src_add>-version_date
              version_time = <ls_diverse_src_add>-version_time
              version_offs = <ls_diverse_src_add>-version_offs
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.

*  GET data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.

*    READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src> INDEX 1.
*    IF sy-subrc EQ 0.
*      READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
*      IF sy-subrc EQ 0.
*        <ls_diverse>-version_date = <ls_diverse_src>-version_date.
*        <ls_diverse>-version_time = <ls_diverse_src>-version_time.
*        <ls_diverse>-version_offs = <ls_diverse_src>-version_offs.
*      ELSE.
*        ls_diverse = VALUE #(
*          item_id = siv_itemid
*          version_date = <ls_diverse_src>-version_date
*          version_time = <ls_diverse_src>-version_time
*          version_offs = <ls_diverse_src>-version_offs
*        ).
*        APPEND ls_diverse TO sis_process_step_data-diverse.
*      ENDIF.
*    ENDIF.
        LOOP AT sis_process_data_src-/idxgl/tou ASSIGNING FIELD-SYMBOL(<ls_tou_src>).
          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc EQ 0.
            READ TABLE mt_tou_data WITH KEY tou_code = <ls_tou_src>-tou_code INTO DATA(ls_tou_data).
            IF sy-subrc EQ 0.
              <ls_diverse>-version_date = ls_tou_data-versiondata[ 1 ]-completion_date.
              <ls_diverse>-version_time = '0000'.
              <ls_diverse>-version_offs = '+00'.
            ENDIF.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
      IF sy-subrc <> 0 OR
        <ls_diverse>-version_date IS INITIAL OR
        <ls_diverse>-version_offs IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-101 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.

    CALL METHOD super->constructor
      EXPORTING
        is_process_data_src     = is_process_data_src
        is_process_data_src_add = is_process_data_src_add.

    IF is_process_data_src IS NOT INITIAL .
      sis_process_data_src = is_process_data_src.
    ENDIF.
    IF is_process_data_src_add IS NOT INITIAL .
      sis_process_data_src_add = is_process_data_src_add.
    ENDIF.

  ENDMETHOD.


  METHOD contact_person.
**<<< 2020-03-04 Heidemueller Übernahme Entwicklungen von Ralf Seiler
*    DATA: ls_contact_details TYPE /idxgc/s_markcom_details.
*    DATA: lr_badi_utilts_snd TYPE REF TO /hfq/badi_utilts_snd.
*
*    FIELD-SYMBOLS: <ls_marketpartner> TYPE /idxgc/s_markpar_details.
*
*** if the formula is bilateral, we need a contact
**    IF sis_calc_form_h-calc_form_stat = /hfq/if_badi_utilts_snd=>gc_formel_versand_bilateral.
*
*    READ TABLE sis_process_step_data-marketpartner ASSIGNING <ls_marketpartner>
*    WITH KEY party_func_qual = /idxgc/if_constants_ide=>gc_nad_qual_ms.
*
*    IF <ls_marketpartner> IS ASSIGNED.
*
*      GET BADI lr_badi_utilts_snd.
*      TRY.
*          CALL BADI lr_badi_utilts_snd->provide_contact
*            EXPORTING
*              is_step_data       = sis_process_step_data
*              iv_message_stat    = sis_calc_form_h-calc_form_stat
*            IMPORTING
*              ev_contact_name    = ls_contact_details-dep_employ_name
*              ev_contact_addr    = ls_contact_details-comm_addr_id
*              ev_contact_qual    = ls_contact_details-comm_addr_qual
*            EXCEPTIONS
*              no_comm_addr_found = 1
*              OTHERS             = 2.
*
*          IF sy-subrc <> 0.
*            CLEAR ls_contact_details.
*          ENDIF.
*
*          " if BAdI-Implementation provided contact-data, use it
*          IF ls_contact_details IS NOT INITIAL.
*            ls_contact_details-party_func_qual = /idxgc/if_constants_ide=>gc_nad_qual_ms.
*            ls_contact_details-com_counter = '001'.
*
*            APPEND ls_contact_details TO sis_process_step_data-markpar_comm.
*          ELSE.
*            " no BAdI - use standard, but only for Z34 messages
*            IF sis_calc_form_h-calc_form_stat = /hfq/if_badi_utilts_snd=>gc_formel_versand_bilateral.  "'Z34
*              super->contact_person( ).
*            ENDIF.
*          ENDIF.
*
*        CATCH /idxgc/cx_general.
*          "Fehler bei Aufruf des BAdI &1.
*          MESSAGE e003(/hfq/utilts_snd) WITH '/hfq/badi_utilts_snd->provide_contact'.
*          EXIT.
*      ENDTRY.
*    ENDIF.
**    ENDIF.
*
**------------------ CHECK if DATA is mandatory and filled ------------------*
*    IF siv_mandatory_data = abap_true.
*      READ TABLE sis_process_step_data-markpar_comm TRANSPORTING NO FIELDS
*        WITH KEY party_func_qual = /idxgc/if_constants_ide=>gc_nad_qual_ms.
*      IF sy-subrc <> 0.
*        MESSAGE e038(/idxgc/ide_add) WITH TEXT-004 INTO siv_mtext.
*        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
*      ENDIF.
*    ENDIF.
**>>>
  ENDMETHOD.


  METHOD energy_direction.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: method to set the energy direction on this calculation step
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

* Data was filled in method CALC_STEP_SEQ
* this method is, by default, only used for the mandatory-field check.

*------------------ Check if data is mandatory and filled ------------------*
*<<< 2020-03-04 Heidemueller Übernahme Entwicklungen von Ralf Seiler
    FIELD-SYMBOLS: <ls_pod_data> TYPE /idxgl/s_pod_data_details.

    IF siv_mandatory_data = abap_true.
*    LOOP AT sis_process_step_data-/idxgl/pod_data TRANSPORTING NO FIELDS
      LOOP AT sis_process_step_data-/idxgl/pod_data ASSIGNING <ls_pod_data>
        WHERE item_id     = siv_itemid
          AND data_type_qual = siv_context_seq.
*        AND flow_direction IS INITIAL.

        IF <ls_pod_data>-ext_ui IS NOT INITIAL         " calc step with MeLo
          AND <ls_pod_data>-flow_direction IS INITIAL. " but without flow-dir

          MESSAGE e038(/idxgc/ide_add) WITH TEXT-014 INTO siv_mtext.
          CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
        ENDIF.
*>>>
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_metering_procedure_details.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: This method is used to Dertmine Voltage Level of Point of Delivery only from Grid
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

    DATA: lr_data_provision     TYPE REF TO /idxgl/badi_data_provision,
          lx_previous           TYPE REF TO /idxgc/cx_general,
          lx_root               TYPE REF TO cx_root,
          lv_class_name         TYPE seoclsname,
          lv_method_name        TYPE seocpdname,
          lt_meter_proc_details TYPE /idxgc/t_profile_details.

    FIELD-SYMBOLS:
      <ls_meter_proc_details> TYPE /idxgc/s_profile_details.

    TRY.
        GET BADI lr_data_provision.
      CATCH cx_badi_not_implemented
          cx_badi_multiply_implemented INTO lx_root.

        MESSAGE e007(/idxgc/general) INTO siv_mtext
         WITH /idxgl/if_constants_ddic=>gc_badi_data_provision. "'/IDXGL/BADI_DATA_PROVISION'
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg
          EXPORTING
            ir_previous = lx_root.
    ENDTRY.

    TRY.
        IF lr_data_provision IS BOUND.
          CALL BADI lr_data_provision->metering_procedure_details
            EXPORTING
              is_process_data_src     = sis_process_data_src
              is_process_data_src_add = sis_process_data_src_add
              is_process_data         = sis_process_step_data
              iv_itemid               = siv_itemid
            IMPORTING
              et_profile_details      = lt_meter_proc_details
            CHANGING
              ct_reg_code_details     = ct_reg_code_details.
        ENDIF.
      CATCH /idxgc/cx_utility_error INTO lx_previous.
        CALL METHOD /idxgc/cl_utility_service=>/idxgc/if_utility_service~get_current_source_pos
          IMPORTING
            ev_class_name  = lv_class_name
            ev_method_name = lv_method_name.
        MESSAGE e021(/idxgc/ide_add) INTO siv_mtext WITH lv_class_name lv_method_name.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ir_previous = lx_previous ).
    ENDTRY.

    READ TABLE lt_meter_proc_details ASSIGNING <ls_meter_proc_details>
      WITH KEY item_id = siv_itemid.
    IF sy-subrc = 0.
      siv_meter_proc = <ls_meter_proc_details>-meter_proc.
    ENDIF.

  ENDMETHOD.


  METHOD get_time_of_use_data.
    DATA lt_range_tou_code TYPE /hfq/t_tou_range_tou_code.
    DATA ls_range_tou_code TYPE /hfq/s_tou_range_tou_code.
    DATA lv_cat_version TYPE /hfq/de_tou_cat_version.

    FIELD-SYMBOLS <ls_diverse> TYPE /idxgc/s_diverse_details.
*    FIELD-SYMBOLS <ls_tou> TYPE /idxgl/s_tou_details.

    READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
    IF sy-subrc EQ 0.
*      IF <ls_diverse>-tou_code IS NOT INITIAL.
*        ls_range_tou_code = VALUE #( sign = 'I' option = 'EQ' low = <ls_diverse>-tou_code ).
      APPEND ls_range_tou_code TO lt_range_tou_code.
*      ENDIF.
*      IF <ls_diverse>-version_date IS NOT INITIAL.
*        lv_cat_version = |{ <ls_diverse>-version_date }{ <ls_diverse>-version_time }{ <ls_diverse>-version_offs }|.
*      ENDIF.
    ENDIF.

*    loop at sis_process_step_data-/idxgl/tou assigning <ls_tou> where tou_code is not initial.
*      ls_range_tou_code = VALUE #( sign = 'I' option = 'EQ' low = <ls_tou>-tou_code ).
*      APPEND ls_range_tou_code to lt_range_tou_code.
*    endloop.

    " Wir lesen nur die Daten zur relevanten Version ein:
    DATA(lo_tou_db) = NEW /hfq/cl_tou_db(  ).
    TRY.
        IF lv_cat_version IS NOT INITIAL.
          mt_tou_data = lo_tou_db->select_via_cat_version(
            iv_cat_version = lv_cat_version
            iv_only_active_version = abap_false
          ).
          DELETE mt_tou_data WHERE tou_code NOT IN lt_range_tou_code.
        ENDIF.
      CATCH /hfq/cx_tou_db_error INTO DATA(lx_tou_db_error). " Ausnahmeklasse zum DB-Layer
        /idxgc/cx_process_error=>raise_proc_exception_from_msg(
          ir_previous = lx_tou_db_error
          is_process_step_key = VALUE #(
            proc_id = sis_process_step_data-proc_id
            proc_ref = sis_process_step_data-proc_ref
            proc_step_no = sis_process_step_data-proc_step_no
            proc_step_ref = sis_process_step_data-proc_step_ref
          )
        ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_utc_offset.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to determine the utc offset
*------------------------------------------------------------------------------*
*  Change History:
*  January   2022: created
*------------------------------------------------------------------------------*

    DATA lv_utc_diff TYPE tznutcdiff.
    DATA lv_utc_sign TYPE tznutcsign.

    CALL FUNCTION 'TZON_GET_OFFSET'
      EXPORTING
        if_timezone      = iv_zonlo      " Time Zone
        if_local_date    = iv_date    " Field of Type DATS
        if_local_time    = iv_time    " Field of type TIMS
      IMPORTING
        ef_utcdiff       = lv_utc_diff      " Difference of time zone from UTC (w/o Summer time)
        ef_utcsign       = lv_utc_sign      " Difference of time zone from UTC (w/o Summer time)
      EXCEPTIONS
        conversion_error = 1.
    IF sy-subrc <> 0.
      CLEAR rv_offset.
      RETURN.
    ENDIF.

    " Es interessiert nur die Stunde. Unterstündlicher Offset nicht relevant.
    rv_offset = |{ lv_utc_sign }{ lv_utc_diff(2) }|.

  ENDMETHOD.


  METHOD instantiate.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: Method to initially fill necessary data
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

    DATA: ls_mescod      TYPE /idxgc/t_mescod,
          lt_calc_form_h TYPE /hfq/t_calc_form_h,
          lr_previous    TYPE REF TO /idxgc/cx_general.

    DATA: lr_badi_utilts_snd TYPE REF TO /hfq/badi_utilts_snd.

    TRY.
        ls_mescod = /idxgc/cl_cust_access=>/idxgc/if_cust_access_add~get_mescod(
                    iv_key_date = sy-datum
                    iv_mestyp   = gc_msgtp_utilts ).
      CATCH /idxgc/cx_config_error INTO lr_previous.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg
          EXPORTING
            ir_previous = lr_previous.
    ENDTRY.
    siv_assigned_code_vdew = ls_mescod-assocode.

    siv_sender = sis_process_step_data-own_servprov.
    siv_receiver = sis_process_step_data-assoc_servprov.

    " gültig ab in die Schrittdaten übernehmen
    IF sis_process_data_src-date_from IS NOT INITIAL
      AND sis_process_step_data-date_from = '00000000'.
      sis_process_step_data-date_from = sis_process_data_src-date_from.
    ENDIF.

    IF sis_process_data_src-time_from IS NOT INITIAL
      AND sis_process_step_data-time_from = '000000'.
      sis_process_step_data-time_from = sis_process_data_src-time_from.
    ENDIF.


    GET BADI lr_badi_utilts_snd.
    TRY.
        CALL BADI lr_badi_utilts_snd->select_relevant_data
          EXPORTING
            iv_malo            = sis_process_step_data-ext_ui
            iv_keydate         = sis_process_step_data-proc_date
            iv_keytime         = sis_process_step_data-time_from
            iv_receiver        = siv_receiver
          IMPORTING
            et_calc_form_h     = lt_calc_form_h
            et_calc_form       = sit_calc_form
          EXCEPTIONS
            no_construct_found = 1                " Kein Konstrukt zur Auswahl gefunden.
            no_calc_form_found = 2                " Keine Berechnungsschritte gefunden.
            OTHERS             = 3.
        IF sy-subrc = 1 OR lt_calc_form_h IS INITIAL.
          " Keine Einträge in Tabelle &1 zu MaLo &2 ab Datum &3 gefunden.
          " Tabellenname ist spezifisch für jede Implementierung, daher via BAdI
          CALL BADI lr_badi_utilts_snd->provide_datatable_names
            IMPORTING
              ev_formel_h_tablename = DATA(lv_tablename).
          IF sy-batch EQ abap_false.
            MESSAGE i001(/hfq/utilts_snd) WITH lv_tablename
                                               sis_process_step_data-ext_ui
                                               sis_process_step_data-proc_date .
            RETURN.
          ENDIF.
        ELSEIF line_exists( lt_calc_form_h[ calc_form_stat = gc_calcform_stat_z33 ] ) AND ( sy-subrc = 2 OR sit_calc_form IS INITIAL ).
          "Keine Berechnungssschritte in Tabelle &1 zu MaLo &2 gefunden.
          " Tabellenname ist spezifisch für jede Implementierung, daher via BAdI
          CALL BADI lr_badi_utilts_snd->provide_datatable_names
            IMPORTING
              ev_formelsteps_tablename = DATA(lv_steps_tablename).

          MESSAGE e002(/hfq/utilts_snd) WITH lv_steps_tablename
                                             sis_process_step_data-ext_ui.
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

    SORT lt_calc_form_h.
    DELETE ADJACENT DUPLICATES FROM lt_calc_form_h.
    IF lines( lt_calc_form_h ) = 1.
      sis_calc_form_h = lt_calc_form_h[ 1 ].
    ELSE.
      "Zu viele Daten!.
      sis_calc_form_h = lt_calc_form_h[ 1 ].
    ENDIF.

    SORT sit_calc_form BY calc_step_id calc_step_id_subnum ref_other_calc_step.
    DELETE ADJACENT DUPLICATES FROM sit_calc_form.

**********************************************************************
*Get all relevant ZZD data
    TRY.
        DATA(lr_tou_db) = NEW /hfq/cl_tou_db( ).
        mt_tou_data = lr_tou_db->select_via_range(
*                          it_range_cat_id        =
*                          it_range_tou_code      =
*                          it_range_cat_version   =
*                          it_range_reg_id        =
*                          it_range_service_id    =
*                          iv_datefrom            =
*                          iv_dateto              =
                              iv_only_active_version = abap_true
                              iv_own_tou             = 'X'
                            ).
      CATCH /hfq/cx_tou_db_error. " Ausnahmeklasse zum DB-Layer
    ENDTRY.
  ENDMETHOD.


  METHOD lossfactor_line.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: method for the lossfactor of the line
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

* Data was filled in method CALC_STEP_SEQ
* this method is, by default, only used for the mandatory-field check.

*------------------ CHECK if DATA is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-/idxgl/pod_data TRANSPORTING NO FIELDS
        WHERE item_id     = siv_itemid
          AND data_type_qual = siv_context_seq
          AND lossfact_line IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-016 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD lossfactor_transformator.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: method for the transformator-lossfactor
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

* Data was filled in method CALC_STEP_SEQ
* this method is, by default, only used for the mandatory-field check.

*------------------ CHECK if DATA is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-/idxgl/pod_data TRANSPORTING NO FIELDS
        WHERE item_id     = siv_itemid
          AND data_type_qual  = siv_context_seq
          AND lossfact_ext IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-015 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD malo_energyamount_seq.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: Start of the MaLo-energyamount-sequence
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

*<<< 2020-03-04 Heidemueller Übernahme Entwicklungen von Ralf Seiler
    "Z 36-SEQ only, for complex formula
    IF sis_calc_form_h-calc_form_stat EQ /hfq/if_badi_utilts_snd=>gc_formel_versand_complex.
*>>>
      siv_context_seq = /idxgl/if_constants_ide=>gc_seq_action_code_z36.

      DELETE sis_process_step_data-/idxgl/pod_data
        WHERE item_id = siv_itemid
          AND data_type_qual = siv_context_seq.

    ENDIF.
  ENDMETHOD.


  METHOD market_location_id.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: This method is used to set the MaLo-ID
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

    DATA: ls_pod      TYPE /idxgc/s_pod_info_details,
          lx_previous TYPE REF TO /idxgc/cx_general.

    CLEAR: sis_process_step_data-pod,
           ls_pod.

* If external POD is unkown, get external POD through internal POD
    IF sis_process_step_data-ext_ui IS INITIAL.
      TRY.
          sis_process_step_data-ext_ui = /idxgc/cl_utility_service_isu=>get_extui_from_intui(
                                               iv_int_ui = sis_process_step_data-int_ui
                                               iv_date   = sis_process_step_data-proc_date ).
        CATCH /idxgc/cx_utility_error INTO lx_previous.
          CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg(
              ir_previous = lx_previous ).
      ENDTRY.
    ENDIF.
* if it is the other way around:
    IF sis_process_step_data-int_ui IS INITIAL.
      TRY.
          CALL METHOD /idxgc/cl_utility_service_isu=>get_intui_from_extui
            EXPORTING
              iv_ext_ui = sis_process_step_data-ext_ui
              iv_date   = sis_process_step_data-proc_date
            IMPORTING
              rv_int_ui = sis_process_step_data-int_ui.

        CATCH /idxgc/cx_utility_error INTO lx_previous.
          CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg(
              ir_previous = lx_previous ).
      ENDTRY.
    ENDIF.

    ls_pod-item_id         = siv_itemid.
    ls_pod-loc_func_qual   = /idxgc/if_constants_ide=>gc_loc_qual_172.
    ls_pod-int_ui          = sis_process_step_data-int_ui.
    ls_pod-ext_ui          = sis_process_step_data-ext_ui.
    APPEND ls_pod TO sis_process_step_data-pod.

*------------------ Check if data is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      READ TABLE sis_process_step_data-pod TRANSPORTING NO FIELDS
          WITH KEY item_id = siv_itemid
                   loc_func_qual = /idxgc/if_constants_ide=>gc_loc_qual_172.
      IF sy-subrc <> 0.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-002 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD mathematical_operator.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: method for the mathematical operator of the step
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*


* Data was filled in method CALC_STEP_SEQ
* this method is, by default, only used for the mandatory-field check.

*------------------ CHECK if DATA is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-/idxgl/pod_data TRANSPORTING NO FIELDS
        WHERE item_id     = siv_itemid
          AND data_type_qual = siv_context_seq
          AND math_operator IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-013 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD message_category.

*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: Method to set the message category
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*  January   2022: Logik implementiert um Nachrichtenkategorie zwischen
*                  Zählzeiten und Berechnungsformeln zu unterscheiden.
*------------------------------------------------------------------------------*


    CASE sis_process_step_data-bmid.
      WHEN gc_bmid_utts0.
        sis_process_step_data-docname_code = gc_message_cat_z36.
      WHEN gc_bmid_touo1.
        sis_process_step_data-docname_code = gc_message_cat_z60.
      WHEN gc_bmid_tour1.
        sis_process_step_data-docname_code = gc_message_cat_z59.
      WHEN OTHERS.
    ENDCASE.

    sis_process_step_data-document_ident = siv_refno.


    IF siv_mandatory_data EQ abap_true.
      IF sis_process_step_data-docname_code IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-017 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDIF.

      IF sis_process_step_data-document_ident IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-018 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD mr_relevance_and_use.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: this method is used to set Market Role Relevance
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

    DATA:
      ls_reg_code_data  TYPE /idxgc/s_reg_code_details,
      ls_data_relevance TYPE /idxgl/s_data_rel_details,
      ls_pod_data       TYPE /idxgl/s_pod_data_details.

    DATA: lo_badi_utilts_snd TYPE REF TO /hfq/badi_utilts_snd.

    FIELD-SYMBOLS:
      <ls_data_relevance>         TYPE /idxgl/s_data_rel_details,
      <ls_data_relevance_src>     TYPE /idxgl/s_data_rel_details,
      <ls_data_relevance_src_add> TYPE /idxgl/s_data_rel_details.

    IF siv_meter_proc IS INITIAL.
      me->get_metering_procedure_details( ).
    ENDIF.

    CHECK siv_meter_proc = /idxgc/if_constants_add=>gc_meter_proc_e01
       OR siv_meter_proc = /idxgc/if_constants_add=>gc_meter_proc_e02
       OR siv_meter_proc = /idxgc/if_constants_add=>gc_meter_proc_e14
       OR siv_meter_proc = /idxgc/if_constants_add=>gc_meter_proc_e24.

    IF siv_context_seq =  /idxgl/if_constants_ide=>gc_seq_action_code_z36.
* Get forecast basis
      GET BADI lo_badi_utilts_snd.
      TRY.

          READ TABLE sis_process_step_data-marketpartner INTO DATA(ls_mr_data)
          WITH KEY party_func_qual = /idxgc/if_constants_ide=>gc_nad_qual_mr.

          IF sy-subrc = 0.

            CALL BADI lo_badi_utilts_snd->provide_mr_relevance_and_use
              EXPORTING
                iv_int_ui_malo    = sis_process_step_data-int_ui
                iv_proc_date      = sis_process_step_data-proc_date
                iv_mr_serviceid   = ls_mr_data-serviceid
                iv_itemid         = siv_itemid
                iv_context_seq    = siv_context_seq
                iv_data_mrrel     = /idxgl/if_constants_ide=>gc_data_mrrel_za7
              IMPORTING
                et_data_relevance = DATA(lt_relevance).

            APPEND LINES OF lt_relevance TO sis_process_step_data-/idxgl/data_relevance.
            SORT sis_process_step_data-/idxgl/data_relevance.
            DELETE ADJACENT DUPLICATES FROM sis_process_step_data-/idxgl/data_relevance.
          ENDIF.

        CATCH /idxgc/cx_general.
          " Fehler bei Aufruf des BAdI &1
          MESSAGE e003(/hfq/utilts_snd) WITH '/hfq/badi_utilts_snd->provide_mr_relevance_and_use'.
          EXIT.
      ENDTRY.
*      READ TABLE sis_process_step_data-/idxgl/pod_data INTO ls_pod_data
*        WITH KEY data_type_qual = /idxgc/if_constants_ide=>gc_seq_action_code_z01.
*      ls_data_relevance-item_id = siv_itemid.
*      ls_data_relevance-ext_ui = ls_reg_code_data-ext_ui.
*      ls_data_relevance-data_type_qual = siv_context_seq.
*      ls_data_relevance-data_mrrel = /idxgl/if_constants_ide=>gc_data_mrrel_za7.
*
*      IF ls_pod_data-forecast_basis = /idxgl/if_constants_ide=>gc_cci_chardesc_code_zc0.
*        ls_data_relevance-data_use = /idxgl/if_constants_ide=>gc_data_use_z85.
*      ELSEIF ls_pod_data-forecast_basis = /idxgl/if_constants_ide=>gc_cci_chardesc_code_za6.
*        ls_data_relevance-data_use = /idxgl/if_constants_ide=>gc_data_use_z86.
*      ENDIF.
*      IF ls_data_relevance-data_use IS NOT INITIAL.
*        APPEND ls_data_relevance TO sis_process_step_data-/idxgl/data_relevance.
*      ENDIF.
*
*      ls_data_relevance-data_use = /idxgl/if_constants_ide=>gc_data_use_z84.
*      APPEND ls_data_relevance TO sis_process_step_data-/idxgl/data_relevance.
*      ls_data_relevance-data_use = /idxgl/if_constants_ide=>gc_data_use_z47.
*      APPEND ls_data_relevance TO sis_process_step_data-/idxgl/data_relevance.
*
*      SORT sis_process_step_data-/idxgl/data_relevance.
*      DELETE ADJACENT DUPLICATES FROM sis_process_step_data-/idxgl/data_relevance.

    ENDIF.

*------------------ Check if data is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-/idxgl/data_relevance ASSIGNING <ls_data_relevance>
          WHERE item_id         = siv_itemid
            AND data_type_qual  = siv_context_seq
            AND ( data_mrrel IS INITIAL
             OR data_use IS INITIAL ).
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-010 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD provide_supply_direct.

    CASE sis_process_step_data-sup_direct_int.
      WHEN /idxgc/if_constants_add=>gc_sup_direct_supply.
        rv_supply_direct = /idxgc/if_constants_add=>gc_supply_direct_z07.

      WHEN /idxgc/if_constants_add=>gc_sup_direct_feeding.
        rv_supply_direct = /idxgc/if_constants_add=>gc_supply_direct_z06.

      WHEN OTHERS.
        SELECT SINGLE bezug FROM eanl
          INNER JOIN euiinstln ON euiinstln~anlage = eanl~anlage
          WHERE euiinstln~int_ui = @sis_process_step_data-int_ui
          INTO @DATA(lv_bezug).

        IF lv_bezug IS INITIAL.
          sis_process_step_data-sup_direct_int = /idxgc/if_constants_add=>gc_sup_direct_supply.

          rv_supply_direct = /idxgc/if_constants_add=>gc_supply_direct_z07.
        ELSE.
          sis_process_step_data-sup_direct_int = /idxgc/if_constants_add=>gc_sup_direct_feeding.

          rv_supply_direct = /idxgc/if_constants_add=>gc_supply_direct_z06.
        ENDIF.

    ENDCASE.
  ENDMETHOD.


  METHOD reference_to_calc_step.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: This method is used for a reference to another calculation step of the formula.
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

    DATA: ls_diverse TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS: <fs_diverse>     TYPE /idxgc/s_diverse_details,
                   <fs_diverse_src> TYPE /idxgc/s_diverse_details.


    CASE siv_context_seq.
      WHEN /idxgl/if_constants_ide=>gc_seq_action_code_z36.
        READ TABLE sis_process_step_data-diverse ASSIGNING <fs_diverse>
          WITH KEY item_id = siv_itemid.
        IF sy-subrc = 0.
          IF <fs_diverse>-calc_step_amount_malo IS INITIAL AND <fs_diverse>-calc_form_stat = gc_calcform_stat_z33.
            <fs_diverse>-calc_step_amount_malo = sis_calc_form_h-calc_step_amount_malo.
          ENDIF.
        ELSE.
          ls_diverse-item_id = siv_itemid.
          ls_diverse-calc_step_amount_malo = sis_calc_form_h-calc_step_amount_malo.
          APPEND ls_diverse TO sis_process_step_data-diverse.
        ENDIF.
      WHEN /idxgl/if_constants_ide=>gc_seq_action_code_z37.

    ENDCASE.

*------------------ CHECK if DATA is mandatory and filled ------------------*
*<<< 2020-03-04 Heidemueller Übernahme Entwicklungen von Ralf Seiler
*    IF siv_mandatory_data = abap_true.
    IF siv_mandatory_data = abap_true AND
      sis_calc_form_h-calc_form_stat EQ gc_calcform_stat_z33. " for not-complex formula calc step needed!
*>>>
      LOOP AT sis_process_step_data-diverse TRANSPORTING NO FIELDS
        WHERE item_id     = siv_itemid
          AND calc_step_amount_malo IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-009 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD reference_to_pod.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: Set reference to point of delivery
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

* Data was filled in method CALC_STEP_SEQ
* this method is, by default, only used for the mandatory-field check.

*------------------ Check if data is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-/idxgl/pod_data TRANSPORTING NO FIELDS
        WHERE item_id     = siv_itemid
          AND data_type_qual = siv_context_seq
          AND ext_ui IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-012 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD reference_to_transaction.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: this method is used to set Reference No. to Transaction Number
*------------------------------------------------------------------------------*
*  Change History:
*  Sep. 2019: Created
*------------------------------------------------------------------------------*
    DATA: ls_diverse TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS: <ls_diverse_src> TYPE /idxgc/s_diverse_details,
                   <ls_diverse>     TYPE /idxgc/s_diverse_details.

* get data from source step data
    READ TABLE sis_process_data_src-diverse ASSIGNING <ls_diverse_src> INDEX 1.
    IF sy-subrc = 0 AND <ls_diverse_src>-transaction_no IS NOT INITIAL.
      READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
      IF sy-subrc = 0.
        <ls_diverse>-refnr_transreq = <ls_diverse_src>-transaction_no.
      ELSE.
        ls_diverse-item_id = siv_itemid.
        ls_diverse-refnr_transreq = <ls_diverse_src>-transaction_no.
        APPEND ls_diverse TO sis_process_step_data-diverse.
      ENDIF.
    ENDIF.

*------------------ Check if data is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-diverse TRANSPORTING NO FIELDS
      WHERE refnr_transreq IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-006 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD status_of_answer.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: Determine the status of the answer
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

    DATA: ls_msgrespstatus TYPE /idxgc/s_msgsts_details.
    FIELD-SYMBOLS: <fs_msgrespstatus>   TYPE /idxgc/s_msgsts_details.

    READ TABLE sis_process_step_data-msgrespstatus
    ASSIGNING <fs_msgrespstatus> WITH KEY item_id = siv_itemid.
    IF sy-subrc = 0.
      <fs_msgrespstatus>-respstatus = <fs_msgrespstatus>-respstatus.
    ELSE.
      "No entry means no negative answer, add acceptance:
      ls_msgrespstatus-item_id = siv_itemid.
      ls_msgrespstatus-respstatus = gc_answer_stat_e15.
      APPEND ls_msgrespstatus TO sis_process_step_data-msgrespstatus.
    ENDIF.

*------------------ CHECK if DATA is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-msgrespstatus TRANSPORTING NO FIELDS
        WHERE item_id     = siv_itemid
          AND respstatus IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-004 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD status_of_calcform.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: Fills the status of the Calculation formula
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

    DATA: ls_diverse TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS: <fs_diverse> TYPE /idxgc/s_diverse_details.


    READ TABLE sis_process_step_data-diverse
      ASSIGNING <fs_diverse> WITH KEY item_id = siv_itemid.
    IF sy-subrc = 0.
      <fs_diverse>-calc_form_stat = sis_calc_form_h-calc_form_stat.
    ELSE.
      " Missing Diverse,
      ls_diverse-item_id = siv_itemid.
      ls_diverse-calc_form_stat = sis_calc_form_h-calc_form_stat.
      APPEND ls_diverse TO sis_process_step_data-diverse.
    ENDIF.


*------------------ CHECK if DATA is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-diverse TRANSPORTING NO FIELDS
        WHERE item_id     = siv_itemid
          AND calc_form_stat IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-005 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD supply_direction.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: this method is used to set the supply direction
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*

    DATA: lv_supply_direct TYPE /idxgc/de_supply_direct,
          ls_diverse       TYPE  /idxgc/s_diverse_details.

    FIELD-SYMBOLS: <fs_diverse> TYPE /idxgc/s_diverse_details.

    CLEAR lv_supply_direct.
    CASE sis_process_step_data-sup_direct_int.
      WHEN /idxgc/if_constants_add=>gc_sup_direct_supply.
        lv_supply_direct = /idxgc/if_constants_add=>gc_supply_direct_z07.
      WHEN /idxgc/if_constants_add=>gc_sup_direct_feeding.
        lv_supply_direct = /idxgc/if_constants_add=>gc_supply_direct_z06.
      WHEN OTHERS.
        SELECT SINGLE bezug FROM eanl
          INNER JOIN euiinstln ON euiinstln~anlage = eanl~anlage
          WHERE euiinstln~int_ui = @sis_process_step_data-int_ui
          INTO @DATA(lv_bezug).
        IF lv_bezug IS INITIAL.
          lv_supply_direct = /idxgc/if_constants_add=>gc_supply_direct_z07.
        ELSE.
          lv_supply_direct = /idxgc/if_constants_add=>gc_supply_direct_z06.
        ENDIF.
    ENDCASE.

    READ TABLE sis_process_step_data-diverse ASSIGNING <fs_diverse>
        WITH KEY item_id = siv_itemid.
    IF sy-subrc = 0.
      <fs_diverse>-supply_direct = lv_supply_direct.
    ELSE.
      ls_diverse-item_id = siv_itemid.
      ls_diverse-supply_direct = lv_supply_direct.
      APPEND ls_diverse TO sis_process_step_data-diverse.
    ENDIF.

*------------------ Check if data is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-diverse ASSIGNING <fs_diverse>
          WHERE item_id         = siv_itemid
            AND supply_direct IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-007 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD tou_definition_code.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Time-of-Use Code
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_tou TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src_add> TYPE /idxgl/s_tou_details.

    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou ASSIGNING <ls_tou_src>.
          READ TABLE sis_process_step_data-/idxgl/tou TRANSPORTING NO FIELDS
              WITH KEY item_id        = <ls_tou_src>-item_id
                       data_type_qual = <ls_tou_src>-data_type_qual
                       tou_code       = <ls_tou_src>-tou_code.
          IF sy-subrc <> 0.
            ls_tou = VALUE #( item_id        = <ls_tou_src>-item_id
                              data_type_qual = <ls_tou_src>-data_type_qual
                              tou_code       = <ls_tou_src>-tou_code ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou ASSIGNING <ls_tou_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou TRANSPORTING NO FIELDS
              WITH KEY item_id        = <ls_tou_src_add>-item_id
                       data_type_qual = <ls_tou_src_add>-data_type_qual
                       tou_code       = <ls_tou_src_add>-tou_code.
          IF sy-subrc <> 0.
            ls_tou = VALUE #( item_id        = <ls_tou_src_add>-item_id
                              data_type_qual = <ls_tou_src_add>-data_type_qual
                              tou_code       = <ls_tou_src_add>-tou_code ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
        LOOP AT sis_process_data_src-/idxgl/tou ASSIGNING <ls_tou_src>.
          READ TABLE sis_process_step_data-/idxgl/tou TRANSPORTING NO FIELDS
              WITH KEY item_id        = <ls_tou_src>-item_id
                       data_type_qual = <ls_tou_src>-data_type_qual
                       tou_code       = <ls_tou_src>-tou_code.
          IF sy-subrc <> 0.
            ls_tou = VALUE #( item_id        = <ls_tou_src>-item_id
                              data_type_qual = <ls_tou_src>-data_type_qual    "<ls_tou_src>-data_type_qual
                              tou_code       = <ls_tou_src>-tou_code ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
    ENDCASE.
    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou> WHERE tou_code IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-102 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD tou_def_type.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Time-of-Use Definition Type.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    FIELD-SYMBOLS <ls_tou_data> TYPE /hfq/s_tou_data.
    FIELD-SYMBOLS <ls_tou_versiondata> TYPE /hfq/s_tou_versiondata.
    DATA ls_tou TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src_add> TYPE /idxgl/s_tou_details.

    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou ASSIGNING <ls_tou_src>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src>-item_id
                       data_type_qual = <ls_tou_src>-data_type_qual
                       tou_code       = <ls_tou_src>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_type = <ls_tou_src>-tou_type.
          ELSE.
            ls_tou = VALUE #( item_id        = <ls_tou_src>-item_id
                              data_type_qual = <ls_tou_src>-data_type_qual
                              tou_code       = <ls_tou_src>-tou_code
                              tou_type       = <ls_tou_src>-tou_type ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou ASSIGNING <ls_tou_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src_add>-item_id
                       data_type_qual = <ls_tou_src_add>-data_type_qual
                       tou_code       = <ls_tou_src_add>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_type = <ls_tou_src_add>-tou_type.
          ELSE.
            ls_tou = VALUE #( item_id        = <ls_tou_src_add>-item_id
                              data_type_qual = <ls_tou_src_add>-data_type_qual
                              tou_code       = <ls_tou_src_add>-tou_code
                              tou_type       = <ls_tou_src_add>-tou_type ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
        IF mt_tou_data IS INITIAL.
          get_time_of_use_data( ).
        ENDIF.

        LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>.
          LOOP AT mt_tou_data ASSIGNING <ls_tou_data> WHERE tou_code EQ <ls_tou>-tou_code.
            READ TABLE <ls_tou_data>-versiondata ASSIGNING <ls_tou_versiondata> INDEX 1.
            IF sy-subrc EQ 0.
              <ls_tou>-tou_type = <ls_tou_versiondata>-type.
              IF <ls_tou>-tou_type EQ /hfq/cl_dp_utilts=>gc_tou_type_else.
                <ls_tou>-tou_type_desc = <ls_tou_versiondata>-type_desc.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou> WHERE tou_type IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-108 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tou_hp_usage.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Determination of Maximum Demand for
**        Atypical Grid Usage.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_tou TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src_add> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_data> TYPE /hfq/s_tou_data.
    FIELD-SYMBOLS <ls_tou_versiondata> TYPE /hfq/s_tou_versiondata.

    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou ASSIGNING <ls_tou_src>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src>-item_id
                       data_type_qual = <ls_tou_src>-data_type_qual
                       tou_code       = <ls_tou_src>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_max_demand = <ls_tou_src>-tou_max_demand.
          ELSE.
            ls_tou = VALUE #( item_id        = <ls_tou_src>-item_id
                              data_type_qual = <ls_tou_src>-data_type_qual
                              tou_code       = <ls_tou_src>-tou_code
                              tou_max_demand = <ls_tou_src>-tou_max_demand ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou ASSIGNING <ls_tou_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src_add>-item_id
                       data_type_qual = <ls_tou_src_add>-data_type_qual
                       tou_code       = <ls_tou_src_add>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_max_demand = <ls_tou_src_add>-tou_max_demand.
          ELSE.
            ls_tou = VALUE #( item_id        = <ls_tou_src_add>-item_id
                              data_type_qual = <ls_tou_src_add>-data_type_qual
                              tou_code       = <ls_tou_src_add>-tou_code
                              tou_max_demand = <ls_tou_src_add>-tou_max_demand ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
        IF mt_tou_data IS INITIAL.
          get_time_of_use_data( ).
        ENDIF.

        LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>.
          LOOP AT mt_tou_data ASSIGNING <ls_tou_data> WHERE tou_code EQ <ls_tou>-tou_code.
            READ TABLE <ls_tou_data>-versiondata ASSIGNING <ls_tou_versiondata> INDEX 1.
            IF sy-subrc EQ 0.
              <ls_tou>-tou_max_demand = <ls_tou_versiondata>-max_demand.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou> WHERE tou_max_demand IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-106 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tou_lightload_compatible.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Light Load Capability.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_tou_reg TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_reg> TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_reg_src> TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_reg_src_add> TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_data> TYPE /hfq/s_tou_data.
    FIELD-SYMBOLS <ls_tou_versiondata> TYPE /hfq/s_tou_versiondata.
    FIELD-SYMBOLS <ls_tou_registerdata> TYPE /hfq/s_tou_registerdata.

    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou_reg ASSIGNING <ls_tou_reg_src>.
          READ TABLE sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg>
              WITH KEY item_id        = <ls_tou_reg_src>-item_id
                       data_type_qual = <ls_tou_reg_src>-data_type_qual
                       tou_code       = <ls_tou_reg_src>-tou_code
                       tou_reg_code   = <ls_tou_reg_src>-tou_reg_code.
          IF sy-subrc EQ 0.
            <ls_tou_reg>-tarif_alloc = <ls_tou_reg_src>-tarif_alloc.
          ELSE.
            ls_tou_reg = VALUE #( tour_key    = <ls_tou_reg_src>-tour_key
                                  tarif_alloc = <ls_tou_reg_src>-tarif_alloc ).
            APPEND ls_tou_reg TO sis_process_step_data-/idxgl/tou_reg.
          ENDIF.
        ENDLOOP.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou_reg ASSIGNING <ls_tou_reg_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg>
              WITH KEY item_id        = <ls_tou_reg_src_add>-item_id
                       data_type_qual = <ls_tou_reg_src_add>-data_type_qual
                       tou_code       = <ls_tou_reg_src_add>-tou_code
                       tou_reg_code   = <ls_tou_reg_src_add>-tou_reg_code.
          IF sy-subrc EQ 0.
            <ls_tou_reg>-tarif_alloc = <ls_tou_reg_src_add>-tarif_alloc.
          ELSE.
            ls_tou_reg = VALUE #( tour_key    = <ls_tou_reg_src_add>-tour_key
                                  tarif_alloc = <ls_tou_reg_src_add>-tarif_alloc ).
            APPEND ls_tou_reg TO sis_process_step_data-/idxgl/tou_reg.
          ENDIF.
        ENDLOOP.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
        IF mt_tou_data IS INITIAL.
          get_time_of_use_data( ).
        ENDIF.

        LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING FIELD-Symbol(<ls_tou>).
          LOOP AT mt_tou_data ASSIGNING <ls_tou_data> WHERE tou_code EQ <ls_tou>-tou_code.
            READ TABLE <ls_tou_data>-versiondata ASSIGNING <ls_tou_versiondata> INDEX 1.
            IF sy-subrc EQ 0.
              LOOP AT <ls_tou_versiondata>-registerdata ASSIGNING <ls_tou_registerdata>.
                READ TABLE sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg>
                WITH KEY item_id        = <ls_tou>-item_id
                         tou_code       = <ls_tou>-tou_code
                         tou_reg_code   = <ls_tou_registerdata>-tou_register.
                IF sy-subrc EQ 0.
                  <ls_tou_reg>-tarif_alloc = <ls_tou_registerdata>-tarif_alloc.
                ELSE.
                  ls_tou_reg = VALUE #(
                    item_id        = <ls_tou>-item_id
                    tou_code       = <ls_tou>-tou_code
                    data_type_qual = /hfq/if_tou_constants=>co_data_type_qual_z38
                    tou_reg_code   = <ls_tou_registerdata>-reg_id
                    tarif_alloc    = <ls_tou_registerdata>-tarif_alloc
                  ).
                  APPEND ls_tou_reg TO sis_process_step_data-/idxgl/tou_reg.
                ENDIF.

              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg> WHERE tarif_alloc IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-110 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tou_orderable.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Time-of-Use Orderability.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_tou TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src_add> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_data> TYPE /hfq/s_tou_data.
    FIELD-SYMBOLS <ls_tou_versiondata> TYPE /hfq/s_tou_versiondata.

    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou ASSIGNING <ls_tou_src>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src>-item_id
                       data_type_qual = <ls_tou_src>-data_type_qual
                       tou_code       = <ls_tou_src>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_orderability = <ls_tou_src>-tou_orderability.
          ELSE.
            ls_tou = VALUE #( item_id          = <ls_tou_src>-item_id
                              data_type_qual   = <ls_tou_src>-data_type_qual
                              tou_code         = <ls_tou_src>-tou_code
                              tou_orderability = <ls_tou_src>-tou_orderability ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou ASSIGNING <ls_tou_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src_add>-item_id
                       data_type_qual = <ls_tou_src_add>-data_type_qual
                       tou_code       = <ls_tou_src_add>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_orderability = <ls_tou_src_add>-tou_orderability.
          ELSE.
            ls_tou = VALUE #( item_id          = <ls_tou_src_add>-item_id
                              data_type_qual   = <ls_tou_src_add>-data_type_qual
                              tou_code         = <ls_tou_src_add>-tou_code
                              tou_orderability = <ls_tou_src_add>-tou_orderability ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
        IF mt_tou_data IS INITIAL.
          get_time_of_use_data( ).
        ENDIF.

        LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>.
          LOOP AT mt_tou_data ASSIGNING <ls_tou_data> WHERE tou_code EQ <ls_tou>-tou_code.
            READ TABLE <ls_tou_data>-versiondata ASSIGNING <ls_tou_versiondata> INDEX 1.
            IF sy-subrc EQ 0.
              <ls_tou>-tou_orderability = <ls_tou_versiondata>-orderability.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou> WHERE tou_orderability IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-107 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tou_register_code.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Time-of-Use Register Code.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_tou_reg TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_reg> TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_reg_src> TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_reg_src_add> TYPE /idxgl/s_tour_details.

    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou_reg ASSIGNING <ls_tou_reg_src>.
          READ TABLE sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg>
              WITH KEY item_id        = <ls_tou_reg_src>-item_id
                       data_type_qual = <ls_tou_reg_src>-data_type_qual
                       tou_code       = <ls_tou_reg_src>-tou_code
                       tou_reg_code   = <ls_tou_reg_src>-tou_reg_code.
          IF sy-subrc <> 0.
            ls_tou_reg = VALUE #( tour_key = <ls_tou_reg_src>-tour_key ).
            APPEND ls_tou_reg TO sis_process_step_data-/idxgl/tou_reg.
          ENDIF.
        ENDLOOP.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou_reg ASSIGNING <ls_tou_reg_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg>
              WITH KEY item_id        = <ls_tou_reg_src_add>-item_id
                       data_type_qual = <ls_tou_reg_src_add>-data_type_qual
                       tou_code       = <ls_tou_reg_src_add>-tou_code
                       tou_reg_code   = <ls_tou_reg_src_add>-tou_reg_code.
          IF sy-subrc <> 0.
            ls_tou_reg = VALUE #( tour_key = <ls_tou_reg_src_add>-tour_key ).
            APPEND ls_tou_reg TO sis_process_step_data-/idxgl/tou_reg.
          ENDIF.
        ENDLOOP.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
        LOOP AT sis_process_data_src-/idxgl/tou_reg ASSIGNING <ls_tou_reg_src>.
          READ TABLE sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg>
              WITH KEY item_id        = <ls_tou_reg_src>-item_id
                       data_type_qual = <ls_tou_reg_src>-data_type_qual
                       tou_code       = <ls_tou_reg_src>-tou_code
                       tou_reg_code   = <ls_tou_reg_src>-tou_reg_code.
          IF sy-subrc <> 0.
            ls_tou_reg = VALUE #( tour_key = <ls_tou_reg_src>-tour_key ).
            APPEND ls_tou_reg TO sis_process_step_data-/idxgl/tou_reg.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg> WHERE tou_reg_code IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-109 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tou_register_ref.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Time-of-Use Code.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_tou_reg TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_reg> TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_reg_src> TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_reg_src_add> TYPE /idxgl/s_tour_details.
    FIELD-SYMBOLS <ls_tou_data> TYPE /hfq/s_tou_data.
    FIELD-SYMBOLS <ls_tou_versiondata> TYPE /hfq/s_tou_versiondata.
    FIELD-SYMBOLS <ls_tou_registerdata> TYPE /hfq/s_tou_registerdata.

    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou_reg ASSIGNING <ls_tou_reg_src>.
          READ TABLE sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg>
              WITH KEY item_id        = <ls_tou_reg_src>-item_id
                       data_type_qual = <ls_tou_reg_src>-data_type_qual
                       tou_code       = <ls_tou_reg_src>-tou_code
                       tou_reg_code   = <ls_tou_reg_src>-tou_reg_code.
          IF sy-subrc <> 0.
            ls_tou_reg = VALUE #( tour_key = <ls_tou_reg_src>-tour_key ).
            APPEND ls_tou_reg TO sis_process_step_data-/idxgl/tou_reg.
          ENDIF.
        ENDLOOP.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou_reg ASSIGNING <ls_tou_reg_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg>
              WITH KEY item_id        = <ls_tou_reg_src_add>-item_id
                       data_type_qual = <ls_tou_reg_src_add>-data_type_qual
                       tou_code       = <ls_tou_reg_src_add>-tou_code
                       tou_reg_code   = <ls_tou_reg_src_add>-tou_reg_code.
          IF sy-subrc <> 0.
            ls_tou_reg = VALUE #( tour_key = <ls_tou_reg_src_add>-tour_key ).
            APPEND ls_tou_reg TO sis_process_step_data-/idxgl/tou_reg.
          ENDIF.
        ENDLOOP.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
        IF mt_tou_data IS INITIAL.
          get_time_of_use_data( ).
        ENDIF.

        LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING FIELD-Symbol(<ls_tou>).
          LOOP AT mt_tou_data ASSIGNING <ls_tou_data> WHERE tou_code EQ <ls_tou>-tou_code.
            READ TABLE <ls_tou_data>-versiondata ASSIGNING <ls_tou_versiondata> INDEX 1.
            IF sy-subrc EQ 0.
              LOOP AT <ls_tou_versiondata>-registerdata ASSIGNING <ls_tou_registerdata>.
                READ TABLE sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg>
                WITH KEY item_id        = <ls_tou>-item_id
                         tou_code       = <ls_tou>-tou_code
                         tou_reg_code   = <ls_tou_registerdata>-tou_register.
                IF sy-subrc <> 0.
                  ls_tou_reg = VALUE #(
                    item_id        = <ls_tou>-item_id
                    tou_code       = <ls_tou>-tou_code
                    data_type_qual = 'Z41'
*                    data_type_qual = /hfq/if_tou_constants=>co_data_type_qual_z38
                    tou_reg_code   = <ls_tou_registerdata>-tou_register
                  ).
                  APPEND ls_tou_reg TO sis_process_step_data-/idxgl/tou_reg.
                ENDIF.

              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou_reg ASSIGNING <ls_tou_reg> WHERE tou_reg_code IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-102 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tou_r_active_registers.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Time-of-Use Code.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_tou_r_out TYPE /idxgl/s_touo_details.
    FIELD-SYMBOLS <ls_tou_r_out> TYPE /idxgl/s_touo_details.
    FIELD-SYMBOLS <ls_tou_r_out_src> TYPE /idxgl/s_touo_details.
    FIELD-SYMBOLS <ls_tou_r_out_src_add> TYPE /idxgl/s_touo_details.

    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou_r_out ASSIGNING <ls_tou_r_out_src>.
          READ TABLE sis_process_step_data-/idxgl/tou_r_out ASSIGNING <ls_tou_r_out>
              WITH KEY item_id          = <ls_tou_r_out_src>-item_id
                       data_type_qual   = <ls_tou_r_out_src>-data_type_qual
                       tou_change_count = <ls_tou_r_out_src>-tou_change_count.
          IF sy-subrc EQ 0.
            <ls_tou_r_out>-touo_data = <ls_tou_r_out_src>-touo_data.
          ELSE.
            APPEND <ls_tou_r_out_src> TO sis_process_step_data-/idxgl/tou_r_out.
          ENDIF.
        ENDLOOP.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou_r_out ASSIGNING <ls_tou_r_out_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou_r_out ASSIGNING <ls_tou_r_out>
              WITH KEY item_id          = <ls_tou_r_out_src_add>-item_id
                       data_type_qual   = <ls_tou_r_out_src_add>-data_type_qual
                       tou_change_count = <ls_tou_r_out_src_add>-tou_change_count.
          IF sy-subrc EQ 0.
            <ls_tou_r_out>-touo_data = <ls_tou_r_out_src_add>-touo_data.
          ELSE.
            APPEND <ls_tou_r_out_src_add> TO sis_process_step_data-/idxgl/tou_r_out.
          ENDIF.
        ENDLOOP.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
        LOOP AT sis_process_data_src-/idxgl/tou_r_out ASSIGNING <ls_tou_r_out_src>.
          READ TABLE sis_process_step_data-/idxgl/tou_r_out ASSIGNING <ls_tou_r_out>
              WITH KEY item_id          = <ls_tou_r_out_src>-item_id
                       data_type_qual   = <ls_tou_r_out_src>-data_type_qual
                       tou_change_count = <ls_tou_r_out_src>-tou_change_count.
          IF sy-subrc EQ 0.
            <ls_tou_r_out_src>-data_type_qual = 'Z43'.
            <ls_tou_r_out>-touo_data = <ls_tou_r_out_src>-touo_data.
          ELSE.
            <ls_tou_r_out_src>-data_type_qual = 'Z43'.
            APPEND <ls_tou_r_out_src> TO sis_process_step_data-/idxgl/tou_r_out.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou_r_out ASSIGNING <ls_tou_r_out> WHERE touo_data IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-102 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tou_r_order_reference.
*------------------------------------------------------------------------------*
** Author: SAP Innovative Business Solutions, Aug. 2019
**
** Usage: This method is used to fill ORDERS reference
*------------------------------------------------------------------------------*
*  Change History:
*  Jan. 2022: Created
*------------------------------------------------------------------------------*
    DATA ls_ref_to_msg         TYPE /idxgc/s_ref2msg_details.
    DATA ls_ref_to_msg_src     TYPE /idxgc/s_ref2msg_details.
    DATA ls_ref_to_msg_src_add TYPE /idxgc/s_ref2msg_details.

    FIELD-SYMBOLS <ls_ref_to_msg> TYPE /idxgc/s_ref2msg_details.

    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        READ TABLE sis_process_data_src-ref_to_msg
           INTO ls_ref_to_msg_src WITH KEY ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi. "AGI

        READ TABLE sis_process_step_data-ref_to_msg
             ASSIGNING <ls_ref_to_msg> WITH KEY ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi.
        IF sy-subrc = 0.
          <ls_ref_to_msg>-ref_no = ls_ref_to_msg_src-ref_no.
        ELSE.
          ls_ref_to_msg-ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi.
          ls_ref_to_msg-ref_no = ls_ref_to_msg_src-ref_no.
          APPEND ls_ref_to_msg TO sis_process_step_data-ref_to_msg.
        ENDIF.

* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        READ TABLE sis_process_data_src_add-ref_to_msg
           INTO ls_ref_to_msg_src_add WITH KEY ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi. "AGI

        READ TABLE sis_process_step_data-ref_to_msg
             ASSIGNING <ls_ref_to_msg> WITH KEY ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi.
        IF sy-subrc = 0.
          <ls_ref_to_msg>-ref_no = ls_ref_to_msg_src_add-ref_no.
        ELSE.
          ls_ref_to_msg-ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi.
          ls_ref_to_msg-ref_no = ls_ref_to_msg_src_add-ref_no.
          APPEND ls_ref_to_msg TO sis_process_step_data-ref_to_msg.
        ENDIF.

* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
* Get data from source step
        READ TABLE sis_process_data_src-ref_to_msg
           INTO ls_ref_to_msg_src WITH KEY ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi. "AGI

        READ TABLE sis_process_step_data-ref_to_msg
             ASSIGNING <ls_ref_to_msg> WITH KEY ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi.
        IF sy-subrc = 0.
          <ls_ref_to_msg>-ref_no = ls_ref_to_msg_src-ref_no.
        ELSE.
          ls_ref_to_msg-ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi.
          ls_ref_to_msg-ref_no = ls_ref_to_msg_src-ref_no.
          APPEND ls_ref_to_msg TO sis_process_step_data-ref_to_msg.
        ENDIF.

    ENDCASE.

* Check whether the field is required, otherwise raise exception for the missing field.
    READ TABLE sis_process_step_data-ref_to_msg INTO ls_ref_to_msg
         WITH KEY ref_qual = /idxgl/if_constants_ide=>gc_rff_qual_agi.
    IF sy-subrc <> 0.
      CLEAR ls_ref_to_msg.
    ENDIF.
    IF ( siv_mandatory_data = abap_true ) AND ( ls_ref_to_msg-ref_no IS INITIAL ).
      MESSAGE e038(/idxgc/ide_add) WITH TEXT-111 INTO siv_mtext.
      /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
    ENDIF.
  ENDMETHOD.


  METHOD tou_r_valid_from_date.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set tou from date z34
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_diverse TYPE /idxgc/s_diverse_details.

    FIELD-SYMBOLS <ls_diverse> TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS <ls_diverse_src> TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS <ls_diverse_src_add> TYPE /idxgc/s_diverse_details.
*
    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src> INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc EQ 0.
            <ls_diverse>-ro_tou_start_date = <ls_diverse_src>-ro_tou_start_date.
            <ls_diverse>-ro_tou_start_time = <ls_diverse_src>-ro_tou_start_time.
            <ls_diverse>-ro_tou_start_offs = <ls_diverse_src>-ro_tou_start_offs.
          ELSE.
            ls_diverse = VALUE #(
              item_id = siv_itemid
              version_date = <ls_diverse_src>-ro_tou_start_date
              version_time = <ls_diverse_src>-ro_tou_start_time
              version_offs = <ls_diverse_src>-ro_tou_start_offs
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src_add> INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc EQ 0.
            <ls_diverse>-ro_tou_start_date = <ls_diverse_src_add>-ro_tou_start_date.
            <ls_diverse>-ro_tou_start_time = <ls_diverse_src_add>-ro_tou_start_time.
            <ls_diverse>-ro_tou_start_offs = <ls_diverse_src_add>-ro_tou_start_offs.
          ELSE.
            ls_diverse = VALUE #(
              item_id = siv_itemid
              version_date = <ls_diverse_src_add>-ro_tou_start_date
              version_time = <ls_diverse_src_add>-ro_tou_start_time
              version_offs = <ls_diverse_src_add>-ro_tou_start_offs
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.


          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc eq 0.
            <ls_diverse>-ro_tou_start_date = <ls_diverse>-ro_tou_start_date.
            <ls_diverse>-ro_tou_start_time = <ls_diverse>-ro_tou_start_time.
            <ls_diverse>-ro_tou_start_offs = <ls_diverse>-ro_tou_start_offs.
          ELSE.
              READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src> INDEX 1.
        IF sy-subrc EQ 0.
            ls_diverse = VALUE #(
              item_id = siv_itemid
              version_date = <ls_diverse_src>-ro_tou_start_date
              version_time = <ls_diverse_src>-ro_tou_start_time
              version_offs = <ls_diverse_src>-ro_tou_start_offs
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
      IF sy-subrc <> 0 OR
        <ls_diverse>-ro_tou_start_date IS INITIAL OR
        <ls_diverse>-ro_tou_start_offs IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-112 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD tou_r_valid_to_date.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set tou to date z35
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_diverse TYPE /idxgc/s_diverse_details.

    FIELD-SYMBOLS <ls_diverse> TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS <ls_diverse_src> TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS <ls_diverse_src_add> TYPE /idxgc/s_diverse_details.
*
    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src> INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc EQ 0.
            <ls_diverse>-ro_tou_end_date = <ls_diverse_src>-ro_tou_end_date.
            <ls_diverse>-ro_tou_end_time = <ls_diverse_src>-ro_tou_end_time.
            <ls_diverse>-ro_tou_end_offs = <ls_diverse_src>-ro_tou_end_offs.
          ELSE.
            ls_diverse = VALUE #(
              item_id = siv_itemid
              version_date = <ls_diverse_src>-ro_tou_end_date
              version_time = <ls_diverse_src>-ro_tou_end_time
              version_offs = <ls_diverse_src>-ro_tou_end_offs
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src_add> INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc EQ 0.
            <ls_diverse>-ro_tou_end_date = <ls_diverse_src_add>-ro_tou_end_date.
            <ls_diverse>-ro_tou_end_time = <ls_diverse_src_add>-ro_tou_end_time.
            <ls_diverse>-ro_tou_end_offs = <ls_diverse_src_add>-ro_tou_end_offs.
          ELSE.
            ls_diverse = VALUE #(
              item_id = siv_itemid
              version_date = <ls_diverse_src_add>-ro_tou_end_date
              version_time = <ls_diverse_src_add>-ro_tou_end_time
              version_offs = <ls_diverse_src_add>-ro_tou_end_offs
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.


        READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
        IF sy-subrc EQ 0.
          <ls_diverse>-ro_tou_end_date = <ls_diverse>-ro_tou_end_date.
          <ls_diverse>-ro_tou_end_time = <ls_diverse>-ro_tou_end_time.
          <ls_diverse>-ro_tou_end_offs = <ls_diverse>-ro_tou_end_offs.
        ELSE.
          READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src> INDEX 1.
          IF sy-subrc EQ 0.
            ls_diverse = VALUE #(
              item_id = siv_itemid
              version_date = <ls_diverse_src>-ro_tou_end_date
              version_time = <ls_diverse_src>-ro_tou_end_time
              version_offs = <ls_diverse_src>-ro_tou_end_offs
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
      IF sy-subrc <> 0 OR
        <ls_diverse>-ro_tou_end_date IS INITIAL OR
        <ls_diverse>-ro_tou_end_offs IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-113 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD tou_transmission_freq.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Time-of-Use Transmission Frequency.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_tou TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src_add> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_data> TYPE /hfq/s_tou_data.
    FIELD-SYMBOLS <ls_tou_versiondata> TYPE /hfq/s_tou_versiondata.
*
    CASE siv_data_processing_mode.
** Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou ASSIGNING <ls_tou_src>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src>-item_id
                       data_type_qual = <ls_tou_src>-data_type_qual
                       tou_code       = <ls_tou_src>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_trans_freq = <ls_tou_src>-tou_trans_freq.
          ELSE.
            ls_tou = VALUE #( item_id        = <ls_tou_src>-item_id
                              data_type_qual = <ls_tou_src>-data_type_qual
                              tou_code       = <ls_tou_src>-tou_code
                              tou_trans_freq = <ls_tou_src>-tou_trans_freq ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
** Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou ASSIGNING <ls_tou_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src_add>-item_id
                       data_type_qual = <ls_tou_src_add>-data_type_qual
                       tou_code       = <ls_tou_src_add>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_trans_freq = <ls_tou_src_add>-tou_trans_freq.
          ELSE.
            ls_tou = VALUE #( item_id        = <ls_tou_src_add>-item_id
                              data_type_qual = <ls_tou_src_add>-data_type_qual
                              tou_code       = <ls_tou_src_add>-tou_code
                              tou_trans_freq = <ls_tou_src_add>-tou_trans_freq ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
** Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.

        IF mt_tou_data IS INITIAL.
          get_time_of_use_data( ).
        ENDIF.

        LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>.
          LOOP AT mt_tou_data ASSIGNING <ls_tou_data> WHERE tou_code EQ <ls_tou>-tou_code.
            READ TABLE <ls_tou_data>-versiondata ASSIGNING <ls_tou_versiondata> INDEX 1.
            IF sy-subrc EQ 0.
              <ls_tou>-tou_trans_freq = <ls_tou_versiondata>-trans_freq.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou> WHERE tou_trans_freq IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-104 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tou_transmission_way.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Time-of-Use Transmission Possibility.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    DATA ls_tou TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_src_add> TYPE /idxgl/s_tou_details.
    FIELD-SYMBOLS <ls_tou_data> TYPE /hfq/s_tou_data.
    FIELD-SYMBOLS <ls_tou_versiondata> TYPE /hfq/s_tou_versiondata.
*
    CASE siv_data_processing_mode.
** Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        LOOP AT sis_process_data_src-/idxgl/tou ASSIGNING <ls_tou_src>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src>-item_id
                       data_type_qual = <ls_tou_src>-data_type_qual
                       tou_code       = <ls_tou_src>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_trans_poss = <ls_tou_src>-tou_trans_poss.
          ELSE.
            ls_tou = VALUE #( item_id        = <ls_tou_src>-item_id
                              data_type_qual = <ls_tou_src>-data_type_qual
                              tou_code       = <ls_tou_src>-tou_code
                              tou_trans_poss = <ls_tou_src>-tou_trans_poss ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
** Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        LOOP AT sis_process_data_src_add-/idxgl/tou ASSIGNING <ls_tou_src_add>.
          READ TABLE sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>
              WITH KEY item_id        = <ls_tou_src_add>-item_id
                       data_type_qual = <ls_tou_src_add>-data_type_qual
                       tou_code       = <ls_tou_src_add>-tou_code.
          IF sy-subrc EQ 0.
            <ls_tou>-tou_trans_poss = <ls_tou_src_add>-tou_trans_poss.
          ELSE.
            ls_tou = VALUE #( item_id        = <ls_tou_src_add>-item_id
                              data_type_qual = <ls_tou_src_add>-data_type_qual
                              tou_code       = <ls_tou_src_add>-tou_code
                              tou_trans_poss = <ls_tou_src_add>-tou_trans_poss ).
            APPEND ls_tou TO sis_process_step_data-/idxgl/tou.
          ENDIF.
        ENDLOOP.
** Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.
        IF mt_tou_data IS INITIAL.
          get_time_of_use_data( ).
        ENDIF.

        LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou>.
          LOOP AT mt_tou_data ASSIGNING <ls_tou_data> WHERE tou_code EQ <ls_tou>-tou_code.
            READ TABLE <ls_tou_data>-versiondata ASSIGNING <ls_tou_versiondata> INDEX 1.
            IF sy-subrc EQ 0.
              <ls_tou>-tou_trans_poss = <ls_tou_versiondata>-trans_poss.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      LOOP AT sis_process_step_data-/idxgl/tou ASSIGNING <ls_tou> WHERE tou_trans_poss IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-105 INTO siv_mtext.
        /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tou_usage.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2022
**
** Usage: This method is used to Set the Time-of-Use Usage Type.
*------------------------------------------------------------------------------*
*  Change History:
*  Jan 2022: created
*
*------------------------------------------------------------------------------*
    "! Dummy-Felder
    DATA ls_diverse TYPE /idxgc/s_diverse_details.

    FIELD-SYMBOLS <ls_diverse> TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS <ls_diverse_src> TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS <ls_diverse_src_add> TYPE /idxgc/s_diverse_details.

    FIELD-SYMBOLS <ls_tou_data> TYPE /hfq/s_tou_data.
*
    CASE siv_data_processing_mode.
* Get data from source step
      WHEN /idxgc/if_constants_add=>gc_data_from_source.
        READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src> INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc EQ 0.
*            <ls_diverse>-tou_usage = <ls_diverse_src>-tou_usage.
          ELSE.
            ls_diverse = VALUE #(
              item_id = siv_itemid
*              tou_usage = <ls_diverse_src>-tou_usage
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.
* Get data from additional source step
      WHEN /idxgc/if_constants_add=>gc_data_from_add_source.
        READ TABLE sis_process_data_src_add-diverse ASSIGNING <ls_diverse_src_add> INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
          IF sy-subrc EQ 0.
*            <ls_diverse>-tou_usage = <ls_diverse_src_add>-tou_usage.
          ELSE.
            ls_diverse = VALUE #(
              item_id = siv_itemid
*              tou_usage = <ls_diverse_src_add>-tou_usage
            ).
            APPEND ls_diverse TO sis_process_step_data-diverse.
          ENDIF.
        ENDIF.
* Get data from default determination logic
      WHEN /idxgc/if_constants_add=>gc_default_processing.

        IF mt_tou_data IS INITIAL.
          get_time_of_use_data( ).
        ENDIF.

        " Prüfen, ob Version aktiv ist:
        READ TABLE mt_tou_data ASSIGNING <ls_tou_data> INDEX 1.
        IF sy-subrc EQ 0.
          READ TABLE <ls_tou_data>-versiondata ASSIGNING FIELD-SYMBOL(<ls_tou_versiondata>) INDEX 1.
          IF sy-subrc EQ 0.
            DATA(lv_tou_status) = COND #( WHEN <ls_tou_versiondata>-status IS INITIAL
                                          THEN /hfq/if_tou_constants=>co_tou_usage_z03
                                          ELSE /hfq/if_tou_constants=>co_tou_usage_z02 ).
            READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
            IF sy-subrc EQ 0.
              <ls_diverse>-tou_usage = lv_tou_status.
            ELSE.
              ls_diverse = VALUE #(
                item_id = siv_itemid
              tou_usage = lv_tou_status
              ).
              APPEND ls_diverse TO sis_process_step_data-diverse.
            ENDIF.
          ENDIF.
        ENDIF.

    ENDCASE.

    IF siv_mandatory_data EQ abap_true.
      READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
      IF sy-subrc <> 0 OR
        <ls_diverse>-tou_usage IS INITIAL.
      MESSAGE e038(/idxgc/ide_add) WITH TEXT-111 INTO siv_mtext.
      /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD transaction_id.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: this method is used to set transaction ID
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*
*------------------------------------------------------------------------------*
    DATA: ls_diverse TYPE /idxgc/s_diverse_details,
          lv_guid    TYPE guid_22.
    CONSTANTS:  lc_guid    TYPE guid_22 VALUE '0000000000000000000000'.
    FIELD-SYMBOLS: <ls_diverse> TYPE /idxgc/s_diverse_details.

    TRY.
        CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c22
          RECEIVING
            uuid = lv_guid.
      CATCH cx_uuid_error.
        lv_guid = lc_guid.
    ENDTRY.

    READ TABLE SIS_PROCESS_DATA_SRC-diverse ASSIGNING <ls_diverse>
      WITH KEY item_id = siv_itemid
               transaction_qual = /idxgl/if_constants_ide=>gc_ide_qual_24.
    IF sy-subrc = 0.
      <ls_diverse>-transaction_no = lv_guid.
    ELSE.
      READ TABLE SIS_PROCESS_DATA_SRC-diverse ASSIGNING <ls_diverse> index 1.
      if sy-subrc eq 0.
        <ls_diverse>-transaction_no = lv_guid.
              APPEND <ls_diverse> TO sis_process_step_data-diverse.
      else.
      ls_diverse-transaction_no = lv_guid.
      ls_diverse-transaction_qual = /idxgl/if_constants_ide=>gc_ide_qual_24.
      ls_diverse-item_id = siv_itemid.

      APPEND ls_diverse TO sis_process_step_data-diverse.
      endif.
    ENDIF.

*------------------ Check if data is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      LOOP AT sis_process_step_data-diverse TRANSPORTING NO FIELDS
        WHERE item_id     = siv_itemid
          AND transaction_qual = /idxgl/if_constants_ide=>gc_ide_qual_24
          AND transaction_no IS INITIAL.
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-001 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD valid_from_date.
*------------------------------------------------------------------------------*
** Author: Hochfrequenz, 2019
**
** Usage: This method is used to Set valid start date
*------------------------------------------------------------------------------*
*  Change History:
*  September 2019: created
*  January   2022: Anpassung am Zeitformat (203 -> 303), Zeitangabe zudem im
*                  UTC-Format, daher muss offset mitgegeben werden.
*                  Offset-Bestimmung über ausgelagerte Methode.
*------------------------------------------------------------------------------*

    DATA: ls_diverse     TYPE /idxgc/s_diverse_details.
    FIELD-SYMBOLS: <ls_diverse>   TYPE /idxgc/s_diverse_details.

    DATA(lv_offset) = get_utc_offset(
      iv_date = sis_process_step_data-proc_date
      iv_time = sis_process_step_data-time_from
    ).

    READ TABLE sis_process_step_data-diverse
    ASSIGNING <ls_diverse> WITH KEY item_id = siv_itemid.
    IF sy-subrc = 0.
      <ls_diverse>-validstart_date = sis_process_step_data-proc_date.
      <ls_diverse>-validstart_time = sis_process_step_data-time_from.
      <ls_diverse>-validstart_offs = lv_offset.
    ELSE.
      ls_diverse-item_id = siv_itemid.
      ls_diverse-validstart_date = sis_process_step_data-proc_date.
      ls_diverse-validstart_time = sis_process_step_data-time_from.
      ls_diverse-validstart_offs = lv_offset.
      APPEND ls_diverse TO sis_process_step_data-diverse.
    ENDIF.

*------------------ Check if data is mandatory and filled ------------------*
    IF siv_mandatory_data = abap_true.
      READ TABLE sis_process_step_data-diverse ASSIGNING <ls_diverse> INDEX 1.
      IF <ls_diverse> IS NOT ASSIGNED
      OR  <ls_diverse>-validstart_offs IS INITIAL
      OR ( <ls_diverse>-validstart_date IS INITIAL AND
         <ls_diverse>-validstart_form IS INITIAL ).
        MESSAGE e038(/idxgc/ide_add) WITH TEXT-003 INTO siv_mtext.
        CALL METHOD /idxgc/cx_process_error=>raise_proc_exception_from_msg( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
