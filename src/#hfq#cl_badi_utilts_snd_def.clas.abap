class /HFQ/CL_BADI_UTILTS_SND_DEF definition
  public
  final
  create public .

public section.

  interfaces /HFQ/IF_BADI_UTILTS_SND .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS /HFQ/CL_BADI_UTILTS_SND_DEF IMPLEMENTATION.


  method /HFQ/IF_BADI_UTILTS_SND~HAS_WAIT_STEP.


  endmethod.


  METHOD /hfq/if_badi_utilts_snd~select_corresponding_melos.
    DATA: lt_tmp_int_ui  TYPE isu_ranges_tab,
          lt_melo_int_ui TYPE isu_ranges_tab,
          lv_melo        TYPE int_ui,
          ls_pod_details TYPE /idxgc/s_pod_info_details.


    SELECT
        'I' AS sign,
        'EQ' AS option,
        int_ui1 AS low
      FROM /idxgc/pod_rel
      WHERE int_ui2 = @iv_malo_int_ui
        AND datefrom  LE @iv_keydate
        AND dateto    GE @iv_keydate
      INTO CORRESPONDING FIELDS OF TABLE @lt_tmp_int_ui.
    IF sy-subrc = 0.
      APPEND LINES OF lt_tmp_int_ui TO lt_melo_int_ui.
    ENDIF.

    CLEAR lt_tmp_int_ui.

    SELECT
        'I' AS sign,
        'EQ' AS option,
        int_ui2 AS low
      FROM /idxgc/pod_rel
      WHERE int_ui1 = @iv_malo_int_ui
        AND datefrom  LE @iv_keydate
        AND dateto    GE @iv_keydate
      INTO TABLE @lt_tmp_int_ui.
    IF sy-subrc = 0.
      APPEND LINES OF lt_tmp_int_ui TO lt_melo_int_ui.
    ENDIF.

    IF lt_melo_int_ui IS NOT INITIAL.

      SELECT ext_ui FROM euitrans
        WHERE int_ui IN @lt_melo_int_ui
        INTO CORRESPONDING FIELDS OF TABLE @et_melo_info.
      IF sy-subrc NE 0.
        "Fehler: Übertragung fehlgeschlagen
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD /hfq/if_badi_utilts_snd~select_relevant_data.
    DATA: lt_malo_range TYPE isu_ranges_tab,
          lv_date       TYPE datum,
          lv_time       TYPE uzeit.
    DATA: lx_previous          TYPE REF TO /idxgc/cx_general,
          lr_installation_type TYPE REF TO /idxgc/badi_installation_type,
          lv_type              TYPE /idxgc/de_installation_type,
          ls_calc_form_h       TYPE /hfq/calc_form_h.

    IF iv_malo IS NOT INITIAL.
      lt_malo_range = VALUE #( ( sign = 'I' option = 'EQ' low = iv_malo ) ).
    ENDIF.

    lv_date = iv_keydate.
    IF lv_date IS INITIAL.
      lv_date = sy-datum.
    ENDIF.
    lv_time = iv_keytime.
    IF lv_time IS INITIAL.
      lv_time = '000000'.
    ENDIF.

    "Zeitscheiben ermitteln:
    SELECT * FROM /hfq/calc_form_h
        WHERE malo_extui IN @lt_malo_range
*          AND ( validstart_date LT @lv_date OR ( validstart_date EQ @lv_date AND validstart_time LE @lv_time ) ) "auch zukünftige Einträge zulassen
          AND ( validend_date GT @lv_date OR ( validend_date EQ @lv_date AND validend_time GE @lv_time ) )
        INTO TABLE @et_calc_form_h.
    IF sy-subrc NE 0.
*      RAISE no_construct_found.
    ENDIF.

    "Berechnungsschritte ermitteln:
    SELECT * FROM /hfq/calc_form
      FOR ALL ENTRIES IN @et_calc_form_h WHERE cf_guid = @et_calc_form_h-cf_guid
      INTO TABLE @et_calc_form.
    IF sy-subrc NE 0.
*      RAISE no_calc_form_found.
    ENDIF.

    IF et_calc_form_h IS INITIAL OR iv_malo IS INITIAL.
      "Prüfung auf Pauschalanlagen:
      TRY .
          GET BADI lr_installation_type.
        CATCH /idxgc/cx_utility_error INTO lx_previous.
          CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = lx_previous ).
      ENDTRY.

      SELECT * FROM euitrans INTO TABLE @DATA(lt_euitrans)
        WHERE datefrom GE @lv_date
          AND dateto LE @lv_date
          AND uistrutyp EQ @/idxgl/if_badi_data_access=>gc_pod_structure_category_malo
          AND ext_ui IN @lt_malo_range.

      LOOP AT lt_euitrans ASSIGNING FIELD-SYMBOL(<fs_euitrans>).
        CLEAR: ls_calc_form_h, lv_type.

        TRY.
            CALL BADI lr_installation_type->get_type
              EXPORTING
                iv_int_ui   = <fs_euitrans>-int_ui
                iv_key_date = lv_date
              IMPORTING
                ev_type     = lv_type.
          CATCH /idxgc/cx_utility_error INTO lx_previous.
            CALL METHOD /idxgc/cx_utility_error=>raise_util_exception_from_msg( ir_previous = lx_previous ).
        ENDTRY.

        IF lv_type = /idexge/if_constants_dp=>gc_inst_type_rate_instal. "Pauschalanlage
          ls_calc_form_h = VALUE #( malo_extui = <fs_euitrans>-ext_ui
                                    validstart_date = <fs_euitrans>-datefrom
                                    validstart_time = <fs_euitrans>-timefrom
                                    validend_date = <fs_euitrans>-dateto
                                    validend_time = <fs_euitrans>-timeto
                                    calc_form_stat  = /hfq/cl_dp_utilts=>gc_calcform_stat_z41
                                   ).
          APPEND ls_calc_form_h TO et_calc_form_h.
        ENDIF.

      ENDLOOP.

      IF et_calc_form_h IS INITIAL.
        RAISE no_construct_found.
      ENDIF.
    ELSEIF et_calc_form IS INITIAL.
      RAISE no_calc_form_found.
    ENDIF.

  ENDMETHOD.


  METHOD /HFQ/IF_BADI_UTILTS_SND~SELECT_SERVICE_TYPES.
    DATA: ls_range       TYPE isu_ranges,
          lv_service_art TYPE sercode,
          lt_service_art TYPE TABLE OF sercode.

    SELECT service FROM tecde
      WHERE intcode = @iv_service_typ
      INTO TABLE @lt_service_art.
      IF sy-subrc NE 0.
        "Keine Serviceart zum Typ &1 gefunden.
        MESSAGE E012(/hfq/utilts_snd) WITH iv_service_typ.
      ELSE.
        LOOP AT lt_service_art INTO lv_service_art.
          ls_range-sign = 'I'.
          ls_range-option = 'EQ'.
          ls_range-low = lv_service_art.

          APPEND ls_range TO rt_serv_type_range.
        ENDLOOP.
      ENDIF.

      IF rt_serv_type_range IS INITIAL.
        "Fehlende Befüllung der MSB-Servicearten im BAdI.
        MESSAGE e011(/hfq/utilts_snd).
      ENDIF.
    ENDMETHOD.
ENDCLASS.
