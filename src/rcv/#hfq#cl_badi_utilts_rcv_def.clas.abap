class /HFQ/CL_BADI_UTILTS_RCV_DEF definition
  public
  final
  create public .

public section.

  interfaces /HFQ/IF_BADI_UTILTS_RCV .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS /HFQ/CL_BADI_UTILTS_RCV_DEF IMPLEMENTATION.


  METHOD /hfq/if_badi_utilts_rcv~check_data_storage.
    DATA: lt_calc_form_h TYPE /hfq/t_calc_form_h,
          lt_calc_form   TYPE /hfq/t_calc_form.

    CLEAR ev_result.

    SELECT * FROM /hfq/calc_form_h
        WHERE malo_extui EQ @iv_malo
          AND ( validstart_date LT @iv_keydate OR ( validstart_date EQ @iv_keydate AND validstart_time LE @iv_keytime ) )
          AND ( validend_date GT @iv_keydate OR ( validend_date EQ @iv_keydate AND validend_time GE @iv_keytime ) )
        INTO TABLE @lt_calc_form_h.
    IF sy-subrc NE 0.
      "Keine Einträge gefunden!
      ev_result = /idxgc/if_constants_add=>gc_cr_error.
      RETURN.
    ENDIF.

    DESCRIBE TABLE lt_calc_form_h LINES DATA(lv_lines).
    IF lv_lines NE 1.
      "Daten nicht korrekt abgegrenzt!
      ev_result = /idxgc/if_constants_add=>gc_cr_error.
      RETURN.
    ENDIF.

    LOOP AT lt_calc_form_h ASSIGNING FIELD-SYMBOL(<fs_calc_form_h>)
      WHERE calc_form_stat = 'Z33'. "Nur bei komplexen Formeln nötig
      SELECT * FROM /hfq/calc_form
        WHERE cf_guid EQ @<fs_calc_form_h>-cf_guid
        INTO table @lt_calc_form.
      IF sy-subrc NE 0.
        ev_result = /idxgc/if_constants_add=>gc_cr_error.
        RETURN.
      ENDIF.

    ENDLOOP.

    IF ev_result IS INITIAL.
      ev_result = /idxgc/if_constants_add=>gc_cr_ok.
    ENDIF.
  ENDMETHOD.


  METHOD /hfq/if_badi_utilts_rcv~save_calc_form.

* Update 0001 - Cambeis 26.02.2021 Lieferrichtung (Supply_direct) in den Kopf verlegt
    DATA: lo_gen_calc_form TYPE REF TO /hfq/cl_gen_calc_form,
          ls_calc_form_h   TYPE /hfq/calc_form_h,
          ls_calc_form     TYPE /hfq/calc_form,
          lt_calc_form     TYPE /hfq/t_calc_form,
          ls_return        TYPE bapiret2,
          lt_return        TYPE bapiret2_t,
          lv_mtext         TYPE string.

* Initialisierung der Kopfdaten und Berechnungsformel über die Prozessdaten
    READ TABLE is_proc_step_data_all-diverse ASSIGNING FIELD-SYMBOL(<fs_diverse>) INDEX 1.
    IF sy-subrc NE 0.
      " Fehlerbehandlung definieren!
    ENDIF.

    ls_calc_form_h = VALUE #( malo_extui = is_proc_step_data_all-ext_ui
                              validstart_date = is_proc_step_data_all-proc_date
                              validend_date = '99991231'
                              calc_form_stat = <fs_diverse>-calc_form_stat
                              calc_step_amount_malo = <fs_diverse>-calc_step_amount_malo
                              supply_direct = <fs_diverse>-supply_direct "0001
                             ).

    LOOP AT is_proc_step_data_all-/idxgl/pod_data ASSIGNING FIELD-SYMBOL(<fs_pod_data>).
      CLEAR: ls_calc_form.

      MOVE-CORRESPONDING <fs_pod_data> TO ls_calc_form.
*      ls_calc_form-supply_direct = <fs_diverse>-supply_direct. "0001
      ls_calc_form-melo_extui = <fs_pod_data>-ext_ui.
      APPEND ls_calc_form TO lt_calc_form.
    ENDLOOP.
*    IF sy-subrc NE 0.
*      " Es wurden keine relevanten Daten für die Berechnungsformel mitgeliefert.
*      MESSAGE e051(/hfq/calc_form) INTO lv_mtext. "Speichern der Berechnungsschritte fehlgeschlagen.
*      /idxgc/cx_process_error=>raise_exception_from_msg( ).
*    ENDIF.


    CREATE OBJECT lo_gen_calc_form.

    CALL METHOD lo_gen_calc_form->gen_malo_timeslice
      EXPORTING
        iv_malo_ext_ui = is_proc_step_data_all-ext_ui
        iv_startdate   = is_proc_step_data_all-proc_date
        iv_enddate     = '99991231'
        iv_status      = <fs_diverse>-calc_form_stat
      IMPORTING
        es_calc_form_h = ls_calc_form_h
      CHANGING
        ct_return      = lt_return
      EXCEPTIONS
        error          = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      ls_return = lt_return[ type = 'E' ].
      MESSAGE ID ls_return-id TYPE 'E' NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4 INTO lv_mtext.
      /idxgc/cx_process_error=>raise_exception_from_msg( ).
    ENDIF.


* Einträge der Berechnungsformel anhängen
    LOOP AT lt_calc_form ASSIGNING FIELD-SYMBOL(<fs_calc_form>).
      CALL METHOD lo_gen_calc_form->add_calc_form_entry
        EXPORTING
          iv_calc_step_id        = <fs_calc_form>-calc_step_id
          iv_calc_step_id_subnum = <fs_calc_form>-calc_step_id_subnum
          iv_ref_other_calc_step = <fs_calc_form>-ref_other_calc_step
          iv_melo_extui          = <fs_calc_form>-melo_extui
*          iv_supply_direct       = <fs_calc_form>-supply_direct
          iv_math_operator       = <fs_calc_form>-math_operator
          iv_flow_direction      = <fs_calc_form>-flow_direction
          iv_lossfact_ext        = <fs_calc_form>-lossfact_ext
          iv_lossfact_line       = <fs_calc_form>-lossfact_line
        CHANGING
          ct_return              = lt_return
        EXCEPTIONS
          error                  = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        ls_return = lt_return[ type = 'E' ].
        MESSAGE ID ls_return-id TYPE 'E' NUMBER ls_return-number
          WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4 INTO lv_mtext.
        /idxgc/cx_process_error=>raise_exception_from_msg( ).
      ENDIF.
    ENDLOOP.

* Berechnungsschritte für die Menge der Marktlokation setzen
    CALL METHOD lo_gen_calc_form->set_main_calc_step
      EXPORTING
        iv_calc_step_malo = CONV string( <fs_diverse>-calc_step_amount_malo )
      CHANGING
        ct_return         = lt_return
      EXCEPTIONS
        error             = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      ls_return = lt_return[ type = 'E' ].
      MESSAGE ID ls_return-id TYPE 'E' NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4 INTO lv_mtext.
      /idxgc/cx_process_error=>raise_exception_from_msg( ).
    ENDIF.

* Einträge in Tabelle speichern
    CALL METHOD lo_gen_calc_form->save
      CHANGING
        ct_return   = lt_return
      EXCEPTIONS
        error_head  = 1                " Speichern der Zeitscheibe fehlgeschlagen.
        error_lines = 2                " Speichern der Berechnungsschritte fehlgeschlagen.
        OTHERS      = 3.
    IF sy-subrc <> 0.
      ls_return = lt_return[ type = 'E' ].
      MESSAGE ID ls_return-id TYPE 'E' NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4 INTO lv_mtext.
      /idxgc/cx_process_error=>raise_exception_from_msg( ).
    ENDIF.


  ENDMETHOD.


  METHOD /hfq/if_badi_utilts_rcv~select_cf_details.
    DATA: lt_malo_range  TYPE isu_ranges_tab,
          lv_date        TYPE datum,
          lv_time        TYPE uzeit,
          lt_calc_form_h TYPE /hfq/t_calc_form_h,
          lt_calc_form   TYPE /hfq/t_calc_form,
          ls_pod_data    TYPE /idxgl/s_pod_data_details.

    lt_malo_range = VALUE #( ( sign = 'I' option = 'EQ' low = iv_malo ) ).

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
          AND ( validstart_date LT @lv_date OR ( validstart_date EQ @lv_date AND validstart_time LE @lv_time ) )
          AND ( validend_date GT @lv_date OR ( validend_date EQ @lv_date AND validend_time GE @lv_time ) )
        INTO TABLE @lt_calc_form_h.
    IF sy-subrc NE 0.
      RAISE no_construct_found.
    ENDIF.

    "Berechnungsschritte ermitteln:
    SELECT * FROM /hfq/calc_form
      FOR ALL ENTRIES IN @lt_calc_form_h WHERE cf_guid = @lt_calc_form_h-cf_guid
      INTO TABLE @lt_calc_form.
    IF sy-subrc NE 0.
      RAISE no_calc_form_found.
    ENDIF.

    " Es sollte nur einen Eintrag pro Malo geben
    READ TABLE lt_calc_form_h ASSIGNING FIELD-SYMBOL(<fs_calc_form_h>) INDEX 1.
    ev_dateto = <fs_calc_form_h>-validend_date.
    ev_datefrom = <fs_calc_form_h>-validstart_date.
    ev_timeto = <fs_calc_form_h>-validend_time.
    ev_timefrom = <fs_calc_form_h>-validstart_time.
    ev_calc_form_stat = <fs_calc_form_h>-calc_form_stat.
    ev_calc_step_amount = <fs_calc_form_h>-calc_step_amount_malo.

    LOOP AT lt_calc_form ASSIGNING FIELD-SYMBOL(<fs_calc_form>).
      CLEAR: ls_pod_data.
      MOVE-CORRESPONDING <fs_calc_form> TO ls_pod_data.
      MOVE <fs_calc_form>-melo_extui TO ls_pod_data-ext_ui.

      APPEND ls_pod_data TO et_pod_data.
    ENDLOOP.


  ENDMETHOD.


  METHOD /hfq/if_badi_utilts_rcv~select_relevant_data.
    DATA: lt_malo_range TYPE isu_ranges_tab,
          lv_date       TYPE datum,
          lv_time       TYPE uzeit.

    lt_malo_range = VALUE #( ( sign = 'I' option = 'EQ' low = iv_malo ) ).

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
          AND ( validstart_date LT @lv_date OR ( validstart_date EQ @lv_date AND validstart_time LE @lv_time ) )
          AND ( validend_date GT @lv_date OR ( validend_date EQ @lv_date AND validend_time GE @lv_time ) )
        INTO TABLE @et_calc_form_h.
    IF sy-subrc NE 0.
      RAISE no_construct_found.
    ENDIF.

    "Berechnungsschritte ermitteln:
    SELECT * FROM /hfq/calc_form
      FOR ALL ENTRIES IN @et_calc_form_h WHERE cf_guid = @et_calc_form_h-cf_guid
      INTO TABLE @et_calc_form.
    IF sy-subrc NE 0.
      RAISE no_calc_form_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
