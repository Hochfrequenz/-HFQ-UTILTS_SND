*&---------------------------------------------------------------------*
*& Report /HFQ/RP_CALC_FORM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /hfq/rp_calc_formula.



PARAMETERS : p_rb1 RADIOBUTTON GROUP rbg DEFAULT 'X'.
PARAMETERS : p_rb2 RADIOBUTTON GROUP rbg.
PARAMETERS : p_sim AS CHECKBOX DEFAULT 'X'.

TYPES : BEGIN OF s_malo,
          ext_ui TYPE ext_ui,
        END OF s_malo.

TYPES : BEGIN OF s_ausgabe,
          ext_ui          TYPE ext_ui,
          einzdat         TYPE einzdat,
          int_ui          TYPE int_ui,
          calc_form_stat  TYPE /idxgc/de_calc_form_stat,
          validstart_date TYPE /IDXGC/DE_VALIDSTART_DATE,
          dateto          TYPE dateto,
          validstart_time TYPE /idxgc/de_validstart_time,
        END OF s_ausgabe.

DATA: ls_ausgabe TYPE s_ausgabe,
      lt_ausgabe TYPE TABLE OF s_ausgabe.



DATA : ls_malo TYPE s_malo.

DATA: ls_calc_form TYPE /hfq/calc_form_h,
      ls_pod_rel   TYPE /idxgc/pod_rel.

DATA: lt_calc_form TYPE TABLE OF /hfq/calc_form_h,
      lt_pod_rel   TYPE TABLE OF /idxgc/pod_rel,
      lt_malo      TYPE TABLE OF s_malo.

DATA go_alv TYPE REF TO cl_salv_table.
DATA go_functions TYPE REF TO cl_salv_functions_list.
DATA go_columns TYPE REF TO cl_salv_columns_table.
DATA go_display TYPE REF TO cl_salv_display_settings.



START-OF-SELECTION.

**selektieren alle Malos
*  SELECT ext_ui FROM euitrans INTO TABLE lt_malo
*    WHERE uistrutyp EQ 'MA'.

"time slices get correct one
*Ermittlung der Vertragsbeginn der Malos

  SELECT ever~einzdat, euitrans~ext_ui, euitrans~int_ui FROM ever
    INNER JOIN eanl ON ever~anlage =  eanl~anlage
    INNER JOIN euiinstln ON eanl~anlage = euiinstln~anlage
    INNER JOIN euitrans  ON euiinstln~int_ui = euitrans~int_ui
    WHERE euitrans~uistrutyp eq 'MA'
    INTO TABLE @DATA(lt_vbeginn).


  CASE abap_true.
    when p_rb1.
      LOOP AT lt_vbeginn ASSIGNING FIELD-SYMBOL(<ls_vbeginn>).


        IF <ls_vbeginn>-einzdat GE '20201001'.
          ls_ausgabe-ext_ui = <ls_vbeginn>-ext_ui.
          ls_ausgabe-validstart_date = <ls_vbeginn>-einzdat.
          ls_ausgabe-calc_form_stat = 'Z40'. "cons
          ls_ausgabe-einzdat = <ls_vbeginn>-einzdat.

          APPEND ls_ausgabe TO lt_ausgabe.

        ELSE.
*Hier noch die Melo und andere Datenelement zu übermitteln.
          ls_ausgabe-ext_ui = <ls_vbeginn>-ext_ui.
          ls_ausgabe-validstart_date = '20201001'.
          ls_ausgabe-calc_form_stat = 'Z40'.
          ls_ausgabe-einzdat = <ls_vbeginn>-einzdat.

          APPEND ls_ausgabe TO lt_ausgabe.

        ENDIF.
      ENDLOOP.

* Simulation.
      IF p_sim IS NOT INITIAL.



        MESSAGE |Daten sind nicht gespeichert|  TYPE 'I'.
* instanz der klasse cl_salv_table erzeugen
        TRY.
            cl_salv_table=>factory(
              IMPORTING r_salv_table = go_alv
              CHANGING t_table = lt_ausgabe ).
          CATCH cx_salv_msg.
        ENDTRY.
* funktionstasten
        go_functions = go_alv->get_functions( ).
        go_functions->set_all( abap_true ).

* optimale Spaltenbreite
        go_columns = go_alv->get_columns( ).
        go_columns->set_optimize( abap_true ).

* Titel
        go_display = go_alv->get_display_settings( ).
        go_display->set_list_header( value = '/HFQ/CALC_FORM_H' ).
        go_display->set_striped_pattern( abap_true ).

* Liste anzeigen
        go_alv->display( ).

      ELSE.
        MODIFY /hfq/calc_form FROM TABLE lt_calc_form.
        if sy-subrc <> 0.
        WRITE: 'Daten nicht gändert oder gespeichert', sy-subrc.
      ELSE.
        WRITE: 'Daten erfolgreich geändert oder gespeichert..'.
      ENDIF.
      ENDIF.

    WHEN  p_rb2.

      LOOP AT lt_vbeginn ASSIGNING FIELD-SYMBOL(<ls_vbeginn2>).

        IF <ls_vbeginn2>-einzdat GE '20201001'.

          ls_pod_rel-int_ui2 = <ls_vbeginn2>-ext_ui.
          ls_pod_rel-int_ui1 = <ls_vbeginn2>-int_ui.
*!!
          ls_pod_rel-datefrom = <ls_vbeginn2>-einzdat.
*noch der Code Z40
          APPEND ls_pod_rel TO lt_pod_rel.

        ELSE.

          ls_pod_rel-int_ui2 = <ls_vbeginn2>-ext_ui.
          ls_pod_rel-datefrom = '20201001'.
          ls_pod_rel-int_ui1 = <ls_vbeginn2>-int_ui.
*noch der Code Z40
          APPEND ls_pod_rel TO lt_pod_rel.
        ENDIF.

      ENDLOOP.

      IF p_sim IS NOT INITIAL.



*WRITE: 'Daten sind nicht gespeichert'.
        MESSAGE |Daten sind nicht gespeichert|  TYPE 'I'.
* instanz der klasse cl_salv_table erzeugen
        TRY.
            cl_salv_table=>factory(
              IMPORTING r_salv_table = go_alv
              CHANGING t_table = lt_pod_rel ).
          CATCH cx_salv_msg.
        ENDTRY.
* Funktionstasten
        go_functions = go_alv->get_functions( ).
        go_functions->set_all( abap_true ).

* optimale Spaltenbreite
        go_columns = go_alv->get_columns( ).
        go_columns->set_optimize( abap_true ).

* Titel und/oder Streifenmuster
        go_display = go_alv->get_display_settings( ).
        go_display->set_list_header( value = '/IDXGC/REL_POD' ).
        go_display->set_striped_pattern( abap_true ).

* Liste anzeigen
        go_alv->display( ).


*WRITE: 'Daten sind nicht verbucht'.
*          MESSAGE |Daten sind nicht verbucht|  TYPE 'I'.
*        ENDIF.
      else.
        MODIFY /idxgc/pod_rel FROM TABLE lt_pod_rel.
        IF sy-subrc <> 0.
          WRITE: 'Daten nicht gändert oder gespeichert', sy-subrc.
        ELSE.
          WRITE: 'Daten erfolgreich geändert oder gespeichert..'.

        ENDIF.
      ENDIF.
    ENDCASE.
