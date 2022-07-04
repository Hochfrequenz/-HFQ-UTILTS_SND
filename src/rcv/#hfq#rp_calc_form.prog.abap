*&---------------------------------------------------------------------*
*& Report /HFQ/RP_CALC_FORM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /hfq/rp_calc_form.

INITIALIZATION.
  DATA: lv_ext_ui TYPE ext_ui.
  SELECTION-SCREEN BEGIN OF BLOCK init WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS  s_ext_ui FOR lv_ext_ui MATCHCODE OBJECT /idxgl/sh_ext_malo.
  PARAMETERS : p_date TYPE dats OBLIGATORY,
               p_sim  AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK init.

CLASS lcl_/hfq/rp_calc_form DEFINITION.

  PUBLIC SECTION.
    METHODS  run
      RAISING
        cx_salv_msg .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS display_alv
      CHANGING
        ct_/hfq/t_calc_form_h TYPE /hfq/t_calc_form_h
      RAISING
        cx_salv_msg.
    METHODS get_required_data
      RETURNING
        VALUE(rt_calc_form_h) TYPE /hfq/t_calc_form_h.
    METHODS update_database_table
      CHANGING
        ct_calc_form_h TYPE /hfq/t_calc_form_h
      RAISING
        cx_salv_msg.
    METHODS simulate_final_data
      CHANGING
        ct_calc_form_h TYPE /hfq/t_calc_form_h
      RAISING
        cx_salv_msg.

ENDCLASS.


CLASS lcl_/hfq/rp_calc_form IMPLEMENTATION.


  METHOD run.

    DATA(lt_calc_form_h) = get_required_data( ).

    IF  p_sim IS INITIAL.

      update_database_table(
      CHANGING
        ct_calc_form_h = lt_calc_form_h ).

    ELSE.

      simulate_final_data(
       CHANGING
         ct_calc_form_h = lt_calc_form_h ).

    ENDIF.

  ENDMETHOD.


  METHOD display_alv.

    cl_salv_table=>factory(
                IMPORTING r_salv_table = DATA(lo_alv)
                CHANGING t_table =  ct_/hfq/t_calc_form_h ).

* buttons for sort, filter, excel download, ...
    DATA(lo_functions) = lo_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

* optimized column width
    DATA(lo_columns) = lo_alv->get_columns( ).
    lo_columns->set_optimize( abap_true ).

* title and/or alternating colours
    DATA(lo_display) = lo_alv->get_display_settings( ).
    lo_display->set_list_header( value = '/HFQ/CALC_FORM_H' ).
    lo_display->set_striped_pattern( abap_true ).

    lo_alv->display( ).

  ENDMETHOD.


  METHOD get_required_data.

    SELECT euitrans~ext_ui,
                       euitrans~int_ui,
                       euitrans~datefrom,
                       euitrans~dateto,
                       ever~einzdat, ever~auszdat
                   FROM euitrans
                   INNER JOIN euiinstln ON euiinstln~int_ui EQ euitrans~int_ui
                   INNER JOIN eanl ON eanl~anlage EQ  euiinstln~anlage
                   INNER JOIN ever ON ever~anlage EQ eanl~anlage
                   WHERE  euitrans~uistrutyp EQ 'MA'
                    AND   euiinstln~datefrom LE @p_date
                    AND   euiinstln~dateto   GE @p_date
                    AND   euitrans~datefrom  LE @p_date
                    AND   euitrans~dateto    GE @p_date
                    AND   ever~einzdat       LE @p_date
                    AND   ever~auszdat       GE @p_date
                    INTO TABLE @DATA(lt_required_data).

    LOOP AT lt_required_data INTO DATA(ls_required_data).

      rt_calc_form_h = VALUE #( BASE rt_calc_form_h
                                                                         (  mandt = sy-mandt
                                                                          malo_extui = ls_required_data-ext_ui
                                                                         validstart_time = '000000'
                                                                         validstart_date = COND #( WHEN ls_required_data-einzdat GE '20201001'
                                                                                                                                 THEN ls_required_data-einzdat
                                                                                                                                 ELSE '20201001' )
                                                                         validend_date = ls_required_data-auszdat
                                                                         validend_time = '235959'
                                                                         calc_form_stat = 'Z40' ) ).


    ENDLOOP.

  ENDMETHOD.


  METHOD update_database_table.

    MODIFY /hfq/calc_form_h FROM TABLE  ct_calc_form_h.

    MESSAGE |Daten sind gespeichert|  TYPE 'I'.

    display_alv(
      CHANGING
        ct_/hfq/t_calc_form_h =  ct_calc_form_h ).

  ENDMETHOD.


  METHOD simulate_final_data.

    MESSAGE |Daten sind nicht gespeichert|  TYPE 'I'.

    display_alv(
      CHANGING
        ct_/hfq/t_calc_form_h =  ct_calc_form_h ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(lo_/hfq/rp_calc_form) = NEW lcl_/hfq/rp_calc_form( ).

  TRY.
      lo_/hfq/rp_calc_form->run( ).
    CATCH cx_salv_msg. " ALV: General Error Class with Message
  ENDTRY.
