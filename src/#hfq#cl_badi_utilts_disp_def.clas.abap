class /HFQ/CL_BADI_UTILTS_DISP_DEF definition
  public
  final
  create public .

public section.

  interfaces /HFQ/IF_BADI_UTILTS_DISP .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS /HFQ/CL_BADI_UTILTS_DISP_DEF IMPLEMENTATION.


  METHOD /hfq/if_badi_utilts_disp~fill_calc_steps.

    SELECT l~cf_guid, ref_other_calc_step, calc_step_id, calc_step_id_subnum FROM /hfq/calc_form AS l
      INNER JOIN /hfq/calc_form_h AS h ON l~cf_guid = h~cf_guid
      WHERE h~malo_extui IN @it_malo
        AND calc_step_id NE ''
        AND ( calc_step_id_subnum EQ '' OR calc_step_id_subnum EQ '1' )
        AND h~validstart_date LE @iv_enddate
        AND h~validend_date GE @iv_startdate
      INTO TABLE @et_top_calc_step.
  ENDMETHOD.


  METHOD /hfq/if_badi_utilts_disp~fill_calc_sub_steps.

    SELECT l~cf_guid, ref_other_calc_step, calc_step_id, calc_step_id_subnum FROM /hfq/calc_form AS l
      INNER JOIN /hfq/calc_form_h AS h ON l~cf_guid = h~cf_guid
      WHERE malo_extui IN @it_malo
        AND calc_step_id_subnum NE ''
        AND h~validstart_date LE @iv_enddate
        AND h~validend_date GE @iv_startdate
      INTO TABLE @et_sub_calc_step.

  ENDMETHOD.


  method /HFQ/IF_BADI_UTILTS_DISP~FILL_SOURCETAB.

    SELECT * FROM /hfq/calc_form AS l
    INNER JOIN /hfq/calc_form_h AS h ON l~cf_guid = h~cf_guid
      WHERE malo_extui IN @it_malo
        AND h~validstart_date LE @iv_enddate
        AND h~validend_date GE @iv_startdate
      INTO CORRESPONDING FIELDS OF TABLE @et_sourcetab.

  endmethod.


  METHOD /hfq/if_badi_utilts_disp~fill_timeslices.

    SELECT malo_extui, cf_guid, validstart_date, validstart_time, validend_date, validend_time FROM /hfq/calc_form_h AS h
      WHERE malo_extui IN @it_malo
        AND validstart_date LE @iv_enddate
        AND validend_date GE @iv_startdate
      INTO TABLE @et_timeslices.
  ENDMETHOD.
ENDCLASS.
