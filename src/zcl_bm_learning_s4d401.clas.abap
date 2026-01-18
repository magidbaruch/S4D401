CLASS zcl_bm_learning_s4d401 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bm_learning_s4d401 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

*    DATA(output) = lcl_abap_sql=>join_syntax(  ).
*    DATA(output) = lcl_abap_sql=>inner_and_outer_join(  ).
*    DATA(output) = lcl_abap_sql=>literals(  ).
*    DATA(output) = lcl_abap_sql=>cast(  ).
*    DATA(output) = lcl_abap_sql=>case( abap_true ).
*    DATA(output) = lcl_abap_sql=>arithmetic_expressions(  ).
*    DATA(output) = lcl_abap_sql=>string_processing( i_oparetion = 3 )."1 concat ,2-transform ,3-substring
*    DATA(output) = lcl_abap_sql=>Date_processing( ).
*    DATA(output) = lcl_abap_sql=>ts_conversions( ).
*    DATA(output) = lcl_abap_sql=>unit_conversions( ).
*    DATA(output) = lcl_abap_sql=>currency_conversions( ).
*    DATA(output) = lcl_abap_sql=>order_by( ).
*    DATA(output) = lcl_abap_sql=>aggregate_functions( ).
*    DATA(output) = lcl_abap_sql=>group_by( ).
*    DATA(output) = lcl_improving_internal_tables=>delete_adjacent_duplicates( ).
*    DATA(output) = lcl_improving_internal_tables=>table_comprehensions( ).
*    DATA(output) = lcl_strings_processing=>regex( ).
*    out->write( output ).


    "1) compare modify with field symbols
*    DATA(flights) = NEW lcl_demo( ).
*    flights->use_work_area( ).
*    flights->use_field_symbol( ).
*    out->write( 'done' ).
*    DATA(output) = lcl_generate_data=>fill_zs4d401_flights( ).
*    out->write( output ).
    "2) compare Standard/Sorted/Hash table access
    DATA(flights) = NEW lcl_flights_p_key_access( ).
    flights->access_standard( ).
    flights->access_sorted( ).
    flights->access_hashed( ).
*
*
    out->write( |Done| ).
    "3) compare accesses to the same table with secondary key.
*    DATA(object) = NEW lcl_flights_s_key_access( ).
*
*
*    object->read_primary( ).
*    object->read_non_key( ).
*    object->read_secondary_1( ).
*    object->read_secondary_2( ).
*    object->read_secondary_3( ).
*
*    out->write( |Done| ).

    "Authority chack
*    DATA(output) = lcl_auth_check=>authority_check( ).
*    out->write( output ).




  ENDMETHOD.
ENDCLASS.
