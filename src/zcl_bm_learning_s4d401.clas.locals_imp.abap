*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_abap_sql DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS join_syntax RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS inner_and_outer_join RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS literals RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS cast RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS case
      IMPORTING is_complex      TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS arithmetic_expressions RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS string_processing
      IMPORTING i_oparetion     TYPE i
      RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS Date_processing RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS ts_conversions RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS unit_conversions RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS currency_conversions RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS order_by RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS select_distinct RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS aggregate_functions RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS group_by RETURNING VALUE(r_output) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_abap_sql IMPLEMENTATION.

  METHOD join_syntax.

    SELECT FROM /dmo/carrier INNER JOIN /dmo/connection
*    SELECT FROM /dmo/carrier AS a INNER JOIN /dmo/connection AS c
         ON /dmo/carrier~carrier_id = /dmo/connection~carrier_id

     FIELDS /dmo/carrier~carrier_id,
            /dmo/carrier~name AS carrier_name,
            /dmo/connection~connection_id,
            /dmo/connection~airport_from_id,
            /dmo/connection~airport_to_id

      WHERE /dmo/carrier~currency_code = 'EUR'
       INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(rew).
      APPEND |{ rew-carrier_id } { rew-carrier_name } { rew-airport_from_id } { rew-airport_from_id } { rew-airport_to_id }| TO r_output.
    ENDLOOP.


  ENDMETHOD.

  METHOD inner_and_outer_join.

    SELECT FROM /dmo/Agency AS a
*                INNER JOIN /dmo/customer AS c
*           LEFT OUTER JOIN /dmo/customer AS c
          RIGHT OUTER JOIN /dmo/customer AS c
             ON a~city         = c~city

         FIELDS agency_id,
                name AS Agency_name,
                a~city AS agency_city,
                c~city AS customer_city,
                customer_id,
                last_name AS customer_name

          WHERE ( c~customer_id < '000010' OR c~customer_id IS NULL )
            AND ( a~agency_id   < '070010' OR a~agency_id   IS NULL )

           INTO TABLE @DATA(result_Join).

    LOOP AT result_Join INTO DATA(rew).

      APPEND |{ rew-agency_id } { rew-agency_name } { rew-agency_city } { rew-customer_city } { rew-customer_id } { rew-customer_name }| TO r_output.
    ENDLOOP.
  ENDMETHOD.

  METHOD literals.
    CONSTANTS c_number TYPE i VALUE 1234.

    SELECT FROM /dmo/carrier
         FIELDS 'Hello'    AS Character,    " Type c
                 1         AS Integer1,     " Type i
                -1         AS Integer2,     " Type i

                @c_number  AS constant      " Type i  (same as constant)

          INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(rew).

      APPEND |{ rew-character } { rew-integer1 } { rew-integer2 } { rew-constant }| TO r_output.

    ENDLOOP.

  ENDMETHOD.

  METHOD cast.
    SELECT FROM /dmo/carrier
                 FIELDS '19891109'                     AS char_8,
                  CAST( '19891109' AS CHAR( 4 ) )      AS char_4,
                  CAST( '19891109' AS NUMC( 8  ) )     AS numc_8,
                  CAST( '19891109' AS INT4 )           AS integer,
                  CAST( '19891109' AS DEC( 10, 2 ) )   AS dec_10_2,
                  CAST( '19891109' AS FLTP )           AS fltp,
                  CAST( '19891109' AS DATS )           AS date

             INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(rew).

      APPEND |{ rew-char_8 } { rew-char_4 } { rew-numc_8 } { rew-integer } { rew-dec_10_2 } { rew-fltp } { rew-date }| TO r_output.

    ENDLOOP.


  ENDMETHOD.

  METHOD case.

    CASE is_complex.

      WHEN abap_false.
        SELECT FROM /dmo/customer
            FIELDS customer_id,
                   title,
                   CASE title
                     WHEN 'Mr.'  THEN 'Mister'
                     WHEN 'Mrs.' THEN 'Misses'
                     ELSE             ' '
                  END AS title_long

           WHERE country_code = 'AT'
            INTO TABLE @DATA(result_simple).

        LOOP AT result_simple INTO DATA(s_rew).

          APPEND |{ s_rew-customer_id } { s_rew-title } { s_rew-title_long }| TO r_output.

        ENDLOOP.
      WHEN OTHERS.
        SELECT FROM /DMO/flight
              FIELDS flight_date,
                     seats_max,
                     seats_occupied,
                     CASE
                       WHEN seats_occupied < seats_max THEN 'Seats Avaliable'
                       WHEN seats_occupied = seats_max THEN 'Fully Booked'
                       WHEN seats_occupied > seats_max THEN 'Overbooked!'
                       ELSE                                 'This is impossible'
                     END AS Booking_State

               WHERE carrier_id    = 'LH'
                 AND connection_id = '0400'
                INTO TABLE @DATA(result_complex).

        LOOP AT result_complex INTO DATA(c_rew).

          APPEND |{ c_rew-flight_date } { c_rew-seats_occupied } { c_rew-seats_max } { c_rew-booking_state }| TO r_output.

        ENDLOOP.
    ENDCASE.



  ENDMETHOD.

  METHOD arithmetic_expressions.

    SELECT FROM /dmo/flight
        FIELDS seats_max,
               seats_occupied,
               seats_max - seats_occupied AS seats_availible,

               (   CAST( seats_occupied AS FLTP )
                 * CAST( 100 AS FLTP )
               ) / CAST(  seats_max AS FLTP )                  AS percentage_fltp,

               div( seats_occupied * 100 , seats_max )         AS percentage_int,

               division(  seats_occupied * 100, seats_max, 2 ) AS percentage_dec

         WHERE carrier_id    = 'LH'
           AND connection_id = '0400'
          INTO TABLE @DATA(result).


    LOOP AT result INTO DATA(rew).
      APPEND |{ rew-seats_occupied } { rew-seats_max } { rew-seats_availible } { rew-percentage_fltp }| TO r_output.
    ENDLOOP.

  ENDMETHOD.


  METHOD string_processing.

    CASE i_oparetion.

      WHEN 1."concat.
        SELECT FROM /dmo/customer
             FIELDS customer_id,

                    street && ',' && ' ' && postal_code && ' ' && city   AS address_expr,

                    concat( street,
                            concat_with_space(  ',',
                                                 concat_with_space( postal_code,
                                                                    upper(  city ),
                                                                    1
                                                                  ),
                                                1
                                             )
                         ) AS address_func

              WHERE country_code = 'ES'
               INTO TABLE @DATA(result_concat).

        LOOP AT result_concat INTO DATA(c_rew).
          APPEND |{ c_rew-customer_id }{ c_rew-address_expr }{ c_rew-address_func }| TO r_output.
        ENDLOOP.

      WHEN 2."transform.

        SELECT FROM /dmo/carrier
           FIELDS carrier_id,
                  name,
                  upper( name )   AS name_upper,
                  lower( name )   AS name_lower,
                  initcap( name ) AS name_initcap

           WHERE carrier_id = 'SR'
            INTO TABLE @DATA(result_transform).

        LOOP AT result_transform INTO DATA(t_rew).
          APPEND |{ t_rew-carrier_id } { t_rew-name } { t_rew-name_upper } { t_rew-name_lower } { t_rew-name_initcap }  | TO r_output.
        ENDLOOP.

      WHEN OTHERS.

        SELECT FROM /dmo/flight
         FIELDS flight_date,
                CAST( flight_date AS CHAR( 8 ) )  AS flight_date_raw,

                left( flight_Date, 4   )          AS year,

                right(  flight_date, 2 )          AS day,

                substring(  flight_date, 5, 2 )   AS month

          WHERE carrier_id = 'LH'
            AND connection_id = '0400'
           INTO TABLE @DATA(result_substring).

        LOOP AT result_substring INTO DATA(s_rew).
          APPEND |{ s_rew-flight_date } { s_rew-flight_date_raw } { s_rew-year } { s_rew-month } { s_rew-day }  | TO r_output.
        ENDLOOP.

    ENDCASE.

  ENDMETHOD.

  METHOD date_processing.

    SELECT FROM /dmo/travel
         FIELDS begin_date,
                end_date,
                is_valid( begin_date  )              AS valid,

                add_days( begin_date, 7 )            AS add_7_days,
                add_months(  begin_date, 3 )         AS add_3_months,
                days_between( begin_date, end_date ) AS duration,

                weekday(  begin_date  )              AS weekday,
                extract_month(  begin_date )         AS month,
                dayname(  begin_date )               AS day_name

          WHERE customer_id = '000001'
            AND days_between( begin_date, end_date ) > 10

           INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(rew).
      APPEND |{ rew-begin_date } { rew-end_date } { rew-valid } { rew-add_7_days } { rew-add_3_months } { rew-duration } { rew-weekday } { rew-month } { rew-day_name } | TO r_output.
    ENDLOOP.


  ENDMETHOD.

  METHOD ts_conversions.

    SELECT FROM /dmo/travel
    FIELDS lastchangedat,

    CAST( lastchangedat AS DEC( 15,0 ) ) AS ts_short,

         tstmp_to_dats( tstmp = CAST( lastchangedat AS DEC( 15,0 ) ),
                        tzone = CAST( 'EST' AS CHAR( 6 ) )
                        ) AS date_est,
         tstmp_to_tims( tstmp = CAST( lastchangedat AS DEC( 15,0 ) ),
                        tzone = CAST( 'EST' AS CHAR( 6 ) )
                        ) AS time_est
    INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(rew).
      APPEND |{ rew-lastchangedat } { rew-ts_short } { rew-date_est } { rew-time_est }| TO r_output.
    ENDLOOP.


  ENDMETHOD.



  METHOD unit_conversions.

    SELECT FROM /dmo/connection
    FIELDS distance,
           distance_unit,

           unit_conversion( quantity = CAST( distance AS QUAN ),
                            source_unit = distance_unit,
                            target_unit = CAST( 'MI' AS UNIT )

                          ) AS distance_mi
           WHERE carrier_id = 'LH'
           INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(rew).
      APPEND |{ rew-distance } { rew-distance_unit } { rew-distance_mi } MI| TO r_output.
    ENDLOOP.


  ENDMETHOD.

  METHOD currency_conversions.

    DATA(today) = cl_abap_context_info=>get_system_date( ).

    TRY.
        SELECT FROM /dmo/travel
             FIELDS total_price,
                    currency_code,
                    currency_conversion(
                        amount           = total_price,
                        source_currency  = currency_code,
                        target_currency  = 'EUR',
                        exchange_rate_date = @today
                    ) AS total_price_EUR
            INTO TABLE @DATA(result)
            UP TO 20 ROWS.

        LOOP AT result INTO DATA(row).
          APPEND |{ row-total_price } { row-currency_code } { row-total_price_eur }| TO r_output.
        ENDLOOP.

      CATCH cx_sy_open_sql_db INTO DATA(lx_error).
        APPEND |Error: { lx_error->get_text( ) }| TO r_output.
        RETURN.
    ENDTRY.



  ENDMETHOD.

  METHOD order_by.

    SELECT FROM /dmo/flight
         FIELDS carrier_id,
                connection_id,
                flight_date,
                seats_max - seats_occupied AS seats
          WHERE carrier_id     = 'AA'
            AND plane_type_id  = 'A320-200'
       ORDER BY seats_max - seats_occupied DESCENDING,
                flight_date                ASCENDING
           INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(row).
      APPEND |{ row-carrier_id } { row-connection_id } { row-flight_date } seats: { row-seats }| TO r_output.
    ENDLOOP.

  ENDMETHOD.

  METHOD select_distinct.

    SELECT FROM /dmo/connection
      FIELDS
           DISTINCT
             airport_from_id,
             distance_unit

    ORDER BY airport_from_id
        INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(row).
      APPEND |{ row-airport_from_id } { row-distance_unit } | TO r_output.
    ENDLOOP.

  ENDMETHOD.



  METHOD aggregate_functions.

    SELECT FROM /dmo/connection
         FIELDS MAX( distance ) AS max,
                MIN( distance ) AS min,
                SUM( distance ) AS sum,
                AVG( distance ) AS average,
                COUNT( * ) AS count,
                COUNT( DISTINCT airport_from_id ) AS count_dist

          WHERE carrier_id = 'LH'
           INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(row).
      APPEND |{ row-max } { row-min } { row-average } { row-sum } { row-count } { row-count_dist }| TO r_output.
    ENDLOOP.


  ENDMETHOD.

  METHOD group_by.

    SELECT FROM /dmo/connection
       FIELDS
            carrier_id,

              MAX( distance ) AS max,
              MIN( distance ) AS min,
              SUM( distance ) AS sum,
              COUNT( * ) AS count

     GROUP BY carrier_id
       INTO TABLE @DATA(result).

    LOOP AT result INTO DATA(row).
      APPEND |{ row-carrier_id } { row-max } { row-min } { row-sum } { row-count } | TO r_output.
    ENDLOOP.

  ENDMETHOD.



ENDCLASS.

CLASS lcl_strings_processing DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS string_functions RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS processing_functions RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS regex RETURNING VALUE(r_output) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_strings_processing IMPLEMENTATION.

*
  METHOD string_functions.

    DATA text   TYPE string VALUE `Let's talk about ABAP`.
    DATA result TYPE i.

    APPEND |Text: "{ text }"| TO r_output.
    APPEND '____________________' TO r_output.
    APPEND ` ` TO r_output.

* numofchar( ) - Count number of characters
    result = numofchar( text ).
    APPEND |numofchar( text ) = { result } - Count total characters in string| TO r_output.
    APPEND ` ` TO r_output.

* find( ) - Find location of first occurrence
    result = find( val = text sub = 'A' ).
    APPEND |find( val = text sub = 'A' ) = { result } - Find first 'A', case sensitive, position starts at 0| TO r_output.

* find( ) - Case insensitive
    result = find( val = text sub = 'A' case = abap_false ).
    APPEND |find( val = text sub = 'A' case = abap_false ) = { result } - Find first 'A', case insensitive| TO r_output.

* find( ) - Last occurrence (occ = -1)
    result = find( val = text sub = 'A' case = abap_false occ = -1 ).
    APPEND |find( val = text sub = 'A' case = abap_false occ = -1 ) = { result } - Find last occurrence from right| TO r_output.

* find( ) - Second last occurrence (occ = -2)
    result = find( val = text sub = 'A' case = abap_false occ = -2 ).
    APPEND |find( val = text sub = 'A' case = abap_false occ = -2 ) = { result } - Find 2nd last occurrence from right| TO r_output.

* find( ) - Second occurrence from left (occ = 2)
    result = find( val = text sub = 'A' case = abap_false occ = 2 ).
    APPEND |find( val = text sub = 'A' case = abap_false occ = 2 ) = { result } - Find 2nd occurrence from left| TO r_output.

* find( ) - With offset (off)
    result = find( val = text sub = 'A' case = abap_false occ = 2 off = 10 ).
    APPEND |find( val = text sub = 'A' case = abap_false occ = 2 off = 10 ) = { result } - Start search at position 10| TO r_output.

* find( ) - With offset and length (off + len)
    result = find( val = text sub = 'A' case = abap_false occ = 2 off = 10 len = 10 ).
    APPEND |find( val = text sub = 'A' case = abap_false occ = 2 off = 10 len = 10 ) = { result } - Search within 10 chars from position 10| TO r_output.

    APPEND ` ` TO r_output.

* find_any_of( ) - Find first position of any character from sub
    result = find_any_of( val = text sub = 'A' ).
    APPEND |find_any_of( val = text sub = 'A' ) = { result } - Find first position of any char from 'A'| TO r_output.

* find_any_of( ) - Multiple characters
    result = find_any_of( val = text sub = 'aeiou' ).
    APPEND |find_any_of( val = text sub = 'aeiou' ) = { result } - Find first vowel position| TO r_output.

    APPEND ` ` TO r_output.

* count( ) - Count occurrences
    result = count( val = text sub = 'A' ).
    APPEND |count( val = text sub = 'A' ) = { result } - Count 'A' occurrences, case sensitive| TO r_output.

    result = count( val = text sub = 'A' case = abap_false ).
    APPEND |count( val = text sub = 'A' case = abap_false ) = { result } - Count 'A' occurrences, case insensitive| TO r_output.

  ENDMETHOD.



  METHOD processing_functions.

    DATA text TYPE string      VALUE ` SAP BTP,   ABAP Environment  `.

* Change Case of characters
**********************************************************************
    APPEND  |TO_UPPER         = {   to_upper(  text ) } | TO r_output.
    APPEND  |TO_LOWER         = {   to_lower(  text ) } | TO r_output.
    APPEND  |TO_MIXED         = {   to_mixed(  text ) } | TO r_output.
    APPEND  |FROM_MIXED       = { from_mixed(  text ) } | TO r_output.


* Change order of characters
**********************************************************************
    APPEND  |REVERSE             = {  reverse( text ) } | TO r_output.
    APPEND  |SHIFT_LEFT  (places)= {  shift_left(  val = text places   = 3  ) } | TO r_output.
    APPEND  |SHIFT_RIGHT (places)= {  shift_right( val = text places   = 3  ) } | TO r_output.
    APPEND  |SHIFT_LEFT  (circ)  = {  shift_left(  val = text circular = 3  ) } | TO r_output.
    APPEND  |SHIFT_RIGHT (circ)  = {  shift_right( val = text circular = 3  ) } | TO r_output.


* Extract a Substring
**********************************************************************
    APPEND  |SUBSTRING       = {  substring(        val = text off = 4 len = 10 ) } | TO r_output.
    APPEND  |SUBSTRING_FROM  = {  substring_from(   val = text sub = 'ABAP'     ) } | TO r_output.
    APPEND  |SUBSTRING_AFTER = {  substring_after(  val = text sub = 'ABAP'     ) } | TO r_output.
    APPEND  |SUBSTRING_TO    = {  substring_to(     val = text sub = 'ABAP'     ) } | TO r_output.
    APPEND  |SUBSTRING_BEFORE= {  substring_before( val = text sub = 'ABAP'     ) } | TO r_output.


* Condense, REPEAT and Segment
**********************************************************************
    APPEND  |CONDENSE         = {   condense( val = text ) } | TO r_output.
    APPEND  |CONDENSE No-gap  = {   condense( val = text  del = ' ' to = '' ) } | TO r_output.
    APPEND  |REPEAT           = {   repeat(   val = text occ = 2 ) } | TO r_output.

    APPEND  |SEGMENT1         = {   segment(  val = text sep = ',' index = 1 ) } |  TO r_output.
    APPEND  |SEGMENT2         = {   segment(  val = text sep = ',' index = 2 ) } |  TO r_output.

  ENDMETHOD.

*  METHOD regex.
*
*    DATA text TYPE string VALUE 'Date 1986-11-09 is in ISO-Formant'.
*    DATA regex TYPE string VALUE '[0-9]{4}(-[0-9]{2}){2}'.
*
*    IF contains( val = text regex = regex ).
*
*      DATA(number) = count( val = text regex = regex ).
*      DATA(offset) = find( val = text regex = regex occ = 1 ).
*      APPEND |original text: { text } | &&
*             |with regex: { regex }| &&
*             |number: { number } offset: { offset }|  TO r_output.
*
*      DATA(date_text) = match( val = text regex = regex occ = 1 ).
*      APPEND |Matched date { date_text }| TO r_output.
*
*      IF matches( val = text regex = regex  ).
*        APPEND |And matchs to ISO pattern| TO r_output.
*      ENDIF.
*    ENDIF.
*
*  ENDMETHOD.
  METHOD regex.

    DATA text  TYPE string VALUE 'Date 1986-11-09 is in ISO-Format'.
    DATA regex TYPE string VALUE '[0-9]{4}(-[0-9]{2}){2}'.
    DATA pcre  TYPE string VALUE '\d{4}(-\d{2}){2}'.

    APPEND 'Regular Expression Functions' TO r_output.
    APPEND '____________________________' TO r_output.
    APPEND ` ` TO r_output.

    APPEND |Original text: "{ text }"| TO r_output.
    APPEND |POSIX regex:   "{ regex }"| TO r_output.
    APPEND |PCRE pattern:  "{ pcre }"| TO r_output.
    APPEND ` ` TO r_output.

    APPEND '=== POSIX REGEX (regex = ) ===' TO r_output.
    APPEND '______________________________' TO r_output.

* contains( ) with POSIX regex
    IF contains( val = text regex = regex ).
      APPEND |contains( val = text regex = regex ) = TRUE - Pattern found| TO r_output.
    ELSE.
      APPEND |contains( val = text regex = regex ) = FALSE - Pattern not found| TO r_output.
    ENDIF.

* count( ) with POSIX regex
    DATA(number_regex) = count( val = text regex = regex ).
    APPEND |count( val = text regex = regex ) = { number_regex }| TO r_output.

* find( ) with POSIX regex
    DATA(offset_regex) = find( val = text regex = regex occ = 1 ).
    APPEND |find( val = text regex = regex occ = 1 ) = { offset_regex }| TO r_output.

* match( ) with POSIX regex
    DATA(match_regex) = match( val = text regex = regex occ = 1 ).
    APPEND |match( val = text regex = regex occ = 1 ) = "{ match_regex }"| TO r_output.

* matches( ) with POSIX regex
    IF matches( val = text regex = regex ).
      APPEND |matches( val = text regex = regex ) = TRUE| TO r_output.
    ELSE.
      APPEND |matches( val = text regex = regex ) = FALSE - Entire text does NOT match| TO r_output.
    ENDIF.

* replace( ) with POSIX regex
    DATA(replaced_regex) = replace( val = text regex = regex with = 'YYYY-MM-DD' ).
    APPEND |replace( regex = regex with = 'YYYY-MM-DD' ) = "{ replaced_regex }"| TO r_output.
    APPEND ` ` TO r_output.

    APPEND '=== PCRE (pcre = ) - Perl Compatible ===' TO r_output.
    APPEND '________________________________________' TO r_output.

* contains( ) with PCRE
    IF contains( val = text pcre = pcre ).
      APPEND |contains( val = text pcre = pcre ) = TRUE - Pattern found| TO r_output.
    ELSE.
      APPEND |contains( val = text pcre = pcre ) = FALSE - Pattern not found| TO r_output.
    ENDIF.

* count( ) with PCRE
    DATA(number_pcre) = count( val = text pcre = pcre ).
    APPEND |count( val = text pcre = pcre ) = { number_pcre }| TO r_output.

* find( ) with PCRE
    DATA(offset_pcre) = find( val = text pcre = pcre occ = 1 ).
    APPEND |find( val = text pcre = pcre occ = 1 ) = { offset_pcre }| TO r_output.

* match( ) with PCRE
    DATA(match_pcre) = match( val = text pcre = pcre occ = 1 ).
    APPEND |match( val = text pcre = pcre occ = 1 ) = "{ match_pcre }"| TO r_output.

* matches( ) with PCRE
    IF matches( val = text pcre = pcre ).
      APPEND |matches( val = text pcre = pcre ) = TRUE| TO r_output.
    ELSE.
      APPEND |matches( val = text pcre = pcre ) = FALSE - Entire text does NOT match| TO r_output.
    ENDIF.

* replace( ) with PCRE
    DATA(replaced_pcre) = replace( val = text pcre = pcre with = 'YYYY-MM-DD' ).
    APPEND |replace( pcre = pcre with = 'YYYY-MM-DD' ) = "{ replaced_pcre }"| TO r_output.
    APPEND ` ` TO r_output.

    APPEND '=== PCRE Advanced Features ===' TO r_output.
    APPEND '______________________________' TO r_output.

* Capture groups with PCRE
    DATA text2 TYPE string VALUE 'Email: john.doe@example.com is valid'.
    DATA pcre_email TYPE string VALUE '(\w+\.\w+)@(\w+\.\w+)'.

    DATA(email_match) = match( val = text2 pcre = pcre_email occ = 1 ).
    APPEND |Email match: "{ email_match }"| TO r_output.

* Case insensitive with PCRE (?i)
    DATA text3 TYPE string VALUE 'Hello WORLD hello world'.
    DATA pcre_case TYPE string VALUE '(?i)hello'.

    DATA(count_case) = count( val = text3 pcre = pcre_case ).
    APPEND |count( pcre = '(?i)hello' ) = { count_case } - Case insensitive| TO r_output.

* Word boundary with PCRE \b
    DATA text4 TYPE string VALUE 'test testing tested'.
    DATA pcre_word TYPE string VALUE '\btest\b'.

    DATA(count_word) = count( val = text4 pcre = pcre_word ).
    APPEND `count( pcre = '\btest\b' ) = ` && count_word && ` - Word boundary` TO r_output.

* Lookahead with PCRE (?=)
    DATA text5 TYPE string VALUE 'USD100 EUR200 USD300'.
    DATA pcre_lookahead TYPE string VALUE '\d+(?=.*USD)'.

    IF contains( val = text5 pcre = pcre_lookahead ).
      APPEND |Lookahead (?=) found numbers followed by USD| TO r_output.
    ENDIF.

* Replace all with occ = 0
    DATA text6 TYPE string VALUE 'Dates: 1986-11-09 and 2023-01-15'.
    DATA(replaced_all) = replace( val = text6 pcre = pcre with = 'DATE' occ = 0 ).
    APPEND |replace( occ = 0 ) replaces ALL: "{ replaced_all }"| TO r_output.

  ENDMETHOD.


ENDCLASS.

CLASS lcl_general DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS conv RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS timestemp RETURNING VALUE(r_output) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_general IMPLEMENTATION.

  METHOD conv.

    DATA number TYPE p LENGTH 3 DECIMALS 2.

    APPEND 'CONV vs EXACT Expressions' TO r_output.
    APPEND '_________________________' TO r_output.
    APPEND ` ` TO r_output.

* CONV - Lossy conversion (allows rounding/truncation)
    number = 1 / 8.
    APPEND |number = 1 / 8 → { number } - Standard assignment, rounds to 2 decimals| TO r_output.
    APPEND |number NUMBER = USER → { number NUMBER = USER } - Formatted with user settings| TO r_output.
    APPEND ` ` TO r_output.

* EXACT - Lossless conversion (raises exception if data lost)
    TRY.
        number = EXACT #( 1 / 8 ).
        APPEND |EXACT #( 1 / 8 ) = { number } - No exception, conversion was exact| TO r_output.
      CATCH cx_sy_conversion_error INTO DATA(error_num).
        APPEND |EXACT #( 1 / 8 ) → Exception: { error_num->get_text( ) } - Data would be lost| TO r_output.
    ENDTRY.
    APPEND ` ` TO r_output.

* CONV with date - Converts without validation
    APPEND 'Date Conversion Examples' TO r_output.
    APPEND '________________________' TO r_output.

    DATA(date_conv) = CONV d( 'ABCDEFGH' ).
    APPEND |CONV d( 'ABCDEFGH' ) = "{ date_conv }" - CONV allows invalid date, no exception| TO r_output.

    DATA(date_valid) = CONV d( '20230115' ).
    APPEND |CONV d( '20230115' ) = "{ date_valid }" - CONV with valid date| TO r_output.
    APPEND ` ` TO r_output.

* EXACT with date - Validates conversion
    TRY.
        DATA(date_exact) = EXACT d( 'ABCDEFGH' ).
        APPEND |EXACT d( 'ABCDEFGH' ) = "{ date_exact }" - No exception| TO r_output.
      CATCH cx_sy_conversion_error INTO DATA(error_date).
        APPEND |EXACT d( 'ABCDEFGH' ) → Exception: Conversion error - Invalid date format| TO r_output.
    ENDTRY.

    TRY.
        DATA(date_exact_valid) = EXACT d( '20230115' ).
        APPEND |EXACT d( '20230115' ) = "{ date_exact_valid }" - Valid date, no exception| TO r_output.
      CATCH cx_sy_conversion_error INTO DATA(error_valid).
        APPEND |EXACT d( '20230115' ) → Exception: { error_valid->get_text( ) }| TO r_output.
    ENDTRY.
    APPEND ` ` TO r_output.

* CONV with CHAR truncation
    APPEND 'Character Truncation Examples' TO r_output.
    APPEND '_____________________________' TO r_output.

    DATA(char2) = CONV char2( 'ABCDEFGH' ).
    APPEND |CONV char2( 'ABCDEFGH' ) = "{ char2 }" - CONV truncates to 2 chars, no exception| TO r_output.

    TRY.
        DATA(char2_exact) = EXACT char2( 'ABCDEFGH' ).
        APPEND |EXACT char2( 'ABCDEFGH' ) = "{ char2_exact }" - No exception| TO r_output.
      CATCH cx_sy_conversion_error INTO DATA(error_char).
        APPEND |EXACT char2( 'ABCDEFGH' ) → Exception: Data would be truncated| TO r_output.
    ENDTRY.

  ENDMETHOD.

  METHOD timestemp.

    DATA timestamp1 TYPE utclong.
    DATA timestamp2 TYPE utclong.
    DATA difference TYPE decfloat34.
    DATA date_user  TYPE d.
    DATA time_user  TYPE t.

    APPEND 'UTCLONG Timestamp Functions' TO r_output.
    APPEND '___________________________' TO r_output.
    APPEND ` ` TO r_output.

* Get current UTC timestamp
    timestamp1 = utclong_current( ).
    APPEND |utclong_current( ) = { timestamp1 } - Current UTC timestamp| TO r_output.
    APPEND ` ` TO r_output.

* Add days to timestamp
    timestamp2 = utclong_add( val = timestamp1 days = 7 ).
    APPEND |utclong_add( val = timestamp1 days = 7 ) = { timestamp2 } - Added 7 days| TO r_output.

* Other utclong_add parameters
    DATA(ts_hours) = utclong_add( val = timestamp1 hours = 5 ).
    APPEND |utclong_add( val = timestamp1 hours = 5 ) = { ts_hours } - Added 5 hours| TO r_output.

    DATA(ts_minutes) = utclong_add( val = timestamp1 minutes = 30 ).
    APPEND |utclong_add( val = timestamp1 minutes = 30 ) = { ts_minutes } - Added 30 minutes| TO r_output.

    DATA(ts_seconds) = utclong_add( val = timestamp1 seconds = 90 ).
    APPEND |utclong_add( val = timestamp1 seconds = 90 ) = { ts_seconds } - Added 90 seconds| TO r_output.
    APPEND ` ` TO r_output.

* Calculate difference between timestamps
    difference = utclong_diff( high = timestamp2 low = timestamp1 ).
    APPEND |utclong_diff( high = timestamp2 low = timestamp1 ) = { difference } seconds| TO r_output.
    APPEND |Difference in days: { difference / 3600 / 24 }| TO r_output.
    APPEND ` ` TO r_output.

* Convert UTCLONG to date and time (user timezone)
    APPEND 'CONVERT UTCLONG Statement' TO r_output.
    APPEND '_________________________' TO r_output.

    DATA(timezone) = cl_abap_context_info=>get_user_time_zone( ).

    CONVERT UTCLONG timestamp1
            INTO DATE date_user
                 TIME time_user
                 TIME ZONE timezone.

    APPEND |Time zone: { timezone }| TO r_output.
    APPEND |Date (user format): { date_user DATE = USER }| TO r_output.
    APPEND |Time (user format): { time_user TIME = USER }| TO r_output.
    APPEND ` ` TO r_output.

* Convert date/time back to UTCLONG
    APPEND 'CONVERT INTO UTCLONG Statement' TO r_output.
    APPEND '______________________________' TO r_output.

    DATA timestamp_back TYPE utclong.

    CONVERT DATE date_user
            TIME time_user
            TIME ZONE timezone
            INTO UTCLONG timestamp_back.

    APPEND |Converted back to UTCLONG: { timestamp_back }| TO r_output.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_generate_data DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS zlrn_airport.
    CLASS-METHODS zlrn_cargoflight.
    CLASS-METHODS fill_zs4d401_flights RETURNING VALUE(r_output) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_generate_data IMPLEMENTATION.

  METHOD zlrn_airport.

    SELECT * FROM zlrn_airport INTO TABLE @DATA(lt_del).
    IF lt_del IS NOT INITIAL.
      DELETE zlrn_airport FROM TABLE @lt_del.
    ENDIF.

    DATA airport TYPE TABLE OF zlrn_airport.
    DATA rew LIKE LINE OF airport.
    DATA output TYPE string_table.

    SELECT
    FROM /dmo/airport
    FIELDS *
    INTO TABLE @DATA(airport_org).

    LOOP AT airport_org INTO DATA(ls_airport_org).
      APPEND INITIAL LINE TO airport ASSIGNING FIELD-SYMBOL(<rew>).

      <rew>-airport_id = ls_airport_org-airport_id.
      <rew>-city       = ls_airport_org-city.
      <rew>-country    = ls_airport_org-country.
      <rew>-name       = ls_airport_org-name.

      " Assign timezone based on country/region
      <rew>-timzone = SWITCH #( ls_airport_org-country
        " Europe
        WHEN 'DE' OR 'FR' OR 'AT' OR 'CH' OR 'NL' OR 'IT' THEN 'UTC+1'   " CET
        WHEN 'GB' THEN 'UTC'                                             " GMT
        WHEN 'RU' THEN 'UTC+3'                                           " MSK

        " North America - need per-airport handling
        WHEN 'US' THEN SWITCH #( ls_airport_org-airport_id
          WHEN 'JFK' OR 'BOS' OR 'MIA' OR 'EWR' THEN 'UTC-5'             " EST
          WHEN 'BNA' OR 'HOU' OR 'MCI'          THEN 'UTC-6'             " CST
          WHEN 'ELP' OR 'DEN'                   THEN 'UTC-7'             " MST
          WHEN 'LAS' OR 'LAX' OR 'SFO'          THEN 'UTC-8'             " PST
          ELSE 'UTC-5' )
        WHEN 'CA' THEN 'UTC-5'                                           " EST (Ottawa)
        WHEN 'MX' THEN 'UTC-6'                                           " CST
        WHEN 'CU' THEN 'UTC-5'                                           " Cuba

        " South America
        WHEN 'BR' THEN 'UTC-3'                                           " BRT

        " Asia
        WHEN 'JP' THEN 'UTC+9'                                           " JST
        WHEN 'SG' THEN 'UTC+8'                                           " SGT
        WHEN 'MY' THEN 'UTC+8'                                           " MYT
        WHEN 'CN' THEN 'UTC+8'                                           " HKT (Hong Kong)
        WHEN 'TH' THEN 'UTC+7'                                           " ICT

        " Australia
        WHEN 'AU' THEN 'UTC+9'                                           " ACST (Alice Springs ~+9:30)

        " Africa
        WHEN 'ZW' THEN 'UTC+2'                                           " CAT
        WHEN 'SA' THEN 'UTC+2'                                           " SAST

        " Spain - special handling (mainland vs Canary Islands)
        WHEN 'ES' THEN SWITCH #( ls_airport_org-airport_id
          WHEN 'ACE' THEN 'UTC'                                          " WET (Canary Islands)
          ELSE 'UTC+1' )                                                 " CET (mainland)

        ELSE 'UTC'
      ).

      APPEND |{ <rew>-airport_id },{ <rew>-city },{ <rew>-country },{ <rew>-name },{ <rew>-timzone }| TO output.
    ENDLOOP.


    INSERT zlrn_airport FROM TABLE @airport.

  ENDMETHOD.

  METHOD zlrn_cargoflight.

*    DATA cargoflight TYPE TABLE OF /dmo/flight.
*    SELECT * FROM zlrn_airport INTO TABLE @DATA(lt_del).
*    IF lt_del IS NOT INITIAL.
*      DELETE zlrn_airport FROM TABLE @lt_del.
*    ENDIF.
*
*    SELECT
*    FROM /dmo/flight
*    FIELDS *
*    INTO TABLE @DATA(flights).
*
*    SELECT
*    FROM /dmo/connection
*    FIELDS *
*    INTO TABLE @DATA(connections).
*
*
*
*    SORT connections BY carrier_id connection_id.
*
*    LOOP AT flights INTO DATA(flight).
*      APPEND INITIAL LINE TO cargoflight ASSIGNING FIELD-SYMBOL(<fs_cflight>).
*
*      <fs_cflight>-carrier_id    = flight-carrier_id.
*      <fs_cflight>-connection_id = flight-connection_id.
*      <fs_cflight>-flight_date   = flight-flight_date.
*      <fs_cflight>-plane_type_id = flight-plane_type_id.
*      READ TABLE connections INTO DATA(connection)
*      WITH KEY carrier_id = flight-carrier_id connection_id = flight-connection_id BINARY SEARCH.
*      IF sy-subrc = 0.
*        <fs_cflight>-airport_from_id = connection-airport_from_id.
*        <fs_cflight>-airport_to_id   = connection-airport_to_id.
*        <fs_cflight>-departure_time  = connection-departure_time.
*        <fs_cflight>-arrival_time    = connection-arrival_time.
*      ENDIF.
*      <fs_cflight>-load_unit = 'KG'.
*      <fs_cflight>-maximum_load = COND #(
*       WHEN flight-plane_type_id = '737-800'    THEN 21000
*       WHEN flight-plane_type_id = '747-400'    THEN 120000
*       WHEN flight-plane_type_id = '767-200'    THEN 30000
*       WHEN flight-plane_type_id = 'A320-200'   THEN 16000
*       WHEN flight-plane_type_id = 'A321-200'   THEN 27000
*       WHEN flight-plane_type_id = 'A340-600'   THEN 75800
*       WHEN flight-plane_type_id = 'A380-800'   THEN 11300
*       WHEN flight-plane_type_id = 'A310-200ST' THEN 120000
*       WHEN flight-plane_type_id = 'A310-200F'  THEN 70000
*       WHEN flight-plane_type_id = '373-200SF'  THEN 70000
*       WHEN flight-plane_type_id = '747-400F'   THEN 120000
*       WHEN flight-plane_type_id = 'A330-200'   THEN 45000
*       WHEN flight-plane_type_id = 'A319-100'   THEN 14000
*       WHEN flight-plane_type_id = 'DC-10-10'   THEN 45000
*       WHEN flight-plane_type_id = 'MD-11'      THEN 90000
*       WHEN flight-plane_type_id = '777-200'    THEN 100000
*       WHEN flight-plane_type_id = '757-200'    THEN 39800
*       ELSE 50000 ).
*      <fs_cflight>-actual_load = ( flight-seats_occupied / flight-seats_max ) * <fs_cflight>-maximum_load.
*    ENDLOOP.
*
*    INSERT zlrn_cargoflight FROM TABLE @cargoflight.

  ENDMETHOD.

  METHOD fill_zs4d401_flights.


    DATA flights TYPE TABLE OF /dmo/flight.
    DATA insert_tab TYPE TABLE OF /dmo/flight.


    DELETE FROM ZS4D401_flights.


    SELECT FROM /dmo/flight FIELDS * ORDER BY carrier_Id, connection_id INTO TABLE @flights .


    LOOP AT flights INTO DATA(first_date).
* Original table flights has 2 flights per connection. Only process the first
      IF sy-tabix MOD 2 = 0. CONTINUE. ENDIF.


*Extend flight dates by 2000 days
      DO 2000 TIMES.
        APPEND first_date TO insert_tab.
        first_date-flight_date += 1.
      ENDDO.
* ENDIF.
    ENDLOOP.


* Read highest connection number for each flight
    SELECT FROM /dmo/flight AS main FIELDS carrier_Id, connection_id, flight_date, price, currency_code, plane_type_id
    WHERE connection_id = ( SELECT MAX( connection_id ) FROM /dmo/flight WHERE carrier_id = main~carrier_id )
    AND flight_Date = ( SELECT MIN( flight_date ) FROM /dmo/flight WHERE carrier_id = main~carrier_id AND connection_id = main~connection_id )
    GROUP BY carrier_id, connection_Id, flight_date, price, currency_code, plane_type_id
    ORDER BY carrier_id, connection_id
    INTO TABLE @DATA(max).


*Add 50 new connection numbers and 2000 days of flights for each
    LOOP AT max INTO DATA(line).
      DO 50 TIMES.
        line-connection_id += 1.
        line-plane_type_id = SWITCH #( CONV i( line-connection_id ) MOD 2 WHEN 0 THEN 'A330' WHEN 1 THEN 'A350' ).




        first_date = CORRESPONDING #( line ).
        DATA(repetitions) = COND i( WHEN line-carrier_id = 'LH' AND line-connection_id = '0405' THEN 4000 ELSE 2000 ).
        DO repetitions TIMES.
          first_date-seats_max = 220.
          APPEND first_date TO insert_tab.
          first_date-flight_date += 1.
        ENDDO.
      ENDDO.
    ENDLOOP.




    SORT insert_tab BY carrier_Id connection_id flight_date.
    DELETE ADJACENT DUPLICATES FROM insert_tab COMPARING carrier_id connection_id flight_date.




    INSERT ZS4D401_flights FROM TABLE @insert_tab.
    APPEND  |Generated { sy-dbcnt } rows in table ZS4D401_flights| TO r_output.



  ENDMETHOD.

ENDCLASS.

CLASS lcl_improving_internal_tables DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS sort RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS delete_adjacent_duplicates RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS table_comprehensions RETURNING VALUE(r_output) TYPE string_table.
    CLASS-METHODS Reductions RETURNING VALUE(r_output) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_improving_internal_tables IMPLEMENTATION.

  METHOD sort.

    TYPES t_flights TYPE STANDARD TABLE OF /dmo/flight
                WITH NON-UNIQUE KEY carrier_id connection_id flight_date.

    DATA flights TYPE t_flights.

    flights = VALUE #( ( client        = sy-mandt
                         carrier_id    = 'LH'
                         connection_id = '0400'
                         flight_date   = '20230201'
                         plane_type_id = '747-400'
                         price         = '600'
                         currency_code = 'EUR' )
                       ( client        = sy-mandt
                         carrier_id    = 'LH'
                         connection_id = '0400'
                         flight_date   = '20230115'
                         plane_type_id = '747-400'
                         price         = '600'
                         currency_code = 'EUR' )
                       ( client        = sy-mandt
                         carrier_id    = 'QF'
                         connection_id = '0006'
                         flight_date   = '20230112'
                         plane_type_id = 'A380'
                         price         = '1600'
                         currency_code = 'AUD' )
                       ( client        = sy-mandt
                         carrier_id    = 'AA'
                         connection_id = '0017'
                         flight_date   = '20230110'
                         plane_type_id = '747-400'
                         price         = '600'
                         currency_code = 'USD' )
                       ( client        = sy-mandt
                         carrier_id    = 'UA'
                         connection_id = '0900'
                         flight_date   = '20230201'
                         plane_type_id = '777-200'
                         price         = '600'
                         currency_code = 'USD' )
                     ).


    APPEND 'Contents Before Sort' TO r_output.
    APPEND '____________________' TO r_output.
    LOOP AT flights INTO DATA(row).
      APPEND |{ row-carrier_id } { row-connection_id } { row-flight_date } { row-plane_type_id } { row-price } { row-currency_code }| TO r_output.
    ENDLOOP.
    APPEND ` ` TO r_output.


* Sort with no additions - sort by primary table key carrier_id connection_id flight_date
    SORT flights.

    APPEND 'Effect of SORT with no additions - sort by primary table key' TO r_output.
    APPEND '____________________________________________________________' TO r_output.
    LOOP AT flights INTO row.
      APPEND |{ row-carrier_id } { row-connection_id } { row-flight_date } { row-plane_type_id } { row-price } { row-currency_code }| TO r_output.
    ENDLOOP.
    APPEND ` ` TO r_output.


* Sort with field list - default sort direction is ascending
    SORT flights BY currency_code plane_type_id.

    APPEND 'Effect of SORT with field list - ascending is default direction' TO r_output.
    APPEND '________________________________________________________________' TO r_output.
    LOOP AT flights INTO row.
      APPEND |{ row-carrier_id } { row-connection_id } { row-flight_date } { row-plane_type_id } { row-price } { row-currency_code }| TO r_output.
    ENDLOOP.
    APPEND ` ` TO r_output.


* Sort with field list and sort directions.
    SORT flights BY carrier_id ASCENDING flight_date DESCENDING.

    APPEND 'Effect of SORT with field list and sort direction' TO r_output.
    APPEND '_________________________________________________' TO r_output.
    LOOP AT flights INTO row.
      APPEND |{ row-carrier_id } { row-connection_id } { row-flight_date } { row-plane_type_id } { row-price } { row-currency_code }| TO r_output.
    ENDLOOP.
    APPEND ` ` TO r_output.

  ENDMETHOD.

  METHOD delete_adjacent_duplicates.

    TYPES t_flights TYPE STANDARD TABLE OF /dmo/flight
                    WITH NON-UNIQUE KEY carrier_id connection_id flight_date.

    DATA flights TYPE t_flights.

    flights = VALUE #( ( client        = sy-mandt
                         carrier_id    = 'LH'
                         connection_id = '0400'
                         flight_date   = '20230201'
                         plane_type_id = '747-400'
                         price         = '600'
                         currency_code = 'EUR' )
                       ( client        = sy-mandt
                         carrier_id    = 'QF'
                         connection_id = '0006'
                         flight_date   = '20230112'
                         plane_type_id = 'A380'
                         price         = '1600'
                         currency_code = 'AUD' )
                       ( client        = sy-mandt
                         carrier_id    = 'AA'
                         connection_id = '0017'
                         flight_date   = '20230110'
                         plane_type_id = '747-400'
                         price         = '600'
                         currency_code = 'USD' )
                       ( client        = sy-mandt
                         carrier_id    = 'LH'
                         connection_id = '0400'
                         flight_date   = '20230301'
                         plane_type_id = '747-400'
                         price         = '600'
                         currency_code = 'EUR' )
                       ( client        = sy-mandt
                         carrier_id    = 'UA'
                         connection_id = '0900'
                         flight_date   = '20230201'
                         plane_type_id = '777-200'
                         price         = '600'
                         currency_code = 'USD' )
                       ( client        = sy-mandt
                         carrier_id    = 'QF'
                         connection_id = '0006'
                         flight_date   = '20230210'
                         plane_type_id = 'A380'
                         price         = '1600'
                         currency_code = 'AUD' )
                     ).

    APPEND 'Contents Before DELETE ADJACENT DUPLICATES' TO r_output.
    APPEND '____________________' TO r_output.
    LOOP AT flights INTO DATA(row).
      APPEND |{ row-carrier_id } { row-connection_id } { row-flight_date } { row-plane_type_id } { row-price } { row-currency_code }| TO r_output.
    ENDLOOP.
    APPEND ` ` TO r_output.

    DELETE ADJACENT DUPLICATES FROM flights.

    APPEND 'Contents after DELETE ADJACENT DUPLICATES' TO r_output.
    APPEND 'Nothing deleted - key values are not adjacent' TO r_output.
    APPEND 'Sort the table before DELETE ADJACENT DUPLICATES' TO r_output.
    LOOP AT flights INTO row.
      APPEND |{ row-carrier_id } { row-connection_id } { row-flight_date } { row-plane_type_id } { row-price } { row-currency_code }| TO r_output.
    ENDLOOP.
    APPEND ` ` TO r_output.

    SORT flights BY carrier_id connection_id flight_date.
    DELETE ADJACENT DUPLICATES FROM flights.

    APPEND 'Contents after DELETE ADJACENT DUPLICATES' TO r_output.
    APPEND 'Nothing deleted - ABAP compares all key values including flight_date, which is different for every entry' TO r_output.
    LOOP AT flights INTO row.
      APPEND |{ row-carrier_id } { row-connection_id } { row-flight_date } { row-plane_type_id } { row-price } { row-currency_code }| TO r_output.
    ENDLOOP.
    APPEND ` ` TO r_output.

    DELETE ADJACENT DUPLICATES FROM flights COMPARING carrier_id connection_id.

    APPEND 'Contents after DELETE ADJACENT DUPLICATES with COMPARING and field list' TO r_output.
    APPEND 'Entries with identical values of carrier_id and connection_id have been deleted' TO r_output.
    LOOP AT flights INTO row.
      APPEND |{ row-carrier_id } { row-connection_id } { row-flight_date } { row-plane_type_id } { row-price } { row-currency_code }| TO r_output.
    ENDLOOP.

    DATA new_flights TYPE STANDARD TABLE OF /dmo/flight WITH NON-UNIQUE KEY carrier_id connection_id flight_date.
    new_flights = VALUE #( FOR line IN flights
                           ( CORRESPONDING #( line ) )
                            ).

  ENDMETHOD.

  METHOD table_comprehensions.

    TYPES: BEGIN OF t_connection,
             carrier_id             TYPE /dmo/carrier_id,
             connection_id          TYPE /dmo/connection_id,
             airport_from_id        TYPE /dmo/airport_from_id,
             departure_airport_name TYPE /dmo/airport_name,
           END OF t_connection.

    TYPES t_connections TYPE STANDARD TABLE OF t_connection
                        WITH NON-UNIQUE KEY carrier_id connection_id.

    DATA connections TYPE TABLE OF /dmo/connection.
    DATA airports    TYPE TABLE OF /dmo/airport.
    DATA result_table TYPE t_connections.

* Aim of the method:
* Read a list of connections from the database and use them to fill an internal table result_table.
* This contains some data from the table connections and adds the name of the departure airport.

    SELECT FROM /dmo/airport FIELDS * INTO TABLE @airports.
    SELECT FROM /dmo/connection FIELDS * INTO TABLE @connections.

    APPEND 'Connection Table' TO r_output.
    APPEND '________________' TO r_output.
    LOOP AT connections INTO DATA(conn).
      APPEND |{ conn-carrier_id } { conn-connection_id } { conn-airport_from_id } { conn-airport_to_id }| TO r_output.
    ENDLOOP.
    APPEND ` ` TO r_output.

* The VALUE expression iterates over the table connections. In each iteration, the variable line
* accesses the current line. Inside the parentheses, we build the next line of result_table by
* copying the values of line-carrier_id, line-connection_id and line-airport_from_id, then
* looking up the airport name in the internal table airports using a table expression

    "Copy between itabs
    "1) CORRESPONDING
    DATA result_table_c TYPE STANDARD TABLE OF t_connection WITH NON-UNIQUE KEY carrier_id connection_id.
    result_table_c = CORRESPONDING #( connections ).
    APPEND 'Results CORRESPONDING' TO r_output.
    APPEND '_______' TO r_output.

    LOOP AT result_table_c INTO DATA(row).
      APPEND |{ row-carrier_id } { row-connection_id } { row-airport_from_id }| TO r_output.
    ENDLOOP.

    "2) VALUE FOR
    DATA result_table_vf TYPE STANDARD TABLE OF t_connection WITH NON-UNIQUE KEY carrier_id connection_id.
    result_table_vf = VALUE #( FOR line IN connections
                               ( CORRESPONDING #( line ) )
                             ).
    APPEND 'Results VALUE FOR' TO r_output.
    APPEND '_______' TO r_output.

    LOOP AT result_table_vf INTO DATA(row_vf).
      APPEND |{ row_vf-carrier_id } { row_vf-connection_id } { row_vf-airport_from_id }| TO r_output.
    ENDLOOP.

    result_table = VALUE #( FOR line IN connections
                            ( carrier_id             = line-carrier_id
                              connection_id          = line-connection_id
                              airport_from_id        = line-airport_from_id
                              departure_airport_name = airports[ airport_id = line-airport_from_id ]-name )
                          ).

    APPEND 'Results' TO r_output.
    APPEND '_______' TO r_output.
    LOOP AT result_table INTO DATA(row_2).
      APPEND |{ row_2-carrier_id } { row_2-connection_id } { row_2-airport_from_id } { row_2-departure_airport_name }| TO r_output.
    ENDLOOP.

  ENDMETHOD.

  METHOD reductions.

    TYPES: BEGIN OF t_results,
             occupied TYPE /dmo/plane_seats_occupied,
             maximum  TYPE /dmo/plane_seats_max,
           END OF t_results.

    TYPES: BEGIN OF t_results_with_avg,
             occupied TYPE /dmo/plane_seats_occupied,
             maximum  TYPE /dmo/plane_seats_max,
             average  TYPE p LENGTH 16 DECIMALS 2,
           END OF t_results_with_avg.

    DATA flights TYPE TABLE OF /dmo/flight.

    SELECT FROM /dmo/flight FIELDS * INTO TABLE @flights.

* Result is a scalar data type
    DATA(sum) = REDUCE i( INIT total = 0
                          FOR line IN flights
                          NEXT total += line-seats_occupied ).

    APPEND 'Result is a scalar data type' TO r_output.
    APPEND '____________________________' TO r_output.
    APPEND |Sum of occupied seats: { sum }| TO r_output.
    APPEND ` ` TO r_output.

* Result is a structured data type
    DATA(results) = REDUCE t_results( INIT totals TYPE t_results
                                      FOR line IN flights
                                      NEXT totals-occupied += line-seats_occupied
                                           totals-maximum  += line-seats_max ).

    APPEND 'Result is a structure' TO r_output.
    APPEND '_____________________' TO r_output.
    APPEND |Occupied: { results-occupied } Maximum: { results-maximum }| TO r_output.
    APPEND ` ` TO r_output.

* Result is a structured data type
* Reduction uses a local helper variable
* Result of the reduction is always the *first* variable declared after init
    DATA(result_with_average) = REDUCE t_results_with_avg(
                                    INIT totals_avg TYPE t_results_with_avg
                                         count = 1
                                    FOR line IN flights
                                    NEXT totals_avg-occupied += line-seats_occupied
                                         totals_avg-maximum  += line-seats_max
                                         totals_avg-average  = totals_avg-occupied / count
                                         count += 1 ).

    APPEND 'Result is a structure. The average is calculated using a local helper variable' TO r_output.
    APPEND '______________________________________________________________________________' TO r_output.
    APPEND |Occupied: { result_with_average-occupied } Maximum: { result_with_average-maximum } Average: { result_with_average-average }| TO r_output.


  ENDMETHOD.

ENDCLASS.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.


    METHODS use_work_area.
    METHODS use_field_symbol.
  PRIVATE SECTION.
    TYPES t_flights TYPE STANDARD TABLE OF /dmo/flight WITH NON-UNIQUE KEY carrier_id connection_id flight_date.


    METHODS loop_field_symbol CHANGING c_flights TYPE t_flights.
    METHODS loop_work_area CHANGING c_flights TYPE t_flights.
ENDCLASS.


CLASS lcl_demo IMPLEMENTATION.


  METHOD use_field_symbol.


    DATA flights TYPE t_flights.
    SELECT FROM /dmo/flight FIELDS * INTO TABLE @flights.
    loop_field_symbol( CHANGING c_flights = flights ).


  ENDMETHOD.


  METHOD use_work_area.

    DATA flights TYPE t_flights.
    SELECT FROM /dmo/flight FIELDS * INTO TABLE @flights.
    loop_work_area( CHANGING c_flights = flights ).

  ENDMETHOD.


  METHOD loop_field_symbol.


    LOOP AT c_flights ASSIGNING FIELD-SYMBOL(<flight>).
      <flight>-seats_occupied += 1.
    ENDLOOP.


  ENDMETHOD.


  METHOD loop_work_area.
    LOOP AT c_flights INTO DATA(flight).
      flight-seats_occupied += 1.
      MODIFY c_flights FROM flight.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.


CLASS lcl_flights_p_key_access DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS access_standard.
    METHODS access_sorted.
    METHODS access_hashed.
  PRIVATE SECTION.


    DATA standard_table TYPE STANDARD TABLE OF zs4d401_flights WITH NON-UNIQUE KEY carrier_id connection_id flight_date.
    DATA sorted_table   TYPE SORTED   TABLE OF zs4d401_flights with non-UNIQUE KEY carrier_id connection_id "flight_date
    .
    DATA hashed_table   TYPE HASHED   TABLE OF zs4d401_flights WITH UNIQUE     KEY carrier_id connection_id flight_date.


    DATA key_carrier_id TYPE /dmo/carrier_id.
    DATA key_connection_id TYPE /dmo/connection_id.
    DATA key_date TYPE /dmo/flight_date.
    METHODS set_line_to_read.


ENDCLASS.


CLASS lcl_flights_p_key_access IMPLEMENTATION.


  METHOD access_hashed.
    DATA(result) = hashed_table[ carrier_Id = me->key_carrier_id connection_Id = me->key_connection_id flight_date = me->key_date ].
  ENDMETHOD.


  METHOD access_sorted.
    DATA(result) = sorted_table[ carrier_Id = me->key_carrier_id connection_Id = me->key_connection_id flight_date = me->key_date ].
  ENDMETHOD.


  METHOD constructor.
    SELECT FROM zs4d401_flights FIELDS * INTO TABLE @standard_table.
    SELECT FROM zs4d401_flights FIELDS * INTO TABLE @sorted_table.
    SELECT FROM zs4d401_flights FIELDS * INTO TABLE @hashed_table.


    set_line_to_read( ).
  ENDMETHOD.


  METHOD access_standard.
    DATA(result) = standard_table[ carrier_Id = me->key_carrier_id connection_Id = me->key_connection_id flight_date = me->key_date ].
  ENDMETHOD.




  METHOD set_line_to_read.
    DATA(line) = standard_table[ CONV i( lines( standard_table ) * '0.65' ) ].
    me->key_carrier_id = line-carrier_Id.
    me->key_connection_Id = line-connection_id.
    me->key_date = line-flight_date.


  ENDMETHOD.




ENDCLASS.


CLASS lcl_flights_s_key_access DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS read_primary.
    METHODS read_non_key.
    METHODS read_secondary_1.
    METHODS read_secondary_2.
    METHODS read_secondary_3.


  PRIVATE SECTION.
    DATA connections    TYPE SORTED TABLE OF Zs4d401_flights WITH NON-UNIQUE KEY carrier_id connection_Id flight_date.
    DATA connections_sk TYPE SORTED TABLE OF Zs4d401_flights WITH NON-UNIQUE KEY carrier_id connection_id flight_date
    WITH NON-UNIQUE SORTED KEY k_plane COMPONENTS plane_type_id.


ENDCLASS.


CLASS lcl_flights_s_key_access IMPLEMENTATION.


  METHOD constructor.
    SELECT FROM Zs4d401_flights FIELDS * INTO TABLE @connections.
    SELECT FROM Zs4d401_flights FIELDS * INTO TABLE @connections_sk.
  ENDMETHOD.


  METHOD read_non_key.
    LOOP AT connections INTO DATA(connection) WHERE plane_type_id = '737-800'.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_primary.
    DATA count TYPE i VALUE 1.
    LOOP AT connections INTO DATA(connection) WHERE carrier_id = 'LH' AND connection_id = '0405' .
      count += 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_secondary_1.
    LOOP AT connections_sk INTO DATA(connection) USING KEY k_plane WHERE plane_type_id = '737-800'.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_secondary_2.
    LOOP AT connections_sk INTO DATA(connection) USING KEY k_plane WHERE plane_type_id = '737-800'.
    ENDLOOP.
  ENDMETHOD.
  METHOD read_secondary_3.
    LOOP AT connections_sk INTO DATA(connection) USING KEY k_plane WHERE plane_type_id = '737-800'.
    ENDLOOP.


  ENDMETHOD.


ENDCLASS.

CLASS lcl_auth_check DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS authority_check RETURNING VALUE(r_output) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_auth_check IMPLEMENTATION.

  METHOD authority_check.

    "Created view ZS4D401_AIRPORTS, access control ZS4D401_AIRPORTS,
    "Defined IAM apps ZBM_ZS4D401_IAM_EXT, business catalog ZBM_BC + bublishing.
    "Define insde fiori business roles->new->zbm->assaing users->save.

    AUTHORITY-CHECK OBJECT 'ZBM_FLIGHT'
    ID '/DMO/CNTRY' FIELD 'DE'.

    APPEND |Result: sy-subrc = { sy-subrc }| TO r_output.

    IF sy-subrc = 0.
      APPEND 'You HAVE authorization for DE' TO r_output.
    ELSE.
      APPEND 'You do NOT have authorization' TO r_output.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
