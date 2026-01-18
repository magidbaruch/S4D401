CLASS zcl_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_test IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

*    SELECT FROM zconnections
*    FIELDS *
*    INTO TABLE @DATA(connections).
*
*    out->write( connections ).
*
    AUTHORITY-CHECK OBJECT 'ZBM'
      ID 'ZCARR' FIELD 'LH'
      ID 'ACTVT' FIELD '03'.

    IF sy-subrc = 0.
      out->write( 'User authorized for LH' ).
    ELSE.
      out->write( |Not authorized. sy-subrc = { sy-subrc }| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
